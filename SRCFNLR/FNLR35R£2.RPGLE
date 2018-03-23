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
010300160623     d
010400160623     d sps             s              2  0 dim(101)
010500020905     D*
010600020905     D* DEFINIZIONE STRUTTURE DATI (INTERNE)
010700960327     D*
010800990511     D FNANM1          DS
010900960405     D  ANMNR1               397    400B 0
011000990511     D FNANM2          DS
011100960705     D  ANMNR2               397    400B 0
011200990511     D FNANM3          DS
011300970529     D  ANMNR3               397    400B 0
011400990511     D FNAN10          DS
011500960417     D  ANMN10               397    400B 0
011600990511     D FNANM7          DS
011700960327     D  ANMNR7               397    400B 0
011800990511     D FNANM8          DS
011900960327     D  ANMNR8               397    400B 0
012000990511     D FNANM9          DS
012100960327     D  ANMNR9               397    400B 0
012200960327     D**
012300911015     D WLBDAT          DS
012400971126     D  G02DAT                 1      8  0
012500971126     D  G02INV                 9     16  0
012600971126     D  G02ERR                17     17
012700971126     D  G02TGI                18     22  0
012800960510     D WGIDAT          DS
012900990512     D  GI8DAT                 1      8  0
013000990512     D  GI8INV                 9     16  0
013100990512     D  WAA                    9     12  0
013200990512     D  WMM                   13     14  0
013300990512     D  WGG                   15     16  0
013400990512     D  GI8TGI                17     21  0
013500960927     D                 DS
013600000503     D  FFV                    1    897
013700000503     D                                     DIM(299)                             LINEE FOGLIO
013800960927    +D  FGVFFV                 1    240
013900960927    +D  FGVFF2               241    447
014000000503    +D  FGWFF3               448    687
014100000503    +D  FGWFF4               688    897
014200960927     D*
014300960927     D                 DS
014400000503    +D  FLP                    1    897
014500000503     D                                     DIM(299)                             LINEE TRANSITO
014600960927    +D  FGVFLP                 1    240
014700960927    +D  FGVFL2               241    447
014800000503    +D  FGWFL3               448    687
014900000503    +D  FGWFL4               688    897
015000960510     D                 DS
015100960510     D  CLNPOM                 1     31
015200960510     D  POM                    1     31
015300960510     D                                     DIM(31)                              FESTIV.MATTINO
015400990121     D                 DS
015500031007     D  DCTAAC                 1      4  0
015600031007     D  DCTMGC                 5      8  0
015700031007     D  DCTDCA                 1      8  0 INZ
015800021008     D*
015900021008     D DSmembro        ds             7    inz
016000021008     D  MbrM                   1      1    inz('M')
016100021008     D  MbrPOP                 2      4  0 inz
016200021008     D  MbrPOA                 5      7  0 inz
016300020905     D*
016400020905     D* DEFINIZIONE STRUTTURE DATI (ESTERNE)
016500020905     D*
016600900622     D KPJBA         E DS
016700021008     ***D  LIBSYS                92    100
016800001102     D  PARSCH               502    502
016900020905     D UT§DSE0F      E DS
017000020905     D CNCR80        E DS
017100960422     D OG148         E DS
017200991217     D OG143         E DS
017300960510     D DS1Y          E DS
017400990902     D DS7A          E DS
017500990902     D DS7AF         E DS
017600960403     D DS7A2         E DS
017700921027     D DS7C          E DS
017800160623     D DS7J          E DS
017900020905     D DCCH          E DS
018000911021     D DSCMZ         E DS
018100990511     D DSANM         E DS                  EXTNAME(FNANM00F)
018200031007     D DLAN          E DS
018300040616     D DANMFLO       E DS
018400960326     D*
018500960326     D* PASSAGGIO PARAMETRI - DAL PGM FNLR34R FNLR49R
018600960326     D DSLR34        E DS                  EXTNAME(FNLR34DS)
018700990513     D  ANM                   89    118
018800921015     D                                     DIM(10)                              ANOMALIE VIDEO
018900040617     D  spo                  140    229
019000040614     D                                     DIM(30)                              ANOMALIE VIDEO
019100960326     D*
019200960326     D* ds per pgm trul06r - caricamento £x
019300960326     D DSUL06        E DS                  EXTNAME(TRUL06DS)
019400960326     D  LIN                    1     90  0
019500960927     D                                     DIM(30)                              COMODO
019600040614      * Parametri per richiamo a TRUL09R - carico p.o. 2 livello in arr
019700040614     d Trul09ds      e ds
019800040614     d  po2liv                19    108  0 dim(30)
019900020905     D* PASSAGGIO PARAMETRI AL PGM DI GESTIONE ANOMALIE
020000020905     D DSLR44        E DS                  EXTNAME(FNLR44DS)
020100020905     D* PASSAGGIO PARAMETRI AL PGM DI ALLOCAZIONE FILE
020200021009     D FNLV60DS      E DS
020300020905     D* CALL A PGM CHE CARICA I DATI FOGLI  - FNLV53R
020400020905     D DSLV53        E DS                  EXTNAME(FNLV53DS)
020500020905     D DSLV55        E DS                  EXTNAME(FNLV55DS)
020600020905     D DSLR33        E DS                  EXTNAME(FNLR33DS)
020700020930     D TIBS55DS      E DS
020800020905     D DSBS02        E DS                  EXTNAME(TIBS02DS)
020900040811      * ds per il controllo della presenza di CA per la spedizione richiesta
021000040811     d FIDN12DS      e ds
021100040811     d  skKey                 26    305    dim(20)
021200040811     d  skAnn                306    325    dim(20)
021300040811     d  skDca                326    485  0 dim(20)
021400040811     d  skFca                486    545  0 dim(20)
021500040811     d  skDch                546    705  0 dim(20)
021600040811     d  skCch                706    745    dim(20)
021700040811
021800040811     d dsKey           ds
021900040811     d  dsaac                         4  0
022000040811     d  dsfil                         3  0
022100040811     d  dsnca                         7  0
022200040811      *
022300031007     D****
022400031007     D* DS RIDEFINIZIONE CHIAVE X TABELLA "LAN"
022500031007     D****
022600031007     D                 DS
022700040615     D  WD34POS                            lIKE(D34POS)
022800031007     D  WD34NRS                            LIKE(D34NRS)
022900031007     D  DSKEYLAN               1      5
023000020905     D*
023100020905     D* DEFINIZIONE CAMPI
023200020905     D*
023300040505     D WLNP            s                   LIKE(anmlnp) inz
023400020905     D FGSTFP          S                   LIKE(FVVFGS) inz
023500020905     D WLNA            S                   LIKE(FVVFGS) inz
023600020905     D WERR            S                   LIKE(D60ERR) inz
023700020905     D WMB1            S                   LIKE(D60MB1) inz
023800020905     D WMB2            S                   LIKE(D60MB2) inz
023900020905     D SAVFL1          S                   LIKE(ANMFL1) inz
024000020905     D SAVFL2          S                   LIKE(ANMFL2) inz
024100020905     D KTFP            S                   LIKE(CLNTFP) inz
024200020905     D KANN            S                   LIKE(CLNANN) inz
024300020905     D KMES            S                   LIKE(CLNMES) inz
024400020905     D FELDID          S                   LIKE(§7ADID) inz
024500020905     D KDCH            S                   LIKE(ANMDCH) inz
024600020905     D KFIL            S                   LIKE(ANMLNP) inz
024700020905     D KLNP            S                   LIKE(ANMLNP) inz
024800020905     D KLNP9           S                   LIKE(ANMLNP) inz
024900020905     D KLNP92          S                   LIKE(ANMLNP) inz
025000020905     D KFLS            S                   LIKE(ANMLNP) inz
025100020905     D KLNA            S                   LIKE(ANMLNA) inz
025200020905     D SAVLNA          S                   LIKE(ANMLNA) inz
025300020905     D KLNA7           S                   LIKE(ANMLNA) inz
025400020905     D KLNA8           S                   LIKE(ANMLNA) inz
025500020905     D KAIE            S                   LIKE(ANMAIE) inz
025600020905     D KCAA            S                   LIKE(ANMCAA) inz
025700020905     D SAVCAA          S                   LIKE(ANMCAA) inz
025800020905     D WCAA            S                   LIKE(ANMCAA) inz
025900020905     D KAAS2           S                   LIKE(ANMAAS) inz
026000020905     D KLNP2           S                   LIKE(ANMLNP) inz
026100020905     D KNRS2           S                   LIKE(ANMNRS) inz
026200020905     D KNSP2           S                   LIKE(ANMNSP) inz
026300020905     D KDAO            S                   LIKE(ANMDAO) inz
026400020905     D W53DFV          S                   LIKE(D53DFV) inz
026500020905     D SAVNFV          S                   LIKE(ANMNFV) inz
026600020905     D CCHPRT          S                   LIKE(ANMCCH) inz
026700020905     D KCDU            S                   LIKE(ANMCDU) inz
026800020905     D WDDET           S                   LIKE(ARTDET) inz
026900020905     D WDFLP           S                   LIKE(ARTFLP) inz
027000020905     D KNPG            S                   LIKE(BRVNPG) inz
027100020905     D WNFV            S                   LIKE(FVVNFV) inz
027200020905     D WNPG            S                   LIKE(FVVNPG) inz
027300020905     D WSPG            S                   LIKE(FVVSPG) inz
027400020905     D WDFV            S                   LIKE(FVVDFV) inz
027500020905     D WFGS            S                   LIKE(FVVFGS) inz
027600020905     D WMAX            S                   LIKE(TBLKEY) inz
027700021203     D KPDPDR          S                   LIKE(APDpdr)
027800021203     D KPDTIP          S                   LIKE(APDtip)
027900040401     D SAV_dslr34      S                   LIKE(dslr34)
028000040614     D savl1dao        S                   LIKE(anmdao) inz
028100040614     D sav2ldao        S                   LIKE(anmdao) inz
028200040615     D kcaeepa         S                   LIKE(caeepa) inz
028300040615     D kcaetfp         S                   LIKE(caetfp) inz
028400040616     D kcaetfe         S                   LIKE(caetfe) inz
028500040615     D kcaedde         S                   LIKE(caedde) inz
028600040616     D kcaetfa         S                   LIKE(caetfa) inz
028700040401     d wgir200         s              1
028800040616     d wsetgt          s              1
028900160623     d ix              s              3  0
029000040401     d wdta            s               d   datfmt(*Iso)
029100040405     d wdta200         s                   LIKE(D34ALA)
029200041020     D KFOA            S                   LIKE(ANTFOA)
029300041020     D KANLNP          S                   LIKE(ANTLNP) inz(000)
029400960508     D*
029500960508     D* DEFINIZIONE COSTANTI
029600960508     D*
029700960508     D CDISG           C                   CONST(' DISGUIDO A XXX IL')
029800040614     D VVALORE         C                   CONST('V-Valore')
029900960807     D CTRANS          C                   CONST(' TRANSITO A XXX IL')
030000960704     D CDOVE           C                   CONST(' IN XXXXX A XXX IL')
030100040616     D CCOAN           C                   CONST('C.A.XXX/XXXXXXX D-
030200040616     D                                     t')
030300040616     D CCOAN1          C                   CONST('IN C.A.XXX/XXXXXXX D-
030400040616     D                                     EL')
030500040615     D CPO             C                   CONST('PUNTO  OPERATIVO :')
030600040615     D CTERpar         C                   CONST('TERMINAL PARTENZA:')
030700040615     D CTERarr         C                   CONST('TERMINAL  ARRIVO :')
030800050503     D CCPcort         C                   CONST('CP-NO C.Parziale')
030900050503     D CCPlung         C                   CONST('CP-NO Consegna Parzi-
031000050503     D                                     ale')
031100050831     D CGestito        C                   CONST(' Gestito da ')
031200911018     C*--------------------------------------------------------------- *
031300911018     C*                  INDICATORI                                    *
031400911018     C*--------------------------------------------------------------- *
031500911021     C* 01    - ELABORAZIONE DEFINITIVA IDD
031600911021     C* 02    - ANOMALIA SEGNACOLLO
031700911021     C* 03    - ANOMALIA BOLLA
031800040616     C* 04
031900960927     C* 05    - RICHIESTE IN PARTENZA LE ANOMALIE IDD3 CHIUSE
032000000317     C* 06    - CONDIZIONA STAMPA DELLA LINEA DI ARRIVO
032100960927     C* 07    - STAMPA LE ANOMALIE LOCALI
032200960510     C* 08    - ELENCO MANCATA TRASMISSIONE ANOMALIE IDD
032300990121     C* 09    - BOLLA IN CONSEGNA
032400960422     C* 10    - ON - NON STAMPA L'ANOMALIA
032500960419     C* 11    - ON - NON STAMPA MA AUMENTA LID E TRASMETTE
032600960419     C* 12    - ON - NON STAMPA NON AUMENTA LID MA TRASMETTE
032700000317     C* 13    - RICHIESTA IDD A STAMPA
032800960403     C* 14    - RICHIESTO INCLUSIONE/ESCLUSIONE DI UNA ANOMALIA
032900960403     C* 15    - RICHIESTO NUMERO FOGLIO
033000960403     C* 16    - RICHIESTA FASE APERTURA
033100921016     C* 17    - PASSATO IL TIPO ANOMALIA
033200960426     C* 18    - STAMPA DEL LIVELLO IDD
033300960426     C* 19    - RICHIESTA CAUSALE CHIUSURA
033400960508     C* 20    - INDICA SE COLLO PARTITO NON ARRIVATO E' IN DISGUIDO
033500960704     C* 21    - COLLO IN INVENT. O IN CONSEGNA E MANCA RECORD BOLLA ARR
033600970314     C* 22    - CONDIZIONAMENTO IN STAMPA DEL N.SEGNACOLLO PER ANOM.300
033700040614     C* 23    - selezione p.o. segnacollo
033800990114     C* 24/29 - INDICANO QUALE FILE STO LEGGENDO
033900960327     C* 30/31 - LETTURA
034000960327     C* 32    - COMODO
034100990122     C* 40/42 - STAMPO NUMERO C.A. E DATE C.A. SE C'E' SU COLLO
034200050503     C* 43    - stampa particolarità consegne: CP se presente
034300051221     C* 44    - stampa dati mittente e destinatario se richiesto
034400151125     C* 45    - stampa se bolla chiusa con consegna anomala 7
034500040615     C* 70    - COLLI DI VALORE MANCANTI
034600040615     C* 80    - campo d34spo pieno, passati p.o. da stampare
034700040615     C* 95/95 - per allocazione membri
034800040615     C* 97    - ANOMALIA IDD
034900960404     C* 98/99 - PER I FILE DI TRASMISSIONE
035000900828     C*-----------------------------------------------------
035100911015     C                   EXSR      IMPO
035200960416     C*
035300001030     C                   SELECT
035400001030     C* SE SOLO INVIO RICHIAMO ROUTINE DI LETTURA
035500001030     C* DELLE ANOMALIE DA TRASMETTERE CON AZIONE SCHEDULATA
035600960416     C* ELABORO SOLO SE LA DATA ULTIMO IDD E' MINORE DELLA DATA ELABORA
035700001030     C     D34GES        WHENEQ    'V'
035800001030     C                   EXSR      SCHANM
035900001030     C*
036000001030    1C     *IN01         WHENEQ    *OFF
036100990512     C     §7ADID        ORLT      D34ALA
036200000317     C*
036300960416     C*
036400960405     C                   MOVEL     'S'           D34RIL
036500960404     C     RILEG         TAG
036600960404     C                   CLEAR                   D34END
036700960327     C**
036800960327     C* RILEGGO SE MI E' STATO CHIESTO DI RICARICARE
036900960327     C**
037000000317    3C     D34RIL        DOWEQ     'S'
037100960327     C                   CLEAR                   D34RIL
037200960402     C                   CLEAR                   WUSCIT
037300960327     C**
037400960326     C* LEGGO PRIMA LE ANOMALIE PARTENZA
037500960326     C**
037600000317    4C     D34PAR        IFEQ      'P'
037700960412     C     D34PAR        OREQ      ' '
037800980327     C                   MOVEL     'P'           WANOM             1
037900970829     C* SONO LE ANOMALIE CHE USANO SOLO FLE PER ESCUDERLE DAL GIRO
038000970829     C*  DELLA LETTURA PER LNP
038100970829     C                   MOVE      FLM           FLE
038200960412     C                   EXSR      PARTEN
038300970529     C*
038400970529     C* POI ELABORO LE ANOMALIE RICEVUTE PER LE QUALI SONO FILIALE DI
038500970529     C* TRANSITO O DISGUIDO (SOLO SE SONO SIMFEL)
038600970529     C* Potenzialmente questa elabor. sarebbe da fare anche per le anom
038700970529     C* arrivo ma per ora non serve perchè in arrivo riparte sempre
038800970529     C* tutto in automatico
038900980401     C* Solo se le anomalie richieste sono anche idd
039000000317    5C     D34IE         IFEQ      ' '
039100980401     C     D34IE         OREQ      'D'
039200000317     C     D34FGS        IFEQ      SIMFEL
039300970529     C                   EXSR      USACDU
039400000317     C                   END
039500000317    5C                   END
039600970829     C**
039700970829     C* POI  LEGGO  LE ANOMALIE UTILIZZANDO ANMFLE
039800970829     C*  CONTENUTE NELLA SKIERA FLR
039900970829     C**
040000970829     C                   MOVE      FLR           FLE
040100990114     C                   SETON                                        24
040200990114     C                   SETOFF                                       25
040300970829     C                   EXSR      USAFLE
040400000317    4C                   ENDIF
040500960327     C**
040600960327     C* POI  LEGGO  LE ANOMALIE ARRIVO
040700960327     C**
040800000317    4C     D34PAR        IFEQ      'A'
040900960412     C     D34PAR        OREQ      ' '
041000980327     C                   MOVEL     'A'           WANOM             1
041100970829     C* SONO LE ANOMALIE CHE USANO SOLO FLE PER ESCUDERLE DAL GIRO
041200970829     C*  DELLA LETTURA PER LNA
041300970829     C                   MOVE      FLO           FLE
041400960412     C                   EXSR      ARRIVO
041500960412     C**
041600960412     C* POI  LEGGO  LE ANOMALIE UTILIZZANDO ANMFLE
041700970829     C*  CONTENUTE NELLA SKIERA FLT
041800960412     C**
041900961017     C                   MOVE      FLT           FLE
042000990114     C                   SETOFF                                       24
042100990114     C                   SETON                                        25
042200960412     C                   EXSR      USAFLE
042300000317    4C                   ENDIF
042400000317    3C                   ENDDO
042500960327     C*
042600960327     C* FINE LETTURA : IN GESTIONE RICHIAMO LO STESSO IL PGM
042700000317    3C     D34F03        IFEQ      ' '
042800960327     C     D34F12        ANDEQ     ' '
042900000317    4C     D34GES        IFEQ      'I'
043000000317     C     D34GES        OREQ      'G'
043100960327     C                   CLEAR                   DSLR44
043200960327     C                   CLEAR                   D34F03
043300960327     C                   CLEAR                   D34F12
043400960327     C                   CLEAR                   D34RIL
043500960327     C                   MOVEL     'S'           D34END
043600960327     C                   MOVEL     DSLR34        D44LR
043700960521     C                   MOVEL     'G'           D44MOD
043800960327     C*
043900960327     C                   MOVEL     DSLR44        KPJBU
044000960327     C                   CALL      'FNLR44R'
044100960327     C                   PARM                    KPJBA
044200111116     C                   PARM                    dslr34
044300960402     C*
044400960402     C                   MOVEL     KPJBU         DSLR44
044500111116     C***                MOVEL     D44LR         DSLR34
044600960404     C     D34RIL        IFEQ      'S'
044700960412     C                   GOTO      RILEG                                        RILETTURA
044800960404     C                   ENDIF
044900960404     C*
045000000317    4C                   ENDIF
045100000317    3C                   ENDIF
045200040401     C**
045300040401     C* Stampa elenco anomalie 200 aperte fino a data apertura = data idd-1
045400040401     C**
045500040401     c     d34ela        ifne      *blanks
045600040401     c                   clear                   fle
045700040401     C                   MOVE      200           FLE(1)
045800040401     C                   SETOFF                                       24
045900040401     C                   SETON                                        25
046000040401     c* mi salvo la dslr34 prima di sporcarla
046100040401     c                   eval      SAV_dslr34 = dslr34
046200040401     c                   eval      wgir200='S'
046300040401     c                   eval      d34ges='S'
046400040401     c                   eval      d34ie ='I'
046500040401     c                   eval      d34anm = '200'
046600040401     c                   eval      d34inc = 'I'
046700040401     c                   eval      d34ela = *blanks
046800040405     c                   eval      wdta200=d34ala
046900070208     c* Cerco il primo giorno lavorativo
047000070208     C                   Z-ADD     D34ala        WDATA8
047100070208     C                   EXSR      RICDAT
047200070208     C                   Z-ADD     WDATAM        d34ala
047300070208     c
047400070208     c**** *iso          Movel     d34ala        wdta
047500070208     c****               subdur    1:*D          wdta
047600070208     c****               Movel     wdta          D34ala
047700040401     C                   exsr      usafle
047800040401     c                   eval      dslr34 = sav_dslr34
047900040401     c                   eval      wgir200=' '
048000040401     c                   endif
048100960422     C**
048200960422     C* STAMPA SE NESSUNA ANOMALIA RISCONTRATA
048300960422     C**
048400000317    3C     D34GES        IFEQ      'S'
048500960422     C*
048600000317    4C     WSTAMP        IFEQ      ' '
048700040615     c                   if        *inOF
048800040615     C                   EXCEPT    TES
048900040615     C  n70              EXCEPT    TES2
049000040615     C                   EXCEPT    TES3
049100040615     C                   SETOFF                                       OF
049200040615     c                   endif
049300960422     C*
049400960422     C                   EXCEPT    NOANOM
049500000317    4C                   ENDIF
049600960422     C**
049700960422     C* STAMPA ELENCO DI CHI NON HA FATTO L'IDD
049800040615     c*  se non richiamato per altre liste
049900960422     C**
050000040615     c                   if        not *in70
050100040615    4C     D34PAR        IFEQ      'P'
050200960506     C     D34IE         ANDEQ     'D'
050300960423     C     D34ELA        ORNE      ' '
050400960424     C     SIMFEL        ANDEQ     D34FGS
050500960423     C                   EXSR      NOIDD
050600000317    4C                   ENDIF
050700000317    3C                   ENDIF
050800040615    3C                   ENDIF
050900960327     C**
051000960327     C* I N V I O   I D D
051100960327     C**
051200000317    3C     *IN01         IFEQ      *ON
051300000317    4C     W7AIDD        IFEQ      'S'
051400960404     C* CONTROLLO PER TRASMETTERE LE CHIUSURE DELLE ANOMALIE  70 E 71
051500960404     C                   EXSR      CHIUAN
051600960405     C* INVIO FILE CON L'ANOMALIA OK IDD
051700970306     C                   EXSR      CAEX
051800000317     C* PER ELABORAZIONE IDD DEFINITIVA TRASMETTO I SALDI ANOMALIE
051900040927    5C***  D34FGS        IFEQ      SIMFEL
052000040927     C***  D34ALA        ORLE      FELDID
052100040927     C***                CALL      'FNLR38R'
052200040927     C***                PARM                    KPJBA
052300040927    5C***                ENDIF
052400000317    4C                   ENDIF
052500921027     C*
052600960409     C* AGGIORNO DATA ULTIMA ESECUZIONE IDD  CON DATA DEL GIORNO
052700960409     C                   MOVEL     '7A'          COD
052800960409     C                   MOVEL(P)  D34FGS        KEY
052900960409     C     KTAB          CHAIN     TABEL                              3130
053000000317    4C     *IN31         IFEQ      *OFF
053100960409     C     *IN30         ANDEQ     *OFF
053200960409     C                   MOVEL     TBLUNI        DS7AF
053300960409     C                   Z-ADD     D34ALA        §7ADID
053400960409     C                   MOVEL     DS7AF         TBLUNI
053500960409     C                   UPDATE    TABEL
053600000317    4C                   END
053700960409     C*
053800000317    3C                   ENDIF
053900001030     C                   ENDSL
054000000317     C**
054100000320     C**
054200960327     C                   CLEAR                   KPJBU
054300111116     C                   MOVEL     DSLR34        KPJBU
054400000320     C* ULTIMA DISALLOCAZIONE
054500021009     c                   eval      D60po0 = SIMfel
054600000320     C                   MOVEL     SAVFL1        D60FL1
054700000320     C                   MOVEL     SAVFL2        D60FL2
054800000320     C                   MOVEL     AM8GIO        D60DRF
054900000320     C                   MOVEL     'D'           D60AZN
055000000320     C                   MOVEL     'FNANM00T'    D60FIL
055100021009     C                   MOVEL     FNLV60DS      KPJBU
055200000320     C                   CALL      'FNLV60R'
055300000320     C                   PARM                    KPJBA
055400000320     C                   CLEAR                   SAVFL1
055500000320     C                   CLEAR                   SAVFL2
055600970603     C*
055700970603     C* RICHIAMO PGM PER CHIUDERE IN LR
055800970603     C     WCL53         IFEQ      '1'
055900970603     C                   MOVEL     'C'           D53TLA
056000970603     C                   CALL      'FNLV53R'
056100970603     C                   PARM                    DSLV53
056200970603     C                   END
056300971118     C*
056400971118     C                   MOVEL     'C'           D55TLA
056500971118     C                   CALL      'FNLV55R'
056600971118     C                   PARM                    DSLV55
056700020930     C                   CLEAR                   TIBS55DS
056800971125     C                   MOVEL     'C'           I50TLA
056900020930     C                   CALL      'TIBS55R'
057000020930     C                   PARM                    TIBS55DS
057100990122     C                   CLEAR                   DSBS02
057200990122     C                   MOVEL     'C'           T02TLA
057300990122     C                   CALL      'TIBS02R'
057400990122     C                   PARM                    KPJBA
057500990122     C                   PARM                    DSBS02
057600040614     C*
057700040614     c                   clear                   trul09ds
057800040614     C                   MOVEL     'C'           i09TLA
057900040614     C                   CALL      'TRUL09R'
058000040614     C                   PARM                    trul09ds
058100031031     C*
058200031031     C* Se aperto wrk-file WFLAN lo chiudo
058300031031     C                   IF        %open(WFLAN01L)
058400031031     C                   CLOSE     WFLAN01L
058500031031     C                   ENDIF
058600960416     C*
058700000000     C                   SETON                                        LR
058800911018     C*
058900911018     C* IMPOSTO CAMPI FISSI -----------------------------------------*
059000911018     C     IMPO          BEGSR
059100921015     C                   Z-ADD     0             PAGE2             4 0
059200921029     C                   Z-ADD     0             PAGE1             4 0
059300921029     C                   TIME                    UTIME             6 0
059400960604     C                   Z-ADD     0             SAVFL1
059500960403     C*
059600960403     C* SE HO RICHIESTO LA STAMPA APRO I FILE DI STAMPA
059700960403     C     D34GES        IFEQ      'S'
059800000317     C**
059900960403     C                   OPEN      PRTF198
060000960403     C     D34ELA        IFNE      ' '
060100960403     C                   OPEN      PRTF198X
060200960403     C                   ENDIF
060300971120     C**
060400971120     C                   SETON                                        OFOE
060500960604     C**
060600960927     C* DECODIFICO IL P.O. IN GESTIONE
060700040614     c                   if        d34fgs>0
060800960604     C     D34FGS        CHAIN     AZORG01L                           30
060900960604     C  N30              MOVEL     ORGDES        PRTFGS           20
061000040615     C* IMPOSTO LA SCRITTA IN TESTATA
061100040615     C                   MOVEL     CPO           PRTPO            18
061200040614     c                   endif
061300040615     c
061400040615     C* SE VENGO DALLA STAMPA DEI COLLI DI VALORE IMPOSTO
061500040615     C*  INDICATORE 70 E LA SCRITTA IN TESTATA
061600040615    1C                   IF        D34STV='S'
061700040615     C                   SETON                                        70
061800040615     C* SE STAMPA PER TERMINAL, IMPOSTO TERMINAL IN TESTATA
061900040615    2C                   IF        D34SPO<>*BLANKS  and d34spo<>*zeros
062000040615    3C                   IF        D34PAR='A'
062100040615     C                   MOVEL     CTERARR       PRTPO
062200040615     C                   ELSE
062300040615     C                   MOVEL     CTERPAR       PRTPO
062400040615    3C                   ENDIF
062500040615    2C                   ENDIF
062600040615    1C                   ENDIF
062700980402     C**
062800980402     C* DECODIFICO LA LINEA DI ARRIVO SE RICHIESTA
062900980402     C     D34LNA        IFNE      *ZEROS
063000980403     C     D34TRA        ANDNE     'S'
063100980402     C                   SETON                                        06
063200980402     C     D34LNA        CHAIN     AZORG01L                           30
063300040614     C  N30              MOVEL     ORGDES        PRTLNA           16
063400980402     C                   END
063500000317     C                   ENDIF
063600040614     c*
063700040614     c* Selezione p.o. segnacollo
063800040614     c                   if        d34pos>0
063900040614     C                   SETON                                        23
064000040614     C                   ENDIF
064100960327     C*
064200960412     C* CARICO IN UNA SCHIERA TUTTE LE ANOMALIE IDD E LE ANOMALIE
064300960412     C*  DA LEGGERE CON ANMFLE
064400960326     C                   MOVEL     '7C'          COD
064500960412     C                   Z-ADD     0             Y                 3 0
064600961017     C                   Z-ADD     0             T                 3 0
064700970829     C                   Z-ADD     0             U                 3 0
064800970829     C                   Z-ADD     0             V                 3 0
064900960326     C     KTAB2         SETLL     TABEL
065000960409     C     KTAB2         READE(N)  TABEL                                  30
065100960416    1C     *IN30         DOWEQ     *OFF
065200960326     C*
065300960416    2C     TBLFLG        IFEQ      ' '
065400960326     C                   MOVEL     TBLUNI        DS7C
065500960506     C*
065600960412     C* ANOMALIE CHE CONSIDERANO ANMFLE
065700961017     C* §7CUEA='S' --> CONSIDERANO SOLO ANMFLE
065800961017     C* §7CUEA='E' --> CONSIDERANO SIA ANMFLE CHE ANMLNA
065900960416    3C     §7CUEA        IFEQ      'S'
066000960412     C                   ADD       1             Y
066100961017     C                   ADD       1             T
066200961017     C                   MOVEL     TBLKEY        FLO(Y)
066300961023     C                   MOVEL     TBLKEY        FLT(T)
066400961017   X3C                   ELSE
066500961017    4C     §7CUEA        IFEQ      'E'
066600961017     C                   ADD       1             T
066700961023     C                   MOVEL     TBLKEY        FLT(T)
066800961017    4C                   ENDIF
066900960416    3C                   ENDIF
067000970829     C** PARTENZA
067100970829    3C     §7CUEP        IFEQ      'S'
067200970829     C                   ADD       1             U
067300970829     C                   ADD       1             V
067400970829     C                   MOVEL     TBLKEY        FLM(U)
067500970829     C                   MOVEL     TBLKEY        FLR(V)
067600970829   X3C                   ELSE
067700970829    4C     §7CUEP        IFEQ      'E'
067800970829     C                   ADD       1             V
067900970829     C                   MOVEL     TBLKEY        FLR(V)
068000970829    4C                   ENDIF
068100970829    3C                   ENDIF
068200960416    2C                   ENDIF
068300960326     C*
068400960409     C     KTAB2         READE(N)  TABEL                                  30
068500000317    1C                   ENDDO
068600160623     c* Carico le pistola reali L o V
068700160623      /free
068800160623       ix=0;
068900160623       clear sps;
069000160623       setll (1:'7J') tabel00f;
069100160623       reade (1:'7J') tabel00f;
069200160623
069300160623  1    dow    not %eof(tabel00f)  ;
069400160623          ds7j=tbluni             ;
069500160623          if §7jtps = 'L' or §7jtps='C'  ;
069600160623           ix=ix+1;
069700160623           sps(ix)=%dec(%subst(tblkey:1:2):2:0);
069800160623          endif  ;
069900160623          reade (1:'7J') tabel00f  ;
070000160623  1    enddo     ;
070100160623      /end-free
070200160623     c
070300921015     C*
070400960327     C* GIRO DATA FOGLIO SE C'E'
070500960327     C                   MOVE      D34DFV        G02INV
070600960327     C                   MOVEL     '3'           G02ERR
070700971126     C                   CALL      'XSRDA8'
070800960327     C                   PARM                    WLBDAT
070900990513     C                   MOVEL     G02DAT        STADFV            8 0
071000960327     C*
071100960326     C* SE NON IMMESSA DAL AL  = ALLA DATA DAL
071200960326     C     D34ALA        IFEQ      0
071300960403     C                   MOVEL     D34DLA        D34ALA                         DT APERTURA
071400960326     C                   ENDIF
071500960403     C     D34ACA        IFEQ      0
071600960403     C                   MOVEL     D34DCA        D34ACA                         DT CHIUSURA
071700960403     C                   ENDIF
071800960403     C*
071900960403     C* 14 ON - RICHIESTA ALMENO UNA ANOMALIA
072000960403     C     D34ANM        IFNE      *BLANKS
072100960403     C                   SETON                                        14
072200960403     C     D34INC        IFEQ      'I'
072300960403     C                   MOVEL     STA(22)       STARIG           66
072400960403     C                   ELSE
072500960403     C                   MOVEL     STA(23)       STARIG           66
072600960403     C                   ENDIF
072700960403     C                   ENDIF
072800921015     C*
072900921029     C* 15 ON  - IMMESSO NUMERO FOGLIO
073000960326     C     D34NFV        IFGT      0
073100921015     C                   SETON                                        15
073200960403     C                   ENDIF
073300921029     C*
073400921029     C* 16 ON  - IMMESSA FASE APERTURA
073500960326     C     D34FAS        IFNE      ' '
073600921015     C                   SETON                                        16
073700960403     C                   MOVEL     '7D'          COD
073800960403     C                   MOVEL(P)  D34FAS        KEY
073900960409     C     KTAB          CHAIN(N)  TABEL                              32
074000980403     C  N32              MOVEL     TBLUNI        TESFSA           12
074100960403     C  N32              MOVEL     TBLUNI        DESFV             8
074200960403     C                   ENDIF
074300040611     C**                           ì
074400960426     C* 19 ON  - IMMESSA CAUSALE CHIUSURA
074500960426     C     D34CCH        IFNE      ' '
074600960426     C                   SETON                                        19
074700960426     C                   MOVEL     '7E'          COD
074800960426     C                   MOVEL(P)  D34CCH        KEY
074900960426     C     KTAB          CHAIN(N)  TABEL                              32
075000960426     C  N32              MOVEL     TBLUNI        TESCCH           25
075100960426     C                   ENDIF
075200960403     C*
075300960403     C* RICHIESTA DELLE ANOMALIE SOLO APERTE O SOLO CHIUSE
075400960403     C     D34APR        IFEQ      'S'
075500960506     C                   MOVEL     DAN(7)        K51(3)
075600960426     C                   ENDIF
075700960426     C     D34APR        IFEQ      'C'
075800960506     C                   MOVEL     DAN(8)        K51(3)
075900960403     C                   ENDIF
076000040611     c                   if        d34bva='V'
076100040611     C                   MOVEL     vvalore       PRTVAL            8
076200040611     C                   ENDIF
076300921029     C**
076400921029     C* 13 ON  - RICHIESTA I.D.D.
076500921029     C**
076600000317    1C     D34ELA        IFNE      ' '
076700921029     C                   SETON                                        13
076800960403     C* CHAIN A VARIABILI ARRIVI PER VEDERE A QUALE LIVELLO SI CHIUDE
076900960403     C* L'IDD
077000960403     C                   MOVEL     '7A'          COD
077100960403     C                   MOVEL     '2       '    KEY
077200960409     C     KTAB          CHAIN(N)  TABEL                              30
077300960403     C  N30              MOVEL     TBLUNI        DS7A2
077400921029     C*
077500921029     C* 01 ON  - ELABORAZIONE DEFINITIVA I.D.D.
077600000317    2C     D34ELA        IFEQ      'S'
077700921029     C                   SETON                                        01
077800960419     C*
077900960419     C* SE NON SONO SIMFEL VEDO A CHE DATA HA LANCIATO IL 1 LIVELLO
078000000317    3C     D34FGS        IFNE      SIMFEL
078100960419     C                   MOVEL     '7A'          COD
078200960419     C                   MOVEL(P)  SIMFEL        KEY
078300960419     C     KTAB          CHAIN(N)  TABEL                              31
078400960419     C   31              CLEAR                   DS7AF
078500960419     C  N31              MOVEL     TBLUNI        DS7AF
078600960419     C                   MOVE      §7ADID        FELDID
078700000317    3C                   ENDIF
078800921029     C* CHAIN A VARIABILI ARRIVI PER VEDERE SE DEVO INVIARE IDD O NO
078900921029     C                   MOVEL     '7A'          COD
079000921029     C                   MOVEL     '1       '    KEY
079100960409     C     KTAB          CHAIN(N)  TABEL                              30
079200990902     C  N30              MOVEL     TBLUNI        DS7A
079300990902     C   30              CLEAR                   DS7A
079400960417     C*
079500960417     C                   MOVEL     §7AIDD        W7AIDD            1
079600000317     C**
079700000317     C* SE DATA DI ELABORAZIONE <= ALLA DATA IN TABELLA, ESCO SENZA
079800000317     C*  ESEGUIRE IL PGM
079900000317     C                   MOVEL     '7A'          COD
080000000317     C                   MOVEL(P)  D34FGS        KEY
080100000317     C     KTAB          CHAIN(N)  TABEL                              31
080200000317     C   31              CLEAR                   DS7AF
080300000317     C  N31              MOVEL     TBLUNI        DS7AF
080400000317    2C                   ENDIF
080500000317    1C                   ENDIF
080600000317     C**
080700000317     C* D34GES='V' SOLO INVIO, IMPOSTO "S" IN W7AIDD
080800000317     C     D34GES        IFEQ      'V'
080900000317     C                   MOVEL     'S'           W7AIDD
081000000317     C                   ENDIF
081100921029     C*
081200000317    1C     D34GES        IFEQ      'V'
081300000317     C     D34ELA        OREQ      'S'
081400921029     C* SE DEVO INVIARE: CHKOBJ E ADDPF PER TUTTI I MEMBRI
081500000317    2C     W7AIDD        IFEQ      'S'
081600921029     C*
081700021008     C                   MOVE      ')'           VAR               8
081800060619     C                   Z-ADD     49            LUNG             15 5
081900921029     C* MEMBRO RICEZIONE
082000021008     c                   reset                   DSmembro
082100021008     c                   eval      MbrPOP = SIMfel
082200021008     c                   eval      MbrPOA = D34fgs
082300040615     c                   movel(p)  DSmembro      wMBR             10
082400000317    2C                   END
082500000317    1C                   END
082600960506     C**
082700960506     C*  ANOMALIE PARTENZA ARRIVO
082800960506     C**
082900960506     C     D34PAR        IFEQ      'P'
083000960506     C                   MOVEL     DAN(2)        K51(1)
083100960506     C                   ENDIF
083200960506     C     D34PAR        IFEQ      'A'
083300960506     C                   MOVEL     DAN(1)        K51(1)
083400960506     C                   ENDIF
083500921029     C**
083600960506     C* 17 OFF - RICHIESTO SPECIFICO TIPO DI ANOMALIE  INT/IDD/ NON IDD
083700921029     C**
083800960326    1C     D34IE         IFNE      ' '
083900921016     C                   SETON                                        17
084000921016     C* ANOMALIE INTERNE
084100960506     C     D34IE         IFEQ      'I'
084200960506     C                   MOVEL     DAN(5)        K51(2)
084300960327     C                   MOVEL     'I'           EL(1)
084400960403     C                   SETON                                        18
084500960506     C                   ELSE
084600960506     C* ANOMALIE IDD
084700960506    3C     D34IE         IFEQ      'D'
084800960506     C                   MOVEL     DAN(4)        K51(2)
084900960506   X3C                   ELSE
085000960506     C                   MOVEL     DAN(3)        K51(2)
085100960506    3C                   ENDIF
085200960506     C**
085300960506    3C     D34PAR        IFEQ      'P'
085400040116     C****               MOVEL     'R'           EL(1)
085500040116     C                   MOVEL     'E'           EL(1)
085600960506     C                   ELSE
085700960506     C                   MOVEL     'E'           EL(1)
085800960506    3C                   ENDIF
085900960506    2C                   ENDIF
086000960506     C*
086100960506   X1C                   ELSE
086200960506     C                   MOVEL     DAN(6)        K51(2)
086300960326    1C                   ENDIF
086400960506     C                   MOVEA     K51           DESIE            50
086500961018     C***
086600961018     C* CARICO £1
086700961018     C***
086800961104     C                   CLEAR                   DSUL06
086900961018     C                   MOVE      '£1'          D06COD
087000961018     C                   MOVEL     *BLANKS       D06TLA
087100961104     C                   MOVEL     SIMFEL        D06KEY
087200961018     C                   MOVEL     DSUL06        KPJBU
087300961018     C                   CALL      'TRUL06R'
087400961018     C                   PARM                    KPJBA
087500961018     C                   MOVEL     KPJBU         DSUL06
087600961018     C                   MOVEA     LIN           L1
087700960326     C*
087800960927     C* CALCOLO DATA -1 SE RICHIESTE IN PARTENZA LE ANOMALIE IDD
087900960927    1C     D34IE         IFEQ      'D'
088000990512     C     D34DCA        ANDGT     0
088100960927     C*
088200960927    2C     D34PAR        IFEQ      'P'
088300960927     C     D34PAR        OREQ      ' '
088400960927     C* SE IDD CHIUSE O CON CAUSALE PR
088500960927    3C     D34APR        IFEQ      'C'
088600960927     C     D34CCH        OREQ      'PR'
088700960927     C                   SETON                                        05
088800960927     C*
088900971126     C                   Z-ADD     D34DCA        WDATA8
089000960927     C                   EXSR      RICDAT
089100990512     C                   Z-ADD     WDATAM        W08DM1            8 0
089200960927     C*
089300971126    4C     D34ACA        IFGT      0
089400971126     C                   Z-ADD     D34ACA        WDATA8
089500960927     C                   EXSR      RICDAT
089600990512     C                   Z-ADD     WDATAM        W08AM1            8 0
089700960927    4C                   ENDIF
089800960927    3C                   ENDIF
089900960927    2C                   ENDIF
090000960927    1C                   ENDIF
090100960927     C*
090200960927     C* DETERMINO IN UNA SCHIERA I P.O. CHE FANNO L'IDD IL GIORNO DOPO
090300960927    1C     D34PAR        IFEQ      'P'
090400960927     C     D34PAR        OREQ      ' '
090500960927    2C     D34IE         IFNE      'I'
090600960927     C                   Z-ADD     1             L
090700960927     C     *LOVAL        SETLL     AZORG01L
090800960927     C                   READ      AZORG01L                               30
090900960927     C*
091000960927    3C     *IN30         DOWEQ     *OFF
091100960927     C* ESCLUDO LE ANNULLATE
091200960927    4C     ORGFVA        IFEQ      ' '
091300960927     C* SOLO FILIALE E AGENZIA
091400960927    5C     ORGFAG        IFEQ      'F'
091500960927     C     ORGFAG        OREQ      'A'
091600960927     C                   MOVEL     ORGDE8        OG148
091700960927     C**
091800960927    6C     §OGEID        IFNE      ' '
091900960927     C                   Z-ADD     ORGFIL        FM1(L)                         GIORNO PREC
092000960927     C                   ADD       1             L
092100960927    6C                   ENDIF
092200960927    5C                   ENDIF
092300960927    4C                   ENDIF
092400960927     C*
092500960927     C                   READ      AZORG01L                               30
092600960927    3C                   ENDDO
092700960927     C*
092800960927    2C                   ENDIF
092900960927    1C                   ENDIF
093000971126     C***
093100040614     C* CARICO LA L6 DEI P.O. IN GESTIONE  --> p.o. da elaborare solo se
093200040614     c*        schiera p.o. passata vuota
093300971126     C***
093400040614    0c                   if        d34spo=*blanks or d34spo=*zeros
093500971126     C                   CLEAR                   DSUL06
093600971126     C                   MOVE      '£6'          D06COD
093700971126     C                   MOVEL     'L'           D06TLA
093800971126     C                   MOVEL     D34FGS        D06KEY
093900971126     C                   MOVEL     DSUL06        KPJBU
094000971126     C                   CALL      'TRUL06R'
094100971126     C                   PARM                    KPJBA
094200971126     C                   MOVEL     KPJBU         DSUL06
094300971126     C                   MOVEA     LIN           L6
094400971126     C*
094500971126     C* VEDO SE IL P.O. E' UN TERMINAL DI PARTENZA E SE HA DEI P.O.
094600050906     C* ESTERI O D.P.D. o la 101 resi --> LI CARICO
094700971126     C                   CLEAR                   DSLV55
094800971126     C                   MOVEL     'P'           D55TPT
094900971126     C                   MOVEL     D34FGS        D55LIN
095000980403     C                   MOVEL     AM8GIO        D55DRF
095100971126     C                   CALL      'FNLV55R'
095200971126     C                   PARM                    DSLV55
095300971126     C     D55ERR        IFNE      ' '
095400971126     C                   MOVEL     D55LIN        D55TFP
095500971126     C                   ENDIF
095600971126     C                   MOVEL     D55TFP        FGSTFP
095700971126     C*§
095800971126    1C     D34FGS        IFEQ      D55TFP
095900971126     C                   Z-ADD     1             L
096000971126     C                   CLEAR                   W0030
096100971126     C     W0030         LOOKUP    L6(L)                                  30
096200971126     C*
096300971126     C     *LOVAL        SETLL     AZORG01L
096400971126     C                   READ      AZORG01L                               30
096500971126     C*
096600971126    2C     *IN30         DOWEQ     *OFF
096700991217    3C     ORGFAG        IFEQ      'F'
096800991217     C     ORGFAG        OREQ      'A'
096900971126     C* P.O. NON ANNULLATO
097000991217    4C     ORGFVA        IFEQ      ' '
097100991217     C                   MOVEL     ORGDE3        OG143
097200991217     C*
097300020221    5C     §OGNTW        IFEQ      'EEX'                                        ESTERA
097400020221     C     §OGNTW        OREQ      'EUP'
097500020221     C     §OGNTW        OREQ      'FED'
097600020221     C     §OGNTW        OREQ      'DPD'                                        DPD
097700050906     c* anche la 101 resi la tratto come estero: caricata col terminal
097800050906     C     orgfil        OREQ      101                                          DPD
097900971126     C*
098000971126     C* VEDO SE HA COME TERMINAL DI PARTENZA LA FIL.GESTIONE
098100971126     C                   CLEAR                   DSLV55
098200971126     C                   MOVEL     'P'           D55TPT
098300971126     C                   MOVEL     ORGFIL        D55LIN
098400980403     C                   MOVEL     AM8GIO        D55DRF
098500971126     C                   CALL      'FNLV55R'
098600971126     C                   PARM                    DSLV55
098700991217    6C     D55ERR        IFNE      ' '
098800971126     C                   MOVEL     D55LIN        D55TFP
098900991217    6C                   ENDIF
099000991217    6C     D55TFP        IFEQ      D34FGS
099100971126     C                   Z-ADD     ORGFIL        L6(L)
099200971126     C                   ADD       1             L
099300991217    6C                   ENDIF
099400991217    5C                   ENDIF
099500971126    4C                   ENDIF
099600971126    3C                   ENDIF
099700971126     C*
099800971126     C                   READ      AZORG01L                               30
099900971126    2C                   ENDDO
100000971126     C*
100100971126    1C                   ENDIF
100200040614   x0c                   else
100300040617     c                   z-add     1             xx
100400040617     c                   z-add     1             yy                3 0
100500040617     c                   do        30            xx
100600040617     c                   if        spo(xx)>*zeros
100700040617     c* imposto schiera di elaborazione
100800040617     c                   movel     spo(xx)       l6(yy)
100900040617     c* imposto schiera di stampa alfanumerica
101000040617     c                   movel     spo(xx)       stpo(yy)
101100040617     c                   add       1             yy
101200040615     c                   seton                                        80
101300040617     c                   endif
101400040615     c                   enddo
101500040617     c
101600040614     c                   endif
101700960927     C*
101800911018     C                   ENDSR
101900960412     C*
102000960412     C* LEGGO LE ANOMALIE IN PARTENZA --------------------------------*
102100960412     C     PARTEN        BEGSR
102200960412     C                   MOVEL     'N'           WFLE              1
102300960412     C*
102400960412     C* IMPOSTO LE ANOMALIE CHE DEVO LEGGERE
102500960412     C*
102600960412    2C     D34IE         IFEQ      ' '
102700960412     C                   CLEAR                   EL
102800040116     C**                 MOVEL     'R'           EL(1)
102900040116     C                   MOVEL     'E'           EL(1)
103000960412     C                   MOVEL     'I'           EL(2)
103100960412    2C                   ENDIF
103200960412     C**
103300960412     C                   Z-ADD     1             L                 3 0
103400960412    2C     L6(L)         DOWGT     0
103500960412     C     WUSCIT        ANDEQ     ' '
103600960412     C**
103700960412     C                   MOVEL     L6(L)         KFIL
103800960412     C**
103900960412     C                   Z-ADD     1             E                 3 0
104000960412    3C     EL(E)         DOWNE     ' '
104100960412     C     WUSCIT        ANDEQ     ' '
104200960412     C                   MOVEL     EL(E)         KAIE
104300970217     C                   Z-SUB     1             KLNA
104400960412     C                   SETOFF                                       30
104500960412     C**
104600960412    4C     *IN30         DOWEQ     *OFF
104700960412     C     WUSCIT        ANDEQ     ' '
104800990512     C     KANM7         SETGT     FNANM07L
104900990512     C     KANM71        READE     FNANM07L                               30
105000960412     C   30              MOVEL     999           KLNA7
105100960412     C  N30              MOVEL     ANMLNA        KLNA7
105200960412     C*
105300960412     C                   CLEAR                   KLNP
105400990512     C     KANM8         SETGT     FNANM08L
105500990512     C     KANM9         READE     FNANM08L                               30
105600960412     C   30              MOVEL     999           KLNA8
105700960412     C  N30              MOVEL     ANMLNA        KLNA8
105800960412     C*
105900960412    5C     KLNA7         IFNE      999
106000960412     C     KLNA8         ORNE      999
106100960412     C                   SETOFF                                       30
106200960412     C** TROVATA LA LNA
106300960412    6C     KLNA7         IFLT      KLNA8
106400960412     C                   MOVEL     KLNA7         KLNA
106500960412     C                   ELSE
106600960412     C                   MOVEL     KLNA8         KLNA
106700960412    6C                   ENDIF
106800960927     C*
106900960927     C* SE STO STAMPANDO LE RICEVUTE, VEDO SE STAMPARE LE LOCALI
107000040116     C********           EXSR      LOCALI
107100960412     C**
107200040116     C**** WUSCIT        IFEQ      ' '
107300960927     C                   EXSR      LEGPAR
107400040116     C****               ENDIF
107500960412     C**
107600960927   X5C                   ELSE
107700960412     C                   SETON                                        30
107800990518     C                   Z-ADD     999           KLNA
107900990518     C*
108000990518     C* SE STO STAMPANDO LE RICEVUTE, VEDO SE STAMPARE LE LOCALI
108100040116     C*****              EXSR      LOCALI
108200960412    5C                   ENDIF
108300960412    4C                   ENDDO
108400960412     C*
108500960412     C                   ADD       1             E
108600960412    3C                   ENDDO
108700960412     C*
108800960412     C                   ADD       1             L
108900960412    2C                   ENDDO
109000960412     C                   ENDSR
109100960927     C*
109200960927     C* LETTURA ANOMALIE PER LA PARTENZA------------------------------*
109300960927     C     LEGPAR        BEGSR
109400960927    6C                   SELECT
109500960927     C* SOLO ANOMALIE APERTE
109600961218     C*  SOLTANTO SE NON STO LEGGENDO LE LOCALI PERCHE' ALTRIMENTI
109700961218     C*  NON RIUSCIREI A LEGGERE LE ANOMALIE 70 E 71 CHE SONO
109800961218     C*  CHIUSE VIRUTALMENTE MA NON DI FATTO (MANCA SOLO AL CASUALE CH)
109900040116     C**   D34APR        WHENEQ    'S'
110000040116     C**   *IN07         ANDEQ     *OFF
110100040116     C**                 CLEAR                   KDCH
110200040116     C**   KANM72        SETLL     FNANM07L
110300040116     C**   KANM72        READE     FNANM07L                               31
110400960927     C* SOLO ANOMALIE CHIUSE
110500960927     C     D34APR        WHENEQ    'C'
110600960927     C                   Z-ADD     1             KDCH
110700990512     C     KANM72        SETLL     FNANM07L
110800990512     C     KANM7         READE     FNANM07L                               31
110900960927   X6C                   OTHER
111000990512     C     KANM7         SETLL     FNANM07L
111100990512     C     KANM7         READE     FNANM07L                               31
111200960927    6C                   ENDSL
111300960927     C**
111400960927     C                   SETON                                        27
111500960927     C                   SETOFF                                       252829
111600990114     C                   SETOFF                                       2624
111700960927    6C     *IN31         DOWEQ     *OFF
111800960927     C**
111900960927     C                   EXSR      ELASTA                                        STAMPA
112000960927     C** RILEGGO DA CAPO
112100960927    7C     WUSCIT        IFEQ      ' '
112200960927     C*
112300040116    8C**   D34APR        IFEQ      'S'
112400040116     C**   *IN07         ANDEQ     *OFF
112500040116     C**   KANM72        READE     FNANM07L                               31     ANOM APERTE
112600040116   X8C**                 ELSE
112700990512     C     KANM7         READE     FNANM07L                               31     TUTTE O CHI
112800040116    8C**                 ENDIF
112900960927    7C                   ENDIF
113000960927    6C                   ENDDO
113100960927     C*
113200960927     C* NUOVO CICLO LEGGENDO CON LNP = 0 E FLS = LNP LETTA
113300960927   6AC     WUSCIT        IFEQ      ' '
113400960927     C                   CLEAR                   KLNP
113500960927    6C                   SELECT
113600960927     C* SOLO ANOMALIE APERTE
113700040116     C**   D34APR        WHENEQ    'S'
113800040116     C**   *IN07         ANDEQ     *OFF
113900040116     C**                 CLEAR                   KDCH
114000040116     C**   KANM82        SETLL     FNANM08L
114100040116     C**   KANM82        READE     FNANM08L                               31
114200960927     C* SOLO ANOMALIE CHIUSE
114300960927     C     D34APR        WHENEQ    'C'
114400960927     C                   Z-ADD     1             KDCH
114500990512     C     KANM82        SETLL     FNANM08L
114600990512     C     KANM8         READE     FNANM08L                               31
114700960927   X6C                   OTHER
114800990512     C     KANM8         SETLL     FNANM08L
114900990512     C     KANM8         READE     FNANM08L                               31
115000960927    6C                   ENDSL
115100960927     C**
115200960927     C                   SETON                                        28
115300960927     C                   SETOFF                                       252729
115400990114     C                   SETOFF                                       2624
115500960927    6C     *IN31         DOWEQ     *OFF
115600960927     C**
115700960927     C                   EXSR      ELASTA                                        STAMPA
115800960927     C**
115900960927    7C     WUSCIT        IFEQ      ' '
116000960927     C*
116100040116    8C**   D34APR        IFEQ      'S'
116200040116     C**   *IN07         ANDEQ     *OFF
116300040116     C**   KANM82        READE     FNANM08L                               31     ANOM APERTE
116400040116   X8C**                 ELSE
116500990512     C     KANM8         READE     FNANM08L                               31     TUTTE O CHI
116600040116    8C**                 ENDIF
116700960927    7C                   ENDIF
116800960927     C*
116900960927    6C                   ENDDO
117000960927   6AC                   ENDIF
117100960927     C                   ENDSR
117200960412     C*
117300960412     C* LEGGO LE ANOMALIE IN ARRIVO ----------------------------------*
117400960412     C     ARRIVO        BEGSR
117500960412     C                   MOVEL     'N'           WFLE              1
117600960412     C*
117700960412     C* IMPOSTO LE ANOMALIE CHE DEVO LEGGERE
117800960412    2C     D34IE         IFEQ      ' '
117900960412     C                   CLEAR                   EL
118000960412     C                   MOVEL     'E'           EL(1)
118100960412     C                   MOVEL     'I'           EL(2)
118200960412    2C                   ENDIF
118300960412     C**
118400960412     C                   Z-ADD     1             L                 3 0
118500960412    2C     L6(L)         DOWGT     0
118600960412     C     WUSCIT        ANDEQ     ' '
118700960412     C**
118800960412     C                   MOVEL     L6(L)         KFIL
118900960412     C**
119000960412     C                   Z-ADD     1             E                 3 0
119100960412    3C     EL(E)         DOWNE     ' '
119200960412     C     WUSCIT        ANDEQ     ' '
119300960412     C                   MOVEL     EL(E)         KAIE
119400960412     C                   CLEAR                   KLNP
119500960412     C                   SETOFF                                       30
119600960412     C**
119700960412    4C     *IN30         DOWEQ     *OFF
119800960412     C     WUSCIT        ANDEQ     ' '
119900960412     C                   CLEAR                   KFLS
120000990512     C     KANM9         SETGT     FNANM09L
120100990512     C     KANM71        READE     FNANM09L                               30
120200960412     C   30              MOVEL     999           KLNP9
120300960412     C  N30              MOVEL     ANMLNP        KLNP9
120400960412     C*
120500960412     C                   MOVEL     KLNP          KFLS
120600960412     C                   CLEAR                   KLNP
120700990512     C     KANM94        SETGT     FNANM09L
120800990512     C     KANM9         READE     FNANM09L                               30
120900960412     C   30              MOVEL     999           KLNP92
121000960412     C  N30              MOVEL     ANMFLS        KLNP92
121100960412     C                   CLEAR                   KFLS
121200960412     C*
121300960412    5C     KLNP9         IFNE      999
121400960412     C     KLNP92        ORNE      999
121500960412     C                   SETOFF                                       30
121600960412     C** TROVATA LA LNP
121700960412     C     KLNP9         IFLT      KLNP92
121800960412     C                   MOVEL     KLNP9         KLNP
121900960412     C                   ELSE
122000960412     C                   MOVEL     KLNP92        KLNP
122100960412     C                   ENDIF
122200960412     C**
122300990512     C     KANM9         SETLL     FNANM09L
122400990512     C     KANM9         READE     FNANM09L                               31
122500960412     C**
122600960412     C                   SETON                                        29
122700960412     C                   SETOFF                                       252728
122800990114     C                   SETOFF                                       2624
122900960412    6C     *IN31         DOWEQ     *OFF
123000960412     C**
123100960412     C                   EXSR      ELASTA                                        STAMPA
123200960412     C**
123300960412     C     WUSCIT        IFEQ      ' '
123400990512     C     KANM9         READE     FNANM09L                               31     TUTTE O CHI
123500960412     C                   ENDIF
123600960412    6C                   ENDDO
123700960412     C*
123800960412     C* NUOVO CICLO LEGGENDO CON LNP = 0 E FLS = LNP LETTA
123900960412   6AC     WUSCIT        IFEQ      ' '
124000960412     C                   MOVEL     KLNP          KFLS
124100960412     C                   CLEAR                   KLNP
124200960412    6C                   SELECT
124300960419     C* SOLO ANOMALIE APERTE (SE NO CONFERMA IDD)
124400960412     C     D34APR        WHENEQ    'S'
124500960419     C     D34ELA        ANDNE     'S'
124600960412     C                   CLEAR                   KDCH
124700990512     C     KANM93        SETLL     FNANM09L
124800990512     C     KANM93        READE     FNANM09L                               31
124900960419     C* SOLO ANOMALIE CHIUSE  (SE NO CONFERMA IDD)
125000960412     C     D34APR        WHENEQ    'C'
125100960419     C     D34ELA        ANDNE     'S'
125200960412     C                   Z-ADD     1             KDCH
125300990512     C     KANM93        SETLL     FNANM09L
125400990512     C     KANM94        READE     FNANM09L                               31
125500960412   X6C                   OTHER
125600990512     C     KANM94        SETLL     FNANM09L
125700990512     C     KANM94        READE     FNANM09L                               31
125800960412    6C                   ENDSL
125900960412     C**
126000960412    6C     *IN31         DOWEQ     *OFF
126100960412     C**
126200960412     C                   EXSR      ELASTA                                        STAMPA
126300960412     C**
126400960412    7C     WUSCIT        IFEQ      ' '
126500960412     C*
126600960412    8C     D34APR        IFEQ      'S'
126700960423     C     D34ELA        ANDNE     'S'
126800990512     C     KANM93        READE     FNANM09L                               31     ANOM APERTE
126900960412   X8C                   ELSE
127000990512     C     KANM94        READE     FNANM09L                               31     TUTTE O CHI
127100960412    8C                   ENDIF
127200960412    7C                   ENDIF
127300960412     C*
127400960412    6C                   ENDDO
127500960412   6AC                   ENDIF
127600960412     C**
127700960412     C* REIMPOSTO IL CAMPO DELLA LINEA DI PARTENZA
127800960412     C                   MOVEL     KFLS          KLNP
127900960412     C                   CLEAR                   KFLS
128000960412     C                   ELSE
128100960412     C                   SETON                                        30
128200960412    5C                   ENDIF
128300960412    4C                   ENDDO
128400960412     C*
128500960412     C                   ADD       1             E
128600960412    3C                   ENDDO
128700960412     C*
128800960412     C                   ADD       1             L
128900960412    2C                   ENDDO
129000960412     C                   ENDSR
129100960412     C*
129200960412     C* LEGGO LE ANOMALIE CON ANMFLE ---------------------------------*
129300960412     C     USAFLE        BEGSR
129400960412     C                   MOVEL     'S'           WFLE              1
129500960412     C                   Z-ADD     1             Y
129600960412     C**
129700960412    1C     FLE(Y)        DOWGT     0
129800960412     C     WUSCIT        ANDEQ     ' '
129900960412     C                   MOVEL     FLE(Y)        KCAA
130000960412     C                   CLEAR                   KFIL
130100960412     C                   SETOFF                                       30
130200960523     C*
130300960508     C* VERIFICO SE L'ANOMALIA E' SECONDO I PARAMETRI RICHIESTI
130400960508     C                   Z-ADD     KCAA          ANMCAA
130500960508     C                   EXSR      LEGCAA
130600960523     C*
130700960508     C* RICHIESTE LE ANOMALIE INTERNE
130800960508     C     D34IE         IFEQ      'I'
130900960508     C     §7CAIE        ANDNE     'I'
131000960508     C                   SETON                                        30
131100960508     C                   ENDIF
131200960523     C**
131300960523     C* RICHIESTE LE ANOMALIE NON IDD
131400960523     C     D34IE         IFEQ      'N'
131500960523     C     §7CIDD        ANDEQ     'S'
131600060428     C     d34ie         oreq      'N'
131700060428     C     §7CAIE        ANDNE     'E'
131800960523     C                   SETON                                        30
131900960523     C                   ENDIF
132000960508     C**
132100960508     C* RICHIESTE LE ANOMALIE IDD
132200960523    1C     D34IE         IFEQ      'D'
132300960508     C     §7CIDD        ANDNE     'S'
132400960523     C*  SE E' LA CONFERMA DELL'IDD LE DEVO ELABORARE COMUNQUE
132500960523    2C     D34ELA        IFNE      'S'
132600960508     C                   SETON                                        30
132700960523   X2C                   ELSE
132800960523    3C     §7CAIE        IFNE      'E'                                          ESTERNA NO I
132900960523     C                   SETON                                        30
133000960523    3C                   ENDIF
133100960523    2C                   ENDIF
133200960523    1C                   ENDIF
133300960412     C**
133400960417    2C     *IN30         DOWEQ     *OFF
133500990512     C     KANM5         SETGT     FNANM10L
133600990512     C     KCAA          READE     FNANM10L                               30
133700960412    3C     *IN30         IFEQ      *OFF
133800960412     C                   MOVEL     ANMLNA        KFIL
133900960412     C                   CLEAR                   KLNP
134000960412     C                   CLEAR                   KFLS
134100960412     C                   SETOFF                                       31
134200960412     C*
134300960412    4C     *IN31         DOWEQ     *OFF
134400960412     C     WUSCIT        ANDEQ     ' '
134500990512     C     KANM51        SETGT     FNANM10L
134600990512     C     KANM5         READE     FNANM10L                               31
134700960412     C  N31              MOVEL     ANMLNP        KLNP9
134800960412     C   31              MOVEL     999           KLNP9
134900960412     C*
135000960412     C                   MOVEL     KLNP          KFLS
135100960412     C                   CLEAR                   KLNP
135200990512     C     KANM52        SETGT     FNANM10L
135300990512     C     KANM51        READE     FNANM10L                               31
135400960412     C  N31              MOVEL     ANMFLS        KLNP92
135500960412     C   31              MOVEL     999           KLNP92
135600960412     C*
135700960412    5C     KLNP9         IFNE      999
135800960416     C     KLNP92        ORNE      999
135900960412     C** TROVATA LA LNP
136000960412    6C     KLNP9         IFLT      KLNP92
136100960412     C                   MOVEL     KLNP9         KLNP
136200960412     C                   ELSE
136300960412     C                   MOVEL     KLNP92        KLNP
136400960412    6C                   ENDIF
136500960412     C**
136600990512     C     KANM51        SETLL     FNANM10L
136700990512     C     KANM51        READE     FNANM10L                               31
136800960412     C**
136900960412     C                   SETOFF                                       272829
137000970529     C                   SETOFF                                       26
137100960412    6C     *IN31         DOWEQ     *OFF
137200960412     C**
137300960412     C                   EXSR      ELASTA                                        STAMPA
137400960412     C**
137500960412    7C     WUSCIT        IFEQ      ' '
137600990512     C     KANM51        READE     FNANM10L                               31     TUTTE O CHI
137700960412    7C                   ENDIF
137800960412    6C                   ENDDO
137900960412     C*
138000960412     C* NUOVO CICLO LEGGENDO CON LNP = 0 E FLS = LNP LETTA
138100960412    6C     WUSCIT        IFEQ      ' '
138200960412     C                   MOVEL     KLNP          KFLS
138300960412     C                   CLEAR                   KLNP
138400990512     C     KANM52        SETLL     FNANM10L
138500990512     C     KANM52        READE     FNANM10L                               31
138600960412     C**
138700960412    7C     *IN31         DOWEQ     *OFF
138800960412     C**
138900960412     C                   EXSR      ELASTA                                        STAMPA
139000960412     C**
139100960412    8C     WUSCIT        IFEQ      ' '
139200960412     C*
139300990512     C     KANM52        READE     FNANM10L                               31     TUTTE O CHI
139400960412    8C                   ENDIF
139500960412     C*
139600960412    7C                   ENDDO
139700960412    6C                   ENDIF
139800960412     C**
139900960412     C* REIMPOSTO IL CAMPO DELLA LINEA DI PARTENZA
140000960412     C                   MOVEL     KFLS          KLNP
140100960412     C                   CLEAR                   KFLS
140200960412     C                   SETOFF                                       31
140300960412   X5C                   ELSE
140400960412     C                   SETON                                        31
140500960412    5C                   ENDIF
140600960412    4C                   ENDDO
140700960412    3C                   ENDIF
140800960412    2C                   ENDDO
140900960412     C*
141000960412     C                   ADD       1             Y
141100960412    1C                   ENDDO
141200960412     C*
141300960412     C                   ENDSR
141400970529     C*
141500040614     C* LEGGO LE ANOMALIE CON ANMCDU solo in partenza ----------------*
141600970529     C     USACDU        BEGSR
141700970529     C                   MOVEL     'N'           WFLE              1
141800040615     C                   MOVEL     'S'           Wcdu              1
141900970529     C                   SETOFF                                       252728
142000990114     C                   SETOFF                                       2924
142100970529     C                   SETON                                        26
142200970529     C* LEGGO TUTTE LE ANOMALIE RICEVUTE CON CDU=SIMFEL SCARTANDO
142300040616     C* QUELLE CON LNP O FLS PRESENTI IN £1(lo scato avviene nella
142400040616     c*        exsr CONFR)
142500970529     C****
142600970529     C* 1°CICLO: LEGGO CON LNP > 0 E <> £1
142700970529     C****
142800040116     C                   MOVEL     'E'           KAIE
142900970529     C                   Z-ADD     SIMFEL        KCDU
143000970529     C                   Z-ADD     1             ANMLNP
143100040616     c
143200970529    0C     WUSCIT        IFEQ      *BLANKS
143300990512     C     KANM3         SETLL     FNANM03L
143400990512     C     KANM3A        READE     FNANM03L                               30
143500970529    1C     *IN30         DOWEQ     *OFF
143600040615     c                   eval      wsetgt='N'
143700040615     c
143800040429     C                   EXSR      ELASTA
143900040615     c*
144000040615     c                   if        wsetgt='S'
144100040615     C     KANM3         SETGT     FNANM03L
144200040429     c                   endif
144300040615     c
144400970529    2C     WUSCIT        IFEQ      *BLANKS
144500990512     C     KANM3A        READE     FNANM03L                               30
144600970529    2C                   END
144700970529    1C                   ENDDO
144800970529     C****
144900970529     C* 2°CICLO: LEGGO CON LNP = 0
145000970529     C*                 ED FLS > 0 E <> £1
145100970529     C****
145200970529   1AC     WUSCIT        IFEQ      *BLANKS
145300970529     C                   MOVE      *ZEROS        KLNP
145400970529     C                   Z-ADD     1             ANMFLS
145500990512     C     KANM3B        SETLL     FNANM03L
145600990512     C     KANM3D        READE     FNANM03L                               30
145700970529    1C     *IN30         DOWEQ     *OFF
145800040616     c                   eval      wsetgt='N'
145900040616     c
146000040429     C                   EXSR      ELASTA
146100040616     c
146200040616     C* Fls in £1 --> Scarto e mi posiziono su nuova Fls
146300040616    2c                   if        wsetgt='S'
146400990512     C     KANM3B        SETGT     FNANM03L
146500970529    2C                   END
146600040616    c
146700970529    2C     WUSCIT        IFEQ      *BLANKS
146800990512     C     KANM3D        READE     FNANM03L                               30
146900040616    2C                   END
147000970529    1C                   ENDDO
147100970529   1AC                   END
147200970529    0C                   END
147300040615     C                   MOVEL     'N'           Wcdu              1
147400970529     C                   ENDSR
147500960327     C*
147600960523     C* ELABORO I DATI DELLE ANOMALIE PER LA STAMPA -----------------*
147700960327     C     ELASTA        BEGSR
147800960327     C*
147900960327     C* CONFRONTO SELEZIONI FATTE
148000960327     C                   EXSR      CONFR
148100960327     C**
148200960327    1C     *IN10         IFEQ      *ON
148300960327     C**
148400960327     C* SE ELABORAZIONE DEFINITIVA IDD, INVIO ANCHE LE ANOMALIE
148500960705     C*  CHE CI SONO DA TRASMETTERE
148600960327    2C     *IN01         IFEQ      *ON
148700960419     C     *IN12         ANDEQ     *ON
148800960327    3C     ANMFT1        IFEQ      ' '
148900960412     C     ANMFL1        ANDGT     0
149000960327     C     ANMFT2        OREQ      ' '
149100960412     C     ANMFL2        ANDGT     0
149200960705     C                   CLEAR                   WTRM
149300960827     C* SERIE > 0
149400960827    4C     ANMNRS        IFGT      *ZEROS
149500960827     C**SE ANOMALIA 55 ED ESISTE BOLLA PARTENZA NON LA TRASMETTO E
149600960827     C* LA CHIUDO. CHIUDO ANCHE LA 200
149700960827     C* (SE BOLLA DA ME CONFERMABILE E CONFERMABILE ANCHE DA ALTRI AS)
149800960827     C**STESSA COSA SE ANOMALIA 50 (SENZA CHIUDERE LA 200)
149900960827     C     ANMCAA        IFEQ      50
150000960827     C     ANMCAA        OREQ      55
150100990409     C     ANMCAA        OREQ      56
150200960827     C                   EXSR      A50A55
150300960827     C                   ENDIF
150400960827    4C                   ENDIF
150500960827     C*
150600960705    4C     WTRM          IFEQ      ' '
150700990114     C   24
150800990512     COR 25ANMN10        CHAIN     FNANM000                           33
150900000306     C   26ANMNR3        CHAIN     FNANM000                           33
151000000306     C   27ANMNR7        CHAIN     FNANM000                           33
151100990512     C   28ANMNR8        CHAIN     FNANM000                           33
151200990512     C   29ANMNR9        CHAIN     FNANM000                           33
151300960404     C*
151400960327     C                   EXSR      ELAB
151500960705    4C                   ENDIF
151600960327    3C                   ENDIF
151700960327    2C                   ENDIF
151800960327     C**
151900960327   X1C                   ELSE
152000960327     C* 10 OFF - RECORD SELEZIONABILE
152100960327     C                   CLEAR                   WUSCIT
152200980327     C*** TRASFERIMENTO DA FILIALE A FILIALE
152300980327     C**
152400040615     C**   D34TRA        IFEQ      'S'
152500040615     C**                 EXSR      TRASFE
152600040615     C**                 END
152700960327     C**
152800960327     C** G E S T I O N E
152900000317    2C     D34GES        IFEQ      'I'
153000000317     C     D34GES        OREQ      'G'
153100960327     C* RICHIAMO IL PGM FNLR44R PASSANDOGLI IL RECORD DELL'ANOMALIA
153200960327     C*  IL NUMERO RELATIVO DI RECORD
153300960327     C*  LA DS DEI PARAMETRI RICHIESTI
153400960515     C* LO RICHIAMO SOLO SE IL SOGGETTO ANOMALIA E' <> DA "E"
153500040116    3C**   ANMSAN        IFNE      'E'
153600040116     C**   ANMAIE        ANDEQ     'E'
153700040116     C**   ANMAIE        ORNE      'E'
153800040116    c
153900040116    3C     ANMSAN        IFNE      'E'
154000040116     C     wanom         ANDEQ     'A'
154100040116     C     ANMAIE        ORNE      'E'
154200040116     C     wanom         orne      'A'
154300960327     C                   CLEAR                   DSLR44
154400960515    4C     *IN27         IFEQ      *ON
154500960401     C     *IN28         OREQ      *ON
154600970529     C     *IN26         OREQ      *ON
154700990114     C     *IN24         OREQ      *ON
154800990114     C   24              Z-ADD     ANMN10        D44NRR
154900970529     C   26              Z-ADD     ANMNR3        D44NRR
155000960327     C   27              Z-ADD     ANMNR7        D44NRR
155100960327     C   28              Z-ADD     ANMNR8        D44NRR
155200960401     C                   MOVEL     'P'           D34APA
155300960515   X4C                   ELSE
155400960327     C   29              Z-ADD     ANMNR9        D44NRR
155500960417     C   25              Z-ADD     ANMN10        D44NRR
155600960401     C                   MOVEL     'A'           D34APA
155700960515    4C                   ENDIF
155800960327     C                   CLEAR                   D34F03
155900960327     C                   CLEAR                   D34F12
156000960327     C                   CLEAR                   D34RIL
156100960327     C                   MOVEL     DSLR34        D44LR
156200960327     C                   MOVEL     DSANM         D44ANM
156300960521     C                   MOVEL     'G'           D44MOD
156400960327     C*
156500960327     C                   MOVEL     DSLR44        KPJBU
156600960327     C                   CALL      'FNLR44R'
156700960327     C                   PARM                    KPJBA
156800111116     C                   PARM                    dslr34
156900960327     C                   MOVEL     KPJBU         DSLR44
157000111116     C***                MOVEL     D44LR         DSLR34
157100960327     C* SE PREMUTO F3 F12 O RILETTURA
157200960515    4C     D34F03        IFEQ      'S'
157300960327     C     D34F12        OREQ      'S'
157400960327     C     D34RIL        OREQ      'S'
157500960327     C                   MOVEL     'S'           WUSCIT            1
157600960327     C                   SETON                                        3031
157700960515    4C                   ENDIF
157800960515    3C                   ENDIF
157900960327     C**
158000960327   X2C                   ELSE
158100960327     C** S T A M P A
158200960327     C*
158300000317    3C                   SELECT
158400000317     C* ELABORAZIONE IDD
158500000317     C     D34ELA        WHENNE    ' '
158600960405     C  N11              EXSR      STAMPA
158700960327     C*
158800960327     C* ELABORAZIONE DEFINITIVA AGGIORNO E SPEDISCO
158900960405     C   01              EXSR      ELAB
159000960327     C**
159100000317     C     D34GES        WHENEQ    'S'
159200960327     C* NON E' STAMPA IDD --> STAMPO TUTTO
159300960327     C                   EXSR      STAMPA
159400000317     C*
159500000317    3C                   ENDSL
159600960327    2C                   ENDIF
159700960327    1C                   ENDIF
159800960327     C*
159900960327     C                   ENDSR
160000921015     C*
160100921015     C* CONFRONTO RICHIESTA FATTA A VIDEO ---------------------------*
160200921015     C     CONFR         BEGSR
160300960508     C                   SETOFF                                         2097
160400960419     C                   SETOFF                                       101112
160500960509     C                   CLEAR                   PRTZNC
160600960927     C                   CLEAR                   PRTDET
160700960927     C                   CLEAR                   WDDET
160800960927     C                   CLEAR                   WDFLP
160900960927     C                   CLEAR                   PSTAM
161000980327     C* PULISCO IL CAMPO DI CANCELLAZIONE RECORD IN CASO DI TRASFERIMENTO
161100980327     C* A FILIALE
161200040615     C**                 CLEAR                   NODEL
161300040615     C**                 CLEAR                   WRTF
161400030717     C**
161500030717     C* SE RICHIESTO ESEGUO LA SELEZIONE X P.O. SEGNACOLLO E NUMERO SERIE
161600030717     C                   IF        D34POS  > *ZEROS
161700030717     C                   IF        D34POS <> ANMFLS
161800030717     C                   SETON                                        10
161900030717     C                   GOTO      ENDCON
162000030717     C                   ENDIF
162100040614     c
162200030717     C                   IF        D34NRS <> 99
162300030717     C                   IF        D34NRS <> ANMNRS
162400030717     C                   SETON                                        10
162500030717     C                   GOTO      ENDCON
162600030717     C                   ENDIF
162700030717     C                   ENDIF
162800040614     C                   ENDIF
162900040609     c
163000040609     c* se richiesto, faccio vedere solo anomalie di colli di valore
163100040609     c                   if        D34bva='V'  and
163200040609     c                             anmft4<>'V'
163300040609     C                   SETON                                        10
163400040609     C                   GOTO      ENDCON
163500040609     C                   ENDIF
163600000317     C**
163700980402     C* CONTROLLO CHE LA LINEA DI ARRIVO SIA UGUALE A QUELLA RICHIESTA A VIDEO
163800980403     C* SE NON SONO IN UNA FASE DI TRASFERIMENTO
163900980403     C     D34TRA        IFNE      'S'
164000980402     C     D34LNA        IFNE      *ZEROS
164100980402     C     D34LNA        ANDNE     ANMLNA
164200980402     C                   SETON                                        10
164300980402     C                   GOTO      ENDCON
164400980402     C                   ENDIF
164500980403     C                   ENDIF
164600960424     C*
164700960424     C* SE STO LEGGENDO UNA ANOMALIA RICEVUTA, NON LA STAMPO
164800960424     C*  SE IL SEGNACOLLO NEL FRATTEMPO E' STATO CONSEGNATO
164900960524     C*  (NON COME IDD3 DANNI CODICE 5)
165000970127     C*  (NON SE DATA APERTURA ANOMALIA > DATA CONSEGNA)
165100040116    1C***  ANMAIE        IFEQ      'R'
165200040116     C     ANMSCN        ifgt      0
165300040223     C     wanom         andeq     'P'
165400111115     c* In gestione/int non lo faccio --> devo vedere le anomalie come sono
165500111115     c*  per poterle annullare
165600111115     c     d34ges        andeq     'S'
165700960424     C*
165800960424     C     KART          CHAIN     FNBLT27L                           32
165900960424    2C     *IN32         IFEQ      *OFF
166000960424     C*
166100960424    3C     BLTDCM        IFGT      0
166200960927     C     BLTDET        ORGT      0
166300960524     C**
166400960524     C     KBLP          CHAIN     FNBLP01L                           32
166500960524     C**
166600970127    4C     BLTDCM        IFGT      0
166700970127     C     BLPCCA        ANDNE     '5'
166800151125     C     BLPCCA        ANDNE     '7'
166900151125     C     ANMDAO        ANDLE     BLTDCM
167000960424     C* SE HO CHIESTO SOLO LE APERTE ESCO
167100960524    5C     D34APR        IFEQ      'S'
167200960424     C                   SETON                                        10
167300960424     C                   GOTO      ENDCON
167400960524   X5C                   ELSE
167500960424     C* ALTRIMENTI DICO CHE E' CHIUSA COME CONSEGNATA
167600960524    6C     ANMDCH        IFEQ      0
167700960424     C                   MOVE      BLTDCM        ANMDCH
167800960524    6C                   ENDIF
167900960524     C                   MOVEL     'CO'          ANMCCH
168000960524    5C                   ENDIF
168100960424    4C                   ENDIF
168200960424    3C                   ENDIF
168300960424     C*
168400960424     C* SE ANOMALIA 10 NON STAMPO SE LA SPEDIZIONE C'E' ED E' TRASMESSA
168500960424    3C     ANMCAA        IFEQ      10
168600960424     C     KBLP          CHAIN     FNBLP01L                           32
168700960509     C  N32              MOVEL     BLPZNC        PRTZNC            2
168800151125    4C  N32BLPFT1        IFeq      'T'
168900960424     C* SE HO CHIESTO SOLO LE APERTE ESCO
169000960424    5C     D34APR        IFEQ      'S'
169100960424     C                   SETON                                        10
169200960424     C                   GOTO      ENDCON
169300960424   X5C                   ELSE
169400960424    6C     ANMDCH        IFEQ      0
169500960424     C                   Z-ADD     BLPMGS        ANMDCH
169600990512     C                   MOVEL     BLPAAS        ANMDCH
169700960424    6C                   ENDIF
169800960424    5C                   ENDIF
169900960424    4C                   ENDIF
170000960424    3C                   ENDIF
170100960508     C**
170200960508     C** SE ANOMALIA 5 STAMPO SE E' ANDATA IN DISGUIDO
170300960508    3C     ANMCAA        IFEQ      005
170400040617     c
170500970604     C* PRIMA VERIFICO SE PER IL SEGNACOLLO ESISTE UN'ALTRA SPUNTA
170600970604     C* PARTENZA CON DATA > PERCHE' IN QUESTO CASO L'ANOMALIA
170700970604     C* NON E' DA STAMPARE
170800970604   3AC     ANMCDU        IFEQ      SIMFEL
170900970604     C* DETERMINO DATA DEL FOGLIO DELL'ANOMALIA
171000970604    4C     ANMNFV        IFNE      SAVNFV
171100970604     C                   MOVE      ANMNFV        SAVNFV
171200970604     C                   CLEAR                   DSLV53
171300970604     C                   MOVEL     'G'           D53TFO
171400970604     C                   Z-ADD     SIMFEL        D53FEL
171500970604     C                   Z-ADD     ANMNFV        D53NFV
171600020926     C                   Z-ADD     ANMcdu        D53FGS
171700970604     C                   CALL      'FNLV53R'
171800970604     C                   PARM                    DSLV53
171900970604     C                   MOVEL     '1'           WCL53             1
172000970604     C                   Z-ADD     D53DFV        W53DFV
172100970604    4C                   END
172200970603     C*
172300970603     C                   Z-ADD     1             KNPG
172400140618     C     KBRV3A        SETLL     FNBRV05L
172500140618     C     KBRV3A        READE     FNBRV05L                               32
172600970603    4C     *IN32         DOWEQ     *OFF
172700160623
172800970604   4AC     BRVNFV        IFNE      ANMNFV
172900160623     c* Solo se spunta reale lo eseguo
173000160623   4Bc                   if        %lookup(brvnps:sps)>0
173100970603     C                   CLEAR                   DSLV53
173200970604     C                   MOVEL     'G'           D53TFO
173300970603     C                   Z-ADD     SIMFEL        D53FEL
173400970603     C                   Z-ADD     BRVNFV        D53NFV
173500020926     C                   Z-ADD     BRVfgs        D53FGS
173600970603     C                   CALL      'FNLV53R'
173700970603     C                   PARM                    DSLV53
173800970603     C                   MOVEL     '1'           WCL53             1
173900980424     C* QUESTO CONTROLLO LO IGNORO SE SONO IN TRASFERIMENTO
174000980424    5C     D34TRA        IFNE      'S'
174100980424     C     D53ERR        ANDEQ     *BLANKS
174200970604     C     D53DFV        ANDGT     W53DFV
174300970603     C                   SETON                                        10
174400970603     C                   GOTO      ENDCON
174500970603    5C                   END
174600160623   4BC                   END
174700970604   4AC                   END
174800140618     C     KBRV3A        READE     FNBRV05L                               32
174900970603    4C                   ENDDO
175000970604   3AC                   ENDIF
175100970603     C*
175200960927     C                   EXSR      DISPAR
175300960927    3C                   ENDIF
175400960927     C**
175500960424     C* MEMORIZZO IL NUMERO DI SPEDIZIONE SE C'E'
175600960424     C                   MOVE      BLTAAS        ANMAAS
175700960424     C                   MOVE      BLTLNP        ANMLNP
175800960424     C                   MOVE      BLTNSP        ANMNSP
175900960424    2C                   ENDIF
176000960424    1C                   ENDIF
176100040617     C
176200040617     c* Per stampa colli di valore in arrivo, escludo anomalie se non
176300040617     c*  crerate dal p.o. gestione  SE STAMPA DEL TERMINAL
176400040617     c                   if        *in70 and wanom='A' AND ANMCAA=005 AND
176500040617     C                             *IN80
176600040617     c                   if        anmfle<>d34fgs
176700040617     C                   SETON                                        10
176800040617     C                   GOTO      ENDCON
176900040617     c                   endif
177000040617     c                   endif
177100960506     C* CODICE ANOMALIA
177200960508     C                   EXSR      LEGCAA
177300040615     c
177400040615     c* In USACDU escludo sempre le anomalie non IDD (in partenza sempre
177500040615     c*           lette per lnp)
177600040615     c                   if        wcdu='S' and §7caie='E' and §7cidd=' '
177700040615     C                   SETON                                        10
177800040615     C                   GOTO      ENDCON
177900040615    4C                   ENDIF
178000960419     C**
178100960506     C**
178200960419     C* 1) SE CONTROLLO NON PASSATO NON FACCIO NULLA
178300960419     C*
178400960416     C* SE DEVO USARE FLE, VERIFICO SE E' IN L6
178500961230    1C     WFLE          IFEQ      'S'
178600960416     C     ANMFLE        LOOKUP    L6                                     32
178700960416     C  N32              SETON                                            10
178800960417     C  N32              GOTO      ENDCON
178900961230     C*
179000970829    2C     WANOM         IFEQ      'A'
179100961017     C* VERIFICO ANCHE CHE NON SIA ANOMALIA CHE UTILIZZA
179200961017     C* SIA LNA CHE FLE CON LNA=FLE
179300970829    3C     §7CUEA        IFEQ      'E'
179400970829    4C     ANMLNA        IFEQ      ANMFLE
179500961017     C                   SETON                                        10
179600961017     C                   GOTO      ENDCON
179700970829    4C                   ENDIF
179800040413     C* SE E' IN L6  MA LO E' ANCHE LA LNA
179900961230     C*  ED E' UN TIPO ANOMALIA CHE VIENE STAMPATO  PER ENTRANBI
180000961230     C*  ESCLUDO PERCHE' L'HO GIA' STAMPATA
180100961230     C     ANMLNA        LOOKUP    L6                                     32
180200961230     C   32              SETON                                            10
180300961230     C   32              GOTO      ENDCON
180400970829    3C                   ENDIF
180500970829     C*
180600970829   X2C                   ELSE
180700970829     C* FLE UTILIZZATO IN PARTENZA
180800970829    3C     §7CUEP        IFEQ      'E'
180900970829     C* PRENDO O LNP O FLS
181000970829    4C     ANMLNP        IFGT      0
181100971125     C                   Z-ADD     ANMLNP        W0030             3 0
181200970829     C                   ELSE
181300970829     C                   Z-ADD     ANMFLS        W0030
181400970829    4C                   ENDIF
181500970829     C     W0030         LOOKUP    L1                                     32
181600970829     C   32              SETON                                            10
181700970829     C   32              GOTO      ENDCON
181800970829    3C                   ENDIF
181900970829     C**
182000970829    2C                   ENDIF
182100961230    1C                   ENDIF
182200980327     C* IN CASO DI TRASFERIMENTO DI FILIALE
182300040615    1C**   WFLE          IFNE      'S'
182400040615     C**   D34TRA        ANDEQ     'S'
182500040615    2C**   §7CUEA        IFEQ      'E'
182600980327     C* ARRIVO
182700040615     C**   WANOM         ANDEQ     'A'
182800040615     C**   ANMFLE        LOOKUP    L6                                     32
182900040615     C**N32ANMFLE        LOOKUP    L1                                     34
183000040615     C**N32
183100040615     C*N 34              MOVEL     'S'           NODEL             1
183200040615    2C**                 END
183300980327     C* PARTENZA
183400040615    2C**   §7CUEP        IFEQ      'E'
183500040615     C**   WANOM         ANDEQ     'P'
183600040615     C**   ANMFLE        LOOKUP    L6                                     32
183700040615     C**N32ANMFLE        LOOKUP    L1                                     34
183800040615     C**N32
183900040615     C*N 34              MOVEL     'S'           NODEL
184000040615    2C**                 END
184100980327     C*
184200040615    1C**                 END
184300960419     C**
184400960419     C* 2) SE CONTROLLO NON PASSATO NON FACCIO NULLA
184500960419     C**
184600960419     C* DATA APERTURA ANOMALIA DAL/AL
184700960419    1C     D34DLA        IFGT      0
184800960419    1C     D34ALA        ORGT      0
184900990512    2C     ANMDAO        IFLT      D34DLA
185000990512     C     ANMDAO        ORGT      D34ALA
185100960419     C                   SETON                                        10
185200960419     C                   GOTO      ENDCON
185300960419    2C                   ENDIF
185400960419    1C                   ENDIF
185500040615     c**
185600040615     c* 2a) Il controllo che faceva in USACDU lo faccio qui dopo aver
185700040615     c*     controllato la data
185800040615    1c                   if        wcdu='S'
185900040615     c
186000040615     c* cerco alla data anmdao se d34fgs è terminal di anmlnp
186100040615     c                   eval      kcaeepa='P'
186200040615    2c                   if        anmlnp>0
186300040615     c                   eval      kcaetfp=anmlnp
186400040615     c                   else
186500040615     c                   eval      kcaetfp=anmfls
186600040615    2c                   endif
186700040615     c                   eval      kcaetfe=d34fgs
186800040615     c                   eval      kcaedde=anmdao
186900040615     c     kcae          setll     azcae03l
187000040615     c     kcae2         readpe    azcae03l
187100040616     c
187200040616    2c                   dow       not %eof(azcae03l) and caeatb<>*blanks
187300040616     c     kcae2         readpe    azcae03l
187400040616    2c                   enddo
187500040616     c
187600040616    2c                   if        not %eof(azcae03l)
187700040615     c* se decorrente alla data anomalia --> p.o. della £1 di d34fgs escludo
187800040615    3c                   if        caedsc>=anmdao
187900040615     c                   seton                                        10
188000040615     c* Se Compreso tutto il periodo richiesto a video, escludo tutta la
188100040615     c*  lnp e quindi faccio SETGT in usacdu
188200040615    4c                   if        caedde<=d34dla  and
188300040615     c                             caedsc>=d34ala
188400040615     c                   eval      wsetgt='S'
188500040615    4c                   endif
188600040615     c
188700040615    3c                   endif
188800040615    2c                   endif
188900040615     c   10              goto      endcon
189000040615     c*
189100040615     c* Escludo anche dalla stampa anomalie con anmlna 2 livello di
189200040616     c*  d34fgs per non stampare anomalie di arrivo e  non di transito
189300040615    2c                   if        anmlna=d34fgs
189400040615     C                   SETON                                        10
189500040615     C                   GOTO      ENDCON
189600040616   x2c                   else
189700040616     c
189800040616     c                   clear                   wanomok           1
189900040616     c* Controllo il terminal di arrivo per sta anomalia
190000040616     c                   eval      kcaeepa='A'
190100040616    3c                   if        anmlnp>0
190200040616     c                   eval      kcaetfp=anmlnp
190300040616     c                   else
190400040616     c                   eval      kcaetfp=anmfls
190500040616    3c                   endif
190600040616     c                   eval      kcaetfa=anmlna
190700040616     c                   eval      kcaedde=anmdao
190800040616     c
190900040616     c                   EXSR      CercaCAE
191000040616     c
191100040616     c* Se non ho trovato eccezione terminal, cerco terminal normale
191200040616    3c                   if        wanomok=' '
191300040616     c                   clear                   kcaetfp
191400040616     c                   EXSR      CercaCAE
191500040616    3c                   endif
191600040616    c
191700040616     c* anomalia da non stampare
191800040616    3c                   if        wanomok='N'
191900040616     C                   SETON                                        10
192000040616     C                   GOTO      ENDCON
192100040616    3c                   endif
192200040616     c
192300040616    2c                   endif
192400040615    1c                   endif
192500960419     C**
192600960419     C* 3) SE CONTROLLO NON PASSATO NON FACCIO NULLA
192700960419     C**
192800960419     C* SE NON STO ELABORANDO PER FLE ESCLUDO LE ANOMALIE FLE
192900970829     C     WFLE          IFEQ      'N'
193000960419     C     ANMCAA        LOOKUP    FLE                                    10
193100960419     C   10              GOTO      ENDCON
193200960419     C                   ENDIF
193300960419     C**
193400960419     C* 4) SE E COMUNQUE ANOMALIE ESTERNA ANCHE SE NO IDD LA TRASMETTO
193500960419     C**
193600960327     C* SE HO RICHIESTO LE ESTERNE O LE RICEVUTE, VEDO SE
193700960327     C*  SONO QUELLE IDD O NO
193800960327     C     D34IE         IFNE      'I'
193900960327     C                   MOVEL     ANMCAA        W003A             3
194000960506     C     §7CIDD        IFEQ      'S'
194100960506     C                   SETON                                        97
194200960506     C                   ENDIF
194300960404     C* 97 ON - E' ANOMALIA IDD
194400960404    2C     *IN97         IFEQ      *ON
194500960506    3C     D34IE         IFEQ      'N'
194600960327     C                   SETON                                        10
194700960327     C                   GOTO      ENDCON
194800960327    3C                   ENDIF
194900960327     C*
195000960327   X2C                   ELSE
195100960404     C* 97 OFF  - NO IDD
195200960506    3C     D34IE         IFEQ      'D'
195300960327     C                   SETON                                        10
195400960419     C* 12 ON SE COMUNQUE ANOMALIA ESTERNA
195500040116     C   01aNMAIE        IFEQ      'E'
195600960419     C                   SETON                                        12
195700960419    3C                   ENDIF
195800960419     C*
195900960327     C                   GOTO      ENDCON
196000960327    3C                   ENDIF
196100960327    2C                   ENDIF
196200960327    1C                   ENDIF
196300960419     C**
196400960419     C* 5) SE ANOMALIA IDD CHIUSA TRASMETTO SENZA STAMPARE
196500960419     C**
196600960419     C* STAMPA SOLO ANOMALIE APERTE
196700961217     C* PER LE ANOMALIE LOCALI 70 E 71 FACCIO UN GIRO A PARTE
196800040116    0C**   *IN07         IFEQ      *OFF
196900040116     C**   *IN07         OREQ      *ON
197000040116     C**   ANMCAA        ANDNE     70
197100040116     C**   ANMCAA        ANDNE     71
197200040116     c
197300040116     c                   if        wanom='A'  or
197400040116     c                             wanom='P' and anmcaa<>70 and anmcaa<>71
197500961217     C*
197600960419    1C     D34APR        IFEQ      'S'
197700960419     C     ANMDCH        ANDNE     0
197800960419     C                   SETON                                        10
197900960419     C   01ANMAIE        IFEQ      'E'
198000960419     C                   SETON                                        12
198100960419     C                   ENDIF
198200960419     C                   GOTO      ENDCON
198300960419    1C                   ENDIF
198400960419     C* SOLO ANOMALIE CHIUSE
198500960419    1C     D34APR        IFEQ      'C'
198600960419     C     ANMDCH        ANDEQ     0
198700960419     C                   SETON                                        10
198800960419     C                   GOTO      ENDCON
198900960419    1C                   ENDIF
199000961217     C*
199100961217   X0C                   ELSE
199200961217     C* NON TESTO LA DATA DI CHIUSURA MA LA CASUALE
199300961217    1C     D34APR        IFEQ      'S'
199400961217    2C     ANMCCH        IFNE      '  '
199500961217     C                   SETON                                        10
199600961217   X2C                   ELSE
199700961217     C*
199800961217     C* PRIMA DI STAMPARLA VERIFICO SE CE N'E' UN'ALTRA COM DATA >
199900961217     C*                    MI SALVO LA DATA
200000990512     C                   MOVEL     ANMDAO        W0080             8 0
200100990512     C                   MOVEL     ANMAAS        KAAS2
200200961217     C                   MOVEL     ANMLNP        KLNP2
200300961217     C                   MOVEL     ANMNRS        KNRS2
200400961217     C                   MOVEL     ANMNSP        KNSP2
200500961217     C                   Z-ADD     ANMCAA        KCAA
200600961217     C*
200700961217     C* LEGGO IL COLLO FINTANTO CHE TROVO DELLE ANOMALIE INSERITE
200800961217     C*  NEL CASO STAMPERO' QUELLA
200900990512     C     KANM1         SETLL     FNANM01L
201000990512     C     KANM1         READE     FNANM01L                               31
201100961217    3C     *IN31         DOWEQ     *OFF
201200990512     C     ANMDAO        ANDLE     W0080
201300990512     C     KANM1         READE     FNANM01L                               31
201400961217    3C                   ENDDO
201500961217     C*
201600961217    3C     *IN31         IFEQ      *ON
201700990512     C   27ANMNR7        CHAIN(N)  FNANM000                           33
201800990512     C   28ANMNR8        CHAIN(N)  FNANM000                           33
201900961217     C*
202000961217     C                   CLEAR                   ANMDCH
202100961217     C                   CLEAR                   ANMFSC
202200961217   X3C                   ELSE
202300961217     C                   SETON                                        10
202400961217     C                   GOTO      ENDCON
202500961217    3C                   ENDIF
202600961217    2C                   ENDIF
202700961217    1C                   ENDIF
202800961217     C*
202900961217    1C     D34APR        IFEQ      'C'
203000961217     C     ANMCCH        ANDEQ     '  '
203100961217     C                   SETON                                        10
203200961217     C                   GOTO      ENDCON
203300961217    1C                   ENDIF
203400961217     C*
203500961217    0C                   ENDIF
203600960419     C**
203700960426     C* 7) TUTTI GLI ALTRI CONFRONTI, TRANNE IL LIVELLO IDD NON FANNO
203800960419     C*    NE STAMPARE NE TRASMETTERE
203900960419     C**
204000960327     C* DATA CHIUSURA ANOMALIA DAL/AL
204100960927     C* PER LE ANOMALIE NON IDD3
204200960927     C     *IN05         IFEQ      *ON
204300960927     C     ANMCCH        ANDEQ     'PR'
204400960927     C     ANMLNA        LOOKUP    FM1                                    32
204500960927     C                   ENDIF
204600960927     C*
204700960927    0C     *IN05         IFEQ      *OFF
204800960927     C     ANMCCH        ORNE      'PR'
204900960927     C     *IN32         OREQ      *OFF
205000960927     C*
205100960327    1C     D34DCA        IFGT      0
205200960419     C     D34ACA        ORGT      0
205300990512    2C     ANMDCH        IFLT      D34DCA
205400990512     C     ANMDCH        ORGT      D34ACA
205500960327     C                   SETON                                        10
205600960327     C                   GOTO      ENDCON
205700960327    2C                   ENDIF
205800960327    1C                   ENDIF
205900960927     C*
206000960927   X0C                   ELSE
206100990512    1C     ANMDCH        IFLT      W08DM1
206200990512     C     ANMDCH        ORGT      W08AM1
206300960927     C                   SETON                                        10
206400960927     C                   GOTO      ENDCON
206500960927    1C                   ENDIF
206600960927    0C                   ENDIF
206700921015     C*
206800921015     C* STAMPA FASE SPECIFICA
206900960327    1C     D34FAS        IFNE      ' '
207000960326     C     ANMFSA        ANDNE     D34FAS
207100921015     C                   SETON                                        10
207200921015     C                   GOTO      ENDCON
207300960327    1C                   ENDIF
207400960426     C*
207500960506     C* STAMPA CAUSALE CHIUSURA SPECIFICA
207600960426    1C     D34CCH        IFNE      ' '
207700960426     C     ANMCCH        ANDNE     D34CCH
207800960426     C                   SETON                                        10
207900960426     C                   GOTO      ENDCON
208000960426    1C                   ENDIF
208100961108     C*
208200040116     C* in partenza escludo le anomalie appena create (livello 0 o 2
208300040616     c*  per anom 15) solo per le anomalie IDD e se non prevista la
208400040616     c*  stampa dalla DS D34SL0
208500040116     C** 07ANMLID        IFEQ      0
208600040616     c                   if        d34sl0=' '
208700040119     c                   if        anmlid=§7clid  and §7cidd='S' and
208800040119     c                             wanom='P' and §7cinv=' '
208900961108     C                   SETON                                        10
209000961108     C                   GOTO      ENDCON
209100961108    1C                   ENDIF
209200040616    1C                   ENDIF
209300960419     C*
209400960506     C* STAMPA CODICI ANOMALIE RICHIESTE
209500960327    1C     D34ANM        IFNE      *BLANKS
209600921015     C                   MOVEL     ANMCAA        ALF3              3
209700921015     C     ALF3          LOOKUP    ANM                                    31
209800960327    2C     D34INC        IFEQ      'I'
209900921015     C  N31              SETON                                        10
210000921015     C  N31              GOTO      ENDCON
210100921015     C*
210200960327   X2C                   ELSE
210300921015     C*
210400921015     C   31              SETON                                        10
210500921015     C   31              GOTO      ENDCON
210600960327    2C                   END
210700960402     C*
210800960402     C                   ELSE
210900960402     C*
211000960402     C* SE NON HO RICHIESTO UNO SPECIFICO CODICE ANOMALIA
211100960402     C*  GESTISCO SOLO QUELLI STAMPABILI
211200960402     C     §7CSTA        IFNE      'S'
211300960402     C                   SETON                                        10
211400960402     C                   GOTO      ENDCON
211500960506    2C                   ENDIF
211600960506    1C                   ENDIF
211700921015     C*
211800921015     C* NUMERO FOGLIO VIAGGIO
211900960327    1C     D34NFV        IFNE      0
212000960326     C     ANMNFV        ANDNE     D34NFV
212100921015     C                   SETON                                        10
212200921015     C                   GOTO      ENDCON
212300960327    1C                   ENDIF
212400960506     C*
212500960506     C* SE ANOMALIA INTERNA VEDO SE RICHIESTO PART O ARR
212600960506    1C     ANMAIE        IFEQ      'I'
212700960506     C     D34PAR        ANDNE     ' '
212800960506     C*
212900960506     C* RICHIESTE LE ANOMALIE PARTENZA MA QUESTA E' ANOMALIA ARRIVO
213000960506    2C     D34PAR        IFEQ      'P'
213100960506     C     §7CPAR        ANDEQ     'A'
213200960506     C                   SETON                                        10
213300960506    2C                   ENDIF
213400960506     C* VICEVERSA
213500960506    2C     D34PAR        IFEQ      'A'
213600960506     C     §7CPAR        ANDEQ     'P'
213700960506     C                   SETON                                        10
213800960506    2C                   ENDIF
213900960506     C*
214000960506     C   10              GOTO      ENDCON
214100960506    1C                   ENDIF
214200040615     c
214300980327     C* IN CASO DI TRASFERIMENTO DI FILIALE
214400040615    1C**   D34TRA        IFEQ      'S'
214500980327     C* ANOMALIA ESTERNA
214600040615     C**   ANMAIE        ANDEQ     'E'
214700980327     C* ARRIVO
214800040615    2C**   WANOM         IFEQ      'A'
214900980327     C* PRENDO O LNP O FLS
215000040615    3C**   ANMLNP        IFGT      0
215100040615     C**                 Z-ADD     ANMLNP        W0030             3 0
215200040615     C**                 ELSE
215300040615     C**                 Z-ADD     ANMFLS        W0030
215400040615    3C**                 ENDIF
215500980327     C*
215600040615     C**   W0030         LOOKUP    L6                                     32
215700040615     C**N32W0030         LOOKUP    L1                                     34
215800040116     C**N32
215900040116     C** 34              MOVEL     'R'           WRTF
216000040116     C* N32
216100040116     C*N 34              MOVEL     'E'           WRTT
216200040615     C* N32
216300040615     C*N 34              MOVEL     'U'           NODEL
216400040615    2C**                 ENDIF
216500980327     C* PARTENZE
216600040615    2C**   WANOM         IFEQ      'P'
216700040615     C**   ANMLNA        LOOKUP    L6                                     32
216800040615     C**N32ANMLNA        LOOKUP    L1                                     34
216900040116     C**N32
217000040116     C*N 34              MOVEL     'R'           WRTT              1
217100040615     C**N32
217200040615     C*N 34              MOVEL     'E'           WRTF              1
217300040615     C**N32
217400040615     C*N 34              MOVEL     'U'           NODEL
217500040615    2C**                 ENDIF
217600980327     C*
217700040615    1C**                 ENDIF
217800960403     C* DAL LIVELLO IDD
217900980327     C     *IN97         IFEQ      *ON
218000980327     C     *IN01         ANDEQ     *ON
218100980327     C     D34TRA        OREQ      'S'
218200000317     C     D34GES        OREQ      'V'
218300990114     C   24
218400990512     COR 25ANMN10        CHAIN     FNANM000                           33
218500000306     C   26ANMNR3        CHAIN     FNANM000                           33
218600000306     C   27ANMNR7        CHAIN     FNANM000                           33
218700990512     C   28ANMNR8        CHAIN     FNANM000                           33
218800990512     C   29ANMNR9        CHAIN     FNANM000                           33
218900960403     C                   ENDIF
219000960403     C*
219100960404    1C   97D34ELA        IFNE      ' '
219200960403     C                   ADD       1             ANMLID
219300960403     C                   ENDIF
219400960403     C**
219500960405     C   97ANMLID        IFLT      D34LID
219600960405     C* SE NON E' ELABORAZIONE DEFINITIVA IDD 10 ON
219700960405     C     D34ELA        IFNE      'S'
219800960403     C                   SETON                                        10
219900960417     C                   GOTO      ENDCON
220000960405     C                   ELSE
220100960419     C* ALTRIMENTI ACCENDO L' 11  PERCHE' DEVO AGGIORNARE SENZA STAMPAR
220200960405     C                   SETON                                        11
220300960405     C                   ENDIF
220400960405     C*
220500960403    2C                   ENDIF
220600960412     C*
220700960327     C     ENDCON        ENDSR
220800040616     c     CercaCAE      begsr
220900040616     c     kcae1         setll     azcae01l
221000040616     c     kcae3         readpe    azcae01l
221100040616    3c                   dow       not %eof(azcae01l) and caeatb<>*blanks
221200040616     c     kcae3         readpe    azcae01l
221300040616    3c                   enddo
221400040616     c
221500040616    3c                   if        not %eof(azcae01l)
221600040616    4c                   if        caedde<=anmdao   and caedsc>=anmdao
221700040616     c* Trovata eccezione terminal --> se il terminal  d34fgs esco
221800040616    5c                   if        caetfe=d34fgs
221900040616     c* Anomalia da non stampare
222000040616     c                   eval      wanomok='N'
222100040616     c                   else
222200040616     c* Anomalia da     stampare
222300040616     c                   eval      wanomok='S'
222400040616    5c                   endif
222500040616    4c                   endif
222600040616    3c                   endif
222700040616     c                   ENDSR
222800960927     C*
222900960927     C* CONTROLLO SE C'E' DISGUIDO IN LETTURA ANOMALIE PARTENZA ------*
223000960930     C     DISPAR        BEGSR
223100971124     C* VEDO SE E' PRESENTE UNA ANOMALIA DI DISGUIDO
223200990409     C                   SETOFF                                       20
223300990409     C                   DO        2
223400990409     C                   Z-ADD     56            KCAA
223500990409     C     *IN20         IFEQ      *OFF
223600990512     C     KANM2         CHAIN     FNANM02L                           32
223700960927    1C     *IN32         DOWEQ     *OFF
223800971126    2C     AN2DAO        IFGT      WDDET
223900960927     C                   SETON                                        20
224000960927     C                   MOVEL     CDISG         PSTAM
224100960927     C                   MOVEL     AN2FLE        WDFLP
224200971126     C                   Z-ADD     AN2DAO        WDDET
224300960927    2C                   ENDIF
224400990512     C     KANM2         READE     FNANM02L                               32
224500960927    1C                   ENDDO
224600990409     C*
224700990409     C                   Z-ADD     55            KCAA
224800990409    2C                   ENDIF
224900990409    1C                   ENDDO
225000960927     C*
225100960927     C* SE NON TROVATA CONTROLLO BLTFLP
225200960927    1C     *IN20         IFEQ      *OFF
225300960927     C     BLTDET        ANDGT     0
225400960927     C**
225500960927     C* CHAIN IL FV
225600020807     C     K02FGV01      CHAIN     FNFGV01L                           32
225700960927    4C     *IN32         IFEQ      *OFF
225800020807     C     K02FGV01      CHAIN     FNFGW01L                           32
225900000503    5C     *IN32         IFEQ      *ON
226000000503     C     FGWATB        ORNE      ' '
226100000503     C                   CLEAR                   FGWFF3
226200000503     C                   CLEAR                   FGWFF4
226300000503     C                   CLEAR                   FGWFL3
226400000503     C                   CLEAR                   FGWFL4
226500000503    5C                   ENDIF
226600960927     C                   MOVEL     BLTFLP        W003A             3
226700960930     C                   MOVEL     ANMLNA        W003AA            3
226800961001     C                   Z-ADD     0             XX                3 0
226900961001     C                   SETON                                        32
227000961001     C*
227100961001    5C     *IN32         DOWEQ     *ON
227200961001     C     PSTAM         ANDEQ     *BLANKS
227300961001     C                   ADD       1             XX
227400960927     C     W003A         LOOKUP    FLP(XX)                                32
227500960927     C**
227600961001    6C     *IN32         IFEQ      *ON
227700960930     C     FFV(XX)       ANDEQ     W003AA
227800960930     C                   MOVEL     CTRANS        PSTAM            18
227900961001    6C                   ENDIF
228000961001    5C                   ENDDO
228100961001     C**
228200961001    5C     PSTAM         IFEQ      *BLANKS
228300960927     C                   MOVEL     CDISG         PSTAM
228400960927    5C                   ENDIF
228500960927   X4C                   ELSE
228600960927     C                   MOVEL     CDISG         PSTAM
228700960927    4C                   ENDIF
228800960927     C                   SETON                                        20
228900960927     C                   MOVEL     BLTFLP        WDFLP
229000960927     C                   MOVEL     BLTDET        WDDET
229100960927    1C                   ENDIF
229200960927     C                   ENDSR
229300960508     C*
229400960508     C* CHAIN SULLE TABELLE DELL'ANOMALIA ---------------------------*
229500960508     C     LEGCAA        BEGSR
229600960508     C     ANMCAA        IFNE      SAVCAA
229700960508     C                   MOVEL     '7C'          COD
229800960508     C                   MOVEL(P)  ANMCAA        KEY
229900960508     C     KTAB          CHAIN(N)  TABEL                              31
230000960508     C  N31              MOVEL     TBLUNI        DS7C
230100960508     C   31              CLEAR                   DS7C
230200960508     C                   MOVEL     ANMCAA        SAVCAA
230300960508     C                   ENDIF
230400960508     C                   ENDSR
230500911015     C*
230600960827     C* CHIUDO ANOMALIE DI DISGUIDO O ANOMALIE 50 SE ESISTE BLP ------*
230700960827     C     A50A55        BEGSR
230800021025     C                   SETOFF                                       32
230900021025     C     KART          CHAIN     FNBLT27L                           39
231000021025     C  N39BLTLNP        LOOKUP    L1                                     32
231100021025    2C     *IN32         IFEQ      *ON
231200960705     C* TROVATA BOLLA PARTENZA:
231300990409     C* CHIUDO ANOMALIA 55 O 56
231400960705     C                   MOVE      'N'           WTRM              1
231500960705     C                   CLEAR                   DSLR33
231600960705     C                   MOVE      ANMDAO        D33DCH
231700960705     C                   MOVEL     'M'           D33FSC
231800960705     C                   MOVEL     'AN'          D33CCH
231900960705     C                   MOVEL     'S'           D33MAC
232000990114     C   24
232100990114     COR 25              MOVE      ANMN10        D33NRR
232200000306     C   26              MOVE      ANMNR3        D33NRR
232300000306     C   27              MOVE      ANMNR7        D33NRR
232400960709     C   28              MOVE      ANMNR8        D33NRR
232500960709     C   29              MOVE      ANMNR9        D33NRR
232600960705     C                   CALL      'FNLR33R'
232700960705     C                   PARM                    DSLR33
232800960827     C* SE ANOMALIA 55:
232900960827    3C     ANMCAA        IFEQ      55
233000990409    3C     ANMCAA        OREQ      56
233100960705     C* CHIUDO ANOMALIA 200
233200960709     C                   MOVE      KCAA          WCAA
233300960705     C                   Z-ADD     200           KCAA
233400990512     C     KANM2         CHAIN     FNANM02L                           32
233500960705     C     *IN32         IFEQ      *OFF
233600960705     C                   CLEAR                   DSLR33
233700960705     C                   MOVE      AN2DAO        D33DCH
233800960705     C                   MOVEL     'AN'          D33CCH
233900960705     C                   MOVE      ANMNR2        D33NRR
234000960705     C                   CALL      'FNLR33R'
234100960705     C                   PARM                    DSLR33
234200960705     C                   END
234300960709     C                   MOVE      WCAA          KCAA
234400960827    3C                   END
234500960705    2C                   END
234600960705     C                   ENDSR
234700960705     C*
234800911021     C* STAMPO ANOMALIE ---------------------------------------------*
234900900621     C     STAMPA        BEGSR
235000151125     c                   setoff                                       434445
235100151125     c                   clear                   wdcm
235200960405     C* PER IDD
235300911025     C* SE DIVENTA 3 LA CHIUDO CON CAUSALE PR = PRATICA
235400960405    1C   97D34ELA        IFNE      ' '
235500960403    2C     ANMLID        IFEQ      §7ALCI
235600990512     C                   MOVEL     D34ALA        ANMDCH
235700911025     C                   MOVEL     'PR'          ANMCCH
235800921027     C                   MOVEL     'D'           ANMFSC
235900960327    2C                   ENDIF
236000960327    1C                   ENDIF
236100960514     C*
236200960514     C* SE IL SOGGETTO ANOMALIA E' "E" --> NON STAMPO L'ANOMALIA
236300960515     C*                                    SE IDD IN ARRIVO
236400040116    c
236500040116    3C     ANMSAN        IFNE      'E'
236600040116     C     wanom         ANDEQ     'A'
236700040116     C     ANMAIE        ORNE      'E'
236800040116     C     wanom         orne      'A'
236900911025     C*
237000911021     C* NUMERO SPEDIZIONE
237100911021     C     ANMNSP        COMP      0                                  0202
237200911021     C* NUMERO SEGNACOLLO
237300970314     C     ANMFLS        COMP      0                                  0303
237400970314     C     ANMCAA        COMP      300                                    22
237500960417     C**
237600960417     C* SE C'E' IL NUMERO DI SPEDIZIONE, VADO A PRENDERE LA DATA
237700960514    2C     ANMNSP        IFGT      0
237800960417     C                   SETOFF                                       33
237900960417     C                   CLEAR                   G02INV
238000960417     C*
238100960417     C     KANM          CHAIN     FNBLP01L                           33
238200960514    4C     *IN33         IFEQ      *OFF
238300960509     C                   MOVEL     BLPZNC        PRTZNC
238400960417     C                   Z-ADD     BLPMGS        G02INV
238500971126     C                   MOVEL     BLPAAS        G02INV
238600050503     c* Se c'e' particolarità consegna CP, la stampo
238700050503     c                   if        blpgma='CP'
238800050503     c                   seton                                        43
238900050503     c                   endif
239000151125     c* Se consegna anomala 7 stampo
239100151125     c                   if        blpcca='7' and blpdcm>0
239200151125     c                   seton                                        45
239300151125     c                   movel     blpdcm        wdcm              8 0
239400151125     c                   endif
239500151125     c
239600051221     c* Se richiesto stampo dati mittente e destinatario
239700051221     c                   if        d34med = 'S' and d34ela = ' '
239800051221     c                   seton                                        44
239900051221     c                   movel     blprsm        prtrsm           32
240000051222     c* rmn : lo stampo eliminando gli spazi a sinistra non significativi
240100051222     c                   move      blprmn        prtrmn           15
240200051222     c     ' 0'          check     prtrmn        N                 2 0
240300051222     c                   eval      prtrmn = %subst(prtrmn:N)
240400051221     c                   endif
240500050503     c
240600960514    4C                   ENDIF
240700960417     C*
240800961018     C                   CLEAR                   PRTDSP
240900960514    3C     G02INV        IFGT      0
241000960417     C                   MOVEL     3             G02ERR
241100971126     C                   CALL      'XSRDA8'
241200960417     C                   PARM                    WLBDAT
241300960417     C                   MOVEL     G02DAT        PRTDSP            4 0
241400960514    3C                   ENDIF
241500151125     c*
241600151125    3c                   if        wdcm>0  and *in45
241700151125     c                   eval      g02inv=wdcm
241800151125     C                   MOVEL     3             G02ERR
241900151125     C                   CALL      'XSRDA8'
242000151125     C                   PARM                    WLBDAT
242100151125     C                   MOVEL     G02DAT        PRTDcm            4 0
242200151125    3C                   ENDIF
242300151125    2C                   ENDIF
242400960508     C*
242500960508     C* PER L'ANOMALIA 005 VERIFICO SE IL COLLO E' ANDATO IN DISGUIDO
242600040116     C***  ANMAIE        IFNE      'R'
242700040116     C     ANMCAA        ifeq      005
242800960508     C                   EXSR      ANM005
242900960514    2C                   ENDIF
243000960927     C*
243100960508     C* SE HO TROVATO IL DISGUIDO LO STAMPO
243200960514    2C     *IN20         IFEQ      *ON
243300960508     C                   MOVE      WDDET         G02INV
243400960508     C                   MOVEL     3             G02ERR
243500971126     C                   CALL      'XSRDA8'
243600960508     C                   PARM                    WLBDAT
243700960508     C                   MOVEL     G02DAT        PRTDET            4 0
243800960514    2C                   ENDIF
243900911021     C* TIPO ANOMALIA
244000960514    2C     ANMTAN        IFNE      SAVTAN
244100911021     C                   MOVEL     '7F'          COD
244200960327     C                   MOVEL(P)  ANMTAN        KEY
244300960409     C     KTAB          CHAIN(N)  TABEL                              31
244400040616     C   31              MOVEL     *BLANKS       tbluni
244500040616     c* Vedo semettere o meno il tipo di anomalia (Idd/int/no idd)
244600040616     c                   if        *in17
244700040616     C                   MOVEL     TBLUNI        DESTAN           12
244800040616     c                   else
244900040616     c                   movel     tbluni        w008a             8
245000040616     c                   endif
245100921015     C*
245200921015     C                   MOVEL     ANMTAN        SAVTAN            1
245300960514    2C                   END
245400040616     c*
245500040616     c                   if        not *in17
245600040616     c                   clear                   destan
245700040616     C* ANOMALIA
245800040616    2C     ANMAIE        IFEQ      'I'
245900040616     C                   MOVEL     'INT'         DESAIE            3
246000040616   X2C                   ELSE
246100040616     C**
246200040616    3C     §7CIDD        IFEQ      'S'
246300040616     C                   MOVEL     'IDD'         DESAIE                         IDD
246400040616     C                   ELSE
246500040616     C                   MOVEL     'NON'         DESAIE                         NON IDD
246600040616    3C                   ENDIF
246700040616    2C                   ENDIF
246800040616     c                   movel     desaie        destan
246900040616     c                   move      w008a         destan
247000040616     c                   endif
247100960506     C*
247200960506     C* DESCRIZIONE CODICE ANOMALIA
247300960506     C                   MOVEL     §7CDES        DESCAA           25
247400960704     C*
247500960705     C* SE ANOMALIA 10-MANCA RECORD BOLLA ARRIVO O 138-COLLO GIA' CONS.
247600960705     C* CONTROLLO SE COLLO SPARATO IN INVENTARIO O IN CONSEGNA
247700960704     C                   SETOFF                                       21
247800960704     C     ANMCAA        IFEQ      10
247900960705     C     ANMCAA        OREQ      138
248000960704     C     ANMLNA        LOOKUP    L6                                     32
248100960704     C     *IN32         IFEQ      *ON
248200960704     C                   EXSR      RCRSPU
248300960704     C                   END
248400960704     C                   END
248500960402     C*
248600950320     C* SE BOLLA IN CONSEGNA REPERISCO NR/DATA DISTINTA E PADRONCINO
248700950320     C                   SETOFF                                       09
248800960514    2C     ANMCAA        IFEQ      135
248900950321     C     KART          CHAIN     FNART27L                           31
249000960514    3C     *IN31         IFEQ      *OFF
249100950320     C     KARB          CHAIN     FNARB01L                           31
249200960514    4C     *IN31         IFEQ      *OFF
249300960509     C                   MOVEL     ARBZNC        PRTZNC
249400960509     C*
249500960514    5C     ARBNDC        IFNE      0
249600950320     C                   SETON                                        09
249700950320     C*
249800950320     C                   MOVEL     DESCAA        DESCA1           18
249900950320     C                   MOVE      ':'           DESCA1
250000950320     C* Data distinta
250100950320     C                   MOVE      ARBDDC        G02INV
250200950320     C                   MOVEL     3             G02ERR
250300971126     C                   CALL      'XSRDA8'
250400950320     C                   PARM                    WLBDAT
250500950320     C                   MOVEL     G02DAT        STADDC            4 0
250600950320     C* Padroncino
250700950320     C                   MOVE      *BLANKS       STAPDC           12
250800021203     C*    ARBPDC        CHAIN     FNAPD01L                           32
250900021205     C                   move      arbpdc        kpdpdr
251000021203     C                   move      'A'           kpdtip
251100021203     C     kfapd         chain     fiapd01L                           32
251200960327     C  N32              MOVEL     APDRSC        STAPDC
251300960514    5C                   END
251400960514    4C                   END
251500960514    3C                   END
251600960514    2C                   END
251700990121     C**
251800990121     C** NUMERO CA SE C'E'
251900990122     C                   SETOFF                                       404142
252000990121     C     KART          CHAIN     FNDCD02L                           32
252100990121    2C     *IN32         IFEQ      *OFF
252200990121     C     DCDATB        ANDEQ     ' '
252300990121     C     KDCT          CHAIN     FNDCT01L                           32
252400050221      * se dctdt2 > 0 la c.a. non è valida x la filiale
252500050221     c                   If        Not *In32 and dctdt2 > 0
252600050221     c                   Eval      *In32 = *On
252700050221     c                   EndIf
252800990121     C* CONTROLLO LA CAUSALE DI CHIUSURA SE C'E'
252900990121    3C  N32DCTDCH        IFGT      0
253000990121     C     DCTCCH        ANDNE     ' '
253100990121     C                   CLEAR                   DSBS02
253200990121     C                   MOVEL     'C'           T02MOD
253300990121     C                   MOVEL     KNSIF         T02SIF
253400990121     C                   MOVEL     'CCH'         T02COD
253500990121     C                   MOVEL     DCTCCH        T02KE1
253600990121     C                   CALL      'TIBS02R'
253700990121     C                   PARM                    KPJBA
253800990121     C                   PARM                    DSBS02
253900990121    4C     T02ERR        IFEQ      ' '
254000990121     C                   MOVEL     T02UNI        DCCH
254100990121     C                   ELSE
254200020905     ***C                   MOVEL     'S'           §CHCRA
254300020905     C                   MOVEL     'S'           §CCHCRAN
254400990121    4C                   ENDIF
254500990121    3C                   ENDIF
254600990121     C**
254700990121     C** STAMPO SE CHIUSA C.A. SENZA CAUSALE
254800990122    3C  N32DCTCCH        IFEQ      *BLANKS
254900990121     C     DCTDCH        ANDGT     0
255000990121     C** STAMPO SE CHIUSA C.A. CON CAUSALE PER CUI CREARE ANOMALIA
255100990121     C*  (SIGNIFICA CHE E' CA REALE)
255200020905     ***C     §CHCRA        OREQ      'S'
255300020905     C     §CCHCRAN      OREQ      'S'
255400990121     C     DCTDCH        ANDGT     0
255500990122     C* STAMPO SE C.A. APERTA E DETT C.A. APERTO
255600990122     C     DCTDCH        OREQ      0
255700990122     C     DCDDCH        ANDEQ     0
255800990122     C* DECODIFICA DANNI
255900990122     C                   EXSR      DECDAN
256000990122    3C                   ENDIF
256100990121    2C                   ENDIF
256200050503     c
256300050503     C* VERIFICO PER CP SE STAMPARE decodifica corta o lunga
256400050503     c                   clear                   decodicp         23
256500050503     c                   if        *in43
256600050503     c                   if        *in41
256700050503     c                   eval      decodicp=CCPcort
256800050503     c                   else
256900050503     c                   eval      decodicp=CCPlung
257000050503     c                   endif
257100050503     c                   endif
257200990122     C**
257300990122     C* SE NON E' SU C.A. PER LA 151 STAMPO LA 1 C.A. CHIUSA CHE TROVO
257400990122    2C     *IN32         IFEQ      *ON
257500990122     C     ANMNSP        ANDGT     0
257600990122     C     ANMCAA        ANDEQ     151
257700990122     C                   CLEAR                   WTRDAN
257800990122     C**
257900040811     C**""**     KANM          CHAIN     FNDCT02L                           32
258000040811    3C**""**     *IN32         DOWEQ     *OFF
258100040811    4C**""**     DCTATB        IFNE      ' '
258200040811     C**""**     DCTDCH        OREQ      0
258300040811     C**""**     KANM          READE     FNDCT02L                               32
258400040811     C**""**                   ELSE
258500040811     C**""**                   SETON                                        32
258600040811     C**""**                   MOVEL     'S'           WTRDAN            1
258700040811    4C**""**                   ENDIF
258800040811    3C**""**                   ENDDO
258900990122     C**
259000040811      * Sostituisco la lettura del file FNDTC02 con
259100040811      * richiamo del programma FIDN12R passando la DS richiedendo le CA non annul e chiuse
259200040811      * legate alla spedizione che passo
259300040811      *
259400040811     c                   clear                   fidn12ds
259500040811     c                   eval      i12aas = anmaas
259600040811     c                   eval      i12lnp = anmlnp
259700040811     c                   eval      i12nrs = anmnrs
259800040811     c                   eval      i12nsp = anmnsp
259900040811     c                   eval      i12fel = 'E'
260000040811     c                   eval      i12fan = 'E'
260100040811      *
260200040811     c                   call      'FIDN12R'
260300040811     c                   parm                    fidn12ds
260400040811      *
260500040811      * se non ci sono errori
260600040811     c                   if        o12err = ' '
260700040811      * se numero di CA trovate maggiore di zero
260800040811     c                   if        o12nca > 0
260900040811      * prendo solo la prima CA chiusa
261000040811     c                   z-add     *zeros        jj                2 0
261100040811     c     1             do        o12nca        jj
261200040811     c                   if        skdch(jj) > 0
261300040811     c                   movel     'S'           wtrdan            1
261400040811     c                   movea     skkey(jj)     dskey
261500040811     C                   Z-ADD     dsfil         dctfil
261600040811     C                   Z-ADD     dsnca         dctnca
261700040811     C                   Z-ADD     skdca(jj)     dctdca
261800040811     C                   Z-ADD     skdch(jj)     dctdch
261900040811     c                   z-add     o12nca        jj
262000040811     c                   endif
262100040811     C                   ENDDO
262200040811      *
262300040811     c                   endif
262400040811     c                   endif
262500040811     C*
262600990122     C** TROVATA LA C.A. CHIUSA: STAMPO I DATI
262700990122     C     WTRDAN        IFEQ      'S'
262800990122     C                   EXSR      DECDAN
262900990122    3C                   ENDIF
263000990122    2C                   ENDIF
263100911021     C* FASE APERTURA
263200960514    2C     ANMFSA        IFNE      SAVFSA
263300911021     C                   MOVEL     '7D'          COD
263400960327     C                   MOVEL(P)  ANMFSA        KEY
263500960409     C     KTAB          CHAIN(N)  TABEL                              32
263600040616     C  N32              MOVEL     TBLUNI        DESFSA            8
263700960327     C   32              MOVEL     *BLANKS       DESFSA
263800921015     C*
263900921015     C                   MOVEL     ANMFSA        SAVFSA            1
264000960514    2C                   END
264100911021     C* DATA APERTURA
264200971126     C                   CLEAR                   G02INV
264300911021     C                   MOVE      ANMDAO        G02INV
264400911018     C                   MOVEL     3             G02ERR
264500971126     C                   CALL      'XSRDA8'
264600911018     C                   PARM                    WLBDAT
264700950320     C                   MOVEL     G02DAT        STADAO            4 0
264800911021     C* FASE CHIUSURA
264900960514    2C     ANMFSC        IFNE      SAVFSC
265000911021     C                   MOVEL     '7D'          COD
265100960327     C                   MOVEL(P)  ANMFSC        KEY
265200960409     C     KTAB          CHAIN(N)  TABEL                              32
265300040616     C  N32              MOVEL     TBLUNI        DESFSC            8
265400960327     C   32              MOVEL     *BLANKS       DESFSC
265500921015     C*
265600921015     C                   MOVEL     ANMFSC        SAVFSC            1
265700960514    2C                   END
265800960506     C*
265900960508     C** SE RICEVUTA STAMPO ANMFLE
266000040116    2C**   ANMAIE        IFEQ      'R'
266100040116     C**                 SETON                                        04
266200040116     C**                 ELSE
266300040116     C**                 SETOFF                                       04
266400040116    2C**                 ENDIF
266500040116     c* lo faccio se sto leggendo le anomalie partenza
266600040616    2C**   wanom         IFEQ      'P'
266700040616     C**                 SETON                                        04
266800040616     C**                 ELSE
266900040616     C**                 SETOFF                                       04
267000040616    2C**                 ENDIF
267100911021     C* DATA CHIUSURA
267200960514    2C     ANMDCH        IFGT      0
267300971126     C                   CLEAR                   G02INV
267400911021     C                   MOVE      ANMDCH        G02INV
267500911021     C                   MOVEL     3             G02ERR
267600971126     C                   CALL      'XSRDA8'
267700911021     C                   PARM                    WLBDAT
267800950320     C                   MOVEL     G02DAT        STADCH            4 0
267900960417     C                   ELSE
268000960417     C                   CLEAR                   STADCH
268100960514    2C                   ENDIF
268200911021     C* CAUSALE CHIUSURA
268300960514    2C     ANMCCH        IFNE      SAVCCH
268400970523     C     ANMCCH        IFNE      'RA'
268500911021     C                   MOVEL     '7E'          COD
268600960417     C                   MOVEL(P)  ANMCCH        KEY
268700960409     C     KTAB          CHAIN(N)  TABEL                              31
268800960417     C  N31              MOVEL     TBLUNI        DESCCH           12
268900911021     C   31              MOVEL     *BLANKS       DESCCH
269000970523     C                   ELSE
269100970523     C                   CLEAR                   DESCCH
269200970523     C                   END
269300921015     C*
269400921015     C                   MOVEL     ANMCCH        SAVCCH            2
269500960514    2C                   END
269600970523     C*
269700970523     C     ANMCCH        IFEQ      'RA'
269800970523     C                   CLEAR                   CCHPRT
269900970523     C                   ELSE
270000970523     C                   MOVE      ANMCCH        CCHPRT
270100970523     C                   END
270200040616     C* DATA/ora creazione anomalia
270300040616     c                   clear                   stadac
270400040616     c                   clear                   staorc
270500040616     c                   if        anmflo<>*blanks
270600040616     c                   movel     anmflo        danmflo
270700040616     C                   CLEAR                   G02INV
270800040616     C                   MOVE      §anmdac       G02INV
270900040616     C                   MOVEL     3             G02ERR
271000040616     C                   CALL      'XSRDA8'
271100040616     C                   PARM                    WLBDAT
271200040616     C                   MOVEL     G02DAT        STADAc            4 0
271300040616     c                   move      §anmorc       staorc            4 0
271400040616     c                   endif
271500970523     C*
271600960327     C**
271700960403     C* SOLO SE ANOMALIA ESTERNE IDD STAMPO IL LIVELLO IDD
271800960404     C   97              MOVEL     ANMLID        STALID            1
271900960404     C  N97              CLEAR                   STALID
272000960509     C**
272100960509     C* SE NON L'HO ANCORA TROVATA PRENDO LA ZONA DALLE SPUNTE
272200960514    2C     PRTZNC        IFEQ      '  '
272300070111     C     KART          SETLL     FNBRV07L
272400070111     C     KART          READE     FNBRV07L                               32
272500960514    3C     *IN32         DOWEQ     *OFF
272600960509     C     PRTZNC        ANDEQ     '  '
272700960514    4C     BRVZNC        IFGT      0
272800960509     C                   MOVEL     BRVZNC        PRTZNC
272900960509     C                   ELSE
273000070111     C     KART          READE     FNBRV07L                               32
273100960514    4C                   ENDIF
273200960514    3C                   ENDDO
273300960514    2C                   ENDIF
273400990512     C* ANNO A 2
273500990512     C   02              MOVE      ANMAAS        PRTAAS            2 0
273600911025     C*
273700900622     C* STAMPO DETTAGLIO
273800921015     C*
273900921015     C* STAMPA TESTATA
274000960514    2C     D34ELA        IFNE      ' '
274100960403     C     ANMLID        ANDLT     §7ALCI
274200960327     C     D34ELA        OREQ      ' '
274300960327     C*
274400040615     c                   if        *inOF
274500040615     C                   EXCEPT    TES
274600040615     C  n70              EXCEPT    TES2
274700040615     C                   EXCEPT    TES3
274800040615     C                   SETOFF                                       OF
274900040615     c                   endif
275000921015     C*
275100921015     C                   EXCEPT    DET
275200050503     c   41
275300050503     cor 43              except    det1
275400051221     c   44              except    det2
275500151125     c   45              except    det3
275600050503     c
275700031007     C   55              EXSR      WRIDET
275800960422     C                   MOVEL     '1'           WSTAMP            1
275900921015     C*
276000960514   X2C                   ELSE
276100921015     C*
276200921015     C   OE              EXCEPT    TESX
276300921015     C   OE              SETOFF                                       OE
276400921015     C*
276500911025     C                   EXCEPT    DETX
276600050503     c   41
276700050503     Cor 43              EXCEPT    DETX1
276800031007     C   55              EXSR      WRIDET
276900960514    2C                   ENDIF
277000900622     C*
277100040616     C**N17              SETOFF                                       04
277200040401     c* Scrivo file per report di sede
277300040401     c     wgir200       ifeq      'S'
277400040407     c     *in01         andeq     *on
277500040401     c                   exsr      srant
277600040401     c                   endif
277700960514    1C                   ENDIF
277800900622     C                   ENDSR
277900990122     C*
278000990122     C* DECODIFICA DATI RECORD C.A. ----------------------------------*
278100990122     C     DECDAN        BEGSR
278200990122     C* SE 09 20 O 21 STAMPO SU 2 RIGA ALTRIMENTI SULLA PRIMA
278300990122    1C     *IN09         IFEQ      *OFF
278400990122     C     *IN20         ANDEQ     *OFF
278500990122     C     *IN21         ANDEQ     *OFF
278600990122     C                   SETON                                        40
278700990122     C                   MOVEL     DESCAA        DESCA1           18
278800990122     C                   MOVE      ':'           DESCA1
278900990122     C                   ELSE
279000990122     C                   SETON                                        41
279100990122    1C                   ENDIF
279200990122     C**
279300990122     C* Data C.A.
279400990122     C                   MOVE      DCTDCA        G02INV
279500990122     C                   MOVEL     3             G02ERR
279600990122     C                   CALL      'XSRDA8'
279700990122     C                   PARM                    WLBDAT
279800990122     C                   MOVEL     G02DAT        STADCA            6 0
279900990122     C                   Z-ADD     G02DAT        W0020             2 0
280000990122     C                   MOVE      W0020         STADCA
280100990122     C* Data CHIUSURA C.A.
280200990122    1C     DCTDCH        IFGT      0
280300990122     C                   MOVE      DCTDCH        G02INV
280400990122     C                   MOVEL     3             G02ERR
280500990122     C                   CALL      'XSRDA8'
280600990122     C                   PARM                    WLBDAT
280700990122     C                   MOVEL     G02DAT        STACHA            4 0
280800990122     C                   SETON                                        42
280900990122     C**
281000990122    1C                   ENDIF
281100990122     C                   ENDSR
281200960508     C*
281300960508     C* CONTROLLO PER ANOMALIA 005 SE E' COLLO IN DISGUIDO ----------*
281400960508     C     ANM005        BEGSR
281500960508     C* PROVO A CHAINARE FNART
281600960508     C     KART          CHAIN     FNART27L                           32
281700960927    1C     *IN32         IFEQ      *OFF
281800960927     C     ARTDET        ANDGT     0
281900960508     C                   SETON                                        20
282000960508     C                   MOVEL     ARTFLP        WDFLP
282100960508     C                   MOVEL     ARTDET        WDDET
282200960807     C*
282300960927     C                   Z-ADD     1             KNPG
282400960927     C* CERCO LA SPUNTA CON FGS=ARTFLP E DATA= ARTDET
282500140618     C     KBRV3         SETLL     FNBRV05L
282600140618     C     KBRV3         READE     FNBRV05L                               32
282700960927    2C     *IN32         DOWEQ     *OFF
282800960509     C* ZONA
282900960927    3C     BRVZNC        IFGT      0
283000960509     C                   MOVEL     BRVZNC        PRTZNC
283100960927    3C                   ENDIF
283200960927     C*
283300000201     C                   Z-ADD     BRVNFV        WNFV
283400960927     C     KFVA          CHAIN     FNFVA01L                           32
283500960927    3C     *IN32         IFEQ      *OFF
283600040617     C     FVADFV        ANDEQ     ARTDUT
283700960927     C                   SETON                                        32
283800960927    4C     BRVATR        IFEQ      'T'
283900960927     C                   MOVEL     CTRANS        PSTAM
284000960927     C                   ELSE
284100960927     C                   MOVEL     CDISG         PSTAM
284200960927    4C                   ENDIF
284300960927   X3C                   ELSE
284400960927     C                   SETOFF                                       32
284500960927    3C                   ENDIF
284600960927     C**
284700140618     C  N32KBRV3         READE     FNBRV05L                               32
284800960927    2C                   ENDDO
284900960927     C**
285000960927     C* SE NON TROVATO METTO DISGUIDO
285100960927     C     PSTAM         IFEQ      *BLANKS
285200960927     C                   MOVEL     CDISG         PSTAM
285300960927     C                   ENDIF
285400960927    1C                   ENDIF
285500960508     C                   ENDSR
285600911021     C*
285700960704     C* RICERCA SPUNTA INVENTARIO O CONSEGNA PER ANOMALIA 10 --------*
285800960704     C     RCRSPU        BEGSR
285900960704     C                   CLEAR                   WNPG
286000960704     C                   CLEAR                   WSPG
286100960704     C                   CLEAR                   WDFV
286200960704     C                   CLEAR                   WFGS
286300960704     C* CERCO SPUNTA INVENTARIO E CONSEGNA E PRENDO QUELLA PIU' RECENTE
286400960704    1C                   DO        2             II                1 0
286500960704    2C     II            IFEQ      1
286600960704     C                   Z-ADD     3             KNPG
286700960704     C                   ELSE
286800960704     C                   Z-ADD     4             KNPG
286900960704    2C                   END
287000140618     C     KBRV          SETLL     FNBRV05L
287100140618     C     KBRV          READE     FNBRV05L                               32
287200960704    2C     *IN32         DOWEQ     *OFF
287300000201     C                   Z-ADD     BRVNFV        WNFV
287400960704     C     KFVV          CHAIN     FNFVV01L                           32
287500960704    3C     *IN32         IFEQ      *OFF
287600960704     C     FVVDFV        ANDGT     WDFV
287700960704     C                   MOVE      FVVNPG        WNPG
287800960704     C                   MOVE      FVVSPG        WSPG
287900960704     C                   MOVE      FVVDFV        WDFV
288000960704     C                   MOVE      FVVFGS        WFGS
288100960704    3C                   END
288200140618     C     KBRV          READE     FNBRV05L                               32
288300960704    2C                   ENDDO
288400960704    1C                   ENDDO
288500960704    1C     WDFV          IFGT      0
288600960704     C                   SETON                                        21
288700960704     C* ROVESCIO DATA SPUNTA
288800960704     C                   CLEAR                   WLBDAT
288900960704     C                   MOVE      WDFV          G02INV
289000960704     C                   MOVEL     3             G02ERR
289100971126     C                   CALL      'XSRDA8'
289200960704     C                   PARM                    WLBDAT
289300960704     C                   MOVEL     G02DAT        PRTDFV            4 0
289400960704     C* IMPOSTO CAMPO PER STAMPA DECODIFICA CATEGORIA/SOTTOCATEGORIA
289500960704    2C     WNPG          IFEQ      3
289600960704     C                   MOVEL     'IM'          W003A             3
289700960704     C                   MOVE      WSPG          W003A
289800960704     C                   MOVEL(P)  W003A         PRTDOV            5
289900960704     C                   ELSE
290000960704     C                   MOVEL(P)  'CONS.'       PRTDOV
290100960704    2C                   END
290200960704    1C                   END
290300960704     C                   ENDSR
290400911021     C* INVIO ANOMALIE ----------------------------------------------*
290500911021     C     ELAB          BEGSR
290600990512     C                   MOVEL     'FNANM00T'    D60FIL
290700971126     C                   Z-ADD     AM8GIO        D60DRF
290800971128     C                   MOVEL(P)  '*SHRUPD'     D60VNC
290900921027     C* FLAG TRASMISSIONE
291000960604    1C     ANMFL1        IFNE      0
291100921027     C                   MOVEL     'T'           ANMFT1
291200000824     C                   MOVE      AM8GIO        ANMDTR
291300960604    1C                   END
291400960604    1C     ANMFL2        IFNE      0
291500921027     C                   MOVEL     'T'           ANMFT2
291600000824     C                   MOVE      AM8GIO        ANMDTR
291700960604    1C                   END
291800921027     C* SI INVIO IDD
291900960523    1C     W7AIDD        IFEQ      'S'
292000960604     C* DISALLOCAZIONE OGGETTI
292100021009     c                   eval      D60po0 = SIMfel
292200961218     C                   CLEAR                   D60FL1
292300961218     C                   CLEAR                   D60FL2
292400960604     C*
292500961218    2C  N95SAVFL1        IFNE      ANMFL1
292600961218     C                   MOVEL     SAVFL1        D60FL1
292700960604    2C                   ENDIF
292800961218    2C  N96SAVFL2        IFNE      ANMFL2
292900961218     C                   MOVEL     SAVFL2        D60FL2
293000960604    2C                   ENDIF
293100960604     C*
293200960604    2C     D60FL1        IFGT      0
293300960604     C     D60FL2        ORGT      0
293400990512     C   99              CLOSE     FNANM00T
293500960604     C   99              SETOFF                                       99
293600960604     C                   MOVEL     'D'           D60AZN
293700021009     C                   MOVEL     FNLV60DS      KPJBU
293800960604     C                   CALL      'FNLV60R'
293900960604     C                   PARM                    KPJBA
294000960604    2C                   ENDIF
294100960604     C* ALLOCAZIONE OGGETTI
294200021009     c                   eval      D60po0 = SIMfel
294300961218     C                   CLEAR                   D60FL1
294400961218     C                   CLEAR                   D60FL2
294500961218     C                   SETOFF                                       9596
294600961218     C* ANMFL1 >0 E IN L1 --> 95 ON
294700120217     c                   if        d34ges<>'V'
294800961218     C     ANMFL1        IFGT      0
294900961218     C     ANMFL1        LOOKUP    L1                                     95
295000961218     C                   ENDIF
295100961218     C* ANMFL2 >0 E IN L1 --> 96 ON
295200961218     C     ANMFL2        IFGT      0
295300961218     C     ANMFL2        LOOKUP    L1                                     96
295400961218     C                   ENDIF
295500120217     c
295600120217     C                   ENDIF
295700960604     C*
295800961218    2C  N95ANMFL1        IFNE      SAVFL1
295900961218     C                   MOVEL     ANMFL1        D60FL1
296000960604    2C                   ENDIF
296100961218    2C  N96ANMFL2        IFNE      SAVFL2
296200961218     C                   MOVEL     ANMFL2        D60FL2
296300960604    2C                   ENDIF
296400960604     C*
296500960604    2C     D60FL1        IFGT      0
296600960604     C     D60FL2        ORGT      0
296700960604     C                   MOVEL     'A'           D60AZN
296800021009     C                   MOVEL     FNLV60DS      KPJBU
296900960604     C                   CALL      'FNLV60R'
297000960604     C                   PARM                    KPJBA
297100021009     C                   MOVEL     KPJBU         FNLV60DS
297200960604     C                   MOVEL     D60ERR        WERR
297300961023     C     D60FL1        IFGT      0
297400960604     C                   MOVEL     D60MB1        WMB1
297500961023     C                   ENDIF
297600961023     C     D60FL2        IFGT      0
297700960604     C                   MOVEL     D60MB2        WMB2
297800961023     C                   ENDIF
297900021009     C     WMB2          IFEQ      *BLANKS
298000040223     C**** D60FL2        OREQ      *zeros
298100961023     C                   CLEAR                   ANMFL2
298200961023     C                   ENDIF
298300960604    2C                   ENDIF
298400960604     C**
298500960604     C* NON CI SONO ERRORI --> SCRIVO E AGGIORNO
298600961219    2C     WERR          IFNE      'S'
298700960604     C                   MOVEL     ANMFL1        SAVFL1
298800960604     C                   MOVEL     ANMFL2        SAVFL2
298900961218    3C  N95SAVFL1        IFGT      0
299000021008     ***C                   MOVEL     WMB1          VAR
299100021009     c                   movel     wMB1          DSmembro
299200021008     c                   movel     DSmembro      VAR
299300950115     C*
299400960604     C     *IN99         IFEQ      *OFF
299500990512     C                   MOVE      VAR           CMD1                           FNANM00T
299600911021     C                   MOVEL     *BLANKS       COMMAN           80
299700911021     C                   MOVEA     CMD1(1)       COMMAN
299800960604     C                   CALL      'QCMDEXC'                              98
299900911021     C                   PARM                    COMMAN
300000911021     C                   PARM                    LUNG
300100960604     C*
300200960604     C   98              MOVEL     'S'           WERR
300300911021     C**
300400911021     C** SCRIVO FILE INVIO
300500911021     C**
300600990512     C  N98              OPEN      FNANM00T
300700960604     C                   ENDIF
300800960604     C*
300900990512     C  N98              WRITE     FNANM0TT                                      INVIO
301000960604     C  N98              SETON                                        99
301100960604    3C                   ENDIF
301200960604    2C                   ENDIF
301300960604     C**
301400960604     C* NON CI SONO ERRORI --> SCRIVO E AGGIORNO
301500961219    2C  N96WERR          IFNE      'S'
301600961023     C     SAVFL2        ANDGT     0
301700961023     C     WMB2          ANDNE     WMB1
301800990512     C                   CLOSE     FNANM00T
301900960604     C                   SETOFF                                       99
302000960604     C**
302100021009     c                   movel     wMB2          DSmembro
302200021008     c                   movel     DSmembro      VAR
302300960604     C*
302400990512     C                   MOVE      VAR           CMD1                           FNANM00T
302500960604     C                   MOVEL     *BLANKS       COMMAN           80
302600960604     C                   MOVEA     CMD1(1)       COMMAN
302700960604     C                   CALL      'QCMDEXC'                              98
302800960604     C                   PARM                    COMMAN
302900960604     C                   PARM                    LUNG
303000960604     C*
303100960604     C   98              MOVEL     'S'           WERR
303200960604     C**
303300960604     C** SCRIVO FILE INVIO
303400960604     C**
303500990512     C  N98              OPEN      FNANM00T
303600990512     C  N98              WRITE     FNANM0TT                                      INVIO
303700990512     C  N98              CLOSE     FNANM00T
303800970306    2C                   ENDIF
303900970306     C** PER REPORT DI SEDE
304000970306    3C     ANMCAA        IFEQ      55
304100990409    3C     ANMCAA        OREQ      56
304200040504     c* la lnp disguido è quella della bolla, se c'e'
304300040504     c                   if        anmlnp>0
304400040504     c                   movel     anmlnp        wlnp
304500040504     c                   else
304600040504     C                   MOVEL     ANMFLS        wLNP
304700040504     c                   endif
304800040504     c
304900970306     C     KA55          CHAIN     FNANT02L                           39
305000970306     C*
305100970306    4C     *IN39         IFEQ      *ON
305200970306     C                   CLEAR                   FNANT000
305300970306     C                   MOVEL     ANMFLE        ANTFOA
305400040504     C****               MOVEL     ANMFLS        ANTLNP
305500040504     C                   MOVEL     wlnp          ANTLNP
305600990512     C                   MOVEL     ANMDAO        ANTDAO
305700970306     C                   MOVEL     ANMCAA        ANTCAA
305800970306     C                   Z-ADD     1             ANTTOT
305900990504     C*
306000990504     C                   CLEAR                   DSLV55
306100990504     C                   MOVEL     'P'           D55TPT
306200040504     C**                 MOVEL     anmfls        D55LIN
306300040504     C                   MOVEL     wlnp          D55LIN
306400990512     C                   MOVEL     ANMDAO        D55DRF
306500990504     C                   CALL      'FNLV55R'
306600990504     C                   PARM                    DSLV55
306700000317     C* PER LE ANOMALIE 55 E 56 ANTLNA CONTIENE IL TERMINAL DI PARTENZA
306800990504     C*  DELLE BOLLE
306900990504     C                   MOVEL     D55TFP        ANTLNA
307000040927     C                   Z-ADD     AM8GIO        antdt1
307100970306     C                   WRITE     FNANT000
307200970306   X4C                   ELSE
307300970306     C                   ADD       1             ANTTOT
307400040927     C****               CLEAR                   ANTFT1
307500040927     C****               CLEAR                   ANTFT2
307600040927     C                   Z-ADD     AM8GIO        antdt1
307700970306     C                   UPDATE    FNANT000
307800970306    4C                   ENDIF
307900970306    3C                   ENDIF
308000960523    1C                   ENDIF
308100911021     C*
308200960404     C* LE ANOMALIE 70 E 71 LE CHIUDO
308300960404     C* CON CAUSALE
308400000317    1C   01ANMCAA        IFEQ      70
308500960404     C     ANMCAA        OREQ      71
308600960523    2C     ANMDCH        IFEQ      0
308700990512     C                   MOVEL     D34ALA        ANMDCH
308800960404     C                   MOVEL     'D'           ANMFSC
308900970305     C*
309000970305    3C     ANMCAA        IFEQ      71
309100970306     C     KA71          CHAIN     FNANT02L                           39
309200970305     C*
309300970305    4C     *IN39         IFEQ      *ON
309400970305     C                   CLEAR                   FNANT000
309500970306     C                   MOVEL     ANMFLE        ANTFOA
309600970306     C                   MOVEL     ANMLNP        ANTLNP
309700990512     C                   MOVEL     ANMDAO        ANTDAO
309800970305     C                   MOVEL     ANMCAA        ANTCAA
309900970305     C                   Z-ADD     1             ANTTOT
310000040927     c                   z-add     am8gio        antdt1
310100970305     C                   WRITE     FNANT000
310200970305   X4C                   ELSE
310300970305     C                   ADD       1             ANTTOT
310400040927     c                   z-add     am8gio        antdt1
310500970305     C                   UPDATE    FNANT000
310600970305    4C                   ENDIF
310700970305    3C                   ENDIF
310800970305     C**
310900960523    2C                   ENDIF
311000960523    1C                   ENDIF
311100970523     C*
311200970523     C* SE CAUSALE CHIUSURA = 'RA' LA ABBLENCO
311300970523     C* ('RA' --> RIPRISTINO DI ANOMALIA 10 CHIUSA)
311400970523     C     ANMCCH        IFEQ      'RA'
311500970523     C                   CLEAR                   ANMCCH
311600970523     C                   END
311700960404     C*
311800000317     C* SE NON CI SONO STATI ERRORI DI TRASMISSIONE UPDATO
311900961219     C     WERR          IFNE      'S'
312000001102     C     D34GES        IFEQ      'V'
312100001102     C                   UPDATE    FNANM011
312200001102     C                   ELSE
312300990512     C                   UPDATE    FNANM000                                      AGGIORNO
312400001102     C                   ENDIF
312500961023     C                   ELSE
312600001102     C     D34GES        IFEQ      'V'
312700001102     C                   UNLOCK    FNANM11L
312800001102     C                   ELSE
312900990512     C                   UNLOCK    FNANM00F
313000001102     C                   ENDIF
313100960604    1C                   ENDIF
313200911021     C                   ENDSR
313300040401     C* SCRITTURA FILE PER REPORT DI SEDE ---------------------------*
313400040401     C     SRANT         BEGSR
313500040401     C** PER REPORT DI SEDE
313600040504     c                   if        anmlnp>0
313700040504     c                   movel     anmlnp        wlnp
313800040504     c                   else
313900040504     C                   MOVEL     ANMFLS        wLNP
314000040504     c                   endif
314100040405     C     KA200         CHAIN     FNANT02L                           39
314200040401     C*
314300040401    4C     *IN39         IFEQ      *ON
314400040401     C                   CLEAR                   FNANT000
314500040401     C                   MOVEL     ANMFLE        ANTFOA
314600040504     C                   MOVEL     wlnp          ANTLNP
314700040405     C* Come data anomalia metto data idd per poter conteggiare le anomalie
314800040405     c* lette tutte nello stesso report controllo qualità
314900040405     C                   MOVEL     WDTA200       ANTDAO
315000040401     C                   MOVEL     ANMCAA        ANTCAA
315100040401     C                   Z-ADD     1             ANTTOT
315200040401     C*
315300040401     C                   CLEAR                   DSLV55
315400040401     C                   MOVEL     'P'           D55TPT
315500040401     C                   MOVEL     ANMFLe        D55LIN
315600040401     C                   MOVEL     ANMDAO        D55DRF
315700040401     C                   CALL      'FNLV55R'
315800040401     C                   PARM                    DSLV55
315900040401     C* PER l'anomalia 200 ANTLNA CONTIENE IL TERMINAL DI PARTENZA
316000040401     C*  del p.o. che ha generato il disguido
316100040401     C                   MOVEL     D55TFP        ANTLNA
316200040927     c                   z-add     am8gio        antdt1
316300040401     C                   WRITE     FNANT000
316400040401     C                   ELSE
316500040401     C                   ADD       1             ANTTOT
316600040927     c                   z-add     am8gio        antdt1
316700040401     C                   UPDATE    FNANT000
316800040401     C                   ENDIF
316900040401     c                   endsr
317000911021     C*
317100960404     C* CONTROLLO PER TRASMETTERE LA CHIUSURA ANOMALIE 70 E 71 ------*
317200960404     C     CHIUAN        BEGSR
317300960417     C** MANCA BAM
317400960417     C                   Z-ADD     71            KCAA
317500960404     C                   Z-ADD     1             L
317600960404     C*
317700960417    1C     L6(L)         DOWGT     0
317800960404     C                   Z-ADD     L6(L)         KFIL
317900990512     C     KANM5         SETLL     FNANM10L
318000990512     C     KANM5         READE     FNANM10L                               30
318100960417    2C     *IN30         DOWEQ     *OFF
318200960417     C                   EXSR      BAM
318300990512     C     KANM5         READE     FNANM10L                               30
318400960417    2C                   ENDDO
318500960404     C**
318600960404     C                   ADD       1             L
318700960417    1C                   ENDDO
318800960417     C** ECCEDE BAM
318900960417     C                   Z-ADD     70            KCAA
319000021021     C                   Z-ADD     1             L
319100021021    1C     L6(L)         DOWGT     0
319200021021     C                   Z-ADD     L6(L)         KFIL
319300021021     C     Kanm20        SETLL     FNANM20L
319400021021     C     Kanm20        READE     FNANM20L                               30
319500960417    2C     *IN30         DOWEQ     *OFF
319600960417     C* SE ANMFLE E' PRESENTE IL L6 --> ELABORO
319700021021     c*  tolto il controllo perchè anmfle già in key
319800021021     C**   ANMFLE        LOOKUP    L6                                     31
319900021021     C**   *IN31         IFEQ      *ON
320000960417     C                   EXSR      BAM
320100021021     C**                 ENDIF
320200960417     C*
320300021021     C     Kanm20        READE     FNANM20L                               30
320400960417    2C                   ENDDO
320500021021     C**
320600021021     C                   ADD       1             L
320700021021    2C                   ENDDO
320800960404     C*
320900960404     C                   ENDSR
321000960417     C*
321100960417     C* CONTROLLO LE ANOMALIE 70 E 71 PER CHIUDERLE -----------------*
321200960417     C     BAM           BEGSR
321300960417     C                   MOVEL     ANMAAS        KAAS2
321400960417     C                   MOVEL     ANMLNP        KLNP2
321500960417     C                   MOVEL     ANMNRS        KNRS2
321600960417     C                   MOVEL     ANMNSP        KNSP2
321700960417     C*
321800960417     C* LEGGO IL COLLO FINTANTO CHE TROVO DELLE ANOMALIE INSERITE
321900990512     C     KANM1         SETLL     FNANM01L
322000990512     C     KANM1         READE     FNANM01L                               31
322100960417    4C     *IN31         DOWEQ     *OFF
322200990512     C     KANM1         READE     FNANM01L                               31
322300960417    4C                   ENDDO
322400960417     C*
322500960417     C* LETTA L'ULTIMA, VEDO CHE DATA CHIUSURA HA
322600960417    4C     ANMCCH        IFEQ      '  '
322700970424     C     ANMDCH        ANDGT     0
322800990512     C     ANMDCH        ANDLT     D34ALA
322900960417     C* RICHAINO CON NUMERO RELATIVO DI RECORD
323000990512     C     ANMNR1        CHAIN     FNANM00F                           31
323100960417     C* TRASMETTO IN PARTENZA LA CHIUSURA DELLA ANOMALIA
323200960417     C                   MOVEL     'OB'          ANMCCH
323300990512     C                   MOVEL     D34ALA        ANMDCH
323400960417     C                   EXSR      ELAB
323500960417    4C                   ENDIF
323600960417     C                   ENDSR
323700960422     C*
323800960423     C* STAMPA DI CHI NON HA FATTO L'IDD ----------------------------*
323900960423     C     NOIDD         BEGSR
324000960422     C                   SETON                                        08
324100040615     C                   SETOff                                       09
324200960423     C                   CLEAR                   WMANID
324300960423     C                   Z-ADD     95            KCAA
324400960422     C* SE IMMESSA LA DATA AL CERCO NELLA DATA AL
324500960422     C*  ALTRIMENTI CERCO LA DATA IDD PIU' ALTA CHE HO
324600990512    1C     D34ALA        IFEQ      0
324700990512     C     D34ACA        ANDEQ     0
324800990512     C     KCAA          SETGT     FNANM04L
324900990512     C     KCAA          READPE    FNANM04L                               30
325000960422    2C     *IN30         DOWEQ     *OFF
325100050504     C**   ANMAIE        ANDEQ     'E'
325200050504    2C**   *IN30         OREQ      *OFF
325300990512     C     ANMDAO        ANDGE     AM8GIO
325400990512     C     KCAA          READPE    FNANM04L                               30
325500960422    2C                   ENDDO
325600960422     C*
325700971126     C     *IN30         IFEQ      *OFF
325800971126     C                   Z-ADD     ANMDAO        WDATA8            8 0
325900971126     C                   ENDIF
326000971126     C**
326100960422   X1C                   ELSE
326200971126     C     D34ALA        IFGT      0
326300971126     C                   MOVEL     D34ALA        WDATA8
326400961016     C                   ELSE
326500971126     C                   MOVEL     D34ACA        WDATA8
326600961016     C                   ENDIF
326700961016     C*
326800960422     C                   SETOFF                                       30
326900960422    1C                   ENDIF
327000960510     C*
327100960926     C* PER I P.O. CHE FANNO L'IDD IL GIORNO DOPO
327200960510     C*  CERCO LA DATA PRECEDENTE
327300960510     C  N30              EXSR      RICDAT
327400990512     C  N30              MOVEL     GI8INV        WDATAM            8 0
327500960510     C*
327600960422     C* 30 OFF - TROVATA
327700960422    1C     *IN30         IFEQ      *OFF
327800960422     C                   Z-ADD     0             L
327900990518     C* GIRO LA DATA ELABORAZIONE DEL GIORNO
328000971126     C                   MOVE      WDATA8        G02INV
328100960422     C                   MOVEL     '3'           G02ERR
328200971126     C                   CALL      'XSRDA8'
328300960422     C                   PARM                    WLBDAT
328400990513     C                   MOVEL     G02DAT        STADCI            8 0
328500960510     C*
328600971126     C                   CLEAR                   G02INV
328700960510     C                   MOVE      WDATAM        G02INV
328800960510     C                   MOVEL     '3'           G02ERR
328900971126     C                   CALL      'XSRDA8'
329000960510     C                   PARM                    WLBDAT
329100990513     C                   MOVEL     G02DAT        STADCM            8 0
329200960422     C*
329300960422     C     *LOVAL        SETLL     AZORG01L
329400960423     C                   READ      AZORG01L                               30
329500960422     C*
329600960422    2C     *IN30         DOWEQ     *OFF
329700960423    C**
329800960510     C* ESCLUDO LE ANNULLATE
329900960422    3C     ORGFVA        IFEQ      ' '
330000960423     C**
330100960422     C* SOLO FILIALE E AGENZIA
330200960422    4C     ORGFAG        IFEQ      'F'
330300960422     C     ORGFAG        OREQ      'A'
330400960423     C**
330500960926     C* ESCLUDO I P.O. CHE NON DEVONO FARE IDD
330600960422     C                   MOVEL     ORGDE8        OG148
330700050831     C                   MOVEL     ORGDE3        OG143
330800960422    5C     §OGNID        IFEQ      ' '
330900971124     C* CERCO IL TERMINAL DI PARTENZA
331000971124     C* SE STO ELABORANDO L'IDD E NON SONO IL TERMINAL DI PARTENZA
331100971124   5AC     D34ELA        IFNE      ' '
331200971124     C     ORGFIL        ANDNE     D34FGS
331300971124     C**
331400971124     C                   CLEAR                   DSLV55
331500971124     C                   MOVEL     'P'           D55TPT
331600971124     C                   MOVEL     ORGFIL        D55LIN
331700980403     C                   Z-ADD     AM8GIO        D55DRF
331800971124     C                   CALL      'FNLV55R'
331900971124     C                   PARM                    DSLV55
332000971124   5BC     D55ERR        IFNE      ' '
332100971124     C                   Z-ADD     SIMFEL        D55TFP
332200971124   5BC                   ENDIF
332300971124   5AC                   ENDIF
332400960510     C**
332500960509     C* SE ANOMALIE IDD --> ESCLUDO SE NON SONO DELL'AREA
332600960423   5AC     D34ELA        IFEQ      ' '
332700971124     C     ORGFIL        ORNE      D34FGS
332800971124     C     D55TFP        ANDEQ     D34FGS
332900960422     C*
333000960422     C                   MOVEL     ORGFIL        KLNA
333100960510     C* IMPOSTO LA DATA
333200960510     C     §OGEID        IFEQ      ' '
333300971126     C                   Z-ADD     WDATA8        KDAO                           DEL GIORNO
333400960510     C                   ELSE
333500960510     C                   Z-ADD     WDATAM        KDAO                           GIORNO PREC
333600960510     C                   ENDIF
333700960510     C*
333800990512     C     KANM4         SETLL     FNANM04L                               31
333900960422    6C     *IN31         IFEQ      *OFF
334000960423     C                   MOVEL     '1'           WMANID            1
334100960422     C*
334200960422     C* NON TROVATA STAMPO ERRORE
334300960422    7C     L             IFEQ      7
334400960422     C                   EXSR      ERRIDD
334500960422    7C                   ENDIF
334600050830     c
334700050830     C                   ADD       1             L
334800050830     C                   Z-ADD     ORGFIL        FID(L)
334900050831     C                   MOVEL     ORGDES        DFI(L)
335000050831     c                   clear                   dslv55
335100050830     c* Verifico se il £6
335200050830     c                   if        §ogntw<>'EEX' and §ogntw<>'DPD' and
335300050830     c                             §ogntw<>'FED' and §ogntw<>'EUP'
335400050830     C                   MOVEL     '6'           D55TPT
335500050831     c                   else
335600050831     c                   movel     'P'           D55tpt
335700050831     c                   endif
335800050830     C                   MOVEL     ORGFIL        D55LIN
335900050830     C                   Z-ADD     kdao          D55DRF
336000050830     C                   CALL      'FNLV55R'
336100050830     C                   PARM                    DSLV55
336200960422     C**
336300050831     c                   if        §ogntw<>'EEX' and §ogntw<>'DPD' and
336400050906     c                             §ogntw<>'FED' and §ogntw<>'EUP' and
336500050906     c                             orgfil<>101
336600050906     c
336700050830     c                   if        d55tfa=orgfil
336800050831     c* Imposto la decodifica del p.o. se non è in gestione ad altri
336900050831     C***                MOVEL     ORGDES        DFI(L)
337000050830     c                   else
337100050831     c* Imposto il p.o. gestione
337200050831     c****               eval      DFI(L)=CGestito
337300050831     c                   movel     d55tfa        w003a             3
337400050831     c                   eval      %subst(DFI(L):8:12)=CGESTITO
337500050831     c                   eval      %subst(DFI(L):20:3)=w003a
337600050830     c                   endif
337700050830     c
337800050830     c                   else
337900050906     c* Imposto il terminal di partenza del P.o estero o della 101
338000050906     c*  come gestione
338100050831     c***                eval      DFI(L)=CGestito
338200050831     c                   movel     d55tfp        w003a             3
338300050831     c                   eval      %subst(DFI(L):8:12)=CGESTITO
338400050831     c                   eval      %subst(DFI(L):20:3)=w003a
338500050830     c                   endif
338600960422     C*
338700960422    6C                   ENDIF
338800960422     C*
338900960423   5AC                   ENDIF
339000960423    5C                   ENDIF
339100960422    4C                   ENDIF
339200960422    3C                   ENDIF
339300960422     C*
339400960423     C                   READ      AZORG01L                               30
339500960422    2C                   ENDDO
339600960422     C*
339700960422     C* SE NE RIMANGONO FINISCO DI STAMPARE
339800960423    2C     L             IFGT      0
339900960422     C                   EXSR      ERRIDD
340000960423   X2C                   ELSE
340100960423     C* SE CI SONO TUTTI LO DICO
340200960423    3C     WMANID        IFEQ      ' '
340300960423     C*
340400960423    4C     *INOF         IFEQ      *ON
340500960423     C     *IN09         OREQ      *OFF
340600040615     c                   if        *inof
340700040615     C                   EXCEPT    TES
340800040615     C  n70              EXCEPT    TES2
340900040615     c                   endif
341000960423     C*
341100960423    5C     D34ELA        IFEQ      ' '
341200960423     C                   EXCEPT    TESIDR
341300960423     C                   ELSE
341400960423     C                   EXCEPT    TESIDE
341500960423    5C                   ENDIF
341600960423     C                   SETON                                        09
341700960423     C                   SETOFF                                       OF
341800960423    4C                   ENDIF
341900960423     C**
342000960423     C* TUTTI HANNO ESEGUITO L'IDD
342100960423     C                   EXCEPT    SIIDD
342200960423    3C                   ENDIF
342300960423     C*
342400960423    2C                   ENDIF
342500960422     C*
342600960422    1C                   ENDIF
342700960422     C*
342800960422     C                   SETOFF                                       08
342900960422     C                   ENDSR
343000960510     C*
343100960926     C* RICERCA DATA IDD-1 X I P.O. CHE LO FANNO IL GG DOPO----------*
343200990512     C     RICDAT        BEGSR
343300960510     C* UTILIZZO IL CALENDARIO
343400971126     C                   MOVE      WDATA8        G02INV
343500960510     C                   MOVEL     '3'           G02ERR
343600971126     C                   CALL      'XSRDA8'
343700960510     C                   PARM                    WLBDAT
343800960510     C*
343900960510     C                   MOVEL     'S'           WFEST             1
344000960510     C                   CLEAR                   KTFP
344100960510     C* CERCO IL PRIMO GIORNO LAVORATIVO PRECEDENTE
344200960510     C     WFEST         DOWEQ     'S'
344300960510     C                   SUB       1             G02TGI
344400990512     C                   Z-ADD     G02TGI        GI8TGI
344500990518     C                   CALL      'XSRGI8'
344600960510     C                   PARM                    WGIDAT
344700990512     C                   Z-ADD     GI8INV        WDATAM
344800960510     C*
344900960510     C                   CLEAR                   WFEST
345000960510     C     KCLN          CHAIN     AZCLN01L                           31
345100960510    1C     *IN31         IFEQ      *OFF
345200960510     C     POM(WGG)      ANDNE     *BLANKS
345300960510     C                   MOVEL     '1Y'          COD
345400960510     C                   MOVEL(P)  POM(WGG)      KEY
345500960510     C     KTAB          CHAIN     TABEL                              31
345600960510    2C     *IN31         IFEQ      *OFF
345700960510     C                   MOVEL     TBLUNI        DS1Y
345800960510    3C     §1YFEI        IFEQ      'S'
345900960510     C                   MOVEL     'S'           WFEST
346000960510    3C                   ENDIF
346100960510    2C                   ENDIF
346200960510    1C                   ENDIF
346300960510     C                   ENDDO
346400960510     C                   ENDSR
346500960422     C*
346600960422     C* STAMPA ERRORE NO IDD ----------------------------------------*
346700960422     C     ERRIDD        BEGSR
346800960422     C* SE OF ACCESO LA PRIMA VOLTA STAMPO LA TESTATA COMPLETA
346900960422    1C     *INOF         IFEQ      *ON
347000960423     C     *IN09         OREQ      *OFF
347100040615     c                   if        *inOF
347200040615     C                   EXCEPT    TES
347300040615     C  n70              EXCEPT    TES2
347400040615     c                   endif
347500960423     C*
347600960423     C     D34ELA        IFEQ      ' '
347700960423     C                   EXCEPT    TESIDR
347800960423     C                   ELSE
347900960423     C                   EXCEPT    TESIDE
348000960423     C                   ENDIF
348100960423     C                   SETON                                        09
348200960423     C                   SETOFF                                       OF
348300960422    1C                   ENDIF
348400960422     C*
348500960422     C                   EXCEPT    DETIDD
348600960422     C*
348700960422     C                   CLEAR                   FID
348800960422     C                   CLEAR                   DFI
348900960422     C                   CLEAR                   L
349000960422     C*
349100960422     C                   ENDSR
349200960404     C*
349300911021     C* INVIO MEMBRI TRASMISSIONE -----------------------------------*
349400911021     C     CAEX          BEGSR
349500050804     C* CREO ANOMALIA OK IDD SUL SISTEMA per tutti i p.o. della £6
349600000317     C     D34ELA        IFEQ      'S'
349700060619     c* apro il file anomalie di sede per scriverci l'anomalia 95
349800060619     c                   EXSR      opensed
349900060619     c
350000050804     C                   Z-ADD     1             LL                3 0
350100050804    2C     L6(LL)        DOWGT     0
350200050804     c
350300050804     C                   Z-ADD     L6(LL)        anmlna
350400050804     C                   Z-ADD     FGSTFP        D55TFP
350500911021     C                   Z-ADD     95            ANMCAA
350600960404     C                   EXSR      CRTANM
350700990512     C                   WRITE     FNANM000
350800060619     c                   if        %open(fnanmsed)
350900060619     C                   WRITE     FNANM0sd
351000060619     c                   endif
351100050804     c
351200050804     c                   add       1             LL
351300050804     c                   enddo
351400050804     c
351500060711    1C                   ENDIF
351600060711     c
351700060711     C* Trasmissione membri  solo se chi esegue è il terminal o se
351800060711     c*  un 2 livello che lo fa dopo il terminal
351900060711    0C     D34FGS        IFEQ      SIMFEL
352000060711     C     D34ALA        ORLE      FELDID
352100911021     C*
352200921029     C     *LOVAL        SETLL     AZORG
352300911021     C                   READ      AZORG                                  30
352400911021     C*
352500960604    1C     *IN30         DOWEQ     *OFF
352600971124    2C     ORGFAG        IFEQ      'F'
352700971124     C     ORGFAG        OREQ      'A'
352800971124    3C     ORGFVA        IFEQ      ' '
352900971124     C* PRENDO IL TERMINAL DI PARTENZA
353000971124     C                   CLEAR                   DSLV55
353100971124     C                   MOVEL     'P'           D55TPT
353200971124     C                   MOVEL     ORGFIL        D55LIN
353300980403     C                   MOVEL     AM8GIO        D55DRF
353400971124     C                   CALL      'FNLV55R'
353500971124     C                   PARM                    DSLV55
353600990517     C* SE NON TROVATO TERMINAL DI PARTENZA NON INVIO OK IDD
353700990517     C* (P.O. NON ANCORA OPERATIVO MA GIA' PRESENTE IN ORGANIGRAMMA)
353800990517    4C     D55ERR        IFEQ      *BLANKS
353900990517    4C     ORGFIL        ANDEQ     D55TFP
354000990331     C     ORGFIL        ANDNE     SIMFEL
354100120217     c
354200120217    4C     D55ERR        orEQ      *BLANKS
354300120217     C     ORGFIL        ANDEQ     D55TFP
354400120217     C     ORGFIL        ANDeq     SIMFEL
354500120217     c     d34ges        andeq     'V'
354600060711     c
354700990610     C* LA SEDE NON HA TERMINAL MA BISOGNA INVIARLA LO STESSO
354800060711     c* La sede non la invio più scrivo subito fnanm00f di sede
354900960409     C* SOLO PER FILIALI O AGENZIE
355000990512     C   99              CLOSE     FNANM00T
355100960604     C   99              SETOFF                                       99
355200960604     C* DISALLOCAZIONE
355300021009     c                   eval      D60po0 = SIMfel
355400960604     C                   MOVEL     SAVFL1        D60FL1
355500960604     C                   MOVEL     SAVFL2        D60FL2
355600971126     C                   MOVEL     AM8GIO        D60DRF
355700960604     C                   MOVEL     'D'           D60AZN
355800990512     C                   MOVEL     'FNANM00T'    D60FIL
355900021009     C                   MOVEL     FNLV60DS      KPJBU
356000960604     C                   CALL      'FNLV60R'
356100960604     C                   PARM                    KPJBA
356200960604     C                   CLEAR                   SAVFL1
356300960604     C                   CLEAR                   SAVFL2
356400960604     C** ALLOCAZIONE
356500021009     c                   eval      D60po0 = SIMfel
356600021008     c                   reset                   DSmembro
356700021008     c                   eval      MbrPOP = SIMfel
356800021008     c                   eval      MbrPOA = D55tfp
356900021008     c                   movel     DSmembro      VAR
357000960604     C                   CLEAR                   D60FL2
357100971124     C                   MOVEL     D55TFP        D60FL1
357200060711     C                   MOVEL     'X'           D60AZN
357300971126     C                   MOVEL     AM8GIO        D60DRF
357400971128     C                   MOVEL(P)  '*EXCL  '     D60VNC
357500021009     C                   MOVEL     FNLV60DS      KPJBU
357600960604     C                   CALL      'FNLV60R'
357700960604     C                   PARM                    KPJBA
357800021009     C                   MOVEL     KPJBU         FNLV60DS
357900960604     C**
358000960604     C* NON TROVATI ERRORI DI ALLOCAZIONE
358100971124    5C     D60ERR        IFEQ      'N'
358200971124     C                   MOVEL     D55TFP        SAVFL1
358300021009     c                   movel     D60mb1        DSmembro
358400021008     c                   movel     DSmembro      VAR
358500021009     c                   movel     D60mb1        wMBR
358600000317     C**
358700911025     C* PRENDO NUMERO RECORD
358800990512     C                   MOVEL     'FNANM00T'    PGM              10
358900911021     C                   EXSR      NUMREC
359000960405     C*
359100060711     C*
359200971124    6C     NRR           IFGT      0
359300060711     c* controllo spostato sopra
359400060711    7C***  D34FGS        IFEQ      SIMFEL
359500060711     C***  D34ALA        ORLE      FELDID
359600911021     C                   MOVEL     ORGFEL        §DST              3
359700971125     C*
359800020930     C                   CLEAR                   TIBS55DS
359900971125     C                   MOVEL     SIMFEL        I50PPO
360000971125     C                   MOVEL     ORGFIL        I50APO
360100020930     C                   CALL      'TIBS55R'
360200020930     C                   PARM                    TIBS55DS
360300971125     C*
360400971125     C                   MOVEL     O50PLA        CMZLBD
360500971125     C                   MOVEL     O50ALA        CMZLBA
360600971125     C                   MOVEL     O50ASI        CMZSYF
360700911021     C*
360800960405     C                   EXSR      X25ANM
360900060711    7C****               ENDIF
361000971124    6C                   ENDIF
361100960604    5C                   ENDIF
361200960604    4C                   ENDIF
361300960604    3C                   END
361400960604    2C                   END
361500911021     C*
361600911021     C                   READ      AZORG                                  30
361700960604    1C                   ENDDO
361800060711    0C                   ENDif
361900911021     C                   ENDSR
362000060619     C*
362100060619     C* open file anomalie di sede ----------------------------------*
362200060619     C     OPENSED       BEGSR
362300060619     C                   MOVEL     *BLANKS       COMMAN           80
362400060619     C                   MOVEL     OVRSED(1)     COMMAN
362500060706     C                   CALL      'QCMDEXC'                            98
362600060619     C                   PARM                    COMMAN
362700060619     C                   PARM                    LUNG
362800060619     C                   OPEN(E)   FNANMSED
362900060619     c* provo in gaitragrps
363000060619     C                   if        %error
363100060710     c                   eval      %subst(comman:38:11)='PS/FNANM00F'
363200060706     C                   CALL      'QCMDEXC'                            98
363300060619     C                   PARM                    COMMAN
363400060619     C                   PARM                    LUNG
363500060619     C                   OPEN(E)   FNANMSED
363600060619     c                   endif
363700060619     c
363800060619     C                   ENDSR
363900911021     C*
364000911021     C* CREO ANOMALIE ESEGUITO IDD ----------------------------------*
364100911021     C     CRTANM        BEGSR
364200911021     C                   MOVEL     *BLANKS       ANMATB
364300950922     C                   CLEAR                   ANMCDU
364400911021     C                   Z-ADD     0             ANMFLS
364500911021     C                   Z-ADD     0             ANMNRS
364600911021     C                   Z-ADD     0             ANMSCN
364700911021     C                   Z-ADD     0             ANMAAS
364800960404     C                   Z-ADD     0             ANMLNP
364900911021     C                   Z-ADD     0             ANMNSP
365000911021     C                   MOVEL     '7C'          COD
365100960404     C                   MOVEL(P)  ANMCAA        KEY
365200960409     C     KTAB          CHAIN(N)  TABEL                              31
365300911021     C  N31              MOVEL     TBLUNI        DS7C
365400911021     C   31              MOVEL     *BLANKS       DS7C
365500911021     C                   MOVEL     §7CTAN        ANMTAN
365600911021     C                   MOVEL     §7CSAN        ANMSAN
365700921027     C                   MOVEL     'D'           ANMFSA
365800990512     C                   MOVEL     D34ALA        ANMDAO
365900911021     C                   MOVEL     §7CAIE        ANMAIE
366000911025     C                   MOVEL     ' '           ANMFT1
366100911025     C                   MOVEL     *ZEROS        ANMFL1
366200000824     C                   MOVEL     *ZEROS        ANMDTR
366300911021     C* CHIUSURA AUTOMATICA
366400911021     C     §7CCHA        IFEQ      'S'
366500921027     C                   MOVEL     'D'           ANMFSC
366600990512     C                   MOVEL     D34ALA        ANMDCH
366700911021     C                   END
366800960509     C* DA INVIARE
366900911021     C     §7CAIE        IFEQ      'E'
367000971124     C                   MOVEL     D55TFP        ANMFL1
367100911025     C                   MOVEL     'T'           ANMFT1
367200000824     C                   MOVE      AM8GIO        ANMDTR
367300911021     C                   END
367400911021     C                   Z-ADD     0             ANMFL2
367500911021     C                   MOVEL     *BLANKS       ANMFT2
367600911021     C                   MOVEL     *BLANKS       ANMCCH
367700911021     C                   Z-ADD     0             ANMNPS
367800911021     C                   Z-ADD     0             ANMNFV
367900911021     C                   MOVEL     SIMFEL        ANMFLE
368000911021     C                   MOVEL     §7CLID        ANMLID
368100990512     C                   CLEAR                   ANMDUE
368200990512     C                   CLEAR                   ANMNPG
368300990512     C                   CLEAR                   ANMFGS
368400990512     C                   CLEAR                   ANMFLO
368500040601     c* data ora di creazione
368600040601     c                   z-add     *date         wdata             6 0
368700040601     c                   time                    wora              6 0
368800040607     c                   movel     wora          wora4             4 0
368900040601     c                   movel     wdata         anmflo
369000040607     c                   move      wora4         anmflo
369100040601     c
369200990512     C                   CLEAR                   ANMFT3
369300990512     C                   CLEAR                   ANMFL3
369400990512     C                   CLEAR                   ANMFT4
369500040601     C                   z-add     999           ANMFL4
369600911021     C*
369700911021     C                   ENDSR
369800911021     C*
369900990512     C** SOTTOMETTE TRASMISSIONE FNANM00T ---------------------------*
370000911021     C     X25ANM        BEGSR
370100911021     C                   MOVE      *BLANK        CMZPRR                          PRE-REQ.
370200911021     C                   MOVE      'S'           §CLR              1             CLEAR
370300911021     C                   MOVEL     §DST          CMZDST                          DESTINAT.
370400990512     C                   MOVEL     'FNANM00T'    CMZFLD                          DA: >>>
370500911021     C                   MOVEL     *BLANK        CMZMBD
370600021008     C                   MOVEL     wMBR          CMZMBD
370700990512     C                   MOVEL     'FNANM00R'    CMZFLA                          A: >>>
370800911021     C                   MOVEL     *BLANK        CMZMBA
370900021008     C                   MOVEL     wMBR          CMZMBA
371000911021     C                   MOVE      '0000'        CMZORI                          ORA
371100911021     C                   MOVE      '0000000'     CMZDTI                          DATA
371200040615     C**   D34TRA        IFEQ      'S'
371300040615     C**                 MOVE      'T'           CMZURG
371400040615     C**                 ELSE
371500911115     C                   MOVE      'N'           CMZURG                          ATTESA
371600040615     C**                 END
371700911021     C                   MOVE      'A'           CMZADD                          IN ADD
371800911021     C                   MOVE      §CLR          CMZCLR                          CLEAR
371900911021     C                   MOVE      'S'           CMZFTR                          DA TRANS.
372000911021     C                   MOVE      *BLANK        CMZERR                          1= ERR.
372100040615     C**   D34TRA        IFEQ      'S'
372200040615     C**                 MOVE      'FF20'        CMZAZN                          AZIONE
372300040615     C**                 ELSE
372400990518     C                   MOVE      'LR16'        CMZAZN                          AZIONE
372500040615     C**                 END
372600911021     C                   MOVE      NRR           CMZNRR                          N.REC.
372700020930     C                   CALL      'TIBS53C'
372800971125     C                   PARM                    O50LCL
372900971125     C                   PARM                    CMZDST
373000911021     C                   PARM                    CMZFLD
373100911021     C                   PARM                    CMZMBD
373200911021     C                   PARM                    CMZLBD
373300911021     C                   PARM                    CMZFLA
373400911021     C                   PARM                    CMZMBA
373500911021     C                   PARM                    CMZLBA
373600911021     C                   PARM                    CMZORI
373700911021     C                   PARM                    CMZDTI
373800911021     C                   PARM                    CMZURG
373900911021     C                   PARM                    CMZADD
374000911021     C                   PARM                    CMZCLR
374100911021     C                   PARM                    CMZFTR
374200911021     C                   PARM                    CMZERR
374300911021     C                   PARM                    CMZAZN
374400911021     C                   PARM                    CMZSYF
374500911021     C                   PARM                    CMZNRR
374600911021     C                   PARM                    CMZPRR
374700971125     C                   PARM                    W236            236
374800911021     C                   ENDSR
374900911021     C*
375000911021     C* PRELEVO NUMERO RECORD DAL MEMBRO DEL FILE --------------------*
375100911021     C     NUMREC        BEGSR
375200021008     ***C                   MOVEL     'M   '        VAR2              4
375300021008     ***C                   MOVE      D55TFP        VAR2
375400021008     ***C                   MOVEL     VAR2          ALF10
375500021008     c                   reset                   DSmembro
375600021008     c                   eval      MbrPOP = SIMfel
375700021008     c                   eval      MbrPOA = D55tfp
375800021008     c                   movel     DSmembro      ALF10
375900911021     C                   Z-ADD     0             NRR              10 0
376000921027     C                   CALL      'TRUL51C'
376100921027     C                   PARM                    KNSIF
376200921027     C                   PARM                    PGM
376300921027     C                   PARM                    ALF10            10
376400911021     C                   PARM                    NRR
376500911021     C                   ENDSR
376600980327     C*-------------------------------------------------------------------
376700980327     C* TRASFERIMENTO DA FILIALE A FILIALE
376800040615     C**   TRASFE        BEGSR
376900980327     C* SCRIVO IL RECORD NEL T
377000040615     C**   NODEL         IFEQ      'U'
377100040116     C******             MOVEL     WRTT          ANMAIE
377200980327     C*
377300040615     C**                 MOVEL     'T'           ANMFT1
377400040615     C**                 MOVE      AM8GIO        ANMDTR
377500040615     C**   ANMFL2        IFNE      *ZEROS
377600040615     C**                 MOVEL     'T'           ANMFT2
377700040615     C**                 END
377800040615     C**                 END
377900980327     C*
378000040615     C**   ANMCAA        IFEQ      70
378100040615     C*+   ANMCAA        OREQ      71
378200040116     C**   ANMAIE        IFEQ      'R'
378300040615     c**   wanom         ifeq      'P'
378400040615     C**   ANMCCH        ANDEQ     '  '
378500040615     C**                 CLEAR                   ANMDCH
378600040615     C**                 ENDIF
378700040615     C**                 ENDIF
378800980327     C*
378900040615     C**                 WRITE     FNANM0TT
379000040615     C**                 ADD       1             RECAN             5 0
379100980327     C*
379200980327     C* SE NON SONO IN FASE DECISIVA
379300040615     C**   D34AGG        IFEQ      'AGG'
379400040615     C**   NODEL         IFEQ      'U'
379500040615     C**                 MOVEL     WRTF          ANMAIE
379600040615     C**                 MOVEL     'T'           ANMFT1
379700040615     C**                 MOVE      AM8GIO        ANMDTR
379800040615     C**   ANMFL2        IFNE      *ZEROS
379900040615     C**                 MOVEL     'T'           ANMFT2
380000040615     C**                 END
380100040615     C**   ANMCAA        IFEQ      70
380200040615     C**   ANMCAA        OREQ      71
380300040116     C**   ANMAIE        IFEQ      'R'
380400040615     c**   wanom         ifeq      'P'
380500040615     C**   ANMCCH        ANDEQ     '  '
380600040615     C**                 CLEAR                   ANMDCH
380700040615     C**                 ENDIF
380800040615     C**                 ENDIF
380900040615     C**                 UPDATE    FNANM000
381000040615     C**                 END
381100980327     C*
381200040615     C**   NODEL         IFEQ      ' '
381300040615     C**                 DELETE    FNANM000
381400040615     C**                 END
381500980327     C*
381600040615     C**   NODEL         IFEQ      'S'
381700040615     C**                 UNLOCK    FNANM00F
381800040615     C**                 END
381900980327     C*
382000040615     C**                 END
382100980327     C*
382200040615     C**                 ENDSR
382300980327     C**-------------------------------------------------------------**
382400980327     C** ESEGUE CALL QCAEXEC SUL MEMBRO                              **
382500980327     C**-------------------------------------------------------------**
382600040615     C**   CAEX2         BEGSR
382700040615     C**                 MOVE      ')'           VAR
382800040615     C**                 Z-ADD     48            LUNG             15 5
382900040615     c**                 clear                   DSmembro
383000040615     c**                 eval      MbrM   = 'M'
383100040615     c**                 eval      MbrPOP = SIMfel
383200040615     c**                 eval      MbrPOA = D34fgs
383300040615     c**                 movel     DSmembro      VAR
383400980327     C** FILIALE DI PARTENZA PER APERTURA MEMBRO
383500980327     C** CONTROLLO SE ESISTE IL MEMBRO
383600040615     C**                 SETOFF                                       20        FNANM00T
383700040615     C**                 MOVE      VAR           CMD5
383800040615     C**                 MOVEL     *BLANKS       COMMAN           80
383900040615     C**                 MOVEA     CMD5(1)       COMMAN
384000040615     C**                 CALL      'QCMDEXC'                            20
384100040615     C**                 PARM                    COMMAN
384200040615     C**                 PARM                    LUNG
384300980327     C** CONTROLLO SE ESISTE IL MEMBRO
384400040615     C** 20              DO
384500040615     C**                 MOVE      VAR           CMD3
384600040615     C**                 MOVEL     *BLANKS       COMMAN
384700040615     C**                 MOVEA     CMD3(1)       COMMAN
384800040615     C**                 CALL      'QCMDEXC'
384900040615     C**                 PARM                    COMMAN
385000040615     C**                 PARM                    LUNG
385100040615     C**                 END
385200980327     C** CONTROLLO SE ESISTE IL MEMBRO
385300980327     C** ESEGUE IN OGNI CASO OVRDBF DEL MEMBRO DESIDERATO
385400040615     C**                 MOVE      VAR           CMD1
385500040615     C**                 MOVEL     *BLANKS       COMMAN
385600040615     C**                 MOVEA     CMD1(1)       COMMAN
385700040615     C**                 CALL      'QCMDEXC'
385800040615     C**                 PARM                    COMMAN
385900040615     C**                 PARM                    LUNG
386000980327     C*
386100040615     C**                 OPEN      FNANM00T
386200040615     C**                 ENDSR
386300001030     C*-------------------------------------------------------------------
386400001030     C* TRASMISSIONE SCHEDULATA ANOMALIE (ESTERNE NON TRASMESSE)
386500001030     C     SCHANM        BEGSR
386600001102     C* N.B. LEGGO SOLO LE ANOMALIE ESTERNE VISTO CHE PER IL MOMENTO
386700001102     C*      LE AZIONI CHE PREVEDONO L'INVIO SCHEDULATO SONO TUTTE
386800001102     C*      ANOMALIE ESTERNE
386900001102     C                   MOVEL     'E'           KAIE
387000001030     C                   MOVEL     '7C'          COD
387100001030     C     KTAB2         SETLL     TABEL
387200001030     C     KTAB2         READE     TABEL                                  30
387300001030     C     *IN30         DOWEQ     *OFF
387400001030     C                   MOVEL     TBLUNI        DS7C
387500001030     C     §7CINV        IFEQ      'S'
387600001102     C                   MOVEL     TBLKEY        KCAA
387700001030     C     KAN11         SETLL     FNANM11L
387800001030     C     KAN11         READE     FNANM11L                               30
387900001030     C     *IN30         DOWEQ     *OFF
388000001030     C                   EXSR      ELAB
388100001030     C     KAN11         READE     FNANM11L                               30
388200001030     C                   ENDDO
388300001030     C                   ENDIF
388400001030     C     KTAB2         READE     TABEL                                  30
388500001030     C                   ENDDO
388600001030     C                   EXSR      CAEX
388700041020     c
388800041020     c* Leggo le anomalie 145 146 per le quali devo scrivere fnant
388900041020     c* sbr   spostata dal pgm fnlr76r che viene eliminato
389000041020     C                   MOVEL     'I'           KAIE
389100041020     c                   z-add     145           kcaa
389200041020     c                   do        2
389300041020     c     kan11         setll     fnanm11l
389400041020     C     KAN11         READE     FNANM11L
389500041020     c                   dow       not %eof(fnanm11l)
389600041020     c                   if        anmcch<>'AN'
389700041020     c                   EXSR      SCRANT
389800041020     c                   update    fnanm011
389900041020     C                   ENDIF
390000041020     c
390100041020     C     KAN11         READE     FNANM11L
390200041020     c                   enddo
390300041020     c                   z-add     146           kcaa
390400041020     c                   enddo
390500001030     C                   ENDSR
390600031007     C*-------------------------------------------------------------------
390700031007     C* WRIDET - ROUTINE SCRITTURA WRK-FILE WFLAN00F SU STAMPA FORMATO "DET"
390800031007     C*-------------------------------------------------------------------
390900031007     C     WRIDET        BEGSR
391000031007     C*
391100031007     C* Inizializzo il buffer del formato record
391200031007     C                   CLEAR                   WFLAN000
391300031007     C*
391400031031     C                   EVAL      WLANFGS = D34FGS
391500031007     C                   EVAL      WLANPOA = D34PAR
391600031007     C                   EVAL      WLANAAS = ANMAAS
391700031007     C                   EVAL      WLANLNP = ANMLNP
391800031007     C                   EVAL      WLANNRS = ANMNRS
391900031007     C                   EVAL      WLANNSP = ANMNSP
392000031007     C                   EVAL      WLANLNA = ANMLNA
392100031007     C                   MOVE      PRTDSP        WLANDSP
392200031007     C                   EVAL      WLANFLS = ANMFLS
392300031007     C                   EVAL      WLANSCN = ANMSCN
392400031007     C                   MOVE      PRTZNC        WLANZNC
392500031007     C                   EVAL      WLANNPS = ANMNPS
392600031007     C                   EVAL      WLANAIE = DESAIE
392700031007     C                   EVAL      WLANDEA = DESTAN
392800031007     C                   EVAL      WLANCAA = ANMCAA
392900031007     C                   EVAL      WLANDEC = DESCAA
393000031007     C                   EVAL      WLANNDC = ARBNDC
393100031007     C                   MOVE      STADDC        WLANDDC
393200031007     C                   EVAL      WLANPDC = STAPDC
393300031007     C* Se già valorizzato il campo PSTAM compondo la descrizione
393400031007     C                   IF        PSTAM <> *blanks
393500031007     C                   EVAL      WLANCO1 = PSTAM
393600031007     C                   MOVE      WDFLP         WDFLPALFA         3
393700031007     C                   EVAL      %subst(WLANCO1:13:3) = WDFLPALFA
393800031007     C                   MOVE      PRTDET        WPRTDET           4
393900031007     C                   EVAL      WLANCO1 = %trim(WLANCO1) + ' ' +
394000031007     C                                       %subst(WPRTDET:1:2)  + '/' +
394100031007     C                                       %subst(WPRTDET:3:2)
394200031007     C***                EVAL      WLANFLP = WDFLP
394300031007     C***                MOVE      PRTDET        WLANDET
394400031007     C                   ENDIF
394500031007     C*
394600031007     C* Se valorizzato il campo WFGS compondo la descrizione
394700031007     C                   IF        WFGS <> *zeros
394800031007     C                   EVAL      WLANCO2 = CDOVE
394900031007     C                   MOVE      WFGS          WDFLPALFA
395000031007     C                   EVAL      %subst(WLANCO2:13:3) = WDFLPALFA
395100031007     C                   EVAL      %subst(WLANCO2:5:5) = PRTDOV
395200031007     C                   MOVE      PRTDFV        WPRTDET
395300031007     C                   EVAL      WLANCO2 = %trim(WLANCO2) + ' ' +
395400031007     C                                       %subst(WPRTDET:1:2)  + '/' +
395500031007     C                                       %subst(WPRTDET:3:2)
395600031007     C***                EVAL      WLANDOV = PRTDOV
395700031007     C***                EVAL      WLANFGS = WFGS
395800031007     C***                MOVE      PRTDFV        WLANDFV
395900031007     C                   ENDIF
396000031007     C*
396100031007     C* Se valorizzato il numero C.A. compongo la descrizione
396200031007     C                   IF        DCTNCA > *zeros
396300040616     C                   EVAL      WLANCO3 = CCOAN1
396400031007     C                   MOVE      DCTFIL        WDFLPALFA
396500031007     C                   EVAL      %subst(WLANCO3:8:3) = WDFLPALFA
396600031007     C                   MOVE      DCTNCA        WNCAALFA          7
396700031007     C                   EVAL      %subst(WLANCO3:12:7) = WNCAALFA
396800031007     C                   MOVE      STADCA        WDCAALFA          6
396900031007     C                   EVAL      WLANCO3 = %trim(WLANCO3) + ' ' +
397000031007     C                                       %subst(WDCAALFA:1:2)  + '/' +
397100031007     C                                       %subst(WDCAALFA:3:2)  + '/' +
397200031007     C                                       %subst(WDCAALFA:5:2)
397300031007     C***                EVAL      WLANFIL = DCTFIL
397400031007     C***                EVAL      WLANNCA = DCTNCA
397500031007     C***                MOVE      DCTDCA        WLANDCA
397600031007     C                   ENDIF
397700031007     C*
397800031007     C                   EVAL      WLANLID = ANMLID
397900031007     C                   EVAL      WLANDFA = DESFSA
398000031007     C                   MOVE      STADAO        WLANDAA
398100031007     C                   EVAL      WLANCDU = ANMCDU
398200031007     C                   EVAL      WLANNFV = ANMNFV
398300031007     C                   EVAL      WLANDFC = DESFSC
398400031007     C                   MOVE      STADCH        WLANDAC
398500031007     C                   MOVE      CCHPRT        WLANCCH
398600031007     C                   EVAL      WLANDCC = DESCCH
398700031007     C                   EVAL      WLANFLE = ANMFLE
398800031007     C                   EVAL      WLANCHA = DCTDCH
398900031007     C*
399000031007     C* SCarico il buffer sul file
399100031007     C                   WRITE     WFLAN000
399200031007     C*
399300031007     C                   ENDSR
399400031007     C*-------------------------------------------------------------------
399500031007     C* DELWF  - ROUTINE DI CANCELLAZIONE WRK-FILE WFLAN00F X LANCIO CORRENTE
399600031007     C*-------------------------------------------------------------------
399700031007     C     DELWF         BEGSR
399800031007     C*
399900031007     C     KLAN01        SETLL     WFLAN01L
400000031007     C                   IF        %equal(WFLAN01L)
400100031007     C     KLAN01        READE     WFLAN01L
400200031007     C                   DOW       not %eof(WFLAN01L)
400300031007     C                   DELETE    WFLAN01L
400400031007     C     KLAN01        READE     WFLAN01L
400500031007     C                   ENDDO
400600031007     C                   ENDIF
400700031007     C*
400800031007     C                   ENDSR
400900960327     C****************************************************************
401000960327     C     *INZSR        BEGSR
401100960327     C*
401200960327     C     *ENTRY        PLIST
401300960327     C                   PARM                    KPJBA
401400960327     C                   MOVEL     KPJBU         DSLR34
401500960327     C                   Z-ADD     1             CODUT
401600960327     C                   CALL      'X§PARUT'
401700020905     C                   PARM                    UT§DSE0F
401800960327     C                   MOVEL     RAGUT         RSUT             20
401900960327     C                   MOVEL     REC80         CNCR80
402000031007     C*
402100031007     C* Verifico subito se per il lancio richiesto è prevista anche la creazione del wrk-file
402200031009     C* Lista Anomalie (WFLAN00F)
402300111201     C                   IF        DSLR34 <> *blanks  and                       * NO x trasmisisone
402400111201     c                             parsch<>'V'
402500031009     C***                IF        D34IE = 'D'                                  * solo anomalie IDD
402600031007     C* Imposto a NO l'indicatore d condizionamento creazoine wrk-file
402700031007     C                   SETOFF                                       55
402800031007     C* Verifico se x il cliente richiesto nel lancio è prevista la creazine del wrk-file
402900031007     C                   EVAL      WD34POS = D34POS
403000031007     C                   EVAL      WD34NRS = D34NRS
403100031007     C                   EVAL      TBECOD = 'LAN'
403200031007     C                   EVAL      TBEKE1 = DSKEYLAN
403300031007     C     KTBE01P       CHAIN     TNTBE01L
403400031007     C                   IF        %found(TNTBE01L)
403500031007     C* Se trovo il cliente in tabella, verifico i flag x le abilitazioni elaborazione
403600031007     C* anomalie in partenza e/o in arrivo
403700031007     C                   EVAL      DLAN = TBEUNI
403800031007     C                   IF        (§LANPAR = 'S' AND D34PAR = 'P') OR
403900031007     C                             (§LANARR = 'S' AND D34PAR = 'A')
404000031007     C                   SETON                                        55
404100031031     C                   OPEN      WFLAN01L
404200031007     C                   EXSR      DELWF
404300031007     C                   ENDIF
404400031007     C                   ENDIF
404500031007     C***                ENDIF
404600031009     C                   ENDIF
404700960327     C*-----------------------------------------------------
404800960327     C     KTAB          KLIST
404900960327     C                   KFLD                    CODUT
405000960327     C                   KFLD                    COD               2
405100960327     C                   KFLD                    KEY               8
405200031007     C     KTBE01P       KLIST                                                  * Parziale
405300031007     C                   KFLD                    TBECOD
405400031007     C                   KFLD                    TBEKE1
405500031007     C     KLAN01        KLIST                                                  * Completa
405600031031     C                   KFLD                    D34FGS
405700031007     C                   KFLD                    D34POS
405800031007     C                   KFLD                    D34NRS
405900960327     C     KTAB2         KLIST
406000960327     C                   KFLD                    CODUT
406100960327     C                   KFLD                    COD               2
406200960927     C     KFVA          KLIST
406300960927     C                   KFLD                    BRVFGS
406400000201     C                   KFLD                    WNFV
406500960510     C     KCLN          KLIST
406600960510     C                   KFLD                    KTFP
406700960510     C                   KFLD                    KLNA
406800990512     C                   KFLD                    WAA
406900990512     C                   KFLD                    WMM
407000960417     C     KANM          KLIST
407100990512     C                   KFLD                    ANMAAS
407200960417     C                   KFLD                    ANMLNP
407300960417     C                   KFLD                    ANMNRS
407400960417     C                   KFLD                    ANMNSP
407500020614     C     KBTP          KLIST
407600020614     C                   KFLD                    SIMFEL
407700020614     C                   KFLD                    ANMAAS
407800020614     C                   KFLD                    ANMLNP
407900020614     C                   KFLD                    ANMNRS
408000020614     C                   KFLD                    ANMNSP
408100960405     C     KANM1         KLIST
408200960405     C                   KFLD                    KAAS2
408300960405     C                   KFLD                    KLNP2
408400960405     C                   KFLD                    KNRS2
408500960405     C                   KFLD                    KNSP2
408600960404     C                   KFLD                    KCAA
408700960509     C     KANM2         KLIST
408800960509     C                   KFLD                    ANMFLS
408900960509     C                   KFLD                    ANMLNA
409000960509     C                   KFLD                    ANMNRS
409100960509     C                   KFLD                    ANMSCN
409200960509     C                   KFLD                    KCAA
409300021021     C     KANM20        KLIST
409400021021     C                   KFLD                    KFIL
409500021021     C                   KFLD                    KCAA
409600970529     C     KANM3         KLIST
409700970529     C                   KFLD                    KAIE
409800970529     C                   KFLD                    KCDU
409900970529     C                   KFLD                    ANMLNP
410000970529     C     KANM3A        KLIST
410100970529     C                   KFLD                    KAIE
410200970529     C                   KFLD                    KCDU
410300970529     C     KANM3B        KLIST
410400970529     C                   KFLD                    KAIE
410500970529     C                   KFLD                    KCDU
410600970529     C                   KFLD                    KLNP
410700970529     C                   KFLD                    ANMFLS
410800970529     C     KANM3D        KLIST
410900970529     C                   KFLD                    KAIE
411000970529     C                   KFLD                    KCDU
411100970529     C                   KFLD                    KLNP
411200960422     C     KANM4         KLIST
411300960422     C                   KFLD                    KCAA
411400960422     C                   KFLD                    KDAO
411500960423     C                   KFLD                    KLNA
411600960404     C     KANM5         KLIST
411700960404     C                   KFLD                    KCAA
411800960404     C                   KFLD                    KFIL
411900960412     C     KANM51        KLIST
412000960412     C                   KFLD                    KCAA
412100960412     C                   KFLD                    KFIL
412200960412     C                   KFLD                    KLNP
412300960412     C     KANM52        KLIST
412400960412     C                   KFLD                    KCAA
412500960412     C                   KFLD                    KFIL
412600960412     C                   KFLD                    KLNP
412700960412     C                   KFLD                    KFLS
412800960327     C     KANM7         KLIST
412900960327     C                   KFLD                    KAIE
413000960327     C                   KFLD                    KFIL
413100960327     C                   KFLD                    KLNA
413200960327     C     KANM71        KLIST
413300960327     C                   KFLD                    KAIE
413400960327     C                   KFLD                    KFIL
413500960327     C     KANM72        KLIST
413600960327     C                   KFLD                    KAIE
413700960327     C                   KFLD                    KFIL
413800960327     C                   KFLD                    KLNA
413900960327     C                   KFLD                    KDCH
414000960327     C     KANM8         KLIST
414100960327     C                   KFLD                    KAIE
414200960327     C                   KFLD                    KFIL
414300960327     C                   KFLD                    KLNP
414400960327     C                   KFLD                    KLNA
414500960327     C     KANM82        KLIST
414600960327     C                   KFLD                    KAIE
414700960327     C                   KFLD                    KFIL
414800960327     C                   KFLD                    KLNP
414900960327     C                   KFLD                    KLNA
415000960327     C                   KFLD                    KDCH
415100960327     C     KANM9         KLIST
415200960327     C                   KFLD                    KAIE
415300960327     C                   KFLD                    KFIL
415400960327     C                   KFLD                    KLNP
415500960327     C     KANM93        KLIST
415600960327     C                   KFLD                    KAIE
415700960327     C                   KFLD                    KFIL
415800960327     C                   KFLD                    KLNP
415900960327     C                   KFLD                    KFLS
416000960327     C                   KFLD                    KDCH
416100960327     C     KANM94        KLIST
416200960327     C                   KFLD                    KAIE
416300960327     C                   KFLD                    KFIL
416400960327     C                   KFLD                    KLNP
416500960327     C                   KFLD                    KFLS
416600001102     C     KAN11         KLIST
416700001102     C                   KFLD                    KAIE
416800001102     C                   KFLD                    KCAA
416900960327     C     KART          KLIST
417000960327     C                   KFLD                    ANMFLS
417100960327     C                   KFLD                    ANMLNA
417200960327     C                   KFLD                    ANMNRS
417300960327     C                   KFLD                    ANMSCN
417400960327     C     KARB          KLIST
417500960327     C                   KFLD                    ARTAAS
417600960327     C                   KFLD                    ARTLNP
417700960327     C                   KFLD                    ARTNRS
417800960327     C                   KFLD                    ARTNSP
417900960412     C     KBLP          KLIST
418000960412     C                   KFLD                    BLTAAS
418100960412     C                   KFLD                    BLTLNP
418200960412     C                   KFLD                    BLTNRS
418300960412     C                   KFLD                    BLTNSP
418400960508     C     KFVV          KLIST
418500960508     C                   KFLD                    BRVNPG
418600000201     C                   KFLD                    WNFV
418700960508     C                   KFLD                    BRVFGS
418800960704     C     KBRV          KLIST
418900960704     C                   KFLD                    KNPG
419000960704     C                   KFLD                    ANMFLS
419100960704     C                   KFLD                    ANMLNA
419200960704     C                   KFLD                    ANMNRS
419300960704     C                   KFLD                    ANMSCN
419400960927     C     KBRV3         KLIST
419500960927     C                   KFLD                    KNPG
419600960927     C                   KFLD                    ANMFLS
419700960927     C                   KFLD                    ANMLNA
419800960927     C                   KFLD                    ANMNRS
419900960927     C                   KFLD                    ANMSCN
420000960927     C                   KFLD                    ARTFLP
420100970604     C     KBRV3A        KLIST
420200970604     C                   KFLD                    KNPG
420300970604     C                   KFLD                    ANMFLS
420400970604     C                   KFLD                    ANMLNA
420500970604     C                   KFLD                    ANMNRS
420600970604     C                   KFLD                    ANMSCN
420700970604     C                   KFLD                    SIMFEL
420800970306     C     KA71          KLIST
420900970306     C                   KFLD                    ANMFLE
421000990512     C                   KFLD                    ANMDAO
421100970305     C                   KFLD                    ANMCAA
421200970306     C                   KFLD                    ANMLNP
421300970306     C     KA55          KLIST
421400970306     C                   KFLD                    ANMFLE
421500990512     C                   KFLD                    ANMDAO
421600970306     C                   KFLD                    ANMCAA
421700040504     C                   KFLD                    wlnp
421800040405     C     KA200         KLIST
421900040405     C                   KFLD                    ANMFLE
422000040405     C                   KFLD                    wdta200
422100040405     C                   KFLD                    ANMCAA
422200040504     C                   KFLD                    wlnp
422300041020     C     KANT          KLIST
422400041020     C                   KFLD                    KFOA
422500041020     C                   KFLD                    ANMDAO
422600041020     C                   KFLD                    ANMCAA                                     NE
422700041020     C                   KFLD                    KANLNP                                     NE
422800041020     C                   KFLD                    ANMLNA                                     NE
422900990122     C     KDCT          KLIST
423000990122     C                   KFLD                    DCDAAC
423100990122     C                   KFLD                    DCDFIL
423200990122     C                   KFLD                    DCDNCA
423300020807     C     K02FGV01      klist
423400020807     C                   kfld                    BLTnfv
423500020926     C                   kfld                    SIMfel
423600021203
423700021203     C     Kfapd         klist
423800021203     C                   kfld                    kpdtip
423900021203     C                   kfld                    kpdpdr
424000040615
424100040615     C     Kcae          klist
424200040615     C                   kfld                    kcaeepa
424300040615     C                   kfld                    kcaetfp
424400040615     C                   kfld                    kcaetfe
424500040615     C                   kfld                    kcaedde
424600040615     C     Kcae2         klist
424700040615     C                   kfld                    kcaeepa
424800040615     C                   kfld                    kcaetfp
424900040615     C                   kfld                    kcaetfe
425000040616     C     Kcae1         klist
425100040616     C                   kfld                    kcaeepa
425200040616     C                   kfld                    kcaetfp
425300040616     C                   kfld                    kcaetfa
425400040616     C                   kfld                    kcaedde
425500040616     C     Kcae3         klist
425600040616     C                   kfld                    kcaeepa
425700040616     C                   kfld                    kcaetfp
425800040616     C                   kfld                    kcaetfa
425900020905     ***C* DEFINIZIONE CAMPI
426000020905     ***C     *LIKE         DEFINE    FVVFGS        FGSTFP
426100020905     ***C     *LIKE         DEFINE    FVVFGS        WLNA
426200020905     ***C     *LIKE         DEFINE    D60ERR        WERR
426300020905     ***C     *LIKE         DEFINE    D60MB1        WMB1
426400020905     ***C     *LIKE         DEFINE    D60MB2        WMB2
426500020905     ***C     *LIKE         DEFINE    ANMFL1        SAVFL1
426600020905     ***C     *LIKE         DEFINE    ANMFL2        SAVFL2
426700020905     ***C     *LIKE         DEFINE    CLNTFP        KTFP
426800020905     ***C     *LIKE         DEFINE    CLNANN        KANN
426900020905     ***C     *LIKE         DEFINE    CLNMES        KMES
427000020905     ***C     *LIKE         DEFINE    §7ADID        FELDID
427100020905     ***C     *LIKE         DEFINE    ANMDCH        KDCH
427200020905     ***C     *LIKE         DEFINE    ANMLNP        KFIL
427300020905     ***C     *LIKE         DEFINE    ANMLNP        KLNP
427400020905     ***C     *LIKE         DEFINE    ANMLNP        KLNP9
427500020905     ***C     *LIKE         DEFINE    ANMLNP        KLNP92
427600020905     ***C     *LIKE         DEFINE    ANMLNP        KFLS
427700020905     ***C     *LIKE         DEFINE    ANMLNA        KLNA
427800020905     ***C     *LIKE         DEFINE    ANMLNA        SAVLNA
427900020905     ***C     *LIKE         DEFINE    ANMLNA        KLNA7
428000020905     ***C     *LIKE         DEFINE    ANMLNA        KLNA8
428100020905     ***C     *LIKE         DEFINE    ANMAIE        KAIE
428200020905     ***C     *LIKE         DEFINE    ANMCAA        KCAA
428300020905     ***C     *LIKE         DEFINE    ANMCAA        SAVCAA
428400020905     ***C     *LIKE         DEFINE    ANMCAA        WCAA
428500020905     ***C     *LIKE         DEFINE    ANMAAS        KAAS2
428600020905     ***C     *LIKE         DEFINE    ANMLNP        KLNP2
428700020905     ***C     *LIKE         DEFINE    ANMNRS        KNRS2
428800020905     ***C     *LIKE         DEFINE    ANMNSP        KNSP2
428900020905     ***C     *LIKE         DEFINE    ANMDAO        KDAO
429000020905     ***C     *LIKE         DEFINE    D53DFV        W53DFV
429100020905     ***C     *LIKE         DEFINE    ANMNFV        SAVNFV
429200020905     ***C     *LIKE         DEFINE    ANMCCH        CCHPRT
429300020905     ***C     *LIKE         DEFINE    ANMCDU        KCDU
429400020905     ***C     *LIKE         DEFINE    ARTDET        WDDET
429500020905     ***C     *LIKE         DEFINE    ARTFLP        WDFLP
429600020905     ***C     *LIKE         DEFINE    BRVNPG        KNPG
429700020905     ***C     *LIKE         DEFINE    FVVNFV        WNFV
429800020905     ***C     *LIKE         DEFINE    FVVNPG        WNPG
429900020905     ***C     *LIKE         DEFINE    FVVSPG        WSPG
430000020905     ***C     *LIKE         DEFINE    FVVDFV        WDFV
430100020905     ***C     *LIKE         DEFINE    FVVFGS        WFGS
430200020905     ***C     *LIKE         DEFINE    TBLKEY        WMAX
430300960417     C*
430400960402     C* UDATE IN GGMMAAAA
430500960417     C                   TIME                    W0140            14 0
430600960402     C                   MOVE      W0140         WDTGIO            8 0
430700960422     C*
430800960422     C* PRENDO DATA IN AAMMGG
430900960422     C                   MOVEL     WDTGIO        G02DAT
431000960422     C                   MOVEL     0             G02ERR
431100971126     C                   CALL      'XSRDA8'
431200960422     C                   PARM                    WLBDAT
431300971126     C                   MOVE      G02INV        AM8GIO            8 0
431400980327     C* SE RICHIESTO TRASFERIMENTO APRO I MEMBRI
431500040615     C**   D34TRA        IFEQ      'S'
431600040615     C**                 EXSR      CAEX2
431700040615     C**                 END
431800001102     C* SE AZIONE SCHEDULATA IMPOSTO D34GES E D34FGS
431900001102     C     PARSCH        IFEQ      'V'
432000001102     C                   CLEAR                   DSLR34
432100001102     C                   MOVEL     'V'           D34GES
432200001102     C                   Z-ADD     SIMFEL        D34FGS
432300001102     C                   ENDIF
432400960327     C                   ENDSR
432500041020     C**************************************************************************
432600041020     C* SCRITTURA DEL FILE FNANT00F
432700041020     C**************************************************************************
432800041020     C     SCRANT        BEGSR
432900041020     C**
433000041020     C* PER L'ANOMALIA 145  FOA = ANMFLE
433100041020     C* PER L'ANOMALIA 146  FOA  = TERMINAL DI ARRIVO
433200041020    2C     ANMCAA        IFEQ      145
433300041020     C                   Z-ADD     ANMFLE        KFOA
433400041020   X2C                   ELSE
433500041020     C*
433600041020     C                   CLEAR                   KFOA
433700041020     C* PRENDO FLS SE LNP =0
433800041020     C*
433900041020     C                   CLEAR                   DSLV55
434000041020     C                   MOVEL     'A'           D55TPT
434100041020     C                   Z-ADD     ANMLNA        D55LIN
434200041020     C                   Z-ADD     ANMDAO        D55DRF
434300041020    3C     ANMLNP        IFGT      0
434400041020     C                   Z-ADD     ANMLNP        d55lnp
434500041020     C                   ELSE
434600041020     C                   Z-ADD     ANMFLS        d55lnp
434700041020    3C                   ENDIF
434800041020     C                   CALL      'FNLV55R'
434900041020     C                   PARM                    DSLV55
435000041020     C                   Z-ADD     D55TFA        KFOA
435100041020    2C                   ENDIF
435200041020     C*
435300041020     C     KANT          CHAIN     FNANT02L                           39
435400041020     C*
435500041020    2C     *IN39         IFEQ      *ON
435600041020     C                   CLEAR                   FNANT000
435700041020     C                   MOVEL     KFOA          ANTFOA
435800041020     C                   MOVEL     ANMLNA        ANTLNA
435900041020     C                   MOVEL     ANMDAO        ANTDAO
436000041020     C                   MOVEL     ANMCAA        ANTCAA
436100041020     C                   Z-ADD     1             ANTTOT
436200041020     c                   z-add     *date         antdt1
436300041020     C                   WRITE     FNANT000
436400041020   X2C                   ELSE
436500041020     C                   ADD       1             ANTTOT
436600041020     C                   CLEAR                   ANTFT1
436700041020     C                   CLEAR                   ANTFT2
436800041020     c                   z-add     *date         antdt1
436900041020     C                   UPDATE    FNANT000
437000041020    2C                   ENDIF
437100041020     C*
437200041020     C                   MOVEL     'T'           ANMFT1
437300041020     C                   ENDSR
437400911021     C*
437500960422     O* TESTATA IDD
437600911021     OPRTF198   E            TES              02
437700960604     O                       STA(1)              66
437800960604     O              N13      STA(2)             132
437900040614     o               13      STA(19)            132
438000921015     O                       STA(3)             198
438100900626     O                       RSUT                20
438200040615     O                       prtpo               46
438300040615     O                       D34FGS              50
438400960926     O                       PRTFGS              71
438500960402     O                       WDTGIO             168 '  .  .    '
438600921029     O                       PAGE1         Z    198
438700921015     O          E            TES         1
438800040615     O               70      STA(34)            132
438900960402     O                       UTIME              167 '  :  :  '
439000960402     O                  01                      190 '-C O N F E R M A-'
439100040615     O          E            TES         1
439200960402     O                       STA(18)             66
439300960402     O                       STA(20)            132
439400960402     O                       STA(21)            198
439500960506     O                       DESIE               61
439600960402     O                       D34DLG             106 '  /  /    '
439700960402     O                       D34ALG             123 '  /  /    '
439800960403     O                       D34DCG             172 '  /  /    '
439900960402     O                       D34ACG             189 '  /  /    '
440000030717     C*
440100040615     O          E            TES2        1
440200960403     O                  15   STA(16)             66
440300960403     O                 N15   STA(17)            132
440400960403     O                 N18   STA(15)            198
440500960403     O                  15   DESFV               29
440600990513     O                  15   D34NFV        Z     44
440700990513     O                  15   STADFV              61 '  /  /    '
440800980403     O                 N15   TESFSA             103
440900040611     O                 N18   D34LID             198
441000040614     O                                          120 'LINEA ARRIVO :'
441100980403     O               06      D34LNA             124
441200040614     O               06      PRTLNA             141
441300040614     O                                          154 'TIPO MERCE :'
441400040614     O                       PRTVAL             163
441500030717     O*
441600040615     O          E    23      TES2        1
441700030717     O                       STA(33)            132
441800030717     O                       D34POS              87
441900030717     O                       D34NRS             110
442000960403     O*
442100040615     O          E   N14 19   TES2        1
442200960426     O                       STA(32)             66
442300960426     O                       D34CCH              29
442400960426     O                                         +  0 '-'
442500960426     O                       TESCCH            +  0
442600040615     O          E    14      TES2        1
442700960426     O                  19   STA(32)             66
442800960426     O                       STARIG             132
442900960426     O                  19   D34CCH              29
443000960426     O                                         +  0 '-'
443100960426     O                  19   TESCCH            +  0
443200960418     O                       ANM(1)              94
443300960403     O                       ANM(2)            +  3
443400960403     O                       ANM(3)            +  3
443500960403     O                       ANM(4)            +  3
443600960403     O                       ANM(5)            +  3
443700960403     O                       ANM(6)            +  3
443800960403     O                       ANM(7)            +  3
443900960403     O                       ANM(8)            +  3
444000960403     O                       ANM(9)            +  3
444100960403     O                       ANM(10)           +  3
444200040615     o*
444300040615     O          E    80      TES3        1
444400040615     O                       STA(35)             66
444500040615     o                       stpo(1)             20
444600040615     o                       stpo(2)           +  1
444700040615     o                       stpo(3)           +  1
444800040615     o                       stpo(4)           +  1
444900040615     o                       stpo(5)           +  1
445000040615     o                       stpo(6)           +  1
445100040615     o                       stpo(7)           +  1
445200040615     o                       stpo(8)           +  1
445300040615     o                       stpo(9)           +  1
445400040615     o                       stpo(10)          +  1
445500040615     o                       stpo(11)          +  1
445600040615     o                       stpo(12)          +  1
445700040615     o                       stpo(13)          +  1
445800040615     o                       stpo(14)          +  1
445900040615     o                       stpo(15)          +  1
446000040615     o                       stpo(16)          +  1
446100040615     o                       stpo(17)          +  1
446200040615     o                       stpo(18)          +  1
446300040615     o                       stpo(19)          +  1
446400040615     o                       stpo(20)          +  1
446500040615     o                       stpo(21)          +  1
446600040615     o                       stpo(22)          +  1
446700040615     o                       stpo(23)          +  1
446800040615     o                       stpo(24)          +  1
446900040615     o                       stpo(25)          +  1
447000040615     o                       stpo(26)          +  1
447100040615     o                       stpo(27)          +  1
447200040615     o                       stpo(28)          +  1
447300040615     o                       stpo(29)          +  1
447400040615     o                       stpo(30)          +  1
447500040615     O          E            TES3        2
447600911021     O                       STA(4)              66
447700911021     O                       STA(7)             132
447800911021     O                       STA(10)            198
447900040616     O**               N04 17                   198 '     '
448000040615     O          E            TES3        1
448100911018     O                       STA(5)              66
448200911018     O                       STA(8)             132
448300911021     O                       STA(11)            198
448400040616     O**               N04 17                   198 '     '
448500960422     O*
448600960423     O          E            TESIDR      3  1
448700960422     O                       STA(25)             66
448800960422     O                       STA(26)            132
448900960513     O                       STA(13)            198
449000990513     O                       STADCI              82 '  /  /    '
449100990513     O                       STADCM             166 '  /  /    '
449200960423     O*
449300960423     O          E            TESIDE      3  1
449400960423     O                       STA(30)             66
449500960423     O                       STA(31)            132
449600960513     O                       STA(13)            198
449700990513     O                       STADCI              84 '  /  /    '
449800990513     O                       STADCM             166 '  /  /    '
449900911015     O*
450000911015     O          E            DET         1
450100990512     O                  02   PRTAAS        Z
450200921015     O                  02   ANMLNP        Z   +  1
450300921015     O                  02   ANMNRS        Z   +  1
450400921015     O                  02   ANMNSP        Z   +  1
450500921015     O                  02   ANMLNA            +  1
450600960417     O                  02   PRTDSP            +  1 '  /  '
450700960417     O*
450800990121     O                  03   ANMFLS            +  3
450900921015     O                  03   ANMLNA            +  1
451000921015     O                  03   ANMNRS        Z   +  1
451100921015     O                  03   ANMSCN        Z   +  1
451200990121     O                  22   ANMSCN        Z     48
451300040609     O                  03   PRTZNC            +  1
451400040609     O                  03   anmft4            +  0
451500990121     O                  03   ANMNPS        Z   +  1
451600040616     O                       DESTAN            +  1
451700990121     O                       ANMCAA            +  1
451800921015     O                                         +  0 '-'
451900990122     O              N40   N09DESCAA            +  0
452000990122     O                     09DESCA1              95
452100950320     O                     09ARBNDC        Z   +  0
452200950320     O                     09STADDC            +  1 '  /  '
452300950320     O                     09STAPDC            +  1
452400040616     O                     20PSTAM              110
452500040616     O                     20WDFLP              107
452600040616     O                     20PRTDET             118 '  /  '
452700040616     O                     21CDOVE              110
452800040616     O                     21PRTDOV             101
452900040616     O                     21WFGS               107
453000040616     O                     21PRTDFV             118 '  /  '
453100990121     O* NUMERO C.A. SU CUI E' IL COLLO
453200040616     O                     40CCOAN              109
453300040616     O                     40DESCA1              91
453400040616     O                     40DCTFIL        Z     98
453500040616     O                     40DCTNCA        Z    106
453600040616     O                     40STADCA             118 '  /  /  '
453700040616     O                  42 40                   124 'CHIUSA'
453800040616     O                       STALID             125
453900040616     O                       STADAc            +  1 '  /  '
454000040616     O                       STAorc            +  1 '  :  '
454100040616     O                       DESFSA            +  1
454200950320     O                       STADAO            +  1 '  /  '
454300040616     O                       ANMCDU        Z   +  1
454400040616     O                       ANMNFV        Z   +  1
454500990121     O                       DESFSC            +  1
454600950320     O                       STADCH            +  1 '  /  '
454700970522     O                       CCHPRT            +  1
454800960423     O                                         +  0 '-'
454900960423     O                       DESCCH            +  0
455000040616     O                       ANMFLE            +  1
455100050503     O*  STAMPA NUMERO C.A. e/o particolarità consegna CP
455200050503     O          E            DET1        1
455300050503     O               43      decodiCP            96
455400050503     O               41      CCOAN              109
455500050503     O               41      DCTFIL        Z     98
455600050503     O               41      DCTNCA        Z    106
455700050503     O               41      STADCA             118 '  /  /  '
455800050503     O               41                         128 'CHIUSA IL'
455900050503     O               41      STACHA             134 '  /  '
456000050504     O*  STAMPA NUMERO C.A. e/o particolarità consegna CP
456100050504     O          E            DET1        0  0
456200050504     O               43      decodiCP            96
456300051221     O*  STAMPA dati mittente e destinatario
456400051221     O          E            DET2        1
456500051222     o*                                        + 30 'Mittente:'
456600051222     o                                         + 69 'Mittente:'
456700051221     o                       blprsm            +  0
456800051222     o                                         +  2 'Rif.Mitt.Num:'
456900051222     o                       prtrmn            +  0
457000051222     o                                         +  2 'Destinatario:'
457100051221     o                       blprsd            +  0
457200151125     O*  STAMPA consegna anomala 7
457300151125     O          E            DET3        1
457400151125     o                                         + 30 'Bolla chiusa per '
457500151125     o                                         +  0 'merce mai affidata il'
457600151125     o                       prtdcm            +  2 '  /  '
457700960422     O*
457800960422     O*  STAMPA RIGA: DETTAGLIO FILIALE CHE NON HANNO FATTO L'IDD
457900960422     O          E            DETIDD      1
458000960423     O                       FID(1)        Z
458100960422     O                                         +  0 '-'
458200960422     O                       DFI(1)            +  0
458300050831     O                       FID(2)        Z   +  2
458400960422     O                                         +  0 '-'
458500960422     O                       DFI(2)            +  0
458600050831     O                       FID(3)        Z   +  2
458700960422     O                                         +  0 '-'
458800960422     O                       DFI(3)            +  0
458900050831     O                       FID(4)        Z   +  2
459000960422     O                                         +  0 '-'
459100960422     O                       DFI(4)            +  0
459200050831     O                       FID(5)        Z   +  2
459300960422     O                                         +  0 '-'
459400960422     O                       DFI(5)            +  0
459500050831     O                       FID(6)        Z   +  2
459600960422     O                                         +  0 '-'
459700960422     O                       DFI(6)            +  0
459800050831     O                       FID(7)        Z   +  2
459900960422     O                                         +  0 '-'
460000960422     O                       DFI(7)            +  0
460100960422     O*
460200960422     O*  STAMPA RIGA: NESSUNA ANOMALIA RISCONTRATA
460300960422     O          E            NOANOM      3  3
460400960422     O                       STA(24)            132
460500960423     O*  STAMPA RIGA: TUTTE LE FILIALI HANNO FATTO L'IDD
460600960423     O          E            SIIDD       2
460700960423     O                       STA(27)             66
460800960423     O                       STA(28)            132
460900960423     O                       STA(29)            198
461000960422     O*  STAMPA IDD3
461100911025     OPRTF198X  E            TESX             02
461200960604     O                       STA(1)              66
461300040618     O                       STA(14)            132
461400040618     O                       STA(3)             198
461500040618     O                       prtpo               46
461600960926     O                       D34FGS              50
461700960926     O                       PRTFGS              71
461800960604     O                       §7ALCI             121
461900911025     O                       RSUT                20
462000960402     O                       WDTGIO             168 '  .  .    '
462100921015     O                       PAGE2         Z    198
462200960403     O          E            TESX        1
462300960402     O                       UTIME              167 '  :  :  '
462400960402     O                  01                      190 '-C O N F E R M A-'
462500921015     O          E            TESX        2
462600911025     O                       STA(4)              66
462700911025     O                       STA(7)             132
462800911025     O                       STA(10)            198
462900921016     O                                          198 '     '
463000911025     O          E            TESX        1
463100911025     O                       STA(5)              66
463200911025     O                       STA(8)             132
463300911025     O                       STA(11)            198
463400921016     O                                          198 '     '
463500911025     O*
463600911025     O          E            DETX        1
463700990512     O                  02   PRTAAS        Z
463800921015     O                  02   ANMLNP        Z   +  1
463900921015     O                  02   ANMNRS        Z   +  1
464000921015     O                  02   ANMNSP        Z   +  1
464100921015     O                  02   ANMLNA            +  1
464200960417     O                  02   PRTDSP            +  1 '  /  '
464300960417     O*
464400990121     O                  03   ANMFLS            +  3
464500921015     O                  03   ANMLNA            +  1
464600921015     O                  03   ANMNRS        Z   +  1
464700921015     O                  03   ANMSCN        Z   +  1
464800130423     O                  03   PRTZNC            +  1
464900130423     O                  03   anmft4            +  0
465000990121     O                  03   ANMNPS        Z   +  1
465100990121     O                       DESTAN            +  1
465200990121     O                       ANMCAA            +  1
465300921015     O                                         +  0 '-'
465400990122     O                    N40DESCAA            +  0
465500040616     O                     20PSTAM              110
465600040616     O                     20WDFLP              107
465700040616     O                     20PRTDET             118 '  /  '
465800040616     O                     21CDOVE              110
465900040616     O                     21PRTDOV             101
466000040616     O                     21WFGS               107
466100040616     O                     21PRTDFV             118 '  /  '
466200990122     O* NUMERO C.A. SU CUI E' IL COLLO
466300040616     O                     40CCOAN              109
466400040616     O                     40DESCA1              91
466500040616     O                     40DCTFIL        Z     98
466600040616     O                     40DCTNCA        Z    106
466700040616     O                     40STADCA             118 '  /  /  '
466800040616     O                  42 40                   124 'CHIUSA'
466900040616     O                       ANMLID             125
467000040616     O                       STADAc            +  1 '  /  '
467100040616     O                       STAorc            +  1 '  :  '
467200040616     O                       DESFSA            +  1
467300040616     O                       STADAO            +  1 '  /  '
467400040616     O                       ANMCDU        Z   +  1
467500040616     O                       ANMNFV        Z   +  1
467600990121     O                       DESFSC            +  1
467700040616     O                       STADCH            +  1 '  /  '
467800960423     O                       ANMCCH            +  1
467900960423     O                                         +  0 '-'
468000960423     O                       DESCCH            +  0
468100040616     O                       ANMFLE            +  1
468200990122     O*  STAMPA NUMERO C.A.
468300050503     O          E            DETX1       1
468400050503     O               43      decodiCP            96
468500050503     O               41      CCOAN              109
468600050503     O               41      DCTFIL        Z     98
468700050503     O               41      DCTNCA        Z    106
468800050503     O               41      STADCA             118 '  /  /  '
468900050503     O               41                         128 'CHIUSA IL'
469000050503     O               41      STACHA             134 '  /  '
469100050504     O          E            DETX1       0  0
469200050504     O               43      decodiCP            96
469300990122     O*
469400990512**
469500040615XXXXXXXXXXXXXXXXXXXX        xxxxxxxxxxxxxxxxxx XXX-XXXXXXXXXXXXXXX
469600960926XXXXX           **   L I S T A     A N O M A L I E   **             2
469700960417    FNLR35R               XX/XX/XXXX                     PAG. XXXX  3
469800040616    S P E D I Z I O N E          S E G N A C O L L O      A      N
469900040616AN LNP SER NUMERO LNA  DATA   LNP LNA SER NUMERO ZON PIS T I P O
470000040616XX XXX XX XXXXXXX XXX XX/XX   XXX XXX XX XXXXXXX xxV XX xxxxxxxxxx
470100040616      O      M      A      L      I      A               LIV CREAZ
470200040616    C  O  D  I  C  E                                     IDD DATA
470300040616xx XXX-bolla in consegna:XXXXXXX XX/XX XXXXXXXXXXXX  X    X XX/xx
470400040616IONE  A  P  E  R  T  U  R  A    C  H  I  U  S  U  R  A         P.O  10
470500040616 ORA  FASE     DATA   FOGLIO    FASE      DATA  CAUSALE        EMI  11
470600040616xx:xx xxxxxxxx XX/XX XXX XXXXXX xxxxxxxx XX/XX XX-XXXXXXXXXXXX XXX 12
470700990513RNO PRECEDENTE, IN DATA XX/XX/XXXX) :
470800960604        **   E L A B O R A Z I O N E      " I. D. D.  X "   **     14
470900040614                                  ANOMALIE IDD DAL LIVELLO IDD : X 15
471000990513ANOMALIE DEL FOGLIO  XXXXXXXX   N.RO  XXXXXX   DEL XX/XX/XXXX      16
471100040614FASE APERTURA ANOMALIE : XXXXXXXXXXXX                              17
471200960403ANOMALIE :  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX         18
471300960604        **   E L A B O R A Z I O N E      "I. D. D."   **          19
471400960402DATA APERTURA ANOMALIA : DAL  XX/XX/XXXX   AL  XX/XX/XXXX          20
471500960402DATA CHIUSURA ANOMALIA : DAL  XX/XX/XXXX   AL  XX/XX/XXXX          21
471600960506CODICI       RICHIESTI : XXX                                       22
471700960506CODICI  NON  RICHIESTI : XXX                                       23
471800960422 *** NESSUNA  ANOMALIA  RISCONTRATA  NEI  PARAMETRI  RICHIESTI *** 24
471900960926ELENCO PUNTI OPER.DI CUI MANCA LA TRASMISSIONE ANOMALIE I.D.D. IN  25
472000990513DATA  XX/XX/XXXX     (PER I PUNTI OPER. CHE ESEGUONO L'IDD DEL GIO 26
472100960926                                                    T U T T I    I 27
472200030717    P U N T I    O P E R A T I V I    H A N N O    C O N F E R M A 28
472300030717 T O    L'  I . D . D .                                            29
472400030717ELENCO PUNTI OPER.DELL'AREA DI CUI MANCA LA CONFERMA DELL'I.D.D. I 30
472500030717N DATA  XX/XX/XXXX   (PER I PUNTI OPER. CHE ESEGUONO L'IDD DEL GIO 31
472600960426CAUSALE CHIUSURA ANOMALIA: XX-XXXXXXXXXXXXXXXXXXXXXXXXX            32
472700030717P.O. SEGNACOLLO : XXX      NUMERO SERIE : XX                       33
472800040615                **  COLLI   DI   VALORE   MANCANTI   **            34
472900040615Punti Operativi: xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx   35
473000921016**
473100960506A R R I V O
473200960506P A R T E N Z A
473300960506  N O N    I.D.D.
473400960506  I . D . D.
473500960506  I N T E R N E
473600960506  T U T T E
473700960506     SOLO APERTE
473800960506     SOLO CHIUSE
473900021008**         CMD1
474000060619OVRDBF FILE(FNANM00T)                MBR(M000000)
474100980327**         CMD3
474200060619ADDPFM FILE(FNANM00T)                MBR(M000000)
474300980327**         CMD5
474400060619CHKOBJ OBJ(FNANM00T) OBJTYPE(*FILE)  MBR(M000000)
474500060619**         OVRSED
474600060710OVRDBF FILE(FNANMSED) TOFILE(GAITRAGRU/FNANM00F )

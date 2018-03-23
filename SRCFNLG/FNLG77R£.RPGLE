000100930730     H DECEDIT('0,') DATEDIT(*DMY.)
000200941013     H* FNLG77R *----------------------------------------------------*
000300020808     H*                                                              *
000400930730     H*  - STATISTICA GIACENZE IN PARTENZA - CONTROLLO UFF. GIAC. -  *
000500930730     H*                                                              *
000600930730     H*--------------------------------------------------------------*
000700940224     H*  15 = LOKUP SU SHIERA DELLE FILIALI                          *
000800010921     H*  18 = LETTURA INIZIALE FLFG765L                              *
000900940224     H*  20 = CHAIN SU TFTGC01L                                      *
001000940224     H*  30 = CHAIN SU TABELLA 2G                                    *
001100940224     H*  34 = CHAIN SU TABELLA 2A                                    *
001200940224     H*  40 = CHAIN SU FLEVB01L                                      *
001300940224     H*  50 = READ  SU FLGCP000                                      *
001400940224     H*  60 = READ  SU FLGCA000                                      *
001500010921     H*  80 = SETLL SU FLFG765L                                      *
001600940224     H*--------------------------------------------------------------*
001700050225     FTIGCP07l  IF   E           k DISK
001800050225     FTIGCP24l  IF   E           k DISK
001900050225     F                                     RENAME(TIGCP000:TIGCP024)
002000990803     FFNEVB01L  IF   E           K DISK
002100941013     FFNBLP01L  IF   E           K DISK
002200941013     FFNARB01L  IF   E           K DISK
002300990730     FTITGC01L  IF   E           K DISK
002400930730     FTABEL00F  IF   E           K DISK
002500010921     FFLFG765F  O    E             DISK
002600010921     FFLFG765L  UF   E           K DISK
002700010921     F                                     RENAME(FLFG76:FLFG765)
002800930803     F* SCHIERE GIACENZE IN PARTENZA *-----------------------------------------*
002900020305     E* NR. GIA. DA APRIRE
003000010921     D NGDA            S              5  0 DIM(500)
003100020305     E* NR. GIA. NON APERTE
003200010921     D NGNA            S              5  0 DIM(500)
003300020305     E* NR. GIA. AP. IN RIT                                                 .
003400010921     D NGAR            S              5  0 DIM(500)
003500020305     E* NR. GIORNI RIT. AP.
003600010921     D NGRA            S              6  0 DIM(500)
003700020305     E* NR. DISP DA IMMETTE                                                 RE
003800010921     D NDDI            S              5  0 DIM(500)
003900020305     E* NR. DISP DI RESO OL                                                 TRE
004000010921     D NDRO            S              5  0 DIM(500)
004100020305     E* NR. DISP NON IMMESS                                                 E
004200010921     D NDNI            S              5  0 DIM(500)
004300020305     E* NR. GIORNI RIT. IMM                                                 IS.
004400010921     D NGRD            S              6  0 DIM(500)
004500020305     E* NR. GIAC. DA CESSAR                                                 E
004600010921     D NGDC            S              5  0 DIM(500)
004700020305     E* NR. GIAC. NON CESSA                                                 TE
004800010921     D NGNC            S              5  0 DIM(500)
004900020305     E* NR. GIAC. CESSATE O                                                 LTRE
005000010921     D NGCO            S              5  0 DIM(500)
005100020305     E* NR. GIORNI RIT. CES                                                 SAZ.
005200010921     D NGRC            S              6  0 DIM(500)
005300020305     E* NR. GIAC. SENZA SPE                                                 SE
005400010921     D NGSS            S              5  0 DIM(500)
005500930803     F* SCHIERE GIACENZE IN ARRIVO *-------------------------------------------*
005600020305     E* NR. GIA. APERTE
005700020305     D AGAP            S              5  0 DIM(500)
005800020305     E* NR. GIA. AP. IN RIT
005900020305     D AGAR            S              5  0 DIM(500)
006000020305     E* NR. GIORNI RIT. AP.
006100020305     D AGRA            S              6  0 DIM(500)
006200020305     E* NR. DISP DA ESEGUIRE
006300020305     D ADDE            S              5  0 DIM(500)
006400020305     E* NR. DISP ESEGUITE Rit.
006500020305     D ADER            S              5  0 DIM(500)
006600020305     E* NR. GIORNI RIT. ESECUZ.
006700010921     D AGRE            S              6  0 DIM(500)
006800930804     F* SCHIERE VARIE *--------------------------------------------------------*
006900020305     E* FILIALI ESAMINATE
007000020305     D SKI             S              3  0 DIM(19)
007100020305     E* COD. NAZIONE
007200020305     D*CNZ             S              3    DIM(500)
007300020305     E* FLAG ITALIA/ESTERO
007400020305     D*ITA             S              1    DIM(500)
007500020305     E* COD. TAB. 7Q
007600020305     D C7Q             S              2    DIM(100)
007700020305     E* FLAG TAB. 7Q
007800950113     D D7Q             S             89    DIM(100)
007900950116     D TCUDS           DS
008000950116     D  F3                     3      3
008100950116     D  F4                     4      4
008200950116     D  F56                    5      6
008300950116     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
008400950116     D  TCU                  398    697
008500950116     D                                     DIM(50)
008600950116     D  KCU                  698    847P 0
008700950116     D                                     DIM(50)
008800950116     D                                     PACKEVEN
008900150514     D fnlv59ds      E DS
009000950116     D TRUL52        E DS                  EXTNAME(TRUL52DS)
009100930203     D DS2G          E DS
009200050210     D DS20          E DS
009300950113     D DS7Q          E DS
009400000202     D OG143         E DS
009500950113     D DS15          E DS
009600930804     D DS2A          E DS
009700930203     D PARAM           DS
009800941013     D  DAL                    1      8  0
009900941013     D  AL                     9     16  0
010000941013     D  ANNO                  17     20  0
010100941013     D  PER                   21     22  0
010200050301     D  FIL                   23     25  0
010300900418     D KPJBA         E DS
010400020305     D TRUL27        E DS                  EXTNAME(TRUL27DS)
010500020808     d trul06ds      e ds
010600020808     d  lin                    1     90  0 dim(30)
010700020809     d savkpjbu        s                   like(kpjbu)
010800930803     D*-------------
010900941013     D* DS PER ROUTINE 'XSRDA8'
011000930803     D*-------------
011100941013     D WLBDA8          DS
011200941013     D  G02DAT                 1      8  0
011300941013     D  G02INV                 9     16  0
011400941013     D  G02ERR                17     17
011500941013     D  G02TGI                18     22  0
011600930803     D*-------------
011700990906     D* DS PER ROUTINE 'XGGFEST'
011800930803     D*-------------
011900941013     D WDAT8           DS
012000941013     D  DATADA                 1      8  0
012100941013     D  DATAA                  9     16  0
012200941013     D  GGF                   17     21  0
012300930803     D*-------------
012400941013     D* DS PER ROUTINE 'XSRGI8'
012500930803     D*-------------
012600930803     D WGIDAT          DS
012700941013     D  GIODAT                 1      8  0
012800941013     D  GIOINV                 9     16  0
012900941013     D  GIOTGI                17     21  0
013000900423     C*---------------------------------------------------------------*
013100020808     c                   do        30            ff                3 0
013200020808     c                   setoff                                       50
013300020808     c                   if        lin(ff) = 0
013400020808     c                   leave
013500020808     c                   endif
013600050225     C     lin(ff)       SETLL     tigcp07l
013700020808     C                   DO        *HIVAL
013800050225     C     lin(ff)       READE     tigcp07l                               50
013900020808     C   50              leave
014000930803     C** MEMORIZZO FILIALE IN ESAME
014100930730     C                   Z-ADD     GCPLNP        I                 3 0
014200930804     C** MEMORIZZO FILIALE IN ESAME IN SCHIERA
014300930804     C     I             LOOKUP    SKI                                    15
014400930804     C  N15              ADD       1             H                 2 0
014500930804     C  N15              Z-ADD     I             SKI(H)
014600930803     C** MEMORIZZO DATA APERTURA O RIAPERTURA SE PRESENTE
014700050225     C                   Z-ADD     GCPDUR        DATCON            8 0
014800950113     C*
014900950116     C                   Z-ADD     DATCON        DTAPER
015000950113     C                   EXSR      PARGCP
015100930730     C*-----------
015200930730     C* APERTURA
015300930730     C*-----------
015400950116     C     DTAPER        IFGE      DAL
015500950116     C     DTAPER        ANDLE     AL
015600020305     c* da aprire
015700931126     C                   ADD       1             NGDA(I)
015800931126     C*  SE  LA DATA LETTERA AL MITTENTE E' = 0  CALCOLO NON FATTE
015900931126     C*  - FORZO DATA LETT.= UDATE X CALC.RITARDO
016000931126     C     GCPDMM        IFEQ      0
016100931126     C*  SE NON RISULTA GIA' CHIUSA  ( PUO' SUCCEDERE
016200931126     C*  CHE L'ARRIVO LA CHIUDE PRIMA CHE SIA STAMPATO MODULO)
016300931126     C     GCPDCG        IFEQ      *ZEROS
016400020305     c* non aperte
016500931126     C                   ADD       1             NGNA(I)
016600931126     C                   MOVE      DATELA        GCPDMM
016700931126     C                   ELSE
016800950116     C                   MOVE      DTAPER        GCPDMM
016900931126     C                   ENDIF
017000931126     C                   ENDIF
017100931126     C** --- CALCOLO  RITARDO ---
017200930802     C     GCPDMM        IFGT      0
017300930803     C** TRASFORMO IN GIORNI ASSOLUTI LE DUE DATE DA CONFRONTARE E DETERMINO
017400930803     C** SE CI SONO RITARDI
017500950116     C                   Z-ADD     DTAPER        DATA1             8 0
017600941013     C                   Z-ADD     GCPDMM        DATA2             8 0
017700930802     C                   EXSR      CONV
017800930803     C** CALCOLO LA DIFFERENZA TRA LE DUE DATE
017900010921     C     GIO2          SUB       GIO1          RITAP             5 0
018000930804     C     RITAP         IFGT      §2GGRP
018100930803     C** CALCOLO GIORNI FESTIVI INTERCORRENTI TRA LE DUE DATE E LI SOTTRAGGO
018200930803     C** DAI GIORNI DI RITARDO PER VEDERE SE SONO ANCORA IN RITARDO
018300930802     C                   EXSR      FEST
018400930802     C     RITAP         SUB       GGF           RITAP
018500930804     C     RITAP         IFGT      §2GGRP
018600931125     C     RITAP         SUB       §2GGRP        RITAP
018700930802     C                   ADD       1             NGAR(I)
018800930804     C                   ADD       RITAP         NGRA(I)
018900930802     C                   ENDIF
019000930802     C                   ENDIF
019100930802     C                   ENDIF
019200930802     C*
019300930802     C                   ENDIF
019400930802     C*-----------
019500930802     C* IMMISSIONE DISPOSIZIONI DI RESO
019600930802     C*-----------
019700941013     C** Reperisco gg. rientro da tariffa
019800941013     C                   EXSR      REPGGR
019900930803     C** CALCOLO SE LA DATA E' INFERIORE ALLA DATA LIMITE SUPERIORE QUINDI LA
020000930803     C** MERCE SAREBBE DA RIENTRARE
020100950116     C                   Z-ADD     DTAPER        G02INV
020200930802     C                   MOVE      '3'           G02ERR
020300941013     C                   CALL      'XSRDA8'
020400941013     C                   PARM                    WLBDA8
020500930802     C     G02TGI        ADD       GIOR          GIOTGI
020600941013     C                   CALL      'XSRGI8'
020700941013     C                   PARM                    WGIDAT
020800931124     C** SE DATA APERT.+GG RIENTRO E' COMPRESA NELL'INTERVALLO
020900931124     C**(CIOè AVREI DOVUTO DARE DISPOSIZ. X SCADUTI TERMINI)
021000930802     C     GIOINV        IFGE      DAL
021100930802     C     GIOINV        ANDLE     AL
021200931124     C*
021300931126     C** NON  SONO STATE DATE DISPOSIZIONI NELL'PERIODO ELAB.
021400941013     C     GCPDIS        IFEQ      *BLANKS
021500020305     c* non immesse
021600931126     C                   ADD       1             NDNI(I)
021700020305     c* da immettere
021800931126     C                   ADD       1             NDDI(I)
021900931126     C*  - FORZO DATA IMMISS .= UDATE X CALC.RITARDO
022000931126     C                   MOVE      DATELA        GCPDDM
022100931126     C                   ELSE
022200931126     C** SONO STATE DATE DISPOSIZIONI DI RESO
022300941013     C     GCPDIS        IFEQ      'R'
022400931126     C                   ADD       1             NDDI(I)
022500931126     C                   ENDIF
022600931126     C                   ENDIF
022700931126     C** --- CALCOLO  RITARDO ---
022800931124     C** E SONO STATE DATE DISPOSIZIONI DI RESO
022900931126     C** O NON SONO STATE IMMESSE
023000941013     C     GCPDIS        IFEQ      'R'
023100941013     C     GCPDIS        OREQ      *BLANKS
023200931123     C** CALCOLO GIORNI DI RITARDO SU IMMISSIONE DISPOSIZIONE
023300020305     C* D.APER.+GG RESO
023400931123     C                   Z-ADD     GIOINV        DATA1
023500020305     C* DATA IMMISS.DIS
023600931123     C                   Z-ADD     GCPDDM        DATA2
023700930802     C                   EXSR      CONV
023800010921     C     GIO2          SUB       GIO1          RITDI             5 0
023900930802     C     RITDI         IFGT      §2GGRD
024000930802     C** CALCOLO GIORNI FESTIVI TRA LE DUE DATE
024100930802     C                   EXSR      FEST
024200930802     C     RITDI         SUB       GGF           RITDI
024300930802     C     RITDI         IFGT      §2GGRD
024400931125     C     RITDI         SUB       §2GGRD        RITDI
024500020305     C*NR.IMMESSE RITAR
024600931123     C                   ADD       1             NDRO(I)
024700020305     C*GG RITARDO
024800931123     C                   ADD       RITDI         NGRD(I)
024900930802     C                   ENDIF
025000930802     C                   ENDIF
025100931124     C                   ENDIF
025200930802     C*
025300930804     C                   ENDIF
025400930802     C*-----------
025500930802     C* CESSATE GIACENZE
025600930802     C*-----------
025700930802     C     GCPDCG        IFGE      DAL
025800930802     C     GCPDCG        ANDLE     AL
025900050808     C     GCPfrg        ANDeq     0
026000020305     C* DA CHIUDERE
026100050420     c                   if        gcpfas >=40
026200931126     C                   ADD       1             NGDC(I)
026300050412     c                   end
026400930803     C** PER QUELLE CESSATE CONTO QUELLE SENZA SPESE
026500950210     C     GCPNBW        IFEQ      *ZEROS
026600950301     C     GCPDBG        ANDGT     0
026700050420     c     gcpfas        andge     50
026800930803     C                   ADD       1             NGSS(I)
026900930803     C                   ENDIF
027000950301     C** NON  SONO STATE CHIUSE
027100950301     C     GCPDBG        IFEQ      0
027200050406     c     gcpfas        andeq     40
027300020305     C* NON CHIUSE
027400950301     C                   ADD       1             NGNC(I)
027500950301     C*  - FORZO DATA BOLLA ADD. = UDATE X CALC.RITARDO
027600950301     C                   MOVE      DATELA        GCPDBG
027700950301     C                   ENDIF
027800930803     C*
027900950301     C** --- CALCOLO  RITARDO ---
028000950301     C**  TRA DATA BOLLA ADDEBITO E DATA CHIUSURA GIACENZA
028100950301     C     GCPDBG        IFGT      0
028200930803     C                   Z-ADD     GCPDCG        DATA1
028300930803     C                   Z-ADD     GCPDBG        DATA2
028400930803     C                   EXSR      CONV
028500010921     C     GIO2          SUB       GIO1          RITCE             5 0
028600930803     C     RITCE         IFGT      §2GGRC
028700930803     C** CALCOLO GIORNI FESTIVI TRA LE DUE DATE
028800930803     C                   EXSR      FEST
028900930803     C     RITCE         SUB       GGF           RITCE
029000930803     C     RITCE         IFGT      §2GGRC
029100931125     C     RITCE         SUB       §2GGRC        RITCE
029200930803     C                   ADD       1             NGCO(I)
029300930803     C                   ADD       RITCE         NGRC(I)
029400930803     C                   ENDIF
029500930803     C                   ENDIF
029600930803     C                   ENDIF
029700930803     C                   ENDIF
029800930804     C*
029900930803     C                   ENDDO
030000020808     C                   enddo
030100930803     C*************** FINE CICLO PARTENZA ******************
030200020808     c
030300020808     c                   do        30            ff                3 0
030400020808     c                   setoff                                       60
030500020808     c                   if        lin(ff) = 0
030600020808     c                   leave
030700020808     c                   endif
030800050225     C     lin(ff)       SETLL     tigcp24L
030900020808     C                   DO        *HIVAL
031000050225     C     lin(ff)       READE     tigcp24L                               60
031100020808     C   60              leave
031200020808     C     GCPATB        IFEQ      *BLANKS
031300930803     C** MEMORIZZO DATA APERTURA O RIAPERTURA SE PRESENTE
031400941013     C                   Z-ADD     GCPDUR        DATCON
031500930804     C** MEMORIZZO FILIALE IN ESAME
031600930804     C                   Z-ADD     GCPFGC        I                 3 0
031700930804     C** MEMORIZZO FILIALE IN ESAME IN SCHIERA
031800930804     C     I             LOOKUP    SKI                                    15
031900930804     C  N15              ADD       1             H
032000930804     C  N15              Z-ADD     I             SKI(H)
032100930803     C*-----------
032200930803     C* APERTURA
032300930803     C*-----------
032400930803     C     DATCON        IFGE      DAL
032500930803     C     DATCON        ANDLE     AL
032600941013     C** Eseguo posiz. su bolla per vedere se c'era il fermo deposito
032700050225     C                   Z-ADD     GCPAASa       KAAS
032800050225     C                   Z-ADD     GCPLNPa       KLNP
032900050225     C                   Z-ADD     GCPNSPa       KNSP
033000050225     C                   Z-ADD     GCPNRSa       KNRS
033100941013     C                   MOVEL     *BLANKS       ARBFFD
033200950116     C                   MOVEL     *BLANKS       ARBGGA
033300941013     C     KSPE          CHAIN     FNARB01L                           31
033400941013     C     ARBFFD        IFEQ      'S'
033500051107     C     ARBFFD        oreq      'M'
033600941013     C                   MOVEL     *BLANKS       KEY
033700941013     C                   MOVEL     'F  '         KEY
033800941013     C                   ELSE
033900930804     C** CONFRONTO DATA APERTURA CON DATA COMUNICAZIONE TENENDO CONTO
034000930804     C** DEGLI EVENTUALI GIORNI LASCIATO AVVISO
034100990803     C                   Z-ADD     GCPAAS        KAAS
034200941013     C                   Z-ADD     GCPDXD        KDEV
034300930804     C                   ADD       1             AGAP(I)
034400930804     C                   Z-ADD     0             GGG
034500990803     C     KEVB          SETLL     FNEVB01L                           40
034600930804     C     *IN40         DOWEQ     *OFF
034700990803     C     KEVBS         READE     FNEVB000                               40
034800931124     C* SOMMO TUTTI I GIORNI L.AVV FINO ALL'APERT.GIAC.
034900990803     C  N40EVBDEV        IFLT      DATCON
035000930804     C                   MOVEL     *BLANKS       KEY
035100930804     C                   MOVEL     EVBCEV        KEY
035200941013     C                   END
035300941013     C                   ENDDO
035400941013     C                   ENDIF
035500941013     C*
035600941013     C                   MOVEL     '2A'          COD
035700930804     C     KTAB          CHAIN     TABEL                              39
035800930804     C  N39              MOVEL     TBLUNI        DS2A
035900930804     C  N39§2AGLA        IFGT      0
036000931124     C                   ADD       §2AGLA        GGG               3 0
036100930804     C                   ENDIF
036200930804     C*
036300930804     C** TROVO DIFFERENZA TRA LE DUE DATE
036400950116     C                   Z-ADD     GCPDXD        DATDXD
036500950116     C                   EXSR      PARGCA
036600950116     C                   Z-ADD     DATDXD        DATA1
036700930804     C                   Z-ADD     DATCON        DATA2
036800930804     C                   EXSR      CONV
036900010921     C     GIO2          SUB       GIO1          RITAA             5 0
037000930804     C     RITAA         SUB       GGG           RITAA
037100930804     C** SE HO RITARDO CONTO GIORNI FESTIVI TRA LE DUE DATE
037200930804     C     RITAA         IFGT      §2GRAA
037300930804     C                   EXSR      FEST
037400930804     C     RITAA         SUB       GGF           RITAA
037500930804     C     RITAA         IFGT      §2GRAA
037600931125     C     RITAA         SUB       §2GRAA        RITAA
037700930804     C                   ADD       1             AGAR(I)
037800930804     C                   ADD       RITAA         AGRA(I)
037900930804     C                   ENDIF
038000930804     C                   ENDIF
038100930804     C*
038200930804     C                   ENDIF
038300930804     C*-----------
038400930804     C* ESECUZIONE DISPOSIZIONI
038500930804     C*-----------
038600050225     C     GCPDeda       IFGE      DAL
038700050225     C     GCPDeda       ANDLE     AL
038800930804     C                   ADD       1             ADDE(I)
038900930804     C** CALCOLO LA DIFFERENZA TRA LA DATA ESECUZIONE E LA DATA DISPOSIZIONE
039000020305     c* data eseguib.
039100941013     C     GCPDED        IFGT      0
039200941013     C                   Z-ADD     GCPDED        DATA1
039300941013     C                   ELSE
039400050225     c* data immissione
039500931124     C     GCPDLD        IFGT      0
039600931124     C                   Z-ADD     GCPDLD        DATA1
039700931124     C                   ELSE
039800020305     c* data immiss.
039900931124     C                   Z-ADD     GCPDDM        DATA1
040000931124     C                   END
040100941013     C                   END
040200941013     C*
040300050225     C                   Z-ADD     GCPDeda       DATA2
040400930804     C                   EXSR      CONV
040500010921     C     GIO2          SUB       GIO1          RITES             5 0
040600930804     C** SE HO RITARDO CONTO GIORNI FESTIVI TRA LE DUE DATE
040700930804     C     RITES         IFGT      §2GREA
040800930804     C                   EXSR      FEST
040900930804     C     RITES         SUB       GGF           RITES
041000930804     C     RITES         IFGT      §2GREA
041100931125     C     RITES         SUB       §2GREA        RITES
041200930804     C                   ADD       1             ADER(I)
041300930804     C                   ADD       RITES         AGRE(I)
041400930804     C                   ENDIF
041500930804     C                   ENDIF
041600930804     C*
041700930804     C                   ENDIF
041800930804     C*
041900930804     C                   ENDIF
042000930803     C                   ENDDO
042100020808     c                   enddo
042200930804     C**************** FINE CICLO ARRIVO *******************
042300930804     C                   DO        19            Z                 3 0
042400930804     C     SKI(Z)        IFGT      0
042500930804     C                   Z-ADD     SKI(Z)        X                 3 0
042600930803     C*-----------------------------------------------------------*
042700930804     C                   Z-ADD     NGDA(X)       FG7GDA
042800930804     C                   Z-ADD     NGNA(X)       FG7GNA
042900930804     C                   Z-ADD     NGAR(X)       FG7GAR
043000930804     C                   Z-ADD     NGRA(X)       FG7GRA
043100930804     C                   Z-ADD     NDDI(X)       FG7DDI
043200930804     C                   Z-ADD     NDRO(X)       FG7DRO
043300930804     C                   Z-ADD     NDNI(X)       FG7DNI
043400930804     C                   Z-ADD     NGRD(X)       FG7GRD
043500930804     C                   Z-ADD     NGDC(X)       FG7GDC
043600930804     C                   Z-ADD     NGNC(X)       FG7GNC
043700930804     C                   Z-ADD     NGCO(X)       FG7GCO
043800930804     C                   Z-ADD     NGRC(X)       FG7GRC
043900930804     C                   Z-ADD     NGSS(X)       FG7GSS
044000930804     C                   Z-ADD     AGAP(X)       FG7GAP
044100930811     C                   Z-ADD     AGAR(X)       FG7AAR
044200930804     C                   Z-ADD     AGRA(X)       FG7AGR
044300930804     C                   Z-ADD     ADDE(X)       FG7DDE
044400930804     C                   Z-ADD     ADER(X)       FG7DER
044500930804     C                   Z-ADD     AGRE(X)       FG7GRE
044600930804     C                   Z-ADD     X             FG7FIL
044700010921     C                   WRITE     FLFG76
044800930804     C                   ENDIF
044900930803     C*
045000930804     C                   ENDDO
045100930730     C*
045200020306     C                   CLEAR                   TRUL27
045300020306     C                   MOVEL     'C'           I27TLA
045400020306     C                   CALL      'TRUL27R'
045500020306     C                   PARM                    TRUL27
045600930804     C                   SETON                                        LR
045700950113     C*-----------------------------------------------------------------*
045800950113     C*  PARGCP: Gestione codici particolarità in partenza           ---*
045900950113     C*-----------------------------------------------------------------*
046000950113     C     PARGCP        BEGSR
046100950113     C                   Z-ADD     GCPAAS        KAAS
046200950113     C                   Z-ADD     GCPLNP        KLNP
046300950113     C                   Z-ADD     GCPNSP        KNSP
046400950113     C                   Z-ADD     GCPNRS        KNRS
046500950113     C                   MOVEL     *BLANKS       BLPCTR
046600950113     C                   MOVEL     *BLANKS       BLPGGA
046700950113     C     KSPE          CHAIN     FNBLP01L                           31
046800950116     C     BLPGGA        IFNE      *BLANKS
046900950113     C                   Z-ADD     1             §F
047000950113     C     BLPGGA        LOOKUP    C7Q(§F)                                32
047100950113     C     *IN32         IFEQ      '1'
047200950113     C                   MOVEL     D7Q(§F)       DS7Q
047300950113     C     §7Q3MF        IFEQ      'A'
047400950113     C     §7Q3EV        ANDNE     *BLANKS
047500950113     C                   MOVEL     '2A'          COD
047600950113     C                   MOVEL     *BLANK        KEY
047700950113     C                   MOVEL     §7Q3EV        KEY
047800950113     C     KTAB          CHAIN     TABEL                              23
047900950113     C     *IN23         IFEQ      '0'
048000950113     C                   MOVEL     TBLUNI        DS2A
048100950113     C     §2AGLA        IFGT      0
048200950113     C                   CLEAR                   TRUL52
048300950113     C                   Z-ADD     §2AGLA        D52GIO
048400950113     C                   Z-ADD     DATCON        D52DIN
048500950113     C                   CALL      'TRUL52R'
048600950113     C                   PARM                    TRUL52
048700950113     C     D52ERR        IFEQ      *BLANKS
048800950116     C                   Z-ADD     D52DFI        DTAPER            8 0
048900950113     C                   END
049000950113     C                   END
049100950113     C                   END
049200950113     C*
049300950113     C                   END
049400950113     C                   END
049500950116     C                   END
049600950113     C*
049700950113     C                   ENDSR
049800950116     C*-----------------------------------------------------------------*
049900950116     C*  PARGCA: Gestione codici particolarità in Arrivo             ---*
050000950116     C*-----------------------------------------------------------------*
050100950116     C     PARGCA        BEGSR
050200950116     C*
050300950116     C     ARBGGA        IFNE      *BLANKS
050400950116     C                   Z-ADD     1             §F
050500950116     C     ARBGGA        LOOKUP    C7Q(§F)                                32
050600950116     C     *IN32         IFEQ      '1'
050700950116     C                   MOVEL     D7Q(§F)       DS7Q
050800950116     C     §7Q2MF        IFEQ      'A'
050900950116     C     §7Q2EV        ANDNE     *BLANKS
051000950116     C                   MOVEL     '2A'          COD
051100950116     C                   MOVEL     *BLANK        KEY
051200950116     C                   MOVEL     §7Q2EV        KEY
051300950116     C     KTAB          CHAIN     TABEL                              23
051400950116     C     *IN23         IFEQ      '0'
051500950116     C                   MOVEL     TBLUNI        DS2A
051600950116     C     §2AGLA        IFGT      0
051700950116     C                   CLEAR                   TRUL52
051800950116     C                   Z-ADD     §2AGLA        D52GIO
051900950116     C                   Z-ADD     DATDXD        D52DIN
052000950116     C                   CALL      'TRUL52R'
052100950116     C                   PARM                    TRUL52
052200950116     C     D52ERR        IFEQ      *BLANKS
052300950116     C                   Z-ADD     D52DFI        DATDXD            8 0
052400950116     C                   END
052500950116     C                   END
052600950116     C                   END
052700950116     C*
052800950116     C                   END
052900950116     C                   END
053000950116     C                   END
053100950116     C*
053200950116     C                   ENDSR
053300941013     C*-----------------------------------------------------------------*
053400941013     C*  Reperisco giorni rientro             --------------------------*
053500941013     C*-----------------------------------------------------------------*
053600941013     C     REPGGR        BEGSR
053700941013     C** Posizionamento sulla bolla per reperire codice tariffa
053800950113     C                   MOVEL     'N'           WTROVA            1
053900941013     C                   Z-ADD     0             GIOR              2 0
054000941013     C                   Z-ADD     GCPSCM        KKSC
054100941013     C                   MOVEL     BLPCTR        KCTR
054200941013     C                   MOVEL     BLPAAS        DATSPE            8 0
054300941013     C                   MOVE      BLPMGS        DATSPE
054400020305     C* RICHIAMO FUNZIONE TRUL27R PER REPERIRE IL "FIE" E TARIFFA DI
054500020305     C* CARTELLO
054600020305     C                   CLEAR                   TRUL27
054700021118     C                   MOVE      BLPctr        I27ctb
054800020305     C                   MOVEL     BLPTSP        I27TSP
054900020305     C                   MOVE      BLPLNA        I27LNA
055000020305     C                   MOVE      BLPLNP        I27LNP
055100020305     C                   MOVE      KKSC          I27CLI
055200020305     C                   MOVE      datspe        I27DTA
055300020305     C                   CALL      'TRUL27R'
055400020305     C                   PARM                    TRUL27
055500941013     C                   Z-ADD     GCPSCM        APPO              4 0
055600941013     C     APPO          IFNE      8888
055700941013     C     APPO          ANDNE     9999
055800950113     C     APPO          ANDNE     0
055900150514     C                   CLEAR                   fnlv59ds
056000150514     C                   MOVEL     ' '           ilv59TBO
056100150514     C                   Z-ADD     DATSPE        ilv59DSP
056200150514     C                   MOVE      BLPCTR        ilv59CTR
056300150514     C                   MOVE      KKSC          ilv59KSC
056400150514     C                   MOVE      KCI           ilv59KCI
056500150514     C                   MOVEL     o27fie        ilv59fie
056600150514     C                   MOVEL     BLPTSP        ilv59TSP
056700150514     C                   MOVEL     'S'           ilv59CON
056800150514     c                   eval      ilv59ta2='B'
056900150514     c                   eval      ilv59blo='S'
057000150514     C                   CALL      'FNLV59R'
057100150514     C                   PARM                    fnlv59ds
057200950113     C*
057300150514     C     olv59ERR      IFEQ      *BLANKS
057400150514     C     olv59tks      ANDEQ     'S'
057500950113     C*  Se ho trovato tariffa in vigore mi posiziono sul file
057600950113     C*  delle tariffe giacenze con il proggressivo giusto
057700150514     C                   MOVE      olv59PRG      KPRG
057800150514     C                   MOVE      ilv59KSC      KKSC
057900150514     C                   MOVE      olv59CTR      KCTR
058000990730     C     KTGC          CHAIN     TITGC01L                           31
058100950113     C     *IN31         IFEQ      '0'
058200950116     C     TGCGGR        ANDNE     *ZEROS
058300950113     C                   MOVE      TGCGGR        GIOR
058400950113     C                   MOVEL     'S'           WTROVA
058500950113     C                   END
058600950113     C                   END
058700950113     C                   END
058800950113     C*  Se non trovato gg. rientro su tariffa cliente li cerco
058900950113     C*  sulla tariffa di cartello
059000950113     C     WTROVA        IFNE      'S'
059100950113     C*  Se ho trovato tariffa in vigore mi posiziono sul file
059200950113     C*  delle tariffe giacenze con il proggressivo giusto
059300020305     C                   MOVE      o27PRG        KPRG
059400020305     C                   MOVE      o27KSC        KKSC
059500020305     C                   MOVE      o27CTR        KCTR
059600990730     C     KTGC          CHAIN     TITGC01L                           31
059700950113     C     *IN31         IFEQ      '0'
059800950116     C     TGCGGR        ANDNE     *ZEROS
059900950113     C                   MOVE      TGCGGR        GIOR
060000950113     C                   MOVEL     'S'           WTROVA
060100950113     C                   END
060200950113     C                   END
060300941013     C** SE NON TROVO SU TARIFFA CARTELLO PRELEVO DA TABELLA
060400950113     C     WTROVA        IFNE      'S'
060500941013     C                   Z-ADD     §2GGGL        GIOR
060600941013     C                   ENDIF
060700020305     C     O27FIE        IFNE      'I'
060800020305     C                   SELECT
060900020305      * Se è da DPD
061000050210     C     o27ntw        WHENEQ    'DPD'
061100020305     C     gior          IFGT      §2GGDP
061200020305     C                   MOVE      §2GGDP        gior
061300020305     C                   END
061400020305      * Se è FEDEX
061500050210     C     o27ntw        WHENEQ    'FED'
061600020305     C     gior          IFGT      §2GGFE
061700020305     C                   MOVE      §2GGFE        gior
061800020305     C                   END
061900020305      * Se è estero
062000050210     C     o27ntw        WHENEQ    'EEX'
062100050210     C     o27ntw        oreq      'EUP'
062200020305     C     gior          IFGT      §2GGRE
062300020305     C                   MOVE      §2GGRE        gior
062400020305     C                   END
062500040112      *
062600020305     C                   ENDSL
062700020305     C                   END
062800050210     C* Il chiodo è stato sostituito con la tabella "20"
062900050210     C*  Se ci sono eccezioni ai resi x decorsi termini sul cliente
063000050210     C*   sui giorni da prendere
063100050210      * Se è un estero Euroexpress
063200050210     C     O27FIE        IFne      'I'
063300050210     C     o27ntw        andeq     'EEX'
063400050210     C                   MOVEL     '20'          COD
063500050210     C                   MOVEL     *blank        KEY
063600050210     C                   MOVEL     gcpSCm        KEY
063700050210     c     ktab          chain     tabel00f
063800050210     c                   if        %found(tabel00f)
063900050210     c                   movel     tbluni        ds20
064000050210     C                   Z-ADD     §20GRE        GIOR
064100050210     c                   endif
064200050210     c                   END
064300941013     C*
064400941013     C                   ENDSR
064500930802     C*-----------
064600930802     C* CONVERTO IN GIORNI LE DUE DATE DA CONFRONTARE
064700930802     C*-----------
064800930802     C     CONV          BEGSR
064900930802     C                   Z-ADD     DATA1         G02INV
065000930802     C                   MOVEL     '3'           G02ERR
065100941013     C                   CALL      'XSRDA8'
065200941013     C                   PARM                    WLBDA8
065300930802     C                   Z-ADD     G02TGI        GIO1              5 0
065400930802     C                   Z-ADD     DATA2         G02INV
065500930802     C                   MOVEL     '3'           G02ERR
065600941013     C                   CALL      'XSRDA8'
065700941013     C                   PARM                    WLBDA8
065800930802     C                   Z-ADD     G02TGI        GIO2              5 0
065900930802     C                   ENDSR
066000930802     C*-----------
066100930802     C* CALCOLO GIORNI FESTIVI INTERCORRENTE TRA LE DUE DATE IN ESAME
066200930802     C*-----------
066300930802     C     FEST          BEGSR
066400930802     C                   Z-ADD     DATA1         DATADA
066500930802     C                   Z-ADD     DATA2         DATAA
066600990906     C                   CALL      'XGGFEST'
066700941013     C                   PARM                    WDAT8
066800930802     C                   ENDSR
066900930803     C*-----------
067000930803     C* SUBROUTINE INIZIALE
067100930803     C*-----------
067200930803     C     *INZSR        BEGSR
067300930803     C     *ENTRY        PLIST
067400930803     C                   PARM                    KPJBA
067500930806     C                   MOVEL     KPJBU         PARAM
067600930804     C                   Z-ADD     1             CODUT             1 0
067700950116     C*---------------------------------------------------------------*
067800950116     C                   CALL      'X§PARUT'
067900950116     C                   PARM                    UT§DSE
068000950116     C*--- RICERCA CAPOCONTI------------------------------------------*
068100950116     C                   DO        50            X
068200950116     C                   MOVE      TCU(X)        TCUDS
068300950116     C     F56           IFEQ      'CG'
068400950116     C     F4            ANDEQ     '1'
068500950116     C     F3            ANDEQ     '0'
068600950116     C                   Z-ADD     KCU(X)        KCI               4 0
068700950116     C                   END
068800950116     C                   END
068900950116     C*---------------------------------------------------------------*
069000930803     C** CHIAVE DI TABEL
069100930803     C     KTAB          KLIST
069200930803     C                   KFLD                    CODUT
069300930803     C                   KFLD                    COD               2
069400930803     C                   KFLD                    KEY               8
069500950113     C*
069600950113     C     KTAB1         KLIST
069700950113     C                   KFLD                    CODUT
069800950113     C                   KFLD                    COD               2
069900930804     C** CHIAVE DI FLEVB
070000930804     C     KEVB          KLIST
070100990803     C                   KFLD                    KAAS
070200941013     C                   KFLD                    KLNP
070300941013     C                   KFLD                    KNRS
070400941013     C                   KFLD                    KNSP
070500941013     C                   KFLD                    KDEV
070600931124     C** CHIAVE DI FLEVB
070700931124     C     KEVBS         KLIST
070800990803     C                   KFLD                    KAAS
070900941013     C                   KFLD                    KLNP
071000941013     C                   KFLD                    KNRS
071100941013     C                   KFLD                    KNSP
071200010921     C** CHIAVE DI FLFG765L
071300930810     C     KFG7          KLIST
071400010921     C                   KFLD                    KAA4
071500941013     C                   KFLD                    KPER
071600020829     C                   KFLD                    linea             3 0
071700941013     C** CHIAVE DI FNBLP/FNARB
071800941013     C     KSPE          KLIST
071900941013     C                   KFLD                    KAAS
072000941013     C                   KFLD                    KLNP
072100941013     C                   KFLD                    KNRS
072200941013     C                   KFLD                    KNSP
072300941013     C** CHIAVI DI TNTAM/TNTGC
072400941013     C     KTGC          KLIST
072500941013     C                   KFLD                    KKSC
072600941013     C                   KFLD                    KCTR
072700941013     C                   KFLD                    KPRG
072800941013     C*----------------------------------------------------*
072900941013     C*  Definizione variabili
073000941013     C     *LIKE         DEFINE    EVBDEV        KDEV
073100010921     C     *LIKE         DEFINE    FG7AAS        KAA4
073200941013     C     *LIKE         DEFINE    FG7PER        KPER
073300941013     C     *LIKE         DEFINE    GCPAAS        KAAS
073400941013     C     *LIKE         DEFINE    GCPLNP        KLNP
073500941013     C     *LIKE         DEFINE    GCPNRS        KNRS
073600941013     C     *LIKE         DEFINE    GCPNSP        KNSP
073700941013     C     *LIKE         DEFINE    GCPSCM        KKSC
073800950113     C     *LIKE         DEFINE    TGCCTR        KCTR
073900950113     C     *LIKE         DEFINE    TGCPRG        KPRG
074000941013     C*----------------------------------------------------*
074100941013     C* DATA ELABORAZ. IN AAMMGG
074200941013     C                   TIME                    WHHDAT           14 0
074300941013     C                   MOVE      WHHDAT        G02DAT
074400941013     C                   MOVE      *BLANKS       G02ERR
074500941013     C                   CALL      'XSRDA8'
074600941013     C                   PARM                    WLBDA8
074700941013     C                   Z-ADD     G02INV        DATELA            8 0
074800941013     C*----------------------------------------------------*
074900020808     C* Caricamento £1
075000020808     c                   clear                   trul06ds
075100020809     c                   movel     kpjbu         savkpjbu
075200020808     c                   eval      d06cod = 'PP'
075300050301     c                   movel     fil           d06key
075400020808     c                   movel     trul06ds      kpjbu
075500020808     c                   call      'TRUL06R'
075600020808     c                   parm                    kpjba
075700020808     c                   movel     kpjbu         trul06ds
075800020809     c                   movel     savkpjbu      kpjbu
075900941013     C** CARICO TABELLA '2G'
076000941013     C                   MOVEL     '2G'          COD
076100941013     C                   MOVEL     *BLANKS       KEY
076200941013     C                   MOVEL     1             KEY
076300941013     C     KTAB          CHAIN     TABEL                              30
076400941013     C  N30              MOVEL     TBLUNI        DS2G
076500941013     C   30              CLEAR                   DS2G
076600950113     C* Carico DATI TABELLA PARTICOLARITA'
076700950113     C                   Z-ADD     0             §F                3 0
076800950113     C                   MOVEL     '7Q'          COD
076900950113     C                   MOVEL     *BLANK        KEY
077000950113     C     KTAB1         CHAIN     TABEL                              21
077100950113     C     *IN21         DOWEQ     '0'
077200950113     C     §F            ANDLE     100
077300950113     C     TBLFLG        IFEQ      *BLANK
077400950113     C                   ADD       1             §F
077500950113     C                   MOVEL     TBLUNI        D7Q(§F)
077600950113     C                   MOVEL     TBLKEY        C7Q(§F)
077700950113     C                   END
077800950113     C     KTAB1         READE     TABEL                                  21
077900950113     C                   END
078000050225     c*
078100010921     C                   Z-ADD     ANNO          KAA4
078200941013     C                   Z-ADD     PER           KPER
078300941013     C** VERIFICO SE IL PERIODO PASSATO E' GIA' STATO ELABORATO
078400020829     c                   do        30            ff
078500020829     c                   if        lin(ff) = 0
078600020829     c                   leave
078700020829     c                   end
078800020829     c                   z-add     lin(ff)       linea
078900010921     C     KFG7          SETLL     FLFG765L                               80
079000941013     C** SE E' GIA' ELABORATO CANCELLO L'ELABORAZIONE PRECEDENTE
079100941013     C     *IN80         IFEQ      *ON
079200941013     C                   SETOFF                                       18
079300941013     C     *IN18         DOWEQ     *OFF
079400010921     C     KFG7          READE     FLFG765L                               18
079500010921     C  N18              DELETE    FLFG765
079600941013     C                   ENDDO
079700941013     C                   ENDIF
079800020829     C                   enddo
079900941013     C*----------------------------------------------------*
080000941013     C                   Z-ADD     0             H
080100941013     C                   CLEAR                   SKI
080200941013     C                   Z-ADD     ANNO          FG7AAS
080300941013     C                   Z-ADD     PER           FG7PER
080400941013     C                   Z-ADD     0             FG7DTR
080500941013     C                   MOVEL     *BLANKS       FG7FTR
080600941013     C*----------------------------------------------------*
080700930803     C                   ENDSR

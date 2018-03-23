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
009000160302     d  KS_CT                 74    118    dim(15)
009100160302     d  KS_pr                119    163    dim(15)
009200160302     d  KS_ie                164    178    dim(15)
009300160302     d
009400950116     D TRUL52        E DS                  EXTNAME(TRUL52DS)
009500930203     D DS2G          E DS
009600050210     D DS20          E DS
009700950113     D DS7Q          E DS
009800000202     D OG143         E DS
009900950113     D DS15          E DS
010000930804     D DS2A          E DS
010100930203     D PARAM           DS
010200941013     D  DAL                    1      8  0
010300941013     D  AL                     9     16  0
010400941013     D  ANNO                  17     20  0
010500941013     D  PER                   21     22  0
010600050301     D  FIL                   23     25  0
010700900418     D KPJBA         E DS
010800160302     D TRUL27        E DS                  EXTNAME(TRUL27DS1)
010900160302     D TRULC7DS      E DS
011000020808     d trul06ds      e ds
011100020808     d  lin                    1     90  0 dim(30)
011200020809     d savkpjbu        s                   like(kpjbu)
011300930803     D*-------------
011400941013     D* DS PER ROUTINE 'XSRDA8'
011500930803     D*-------------
011600941013     D WLBDA8          DS
011700941013     D  G02DAT                 1      8  0
011800941013     D  G02INV                 9     16  0
011900941013     D  G02ERR                17     17
012000941013     D  G02TGI                18     22  0
012100930803     D*-------------
012200990906     D* DS PER ROUTINE 'XGGFEST'
012300930803     D*-------------
012400941013     D WDAT8           DS
012500941013     D  DATADA                 1      8  0
012600941013     D  DATAA                  9     16  0
012700941013     D  GGF                   17     21  0
012800930803     D*-------------
012900941013     D* DS PER ROUTINE 'XSRGI8'
013000930803     D*-------------
013100930803     D WGIDAT          DS
013200941013     D  GIODAT                 1      8  0
013300941013     D  GIOINV                 9     16  0
013400941013     D  GIOTGI                17     21  0
013500900423     C*---------------------------------------------------------------*
013600020808     c                   do        30            ff                3 0
013700020808     c                   setoff                                       50
013800020808     c                   if        lin(ff) = 0
013900020808     c                   leave
014000020808     c                   endif
014100050225     C     lin(ff)       SETLL     tigcp07l
014200020808     C                   DO        *HIVAL
014300050225     C     lin(ff)       READE     tigcp07l                               50
014400020808     C   50              leave
014500930803     C** MEMORIZZO FILIALE IN ESAME
014600930730     C                   Z-ADD     GCPLNP        I                 3 0
014700930804     C** MEMORIZZO FILIALE IN ESAME IN SCHIERA
014800930804     C     I             LOOKUP    SKI                                    15
014900930804     C  N15              ADD       1             H                 2 0
015000930804     C  N15              Z-ADD     I             SKI(H)
015100930803     C** MEMORIZZO DATA APERTURA O RIAPERTURA SE PRESENTE
015200050225     C                   Z-ADD     GCPDUR        DATCON            8 0
015300950113     C*
015400950116     C                   Z-ADD     DATCON        DTAPER
015500950113     C                   EXSR      PARGCP
015600930730     C*-----------
015700930730     C* APERTURA
015800930730     C*-----------
015900950116     C     DTAPER        IFGE      DAL
016000950116     C     DTAPER        ANDLE     AL
016100020305     c* da aprire
016200931126     C                   ADD       1             NGDA(I)
016300931126     C*  SE  LA DATA LETTERA AL MITTENTE E' = 0  CALCOLO NON FATTE
016400931126     C*  - FORZO DATA LETT.= UDATE X CALC.RITARDO
016500931126     C     GCPDMM        IFEQ      0
016600931126     C*  SE NON RISULTA GIA' CHIUSA  ( PUO' SUCCEDERE
016700931126     C*  CHE L'ARRIVO LA CHIUDE PRIMA CHE SIA STAMPATO MODULO)
016800931126     C     GCPDCG        IFEQ      *ZEROS
016900020305     c* non aperte
017000931126     C                   ADD       1             NGNA(I)
017100931126     C                   MOVE      DATELA        GCPDMM
017200931126     C                   ELSE
017300950116     C                   MOVE      DTAPER        GCPDMM
017400931126     C                   ENDIF
017500931126     C                   ENDIF
017600931126     C** --- CALCOLO  RITARDO ---
017700930802     C     GCPDMM        IFGT      0
017800930803     C** TRASFORMO IN GIORNI ASSOLUTI LE DUE DATE DA CONFRONTARE E DETERMINO
017900930803     C** SE CI SONO RITARDI
018000950116     C                   Z-ADD     DTAPER        DATA1             8 0
018100941013     C                   Z-ADD     GCPDMM        DATA2             8 0
018200930802     C                   EXSR      CONV
018300930803     C** CALCOLO LA DIFFERENZA TRA LE DUE DATE
018400010921     C     GIO2          SUB       GIO1          RITAP             5 0
018500930804     C     RITAP         IFGT      §2GGRP
018600930803     C** CALCOLO GIORNI FESTIVI INTERCORRENTI TRA LE DUE DATE E LI SOTTRAGGO
018700930803     C** DAI GIORNI DI RITARDO PER VEDERE SE SONO ANCORA IN RITARDO
018800930802     C                   EXSR      FEST
018900930802     C     RITAP         SUB       GGF           RITAP
019000930804     C     RITAP         IFGT      §2GGRP
019100931125     C     RITAP         SUB       §2GGRP        RITAP
019200930802     C                   ADD       1             NGAR(I)
019300930804     C                   ADD       RITAP         NGRA(I)
019400930802     C                   ENDIF
019500930802     C                   ENDIF
019600930802     C                   ENDIF
019700930802     C*
019800930802     C                   ENDIF
019900930802     C*-----------
020000930802     C* IMMISSIONE DISPOSIZIONI DI RESO
020100930802     C*-----------
020200941013     C** Reperisco gg. rientro da tariffa
020300941013     C                   EXSR      REPGGR
020400930803     C** CALCOLO SE LA DATA E' INFERIORE ALLA DATA LIMITE SUPERIORE QUINDI LA
020500930803     C** MERCE SAREBBE DA RIENTRARE
020600950116     C                   Z-ADD     DTAPER        G02INV
020700930802     C                   MOVE      '3'           G02ERR
020800941013     C                   CALL      'XSRDA8'
020900941013     C                   PARM                    WLBDA8
021000930802     C     G02TGI        ADD       GIOR          GIOTGI
021100941013     C                   CALL      'XSRGI8'
021200941013     C                   PARM                    WGIDAT
021300931124     C** SE DATA APERT.+GG RIENTRO E' COMPRESA NELL'INTERVALLO
021400931124     C**(CIOè AVREI DOVUTO DARE DISPOSIZ. X SCADUTI TERMINI)
021500930802     C     GIOINV        IFGE      DAL
021600930802     C     GIOINV        ANDLE     AL
021700931124     C*
021800931126     C** NON  SONO STATE DATE DISPOSIZIONI NELL'PERIODO ELAB.
021900941013     C     GCPDIS        IFEQ      *BLANKS
022000020305     c* non immesse
022100931126     C                   ADD       1             NDNI(I)
022200020305     c* da immettere
022300931126     C                   ADD       1             NDDI(I)
022400931126     C*  - FORZO DATA IMMISS .= UDATE X CALC.RITARDO
022500931126     C                   MOVE      DATELA        GCPDDM
022600931126     C                   ELSE
022700931126     C** SONO STATE DATE DISPOSIZIONI DI RESO
022800941013     C     GCPDIS        IFEQ      'R'
022900931126     C                   ADD       1             NDDI(I)
023000931126     C                   ENDIF
023100931126     C                   ENDIF
023200931126     C** --- CALCOLO  RITARDO ---
023300931124     C** E SONO STATE DATE DISPOSIZIONI DI RESO
023400931126     C** O NON SONO STATE IMMESSE
023500941013     C     GCPDIS        IFEQ      'R'
023600941013     C     GCPDIS        OREQ      *BLANKS
023700931123     C** CALCOLO GIORNI DI RITARDO SU IMMISSIONE DISPOSIZIONE
023800020305     C* D.APER.+GG RESO
023900931123     C                   Z-ADD     GIOINV        DATA1
024000020305     C* DATA IMMISS.DIS
024100931123     C                   Z-ADD     GCPDDM        DATA2
024200930802     C                   EXSR      CONV
024300010921     C     GIO2          SUB       GIO1          RITDI             5 0
024400930802     C     RITDI         IFGT      §2GGRD
024500930802     C** CALCOLO GIORNI FESTIVI TRA LE DUE DATE
024600930802     C                   EXSR      FEST
024700930802     C     RITDI         SUB       GGF           RITDI
024800930802     C     RITDI         IFGT      §2GGRD
024900931125     C     RITDI         SUB       §2GGRD        RITDI
025000020305     C*NR.IMMESSE RITAR
025100931123     C                   ADD       1             NDRO(I)
025200020305     C*GG RITARDO
025300931123     C                   ADD       RITDI         NGRD(I)
025400930802     C                   ENDIF
025500930802     C                   ENDIF
025600931124     C                   ENDIF
025700930802     C*
025800930804     C                   ENDIF
025900930802     C*-----------
026000930802     C* CESSATE GIACENZE
026100930802     C*-----------
026200930802     C     GCPDCG        IFGE      DAL
026300930802     C     GCPDCG        ANDLE     AL
026400050808     C     GCPfrg        ANDeq     0
026500020305     C* DA CHIUDERE
026600050420     c                   if        gcpfas >=40
026700931126     C                   ADD       1             NGDC(I)
026800050412     c                   end
026900930803     C** PER QUELLE CESSATE CONTO QUELLE SENZA SPESE
027000950210     C     GCPNBW        IFEQ      *ZEROS
027100950301     C     GCPDBG        ANDGT     0
027200050420     c     gcpfas        andge     50
027300930803     C                   ADD       1             NGSS(I)
027400930803     C                   ENDIF
027500950301     C** NON  SONO STATE CHIUSE
027600950301     C     GCPDBG        IFEQ      0
027700050406     c     gcpfas        andeq     40
027800020305     C* NON CHIUSE
027900950301     C                   ADD       1             NGNC(I)
028000950301     C*  - FORZO DATA BOLLA ADD. = UDATE X CALC.RITARDO
028100950301     C                   MOVE      DATELA        GCPDBG
028200950301     C                   ENDIF
028300930803     C*
028400950301     C** --- CALCOLO  RITARDO ---
028500950301     C**  TRA DATA BOLLA ADDEBITO E DATA CHIUSURA GIACENZA
028600950301     C     GCPDBG        IFGT      0
028700930803     C                   Z-ADD     GCPDCG        DATA1
028800930803     C                   Z-ADD     GCPDBG        DATA2
028900930803     C                   EXSR      CONV
029000010921     C     GIO2          SUB       GIO1          RITCE             5 0
029100930803     C     RITCE         IFGT      §2GGRC
029200930803     C** CALCOLO GIORNI FESTIVI TRA LE DUE DATE
029300930803     C                   EXSR      FEST
029400930803     C     RITCE         SUB       GGF           RITCE
029500930803     C     RITCE         IFGT      §2GGRC
029600931125     C     RITCE         SUB       §2GGRC        RITCE
029700930803     C                   ADD       1             NGCO(I)
029800930803     C                   ADD       RITCE         NGRC(I)
029900930803     C                   ENDIF
030000930803     C                   ENDIF
030100930803     C                   ENDIF
030200930803     C                   ENDIF
030300930804     C*
030400930803     C                   ENDDO
030500020808     C                   enddo
030600930803     C*************** FINE CICLO PARTENZA ******************
030700020808     c
030800020808     c                   do        30            ff                3 0
030900020808     c                   setoff                                       60
031000020808     c                   if        lin(ff) = 0
031100020808     c                   leave
031200020808     c                   endif
031300050225     C     lin(ff)       SETLL     tigcp24L
031400020808     C                   DO        *HIVAL
031500050225     C     lin(ff)       READE     tigcp24L                               60
031600020808     C   60              leave
031700020808     C     GCPATB        IFEQ      *BLANKS
031800930803     C** MEMORIZZO DATA APERTURA O RIAPERTURA SE PRESENTE
031900941013     C                   Z-ADD     GCPDUR        DATCON
032000930804     C** MEMORIZZO FILIALE IN ESAME
032100930804     C                   Z-ADD     GCPFGC        I                 3 0
032200930804     C** MEMORIZZO FILIALE IN ESAME IN SCHIERA
032300930804     C     I             LOOKUP    SKI                                    15
032400930804     C  N15              ADD       1             H
032500930804     C  N15              Z-ADD     I             SKI(H)
032600930803     C*-----------
032700930803     C* APERTURA
032800930803     C*-----------
032900930803     C     DATCON        IFGE      DAL
033000930803     C     DATCON        ANDLE     AL
033100941013     C** Eseguo posiz. su bolla per vedere se c'era il fermo deposito
033200050225     C                   Z-ADD     GCPAASa       KAAS
033300050225     C                   Z-ADD     GCPLNPa       KLNP
033400050225     C                   Z-ADD     GCPNSPa       KNSP
033500050225     C                   Z-ADD     GCPNRSa       KNRS
033600941013     C                   MOVEL     *BLANKS       ARBFFD
033700950116     C                   MOVEL     *BLANKS       ARBGGA
033800941013     C     KSPE          CHAIN     FNARB01L                           31
033900941013     C     ARBFFD        IFEQ      'S'
034000051107     C     ARBFFD        oreq      'M'
034100941013     C                   MOVEL     *BLANKS       KEY
034200941013     C                   MOVEL     'F  '         KEY
034300941013     C                   ELSE
034400930804     C** CONFRONTO DATA APERTURA CON DATA COMUNICAZIONE TENENDO CONTO
034500930804     C** DEGLI EVENTUALI GIORNI LASCIATO AVVISO
034600990803     C                   Z-ADD     GCPAAS        KAAS
034700941013     C                   Z-ADD     GCPDXD        KDEV
034800930804     C                   ADD       1             AGAP(I)
034900930804     C                   Z-ADD     0             GGG
035000990803     C     KEVB          SETLL     FNEVB01L                           40
035100930804     C     *IN40         DOWEQ     *OFF
035200990803     C     KEVBS         READE     FNEVB000                               40
035300931124     C* SOMMO TUTTI I GIORNI L.AVV FINO ALL'APERT.GIAC.
035400990803     C  N40EVBDEV        IFLT      DATCON
035500930804     C                   MOVEL     *BLANKS       KEY
035600930804     C                   MOVEL     EVBCEV        KEY
035700941013     C                   END
035800941013     C                   ENDDO
035900941013     C                   ENDIF
036000941013     C*
036100941013     C                   MOVEL     '2A'          COD
036200930804     C     KTAB          CHAIN     TABEL                              39
036300930804     C  N39              MOVEL     TBLUNI        DS2A
036400930804     C  N39§2AGLA        IFGT      0
036500931124     C                   ADD       §2AGLA        GGG               3 0
036600930804     C                   ENDIF
036700930804     C*
036800930804     C** TROVO DIFFERENZA TRA LE DUE DATE
036900950116     C                   Z-ADD     GCPDXD        DATDXD
037000950116     C                   EXSR      PARGCA
037100950116     C                   Z-ADD     DATDXD        DATA1
037200930804     C                   Z-ADD     DATCON        DATA2
037300930804     C                   EXSR      CONV
037400010921     C     GIO2          SUB       GIO1          RITAA             5 0
037500930804     C     RITAA         SUB       GGG           RITAA
037600930804     C** SE HO RITARDO CONTO GIORNI FESTIVI TRA LE DUE DATE
037700930804     C     RITAA         IFGT      §2GRAA
037800930804     C                   EXSR      FEST
037900930804     C     RITAA         SUB       GGF           RITAA
038000930804     C     RITAA         IFGT      §2GRAA
038100931125     C     RITAA         SUB       §2GRAA        RITAA
038200930804     C                   ADD       1             AGAR(I)
038300930804     C                   ADD       RITAA         AGRA(I)
038400930804     C                   ENDIF
038500930804     C                   ENDIF
038600930804     C*
038700930804     C                   ENDIF
038800930804     C*-----------
038900930804     C* ESECUZIONE DISPOSIZIONI
039000930804     C*-----------
039100050225     C     GCPDeda       IFGE      DAL
039200050225     C     GCPDeda       ANDLE     AL
039300930804     C                   ADD       1             ADDE(I)
039400930804     C** CALCOLO LA DIFFERENZA TRA LA DATA ESECUZIONE E LA DATA DISPOSIZIONE
039500020305     c* data eseguib.
039600941013     C     GCPDED        IFGT      0
039700941013     C                   Z-ADD     GCPDED        DATA1
039800941013     C                   ELSE
039900050225     c* data immissione
040000931124     C     GCPDLD        IFGT      0
040100931124     C                   Z-ADD     GCPDLD        DATA1
040200931124     C                   ELSE
040300020305     c* data immiss.
040400931124     C                   Z-ADD     GCPDDM        DATA1
040500931124     C                   END
040600941013     C                   END
040700941013     C*
040800050225     C                   Z-ADD     GCPDeda       DATA2
040900930804     C                   EXSR      CONV
041000010921     C     GIO2          SUB       GIO1          RITES             5 0
041100930804     C** SE HO RITARDO CONTO GIORNI FESTIVI TRA LE DUE DATE
041200930804     C     RITES         IFGT      §2GREA
041300930804     C                   EXSR      FEST
041400930804     C     RITES         SUB       GGF           RITES
041500930804     C     RITES         IFGT      §2GREA
041600931125     C     RITES         SUB       §2GREA        RITES
041700930804     C                   ADD       1             ADER(I)
041800930804     C                   ADD       RITES         AGRE(I)
041900930804     C                   ENDIF
042000930804     C                   ENDIF
042100930804     C*
042200930804     C                   ENDIF
042300930804     C*
042400930804     C                   ENDIF
042500930803     C                   ENDDO
042600020808     c                   enddo
042700930804     C**************** FINE CICLO ARRIVO *******************
042800930804     C                   DO        19            Z                 3 0
042900930804     C     SKI(Z)        IFGT      0
043000930804     C                   Z-ADD     SKI(Z)        X                 3 0
043100930803     C*-----------------------------------------------------------*
043200930804     C                   Z-ADD     NGDA(X)       FG7GDA
043300930804     C                   Z-ADD     NGNA(X)       FG7GNA
043400930804     C                   Z-ADD     NGAR(X)       FG7GAR
043500930804     C                   Z-ADD     NGRA(X)       FG7GRA
043600930804     C                   Z-ADD     NDDI(X)       FG7DDI
043700930804     C                   Z-ADD     NDRO(X)       FG7DRO
043800930804     C                   Z-ADD     NDNI(X)       FG7DNI
043900930804     C                   Z-ADD     NGRD(X)       FG7GRD
044000930804     C                   Z-ADD     NGDC(X)       FG7GDC
044100930804     C                   Z-ADD     NGNC(X)       FG7GNC
044200930804     C                   Z-ADD     NGCO(X)       FG7GCO
044300930804     C                   Z-ADD     NGRC(X)       FG7GRC
044400930804     C                   Z-ADD     NGSS(X)       FG7GSS
044500930804     C                   Z-ADD     AGAP(X)       FG7GAP
044600930811     C                   Z-ADD     AGAR(X)       FG7AAR
044700930804     C                   Z-ADD     AGRA(X)       FG7AGR
044800930804     C                   Z-ADD     ADDE(X)       FG7DDE
044900930804     C                   Z-ADD     ADER(X)       FG7DER
045000930804     C                   Z-ADD     AGRE(X)       FG7GRE
045100930804     C                   Z-ADD     X             FG7FIL
045200010921     C                   WRITE     FLFG76
045300930804     C                   ENDIF
045400930803     C*
045500930804     C                   ENDDO
045600930730     C*
045700020306     C                   CLEAR                   TRUL27
045800020306     C                   MOVEL     'C'           I27TLA
045900160302     C                   CALL      'TRUL27R1'
046000020306     C                   PARM                    TRUL27
046100160302
046200160302     C                   CLEAR                   TRULc7ds
046300160302     C                   MOVEL     'C'           Ic7TLA
046400160302     C                   CALL      'TRULC7R'
046500160302     C                   PARM                    TRULc7ds
046600930804     C                   SETON                                        LR
046700950113     C*-----------------------------------------------------------------*
046800950113     C*  PARGCP: Gestione codici particolarità in partenza           ---*
046900950113     C*-----------------------------------------------------------------*
047000950113     C     PARGCP        BEGSR
047100950113     C                   Z-ADD     GCPAAS        KAAS
047200950113     C                   Z-ADD     GCPLNP        KLNP
047300950113     C                   Z-ADD     GCPNSP        KNSP
047400950113     C                   Z-ADD     GCPNRS        KNRS
047500950113     C                   MOVEL     *BLANKS       BLPCTR
047600950113     C                   MOVEL     *BLANKS       BLPGGA
047700950113     C     KSPE          CHAIN     FNBLP01L                           31
047800950116     C     BLPGGA        IFNE      *BLANKS
047900950113     C                   Z-ADD     1             §F
048000950113     C     BLPGGA        LOOKUP    C7Q(§F)                                32
048100950113     C     *IN32         IFEQ      '1'
048200950113     C                   MOVEL     D7Q(§F)       DS7Q
048300950113     C     §7Q3MF        IFEQ      'A'
048400950113     C     §7Q3EV        ANDNE     *BLANKS
048500950113     C                   MOVEL     '2A'          COD
048600950113     C                   MOVEL     *BLANK        KEY
048700950113     C                   MOVEL     §7Q3EV        KEY
048800950113     C     KTAB          CHAIN     TABEL                              23
048900950113     C     *IN23         IFEQ      '0'
049000950113     C                   MOVEL     TBLUNI        DS2A
049100950113     C     §2AGLA        IFGT      0
049200950113     C                   CLEAR                   TRUL52
049300950113     C                   Z-ADD     §2AGLA        D52GIO
049400950113     C                   Z-ADD     DATCON        D52DIN
049500950113     C                   CALL      'TRUL52R'
049600950113     C                   PARM                    TRUL52
049700950113     C     D52ERR        IFEQ      *BLANKS
049800950116     C                   Z-ADD     D52DFI        DTAPER            8 0
049900950113     C                   END
050000950113     C                   END
050100950113     C                   END
050200950113     C*
050300950113     C                   END
050400950113     C                   END
050500950116     C                   END
050600950113     C*
050700950113     C                   ENDSR
050800950116     C*-----------------------------------------------------------------*
050900950116     C*  PARGCA: Gestione codici particolarità in Arrivo             ---*
051000950116     C*-----------------------------------------------------------------*
051100950116     C     PARGCA        BEGSR
051200950116     C*
051300950116     C     ARBGGA        IFNE      *BLANKS
051400950116     C                   Z-ADD     1             §F
051500950116     C     ARBGGA        LOOKUP    C7Q(§F)                                32
051600950116     C     *IN32         IFEQ      '1'
051700950116     C                   MOVEL     D7Q(§F)       DS7Q
051800950116     C     §7Q2MF        IFEQ      'A'
051900950116     C     §7Q2EV        ANDNE     *BLANKS
052000950116     C                   MOVEL     '2A'          COD
052100950116     C                   MOVEL     *BLANK        KEY
052200950116     C                   MOVEL     §7Q2EV        KEY
052300950116     C     KTAB          CHAIN     TABEL                              23
052400950116     C     *IN23         IFEQ      '0'
052500950116     C                   MOVEL     TBLUNI        DS2A
052600950116     C     §2AGLA        IFGT      0
052700950116     C                   CLEAR                   TRUL52
052800950116     C                   Z-ADD     §2AGLA        D52GIO
052900950116     C                   Z-ADD     DATDXD        D52DIN
053000950116     C                   CALL      'TRUL52R'
053100950116     C                   PARM                    TRUL52
053200950116     C     D52ERR        IFEQ      *BLANKS
053300950116     C                   Z-ADD     D52DFI        DATDXD            8 0
053400950116     C                   END
053500950116     C                   END
053600950116     C                   END
053700950116     C*
053800950116     C                   END
053900950116     C                   END
054000950116     C                   END
054100950116     C*
054200950116     C                   ENDSR
054300941013     C*-----------------------------------------------------------------*
054400941013     C*  Reperisco giorni rientro             --------------------------*
054500941013     C*-----------------------------------------------------------------*
054600941013     C     REPGGR        BEGSR
054700941013     C** Posizionamento sulla bolla per reperire codice tariffa
054800950113     C                   MOVEL     'N'           WTROVA            1
054900941013     C                   Z-ADD     0             GIOR              2 0
055000941013     C                   Z-ADD     GCPSCM        KKSC
055100941013     C                   MOVEL     BLPCTR        KCTR
055200941013     C                   MOVEL     BLPAAS        DATSPE            8 0
055300941013     C                   MOVE      BLPMGS        DATSPE
055400020305     C* RICHIAMO FUNZIONE TRUL27R PER REPERIRE IL "FIE" E TARIFFA DI
055500020305     C* CARTELLO
055600020305     C                   CLEAR                   TRUL27
055700160302     C                   CLEAR                   fnlv59ds
055800021118     C                   MOVE      BLPctr        I27ctb
055900020305     C                   MOVEL     BLPTSP        I27TSP
056000020305     C                   MOVE      BLPLNA        I27LNA
056100020305     C                   MOVE      BLPLNP        I27LNP
056200020305     C                   MOVE      KKSC          I27CLI
056300160302     C                   CALL      'TRUL27R1'
056400020305     C                   PARM                    TRUL27
056500941013     C                   Z-ADD     GCPSCM        APPO              4 0
056600160302     c
056700160302
056800941013     C     APPO          IFNE      8888
056900941013     C     APPO          ANDNE     9999
057000950113     C     APPO          ANDNE     0
057100150514     C                   MOVEL     ' '           ilv59TBO
057200150514     C                   Z-ADD     DATSPE        ilv59DSP
057300150514     C                   MOVE      BLPCTR        ilv59CTR
057400150514     C                   MOVE      KKSC          ilv59KSC
057500150514     C                   MOVE      KCI           ilv59KCI
057600150514     C                   MOVEL     o27fie        ilv59fie
057700150514     C                   MOVEL     BLPTSP        ilv59TSP
057800150514     C                   MOVEL     'S'           ilv59CON
057900150514     c                   eval      ilv59ta2='B'
058000150514     c                   eval      ilv59blo='S'
058100150514     C                   CALL      'FNLV59R'
058200150514     C                   PARM                    fnlv59ds
058300950113     C*
058400150514     C     olv59ERR      IFEQ      *BLANKS
058500150514     C     olv59tks      ANDEQ     'S'
058600950113     C*  Se ho trovato tariffa in vigore mi posiziono sul file
058700950113     C*  delle tariffe giacenze con il proggressivo giusto
058800150514     C                   MOVE      olv59PRG      KPRG
058900150514     C                   MOVE      ilv59KSC      KKSC
059000150514     C                   MOVE      olv59CTR      KCTR
059100990730     C     KTGC          CHAIN     TITGC01L                           31
059200950113     C     *IN31         IFEQ      '0'
059300950116     C     TGCGGR        ANDNE     *ZEROS
059400950113     C                   MOVE      TGCGGR        GIOR
059500950113     C                   MOVEL     'S'           WTROVA
059600950113     C                   END
059700950113     C                   END
059800950113     C                   END
059900950113     C*  Se non trovato gg. rientro su tariffa cliente li cerco
060000950113     C*  sulla tariffa di cartello
060100160302    1C     WTROVA        IFNE      'S'
060200160302     c* Carico la cartello :
060300160302     c*  - per 8888 col fie della bolla
060400160302     c*  - per tariffe cliente esistente col fie della tariffa
060500160302     c                   clear                   trulc7ds
060600160302    2C     olv59ERR      ifeq      *blanks
060700160302     C     olv59tks      andeq     'S'
060800160302     c                   z-add     1             indx              3 0
060900160302     c                   eval      indx=%lookup(ilv59ctr:ks_ct)
061000160302    3c                   if        indx>0 and %editc(olv59prg:'X')=
061100160302     c                             ks_pr(indx)
061200160302     c                   eval      ic7tfie=ks_ie(indx)
061300160302   x3c                   else
061400160302     c                   eval      ic7tntw=o27ntw
061500160302    3c                   endif
061600160302     c
061700160302   x2c                   else
061800160302     c*
061900160302     c                   eval      ic7tntw=o27ntw
062000160302    2c                   endif
062100160302     c
062200160302     C                   move      blpctr        Ic7ctb
062300160302     C                   MOVE      datspe        Ic7DTA
062400160302     c                   call      'TRULC7R'
062500160302     c                   parm                    trulc7ds
062600160302     c
062700950113     C*  Se ho trovato tariffa in vigore mi posiziono sul file
062800950113     C*  delle tariffe giacenze con il proggressivo giusto
062900160302     C                   MOVE      oc7PRGc       KPRG
063000160302     C                   MOVE      oc7KSCc       KKSC
063100160302     C                   MOVE      oc7CTRc       KCTR
063200990730     C     KTGC          CHAIN     TITGC01L                           31
063300950113     C     *IN31         IFEQ      '0'
063400950116     C     TGCGGR        ANDNE     *ZEROS
063500950113     C                   MOVE      TGCGGR        GIOR
063600950113     C                   MOVEL     'S'           WTROVA
063700950113     C                   END
063800950113     C                   END
063900941013     C** SE NON TROVO SU TARIFFA CARTELLO PRELEVO DA TABELLA
064000950113     C     WTROVA        IFNE      'S'
064100941013     C                   Z-ADD     §2GGGL        GIOR
064200941013     C                   ENDIF
064300020305     C     O27FIE        IFNE      'I'
064400020305     C                   SELECT
064500020305      * Se è da DPD
064600050210     C     o27ntw        WHENEQ    'DPD'
064700020305     C     gior          IFGT      §2GGDP
064800020305     C                   MOVE      §2GGDP        gior
064900020305     C                   END
065000020305      * Se è FEDEX
065100050210     C     o27ntw        WHENEQ    'FED'
065200020305     C     gior          IFGT      §2GGFE
065300020305     C                   MOVE      §2GGFE        gior
065400020305     C                   END
065500020305      * Se è estero
065600050210     C     o27ntw        WHENEQ    'EEX'
065700050210     C     o27ntw        oreq      'EUP'
065800020305     C     gior          IFGT      §2GGRE
065900020305     C                   MOVE      §2GGRE        gior
066000020305     C                   END
066100040112      *
066200020305     C                   ENDSL
066300020305     C                   END
066400050210     C* Il chiodo è stato sostituito con la tabella "20"
066500050210     C*  Se ci sono eccezioni ai resi x decorsi termini sul cliente
066600050210     C*   sui giorni da prendere
066700050210      * Se è un estero Euroexpress
066800050210     C     O27FIE        IFne      'I'
066900050210     C     o27ntw        andeq     'EEX'
067000050210     C                   MOVEL     '20'          COD
067100050210     C                   MOVEL     *blank        KEY
067200050210     C                   MOVEL     gcpSCm        KEY
067300050210     c     ktab          chain     tabel00f
067400050210     c                   if        %found(tabel00f)
067500050210     c                   movel     tbluni        ds20
067600050210     C                   Z-ADD     §20GRE        GIOR
067700050210     c                   endif
067800050210     c                   END
067900941013     C*
068000941013     C                   ENDSR
068100930802     C*-----------
068200930802     C* CONVERTO IN GIORNI LE DUE DATE DA CONFRONTARE
068300930802     C*-----------
068400930802     C     CONV          BEGSR
068500930802     C                   Z-ADD     DATA1         G02INV
068600930802     C                   MOVEL     '3'           G02ERR
068700941013     C                   CALL      'XSRDA8'
068800941013     C                   PARM                    WLBDA8
068900930802     C                   Z-ADD     G02TGI        GIO1              5 0
069000930802     C                   Z-ADD     DATA2         G02INV
069100930802     C                   MOVEL     '3'           G02ERR
069200941013     C                   CALL      'XSRDA8'
069300941013     C                   PARM                    WLBDA8
069400930802     C                   Z-ADD     G02TGI        GIO2              5 0
069500930802     C                   ENDSR
069600930802     C*-----------
069700930802     C* CALCOLO GIORNI FESTIVI INTERCORRENTE TRA LE DUE DATE IN ESAME
069800930802     C*-----------
069900930802     C     FEST          BEGSR
070000930802     C                   Z-ADD     DATA1         DATADA
070100930802     C                   Z-ADD     DATA2         DATAA
070200990906     C                   CALL      'XGGFEST'
070300941013     C                   PARM                    WDAT8
070400930802     C                   ENDSR
070500930803     C*-----------
070600930803     C* SUBROUTINE INIZIALE
070700930803     C*-----------
070800930803     C     *INZSR        BEGSR
070900930803     C     *ENTRY        PLIST
071000930803     C                   PARM                    KPJBA
071100930806     C                   MOVEL     KPJBU         PARAM
071200930804     C                   Z-ADD     1             CODUT             1 0
071300950116     C*---------------------------------------------------------------*
071400950116     C                   CALL      'X§PARUT'
071500950116     C                   PARM                    UT§DSE
071600950116     C*--- RICERCA CAPOCONTI------------------------------------------*
071700950116     C                   DO        50            X
071800950116     C                   MOVE      TCU(X)        TCUDS
071900950116     C     F56           IFEQ      'CG'
072000950116     C     F4            ANDEQ     '1'
072100950116     C     F3            ANDEQ     '0'
072200950116     C                   Z-ADD     KCU(X)        KCI               4 0
072300950116     C                   END
072400950116     C                   END
072500950116     C*---------------------------------------------------------------*
072600930803     C** CHIAVE DI TABEL
072700930803     C     KTAB          KLIST
072800930803     C                   KFLD                    CODUT
072900930803     C                   KFLD                    COD               2
073000930803     C                   KFLD                    KEY               8
073100950113     C*
073200950113     C     KTAB1         KLIST
073300950113     C                   KFLD                    CODUT
073400950113     C                   KFLD                    COD               2
073500930804     C** CHIAVE DI FLEVB
073600930804     C     KEVB          KLIST
073700990803     C                   KFLD                    KAAS
073800941013     C                   KFLD                    KLNP
073900941013     C                   KFLD                    KNRS
074000941013     C                   KFLD                    KNSP
074100941013     C                   KFLD                    KDEV
074200931124     C** CHIAVE DI FLEVB
074300931124     C     KEVBS         KLIST
074400990803     C                   KFLD                    KAAS
074500941013     C                   KFLD                    KLNP
074600941013     C                   KFLD                    KNRS
074700941013     C                   KFLD                    KNSP
074800010921     C** CHIAVE DI FLFG765L
074900930810     C     KFG7          KLIST
075000010921     C                   KFLD                    KAA4
075100941013     C                   KFLD                    KPER
075200020829     C                   KFLD                    linea             3 0
075300941013     C** CHIAVE DI FNBLP/FNARB
075400941013     C     KSPE          KLIST
075500941013     C                   KFLD                    KAAS
075600941013     C                   KFLD                    KLNP
075700941013     C                   KFLD                    KNRS
075800941013     C                   KFLD                    KNSP
075900941013     C** CHIAVI DI TNTAM/TNTGC
076000941013     C     KTGC          KLIST
076100941013     C                   KFLD                    KKSC
076200941013     C                   KFLD                    KCTR
076300941013     C                   KFLD                    KPRG
076400941013     C*----------------------------------------------------*
076500941013     C*  Definizione variabili
076600941013     C     *LIKE         DEFINE    EVBDEV        KDEV
076700010921     C     *LIKE         DEFINE    FG7AAS        KAA4
076800941013     C     *LIKE         DEFINE    FG7PER        KPER
076900941013     C     *LIKE         DEFINE    GCPAAS        KAAS
077000941013     C     *LIKE         DEFINE    GCPLNP        KLNP
077100941013     C     *LIKE         DEFINE    GCPNRS        KNRS
077200941013     C     *LIKE         DEFINE    GCPNSP        KNSP
077300941013     C     *LIKE         DEFINE    GCPSCM        KKSC
077400950113     C     *LIKE         DEFINE    TGCCTR        KCTR
077500950113     C     *LIKE         DEFINE    TGCPRG        KPRG
077600941013     C*----------------------------------------------------*
077700941013     C* DATA ELABORAZ. IN AAMMGG
077800941013     C                   TIME                    WHHDAT           14 0
077900941013     C                   MOVE      WHHDAT        G02DAT
078000941013     C                   MOVE      *BLANKS       G02ERR
078100941013     C                   CALL      'XSRDA8'
078200941013     C                   PARM                    WLBDA8
078300941013     C                   Z-ADD     G02INV        DATELA            8 0
078400941013     C*----------------------------------------------------*
078500020808     C* Caricamento £1
078600020808     c                   clear                   trul06ds
078700020809     c                   movel     kpjbu         savkpjbu
078800020808     c                   eval      d06cod = 'PP'
078900050301     c                   movel     fil           d06key
079000020808     c                   movel     trul06ds      kpjbu
079100020808     c                   call      'TRUL06R'
079200020808     c                   parm                    kpjba
079300020808     c                   movel     kpjbu         trul06ds
079400020809     c                   movel     savkpjbu      kpjbu
079500941013     C** CARICO TABELLA '2G'
079600941013     C                   MOVEL     '2G'          COD
079700941013     C                   MOVEL     *BLANKS       KEY
079800941013     C                   MOVEL     1             KEY
079900941013     C     KTAB          CHAIN     TABEL                              30
080000941013     C  N30              MOVEL     TBLUNI        DS2G
080100941013     C   30              CLEAR                   DS2G
080200950113     C* Carico DATI TABELLA PARTICOLARITA'
080300950113     C                   Z-ADD     0             §F                3 0
080400950113     C                   MOVEL     '7Q'          COD
080500950113     C                   MOVEL     *BLANK        KEY
080600950113     C     KTAB1         CHAIN     TABEL                              21
080700950113     C     *IN21         DOWEQ     '0'
080800950113     C     §F            ANDLE     100
080900950113     C     TBLFLG        IFEQ      *BLANK
081000950113     C                   ADD       1             §F
081100950113     C                   MOVEL     TBLUNI        D7Q(§F)
081200950113     C                   MOVEL     TBLKEY        C7Q(§F)
081300950113     C                   END
081400950113     C     KTAB1         READE     TABEL                                  21
081500950113     C                   END
081600050225     c*
081700010921     C                   Z-ADD     ANNO          KAA4
081800941013     C                   Z-ADD     PER           KPER
081900941013     C** VERIFICO SE IL PERIODO PASSATO E' GIA' STATO ELABORATO
082000020829     c                   do        30            ff
082100020829     c                   if        lin(ff) = 0
082200020829     c                   leave
082300020829     c                   end
082400020829     c                   z-add     lin(ff)       linea
082500010921     C     KFG7          SETLL     FLFG765L                               80
082600941013     C** SE E' GIA' ELABORATO CANCELLO L'ELABORAZIONE PRECEDENTE
082700941013     C     *IN80         IFEQ      *ON
082800941013     C                   SETOFF                                       18
082900941013     C     *IN18         DOWEQ     *OFF
083000010921     C     KFG7          READE     FLFG765L                               18
083100010921     C  N18              DELETE    FLFG765
083200941013     C                   ENDDO
083300941013     C                   ENDIF
083400020829     C                   enddo
083500941013     C*----------------------------------------------------*
083600941013     C                   Z-ADD     0             H
083700941013     C                   CLEAR                   SKI
083800941013     C                   Z-ADD     ANNO          FG7AAS
083900941013     C                   Z-ADD     PER           FG7PER
084000941013     C                   Z-ADD     0             FG7DTR
084100941013     C                   MOVEL     *BLANKS       FG7FTR
084200941013     C*----------------------------------------------------*
084300930803     C                   ENDSR

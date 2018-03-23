000100930804     H DECEDIT('0,') DATEDIT(*DMY.)
000200941013     H* FNLG46R *----------------------------------------------------*
000300930804     H*                                                              *
000400930806     H*  - STATISTICA GIACENZE IN PARTENZA - CONTROLLO UFF. GIAC. -  *
000500930804     H*                *- CREAZIONE STAMPA -*                        *
000600930804     H*--------------------------------------------------------------*
000700010921     H*  11 = LETTURA FLFG765L                                       *
000800940224     H*  20 = ELABORO TUTTE LE FILIALI                               *
000900940224     H*  30 = CHAIN SU TABELLA 2G/CHAIN SU AZORG01L                  *
001000940224     H*  50 = CHAIN SU AZORG01L                                      *
001100940224     H*  90 = PRIMA VOLTA                                            *
001200940224     H*--------------------------------------------------------------*
001300010921     FFLFG765L  IF   E           K DISK
001400930804     FTABEL00F  IF   E           K DISK
001500930804     FAZORG01L  IF   E           K DISK
001600930806     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
001700930804     F* SCHIERE VARIE *--------------------------------------------------------*
001800931125     D SKI             S             66    DIM(24) CTDATA PERRCD(1)
001900931221     F* SCHIERE X CARICAMENTO ORGANIGRAMMA ------------------------------------*
002000971118     D CDF             S             10    DIM(500)
002100971118     D DES             S             25    DIM(500)
002200930803     F* DS *-------------------------------------------------------------------*
002300930203     D DS2G          E DS
002400941013     D FNLG44        E DS                  EXTNAME(FNLG44DS)
002500900418     D KPJBA         E DS
002600930803     D*-------------
002700930803     D* DS PER ROUTINE 'XSRDAT'
002800930803     D*-------------
002900941013     D WLBDA8          DS
003000941013     D  G02DAT                 1      8  0
003100941013     D  G02INV                 9     16  0
003200941013     D  G02ERR                17     17
003300941013     D  G02TGI                18     22  0
003400930812     D*------------------
003500930812     D* DS RICERCA RAG. SOCIALE
003600930812     D*------------------
003700930812     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003800930812     D CNCR80        E DS
003900020823     d trul06ds      e ds
004000020823     d  lin                    1     90  0 dim(30)
004100931221     D*------------------
004200931221     D* DS PER SCOMPOSIZIONE CODICI DIVISIONE/AREA/FILIALE
004300931221     D*------------------
004400931221     D                 DS
004500931221     D  DIVIS                  1      1
004600931221     D  AREA                   2      4  0
004700931221     D  LINEA                  5      7  0
004800931221     D  XX                     8     10  0
004900931221     D  CODICE                 1     10  0
005000900423     C*---------------------------------------------------------------*
005100931221     C*  CICLO PRINCIPALE
005200931221     C*---------------------------------------------------------------*
005300931221     C*  SE SONO IN SEDE E HO CHIESTO TUTTE LE FILIALI LE ORDINO SECONDO
005400931221     C*  £3
005500941013     C     D44FIL        IFEQ      0
005600931221     C     SIMFEL        ANDEQ     0
005700931221     C                   Z-ADD     0             YY                3 0
005800931221     C                   MOVEL     CDF(1)        SAVDIV            1
005900931221     C                   EXSR      CNTFIL
006000931221     C                   ELSE
006100010921     C  N20KFG7          CHAIN     FLFG765L                           11
006200010921     C   20KFG71         CHAIN     FLFG765L                           11
006300931221     C   11              MOVEL     'S'           £FINE
006400931221     C                   END
006500931221     C*---------------------------------------------------------------*
006600931221     C*   LOOP DI STAMPA DATI FILIALI
006700931221     C     £FINE         DOWNE     'S'
006800931221     C                   EXSR      REDFIL
006900931221     C*---------------------------------------------------------------*
007000931221     C*   CONTROLLO ESISTENZA PROSSIMA FILIALE
007100931221     C     *IN11         IFEQ      '1'
007200931221     C     FILSAV        IFEQ      0
007300931221     C     SIMFEL        ANDEQ     0
007400931221     C                   EXSR      CNTFIL
007500931221     C                   ELSE
007600931221     C                   MOVEL     'S'           £FINE
007700931221     C                   END
007800931221     C                   END
007900931221     C*---------------------------------------------------------------*
008000931221     C                   END
008100931221     C*---------------------------------------------------------------*
008200931221     C                   SETON                                        LR
008300931221     C*---------------------------------------------------------------*
008400931221     C*  CNTFIL: CONTROLLO ESISTENZA FILIALE
008500931221     C*---------------------------------------------------------------*
008600931221     C     CNTFIL        BEGSR
008700931221     C*  CONTROLLO SE SONO A FINE FILIALI CARICATE
008800931221     C     YY            IFGE      §C
008900931221     C                   MOVEL     'S'           £FINE
009000931221     C                   ELSE
009100931221     C                   ADD       1             YY
009200931221     C                   MOVE      CDF(YY)       CODICE
009300941013     C                   MOVE      LINEA         D44FIL
009400931221     C                   MOVE      DES(XX)       FILIA
009500010921     C     KFG7          CHAIN     FLFG765L                           11
009600931221     C     SAVDIV        IFNE      DIVIS
009700931221     C                   SETON                                        OF
009800931221     C                   MOVEL     DIVIS         SAVDIV
009900931221     C                   END
010000931221     C                   END
010100931221     C*---------------------------------------------------------------*
010200931221     C                   ENDSR
010300931221     C*---------------------------------------------------------------*
010400931221     C*  REDFIL: LETTURA FILIALE
010500931221     C*---------------------------------------------------------------*
010600931221     C     REDFIL        BEGSR
010700931221     C*---------------------------------------------------------------*
010800930805     C     *IN11         DOWEQ     *OFF
010900020823      *solo filiali dell'area se non sono in sede
011000020823     c     fg7fil        lookup    lin                                    66
011100020823     c                   if        simfel <> 0 and not *in66
011200020823     c                   goto      bypass
011300020823     c                   end
011400931221     C*  NUOVA FILIALE
011500931221     C     SIMFEL        IFNE      0
011600931221     C     FILSAV        ORNE      0
011700931221     C     FG7FIL        ANDNE     SAVFIL
011800930805     C     FG7FIL        CHAIN     AZORG01L                           50
011900930805     C  N50              MOVEL     ORGDES        FILIA            25
012000930805     C   50              MOVEL     *ALL'?'       FILIA
012100930806     C                   Z-ADD     FG7FIL        SAVFIL            3 0
012200930805     C                   ENDIF
012300931221     C*
012400930809     C                   EXSR      PULI
012500930805     C                   EXSR      CALC
012600930805     C  N90
012700930805     COR OF              EXCEPT    TESTA
012800930806     C  N90              SETON                                        90
012900930805     C                   EXCEPT    RIGA
013000930212     C*
013100020823     c     bypass        tag
013200931221     C  N20KFG7          READE     FLFG76                                 11
013300931221     C   20KFG71         READE     FLFG76                                 11
013400930805     C                   ENDDO
013500930806     C*------------------------------------------------------------------*
013600931221     C                   ENDSR
013700931221     C*------------------------------------------------------------------*
013800930805     C     CALC          BEGSR
013900930806     C* NR. GIACENZE APERTE IN PARTENZA
014000930809     C     FG7GDA        IFGT      0
014100010921     C                   Z-ADD     FG7GDA        GIAAP             5 0
014200930809     C                   ENDIF
014300930805     C* % GIAC. APERTE IN RITARDO
014400930809     C     FG7GAR        IFGT      0
014500010921     C     FG7GAR        MULT      100           GAR               7 0
014600010921     C     GAR           DIV       GIAAP         PGAR              5 1
014700930805     C* MEDIA GIORNI RITARDO APERTURA
014800010921     C     FG7GRA        DIV       FG7GAR        MGRA              4 1
014900930809     C                   ENDIF
015000930805     C*----------------------------------------------------*
015100930806     C* NR. DISPOSIZIONI IMMESSE IN PARTENZA
015200930809     C     FG7DDI        IFGT      0
015300010921     C                   Z-ADD     FG7DDI        DISIM             5 0
015400930809     C                   ENDIF
015500930805     C* % DISP. IMMESSE IN RITARDO
015600930809     C     FG7DRO        IFGT      0
015700010921     C     FG7DRO        MULT      100           DRO               7 0
015800010921     C     DRO           DIV       DISIM         PDRO              5 1
015900930805     C* MEDIA GIORNI RITARDO IMMISSIONE DISPOSIZ.
016000010921     C     FG7GRD        DIV       FG7DRO        MGRD              4 1
016100930809     C                   ENDIF
016200930805     C*----------------------------------------------------*
016300930806     C* NR. GIACENZE CESSATE IN PARTENZA
016400930809     C     FG7GDC        IFGT      0
016500010921     C     FG7GDC        SUB       FG7GNC        GIACH             5 0
016600010921     C                   Z-ADD     FG7GDC        GIACE             5 0
016700930809     C                   ENDIF
016800930805     C* % GIAC. CHIUSE IN RITARDO
016900930809     C     FG7GCO        IFGT      0
017000010921     C     FG7GCO        MULT      100           GCO               7 0
017100010921     C     GCO           DIV       GIACE         PGCO              5 1
017200930806     C* MEDIA GIORNI RITARDO CESSAZIONE GIAC.
017300010921     C     FG7GRC        DIV       FG7GCO        MGRC              4 1
017400930809     C                   ENDIF
017500930806     C* % GIAC. CESSATE SENZA SPESE
017600930809     C     FG7GSS        IFGT      0
017700931126     C     GIACH         ANDGT     0
017800010921     C     FG7GSS        MULT      100           GSS               7 0
017900010921     C     GSS           DIV       GIACH         PGSS              5 1
018000930809     C                   ENDIF
018100930805     C*----------------------------------------------------*
018200930806     C* % GIAC. APERTE IN RITARDO IN ARRIVO
018300930809     C     FG7AAR        IFGT      0
018400010921     C     FG7AAR        MULT      100           AAR               7 0
018500010921     C     AAR           DIV       FG7GAP        PAAR              5 1
018600930809     C* MEDIA GIORNI RITARDO APERTURA GIAC.
018700010921     C     FG7AGR        DIV       FG7AAR        MAGR              4 1
018800930809     C                   ENDIF
018900930806     C*----------------------------------------------------*
019000930806     C* % DISPOSIZIONI ESEGUITE IN RITARDO IN ARRIVO
019100930809     C     FG7DER        IFGT      0
019200010921     C     FG7DER        MULT      100           DER               7 0
019300010921     C     DER           DIV       FG7DDE        PDER              5 1
019400930806     C* MEDIA GIORNI RITARDO ESECUZIONE DISP.
019500010921     C     FG7GRE        DIV       FG7DER        MGRE              4 1
019600930809     C                   ENDIF
019700930806     C*----------------------------------------------------*
019800010921     C                   Z-ADD     FG7GDA        PGDA              4 0
019900010921     C                   Z-ADD     FG7GNA        PGNA              4 0
020000010921     C                   Z-ADD     FG7DDI        PDDI              4 0
020100010921     C                   Z-ADD     FG7DNI        PDNI              4 0
020200010921     C                   Z-ADD     FG7GDC        PGDC              4 0
020300010921     C                   Z-ADD     FG7GNC        PGNC              4 0
020400010921     C                   Z-ADD     FG7GAP        PGAP              4 0
020500010921     C                   Z-ADD     FG7DDE        PDDE              4 0
020600010921     C*----------------------------------------------------*
020700930806     C                   ENDSR
020800930809     C*-----------
020900930809     C* PULIZIA CAMPI DI STAMPA E D'APPOGGIO
021000930809     C*-----------
021100930809     C     PULI          BEGSR
021200930809     C*
021300930809     C                   Z-ADD     0             GIAAP
021400930809     C                   Z-ADD     0             GAR
021500930809     C                   Z-ADD     0             PGAR
021600930809     C                   Z-ADD     0             MGRA
021700930809     C                   Z-ADD     0             DISIM
021800930809     C                   Z-ADD     0             DRO
021900930809     C                   Z-ADD     0             PDRO
022000930809     C                   Z-ADD     0             MGRD
022100930809     C                   Z-ADD     0             GIACE
022200931126     C                   Z-ADD     0             GIACH
022300930809     C                   Z-ADD     0             GCO
022400930809     C                   Z-ADD     0             PGCO
022500930809     C                   Z-ADD     0             MGRC
022600930809     C                   Z-ADD     0             GSS
022700930809     C                   Z-ADD     0             PGSS
022800930809     C                   Z-ADD     0             AAR
022900930809     C                   Z-ADD     0             PAAR
023000930809     C                   Z-ADD     0             MAGR
023100930809     C                   Z-ADD     0             DER
023200930809     C                   Z-ADD     0             PDER
023300930809     C                   Z-ADD     0             MGRE
023400930809     C*
023500930809     C                   ENDSR
023600931221     C*--------------------------------------------------------*
023700931221     C* SUBROUTINE INIZIALE                                    *
023800931221     C*--------------------------------------------------------*
023900930803     C     *INZSR        BEGSR
024000931221     C*
024100930803     C     *ENTRY        PLIST
024200930803     C                   PARM                    KPJBA
024300941013     C                   MOVEL     KPJBU         FNLG44
024400930804     C                   Z-ADD     1             CODUT             1 0
024500930809     C                   TIME                    ORA               6 0
024600010921     C                   MOVE      D44AA         AA4               4 0
024700961001     C* LA DATA DEL GIORNO LA PRENDO DA TIME
024800961001     C                   TIME                    W0140            14 0
024900961001     C                   MOVE      W0140         UDATE8            8 0
025000931221     C*--------------------------------------------------------*
025100930803     C** CHIAVE DI TABEL
025200930803     C     KTAB          KLIST
025300930803     C                   KFLD                    CODUT
025400930803     C                   KFLD                    COD               2
025500930803     C                   KFLD                    KEY               8
025600930806     C** CHIAVE X PER FLFG76
025700930806     C     KFG7          KLIST
025800010921     C                   KFLD                    AA4
025900941013     C                   KFLD                    D44PER
026000941013     C                   KFLD                    D44FIL
026100930806     C     KFG71         KLIST
026200010921     C                   KFLD                    AA4
026300941013     C                   KFLD                    D44PER
026400931221     C*--------------------------------------------------------*
026500931221     C                   CALL      'X§PARUT'
026600931221     C                   PARM                    UT§DSE
026700931221     C                   MOVEL     REC80         CNCR80
026800931221     C                   MOVEL     RAGUT         RSUT             20
026900931221     C*--------------------------------------------------------*
027000930803     C** CARICO TABELLA '2G'
027100930803     C                   MOVEL     '2G'          COD
027200930803     C                   MOVEL     *BLANKS       KEY
027300930803     C                   MOVEL     1             KEY
027400930803     C     KTAB          CHAIN     TABEL                              30
027500930803     C  N30              MOVEL     TBLUNI        DS2G
027600930803     C   30              CLEAR                   DS2G
027700931221     C*--------------------------------------------------------*
027800931221     C* TESTO SE RICHIESTA UN FILIALE SPECIFICA
027900941013     C     D44FIL        COMP      *ZEROS                                 20
028000941013     C                   MOVEL     D44FIL        FILSAV            3 0
028100931221     C*--------------------------------------------------------*
028200931221     C** CARICO DATI ORGANIGRAMMA IN SCHIERA
028300931221     C     SIMFEL        IFEQ      0
028400941013     C     D44FIL        ANDEQ     0
028500931221     C                   SETOFF                                       20
028600931221     C                   Z-ADD     0             XX
028700931221     C     *LOVAL        SETLL     AZORG01L
028800931221     C                   READ      AZORG01L                               30
028900931221     C                   MOVEA     *HIVAL        CDF
029000931221     C     *IN30         DOWEQ     '0'
029100971118     C     XX            ANDLT     500
029200931221     C     ORGFAG        IFEQ      'F'
029300931221     C     ORGFAG        OREQ      'A'
029400931221     C                   ADD       1             XX
029500931221     C                   MOVE      ORGFL3        DIVIS
029600931221     C                   MOVE      ORGCAR        AREA
029700931221     C                   MOVE      ORGFIL        LINEA
029800931221     C                   MOVE      CODICE        CDF(XX)
029900931221     C                   MOVE      ORGDES        DES(XX)
030000931221     C                   END
030100931221     C                   READ      AZORG01L                               30
030200931221     C                   END
030300931221     C*
030400931221     C                   SORTA     CDF
030500931221     C                   Z-ADD     XX            §C                3 0
030600931221     C                   END
030700020823      * carico £1 se sono in filiale devo leggere solo le linee dell'area
030800020823     c                   clear                   trul06ds
030900020823     c                   eval      d06cod = '£1'
031000020823     c                   movel     simfel        d06key
031100020823     c                   movel     trul06ds      kpjbu
031200020823     c                   call      'TRUL06R'
031300020823     c                   parm                    kpjba
031400020823     c                   movel     kpjbu         trul06ds
031500931221     C*--------------------------------------------------------*
031600930809     C* GIRO LE DATE DEL PERIODO RICHIESTO X LA STAMPA
031700941013     C                   Z-ADD     D44DT1        G02INV
031800930809     C                   MOVEL     '3'           G02ERR
031900941013     C                   CALL      'XSRDA8'
032000941013     C                   PARM                    WLBDA8
032100941013     C                   Z-ADD     G02DAT        PDAL              8 0
032200930809     C*
032300941013     C                   Z-ADD     D44DT2        G02INV
032400930809     C                   MOVEL     '3'           G02ERR
032500941013     C                   CALL      'XSRDA8'
032600941013     C                   PARM                    WLBDA8
032700941013     C                   Z-ADD     G02DAT        PAL               8 0
032800931221     C*--------------------------------------------------------*
032900931221     C                   MOVEL     *ALL'-'       TRAT            198
033000931221     C                   MOVEL     ' '           £FINE             1
033100931221     C*--------------------------------------------------------*
033200930803     C                   ENDSR
033300930806     C*------------------------------------------------------------*
033400930806     OPRTF198   E            TESTA            02
033500930812     O                       RSUT                20
033600930809     O                                          150 '* FLFG77R *'
033700961001     O                       UDATE8             165 '  /  /    '
033800930809     O                       ORA                175 '  :  :  '
033900930809     O                                          190 'Pagina'
034000930806     O                       PAGE          Z   +  1
034100930806     O          E            TESTA            04
034200930806     O                       SKI(1)              66
034300930806     O                       SKI(2)             132
034400930806     O                       SKI(3)             198
034500930806     O          E            TESTA            05
034600930806     O                       SKI(4)              66
034700941013     O                       D44PER              18
034800930809     O                                         +  0 '/'
034900941013     O                       D44AA             +  0
035000930806     O                       SKI(5)             132
035100930806     O                       SKI(6)             198
035200941013     O                       PDAL               162 '  /  /    '
035300941013     O                       PAL                177 '  /  /    '
035400930806     O          E            TESTA            06
035500930806     O                       SKI(1)              66
035600930806     O                       SKI(2)             132
035700930806     O                       SKI(3)             198
035800930806     O          E            TESTA            08
035900930806     O                       SKI(7)              66
036000930809     O                       SKI(18)            132
036100930809     O                       SKI(8)             198
036200930806     O          E            TESTA            10
036300930806     O                       SKI(9)              66
036400930806     O                       SKI(12)            132
036500930806     O                       SKI(15)            198
036600930806     O          E            TESTA            11
036700930806     O                       SKI(10)             66
036800930806     O                       SKI(13)            132
036900930806     O                       SKI(16)            198
037000930806     O          E            TESTA            12
037100930806     O                       SKI(11)             66
037200930806     O                       §2GGRP              46
037300930806     O                       SKI(14)            132
037400930806     O                       §2GGRD              80
037500930806     O                       §2GGRC             115
037600930806     O                       SKI(17)            198
037700930806     O                       §2GRAA             160
037800930806     O                       §2GREA             187
037900931125     O          E            TESTA            13
038000931125     O                       SKI(22)             66
038100931125     O                       SKI(23)            132
038200931125     O                       SKI(24)            198
038300931125     O          E            TESTA            14
038400930809     O                       TRAT               198
038500931125     O          E            TESTA            15
038600931125     O                       SKI(19)             66
038700931125     O                       SKI(20)            132
038800931125     O                       SKI(21)            198
038900930809     O          E            RIGA        1
039000930809     O                       SKI(19)             66
039100930809     O                       SKI(20)            132
039200930809     O                       SKI(21)            198
039300930809     O          E            RIGA        0
039400930806     O                       FILIA               25
039500010921     O                       PGDA          2     32
039600931125     O                       PGAR          2     41
039700930809     O                       MGRA          2     53
039800010921     O                       PGNA          2     61
039900010921     O                       PDDI          2     68
040000930809     O                       PDRO          2     76
040100930809     O                       MGRD          2     87
040200010921     O                       PDNI          2     96
040300010921     O                       PGDC          2    103
040400930809     O                       PGCO          2    111
040500930809     O                       MGRC          2    122
040600010921     O                       PGNC          2    131
040700930809     O                       PGSS          2    140
040800010921     O                       PGAP          2    148
040900930809     O                       PAAR          2    156
041000930809     O                       MAGR          2    167
041100010921     O                       PDDE          2    175
041200930809     O                       PDER          2    183
041300930809     O                       MGRE          2    194
041400931125     O          E            RIGA        1
041500931125     O                       SKI(19)             66
041600931125     O                       SKI(20)            132
041700931125     O                       SKI(21)            198
041800931217**
041900930804***---------------------------------------------------------------  01
042000930804------------------------------------------------------------------  02
042100930804---------------------------------------------------------------***  03
042200930809***    PERIODO:                                                     04
042300931119   S  T  A  T  I  S  T  I  C  A          G  I  A  C  E  N  Z  E     05
042400941013                DAL             AL                             ***  06
042500930809***                                         P    A    R    T    E   07
042600930809                        A    R    R    I    V    O             ***  08
042700930809                           |        A   P   E   R   T   E      |    09
042800931125                           |    | A P E R T E |MEDIA GG.| N O N|    10
042900961001PUNTO OPERATIVO            |N.RO| OLTRE GG.  X| DI RIT. |APERTE|N.  11
043000931126  DISPOSIZIONI SCADUTI TERMINI  |       C    E    S    S    A    T  12
043100931125  |  IMMESSE  |MEDIA GG.|N  O  N|    |  CESSATE  |MEDIA GG.| N O N  13
043200930806RO|OLTRE GG. X| DI RIT. |IMMESSE|N.RO|OLTRE GG.  | DI RIT. |CESSAT  14
043300930809   E       |    A  P  E  R  T  E      |  E  S  E  G  U  I  T  E  |  15
043400931125 | CESSATE |    |  APERTE   |MEDIA GG.|    | ESEGUITE  |MEDIA GG.|  16
043500930806E| NO SPESE|N.RO|OLTRE GG.  | DI RIT. |N.RO|OLTRE GG.  | DI RIT. |  17
043600930809   N    Z    A                                                      18
043700931125                           |    |             |         |      |    19
043800931125  |           |         |       |    |           |         |        20
043900930809 |         |    |           |         |    |           |         |  21
044000931125                           |    |      %      |         |      |    22
044100931125  |     %     |         |       |    |     %     |         |        23
044200931217 |    %    |    |     %     |         |    |     %     |         |  24

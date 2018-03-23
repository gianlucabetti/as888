000100930812     H DECEDIT('0,') DATEDIT(*DMY/)
000200930812     H*---------*
000300930812     H* TRST40R *-----------------------------------------------------*
000400930812     H*---------*                                                     *
000500930812     H*              - STATISTICA CONTROLLO UFF. GIACENZE             *
000600930812     F*---------------------------------------------------------------*
000700930812     F* DATA BASE                                                     *
000800930812     F*---------------------------------------------------------------*
000900010921     FFLFG765L  IF   E           K DISK
001000941013     FFNLG44D   CF   E             WORKSTN
001100930812     D*---------------------------------------------------------------*
001200930812     D* SCHIERA
001300930812     D*---------------------------------------------------------------*
001400930812     D IN              S              1    DIM(99)
001500020430     D L3              S              3  0 DIM(30)
001600930812     D*---------------------------------------------------------------*
001700930812     D* DS
001800930812     D*---------------------------------------------------------------*
001900141217     D DATECNV0F     E DS
002000930812     D KPJBA         E DS
002100930812     D  LIBSYS                92    100
002200930812     D*------------------
002300930812     D* DS LANCIO PGM BATCH
002400930812     D*------------------
002500941013     D FNLG44        E DS                  EXTNAME(FNLG44DS)
002600941013     D FNLG45        E DS                  EXTNAME(FNLG45DS)
002700020430     d trul06ds      e ds
002800020430     d  lin                    1     90  0 dim(30)
002900930812     D                 DS
003000941013     D  AA2                    1      2  0
003100931125     D  PERIO                  3      4  0
003200931125     D  DSPER                  1      4  0
003300141217     D*------------------
003400141217     D da_lunedi       S               D
003500141217     D a_domenica      S               D
003600141217     d DataISO         s               d   datfmt(*iso)
003700141217     d DataEUR         s               d   datfmt(*eur)
003800930812     D*------------------
003900930812     D* DS PER CONVERSIONE DATE
004000930812     D*------------------
004100941013     D WLBDA8          DS
004200941013     D  G02DAT                 1      8  0
004300941013     D  G02INV                 9     16  0
004400941013     D  G02ERR                17     17
004500941013     D  G02TGI                18     22  0
004600930812     D*------------------
004700930812     D* DS RICERCA RAG. SOCIALE
004800930812     D*------------------
004900930812     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005000930812     D CNCR80        E DS
005100930812     D*------------------
005200930812     D* DS PERIODO DI ELABORAZIONE
005300930812     D*------------------
005400930812     D DS2I          E DS
005500941013     C*------------------------------------------------------------------
005600930812     C** IMPOSTAZIONE CAMPI VIDEO
005700941013     C                   TIME                    WHHDAT           14 0
005800941013     C                   MOVE      WHHDAT        VIDAAS
005900931125     C                   MOVE      ' ?'          VIDPER
006000931117     C                   MOVEL     '999'         VIDFIL
006100930812     C*
006200930812     C     *INKC         DOWEQ     *OFF
006300930812     C*
006400930812     C                   MOVEA     *IN           IN
006500930812     C                   MOVEA     *ZEROS        *IN
006600941013     C                   WRITE     LG44D01
006700930812     C                   MOVEA     IN            *IN
006800930812     C*
006900941013     C                   EXFMT     LG44D01
007000930812     C*
007100930812     C  NKC              DO
007200930812     C** CONTROLLO DATI FORMATO VIDEO
007300930812     C                   EXSR      CONTR1
007400930812     C** SE NON CI SONO ERRORI ED HANNO PREMUTO F1 SOTTOMETTO LA STAMPA
007500931221     C  N90*INKF         IFEQ      *ON
007600941013     C                   MOVE      VIDAAS        D44AA
007700941013     C                   MOVE      VIDPER        D44PER
007800941013     C                   MOVE      VIDFIL        D44FIL
007900930812     C     VIDFIL        IFEQ      '999'
008000941013     C                   MOVE      *ZEROS        D44FIL
008100930812     C                   ENDIF
008200941013     C                   Z-ADD     DATDAL        D44DT1
008300941013     C                   Z-ADD     DATAL         D44DT2
008400930812     C*
008500941013     C                   MOVEL     FNLG44        KPJBU
008600941013     C                   MOVEL     'LG46'        KCOAZ
008700930812     C                   CALL      'BCH10'
008800930812     C                   PARM                    KPJBA
008900930812     C                   ENDIF
009000930702     C*
009100930812     C                   ENDDO
009200930702     C*
009300930812     C                   ENDDO
009400930812     C*
009500930812     C                   SETON                                        LR
009600930812     C*========================================================================*
009700930812     C*-----------
009800930812     C* ROUTINE DI CONTROLLO DATI FORMATO VIDEO
009900930812     C*-----------
010000930812     C     CONTR1        BEGSR
010100930812     C*
010200930812     C                   SETOFF                                       902122
010300930812     C                   SETOFF                                       2310
010400931125     C*
010500931125     C* RICERCA FILIALI SE IMMESSO PUNTO INTERROGATIVO
010600931125     C     '?'           SCAN      VIDPER                                 21
010700931125     C     *IN21         IFEQ      *ON
010800941013     C                   MOVE      *ZERO         D45PAA
010900941013     C                   MOVE      *ZERO         D45PRD
011000941013     C                   MOVEL     FNLG45        KPJBU
011100941013     C                   CALL      'FNLG45R'
011200931125     C                   PARM                    KPJBA
011300941013     C                   MOVEL     KPJBU         FNLG45
011400931125     C*
011500931125     C*  CONTROLLO CHE SIA STATA FATTA UNA SELEZIONE
011600941013     C     D45CMD        IFNE      '3'
011700941013     C                   MOVE      D45PAA        VIDAAS
011800941013     C                   MOVE      D45PRD        VIDPER
011900931125     C                   END
012000931125     C                   END
012100931125     C     VIDPER        COMP      '000'                              22  22
012200931125     C  N22              SETON                                        4490
012300930812     C** ANNO OBBLIGATORIO
012400931125     C     *IN90         IFEQ      '0'
012500931117     C     VIDAAS        IFEQ      *ZEROS
012600930812     C                   SETON                                        4090
012700930812     C                   ENDIF
012800930812     C** CONTROLLO SE PERIODO RICHIESTO E' GIA' STATO ELABORATO
012900941013     C                   MOVE      VIDAAS        AA2
013000010921     C                   MOVE      VIDAAS        KAA4
013100931125     C                   MOVE      VIDPER        PERIO
013200010921     C     KFG7          SETLL     FLFG765L                               15
013300930812     C  N15              SETON                                        5090
013400931117     C     *IN15         IFEQ      *ON
013500141217      ***
013600141217      ***  Data una settimana dell'Anno
013700141217      ***    ricava il primo e ultimo giorno
013800141217      ***       AAAAWW --> lunedì e domenica da impostare a video
013900141217      ***
014000141217
014100141217     C/EXEC SQL
014200141217     C+ SELECT DISTINCT
014300141217     C+   DC_WEEK_STARTING_DATE, DC_WEEK_ENDING_DATE
014400141217     C+ INTO         :da_Lunedi, :a_domenica
014500141217     C+ FROM DATECNV0F
014600141217     C+ WHERE DC_WOY_ISO= :PERIO
014700141217     C+              and
014800180109     C+       DC_YOY_ISO= :KAA4
014900141217     C/END-EXEC
015000141217     c                   if        sqlCOD = 0
015100141217     C                   SETON                                        10
015200141217     c                   move      da_lunedi     DATdal            8 0
015300141217     c                   move      da_lunedi     dataiso
015400141217     c                   move      dataiso       dataeur
015500141217     c                   move      dataeur       VIDdal
015600141217      *
015700141217     c                   move      a_domenica    DATal             8 0
015800141217     c                   move      a_domenica    dataiso
015900141217     c                   move      dataiso       dataeur
016000141217     c                   move      dataeur       VIDal
016100141217     c                   end
016200141217
016300930812     C                   ELSE
016400930812     C                   Z-ADD     0             VIDDAL
016500930812     C                   Z-ADD     0             VIDAL
016600931125     C                   ENDIF
016700931125     C                   ENDIF
016800930812     C*
016900941013     C  N90VIDFIL        IFNE      '999'
017000930812     C* RICERCA FILIALI SE IMMESSO PUNTO INTERROGATIVO
017100930812     C     '?'           SCAN      VIDFIL                                 21
017200941013     C     *IN21         IFEQ      *ON
017300941013     C     SIMFEL        IFEQ      0
017400930812     C                   MOVEL     *BLANKS       §COD
017500930812     C                   MOVEL     *BLANKS       §TIP
017600930812     C                   MOVEL     *ZEROS        VIDFIL
017700930812     C                   CALL      'TRSE19R'
017800930812     C                   PARM                    §COD              3
017900930812     C                   PARM                    §TIP              2
018000930812     C                   PARM                    §DES             25
018100930812     C                   MOVEL     §COD          VIDFIL
018200930812     C                   SETON                                          90
018300931117     C                   ELSE
018400931117     C                   SETON                                        4390
018500941013     C                   ENDIF
018600941013     C                   ENDIF
018700931117     C*
018800931117     C** FILIALE DA CONTROLLARE OBBLIGATORIA
018900941013     C  N90VIDFIL        IFEQ      *ZEROS
019000931117     C     VIDFIL        OREQ      *BLANKS
019100931117     C                   SETON                                        4190
019200941013     C                   ELSE
019300930812     C*
019400931125     C     VIDFIL        COMP      '000'                              22  22
019500941013     C     *IN22         IFEQ      *OFF
019600930812     C                   SETON                                        9042
019700941013     C                   ELSE
019800930705     C*
019900930705     C* FILIALE INESISTENTE
020000020430     c                   move      vidfil        vidfiln           3 0
020100020430     C     VIDFILn       LOOKUP    L3                                     12
020200931117     C  N12              SETON                                        9043
020300930705     C*
020400941013     C                   ENDIF
020500941013     C                   ENDIF
020600941013     C                   ENDIF
020700930705     C*
020800930812     C                   ENDSR
020900930812     C*-----------
021000930812     C* ROUTINE INIZIALE
021100930812     C*-----------
021200930812     C     *INZSR        BEGSR
021300930701     C*
021400930701     C     *ENTRY        PLIST
021500930701     C                   PARM                    KPJBA
021600930701     C*
021700930701     C                   Z-ADD     1             CODUT             1 0
021800930701     C                   CALL      'X§PARUT'
021900930701     C                   PARM                    UT§DSE
022000930701     C                   MOVEL     REC80         CNCR80
022100930701     C                   MOVEL     RAGUT         RSUT             20
022200930701     C*
022300010921     C** CHIAVE DI FLFG765L
022400930812     C     KFG7          KLIST
022500010921     C                   KFLD                    KAA4              4 0
022600931125     C                   KFLD                    PERIO
022700931117     C*
022800931117     C** CONTROLLO SE SONO IN SEDE O IN FILIALE
022900931117     C     SIMFEL        COMP      0                                      01
023000930812     C*
023100020430     c                   clear                   trul06ds
023200020430     c                   eval      d06cod = '£1'
023300020430     c                   movel     simfel        d06key
023400020430     c                   movel     trul06ds      kpjbu
023500020430     c                   call      'TRUL06R'
023600020430     c                   parm                    kpjba
023700020430     c                   movel     kpjbu         trul06ds
023800020430     c                   movea     lin           l3
023900931117     C*
024000930701     C                   ENDSR

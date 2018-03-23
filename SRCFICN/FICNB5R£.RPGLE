000100061103     H DECEDIT('0,') DATEDIT(*YMD.)
000200020315     H* FICNB5R *----------------------------------------------------*
000300931116     H*         - RISTAMPA CONTEGGI PADRONCINI
000400000000     H*--------------------------------------------------------------*
000500020315     FFICNB5D   CF   E             WORKSTN
000600931015     FTABEL00F  IF   E           K DISK
000700011114     Ffiftt02L  IF   E           K DISK
000800011114     F                                     RENAME(fiftt000:fifst2)
000900011114     Ffifst02L  IF   E           K DISK
001000021202     Ffiapd01L  IF   E           K DISK
001100930707     F*
001200920810     D KPJBA         E DS
001300020701     D* REM                    1      3
001400020701     D* REMFIL                 4      6
001500950105     D* PASSAGGIO DATI A RICERCA CODICI PADRONCINI      - FNLV24R -
002100910423     D WLBDAT          DS
002200950105     D  G02DAT                 1      8  0
002300950105     D  G02INV                 9     16  0
002400950105     D  G02ERR                17     17
002500950105     D  G02TGI                18     22  0
002501061103     D WGIDAT          DS
002502061103     D  GIODAT                 1      8  0
002503061103     D  GIOINV                 9     16  0
002504061103     D  GIOTGI                17     21  0
002600931015     D                 DS
002700931015     D  VI1PF1                 1      3  0
002800931116     D  VI1PD1                 4      7
002900931028     D  VI1C31                 5      7  0
003000931115     D  VI1P1                  1      7  0
003100020315     D* DS PER FICNB6R - RISTAMPA DISTINTA PADRONCINO
003200020315     D DSCNB6        E DS                  EXTNAME(FICNB6DS) INZ
003201061103     D ds5a          E DS
003300931021     D CNCR80        E DS
003400900615     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003401021202     D fnlv24ds      E DS
003500900521     C****************************************************************
003600900521     C*  RIEPILOGO INDICATORI
003700900521     C***************************************************************
003800931116     C* 30    - DI COMODO
003900931116     C* 40/42 - ERRORI
004000900521     C* 90    - ON INDICA CHE C'E' UN ERRORE NEI CAMPI
004100900521     C*****************************************************************
004200000000     C     *ENTRY        PLIST
004300000000     C                   PARM                    KPJBA
004400000000     C                   Z-ADD     1             CODUT
004500900615     C                   CALL      'X§PARUT'
004600900615     C                   PARM                    UT§DSE
004700000000     C                   MOVEL     RAGUT         RSUT             20
004800900615     C                   MOVEL     REC80         CNCR80
004900900511     C*---------------------------------------------------------------*
005000931015     C     KTAB          KLIST
005100900706     C                   KFLD                    CODUT
005200900706     C                   KFLD                    COD               2
005300900706     C                   KFLD                    KEY               8
005400931116     C     KFTT          KLIST
005500931116     C                   KFLD                    VI1P1
005600931116     C                   KFLD                    W01FVL
005700931116     C                   KFLD                    W01DDC
005800931202     C                   KFLD                    W01TSR
005801021202     C     Kapdw         KLIST
005802021202     C                   KFLD                    apdtip
005803021202     C                   KFLD                    W0070
005804021202     c                   movel     'A'           apdtip
005813061103      **
005814061103     C                   MOVEL     '5A'          COD
005815061103     C                   MOVEL     '1       '    KEY
005816061103     C     ktab          CHAIN     taBEL                              30
005817061103     C  N30              MOVEL     TBLUNI        DS5A
005818061103     c  n30              z-add     §5AARB        pulARB
005820061103     C   30              CLEAR                   DS5A
005821061103     c     *like         define    §5AARB        pulARB
005822061103
005823061103     C* D A T A   del giorno
005824061103     C                   z-add     *date         G02INV
005825061103     C                   MOVEL     '3'           G02ERR
005826061103     C                   CALL      'XSRDA8'
005827061103     C                   PARM                    WLBDAT
005828061103
005829061103     C* BOLLE ARRIVI ITALIA E IMPORT
005830061103     C     G02TGI        SUB       §5AARB        GIOTGI
005832061103     C                   CALL      'XSRGI8'
005833061103     C                   PARM                    WGIDAT
005835061103     C                   Z-ADD     GIOINV        DataLimite        8 0
005837061103     C*
005900931015     C*---------------------------------------------------------------*
006000931116     C     *LIKE         DEFINE    FTTFVL        W01FVL
006100931116     C     *LIKE         DEFINE    FTTTSR        W01TSR
006200931116     C     *LIKE         DEFINE    FTTDDC        W01DDC
006300931116     C                   MOVEL     'C'           W01FVL
006400931116     C*---------------------------------------------------------------*
006500910419     C*
006600931015     C* PULIZIA FORMATO 1
006700931111     C                   EXSR      PUL01
006800930426     C*
006900931015     C     FOR01         TAG
007000950105     C                   EXFMT     LRD1D01
007100931116     C*
007200920323     C** CMD3 - FINE LAVORO
007300931112     C   KC              GOTO      FINE
007400931015     C* CONTROLLI FORMATO1
007500900528     C                   EXSR      CONTR1
007600950105     C  NKF
007700931116     COR 90              GOTO      FOR01
007800931116     C*
007900950109     C                   MOVE      VI1PD1        DD0PDR
008000950105     C                   MOVEL     VI1PF1        DD0PDR
008100950105     C                   MOVEL     W01DDC        DD0DDT
008200950105     C                   MOVEL     VI1SML        DD0SIM
008300950105     C                   MOVEL     VI1TSR        DD0TSR
008400020315     C                   MOVEL     DSCNB6        KPJBU
008500011115     c     knmus         ifeq      *all'1'
008600011115     c     vi1sml        ifeq      ' '
008700020315     c                   call      'FICNB6R'
008800011115     c                   parm                    kpjba
008900011115     c                   else
009000020315     c                   call      'FICNB7C'
009100011115     c                   parm                    kpjba
009200011115     c                   end
009300011115     c                   else
009400940608     C     VI1SML        IFEQ      ' '
009500020315     C                   MOVEL     'FNB6'        KCOAZ
009600940608     C                   ELSE
009700020315     C                   MOVEL     'FNB7'        KCOAZ
009800940608     C                   ENDIF
009900940608     C*
010000940608     C                   CALL      'BCH10'
010100940608     C                   PARM                    KPJBA
010200011115     c                   end
010300900611     C*
010400000000     C     FINE          TAG
010500931027     C                   SETON                                        LR
010600931111     C*
010700931111     C* PULIZIA CAMPI FORMATO 1 --------------------------------------*
010800931111     C     PUL01         BEGSR
010900931116     C                   CLEAR                   VI1PD1
011000931111     C                   Z-ADD     SIMFEL        VI1PF1
011100931116     C                   CLEAR                   VI1DDC
011200931116     C                   CLEAR                   VI1SML
011300950105     C***
011400950105     C* VEDO SE SONO SIMFEL O REMOTO
011500950105     C***
011600020701     C*    REM           IFEQ      'REM'
011700020701     C*    REMFIL        ANDGT     *ZEROS
011800020701     C*                  MOVEL     REMFIL        VI1PF1
011900020701     C*                  SETON                                        09
012000020701     C*                  ELSE
012100020701     C*                  MOVEL     SIMFEL        VI1PF1
012200020701     C*                  ENDIF
012300020701     C                   EVAL      VI1PF1 = SimPOU
012400020701     C                   EVAL      *IN09 = (SimTpP = '2' OR SimTpP = *BLANK)
012500950105     C*
012600931111     C                   ENDSR
012700900528     C*
012800900608     C*--- CONTROLLI FORMATO1 ----------------------------------------*
012900900528     C     CONTR1        BEGSR
013000931111     C                   SETOFF                                       90
013100950105     C* T I P O   P R E S T A Z I O N E
013200950105    1C     VI1TSR        IFNE      ' '
013300950105    2C     VI1TSR        IFEQ      '?'
013400950105     C                   MOVEL     CODUT         §KUT              1 0
013500950105     C                   MOVEL     '1Z'          §COD              2
013600950105     C                   MOVE      ' '           VI1TSR
013700950105     C                   MOVEL     *BLANKS       §KEY              8
013800950105     C                   CALL      'X§TABER'
013900950105     C                   PARM                    §KUT
014000950105     C                   PARM                    §COD
014100950105     C                   PARM                    §KEY
014200950105     C                   PARM                    §DES             30
014300950105     C                   MOVEL     §KEY          VI1TSR
014400950105     C                   SETON                                        90
014500950105     C                   GOTO      ENDCT1
014600950105    2C                   ENDIF
014700950105     C* CONTROLLO
014800950105     C                   MOVEL     '1Z'          COD
014900950105     C                   MOVEL(P)  VI1TSR        KEY
015000950105     C     KTAB          CHAIN     TABEL                              30
015100950105     C     *IN30         IFEQ      *ON
015200950105     C     TBLFLG        OREQ      *OFF
015300950105     C                   SETON                                        4390
015400950105     C                   GOTO      ENDCT1
015500950105     C                   ENDIF
015600950105     C                   ENDIF
015700931116     C* P A D R O N C I N O
015800931116     C* RICERCA
015900931116     C     '?'           SCAN      VI1PD1                                 90
016000931116     C*
016100931116     C     *IN90         IFEQ      *ON
016200021202     C                   MOVEL     *BLANKS       d24rsc
016300021202     C                   Z-ADD     0             d24pdr
016400021202     C                   Z-ADD     VI1PF1        d24FIL
016500021202     C                   MOVEL     'R'           d24FLG
016501021202     C                   MOVEL     'A'           d24tip
016600021202     C                   MOVEL     fnlv24ds      KPJBU
016700950105     C                   CALL      'FNLV24R'
016800931116     C                   PARM                    KPJBA
016900021202     C                   MOVEL     KPJBU         fnlv24ds
017000931116     C                   MOVEL     *ZEROS        VI1PD1
017100021202     C                   MOVE      d24pdr        VI1PD1
017200021202     C                   MOVEL     d24pdr        VI1PF1
017300021202     C                   MOVEL     d24rsc        VI1DPD
017400931116     C                   GOTO      ENDCT1
017500931116     C                   ENDIF
017600931116     C*
017700950105     C* ESEGUO TESTN
017800950105     C                   TESTN                   VI1PD1               30
017900950105     C                   MOVE      VI1PD1        W001A             1
018000950105     C     *IN30         IFEQ      *OFF
018100950105     C     W001A         ORLT      '0'
018200950105     C                   SETON                                        4090
018300950105     C                   GOTO      ENDCT1
018400950105     C                   ENDIF
018500950105     C*
018600950105     C                   MOVEL     VI1PF1        W0070             7 0
018700950105     C                   MOVE      VI1PD1        W0070
018800931116     C*
018900021202     C     kapdw         CHAIN     fiapd000                           30
019000950105     C     *IN30         IFEQ      *ON
019100950105     C                   SETON                                        40  90
019200950105     C                   GOTO      ENDCT1
019300950105    2C                   END
019400950105     C                   MOVEL     APDRSC        VI1DPD
019500931115     C*
019600931018     C* D A T A   D I S T I N T A
019700931018     C                   MOVE      VI1DDC        G02DAT
019800931018     C                   MOVEL     *BLANK        G02ERR
019900950105     C                   CALL      'XSRDA8'
020000931018     C                   PARM                    WLBDAT
020100931021    2C     G02ERR        IFEQ      '1'
020200931116     C                   SETON                                        41  90
020300931018     C                   GOTO      ENDCT1
020400931021    2C                   END
020500931018     C                   Z-ADD     G02INV        W01DDC
020600950105     C                   Z-ADD     G02DAT        VI1DDC
020700931116     C*
020701061103     C* controlla se richiesta una data ristampa antecedente le pulizie
020702061103     C*  di filiale di Bolle Arrivi
020703061103     c                   if        w01ddc < DataLimite
020704061103     C                   SETON                                        4590
020705061103     C                   GOTO      ENDCT1
020706061103     C                   end
020707061103     C*
020800931116     C* CONTROLLO SE ESISTE LA SELEZIONE INDICATA DA RISTAMPARE
020900980324     C     VI1SML        IFEQ      *BLANKS
021000011114     C     KFTT          SETLL     fiftt02L                               30
021100931116     C  N30              SETON                                        4290
021200931116     C  N30              GOTO      ENDCT1
021300980324     C                   ELSE
021400011114     C     KFTT          SETLL     fifst02L                               30
021500980324     C  N30              SETON                                        4290
021600980324     C  N30              GOTO      ENDCT1
021700980324     C                   END
021800910320     C*
021900900614     C     ENDCT1        ENDSR

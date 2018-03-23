000100990121     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990121     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300850318     F* MPH114R  *-MP/38-*-------------------------------------------*
000400000000     F*------------------*                                           *
000500850315     F*       - MANUTENZIONE MOVIMENTI DI MAGAZZINO M.P. -           *
000600850315     F*       --------------------------------------------           *
000700850315     F*                                                   AZN.: H114 *
000800990121     F*         FILE : MPH114D-tmamp01L-tmmmp00F-tmmmp04L            *
000900860708     F*                MPSMP01L-ANTAB00F                             *
001000860708RIC  F*                CNACO00F                                      *
001100000000     F*                                                              *
001200000000     F*         PGM. : RIC001R   - RICERCA INTESTAZIONE DITTA        *
001300000000     F*                RICDATE   - CONTROLLO INVERSIONE DATA         *
001400990317     F*                tii102R   - RICERCA CENTRI DI COSTO           *
001500990317     F*                tii104R   - RICERCA ANAGRAF.PRODOTTI          *
001600990317     F*                tii002r   - RICERCA CAUSALI DI MAGAZZINO      *
001700990317     F*                tii108R   - RICERCA CODICI  DI MAGAZZINO      *
001800000000     F*                                                              *
001900000000     F*--------------------------------------------------------------*
002000990317     FTII201D   CF   E             WORKSTN
002100000000     F                                     SFILE(SFL1:REC1)
002200990121     Ftmamp01L  IF   E           K DISK
002300990121     Ftmmmp00F  UF   E             DISK
002400990121     F                                     RENAME(tmmmp000:MMPFIS)
002500990121     Ftmmmp04L  IF   E           K DISK
002600850315     F                                     INFDS(INF1)
002700990121     F                                     RENAME(tmmmp000:MMPLOG)
002800990121     FtmSMP01L  UF   E           K DISK
002900000000     FANTAB00F  IF   E           K DISK
003000990121RIC  Fanent01l  IF   E           K DISK
003100990121RIC  Ftmlmp01L  IF   E           K DISK
003200010725     FTNTBE01l  IF   E           k disk
003300000000     D AZ              S              1    DIM(15)                              *AZIONI CAUS.
003400910311     D ERR             S             30    DIM(20) CTDATA PERRCD(1)             *ERRORI
003500000000     D*--------------------------------------------------------------*
003600000000     D INF1            DS           502
003700990121     D*-----INFORMATION DS X FILE  tmmmp00F :
003800000000     D  NREL1                397    400B 0
003900871209     D OFNDT2          DS           512
004000871209     D* ------ CODICE INDIVIDUALE RIGA MOVIMENTI
004100871209     D  MOVIND                 1      7  0
004200871209     D  DT2BLK                 8    256
004300871209     D  DT2BL2               257    512
004400000000     D MPHDT1          DS           512
004500850315     D*-----DATA AREA NUMERATORE MOVIM. MAGAZZINO
004600850315     D  FIL01                  1     17
004700850315     D  ACOM                  18     19  0
004800850315     D  FIL02                 20     50
004900850315     D  DGIO                  51     56  0
005000850315     D  FIL04                 57    256
005100850315     D  FIL05                257    502
005200000000     D TB153         E DS                  EXTNAME(ANTAB153)
005300871212     D TB152         E DS                  EXTNAME(ANTAB152)
005400871212     D  MOVCOM                70     70
005500910311     D  FLGACC                74     74
005600990121     D RECLIS        E DS                  EXTNAME(tmlmp00F)
005700990211     D  PRE                   29     68P 5
005800871209     D                                     DIM(5)                               *PREZZI LISTINO
005900990121     D RECSAL        E DS                  EXTNAME(tmSMP00F)
006000990211     D  QTK                   86    145P 2
006100000000     D                                     DIM(10)                              *Q.TA'  SALDI
006200000000     D                 DS
006300000000     D*----- ARTICOLO VERSIONE :
006400871015     D  MMPMAT                 1     12
006500871015     D  ARTIC                  1     12
006600000000     D KPJBA         E DS
006700000000     D*----- PASSAGGIO PARAMETRI
006800000000     D  MSGERR               453    502
006900990121     D soc001        E DS                  EXTNAME(xsoc001ds)
007000990121     D xsocds          DS          1000
007100010725    >D DGED          E DS
007200990121     d dataiso         s               d   datfmt(*iso)
007300990121     d dataeur         s               d   datfmt(*eur)
007400000000     C*----------------------------------------------------*
007500000000     C     *ENTRY        PLIST                                                  *
007600000000     C                   PARM                    KPJBA                          *
007700990121     C*---------- RICERCA DITTA :
007800990121     C                   MOVEL     'SOC001'      TIPXSC
007900990121     C                   MOVEL     *blank        SOCXSC
008000990121     C                   EXSR      REPSOC
008100990121     C     RTNXSC        IFNE      '1'
008200990121     C                   MOVEL     XSOCDS        SOC001
008300990121     C                   MOVEL     xscrgs        desdit
008400990121     C                   MOVEL     xscsoc        wsoc              3
008500990121     c                   end
008600990121     c                   time                    como14           14 0
008700990121     c                   move      como14        woggi             8 0
008800990121     c                   move      woggi         dataeur
008900990121     c                   move      dataeur       dataiso
009000990121     c                   move      dataiso       woggig            8 0
009100010725     C*--- Reperisco la divisa corrente ------------------------------*
009200010725     C     ktbe          KLIST
009300010725     C                   KFLD                    kcod
009400010725     C                   KFLD                    kke1
009500010725     C     *like         Define    tbecod        kcod
009600010725     C     *like         Define    tbeke1        kke1
009700010725     C                   move      'GED'         kcod
009800010725     C                   move      *blanks       kke1
009900010725     C                   movel     'MAGAZZINO'   kke1
010000010725     c                   clear                   dged
010100010725     C     ktbe          CHAIN     tntbe01l                           54
010200010725     C  n54              movel     tbeuni        dged
010300010725      *
010400990121     C     KEYcln        KLIST                                                  *
010500990121     C                   KFLD                    wsoc                           *
010600990121     C                   KFLD                    wksc              8            *
010700850315     C*---------- MOVIMENTI DI MAGAZZINO :                 *
010800850315     C*                                                    *
010900850315     C     KEYMMP        KLIST                                                  *
011000850315     C                   KFLD                    AAA114                         *
011100990121     C                   KFLD                    DAL               8 0          *
011200850315     C*---------- ANAGRAFICO MATERIE PRIME :               *
011300850315     C     KEYAMP        KLIST                                                  *
011400850315     C                   KFLD                    MMPMAT                         *
011500000000     C*---------- SALDI DI MAGAZZINO :                     *
011600000000     C     KEYSMP        KLIST                                                  *
011700850315     C                   KFLD                    AAA114                         *
011800850315     C                   KFLD                    MMPMAG                         *
011900850315     C                   KFLD                    MMPMAT                         *
012000000000     C*---------- ANAGRAFICO TABELLE :                     *
012100000000     C     KEYTAB        KLIST                                                  *
012200000000     C                   KFLD                    TABGRU                         *
012300000000     C                   KFLD                    TABCOD                         *
012400860708RIC  C******************************************************
012500860708RIC  C*---------- ANAGRAFICO CENTRI DI COSTO :             *
012600860708RIC  C     KEYCDC        KLIST                                                  *
012700990121RIC  C                   KFLD                    wsoc                           *
012800990121     c                   kfld                    wtpana            3
012900990121RIC  C                   KFLD                    CODCDC            8            *
013000990121     c                   move      'CDC'         wtpana
013100850315     C*---------- RICERCA DATAAREA :       :               *
013200871015     C                   MOVEL     '99999999'    NOVE             12
013300861230     C                   MOVE      '99999999'    NOVE
013400871015     C                   MOVEL     '?'           INTERR           12
013500850315     C     *DTAARA       DEFINE                  MPHDT1                         *
013600871209     C     *DTAARA       DEFINE                  OFNDT2                         *
013700871212     C     *DTAARA       DEFINE                  DTAUCH            6 0          *
013800000000     C     INIZIO        TAG                                                    *
013900000000     C     *LOCK         IN        MPHDT1                               90      *
014000000000     C   90              GOTO      INIZIO                                       *
014100000000     C                   OUT       MPHDT1                                       *
014200990121     c                   z-add     acom          acom4             4 0
014300990121     c     *dmy          move      dgio          dataeur
014400990121     c                   move      dataeur       dgiov
014500990121     c                   if        acom4 > 40
014600990121     c                   add       1900          acom4
014700990121     c                   else
014800990121     c                   add       2000          acom4
014900990121     c                   end
015000990121     C                   z-add     ACOM4         AAA114                         *
015100871120     C                   MOVE      '1      '     MAG114                         *
015200990121     C                   MOVE      woggi         DAL114                         *
015300990121     C                   MOVE      woggi         AL114                          *
015400850315     C                   MOVE      '999'         CAU114                         *
015500861230     C                   MOVE      NOVE          MAT114                         *
015600850315     C                   MOVE      *BLANKS       DES114                         *
015700990121     C                   MOVE      *ZEROS        DTAGIO            8 0          *
015800850315     C*----------------------------------------------------*
015900000000     C     EMISPT        TAG                                                    *
016000850315     C                   SETOFF                                       01        *
016100850315     C                   WRITE     MPH114TS                                     *
016200850315     C                   EXFMT     MPH114PT                                     *
016300990409     C   Kc              GOTO      FINE                                         *
016400850315     C                   MOVEA     *ZEROS        *IN(1)                         *
016500000000     C                   MOVE      *BLANKS       ERRORE                         *
016600000000     C*---------- CONTROLLO ANNO --------------------------*
016700990121     C     ACOM4         SUB       AAA114        UNO               2 09192      *
016800000000     C   91UNO           COMP      1                                  9090      *
016900000000     C   92UNO           COMP      -1                                 9090      *
017000000000     C   90              MOVEL     ERR(1)        ERRORE                         *
017100000000     C   90              SETON                                          10      *
017200000000     C   90              GOTO      EMISPT                                       *
017300850315     C*--------- CONTROLLO MAGAZZINO ----------------------*
017400850315     C     MAG114        IFNE      '9999999'
017500850315     C     MAG114        IFEQ      '?      '                                    *
017600850315     C                   MOVE      *BLANKS       MAG114                         *
017700000000     C                   MOVE      *BLANKS       DAT153                         *
017800990317     C                   CALL      'TII108R'                                    *
017900850315     C                   PARM                    MAG114                         *
018000000000     C                   PARM                    DAT153           75            *
018100850315     C                   SETON                                            11    *
018200000000     C                   GOTO      EMISPT                                       *
018300000000     C                   END                                                    *
018400000000     C                   MOVE      '153'         TABGRU                         *
018500000000     C                   MOVE      *ZEROS        TABCOD                         *
018600850315     C                   MOVE      MAG114        TABCOD                         *
018700000000     C     KEYTAB        CHAIN     ANTAB00F                           90        *
018800000000     C   90              MOVEL     ERR(2)        ERRORE                         *
018900000000     C   90              SETON                                        11        *
019000000000     C   90              GOTO      EMISPT                                       *
019100850315     C                   END                                                    *
019200850315     C*---------- CONTROLLO DATE MOVIMENTO ----------------*
019300990121     C                   move      DGIOv         dataeur                        *
019400990121     c                   move      dataeur       dataiso
019500990121     C                   move      dataiso       dtagio                         *
019600850315     C*-----------                                         *
019700990121     c                   if        dal114 > 0
019800990121     c     *eur          test(d)                 dal114                 90
019900990121     c  n90              move      dal114        dataeur
020000990121     c                   if        *in90
020100990121     c     *dmy          test(d)                 dal114                 90
020200990121     c  n90*dmy          move      dal114        dataeur
020300990121     c  n90              move      dataeur       dal114
020400990121     c                   end
020500990121     c                   end
020600850315     C*-----------                                         *
020700850315     C  N90              DO                                                     *
020800990121     C                   MOVE      Dataeur       dataiso                        *
020900990121     C                   move      dataiso       XDTAMG            8 0          *
021000850315     C                   MOVE      XDTAMG        DAL                            *
021100990121     c                   if        al114 > 0
021200990121     c     *eur          test(d)                 al114                  90
021300990121     c  n90              move      al114         dataeur
021400990121     c                   if        *in90
021500990121     c     *dmy          test(d)                 al114                  90
021600990121     c  n90*dmy          move      al114         dataeur
021700990121     c  n90              move      dataeur       al114
021800990121     c                   end
021900990121     C  n90              MOVE      Dataeur       dataiso                        *
022000990121     C  n90              move      dataiso       XDTAMG                         *
022100990121     C  N90              MOVE      XDTAMG        AL                8 0          *
022200990121     c                   end
022300850315     C                   END                                                    *
022400860708     C*----------------------------------------------------*
022500850315     C  N90DAL           COMP      AL                                 90        *
022600850315     C   90              MOVEL     ERR(3)        ERRORE                         *
022700850315     C   90              SETON                                            12    *
022800850315     C   90              GOTO      EMISPT                                       *
022900850315     C*----------                                          *
023000850315     C     DAL           COMP      DTAGIO                               90      *
023100850315     C   90              MOVEL     ERR(4)        ERRORE                         *
023200850315     C   90              SETON                                            12    *
023300850315     C   90              GOTO      EMISPT                                       *
023400870518SMP  C*---------- CONTROLLO CAUSALE -----------------------*
023500850315     C     CAU114        IFNE      '999'                                        *
023600850315     C     CAU114        IFEQ      '?  '                                        *
023700000000     C                   MOVE      'MP'          PROCAU            2            *
023800850315     C                   MOVE      *BLANKS       CAU114                         *
023900850315     C                   MOVE      *BLANKS       DECSCA           25            *
024000000000     C                   MOVE      *BLANKS       AZICAU           15            *
024100990317     C                   CALL      'TII002R'                                    *
024200000000     C                   PARM                    KPJBA                          *
024300000000     C                   PARM                    PROCAU                         *
024400850315     C                   PARM                    CAU114                         *
024500000000     C                   PARM                    DECSCA                         *
024600000000     C                   PARM                    AZICAU                         *
024700850315     C                   SETON                                            13    *
024800000000     C                   GOTO      EMISPT                                       *
024900000000     C                   END                                                    *
025000000000     C                   MOVE      '152'         TABGRU                         *
025100000000     C                   MOVE      *ZEROS        TABCOD                         *
025200000000     C                   MOVEL     'MP'          CINQUE            5            *
025300850315     C                   MOVE      CAU114        CINQUE            5            *
025400000000     C                   MOVE      CINQUE        TABCOD                         *
025500000000     C     KEYTAB        CHAIN     ANTAB00F                           90        *
025600850315     C   90              MOVEL     ERR(5)        ERRORE                         *
025700850315     C   90              SETON                                        13        *
025800000000     C   90              GOTO      EMISPT                                       *
025900850315     C                   END                                                    *
026000850315     C*---------- CONTROLLO PRODOTTO ----------------------*
026100861230     C     MAT114        IFNE      NOVE                                         *
026200861230     C     MAT114        IFEQ      INTERR
026300850315     C                   MOVE      *BLANKS       MMPMAT                         *
026400990317     C                   CALL      'TII104R'                                    *
026500850315     C                   PARM                    MMPMAT                         *
026600850315     C                   PARM                    DES114                         *
026700850315     C                   MOVE      MMPMAT        MAT114                         *
026800850315     C                   SETON                                        14        *
026900850315     C                   GOTO      EMISPT                                       *
027000850315     C                   END                                                    *
027100850315     C                   MOVE      MAT114        MMPMAT                         *
027200990121     C     KEYAMP        CHAIN     tmamp01L                           90        *
027300850315     C   90              SETON                                        14        *
027400850315     C   90              MOVEL     ERR(12)       ERRORE                         *
027500850315     C   90              GOTO      EMISPT                                       *
027600850315     C                   END                                                    *
027700850315     C*---------- PULIZIA SUBFILE -------------------------*
027800850315     C                   SETON                                          0170    *PULIZIA
027900000000     C                   WRITE     SFL1CTL                                      *
028000850315     C                   MOVEA     *ZEROS        *IN(2)                         *
028100850315     C                   WRITE     MPH114TS                                     *TESTATA
028200850315     C*---------- RICERCA MOVIMENTI -----------------------*
028300000000     C                   Z-ADD     0             REC1              5 0          *
028400000000     C                   Z-ADD     0             RECT              5 0          *
028500000000     C                   Z-ADD     1             CURSOR                         *
028600871015     C                   MOVE      *ALL'X'       OLDART           12            *
028700850315     C*---------                                           *
028800990121     C     KEYMMP        SETLL     tmmmp04L                           90        *
028900850315     C   90              MOVEL     ERR(6)        ERRORE                         *
029000850315     C   90              GOTO      EMISPT                                       *
029100850315     C     RICMOV        TAG                                                    *
029200850315     C     AAA114        READE     MMPLOG                                 81    *
029300850315     C   81
029400850315     CANN71              SETON                                            90    *
029500850315     C   90              MOVEL     ERR(6)        ERRORE                         *
029600850315     C   90              GOTO      EMISPT                                       *
029700850315     C   81              GOTO      SUFOND                                       *
029800850315     C     MMPDMV        IFGT      AL                                           *
029900850315     C  N71              SETON                                            90    *
030000850315     C   90              MOVEL     ERR(6)        ERRORE                         *
030100850315     C   90              GOTO      EMISPT                                       *
030200850315     C                   GOTO      SUFOND                                       *
030300850315     C                   END                                                    *
030400990603     C     rec1          IFeq      9999                                         *
030500990603     c                   seton                                            90
030600990603     C                   MOVEL     ERR(16)       ERRORE                         *
030700990603     C                   GOTO      EMISPT                                       *
030800990603     C                   END                                                    *
030900850315     C*---------- CONTROLLO MAGAZZINO :                    *
031000850315     C     MAG114        IFNE      '9999999'                                    *
031100850315     C     MMPMAG        CABNE     MAG114        RICMOV                         *
031200850315     C                   END                                                    *
031300850315     C*---------- CONTROLLO CAUSALE :                      *
031400850315     C     CAU114        IFNE      '999'                                        *
031500850315     C     MMPCAU        CABNE     CAU114        RICMOV                         *
031600850315     C                   END                                                    *
031700850315     C*---------- CONTROLLO PRODOTTO :                     *
031800861230     C     MAT114        IFNE      NOVE                                         *
031900850315     C     MMPMAT        CABNE     MAT114        RICMOV                         *
032000850315     C                   END                                                    *
032100850315     C*---------- CARICO DATI SU VIDEO :                   *
032200850315     C     REC1          ADD       1             REC1                 71        *
032300850315     C                   Z-ADD     NREL1         RELREC                         *
032400850315     C                   Z-ADD     REC1          RELVID                         *
032500850315     C                   MOVE      *BLANKS       SCELTW                         *
032600850315     C                   MOVE      *BLANKS       SCELTA                         *
032700990121     C                   move      MMPDMV        dataiso                        *
032800990121     C                   move      dataiso       dataeur                        *
032900990121     C                   move      dataeur       datmov                         *
033000850315     C     ARTIC         IFNE      OLDART                                       *
033100990121     C     KEYAMP        CHAIN     tmamp01L                           72        *
033200850315     C   72              MOVE      *ALL'*'       DESART                         *
033300850315     C  N72              MOVEL     AMPDMT        DESART                         *
033400850315     C                   MOVE      ARTIC         OLDART                         *
033500850315     C                   END                                                    *
033600850315     C                   WRITE     SFL1                                         *
033700850315     C                   GOTO      RICMOV                                       *
033800850315     C*----------------------------------------------------*
033900850315     C     SUFOND        TAG                                                    *
034000850315     C                   SETOFF                                         8040    *
034100850315     C                   EXSR      CARFON                                       *
034200850315     C     EMISFL        TAG                                                    *
034300000000     C                   EXFMT     SFL1CTL                                      *
034400000000     C                   MOVEA     *ZEROS        *IN(80)                        *
034500000000     C                   MOVE      *BLANKS       ERRORE                         *
034600990121     C   Kl              GOTO      EMISPT                                       *
034700000000     C   KA              GOTO      SUFOND                                       *
034800000000     C*--------- LETTURA RIGA DA SUBFILE ------------------*
034900000000     C   71              READC     SFL1                                   31    *
035000000000     C   71
035100000000     CANN31              DO                                                     *
035200000000     C     SCELTA        COMP      'A'                                    31    *
035300850315     C  N31SCELTA        CABNE     'X'           EMISFL                   31    *
035400000000     C                   Z-ADD     RELVID        REC1                           *
035500000000     C     REC1          CHAIN     SFL1                               31        *SFL
035600000000     C   31              GOTO      SUFOND                                       *
035700000000     C*--------- GIA ANNULLATO :                           *
035800000000     C     SCELTW        IFEQ      'A'                                          *
035900000000     C                   MOVE      'A'           SCELTA                         *
036000000000     C                   UPDATE    SFL1                                         *
036100000000     C                   GOTO      SUFOND                                       *
036200000000     C                   END                                                    *
036300000000     C*--------- ANNULLAMENTO SCELTA :                     *
036400000000     C   KA              MOVE      ' '           SCELTA                         *
036500000000     C   KA              UPDATE    SFL1                                         *
036600000000     C   KA              GOTO      SUFOND                                       *
036700000000     C*--------- CARICO DATI SUL FONDO---------------------*
036800850315     C                   SETON                                        804032    *
036900000000     C                   UPDATE    SFL1                                         *
037000000000     C                   SETOFF                                           32    *
037100000000     C                   EXSR      CARFON                                       *
037200850315     C                   GOTO      EMISFL                                       *
037300850315     C                   END                                                    *
037400850315     C*--------- CONTROLLO DATI VARIATI -------------------*
037500850315     C     RELVF         CABEQ     0             SUFOND                         *
037600850315     C                   SETOFF                                           40    *
037700910311     C* CONTROLLO AUTORIZZAZIONE VARIAZIONE
037800910311     C                   MOVEL     KNMUS         PROF              3
037900910311     C     PROF          COMP      'EDP'                              5151
038000910311     C   51              DO
038100910311     C                   MOVE      '152'         TABGRU                         *
038200910311     C                   MOVE      *ZEROS        TABCOD                         *
038300910311     C                   MOVEL     'MP'          CINQUE            5            *
038400910311     C                   MOVE      CAUF          CINQUE            5            *
038500910311     C                   MOVE      CINQUE        TABCOD                         *
038600910311     C     KEYTAB        CHAIN     ANTAB00F                           90        *
038700910311     C                   MOVEL     TABDES        TB152
038800910311     C*
038900910311     C     FLGACC        COMP      '1'                                    50
039000910311     C   50              DO
039100910311     C                   MOVEL     ERR(15)       ERRORE
039200910311     C                   SETON                                        90
039300910311     C                   GOTO      EMISFL
039400910311     C                   END
039500910311     C                   END
039600910311     C*
039700850315     C*---------- QUANTITA :                               *
039800850315     C     QTAF          IFLE      0                                            *
039900850315     C                   SETON                                        9190      *
040000850315     C                   MOVEL     ERR(7)        ERRORE                         *
040100850315     C                   GOTO      EMISFL                                       *
040200850315     C                   END                                                    *
040300860708     C*----------------------------------------------------*
040400871214     C     QTAF          CABEQ     QTAAF         NOCTRQ
040500871214     C                   SETOFF                                       43
040600871215     C                   MOVE      AZF           SEGGIA            1
040700871214     C     SEGGIA        CABEQ     '+'           NOCTRQ
040800871214     C                   Z-ADD     0             DIFGIA           15 2
040900871214     C     QTAF          IFGT      QTAAF
041000871214     C     QTAF          SUB       QTAAF         DIFGIA
041100871214     C                   END
041200871214     C     DIFGIA        CABEQ     0             NOCTRQ
041300871214     C                   EXSR      RICDIS
041400871214     C     DISPON        COMP      DIFGIA                               43
041500871214     C   43              GOTO      EMISFL
041600871214     C** CONTROLLO DISPONIBILITA MAGAZZINO PER PRELIEVO
041700871214     C     NOCTRQ        TAG
041800871214     C*----------------------------------------------------*
041900860708     C*---------- CONTROLLO NUM. E DATA DOCUMENTO ---------*
042000850315     C     NDOCF         COMP      0                                      90    *
042100850315     C  N90DDOCF         COMP      0                                      90    *
042200850315     C  N90              DO                                                     *
042300990121     c     *eur          test(d)                 ddocf                  90
042400990121     c  n90              move      ddocf         dataeur
042500990121     c                   if        *in90
042600990121     c     *dmy          test(d)                 ddocf                  90
042700990121     c  n90*dmy          move      ddocf         dataeur
042800990121     c                   end
042900990121     C  n90              move      dataeur       dataiso                        *
043000990121     C  n90              move      dataiso       ddock             8 0          *
043100850315     C                   END                                                    *
043200850315     C   90              SETON                                        92        *
043300850315     C   90              MOVEL     ERR(8)        ERRORE                         *
043400850315     C   90              GOTO      EMISFL                                       *
043500850315     C*---------- C/COSTO :                                *
043600990121     C     CDCF          IFEQ      '?       '                                   *
043700850315     C                   MOVE      *BLANKS       CDCF                           *
043800850315     C                   MOVE      *ZEROS        CODCDC                         *
043900850315     C                   MOVE      *BLANKS       DESCDC                         *
044000990317     C                   CALL      'TII102R'                                    *
044100990121     C                   PARM                    wsoc                           *
044200850315     C                   PARM                    CODCDC                         *
044300850315     C                   PARM                    DESCDC           48            *
044400850315     C                   MOVE      CODCDC        CDCF                           *
044500850315     C                   SETON                                        93        *
044600850315     C                   GOTO      EMISFL                                       *
044700850315     C                   END                                                    *
044800860708RIC  C                   MOVE      CDCF          CODCDC                         *
044900990121RIC  C     KEYCDC        CHAIN     anent01l                           90        *
045000860708     C*----------------------------------------------------*
045100990121     C  N90CDCF          COMP      '99999999'                             90    *
045200990121     C  N90CDCF          COMP      '00000000'                             90    *
045300850315     C  N90CDCF          COMP      *BLANKS                                90    *
045400850315     C   90              SETON                                            93    *
045500850315     C   90              MOVEL     ERR(9)        ERRORE                         *
045600850315     C   90              GOTO      EMISFL                                       *
045700850315     C*----------------------------------------------------*
045800871212     C     KCAU          KLIST
045900871212     C                   KFLD                    GRU               3
046000871212     C                   KFLD                    COD               7
046100871212     C                   MOVEL     '152'         GRU
046200871214     C                   MOVEL     *ZEROS        COD
046300871214     C                   MOVEL     'MP'          COD5              5
046400871214     C                   MOVE      MMPCAU        COD5
046500871214     C                   MOVE      COD5          COD
046600871212     C     KCAU          CHAIN     ANTAB00F                           37
046700871212     C   37              GOTO      EMISFL
046800871212     C** CAUSALE INESISTENTE
046900871212     C                   MOVEL     TABDES        TB152
047000990121      *
047100871212     C                   IN        DTAUCH
047200990121     c     *ymd          move      dtauch        dataiso
047300990121     c                   move      dataiso       dtauch8           8 0
047400990121     C     MMPDMV        COMP      DTAUCH8                              4242
047500871214     C   42              GOTO      EMISFL
047600871212     C** DATA REGISTRAZIONE MOVIMENTO  DEVE ESSERE MAGG. DATA CONG. COSTI
047700871212     C                   MOVEL     *BLANKS       DESCLI
047800990121     C* N35              DO
047900990121     C*    Kcln          CHAIN     ancln01l                           72
048000990121     C* N72clnsogg       chain     ansog01l                           72
048100990121     c* n72              move      sogdes        descli
048200990121     C*                  END
048300871212     C** DEECODIFICA DEL CLIENTE
048400871210     C     PRZF          COMP      0                                      36
048500871210     C   36              GOTO      EMISFL
048600871212     C*+ SE ESISTE COMMESSA IL COSTO DEVE ESSERE > 0
048700850315     C*----------------------------------------------------*
048800990121     C     RELRF         CHAIN     tmmmp00F                           90        *
048900850315     C   90              MOVEL     ERR(10)       ERRORE                         *
049000850315     C   90              GOTO      EMISFL                                       *
049100850315     C                   MOVE      ARTF          ARTIC                          *
049200850315     C*--------- ELIMINAZIONE MOVIMENTO -------------------*
049300871209     C     SCELTA        IFEQ      'A'                                          *
049400850315     C                   MOVE      'A'           MMPATT                         *
049500850315     C                   EXCEPT    ANNMMP                                       *
049600850315     C                   END                                                    *
049700850315     C*--------- ESEGUO OPERAZIONE INVERSA SU SALDI -------*
049800850315     C                   MOVEA     AZF           AZ                             *
049900000000     C     1             DO        15            X                              *
050000000000     C     AZ(X)         COMP      '+'                                    22    *
050100000000     C     AZ(X)         COMP      '-'                                    23    *
050200000000     C   22              MOVE      '-'           AZ(X)                          *
050300000000     C   23              MOVE      '+'           AZ(X)                          *
050400000000     C                   END                                                    *
050500850315     C                   Z-ADD     QTAAF         QTAW              9 2          *
050600000000     C                   EXSR      AGGSAL                                       *
050700850315     C*--------- ESEGUO OPERAZIONE VARIATA ----------------*
050800850315     C     SCELTA        IFEQ      'X'                                          *
050900850315     C                   MOVEA     AZF           AZ                             *
051000000000     C                   Z-ADD     QTAF          QTAW                           *
051100871214     C                   EXSR      AGGSAL
051200000000     C                   EXSR      AGGMOV                                       *
051300850315     C                   END                                                    *
051400000000     C                   EXSR      AGGVID                                       *
051500871211     C                   WRITE     MPH114TS
051600000000     C                   GOTO      SUFOND                                       *
051700000000     C*----------------------------------------------------*
051800000000     C     FINE          TAG                                                    *
051900000000     C   90              MOVEL     'KPJB207'     KMSMN                          *
052000000000     C                   SETON                                            LR    *
052100000000     C*----------------------------------------------------*
052200871214     C     RICDIS        BEGSR
052300871215     C                   Z-ADD     0             DISPON           15 2
052400990121     C     KEYSMP        CHAIN     tmSMP01L                           90
052500871215     C   90              GOTO      NODSIP
052600871214     C     SMPQGF        SUB       SMPQIM        DISPON
052700871214     C                   SUB       SMPQIP        DISPON
052800871215     C     NODSIP        ENDSR
052900000000     C***------------------------------------------------***
053000000000     C**--------- CARICAMENTO DEL FONDO -----------------***
053100000000     C***------------------------------------------------***
053200000000     CSR   CARFON        BEGSR                                                  *
053300850315     C   80              Z-ADD     RELVID        CURSOR                         *
053400000000     C     CURSOR        DIV       10            CURSOR                         *
053500000000     C                   MVR                     RESTO             5 023        *
053600000000     C     CURSOR        MULT      10            CURSOR                         *
053700000000     C   23CURSOR        ADD       1             CURSOR                         *
053800850315     C*---------- PULIZIA DEL FONDO :                      *
053900871210     C                   SETOFF                                       33
054000000000     C  N80              DO                                                     *
054100000000     C                   Z-ADD     0             RELRF                          *
054200000000     C                   Z-ADD     0             RELVF                          *
054300850315     C                   Z-ADD     0             QTAAF                          *
054400850315     C                   MOVE      *BLANKS       SCEF                           *
054500850315     C                   Z-ADD     0             DMVF                           *
054600850315     C                   MOVE      *BLANKS       ARTF                           *
054700850315     C                   MOVE      *BLANKS       DESART                         *
054800850315     C                   MOVE      *BLANKS       CAUF                           *
054900850315     C                   MOVE      *BLANKS       AZF                            *
055000850315     C                   MOVE      *BLANKS       UMAF                           *
055100850315     C                   Z-ADD     0             QTAF                           *
055200850315     C                   MOVE      *BLANKS       MAGF                           *
055300850315     C                   Z-ADD     0             NDOCF                          *
055400850315     C                   Z-ADD     0             DDOCF                          *
055500850315     C                   Z-ADD     0             PRZF                           *
055600990315     c                   move      *blank        valf
055700850315     C                   MOVE      *ZEROS        CDCF                           *
055800871209     C                   MOVEL     'N'           TIPAF
055900871209     C                   Z-ADD     0             PRZVEN
056000000000     C                   END                                                    *
056100850315     C*---------- CARICAMENTO DA SUBFILE :                 *
056200000000     C   80              DO                                                     *
056300850315     C                   Z-ADD     RELREC        RELRF                40        *
056400000000     C                   Z-ADD     RELVID        RELVF                          *
056500850315     C                   MOVE      MMPQTA        QTAAF                          *
056600850315     C                   MOVE      SCELTA        SCEF                           *
056700990121     c                   if        mmpdmv > 0
056800990121     C                   move      MMPDMV        DATaiso                        *
056900990121     C                   move      dataiso       dataeur                        *
057000990121     C                   move      dataeur       dmvf                           *
057100990121     c                   end
057200850315     C                   MOVE      ARTIC         ARTF                           *
057300850315     C                   MOVE      MMPCAU        CAUF                           *
057400850315     C                   MOVE      MMPOPC        AZF                            *
057500000000     C                   MOVE      MMPUMA        UMAF                           *
057600850315     C                   MOVE      MMPQTA        QTAF                           *
057700000000     C                   MOVE      MMPMAG        MAGF                           *
057800000000     C                   Z-ADD     MMPNDC        NDOCF                          *
057900990121     c                   if        mmpddc > 0
058000990121     C                   move      MMPDDC        DATAiso                        *
058100990121     C                   move      dataiso       dataeur                        *
058200990121     C                   move      dataeur       DDOCF                          *
058300990121     c                   end
058400880116     C                   Z-ADD     MMPPRZ        PRZF                           *
058500990315     C                   move      MMPval        valF                           *
058600850315     C                   MOVE      MMPCDC        CDCF                           *
058700871209     C                   END                                                    *
058800000000     C                   SETOFF                                           90    *
058900000000     C                   MOVE      *BLANKS       ERRORE                         *
059000000000     CSR                 ENDSR                                                  *
059100000000     C***------------------------------------------------***
059200000000     C**--------- AGGIORNAMENTO SALDI DI MAGAZZINO ------***
059300000000     C***------------------------------------------------***
059400000000     CSR   AGGSAL        BEGSR                                                  *
059500850315     C                   SETOFF                                       818283    *
059600850315     C                   SETOFF                                       84        *
059700990121     C     KEYSMP        CHAIN     tmSMP01L                           90        *
059800850315     C   90              MOVEL     ERR(11)       ERRORE                         *
059900850315     C   90              GOTO      EMISFL                                       *
060000990121     C                   move      xsccut        SMPUTE                         *
060100990121     C                   Z-ADD     woggig        SMPDMN                         *DATA MANUT.
060200000000     C*--------- AGGIORNAMENTO ENTRATA --------------------*
060300000000     C     1             DO        5             X                 2 0          *
060400850315     C     AZ(X)         COMP      '+'                                    82    *
060500850315     C     AZ(X)         COMP      '-'                                    81    *
060600850315     C   82QTK(X)        ADD       QTAW          QTK(X)                         *
060700850315     C   81QTK(X)        SUB       QTAW          QTK(X)                         *
060800000000     C                   END                                                    *
060900000000     C*--------- AGGIORNAMENTO USCITA ---------------------*
061000000000     C     6             DO        10            X                              *
061100850315     C     AZ(X)         COMP      '+'                                    82    *
061200850315     C     AZ(X)         COMP      '-'                                    81    *
061300850315     C   82QTK(X)        ADD       QTAW          QTK(X)                         *
061400850315     C   81QTK(X)        SUB       QTAW          QTK(X)                         *
061500000000     C                   END                                                    *
061600000000     C*--------- AGGIORNAMENTO GIACENZA -------------------*
061700850315     C     AZ(15)        COMP      '+'                                    82    *
061800850315     C     AZ(15)        COMP      '-'                                    81    *
061900850315     C   82SMPQGF        ADD       QTAW          SMPQGF                         *GIACENZA +
062000850315     C   81SMPQGF        SUB       QTAW          SMPQGF                         *GIACENZA -
062100000000     C*---------- AGGIORNAMENTO SALDI :                    *
062200990121     C                   UPDATE    tmSMP000                                     *UPDATE
062300850315     CSR                 ENDSR                                                  *
062400000000     C***------------------------------------------------***
062500000000     C**--------- AGGIORNAMENTO MOVIMENTI DI MAGAZZINO --***
062600000000     C***------------------------------------------------***
062700000000     CSR   AGGMOV        BEGSR                                                  *
062800990121     C                   MOVE      xsccut        MMPUTE                         *UTENTE
062900990121     C                   Z-ADD     woggig        MMPDMN                         *DATA MANUT.
063000000000     C                   Z-ADD     DDOCK         MMPDDC                         *DATA DOCUM.
063100000000     C                   Z-ADD     NDOCF         MMPNDC                         *NUM. DOCUM.
063200000000     C                   MOVE      CDCF          MMPCDC                         *C/COSTO
063300850315     C                   Z-ADD     QTAF          MMPQTA                         *QUANTITA'
063400880116     C                   Z-ADD     PRZF          MMPPRZ                         *
063500010725     c                   if        §gedcn <> *blank
063600010725     c                   movel     §gedcn        mmpval
063700010725     c                   else
063800990315     c                   move      xscdiv        mmpval
063900010725     c                   end
064000850315     C                   UPDATE    MMPFIS                                       *
064100000000     CSR                 ENDSR                                                  *
064200000000     C***------------------------------------------------***
064300000000     C**--------- CARICAMENTO DEL VIDEO -----------------***
064400000000     C***------------------------------------------------***
064500000000     CSR   AGGVID        BEGSR                                                  *
064600850315     C     RELVF         CHAIN     SFL1                               78        *
064700850315     C                   Z-ADD     RELVF         CURSOR                         *
064800000000     C     SCEF          IFEQ      'A'                                          *
064900000000     C                   MOVE      SCEF          SCELTW                         *
065000000000     C                   ELSE                                                   *
065100000000     C                   MOVE      *BLANKS       SCELTW                         *
065200000000     C                   END                                                    *
065300000000     C                   MOVE      SCELTW        SCELTA                         *
065400850315     C                   Z-ADD     QTAF          MMPQTA                         *
065500880116     C                   Z-ADD     PRZF          MMPPRZ                         *
065600010725     c                   if        §gedcn <> *blank
065700010725     c                   movel     §gedcn        mmpval
065800010725     c                   else
065900990315     C                   MOVE      xscdiv        MMPval                         *
066000010725     c                   end
066100000000     C                   MOVE      CDCF          MMPCDC                         *
066200000000     C                   Z-ADD     DDOCK         MMPDDC                         *
066300000000     C                   Z-ADD     NDOCF         MMPNDC                         *
066400000000     C  N78              UPDATE    SFL1                                         *
066500000000     CSR                 ENDSR                                                  *
066600990121     C*----------------------------------------------------*
066700990121     C* Reperimento dati società
066800990121     C*----------------------------------------------------*
066900990121     C     REPSOC        BEGSR
067000990121     C*
067100990121     C                   CALLB     'XSOC'
067200990121     C                   PARM                    TIPXSC            6
067300990121     C                   PARM                    SOCXSC            3
067400990121     C                   PARM                    CDSXSC            9 0
067500990121     C                   PARM                    MODXSC            3
067600990121     C                   PARM      *blanks       RTNXSC            1
067700990121     C                   PARM                    XSOCDS
067800990121     C                   PARM                    KPJBA
067900990121     C*
068000990121     c                   if        1 = 2
068100990121     c                   write     mph110p2
068200990121     c                   end
068300990121     C                   ENDSR
068400850315     OMMPFIS    E            ANNMMP
068500000000     O                       MMPATT
068600910311**    ERR
068700000000*- Anno non VALIDO !!!!!!!! -*      ERR,1
068800000000*- Codice Magazzino  ERRATO -*      ERR,2
068900850315*- Controllare DATE  !!!!!! -*      ERR,3
069000850315*- Data Inferiore a Giornale-*      ERR,4
069100850315*- Codice Causale    ERRATO -*      ERR,5
069200850315*- Non Trovati Movimenti  ! -*      ERR,6
069300850315*- Inserire Dati Mancanti ! -*      ERR,7
069400850315*- Documento o Data  ERRATI -*      ERR,8
069500850315*- Codice C/Costo    ERRATO -*      ERR,9
069600850315*- Non trovato Movimento !! -*      ERR,10
069700850315*- Non trovati Saldi !!!!!! -*      ERR,11
069800850315*- Codice Prodotto ERRATO ! -*      ERR,12
069900871210ASSIST.CLIENTE                      ERR,13
070000871210CARICO CLIENTE                      ERR,14
070100910311ACCESSO NEGATO ALLA CAUSALE !!      ERR,15
070200990603Range di date troppo ampio  !!      ERR,16

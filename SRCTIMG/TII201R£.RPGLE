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
003200000000     D AZ              S              1    DIM(15)                              *AZIONI CAUS.
003300910311     D ERR             S             30    DIM(20) CTDATA PERRCD(1)             *ERRORI
003400000000     D*--------------------------------------------------------------*
003500000000     D INF1            DS           502
003600990121     D*-----INFORMATION DS X FILE  tmmmp00F :
003700000000     D  NREL1                397    400B 0
003800871209     D OFNDT2          DS           512
003900871209     D* ------ CODICE INDIVIDUALE RIGA MOVIMENTI
004000871209     D  MOVIND                 1      7  0
004100871209     D  DT2BLK                 8    256
004200871209     D  DT2BL2               257    512
004300000000     D MPHDT1          DS           512
004400850315     D*-----DATA AREA NUMERATORE MOVIM. MAGAZZINO
004500850315     D  FIL01                  1     17
004600850315     D  ACOM                  18     19  0
004700850315     D  FIL02                 20     50
004800850315     D  DGIO                  51     56  0
004900850315     D  FIL04                 57    256
005000850315     D  FIL05                257    502
005100000000     D TB153         E DS                  EXTNAME(ANTAB153)
005200871212     D TB152         E DS                  EXTNAME(ANTAB152)
005300871212     D  MOVCOM                70     70
005400910311     D  FLGACC                74     74
005500990121     D RECLIS        E DS                  EXTNAME(tmlmp00F)
005600990211     D  PRE                   29     68P 5
005700871209     D                                     DIM(5)                               *PREZZI LISTINO
005800990121     D RECSAL        E DS                  EXTNAME(tmSMP00F)
005900990211     D  QTK                   86    145P 2
006000000000     D                                     DIM(10)                              *Q.TA'  SALDI
006100000000     D                 DS
006200000000     D*----- ARTICOLO VERSIONE :
006300871015     D  MMPMAT                 1     12
006400871015     D  ARTIC                  1     12
006500000000     D KPJBA         E DS
006600000000     D*----- PASSAGGIO PARAMETRI
006700000000     D  MSGERR               453    502
006800990121     D soc001        E DS                  EXTNAME(xsoc001ds)
006900990121     D xsocds          DS          1000
007000990121     d dataiso         s               d   datfmt(*iso)
007100990121     d dataeur         s               d   datfmt(*eur)
007200000000     C*----------------------------------------------------*
007300000000     C     *ENTRY        PLIST                                                  *
007400000000     C                   PARM                    KPJBA                          *
007500990121     C*---------- RICERCA DITTA :
007600990121     C                   MOVEL     'SOC001'      TIPXSC
007700990121     C                   MOVEL     *blank        SOCXSC
007800990121     C                   EXSR      REPSOC
007900990121     C     RTNXSC        IFNE      '1'
008000990121     C                   MOVEL     XSOCDS        SOC001
008100990121     C                   MOVEL     xscrgs        desdit
008200990121     C                   MOVEL     xscsoc        wsoc              3
008300990121     c                   end
008400990121     c                   time                    como14           14 0
008500990121     c                   move      como14        woggi             8 0
008600990121     c                   move      woggi         dataeur
008700990121     c                   move      dataeur       dataiso
008800990121     c                   move      dataiso       woggig            8 0
008900990121     C     KEYcln        KLIST                                                  *
009000990121     C                   KFLD                    wsoc                           *
009100990121     C                   KFLD                    wksc              8            *
009200850315     C*---------- MOVIMENTI DI MAGAZZINO :                 *
009300850315     C*                                                    *
009400850315     C     KEYMMP        KLIST                                                  *
009500850315     C                   KFLD                    AAA114                         *
009600990121     C                   KFLD                    DAL               8 0          *
009700850315     C*---------- ANAGRAFICO MATERIE PRIME :               *
009800850315     C     KEYAMP        KLIST                                                  *
009900850315     C                   KFLD                    MMPMAT                         *
010000000000     C*---------- SALDI DI MAGAZZINO :                     *
010100000000     C     KEYSMP        KLIST                                                  *
010200850315     C                   KFLD                    AAA114                         *
010300850315     C                   KFLD                    MMPMAG                         *
010400850315     C                   KFLD                    MMPMAT                         *
010500000000     C*---------- ANAGRAFICO TABELLE :                     *
010600000000     C     KEYTAB        KLIST                                                  *
010700000000     C                   KFLD                    TABGRU                         *
010800000000     C                   KFLD                    TABCOD                         *
010900860708RIC  C******************************************************
011000860708RIC  C*---------- ANAGRAFICO CENTRI DI COSTO :             *
011100860708RIC  C     KEYCDC        KLIST                                                  *
011200990121RIC  C                   KFLD                    wsoc                           *
011300990121     c                   kfld                    wtpana            3
011400990121RIC  C                   KFLD                    CODCDC            8            *
011500990121     c                   move      'CDC'         wtpana
011600850315     C*---------- RICERCA DATAAREA :       :               *
011700871015     C                   MOVEL     '99999999'    NOVE             12
011800861230     C                   MOVE      '99999999'    NOVE
011900871015     C                   MOVEL     '?'           INTERR           12
012000850315     C     *DTAARA       DEFINE                  MPHDT1                         *
012100871209     C     *DTAARA       DEFINE                  OFNDT2                         *
012200871212     C     *DTAARA       DEFINE                  DTAUCH            6 0          *
012300000000     C     INIZIO        TAG                                                    *
012400000000     C     *LOCK         IN        MPHDT1                               90      *
012500000000     C   90              GOTO      INIZIO                                       *
012600000000     C                   OUT       MPHDT1                                       *
012700990121     c                   z-add     acom          acom4             4 0
012800990121     c     *dmy          move      dgio          dataeur
012900990121     c                   move      dataeur       dgiov
013000990121     c                   if        acom4 > 40
013100990121     c                   add       1900          acom4
013200990121     c                   else
013300990121     c                   add       2000          acom4
013400990121     c                   end
013500990121     C                   z-add     ACOM4         AAA114                         *
013600871120     C                   MOVE      '1      '     MAG114                         *
013700990121     C                   MOVE      woggi         DAL114                         *
013800990121     C                   MOVE      woggi         AL114                          *
013900850315     C                   MOVE      '999'         CAU114                         *
014000861230     C                   MOVE      NOVE          MAT114                         *
014100850315     C                   MOVE      *BLANKS       DES114                         *
014200990121     C                   MOVE      *ZEROS        DTAGIO            8 0          *
014300850315     C*----------------------------------------------------*
014400000000     C     EMISPT        TAG                                                    *
014500850315     C                   SETOFF                                       01        *
014600850315     C                   WRITE     MPH114TS                                     *
014700850315     C                   EXFMT     MPH114PT                                     *
014800990409     C   Kc              GOTO      FINE                                         *
014900850315     C                   MOVEA     *ZEROS        *IN(1)                         *
015000000000     C                   MOVE      *BLANKS       ERRORE                         *
015100000000     C*---------- CONTROLLO ANNO --------------------------*
015200990121     C     ACOM4         SUB       AAA114        UNO               2 09192      *
015300000000     C   91UNO           COMP      1                                  9090      *
015400000000     C   92UNO           COMP      -1                                 9090      *
015500000000     C   90              MOVEL     ERR(1)        ERRORE                         *
015600000000     C   90              SETON                                          10      *
015700000000     C   90              GOTO      EMISPT                                       *
015800850315     C*--------- CONTROLLO MAGAZZINO ----------------------*
015900850315     C     MAG114        IFNE      '9999999'
016000850315     C     MAG114        IFEQ      '?      '                                    *
016100850315     C                   MOVE      *BLANKS       MAG114                         *
016200000000     C                   MOVE      *BLANKS       DAT153                         *
016300990317     C                   CALL      'TII108R'                                    *
016400850315     C                   PARM                    MAG114                         *
016500000000     C                   PARM                    DAT153           75            *
016600850315     C                   SETON                                            11    *
016700000000     C                   GOTO      EMISPT                                       *
016800000000     C                   END                                                    *
016900000000     C                   MOVE      '153'         TABGRU                         *
017000000000     C                   MOVE      *ZEROS        TABCOD                         *
017100850315     C                   MOVE      MAG114        TABCOD                         *
017200000000     C     KEYTAB        CHAIN     ANTAB00F                           90        *
017300000000     C   90              MOVEL     ERR(2)        ERRORE                         *
017400000000     C   90              SETON                                        11        *
017500000000     C   90              GOTO      EMISPT                                       *
017600850315     C                   END                                                    *
017700850315     C*---------- CONTROLLO DATE MOVIMENTO ----------------*
017800990121     C                   move      DGIOv         dataeur                        *
017900990121     c                   move      dataeur       dataiso
018000990121     C                   move      dataiso       dtagio                         *
018100850315     C*-----------                                         *
018200990121     c                   if        dal114 > 0
018300990121     c     *eur          test(d)                 dal114                 90
018400990121     c  n90              move      dal114        dataeur
018500990121     c                   if        *in90
018600990121     c     *dmy          test(d)                 dal114                 90
018700990121     c  n90*dmy          move      dal114        dataeur
018800990121     c  n90              move      dataeur       dal114
018900990121     c                   end
019000990121     c                   end
019100850315     C*-----------                                         *
019200850315     C  N90              DO                                                     *
019300990121     C                   MOVE      Dataeur       dataiso                        *
019400990121     C                   move      dataiso       XDTAMG            8 0          *
019500850315     C                   MOVE      XDTAMG        DAL                            *
019600990121     c                   if        al114 > 0
019700990121     c     *eur          test(d)                 al114                  90
019800990121     c  n90              move      al114         dataeur
019900990121     c                   if        *in90
020000990121     c     *dmy          test(d)                 al114                  90
020100990121     c  n90*dmy          move      al114         dataeur
020200990121     c  n90              move      dataeur       al114
020300990121     c                   end
020400990121     C  n90              MOVE      Dataeur       dataiso                        *
020500990121     C  n90              move      dataiso       XDTAMG                         *
020600990121     C  N90              MOVE      XDTAMG        AL                8 0          *
020700990121     c                   end
020800850315     C                   END                                                    *
020900860708     C*----------------------------------------------------*
021000850315     C  N90DAL           COMP      AL                                 90        *
021100850315     C   90              MOVEL     ERR(3)        ERRORE                         *
021200850315     C   90              SETON                                            12    *
021300850315     C   90              GOTO      EMISPT                                       *
021400850315     C*----------                                          *
021500850315     C     DAL           COMP      DTAGIO                               90      *
021600850315     C   90              MOVEL     ERR(4)        ERRORE                         *
021700850315     C   90              SETON                                            12    *
021800850315     C   90              GOTO      EMISPT                                       *
021900870518SMP  C*---------- CONTROLLO CAUSALE -----------------------*
022000850315     C     CAU114        IFNE      '999'                                        *
022100850315     C     CAU114        IFEQ      '?  '                                        *
022200000000     C                   MOVE      'MP'          PROCAU            2            *
022300850315     C                   MOVE      *BLANKS       CAU114                         *
022400850315     C                   MOVE      *BLANKS       DECSCA           25            *
022500000000     C                   MOVE      *BLANKS       AZICAU           15            *
022600990317     C                   CALL      'TII002R'                                    *
022700000000     C                   PARM                    KPJBA                          *
022800000000     C                   PARM                    PROCAU                         *
022900850315     C                   PARM                    CAU114                         *
023000000000     C                   PARM                    DECSCA                         *
023100000000     C                   PARM                    AZICAU                         *
023200850315     C                   SETON                                            13    *
023300000000     C                   GOTO      EMISPT                                       *
023400000000     C                   END                                                    *
023500000000     C                   MOVE      '152'         TABGRU                         *
023600000000     C                   MOVE      *ZEROS        TABCOD                         *
023700000000     C                   MOVEL     'MP'          CINQUE            5            *
023800850315     C                   MOVE      CAU114        CINQUE            5            *
023900000000     C                   MOVE      CINQUE        TABCOD                         *
024000000000     C     KEYTAB        CHAIN     ANTAB00F                           90        *
024100850315     C   90              MOVEL     ERR(5)        ERRORE                         *
024200850315     C   90              SETON                                        13        *
024300000000     C   90              GOTO      EMISPT                                       *
024400850315     C                   END                                                    *
024500850315     C*---------- CONTROLLO PRODOTTO ----------------------*
024600861230     C     MAT114        IFNE      NOVE                                         *
024700861230     C     MAT114        IFEQ      INTERR
024800850315     C                   MOVE      *BLANKS       MMPMAT                         *
024900990317     C                   CALL      'TII104R'                                    *
025000850315     C                   PARM                    MMPMAT                         *
025100850315     C                   PARM                    DES114                         *
025200850315     C                   MOVE      MMPMAT        MAT114                         *
025300850315     C                   SETON                                        14        *
025400850315     C                   GOTO      EMISPT                                       *
025500850315     C                   END                                                    *
025600850315     C                   MOVE      MAT114        MMPMAT                         *
025700990121     C     KEYAMP        CHAIN     tmamp01L                           90        *
025800850315     C   90              SETON                                        14        *
025900850315     C   90              MOVEL     ERR(12)       ERRORE                         *
026000850315     C   90              GOTO      EMISPT                                       *
026100850315     C                   END                                                    *
026200850315     C*---------- PULIZIA SUBFILE -------------------------*
026300850315     C                   SETON                                          0170    *PULIZIA
026400000000     C                   WRITE     SFL1CTL                                      *
026500850315     C                   MOVEA     *ZEROS        *IN(2)                         *
026600850315     C                   WRITE     MPH114TS                                     *TESTATA
026700850315     C*---------- RICERCA MOVIMENTI -----------------------*
026800000000     C                   Z-ADD     0             REC1              5 0          *
026900000000     C                   Z-ADD     0             RECT              5 0          *
027000000000     C                   Z-ADD     1             CURSOR                         *
027100871015     C                   MOVE      *ALL'X'       OLDART           12            *
027200850315     C*---------                                           *
027300990121     C     KEYMMP        SETLL     tmmmp04L                           90        *
027400850315     C   90              MOVEL     ERR(6)        ERRORE                         *
027500850315     C   90              GOTO      EMISPT                                       *
027600850315     C     RICMOV        TAG                                                    *
027700850315     C     AAA114        READE     MMPLOG                                 81    *
027800850315     C   81
027900850315     CANN71              SETON                                            90    *
028000850315     C   90              MOVEL     ERR(6)        ERRORE                         *
028100850315     C   90              GOTO      EMISPT                                       *
028200850315     C   81              GOTO      SUFOND                                       *
028300850315     C     MMPDMV        IFGT      AL                                           *
028400850315     C  N71              SETON                                            90    *
028500850315     C   90              MOVEL     ERR(6)        ERRORE                         *
028600850315     C   90              GOTO      EMISPT                                       *
028700850315     C                   GOTO      SUFOND                                       *
028800850315     C                   END                                                    *
028900990603     C     rec1          IFeq      9999                                         *
029000990603     c                   seton                                            90
029100990603     C                   MOVEL     ERR(16)       ERRORE                         *
029200990603     C                   GOTO      EMISPT                                       *
029300990603     C                   END                                                    *
029400850315     C*---------- CONTROLLO MAGAZZINO :                    *
029500850315     C     MAG114        IFNE      '9999999'                                    *
029600850315     C     MMPMAG        CABNE     MAG114        RICMOV                         *
029700850315     C                   END                                                    *
029800850315     C*---------- CONTROLLO CAUSALE :                      *
029900850315     C     CAU114        IFNE      '999'                                        *
030000850315     C     MMPCAU        CABNE     CAU114        RICMOV                         *
030100850315     C                   END                                                    *
030200850315     C*---------- CONTROLLO PRODOTTO :                     *
030300861230     C     MAT114        IFNE      NOVE                                         *
030400850315     C     MMPMAT        CABNE     MAT114        RICMOV                         *
030500850315     C                   END                                                    *
030600850315     C*---------- CARICO DATI SU VIDEO :                   *
030700850315     C     REC1          ADD       1             REC1                 71        *
030800850315     C                   Z-ADD     NREL1         RELREC                         *
030900850315     C                   Z-ADD     REC1          RELVID                         *
031000850315     C                   MOVE      *BLANKS       SCELTW                         *
031100850315     C                   MOVE      *BLANKS       SCELTA                         *
031200990121     C                   move      MMPDMV        dataiso                        *
031300990121     C                   move      dataiso       dataeur                        *
031400990121     C                   move      dataeur       datmov                         *
031500850315     C     ARTIC         IFNE      OLDART                                       *
031600990121     C     KEYAMP        CHAIN     tmamp01L                           72        *
031700850315     C   72              MOVE      *ALL'*'       DESART                         *
031800850315     C  N72              MOVEL     AMPDMT        DESART                         *
031900850315     C                   MOVE      ARTIC         OLDART                         *
032000850315     C                   END                                                    *
032100850315     C                   WRITE     SFL1                                         *
032200850315     C                   GOTO      RICMOV                                       *
032300850315     C*----------------------------------------------------*
032400850315     C     SUFOND        TAG                                                    *
032500850315     C                   SETOFF                                         8040    *
032600850315     C                   EXSR      CARFON                                       *
032700850315     C     EMISFL        TAG                                                    *
032800000000     C                   EXFMT     SFL1CTL                                      *
032900000000     C                   MOVEA     *ZEROS        *IN(80)                        *
033000000000     C                   MOVE      *BLANKS       ERRORE                         *
033100990121     C   Kl              GOTO      EMISPT                                       *
033200000000     C   KA              GOTO      SUFOND                                       *
033300000000     C*--------- LETTURA RIGA DA SUBFILE ------------------*
033400000000     C   71              READC     SFL1                                   31    *
033500000000     C   71
033600000000     CANN31              DO                                                     *
033700000000     C     SCELTA        COMP      'A'                                    31    *
033800850315     C  N31SCELTA        CABNE     'X'           EMISFL                   31    *
033900000000     C                   Z-ADD     RELVID        REC1                           *
034000000000     C     REC1          CHAIN     SFL1                               31        *SFL
034100000000     C   31              GOTO      SUFOND                                       *
034200000000     C*--------- GIA ANNULLATO :                           *
034300000000     C     SCELTW        IFEQ      'A'                                          *
034400000000     C                   MOVE      'A'           SCELTA                         *
034500000000     C                   UPDATE    SFL1                                         *
034600000000     C                   GOTO      SUFOND                                       *
034700000000     C                   END                                                    *
034800000000     C*--------- ANNULLAMENTO SCELTA :                     *
034900000000     C   KA              MOVE      ' '           SCELTA                         *
035000000000     C   KA              UPDATE    SFL1                                         *
035100000000     C   KA              GOTO      SUFOND                                       *
035200000000     C*--------- CARICO DATI SUL FONDO---------------------*
035300850315     C                   SETON                                        804032    *
035400000000     C                   UPDATE    SFL1                                         *
035500000000     C                   SETOFF                                           32    *
035600000000     C                   EXSR      CARFON                                       *
035700850315     C                   GOTO      EMISFL                                       *
035800850315     C                   END                                                    *
035900850315     C*--------- CONTROLLO DATI VARIATI -------------------*
036000850315     C     RELVF         CABEQ     0             SUFOND                         *
036100850315     C                   SETOFF                                           40    *
036200910311     C* CONTROLLO AUTORIZZAZIONE VARIAZIONE
036300910311     C                   MOVEL     KNMUS         PROF              3
036400910311     C     PROF          COMP      'EDP'                              5151
036500910311     C   51              DO
036600910311     C                   MOVE      '152'         TABGRU                         *
036700910311     C                   MOVE      *ZEROS        TABCOD                         *
036800910311     C                   MOVEL     'MP'          CINQUE            5            *
036900910311     C                   MOVE      CAUF          CINQUE            5            *
037000910311     C                   MOVE      CINQUE        TABCOD                         *
037100910311     C     KEYTAB        CHAIN     ANTAB00F                           90        *
037200910311     C                   MOVEL     TABDES        TB152
037300910311     C*
037400910311     C     FLGACC        COMP      '1'                                    50
037500910311     C   50              DO
037600910311     C                   MOVEL     ERR(15)       ERRORE
037700910311     C                   SETON                                        90
037800910311     C                   GOTO      EMISFL
037900910311     C                   END
038000910311     C                   END
038100910311     C*
038200850315     C*---------- QUANTITA :                               *
038300850315     C     QTAF          IFLE      0                                            *
038400850315     C                   SETON                                        9190      *
038500850315     C                   MOVEL     ERR(7)        ERRORE                         *
038600850315     C                   GOTO      EMISFL                                       *
038700850315     C                   END                                                    *
038800860708     C*----------------------------------------------------*
038900871214     C     QTAF          CABEQ     QTAAF         NOCTRQ
039000871214     C                   SETOFF                                       43
039100871215     C                   MOVE      AZF           SEGGIA            1
039200871214     C     SEGGIA        CABEQ     '+'           NOCTRQ
039300871214     C                   Z-ADD     0             DIFGIA           15 2
039400871214     C     QTAF          IFGT      QTAAF
039500871214     C     QTAF          SUB       QTAAF         DIFGIA
039600871214     C                   END
039700871214     C     DIFGIA        CABEQ     0             NOCTRQ
039800871214     C                   EXSR      RICDIS
039900871214     C     DISPON        COMP      DIFGIA                               43
040000871214     C   43              GOTO      EMISFL
040100871214     C** CONTROLLO DISPONIBILITA MAGAZZINO PER PRELIEVO
040200871214     C     NOCTRQ        TAG
040300871214     C*----------------------------------------------------*
040400860708     C*---------- CONTROLLO NUM. E DATA DOCUMENTO ---------*
040500850315     C     NDOCF         COMP      0                                      90    *
040600850315     C  N90DDOCF         COMP      0                                      90    *
040700850315     C  N90              DO                                                     *
040800990121     c     *eur          test(d)                 ddocf                  90
040900990121     c  n90              move      ddocf         dataeur
041000990121     c                   if        *in90
041100990121     c     *dmy          test(d)                 ddocf                  90
041200990121     c  n90*dmy          move      ddocf         dataeur
041300990121     c                   end
041400990121     C  n90              move      dataeur       dataiso                        *
041500990121     C  n90              move      dataiso       ddock             8 0          *
041600850315     C                   END                                                    *
041700850315     C   90              SETON                                        92        *
041800850315     C   90              MOVEL     ERR(8)        ERRORE                         *
041900850315     C   90              GOTO      EMISFL                                       *
042000850315     C*---------- C/COSTO :                                *
042100990121     C     CDCF          IFEQ      '?       '                                   *
042200850315     C                   MOVE      *BLANKS       CDCF                           *
042300850315     C                   MOVE      *ZEROS        CODCDC                         *
042400850315     C                   MOVE      *BLANKS       DESCDC                         *
042500990317     C                   CALL      'TII102R'                                    *
042600990121     C                   PARM                    wsoc                           *
042700850315     C                   PARM                    CODCDC                         *
042800850315     C                   PARM                    DESCDC           48            *
042900850315     C                   MOVE      CODCDC        CDCF                           *
043000850315     C                   SETON                                        93        *
043100850315     C                   GOTO      EMISFL                                       *
043200850315     C                   END                                                    *
043300860708RIC  C                   MOVE      CDCF          CODCDC                         *
043400990121RIC  C     KEYCDC        CHAIN     anent01l                           90        *
043500860708     C*----------------------------------------------------*
043600990121     C  N90CDCF          COMP      '99999999'                             90    *
043700990121     C  N90CDCF          COMP      '00000000'                             90    *
043800850315     C  N90CDCF          COMP      *BLANKS                                90    *
043900850315     C   90              SETON                                            93    *
044000850315     C   90              MOVEL     ERR(9)        ERRORE                         *
044100850315     C   90              GOTO      EMISFL                                       *
044200850315     C*----------------------------------------------------*
044300871212     C     KCAU          KLIST
044400871212     C                   KFLD                    GRU               3
044500871212     C                   KFLD                    COD               7
044600871212     C                   MOVEL     '152'         GRU
044700871214     C                   MOVEL     *ZEROS        COD
044800871214     C                   MOVEL     'MP'          COD5              5
044900871214     C                   MOVE      MMPCAU        COD5
045000871214     C                   MOVE      COD5          COD
045100871212     C     KCAU          CHAIN     ANTAB00F                           37
045200871212     C   37              GOTO      EMISFL
045300871212     C** CAUSALE INESISTENTE
045400871212     C                   MOVEL     TABDES        TB152
045500990121      *
045600871212     C                   IN        DTAUCH
045700990121     c     *ymd          move      dtauch        dataiso
045800990121     c                   move      dataiso       dtauch8           8 0
045900990121     C     MMPDMV        COMP      DTAUCH8                              4242
046000871214     C   42              GOTO      EMISFL
046100871212     C** DATA REGISTRAZIONE MOVIMENTO  DEVE ESSERE MAGG. DATA CONG. COSTI
046200871212     C                   MOVEL     *BLANKS       DESCLI
046300990121     C* N35              DO
046400990121     C*    Kcln          CHAIN     ancln01l                           72
046500990121     C* N72clnsogg       chain     ansog01l                           72
046600990121     c* n72              move      sogdes        descli
046700990121     C*                  END
046800871212     C** DEECODIFICA DEL CLIENTE
046900871210     C     PRZF          COMP      0                                      36
047000871210     C   36              GOTO      EMISFL
047100871212     C*+ SE ESISTE COMMESSA IL COSTO DEVE ESSERE > 0
047200850315     C*----------------------------------------------------*
047300990121     C     RELRF         CHAIN     tmmmp00F                           90        *
047400850315     C   90              MOVEL     ERR(10)       ERRORE                         *
047500850315     C   90              GOTO      EMISFL                                       *
047600850315     C                   MOVE      ARTF          ARTIC                          *
047700850315     C*--------- ELIMINAZIONE MOVIMENTO -------------------*
047800871209     C     SCELTA        IFEQ      'A'                                          *
047900850315     C                   MOVE      'A'           MMPATT                         *
048000850315     C                   EXCEPT    ANNMMP                                       *
048100850315     C                   END                                                    *
048200850315     C*--------- ESEGUO OPERAZIONE INVERSA SU SALDI -------*
048300850315     C                   MOVEA     AZF           AZ                             *
048400000000     C     1             DO        15            X                              *
048500000000     C     AZ(X)         COMP      '+'                                    22    *
048600000000     C     AZ(X)         COMP      '-'                                    23    *
048700000000     C   22              MOVE      '-'           AZ(X)                          *
048800000000     C   23              MOVE      '+'           AZ(X)                          *
048900000000     C                   END                                                    *
049000850315     C                   Z-ADD     QTAAF         QTAW              9 2          *
049100000000     C                   EXSR      AGGSAL                                       *
049200850315     C*--------- ESEGUO OPERAZIONE VARIATA ----------------*
049300850315     C     SCELTA        IFEQ      'X'                                          *
049400850315     C                   MOVEA     AZF           AZ                             *
049500000000     C                   Z-ADD     QTAF          QTAW                           *
049600871214     C                   EXSR      AGGSAL
049700000000     C                   EXSR      AGGMOV                                       *
049800850315     C                   END                                                    *
049900000000     C                   EXSR      AGGVID                                       *
050000871211     C                   WRITE     MPH114TS
050100000000     C                   GOTO      SUFOND                                       *
050200000000     C*----------------------------------------------------*
050300000000     C     FINE          TAG                                                    *
050400000000     C   90              MOVEL     'KPJB207'     KMSMN                          *
050500000000     C                   SETON                                            LR    *
050600000000     C*----------------------------------------------------*
050700871214     C     RICDIS        BEGSR
050800871215     C                   Z-ADD     0             DISPON           15 2
050900990121     C     KEYSMP        CHAIN     tmSMP01L                           90
051000871215     C   90              GOTO      NODSIP
051100871214     C     SMPQGF        SUB       SMPQIM        DISPON
051200871214     C                   SUB       SMPQIP        DISPON
051300871215     C     NODSIP        ENDSR
051400000000     C***------------------------------------------------***
051500000000     C**--------- CARICAMENTO DEL FONDO -----------------***
051600000000     C***------------------------------------------------***
051700000000     CSR   CARFON        BEGSR                                                  *
051800850315     C   80              Z-ADD     RELVID        CURSOR                         *
051900000000     C     CURSOR        DIV       10            CURSOR                         *
052000000000     C                   MVR                     RESTO             5 023        *
052100000000     C     CURSOR        MULT      10            CURSOR                         *
052200000000     C   23CURSOR        ADD       1             CURSOR                         *
052300850315     C*---------- PULIZIA DEL FONDO :                      *
052400871210     C                   SETOFF                                       33
052500000000     C  N80              DO                                                     *
052600000000     C                   Z-ADD     0             RELRF                          *
052700000000     C                   Z-ADD     0             RELVF                          *
052800850315     C                   Z-ADD     0             QTAAF                          *
052900850315     C                   MOVE      *BLANKS       SCEF                           *
053000850315     C                   Z-ADD     0             DMVF                           *
053100850315     C                   MOVE      *BLANKS       ARTF                           *
053200850315     C                   MOVE      *BLANKS       DESART                         *
053300850315     C                   MOVE      *BLANKS       CAUF                           *
053400850315     C                   MOVE      *BLANKS       AZF                            *
053500850315     C                   MOVE      *BLANKS       UMAF                           *
053600850315     C                   Z-ADD     0             QTAF                           *
053700850315     C                   MOVE      *BLANKS       MAGF                           *
053800850315     C                   Z-ADD     0             NDOCF                          *
053900850315     C                   Z-ADD     0             DDOCF                          *
054000850315     C                   Z-ADD     0             PRZF                           *
054100990315     c                   move      *blank        valf
054200850315     C                   MOVE      *ZEROS        CDCF                           *
054300871209     C                   MOVEL     'N'           TIPAF
054400871209     C                   Z-ADD     0             PRZVEN
054500000000     C                   END                                                    *
054600850315     C*---------- CARICAMENTO DA SUBFILE :                 *
054700000000     C   80              DO                                                     *
054800850315     C                   Z-ADD     RELREC        RELRF                40        *
054900000000     C                   Z-ADD     RELVID        RELVF                          *
055000850315     C                   MOVE      MMPQTA        QTAAF                          *
055100850315     C                   MOVE      SCELTA        SCEF                           *
055200990121     c                   if        mmpdmv > 0
055300990121     C                   move      MMPDMV        DATaiso                        *
055400990121     C                   move      dataiso       dataeur                        *
055500990121     C                   move      dataeur       dmvf                           *
055600990121     c                   end
055700850315     C                   MOVE      ARTIC         ARTF                           *
055800850315     C                   MOVE      MMPCAU        CAUF                           *
055900850315     C                   MOVE      MMPOPC        AZF                            *
056000000000     C                   MOVE      MMPUMA        UMAF                           *
056100850315     C                   MOVE      MMPQTA        QTAF                           *
056200000000     C                   MOVE      MMPMAG        MAGF                           *
056300000000     C                   Z-ADD     MMPNDC        NDOCF                          *
056400990121     c                   if        mmpddc > 0
056500990121     C                   move      MMPDDC        DATAiso                        *
056600990121     C                   move      dataiso       dataeur                        *
056700990121     C                   move      dataeur       DDOCF                          *
056800990121     c                   end
056900880116     C                   Z-ADD     MMPPRZ        PRZF                           *
057000990315     C                   move      MMPval        valF                           *
057100850315     C                   MOVE      MMPCDC        CDCF                           *
057200871209     C                   END                                                    *
057300000000     C                   SETOFF                                           90    *
057400000000     C                   MOVE      *BLANKS       ERRORE                         *
057500000000     CSR                 ENDSR                                                  *
057600000000     C***------------------------------------------------***
057700000000     C**--------- AGGIORNAMENTO SALDI DI MAGAZZINO ------***
057800000000     C***------------------------------------------------***
057900000000     CSR   AGGSAL        BEGSR                                                  *
058000850315     C                   SETOFF                                       818283    *
058100850315     C                   SETOFF                                       84        *
058200990121     C     KEYSMP        CHAIN     tmSMP01L                           90        *
058300850315     C   90              MOVEL     ERR(11)       ERRORE                         *
058400850315     C   90              GOTO      EMISFL                                       *
058500990121     C                   move      xsccut        SMPUTE                         *
058600990121     C                   Z-ADD     woggig        SMPDMN                         *DATA MANUT.
058700000000     C*--------- AGGIORNAMENTO ENTRATA --------------------*
058800000000     C     1             DO        5             X                 2 0          *
058900850315     C     AZ(X)         COMP      '+'                                    82    *
059000850315     C     AZ(X)         COMP      '-'                                    81    *
059100850315     C   82QTK(X)        ADD       QTAW          QTK(X)                         *
059200850315     C   81QTK(X)        SUB       QTAW          QTK(X)                         *
059300000000     C                   END                                                    *
059400000000     C*--------- AGGIORNAMENTO USCITA ---------------------*
059500000000     C     6             DO        10            X                              *
059600850315     C     AZ(X)         COMP      '+'                                    82    *
059700850315     C     AZ(X)         COMP      '-'                                    81    *
059800850315     C   82QTK(X)        ADD       QTAW          QTK(X)                         *
059900850315     C   81QTK(X)        SUB       QTAW          QTK(X)                         *
060000000000     C                   END                                                    *
060100000000     C*--------- AGGIORNAMENTO GIACENZA -------------------*
060200850315     C     AZ(15)        COMP      '+'                                    82    *
060300850315     C     AZ(15)        COMP      '-'                                    81    *
060400850315     C   82SMPQGF        ADD       QTAW          SMPQGF                         *GIACENZA +
060500850315     C   81SMPQGF        SUB       QTAW          SMPQGF                         *GIACENZA -
060600000000     C*---------- AGGIORNAMENTO SALDI :                    *
060700990121     C                   UPDATE    tmSMP000                                     *UPDATE
060800850315     CSR                 ENDSR                                                  *
060900000000     C***------------------------------------------------***
061000000000     C**--------- AGGIORNAMENTO MOVIMENTI DI MAGAZZINO --***
061100000000     C***------------------------------------------------***
061200000000     CSR   AGGMOV        BEGSR                                                  *
061300990121     C                   MOVE      xsccut        MMPUTE                         *UTENTE
061400990121     C                   Z-ADD     woggig        MMPDMN                         *DATA MANUT.
061500000000     C                   Z-ADD     DDOCK         MMPDDC                         *DATA DOCUM.
061600000000     C                   Z-ADD     NDOCF         MMPNDC                         *NUM. DOCUM.
061700000000     C                   MOVE      CDCF          MMPCDC                         *C/COSTO
061800850315     C                   Z-ADD     QTAF          MMPQTA                         *QUANTITA'
061900880116     C                   Z-ADD     PRZF          MMPPRZ                         *
062000990315     c                   move      xscdiv        mmpval
062100850315     C                   UPDATE    MMPFIS                                       *
062200000000     CSR                 ENDSR                                                  *
062300000000     C***------------------------------------------------***
062400000000     C**--------- CARICAMENTO DEL VIDEO -----------------***
062500000000     C***------------------------------------------------***
062600000000     CSR   AGGVID        BEGSR                                                  *
062700850315     C     RELVF         CHAIN     SFL1                               78        *
062800850315     C                   Z-ADD     RELVF         CURSOR                         *
062900000000     C     SCEF          IFEQ      'A'                                          *
063000000000     C                   MOVE      SCEF          SCELTW                         *
063100000000     C                   ELSE                                                   *
063200000000     C                   MOVE      *BLANKS       SCELTW                         *
063300000000     C                   END                                                    *
063400000000     C                   MOVE      SCELTW        SCELTA                         *
063500850315     C                   Z-ADD     QTAF          MMPQTA                         *
063600880116     C                   Z-ADD     PRZF          MMPPRZ                         *
063700990315     C                   MOVE      xscdiv        MMPval                         *
063800000000     C                   MOVE      CDCF          MMPCDC                         *
063900000000     C                   Z-ADD     DDOCK         MMPDDC                         *
064000000000     C                   Z-ADD     NDOCF         MMPNDC                         *
064100000000     C  N78              UPDATE    SFL1                                         *
064200000000     CSR                 ENDSR                                                  *
064300990121     C*----------------------------------------------------*
064400990121     C* Reperimento dati società
064500990121     C*----------------------------------------------------*
064600990121     C     REPSOC        BEGSR
064700990121     C*
064800990121     C                   CALLB     'XSOC'
064900990121     C                   PARM                    TIPXSC            6
065000990121     C                   PARM                    SOCXSC            3
065100990121     C                   PARM                    CDSXSC            9 0
065200990121     C                   PARM                    MODXSC            3
065300990121     C                   PARM      *blanks       RTNXSC            1
065400990121     C                   PARM                    XSOCDS
065500990121     C                   PARM                    KPJBA
065600990121     C*
065700990121     c                   if        1 = 2
065800990121     c                   write     mph110p2
065900990121     c                   end
066000990121     C                   ENDSR
066100850315     OMMPFIS    E            ANNMMP
066200000000     O                       MMPATT
066300910311**    ERR
066400000000*- Anno non VALIDO !!!!!!!! -*      ERR,1
066500000000*- Codice Magazzino  ERRATO -*      ERR,2
066600850315*- Controllare DATE  !!!!!! -*      ERR,3
066700850315*- Data Inferiore a Giornale-*      ERR,4
066800850315*- Codice Causale    ERRATO -*      ERR,5
066900850315*- Non Trovati Movimenti  ! -*      ERR,6
067000850315*- Inserire Dati Mancanti ! -*      ERR,7
067100850315*- Documento o Data  ERRATI -*      ERR,8
067200850315*- Codice C/Costo    ERRATO -*      ERR,9
067300850315*- Non trovato Movimento !! -*      ERR,10
067400850315*- Non trovati Saldi !!!!!! -*      ERR,11
067500850315*- Codice Prodotto ERRATO ! -*      ERR,12
067600871210ASSIST.CLIENTE                      ERR,13
067700871210CARICO CLIENTE                      ERR,14
067800910311ACCESSO NEGATO ALLA CAUSALE !!      ERR,15
067900990603Range di date troppo ampio  !!      ERR,16

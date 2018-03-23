000100990122     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990122     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300850109     F* MPH116R *-MP/38-*--------------------------------------------*
000400850109     F*-----------------*                                            *
000500850109     F*       - INTERROGAZIONE MOVIMENTI DI MAGAZZINO -              *
000600850109     F*       -----------------------------------------              *
000700850109     F*                                                   AZN.: H116 *
000800990122     F*         FILE : MPH116D-tmamp01L-tmmmp01L-                    *
000900850109     F*                ANTAB00F-                                     *
001000850109     F*                                                              *
001100850109     F*         PGM. : RIC001R   - RICERCA INTESTAZIONE DITTA        *
001200850109     F*                RICDATE   - CONTROLLO INVERSIONE DATA         *
001300990317     F*                tii104R   - RICERCA ANAGRAF.PRODOTTI          *
001400990317     F*                tii002r   - RICERCA CAUSALI DI MAGAZZINO      *
001500990317     F*                tii108R   - RICERCA CODICI  DI MAGAZZINO      *
001600850109     F*                                                              *
001700850109     F*         SBM. : AZN.-H119 - STAMPA MOVIMENTI DI MAGAZZINO     *
001800850109     F*--------------------------------------------------------------*
001900990317     FTII203D   CF   E             WORKSTN
002000850109     F                                     SFILE(SFL1:REC1)
002100990122     Ftmamp01L  IF   E           K DISK
002200990122     Ftmmmp01L  IF   E           K DISK
002300850109     FANTAB00F  IF   E           K DISK
002400850111     D ERR             S             30    DIM(6) CTDATA PERRCD(1)              *ERRORI
002500850109     D AZ              S              1    DIM(15)                              *AZ.CAUSALE   CIT
002600880129     D MPHDT1          DS           512
002700880129     D*-----DATA AREA NUMERATORE MOVIM. MAGAZZINO
002800880129     D  NUMDTA                 1      5  0
002900880129     D  FILL1                  6      8
003000880129     D  DATDTA                 9     14  0
003100880129     D  FILL2                 15     17
003200880129     D  ANNDTA                18     19  0
003300880129     D  FIL03                 20     50
003400880129     D  DGIO                  51     56  0
003500880129     D  FIL04                 57    100
003600880129     D  FILL5                101    300
003700880129     D  GIOTOL               301    302  0
003800880129     D  FILL6                303    512
003900850109     D TB153         E DS                  EXTNAME(ANTAB153)
004000990122     D PH119           DS
004100990122     D*----- PASSAGGIO PARAMETRI X STAMPA MOVIMENTI :
004200990122     D  AAA119                 1      4  0
004300990122     D  MAG119                 5     11
004400990122     D  DAL119                12     19  0
004500990122     D  AL119                 20     27  0
004600990122     D  UMI119                31     31
004700990122     D  CAU119                32     46
004800990122     D  KCAU                  32     46
004900990122     D                                     DIM(5)                               *CAUSALI
005000990122     D  CDC119                47     54
005100990122     D  STA119                55     55
005200990122     D  MOV119                56     60  0
005300990122     D  MAT119                61     72
005400990122     d  desdit               187    206
005500850109     D KPJBA         E DS
005600990122     D PH116           DS
005700990122     D*----- PASSAGGIO PARAMETRI X VISUALIZZ. MOVIMENTO :
005800990122     D  AAA116                 1      4  0
005900990122     D  AA116                  1      4
006000990122     D  MAG116                 5     11
006100990122     D  UMI116                21     21
006200990122     D  CAU116                22     24
006300990122     D  DAL116                25     32  0
006400990122     D  kdal                  25     32
006500990122     D  AL116                 33     40  0
006600990122     D  kal                   33     40
006700990122     D  MOV116                41     45  0
006800990122     D  mov                   41     45
006900990122     D  MAT116                46     57
007000990122     D  MSGERR               207    256
007100990122     D soc001        E DS                  EXTNAME(xsoc001ds)
007200990122     D xsocds          DS          1000
007300990122     d dataiso         s               d   datfmt(*iso)
007400990122     d dataeur         s               d   datfmt(*eur)
007500850109     C*----------------------------------------------------*
007600850109     C     *ENTRY        PLIST                                                  *
007700850109     C                   PARM                    KPJBA                          *
007800990122     c                   movel     kpjbu         ph116
007900990122     C                   MOVEL     '?'           INTERR           12
008000990122     C                   MOVE      *ZEROS        Ph119                          *
008100990122     C*---------- RICERCA DITTA :
008200990122     C                   MOVEL     'SOC001'      TIPXSC
008300990122     C                   MOVEL     *blank        SOCXSC
008400990122     C                   EXSR      REPSOC
008500990122     C     RTNXSC        IFNE      '1'
008600990122     C                   MOVEL     XSOCDS        SOC001
008700990122     C                   MOVEL     xscrgs        desdit
008800990122     C                   MOVEL     xscsoc        wsoc              3
008900990122     c                   end
009000990122     c                   time                    como14           14 0
009100990122     c                   move      como14        woggi             8 0
009200990122     c                   move      woggi         dataeur
009300990122     c                   move      dataeur       dataiso
009400990122     c                   move      dataiso       woggig            8 0
009500880129     C     *DTAARA       DEFINE                  MPHDT1                         *
009600850109     C*---------- ANAGRAFICO MATERIE PRIME :               *
009700850109     C     KEYAMP        KLIST                                                  *
009800850109     C                   KFLD                    MAT116                         *
009900850111     C*---------- MOVIMENTI DI MAGAZZINO :                 *
010000850111     C     KEYMMP        KLIST                                                  *
010100990122     C                   KFLD                    AAa116                         *
010200850111     C                   KFLD                    MAG116                         *
010300850111     C                   KFLD                    MAT116                         *
010400850111     C     KY1MMP        KLIST                                                  *
010500990122     C                   KFLD                    AAa116                         *
010600850111     C                   KFLD                    MAG116                         *
010700850111     C                   KFLD                    MAT116                         *
010800990122     C                   KFLD                    DATDAL            8 0          *
010900850109     C*---------- ANAGRAFICO TABELLE :                     *
011000850109     C     KEYTAB        KLIST                                                  *
011100850109     C                   KFLD                    TABGRU                         *
011200850109     C                   KFLD                    TABCOD                         *
011300880129     C                   IN        MPHDT1                               46
011400850109     C     AA116         COMP      *BLANKS                                90    *
011500990122     c   90              do
011600990122     C                   z-add     ANNDTA        AAa116                         *
011700990122     c                   if        aaa116 > 40
011800990122     c                   add       1900          aaa116
011900990122     c                   else
012000990122     c                   add       2000          aaa116
012100990122     c                   endif
012200990122     c                   enddo
012300850109     C     MAG116        COMP      *BLANKS                                90    *
012400871121     C   90              MOVE      '1      '     MAG116                         *
012500850109     C     KDAL          COMP      *BLANKS                                90    *
012600850111     C   90              MOVE      *ZEROS        DAL116                         *
012700990122     C   90              Z-ADD     aaa116        DAL116                         *
012800850109     C   90              MOVEL     '0101'        DAL116                         *
012900850109     C     KAL           COMP      *BLANKS                                90    *
013000990122     C   90              MOVE      woggi         AL116                          *
013100850109     C     MAT116        COMP      *BLANKS                                90    *
013200861230     C   90              MOVE      INTERR        MAT116                         *
013300850614     C     INIZIO        TAG                                                    *
013400850111     C     UMI116        COMP      *BLANKS                                90    *
013500850111     C   90              MOVE      'A'           UMI116                         *
013600850111     C     CAU116        COMP      *BLANKS                                90    *
013700850111     C   90              MOVE      '999'         CAU116                         *
013800850111     C*--------- CONTROLLI SU PROMPT ----------------------*
013900850111     C                   MOVEA     *ZEROS        *IN(1)                         *
014000850111     C                   MOVE      *BLANKS       ERRORE                         *
014100850109     C     EMISPT        TAG                                                    *
014200850109     C                   SETOFF                                           10    *
014300850109     C                   WRITE     MPH116TS                                     *
014400850109     C                   EXFMT     MPH116PT                                     *
014500990409     C   Kc              GOTO      FINE                                         *
014600850111     C                   MOVEA     *ZEROS        *IN(1)                         *
014700850109     C                   MOVE      *BLANKS       ERRORE                         *
014800850109     C*---------- MAGAZZINO :                              *
014900850109     C     MAG116        IFEQ      '?      '                                    *
015000850109     C                   MOVE      *BLANKS       MAG116                         *
015100850109     C                   MOVE      *BLANKS       DAT153                         *
015200990317     C                   CALL      'TII108R'                                    *
015300850109     C                   PARM                    MAG116                         *
015400850109     C                   PARM                    DAT153           75            *
015500850109     C                   GOTO      EMISPT                                       *
015600850109     C                   END                                                    *
015700850109     C                   MOVE      '153'         TABGRU                         *
015800850109     C                   MOVE      *ZEROS        TABCOD                         *
015900850109     C                   MOVE      MAG116        TABCOD                         *
016000850109     C     KEYTAB        CHAIN     ANTAB00F                           90        *
016100850109     C   90              SETON                                            91    *
016200850109     C   90              MOVEL     ERR(1)        ERRORE                         *
016300850109     C   90              GOTO      EMISPT                                       *
016400850109     C                   MOVEL     TABDES        TB153                          *
016500850109     C*---------- CONTROLLO DATA DAL :                     *
016600990122     C                   MOVE      DAL116        dataeur                        *
016700990122     c                   move      dataeur       dataiso
016800990122     c                   move      dataiso       datdal
016900850109     C*---------- CONTROLLO DATA  AL :                     *
017000990122     C                   MOVE      AL116         dataeur                        *
017100990122     c                   move      dataeur       dataiso
017200990122     c                   move      dataiso       datal             8 0
017300850111     C*--------- CONTROLLI SI PROMPT ----------------------*
017400850111     C*---------- VERIFICA DATE :                          *
017500850111     C     DATAL         IFLT      DATDAL                                       *
017600850111     C                   SETON                                        9092      *
017700850111     C                   MOVEL     ERR(2)        ERRORE                         *
017800850111     C                   GOTO      EMISPT                                       *
017900850111     C                   END                                                    *
018000850109     C*---------- PRODOTTO :                               *
018100861230     C     MAT116        IFEQ      INTERR                                       *
018200850109     C                   MOVE      *BLANKS       MAT116                         *
018300990122     C                   MOVE      *BLANKS       desartw          30            *
018400990317     C                   CALL      'TII104R'                                    *
018500850109     C                   PARM                    MAT116                         *
018600990122     C                   PARM      desart        DESARTw                        *
018700850109     C                   GOTO      EMISPT                                       *
018800850109     C                   END                                                    *
018900990122     C     KEYAMP        CHAIN     tmamp01L                           90        *
019000850109     C   90              SETON                                            93    *
019100850109     C   90              MOVEL     ERR(3)        ERRORE                         *
019200850109     C   90              GOTO      EMISPT                                       *
019300850109     C                   MOVEL     AMPDMT        DESART                         *
019400850109     C*---------- CONTROLLO CAUSALE :                      *
019500850109     C     CAU116        IFNE      '999'                                        *
019600850111     C     CAU116        IFEQ      '?  '                                        *
019700850109     C                   MOVE      'MP'          PROCAU            2            *
019800850111     C                   MOVE      *BLANKS       CAU116                         *
019900850111     C                   MOVE      *BLANKS       DESCA            25            *
020000850109     C                   MOVE      *BLANKS       AZICAU           15            *
020100990317     C                   CALL      'TII002R'                                    *
020200850109     C                   PARM                    KPJBA                          *
020300850109     C                   PARM                    PROCAU                         *
020400850111     C                   PARM                    CAU116                         *
020500850111     C                   PARM                    DESCA                          *
020600850109     C                   PARM                    AZICAU                         *
020700850109     C                   GOTO      EMISPT                                       *
020800850109     C                   END                                                    *
020900850109     C                   MOVE      '152'         TABGRU                         *
021000850109     C                   MOVE      *ZEROS        TABCOD                         *
021100850109     C                   MOVEL     'MP'          CINQUE            5            *
021200850111     C                   MOVE      CAU116        CINQUE            5            *
021300850109     C                   MOVE      CINQUE        TABCOD                         *
021400850109     C     KEYTAB        CHAIN     ANTAB00F                           90        *
021500850111     C   90              SETON                                            94    *
021600850111     C   90              MOVEL     ERR(4)        ERRORE                         *
021700850109     C   90              GOTO      EMISPT                                       *
021800850111     C                   END                                                    *
021900850111     C*---------- RICERCA MOVIMENTI -----------------------*
022000850111     C                   Z-ADD     0             REC1              5 0          *
022100850111     C                   Z-ADD     0             TOTDEN                         *
022200850111     C                   Z-ADD     0             TOTDUS                         *
022300850111     C                   Z-ADD     0             SALDO                          *
022400850111     C                   Z-ADD     0             SALDOD           11 2          *
022500850111     C                   Z-ADD     99            OLDVER            2 0          *
022600850111     C                   MOVE      'KKK'         OLDCAU            3            *
022700850111     C     MOV           IFEQ      *BLANKS                                      *
022800850111     C                   MOVE      *ZEROS        MOV116                         *
022900850111     C                   END                                                    *
023000850111     C     MOV116        COMP      0                                  11        *
023100850111     C*---------- PULIZIA SUBFILE :                        *
023200850111     C                   SETON                                            70    *PULIZIA
023300850111     C                   WRITE     SFL1CTL                                      *
023400850111     C                   SETOFF                                           70    *
023500850111     C*---------- RICERCA MOVIMENTI :                      *
023600990122     C     KY1MMP        SETLL     tmmmp01L                           90        *
023700850111     C   90              MOVEL     ERR(5)        ERRORE                         *NO MOVIMENTI
023800850111     C   90              GOTO      EMISPT                                       *
023900850111     C     RICMOV        TAG                                                    *
024000990122     C     KEYMMP        READE     tmmmp000                               95    *
024100850111     C     ENDMOV        TAG                                                    *
024200850111     C   95
024300850111     CAN 71              EXSR      RUTSAL                                       *
024400850111     C   95
024500850111     CAN 71              GOTO      EMISSF                                       *
024600850111     C   95
024700850111     CANN71              SETON                                        90        *
024800850111     C   90              MOVEL     ERR(5)        ERRORE                         *NO MOVIMENTI
024900850109     C   90              GOTO      EMISPT                                       *
025000850111     C*---------- CONTROLLO DATA MOVIMENTO :               *
025100850111     C     MMPDMV        CABLT     DATDAL        RICMOV                         *
025200850111     C     MMPDMV        CABGT     DATAL         RICMOV                         *
025300850111     C   11MMPNMV        CABNE     MOV116        RICMOV                         *
025400850111     C     CAU116        IFNE      '999'                                        *
025500850111     C     MMPCAU        CABNE     CAU116        RICMOV                         *
025600850111     C                   END                                                    *
025700850111     C                   MOVEA     MMPOPC        AZ                             *
025800850111     C*---------- EMISSIONE MOVIMENTI ---------------------*
025900850111     C*---------- ROTTURA DI ARTICOLO/VERSIONE :           *
026000861230     C   05              EXSR      RUTSAL                                       *
026100990122     C     KEYAMP        CHAIN     tmamp01L                           95        *
026200861230     C   95              GOTO      RICMOV                                       *
026300861230     C     UMI116        COMP      'A'                                    95    *UNITA MISURA
026400861230     C   95              MOVE      AMPUMA        UMIV                           *ANAGRAFICA
026500861230     C   95              Z-ADD     1             FCV              11 5    96    *
026600861230     C     UMI116        COMP      'Q'                                    95    *           A
026700861230     C   95              MOVE      AMPUMQ        UMIV                           *ACQUISTO
026800861230     C   95              Z-ADD     AMPFCQ        FCV                      96    *
026900861230     C     UMI116        COMP      'S'                                    95    *           A
027000861230     C   95              MOVE      AMPUMS        UMIV                           *STOCCAGGIO
027100861230     C   95              Z-ADD     AMPFCS        FCV                      96    *
027200861230     C     UMI116        COMP      'V'                                    95    *           A
027300861230     C   95              MOVE      AMPUMV        UMIV                           *VENDITA
027400861230     C   95              Z-ADD     AMPFCV        FCV                      96    *
027500861230     C  N05              MOVE      UMIV          UMIT                           *
027600861230     C                   SETON                                            05    *
027700861230     C*                    END                             *
027800850111     C*---------- IMPOSTO ENTRATE/USCITE :                 *
027900850111     C  N96MMPQTA        DIV(H)    FCV           PIPPO            11 2          *
028000850111     C     AZ(15)        IFEQ      '+'                                          *ENTRATE
028100850111     C                   Z-ADD     PIPPO         ENTRAT                         *
028200850111     C                   Z-ADD     0             USCITE                         *
028300850111     C     TOTDEN        ADD       PIPPO         TOTDEN                         *TOT.ENTRATE
028400850111     C     SALDO         ADD       PIPPO         SALDO                          *
028500850111     C     SALDOD        ADD       PIPPO         SALDOD                         *
028600850111     C                   END                                                    *
028700850111     C     AZ(15)        IFEQ      '-'                                          *USCITE
028800850111     C                   Z-ADD     PIPPO         USCITE                         *
028900850111     C                   Z-ADD     0             ENTRAT                         *
029000850111     C     TOTDUS        ADD       PIPPO         TOTDUS                         *TOT.USCITE
029100850111     C     SALDO         SUB       PIPPO         SALDO                          *
029200850111     C     SALDOD        SUB       PIPPO         SALDOD                         *
029300850111     C                   END                                                    *
029400850111     C     UMIV          IFNE      UMIT                                         *
029500850111     C                   MOVE      '*?*'         UMIT                           *
029600850111     C                   SETON                                            97    *NO TOTALI
029700850111     C                   END                                                    *
029800850111     C*---------- INVERSIONE DATE :                        *
029900990122     c                   if        mmpdmv > 0
030000990122     C                   move      MMPDMV        DATAiso                        *DATA MOVIM.
030100990122     c                   move      dataiso       dataeur
030200990122     c                   move      dataeur       datdmv
030300990122     c                   end
030400990122     c                   if        mmpddc > 0
030500990122     C                   move      MMPDDC        DATAiso                        *DATA DOCUM.
030600990122     c                   move      dataiso       dataeur
030700990122     c                   move      dataeur       datddc
030800990122     c                   end
030900850111     C*---------- EMISSIONE SUBFILE -----------------------*
031000850111     C                   Z-ADD     MMPNMV        NUMNMV                         *
031100850111     C*---------- DECODIFICA CAUSALE :                     *
031200850111     C     MMPCAU        IFNE      OLDCAU                                       *
031300850111     C                   MOVE      '152'         TABGRU                         *
031400850111     C                   MOVE      *ZEROS        TABCOD                         *
031500850111     C                   MOVEL     'MP'          CINQUE                         *
031600850111     C                   MOVE      MMPCAU        CINQUE                         *
031700850111     C                   MOVE      CINQUE        TABCOD                         *
031800850130     C     KEYTAB        CHAIN     ANTAB00F                           95        *
031900850130     C   95              MOVE      *ALL'*'       DESCAU                         *
032000850130     C  N95              MOVEL     TABDES        DESCAU                         *
032100850111     C                   MOVE      MMPCAU        OLDCAU                         *
032200850111     C                   END                                                    *
032300850111     C*---------- SCRITTURA SUBFILE :                      *
032400850111     C     REC1          ADD       1             REC1                 71        *
032500850111     C                   WRITE     SFL1                                         *
032600850111     C                   GOTO      RICMOV                                       *
032700850111     C*---------- EMISSIONE SUBFILE :                      *
032800850109     C     EMISSF        TAG                                                    *
032900990409     c                   do        *hival
033000850111     C                   SETON                                            10    *
033100850111     C                   WRITE     MPH116TS                                     *
033200850109     C                   EXFMT     SFL1CTL                                      *
033300990122     C   Kl              GOTO      INIZIO                                       *
033400850111     C*---------- LANCIO STAMPA MOVIMENTO :                *
033500990409     C   Kh              DO                                                     *
033600850509     C                   MOVEL     AAA116        AAA119                         *
033700850509     C                   MOVEL     MAG116        MAG119                         *
033800850509     C                   MOVEL     DAL116        DAL119                         *
033900850509     C                   MOVEL     AL116         AL119                          *
034000850509     C                   MOVEL     MAT116        MAT119                         *
034100850509     C                   MOVEL     UMI116        UMI119                         *
034200850509     C                   MOVE      '999'         KCAU                           *
034300990122     C                   MOVEL     CAU116        CAu119                         *
034400990122     C                   MOVE      '99999999'    CDC119                         *
034500850509     C                   MOVE      'D'           STA119                         *
034600850509     C                   MOVEL     MOV116        MOV119                         *
034700850614     C                   MOVEL     KPJBU         KPJBW           256            *
034800990122     C                   MOVEL     Ph119         KPJBU                          *
034900990317     C                   MOVE      'I204'        KCOAZ                          *
035000900109     C                   MOVEL     'KSDIFFE'     KQEBI                          *
035100850111     C                   CALL      'BCH09'                                      *
035200850111     C                   PARM                    KPJBA                          *
035300850111     C                   CALL      'BCH10'                                      *
035400850111     C                   PARM                    KPJBA                          *
035500850614     C                   MOVEL     KPJBW         KPJBU                          *
035600850111     C                   END                                                    *
035700990409     c                   enddo
035800850109     C                   GOTO      INIZIO                                       *
035900850109     C*----------------------------------------------------*
036000850109     C     FINE          TAG                                                    *
036100850109     C   90              MOVEL     'KPJB207'     KMSMN                          *
036200850109     C                   SETON                                            LR    *
036300850111     C***------------------------------------------------***
036400850111     C**--------- EMISSIONE SALDO DI VERSIONE -----------***
036500850111     C***------------------------------------------------***
036600850111     CSR   RUTSAL        BEGSR                                                  *
036700850111     C                   Z-ADD     0             DATDMV                         *
036800850111     C                   Z-ADD     0             NUMNMV                         *
036900850111     C                   Z-ADD     0             DATDDC                         *
037000850111     C                   MOVEL     ERR(6)        DESCAU                         *
037100850111     C                   MOVE      'KKK'         OLDCAU                         *
037200850111     C                   Z-ADD     0             ENTRAT                         *
037300850111     C                   Z-ADD     0             USCITE                         *
037400850111     C     SALDOD        COMP      0                                  99        *
037500850111     C   99              Z-ADD     SALDOD        ENTRAT                         *
037600850111     C  N99              Z-ADD     SALDOD        USCITE                         *
037700850111     C*---------- SCRITTURA SUBFILE :                      *
037800850111     C                   SETON                                            99    *
037900850111     C     REC1          ADD       1             REC1                 71        *
038000850111     C                   WRITE     SFL1                                         *
038100850111     C                   SETOFF                                           99    *
038200850111     C                   Z-ADD     0             SALDOD                         *
038300850111     CSR                 ENDSR                                                  *
038400990122     C*----------------------------------------------------*
038500990122     C* Reperimento dati società
038600990122     C*----------------------------------------------------*
038700990122     C     REPSOC        BEGSR
038800990122     C*
038900990122     C                   CALLB     'XSOC'
039000990122     C                   PARM                    TIPXSC            6
039100990122     C                   PARM                    SOCXSC            3
039200990122     C                   PARM                    CDSXSC            9 0
039300990122     C                   PARM                    MODXSC            3
039400990122     C                   PARM      *blanks       RTNXSC            1
039500990122     C                   PARM                    XSOCDS
039600990122     C                   PARM                    KPJBA
039700990122     C*
039800990122     C                   ENDSR
039900850111     C***------------------------------------------------***
040000850111**    ERR                       ERRORI
040100850109*- Cod. Magazzino ERRATO !! -*  Err,1
040200850111*- Date Interrogaz. ERRATE !-*  Err,2
040300850109*- Cod. Articolo  ERRATO !! -*  Err,3
040400000000*- CAUSALE movimenti ERRATA -*  Err,4
040500850111*- Non trovati  movimenti   -*  Err,5
040600850111******* SALDO VERSIONE      -*  Err,6

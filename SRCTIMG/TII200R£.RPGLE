000100990120     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990120     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     F* MPH110R  *-MP/38-*-------------------------------------------*
000400000000     F*------------------*                                           *
000500000000     F*       - IMMISSIONE MOVIMENTI DI MAGAZZINO M.P. -             *
000600000000     F*       ------------------------------------------             *
000700000000     F*                                                   AZN.: H110 *
000800990211     F*         PGM. :                                               *
000900990317     F*                tii102R   - RICERCA CENTRI DI COSTO           *
001000990317     F*                tii104R   - RICERCA ANAGRAF.PRODOTTI          *
001100990317     F*                tii002r   - RICERCA CAUSALI DI MAGAZZINO      *
001200990317     F*                tii108R   - RICERCA CODICI  DI MAGAZZINO      *
001300990317     F*                TII116R   - RICERCA CODICI  LISTINI           *
001400990317     F*                TII116R   - RICERCA CODICI  LISTINI           *
001500990317     F*                TII202R   - VISUALIZZA GIACENZA E DISPONIB.   *
001600990317     F*                TII203R   - VISUALIZZA MOVIMENTI              *
001700850611     F*         AZN. : H119      - STAMPA MOVIMENTI DI MAGAZZINO     *
001800000000     F*                                                              *
001900000000     F*--------------------------------------------------------------*
002000000000     F*    IND.  01 - PRESENZA MAGAZZINO DI SCARICO                  *
002100871203     F*     "    02 - PRESENZA COMMESSA
002200000000     F*     "    03 - MOVIMENTO A QUANTITA'                          *
002300000000     F*     "    04 - MOVIMENTO A VALORE                             *
002400000000     F*     "    05 - PRESENZA C/COSTO SCARICO                       *
002500000000     F*     "    07 - PRESENZA LOTTO / COMMESSA DI SCARICO           *
002600000000     F*--------------------------------------------------------------*
002700990317     FTII200D   CF   E             WORKSTN
002800000000     F                                     SFILE(SFL1:REC1)
002900990120     Ftmamp01L  UF   E           K DISK
003000990120     Ftmlmp01L  IF   E           K DISK
003100990120     Ftmmmp00F  UF A E             DISK
003200000000     F                                     INFDS(INF1)
003300990120     Ftmsmp01L  UF A E           K DISK
003400000000     FANTAB00F  IF   E           K DISK
003500010725     FTNTBE01l  IF   E           k disk
003600990120     Fanent01L  IF   E           K DISK
003700000000     D AZ              S              1    DIM(15)                              *AZIONI CAUS.
003800850403     D FO              S              1    DIM(10)                              *FLAG OPERATIVI
003900910712     D ERR             S             30    DIM(31) CTDATA PERRCD(1)             *ERRORI
004000000000     D*--------------------------------------------------------------*
004100000000     D*
004200000000     D INF1            DS           502
004300990120     D*-----INFORMATION DS X FILE  tmmmp00F :
004400000000     D  NREL1                397    400B 0
004500000000     D MPHDT1          DS           512
004600000000     D*-----DATA AREA NUMERATORE MOVIM. MAGAZZINO
004700871203     D  NUMDTA                 1      5  0
004800000000     D  FILL1                  6      8
004900000000     D  DATDTA                 9     14  0
005000000000     D  FILL2                 15     17
005100000000     D  ANNDTA                18     19  0
005200850611     D  FIL03                 20     50
005300850611     D  DGIO                  51     56  0
005400850611     D  FIL04                 57    100
005500850611     D  FILL5                101    300
005600850611     D  GIOTOL               301    302  0
005700850611     D  FILL6                303    512
005800000000     D TB152         E DS                  EXTNAME(ANTAB152)
005900000000     D  AZE                   30     34
006000000000     D                                     DIM(5)                               *ENTRATE
006100000000     D  AZU                   39     43
006200000000     D                                     DIM(5)                               *USCITE
006300000000     D  AZG                   48     52
006400000000     D                                     DIM(5)                               *GESTIONALI
006500871201     D  MOVCOM                70     70
006600910311     D  FLGACC                74     74
006700871201     D* FLAG COMMESSA
006800000000     D TB153         E DS                  EXTNAME(ANTAB153)
006900990120     D RECLIS        E DS                  EXTNAME(tmlmp00F)
007000990211     D  PRE                   29     68P 5
007100000000     D                                     DIM(5)                               *PREZZI LISTINO
007200990120     D RECSAL        E DS                  EXTNAME(tmsmp00F)
007300990211     D  QTK                   86    145P 2
007400000000     D                                     DIM(10)                              *Q.TA'  SALDI
007500910712     D* FLAG PER CATEGORIA FISCALE MATERIALE DI INSERIMENTO
007600910712     D TB155           DS
007700910712     D  CAMPO                  1     74
007800910712     D  FLGATT                75     75
007900000000     D                 DS
008000000000     D*----- ARTICOLO VERSIONE :
008100871014     D  MMPMAT                 1     12
008200871014     D  ARTIC                  1     12
008300000000     D PH115           DS
008400000000     D*----- PASSAGGIO PARAMETRI X VISUALIZZ. GIACENZA :
008500990121     D  AAA115                 1      4  0
008600990120     D  MAG115                 5     11
008700990120     D  MAT115                12     23
008800990120     D  UMI115                24     24
008900990120     D  SCE115                25     25
009000000000     D PH116           DS
009100000000     D*----- PASSAGGIO PARAMETRI X VISUALIZZ. MOVIMENTO :
009200990120     D  AAA116                 1      4  0
009300990120     D  MAG116                 5     11
009400990120     D  UMI116                21     21
009500990122     D  CAU116                22     24
009600990122     D  DAL116                25     32  0
009700990122     D  AL116                 33     40  0
009800990122     D  MOV116                41     45  0
009900990122     D  MAT116                46     57
010000850611     D PH119           DS
010100850611     D*----- PASSAGGIO PARAMETRI X STAMPA MOVIMENTI :
010200990120     D  AAA119                 1      4  0
010300990120     D  MAG119                 5     11
010400990120     D  DAL119                12     19  0
010500990120     D  AL119                 20     27  0
010600850611     D  UMI119                31     31
010700990120     D  CAU119                32     46
010800990120     D  CDC119                47     54
010900990120     D  STA119                55     55
011000990120     D  MOV119                56     60  0
011100990120     D  MAT119                61     72
011200000000     D KPJBA         E DS
011300000000     D*----- PASSAGGIO PARAMETRI
011400000000     D  MSGERR               453    502
011500871201     D OFNDT2          DS           512
011600871201     D* ------ CODICE INDIVIDUALE RIGA MOVIMENTI
011700871202     D  MOVIND                 1      7  0
011800871202     D  DT2BLK                 8    256
011900871201     D  DT2BL2               257    512
012000990120     D soc001        E DS                  EXTNAME(xsoc001ds)
012100990120     D xsocds          DS          1000
012200010725    >D DGED          E DS
012300990120     d dataiso         s               d   datfmt(*iso)
012400990120     d dataeur         s               d   datfmt(*eur)
012500000000     C*----------------------------------------------------*
012600000000     C     *ENTRY        PLIST                                                  *
012700000000     C                   PARM                    KPJBA                          *
012800990120     C*---------- RICERCA DITTA :
012900990120     C                   MOVEL     'SOC001'      TIPXSC
013000990120     C                   MOVEL     *blank        SOCXSC
013100990120     C                   EXSR      REPSOC
013200990120     C     RTNXSC        IFNE      '1'
013300990120     C                   MOVEL     XSOCDS        SOC001
013400990120     C                   MOVEL     xscrgs        desdit
013500990120     C                   MOVEL     xscsoc        wsoc              3
013600990311     c                   move      xscdiv        valf
013700990120     c                   end
013800990120     C     *DTAARA       DEFINE                  MPHDT1                         *
013900990120     C     *DTAARA       DEFINE                  DTAUCH            6 0          *
014000990120     C     *DTAARA       DEFINE                  OFNDT2                         *
014100990120     c                   time                    como14           14 0
014200990120     c                   move      como14        woggi             8 0
014300990120     c                   move      woggi         dataeur
014400990120     c                   move      dataeur       dataiso
014500990120     c                   move      dataiso       woggig            8 0
014600871014     C*                                                    *
014700871014     C                   MOVEL     '99999999'    NOVE             12
014800861212     C                   MOVE      '99999999'    NOVE
014900871014     C                   MOVEL     '?'           INTERR           12
015000871201     C                   Z-ADD     0             MOVIND
015100010725     C*--- Reperisco la divisa corrente ------------------------------*
015200010725     C     ktbe          KLIST
015300010725     C                   KFLD                    kcod
015400010725     C                   KFLD                    kke1
015500010725     C     *like         Define    tbecod        kcod
015600010725     C     *like         Define    tbeke1        kke1
015700010725     C                   move      'GED'         kcod
015800010725     C                   move      *blanks       kke1
015900010725     C                   movel     'MAGAZZINO'   kke1
016000010725     c                   clear                   dged
016100010725     C     ktbe          CHAIN     tntbe01l                           54
016200010725     C  n54              movel     tbeuni        dged
016300010725     C  n54              movel     §GEDCN        VALF
016400000000     C*---------- ANAGRAFICO MATERIE PRIME :               *
016500000000     C     KEYAMP        KLIST                                                  *
016600000000     C                   KFLD                    ARTF                           *
016700000000     C*---------- ANAGRAFICO LISTINI MAT.PRIME :           *
016800000000     C     KEYLMP        KLIST                                                  *
016900000000     C                   KFLD                    LISF                           *
017000000000     C                   KFLD                    ARTF                           *
017100990311     C                   KFLD                    valf                           *
017200000000     C*---------- SALDI DI MAGAZZINO :                     *
017300000000     C     KEYSMP        KLIST                                                  *
017400000000     C                   KFLD                    ANNO                           *
017500000000     C                   KFLD                    MAGF                           *
017600000000     C                   KFLD                    ARTF                           *
017700000000     C*---------- ANAGRAFICO TABELLE :                     *
017800000000     C     KEYTAB        KLIST                                                  *
017900000000     C                   KFLD                    TABGRU                         *
018000860702     C                   KFLD                    TABCOD                         *
018100860702RIC  C******************************************************
018200860702RIC  C*---------- ANAGRAFICO CENTRI DI COSTO :             *
018300860702RIC  C     KEYCDC        KLIST                                                  *
018400990120RIC  C                   KFLD                    wsoc                           *
018500990120     c                   kfld                    wtpana            3
018600990120RIC  C                   KFLD                    CODCDC            8            *
018700990120     c                   move      'CDC'         wtpana
018800000000     C                   MOVEL     KPJBU         KPJBW           256            *
018900850611     C*----------------------------------------------------*
019000871214     C                   MOVEL     'N'           TIPATC                          ATT.COSTI
019100850403     C     INIZIO        TAG                                                    *
019200850403     C                   MOVEA     *ZEROS        *IN(1)                         *
019300850403     C     *LOCK         IN        MPHDT1                               90      *
019400871201     C   90              GOTO      INIZIO                                       *
019500850403     C                   OUT       MPHDT1                                       *
019600871201     C** DATA AREA DATA CONGELAMENTO DATI COMMESSA
019700871201     C     SUUCH         TAG
019800990120     c                   in        dtauch
019900850611     C*---------- GIRO DATA GIORNALE :                     *
020000990120     c                   if        dgio > 0
020100990120     c     *dmy          move      dgio          dataeur
020200990120     c                   move      dataeur       dgiov
020300990120     c                   move      dataeur       dataiso
020400990120     c                   move      dataiso       datgio            8 0
020500990120     c                   end
020600850611     C*----------------------------------------------------*
020700990308     C                   z-add     ANNDTA        ANNO                           *
020800990120     c                   if        anno > 40
020900990120     c                   add       1900          anno
021000990120     c                   else
021100990120     c                   add       2000          anno
021200990120     c                   end
021300860318     C                   MOVE      'N'           SNSTA                          *
021400990120     C                   MOVE      woggi         DMVDTA                         *
021500000000     C                   MOVE      *BLANKS       ERRORE                         *
021600000000     C     MAGSCA        COMP      *BLANKS                                09    *
021700871201     C   09              MOVE      '1      '     MAGSCA                         *
021800000000     C     CAUSCA        COMP      *BLANKS                                09    *
021900990120     C   09              MOVE      '?  '         CAUSCA                         *
022000000000     C     CDCSCA        COMP      *BLANKS                                09    *
022100990120     C   09              MOVE      '99999999'    CDCSCA                         *
022200000000     C     TIPUMI        COMP      *BLANKS                                09    *
022300000000     C   09              MOVE      'A'           TIPUMI                         *
022400000000     C     LISPT         COMP      *BLANKS                                09    *
022500871120     C   09              MOVE      '1  '         LISPT                          *
022600871120     C   09              MOVE      '1'           COLPT                          *
022700000000     C                   MOVE      *ZEROS        NUMDOC                         *
022800990120     C                   MOVE      woggi         DATDOC                         *
022900871201     C                   MOVE      ' '           FLGSCA            1            *SCARICO
023000850403     C                   MOVE      *BLANKS       OPESCA           10            *
023100850611     C*----------------------------------------------------*
023200000000     C     EMISPT        TAG                                                    *
023300000000     C                   SETOFF                                       0102      *
023400000000     C                   WRITE     MPH110TS                                     *
023500000000     C                   EXFMT     MPH110PT                                     *
023600990409     C   Kc              GOTO      FINE                                         *
023700000000     C*------------                                        *
023800990120     C                   MOVEA     *ZEROS        *IN(12)                        *
023900000000     C                   MOVE      *BLANKS       ERRORE                         *
024000850611     C*---------- CONTROLLO CAMBIO DATA -------------------*
024100850611     C   KD              MOVEL     DMVDTA        GIORNO            2 0          *
024200871228     C   KDGIORNO        COMP      GIOTOL                             90        *
024300850611     C   KD
024400850611     CAN 90              MOVEL     ERR(16)       ERRORE                         *
024500850611     C   KD
024600850611     CAN 90              GOTO      EMISPT                                       *
024700850611     C*---------- CAMBIO DATA MOVIMENTI -------------------*
024800850403     C   KD              SETON                                            25    *
024900850403     C   KD              GOTO      EMISPT                                       *
025000990120     C                   MOVE      DMVDTA        dataeur                        *
025100990120     c                   move      dataeur       dataiso
025200990120     C                   move      dataiso       XDTamg            8 0          *
025300990120     C     XDTAMG        COMP      DATGIO                               9090    *
025400850403     C   90              MOVEL     ERR(15)       ERRORE                         *
025500850403     C   90              SETON                                        2625      *
025600850403     C   90              GOTO      EMISPT                                       *
025700850403     C*---------- CONTROLLO ANNO --------------------------*
025800990120     c                   z-add     anndta        anndta4           4 0
025900990120     c                   if        anndta4 > 40
026000990120     c                   add       1900          anndta4
026100990120     c                   else
026200990120     c                   add       2000          anndta4
026300990120     c                   end
026400990120     C     ANNDTA4       SUB       ANNO          UNO               2 09192      *
026500850403     C   91UNO           COMP      1                                  9090      *
026600850403     C   92UNO           COMP      -1                                 9090      *
026700850403     C   90              MOVEL     ERR(1)        ERRORE                         *
026800850403     C   90              SETON                                          10      *
026900850403     C   90              GOTO      EMISPT                                       *
027000850403     C                   MOVE      ANNO          AAA115                         *
027100850403     C                   MOVE      ANNO          AAA116                         *
027200850611     C                   MOVE      ANNO          AAA119                         *
027300990120     C                   move      DMVDTA        dataeur
027400990120     C                   MOVE      dataeur       dataiso
027500990120     C                   MOVE      dataiso       AMGDTM            8 0
027600871201     C** DATA MOVIMENTO A-M-G
027700000000     C*--------- MAGAZZINO DI SCARICO----------------------*
027800000000     C     MAGSCA        IFEQ      '?      '                                    *
027900000000     C                   MOVE      *BLANKS       MAGSCA                         *
028000000000     C                   MOVE      *BLANKS       DAT153                         *
028100990317     C                   CALL      'TII108R'                                    *
028200000000     C                   PARM                    MAGSCA                         *
028300000000     C                   PARM                    DAT153           75            *
028400850611     C                   SETON                                            11    *
028500000000     C                   GOTO      EMISPT                                       *
028600000000     C                   END                                                    *
028700000000     C                   MOVE      '153'         TABGRU                         *
028800000000     C                   MOVE      *ZEROS        TABCOD                         *
028900000000     C                   MOVE      MAGSCA        TABCOD                         *
029000000000     C     KEYTAB        CHAIN     ANTAB00F                           90        *
029100000000     C   90              MOVEL     ERR(2)        ERRORE                         *
029200000000     C   90              SETON                                        11        *
029300000000     C   90              GOTO      EMISPT                                       *
029400000000     C                   MOVEL     TABDES        TB153                          *
029500000000     C                   MOVEL     DES153        DESSCA                         *
029600000000     C                   MOVE      TIG153        QVMSCA                         *
029700000000     C                   MOVE      TIM153        TIPSCA                         *
029800000000     C                   MOVE      SNC153        SNCSCA            1            *
029900000000     C     SNC153        IFEQ      'S'                                          *
030000000000     C                   SETON                                            05    *C/COSTO
030100000000     C     CDCSCA        COMP      *BLANKS                                95    *
030200990120     C  N95CDCSCA        COMP      '00000000'                             95    *
030300990120     C  N95CDCSCA        COMP      '99999999'                             95    *
030400990318     C   95              MOVEl     CDC153        CDCSCA                         *
030500000000     C                   ELSE                                                   *
030600990120     C                   MOVE      '99999999'    CDCSCA                         *
030700000000     C                   END                                                    *
030800871120     C     SNL153        COMP      'N'                                    07    *LOT/COMM.
030900000000     C                   SETON                                            01    *
031000000000     C*---------- CAUSALE DI SCARICO ----------------------*
031100990120     C     CAUSCA        IFEQ      '?  '                                        *
031200000000     C                   MOVE      'MP'          PROCAU            2            *
031300000000     C                   MOVE      *BLANKS       CAUSCA                         *
031400000000     C                   MOVE      *BLANKS       DECSCA                         *
031500000000     C                   MOVE      *BLANKS       AZICAU           15            *
031600990317     C                   CALL      'TII002R'                                    *
031700000000     C                   PARM                    KPJBA                          *
031800000000     C                   PARM                    PROCAU                         *
031900000000     C                   PARM                    CAUSCA                         *
032000000000     C                   PARM                    DECSCA                         *
032100000000     C                   PARM                    AZICAU                         *
032200850611     C                   SETON                                            12    *
032300000000     C                   GOTO      EMISPT                                       *
032400000000     C                   END                                                    *
032500000000     C                   MOVE      '152'         TABGRU                         *
032600000000     C                   MOVE      *ZEROS        TABCOD                         *
032700990120     C                   MOVEL     'MP'          CINQUE            5            *
032800990120     C                   MOVE      CAUSCA        CINQUE            5            *
032900990120     C                   MOVE      CINQUE        TABCOD                         *
033000000000     C     KEYTAB        CHAIN     ANTAB00F                           90        *
033100000000     C   90              MOVEL     ERR(3)        ERRORE                         *
033200000000     C   90              SETON                                        12        *
033300000000     C   90              GOTO      EMISPT                                       *
033400000000     C                   MOVEL     TABDES        TB152                          *
033500000000     C                   MOVEL     DES152        DECSCA                         *
033600000000     C                   MOVEA     AZE           AZ(1)                          *
033700000000     C                   MOVEA     AZU           AZ(6)                          *
033800000000     C                   MOVEA     AZG           AZ(11)                         *
033900000000     C                   MOVEA     AZ            AZS              15            *
034000910311     C* CONTROLLO SUL PROFILO PER ACCESSO CAUSALI **********
034100940209     C                   MOVEL     KNMUS         PROF              6
034200940209     C     PROF          COMP      'CEP000'                           5151
034300910311     C     FLGACC        COMP      '1'                                    50
034400910311     C   50
034500910311     CAN 51              DO
034600910311     C                   MOVEL     ERR(29)       ERRORE
034700910311     C                   SETON                                        90
034800910311     C                   END
034900910311     C   90              GOTO      EMISPT
035000910311     C*
035100871201     C     AZ(15)        COMP      '*'                                    21
035200871201     C** INVENTARIO
035300871201     C     AZ(15)        IFEQ      '+'
035400871201     C                   MOVEL     'C'           FLGSCA
035500871201     C                   ELSE
035600871201     C                   MOVEL     'S'           FLGSCA
035700871201     C                   END
035800871201     C** CARICO O SCARICO
035900850403     C                   MOVE      OPE152        OPESCA                         *
036000000000     C*---------- C/COSTO DI SCARICO ----------------------*
036100000000     C   05              DO                                                     *
036200990120     C     CDCSCA        COMP      '00000000'                             95    *
036300990120     C  N95CDCSCA        COMP      '99999999'                             95    *
036400000000     C  N95CDCSCA        COMP      *BLANKS                                95    *
036500990120     C   95              MOVE      '?       '    CDCSCA                         *
036600990120     C     CDCSCA        IFEQ      '?       '                                   *
036700000000     C                   MOVE      *BLANKS       CDCSCA                         *
036800000000     C                   MOVE      *ZEROS        CODCDC                         *
036900000000     C                   MOVE      *BLANKS       DESCDC           48            *
037000990317     C                   CALL      'TII102R'                                    *
037100990121     c                   parm                    wsoc
037200000000     C                   PARM                    CODCDC                         *
037300000000     C                   PARM                    DESCDC                         *
037400000000     C                   MOVE      CODCDC        CDCSCA                         *
037500850611     C                   SETON                                            13    *
037600000000     C                   GOTO      EMISPT                                       *
037700000000     C                   END                                                    *
037800990120     C                   END                                                    *
037900860702RIC  C******************************************************
038000871206     C**
038100860702RIC  C                   MOVE      CDCSCA        CODCDC                         *
038200990120RIC  C     KEYCDC        CHAIN     anent01l                           90        *
038300000000     C   90              MOVEL     ERR(4)        ERRORE                         *
038400000000     C   90              SETON                                        13        *
038500000000     C   90              GOTO      EMISPT                                       *
038600000000     C*---------- LISTINO PREZZI --------------------------*
038700871201     C     LISPT         COMP      *BLANKS                                90
038800871201     C   90              MOVEL     ERR(9)        ERRORE
038900871201     C   90              SETON                                        19
039000871201     C   90              GOTO      EMISPT
039100871201     C** LISTINO ERRATO
039200871201     C     COLPT         COMP      0                                      19
039300871201     C   19              SETON                                        90
039400871201     C   90              MOVEL     ERR(9)        ERRORE
039500871201     C   90              GOTO      EMISPT
039600871201     C** COLONNA LISTINO ERRATA
039700000000     C*----------                                          *
039800000000     C     LISPT         IFNE      '999'                                        *
039900000000     C                   MOVE      LISPT         LIS               3            *
040000000000     C                   EXSR      RUTLIS                                       *
040100000000     C  N90              MOVE      LIS           LISPT                          *
040200000000     C  N90COLPT         COMP      0                                      90    *
040300000000     C   90              MOVEL     ERR(6)        ERRORE                         *
040400000000     C   90              SETON                                        19        *
040500000000     C   90              GOTO      EMISPT                                       *
040600000000     C                   END                                                    *
040700000000     C*---------- NUMERO E DATA DOCUMENTO -----------------*
040800000000     C     NUMDOC        COMP      0                                      90    *
040900000000     C  N90DATDOC        COMP      0                                      90    *
041000000000     C                   Z-ADD     NUMDOC        NDOCF                          *
041100000000     C                   Z-ADD     DATDOC        DDOCF                          *
041200000000     C*---------- RICERCA NUMERO MOVIMENTO ----------------*
041300000000     C     SULOCK        TAG                                                    *
041400000000     C     *LOCK         IN        MPHDT1                               90      *
041500000000     C   90              GOTO      SULOCK                                       *
041600850516     C     NUMDTA        ADD       1             NUMDTA                   41    *
041700850516     C   41NUMDTA        ADD       1             NUMDTA                         *
041800990120     C                   MOVE      woggi         DATDTA                         *
041900000000     C                   OUT       MPHDT1                                       *
042000000000     C                   Z-ADD     NUMDTA        NUMMOV                         *
042100850611     C                   MOVE      NUMDTA        MOV116                         *VISUAL.MOV.
042200850403     C                   MOVE      DMVDTA        DAL116                         *
042300850403     C                   MOVE      DMVDTA        AL116                          *
042400850611     C                   MOVE      NUMDTA        MOV119                         *STAMPA MOV.
042500850611     C                   MOVE      DMVDTA        DAL119                         *
042600850611     C                   MOVE      DMVDTA        AL119                          *
042700000000     C*---------- ROVESCIO DATA MOVIMENTO :                *
042800990120     C                   move      DMVDTA        DATAeur                        *
042900990120     C                   move      dataeur       dataiso                        *
043000990120     C                   move      DATAIso       DATMOV            8 0          *
043100000000     C*---------- PULIZIA SUBFILE :                        *
043200000000     C                   SETON                                            70    *PULIZIA
043300000000     C                   WRITE     SFL1CTL                                      *
043400000000     C                   MOVEA     *ZEROS        *IN(10)                        *
043500000000     C                   WRITE     MPH110TS                                     *TESTATA
043600000000     C*---------- EMISSIONE FORMATO -----------------------*
043700000000     C                   Z-ADD     0             REC1              5 0          *
043800000000     C                   Z-ADD     0             RECT              5 0          *
043900000000     C                   Z-ADD     1             CURSOR                         *
044000000000     C     SUFOND        TAG                                                    *
044100000000     C                   SETOFF                                           80    *
044200000000     C                   EXSR      CARFON                                       *
044300871205     C     TIPATC        IFEQ      'N'
044400871205     C                   WRITE     MPH110TS
044500871205     C                   END
044600871205     C** SE EMESSO 3 VIDEATA RIEMETTO LA 1
044700000000     C*---------                                           *
044800000000     C     EMISF1        TAG                                                    *
044900910715     C                   WRITE     MPH110TS                                     *PROVA
045000000000     C                   SETON                                        818283    *
045100000000     C                   SETON                                        848586    *
045200000000     C                   EXFMT     SFL1CTL                                      *
045300000000     C                   MOVEA     *ZEROS        *IN(80)                        *
045400000000     C                   MOVE      *BLANKS       ERRORE                         *
045500850611     C*---------- EMISSIONE STAMPA :                       *
045600990120     C   Kl
045700850611     CAN 28SNSTA         IFEQ      'S'                                          *
045800850611     C                   MOVE      ANNO          AAA119                         *
045900861212     C                   MOVE      NOVE          MAT119                         *
046000850611     C                   MOVEL     TIPUMI        UMI119                         *
046100850611     C                   MOVE      *ALL'9'       CAU119                         *
046200990120     C                   MOVE      '99999999'    CDC119                         *
046300850611     C                   MOVE      'D'           STA119                         *
046400990317     C                   MOVE      'I204'        KCOAZ                          *
046500850614     C                   MOVE      'H'           KJOST                          *CONGELATA
046600850614     C                   MOVEL     'KSNOTTE'     KQEBI                          *
046700850611     C                   MOVEL     MAGSCA        MAG119                         *
046800850611     C                   MOVEL     PH119         KPJBU                          *
046900850611     C                   CALL      'BCH10'                                      *
047000850611     C                   PARM                    KPJBA                          *
047100850611     C                   END                                                    *
047200990120     C   Kl              GOTO      INIZIO                                       *
047300000000     C   KA              GOTO      SUFOND                                       *
047400000000     C*--------- LETTURA RIGA DA SUBFILE ------------------*
047500000000     C   71              READC     SFL1                                   31    *
047600000000     C   71
047700000000     CANN31              DO                                                     *
047800000000     C     SCELTA        COMP      'A'                                    31    *
047900000000     C  N31SCELTA        COMP      'G'                                    31    *
048000000000     C  N31SCELTA        COMP      'D'                                    31    *
048100000000     C  N31SCELTA        COMP      'M'                                    31    *
048200000000     C  N31SCELTA        CABNE     'V'           EMISF1                   31    *
048300000000     C                   Z-ADD     RELVID        REC1                           *
048400000000     C     REC1          CHAIN     SFL1                               31        *SFL
048500000000     C   31              GOTO      SUFOND                                       *
048600000000     C*--------- GIA ANNULLATO :                           *
048700000000     C     SCELTW        IFEQ      'A'                                          *
048800000000     C                   MOVE      'A'           SCELTA                         *
048900000000     C                   UPDATE    SFL1                                         *
049000000000     C                   GOTO      SUFOND                                       *
049100000000     C                   END                                                    *
049200000000     C*--------- ANNULLAMENTO SCELTA :                     *
049300000000     C   KA              MOVE      ' '           SCELTA                         *
049400000000     C   KA              UPDATE    SFL1                                         *
049500000000     C   KA              GOTO      SUFOND                                       *
049600000000     C*--------- CARICO DATI SUL FONDO---------------------*
049700000000     C     SCELTA        CABEQ     'G'           GIATAG                         *
049800000000     C     SCELTA        CABEQ     'D'           GIATAG                         *
049900000000     C     SCELTA        CABEQ     'M'           MOVTAG                         *
050000000000     C                   SETON                                        80  32    *
050100000000     C                   UPDATE    SFL1                                         *
050200000000     C                   SETOFF                                           32    *
050300910712     C                   SETON                                            40    *
050400000000     C                   EXSR      CARFON                                       *
050500000000     C                   GOTO      EMISF1                                       *
050600000000     C*--------- RICHIAMO GIACENZA/DISPONIBILITA'----------*
050700000000     C     GIATAG        TAG                                                    *
050800000000     C                   MOVE      MMPMAG        MAG115                         *
050900000000     C                   MOVE      MMPMAT        MAT115                         *
051000000000     C                   MOVE      TIPUMI        UMI115                         *
051100000000     C                   MOVE      SCELTA        SCE115                         *
051200000000     C                   MOVEL     PH115         KPJBU                          *
051300990317     C                   CALL      'TII202R'                                    *
051400000000     C                   PARM                    KPJBA                          *
051500000000     C                   MOVE      ' '           SCELTA                         *
051600000000     C                   UPDATE    SFL1                                         *
051700000000     C                   GOTO      SUFOND                                       *
051800000000     C*--------- RICHIAMO MOVIMENTI -----------------------*
051900000000     C     MOVTAG        TAG                                                    *
052000000000     C                   MOVE      MMPMAG        MAG116                         *
052100000000     C                   MOVE      MMPMAT        MAT116                         *
052200000000     C                   MOVE      TIPUMI        UMI116                         *
052300000000     C                   MOVE      MMPCAU        CAU116                         *
052400000000     C                   MOVEL     PH116         KPJBU                          *
052500990317     C                   CALL      'TII203R'                                    *
052600000000     C                   PARM                    KPJBA                          *
052700000000     C                   MOVE      ' '           SCELTA                         *
052800000000     C                   UPDATE    SFL1                                         *
052900000000     C                   GOTO      SUFOND                                       *
053000000000     C                   END                                                    *
053100000000     C*--------- CREAZIONE RIGA NUOVA ---------------------*
053200000000     C     RELVF         IFEQ      0                                            *
053300910712     C                   SETOFF                                           40    *
053400000000     C*---------- PRODOTTO :                               *
053500861212     C     ARTF          IFEQ      INTERR                                       *
053600000000     C                   MOVE      *BLANKS       ARTF                           *
053700000000     C                   MOVEL     DESF          DESPRO           30            *
053800990317     C                   CALL      'TII104R'                                    *
053900000000     C                   PARM                    ARTF                           *
054000000000     C                   PARM                    DESPRO                         *
054100000000     C                   MOVEL     DESPRO        DESF                           *
054200990120     C     KEYAMP        CHAIN     tmamp01L                           90        *
054300000000     C   90              SETON                                        96        *
054400000000     C   90              MOVEL     ERR(10)       ERRORE                         *
054500000000     C   90              GOTO      EMISF1                                       *
054600000000     C                   MOVE      AMPUMA        UMAF                           *
054700000000     C                   MOVEL     AMPDMT        DESF                           *
054800000000     C     TIPUMI        COMP      'A'                                    24    *UNITA MISURA
054900000000     C   24              MOVE      AMPUMA        UMSF                           *ANAGRAFICA
055000000000     C     TIPUMI        COMP      'Q'                                    24    *           A
055100000000     C   24              MOVE      AMPUMQ        UMSF                           *ACQUISTO
055200000000     C     TIPUMI        COMP      'S'                                    24    *           A
055300000000     C   24              MOVE      AMPUMS        UMSF                           *STOCCAGGIO
055400000000     C     TIPUMI        COMP      'V'                                    24    *           A
055500000000     C   24              MOVE      AMPUMV        UMSF                           *VENDITA
055600000000     C                   SETON                                        8097      *
055700910712     C*                                                    *
055800000000     C                   GOTO      EMISF1                                       *
055900000000     C                   ELSE                                                   *
056000990120     C     KEYAMP        CHAIN     tmamp01L                           90        *
056100000000     C   90              SETON                                        96        *
056200000000     C   90              MOVEL     ERR(10)       ERRORE                         *
056300000000     C   90              GOTO      EMISF1                                       *
056400000000     C                   MOVE      AMPUMA        UMAF                           *
056500000000     C                   MOVEL     AMPDMT        DESF                           *
056600000000     C     TIPUMI        COMP      'A'                                    24    *UNITA MISURA
056700000000     C   24              MOVE      AMPUMA        UMSF                           *ANAGRAFICA
056800000000     C   24              Z-ADD     1             FCVF                           *
056900000000     C     TIPUMI        COMP      'Q'                                    24    *           A
057000000000     C   24              MOVE      AMPUMQ        UMSF                           *ACQUISTO
057100000000     C   24              Z-ADD     AMPFCQ        FCVF                           *
057200000000     C     TIPUMI        COMP      'S'                                    24    *           A
057300000000     C   24              MOVE      AMPUMS        UMSF                           *STOCCAGGIO
057400000000     C   24              Z-ADD     AMPFCS        FCVF                           *
057500000000     C     TIPUMI        COMP      'V'                                    24    *           A
057600000000     C   24              MOVE      AMPUMV        UMSF                           *VENDITA
057700000000     C   24              Z-ADD     AMPFCV        FCVF                           *
057800000000     C                   END                                                    *
057900000000     C*--------- CREAZIONE RECORD SFL X SCARICO -----------*
058000871203     C                   DO                                                     *
058100000000     C                   MOVE      SNCSCA        SNCF              1            *
058200000000     C                   Z-ADD     QTAF          QTAW             15 2          *
058300850403     C                   MOVE      OPESCA        OPEF                           *
058400000000     C                   MOVE      AZS           AZF                            *
058500000000     C                   MOVEA     AZF           AZ                             *
058600000000     C                   EXSR      CTLFON                                       *
058700000000     C                   SETOFF                                           22    *
058800000000     C                   EXSR      AGGSAL                                       *
058900000000     C                   EXSR      AGGMOV                                       *
059000000000     C                   EXSR      AGGVID                                       *
059100871201     C                   END                                                    *
059200871204     C                   GOTO      SUFOND
059300871204     C                   END                                                    *
059400000000     C*--------- AGGIORNAMENTO RIGA VECCHIA ---------------*
059500000000     C     RELVF         IFNE      0                                            *
059600000000     C                   EXSR      CTLFON                                       *
059700000000     C*--------- ELIMINAZIONE MOVIMENTO PRECEDENTE :       *
059800000000     C     RELRF         IFNE      0                                            *
059900990120     C     RELRF         CHAIN     tmmmp00F                           22        *
060000871203     C  N22              DO
060100871203     C                   MOVE      'A'           MMPATT                         *
060200871203     C                   EXCEPT    ANNMMP                                       *
060300871203     C                   END
060400871203     C** ANNULLO MOV.MAGAZZINO
060500000000     C                   Z-ADD     0             RELRF                          *
060600991102     C*--------- ESEGUO OPERAZIONE INVERSA :               *
060700991102     C     1             DO        15            X
060800991102     C     AZ(X)         COMP      '+'                                    22
060900991102     C     AZ(X)         COMP      '-'                                    23
061000991102     C   22              MOVE      '-'           AZ(X)
061100991102     C   23              MOVE      '+'           AZ(X)
061200991102     C                   END
061300000000     C                   Z-ADD     QTAAF         QTAW                           *
061400000000     C                   SETOFF                                           22    *
061500000000     C                   EXSR      AGGSAL                                       *
061600000000     C                   END                                                    *
061700000000     C*--------- ESEGUO OPERAZIONE VARIATA :               *
061800000000     C     SCELTA        IFEQ      'V'                                          *
061900000000     C                   MOVEA     AZF           AZ                             *
062000000000     C                   Z-ADD     QTAF          QTAW                           *
062100000000     C                   SETOFF                                           22    *
062200000000     C                   EXSR      AGGSAL                                       *
062300000000     C                   EXSR      AGGMOV                                       *
062400000000     C                   END                                                    *
062500000000     C                   EXSR      AGGVID                                       *
062600000000     C                   END                                                    *
062700000000     C                   GOTO      SUFOND                                       *
062800000000     C*----------------------------------------------------*
062900000000     C     FINE          TAG                                                    *
063000000000     C   90              MOVEL     'KPJB207'     KMSMN                          *
063100000000     C                   SETON                                            LR    *
063200990120     C*----------------------------------------------------*
063300871214     C     RICDIS        BEGSR
063400871215     C                   Z-ADD     0             DISPON           15 2
063500990120     C     KEYSMP        CHAIN     tmsmp01L                           90
063600871215     C   90              GOTO      NODSIP
063700871214     C     SMPQGF        SUB       SMPQIM        DISPON
063800871214     C                   SUB       SMPQIP        DISPON
063900871215     C     NODSIP        ENDSR
064000000000     C**--------- CARICAMENTO DEL FONDO -----------------***
064100000000     C***------------------------------------------------***
064200000000     CSR   CARFON        BEGSR                                                  *
064300000000     C                   SETOFF                                       030405    *
064400000000     C                   SETOFF                                       060708    *
064500000000     C                   SETOFF                                       34        *
064600000000     C*---------- POSIZIONAMENTO RIGA :                    *
064700860818     C                   Z-ADD     RELVID        CURSOR                         *
064800000000     C     CURSOR        DIV       10            CURSOR                         *
064900000000     C                   MVR                     RESTO             5 023        *
065000000000     C     CURSOR        MULT      10            CURSOR                         *
065100000000     C   23CURSOR        ADD       1             CURSOR                         *
065200000000     C*---------- CARICAMENTO DA PROMPT :                  *
065300000000     C  N80              DO                                                     *
065400000000     C                   Z-ADD     0             RELRF                          *
065500000000     C                   Z-ADD     0             RELVF                          *
065600000000     C                   Z-ADD     1             FCVF                           *
065700871203     C                   MOVEL     QVMSCA        QF                             *
065800871203     C                   MOVE      QVMSCA        VF                             *
065900871203     C                   MOVE      TIPSCA        TF                             *TIPO MAGAZZINO
066000871203     C                   MOVE      AZS           AZF                            *
066100000000     C                   MOVE      *BLANKS       UMAF                           *
066200000000     C                   MOVE      *BLANKS       MCPF                           *
066300000000     C                   MOVE      *BLANKS       SCEF                           *
066400871203     C                   MOVE      MAGSCA        MAGF                           *
066500871203     C                   MOVE      FLGSCA        CSF                            *
066600861212     C                   MOVE      INTERR        ARTF                           *
066700000000     C                   MOVE      *BLANKS       DESF                           *
066800871203     C                   MOVE      CAUSCA        CAUF                           *
066900871203     C                   MOVE      OPESCA        OPEF                           *
067000000000     C                   MOVE      *BLANKS       UMSF                           *
067100000000     C                   Z-ADD     0             QTAF                           *
067200871203     C                   MOVE      CDCSCA        CDCF                           *
067300000000     C                   MOVE      LISPT         LISF                           *
067400000000     C                   Z-ADD     COLPT         COLF                           *
067500871202     C   02              MOVEL     LISPT         LISV              3
067600871202     C   02              Z-ADD     COLPT         COLV              1 0
067700871202     C** SALVO LISTINO-COLONNA VENDITA
067800871202     C   02              MOVEL     '5'           COLF
067900871201     C** COMMESSA - LISTINO COSTO
068000000000     C                   Z-ADD     0             PRZF                           *
068100000000     C     NDOCF         IFEQ      0                                            *
068200000000     C                   Z-ADD     NUMDOC        NDOCF                          *
068300000000     C                   Z-ADD     DATDOC        DDOCF                          *
068400000000     C                   END                                                    *
068500000000     C                   END                                                    *
068600000000     C*---------- CARICAMENTO DA SUBFILE :                 *
068700000000     C   80              DO                                                     *
068800000000     C                   Z-ADD     RELREC        RELRF                          *
068900000000     C                   Z-ADD     RELVID        RELVF                          *
069000000000     C                   Z-ADD     FCV           FCVF                           *
069100000000     C                   MOVE      MMPFGQ        QF                             *
069200000000     C                   MOVE      MMPFGV        VF                             *
069300000000     C                   MOVE      MMPFGT        TF                             *
069400000000     C                   MOVE      MMPOPC        AZF                            *
069500000000     C                   MOVEA     AZF           AZ                             *
069600000000     C                   MOVE      MMPUMA        UMAF                           *
069700000000     C                   MOVE      MMPMCP        MCPF                           *
069800000000     C                   MOVE      SCELTA        SCEF                           *
069900000000     C                   MOVE      MMPMAG        MAGF                           *
070000000000     C                   MOVE      FLGCS         CSF                            *
070100000000     C                   MOVE      MMPMAT        ARTF                           *
070200000000     C                   MOVEL     DESART        DESF                           *
070300000000     C                   MOVE      MMPCAU        CAUF                           *
070400850403     C                   MOVE      OPESCA        OPEF                           *
070500000000     C                   MOVE      UMI           UMSF                           *
070600000000     C                   Z-ADD     QTAV          QTAF                           *
070700000000     C                   Z-ADD     QTAF          QTAAF                          *
070800000000     C                   MOVE      MMPCDC        CDCF                           *
070900000000     C                   SETON                                            84    *
071000000000     C                   MOVE      LISCOD        LISF                           *
071100000000     C                   Z-ADD     LISCOL        COLF                           *
071200000000     C                   Z-ADD     PRZV          PRZF                           *
071300000000     C                   Z-ADD     MMPNDC        NDOCF                          *
071400990120     C                   move      MMPDDC        dataiso                        *
071500990120     C                   move      dataiso       dataeur                        *
071600990120     C                   move      dataiso       XDTAMG                         *
071700990120     C                   move      dataeur       XDTGMA            8 0          *
071800000000     C                   Z-ADD     XDTGMA        DDOCF                          *
071900000000     C                   END                                                    *
072000860702     C***------------------------------------------------***
072100000000     C*---------- QUANTITA/VALORE :                        *
072200000000     C     QF            COMP      'Q'                                    03    *QUANTITA'
072300000000     C  N03              Z-ADD     1             QTAF                           *SOLO VALORE
072400000000     C     VF            COMP      'V'                                    34    *VALORE
072500000000     C                   SETON                                          0434    *VALORE
072600000000     C*---------- S/N CENTRO DI COSTO :                    *
072700000000     C                   MOVE      SNCSCA        SNCF                           *S/N CDC SCA
072800000000     C     SNCF          COMP      'S'                                    06    *
072900000000     C                   SETOFF                                           90    *
073000000000     C                   MOVE      *BLANKS       ERRORE                         *
073100000000     CSR                 ENDSR                                                  *
073200000000     C***------------------------------------------------***
073300000000     C**--------- CONTROLLI SUL FONDO -------------------***
073400000000     C***------------------------------------------------***
073500000000     CSR   CTLFON        BEGSR                                                  *
073600860702     C*--------- EMISSIONE ERRORI                          *
073700860702     C     EMICTL        TAG                                                    *
073800860702     C   90              SETON                                        818283    *
073900860702     C   90              SETON                                        848586    *
074000860702     C   90              EXFMT     SFL1CTL                                      *
074100910712     C                   SETOFF                                       15        *
074200860702     C                   MOVEA     *ZEROS        *IN(81)                        *
074300860702     C                   MOVE      *BLANKS       ERRORE                         *
074400990120     C   kl              GOTO      INIZIO                                       *
074500860702     C   KA              GOTO      SUFOND                                       *
074600000000     C*---------- QUANTITA :                               *
074700000000     C     AZ(15)        IFNE      '*'                                          *
074800000000     C     QTAF          IFLE      0                                            *
074900000000     C                   SETON                                        9790      *
075000000000     C                   MOVEL     ERR(9)        ERRORE                         *
075100860702     C                   GOTO      EMICTL                                       *
075200860702     C                   ELSE                                                   *
075300860702     C                   Z-ADD     QTAF          QTAW                           *
075400860702     C                   END                                                    *
075500000000     C                   END                                                    *
075600871214     C*---------- QUANTITA :                               *
075700871222     C     AZ(15)        CABEQ     '*'           NOCTRQ
075800871214     C     QTAF          CABEQ     QTAAF         NOCTRQ
075900871215     C                   MOVE      AZF           SEGGIA            1
076000871214     C     SEGGIA        CABEQ     '+'           NOCTRQ
076100871214     C                   Z-ADD     0             DIFGIA           15 2
076200871214     C     QTAF          IFGT      QTAAF
076300871214     C     QTAF          SUB       QTAAF         DIFGIA
076400871214     C                   END
076500871214     C     DIFGIA        CABEQ     0             NOCTRQ
076600871214     C                   EXSR      RICDIS
076700871214     C     DISPON        COMP      DIFGIA                               97
076800871214     C   97              SETON                                        90
076900871214     C   97              MOVEL     ERR(28)       ERRORE
077000871214     C   97              GOTO      EMICTL
077100871214     C** CONTROLLO DISPONIBILITA MAGAZZINO PER PRELIEVO
077200871214     C     NOCTRQ        TAG
077300871214     C*----------------------------------------------------*
077400000000     C*---------- C/COSTO :                                *
077500000000     C     SNCF          IFEQ      'S'                                          *
077600990120     C     CDCF          IFEQ      '?       '                                   *
077700000000     C                   MOVE      *BLANKS       CDCF                           *
077800000000     C                   MOVE      *ZEROS        CODCDC                         *
077900000000     C                   MOVE      *BLANKS       DESCDC                         *
078000990317     C                   CALL      'TII102R'                                    *
078100000000     C                   PARM                    CODCDC                         *
078200000000     C                   PARM                    DESCDC                         *
078300000000     C                   MOVE      CODCDC        CDCF                           *
078400000000     C                   END                                                    *
078500860702RIC  C                   MOVE      CDCF          CODCDC                         *
078600990120RIC  C     KEYCDC        CHAIN     anent01l                           90        *
078700000000     C  N90CDCF          COMP      '9999999'                              90    *
078800000000     C  N90CDCF          COMP      '0000000'                              90    *
078900000000     C  N90CDCF          COMP      *BLANKS                                90    *
079000000000     C   90              SETON                                            98    *
079100000000     C   90              MOVEL     ERR(4)        ERRORE                         *
079200860702     C   90              GOTO      EMICTL                                       *
079300000000     C                   ELSE                                                   *
079400990120     C                   MOVE      '99999999'    CDCF                           *
079500000000     C                   END                                                    *
079600860702     C***------------------------------------------------***
079700000000     C*---------- CONTROLLO NUM. E DATA DOCUMENTO :        *
079800000000     C     NDOCF         COMP      0                                      90    *
079900000000     C  N90DDOCF         COMP      0                                      90    *
080000000000     C  N90              DO                                                     *
080100990120     C                   move      DDOCF         dataeur                        *
080200990120     C                   move      dataeur       dataiso                        *
080300990120     C                   move      dataiso       DDOCK             8 0          *
080400000000     C                   END                                                    *
080500000000     C   90              SETON                                        87        *
080600000000     C   90              MOVEL     ERR(7)        ERRORE                         *
080700860702     C   90              GOTO      EMICTL                                       *
080800860702     C*---------- PREZZO :                                 *
080900880115     C                   SETOFF                                       9099
081000850403     C                   MOVEA     OPEF          FO                             *
081100880115     C     PRZF          IFLE      0                                            *
081200990120     C     KEYLMP        CHAIN     tmlmp01L                           27        *
081300850403     C  N27              Z-ADD     PRE(COLF)     PRZF                           *
081400880115     C                   END                                                    *
081500850403     C*---------                                           *
081600880115     C     FO(3)         IFEQ      'S'                                          *
081700850403     C     PRZF          COMP      0                                    9090    *
081800850403     C   90              MOVEL     ERR(14)       ERRORE                         *
081900850403     C   90              SETON                                        99        *
082000860702     C   90              GOTO      EMICTL                                       *
082100850403     C                   END                                                    *
082200850314     C*--------- RICERCA ANAGRAFICA -----------------------*
082300990120     C     KEYAMP        CHAIN     tmamp01L                           90        *
082400850314     C   90              SETON                                        96        *
082500850314     C   90              MOVEL     ERR(10)       ERRORE                         *
082600860702     C   90              GOTO      EMICTL                                       *
082700850403     CSR                 ENDSR                                                  *
082800000000     C**--------- RICERCA LISTINO -----------------------***
082900000000     C***------------------------------------------------***
083000000000     CSR   RUTLIS        BEGSR                                                  *
083100000000     C     LIS           IFEQ      '?  '                                        *
083200000000     C                   MOVE      *BLANKS       LIS                            *
083300000000     C                   MOVE      *BLANKS       DS161            75            *
083400990317     C                   CALL      'TII116R'                                    *
083500000000     C                   PARM                    LIS                            *
083600000000     C                   PARM                    DS161                          *
083700000000     C                   END                                                    *
083800000000     C                   MOVE      '161'         TABGRU                         *
083900000000     C                   MOVE      *ZEROS        TABCOD                         *
084000000000     C                   MOVE      LIS           TABCOD                         *
084100000000     C     KEYTAB        CHAIN     ANTAB00F                           90        *
084200000000     CSR                 ENDSR                                                  *
084300000000     C***------------------------------------------------***
084400000000     C**--------- AGGIORNAMENTO SALDI DI MAGAZZINO ------***
084500000000     C***------------------------------------------------***
084600000000     CSR   AGGSAL        BEGSR                                                  *
084700000000     C                   SETOFF                                       919293    *
084800000000     C                   SETOFF                                       94        *
084900990120     C     KEYSMP        CHAIN     tmsmp01L                           79        *
085000000000     C   79              MOVE      ' '           SMPATT                         *
085100990120     C   79              move      xsccut        SMPUTE                         *
085200990120     C                   Z-ADD     woggig        SMPDMN                         *DATA MANUT.
085300000000     C   79              DO                                                     *
085400000000     C                   Z-ADD     ANNO          SMPAAC                         *ANNO COMP.
085500000000     C                   MOVE      MAGF          SMPMAG                         *MAGAZZINO
085600000000     C                   MOVE      ARTF          SMPMAT                         *PRODOTTO
085700000000     C                   MOVE      AMPUMA        SMPUMA                         *U.M. ANAGR.
085800000000     C                   Z-ADD     0             SMPPLF                         *LIFO FISC.
085900000000     C                   Z-ADD     0             SMPPLV                         *LIFO VAR.
086000000000     C                   Z-ADD     0             SMPQGI                         *GIAC.INIZ.
086100000000     C                   Z-ADD     0             SMPQOR                         *QTA ORDINI
086200000000     C                   Z-ADD     0             SMPQPR                         *QTA PRODUZ.
086300000000     C                   Z-ADD     0             SMPQIM                         *QTA IMPEG.
086400870518     C                   Z-ADD     0             SMPQIP                         *QTA IMPEG.
086500000000     C                   Z-ADD     0             SMPQGF                         *GIAC.FINALE
086600000000     C                   Z-ADD     0             SMPQE1                         *QTA E-1
086700000000     C                   Z-ADD     0             SMPQE2                         *QTA E-2
086800000000     C                   Z-ADD     0             SMPQE3                         *QTA E-3
086900000000     C                   Z-ADD     0             SMPQE4                         *QTA E-4
087000000000     C                   Z-ADD     0             SMPQE5                         *QTA E-5
087100000000     C                   Z-ADD     0             SMPQU1                         *QTA U-1
087200000000     C                   Z-ADD     0             SMPQU2                         *QTA U-2
087300000000     C                   Z-ADD     0             SMPQU3                         *QTA U-3
087400000000     C                   Z-ADD     0             SMPQU4                         *QTA U-4
087500000000     C                   Z-ADD     0             SMPQU5                         *QTA U-5
087600000000     C                   Z-ADD     DATMOV        SMPDMS                         *DATA SALDO
087700000000     C                   Z-ADD     0             SMPNMS                         *MOV. SALDO
087800000000     C                   Z-ADD     0             SMPPMC                         *PRZ. MEDIO
087900000000     C                   MOVE      *BLANKS       SMPFLG                         *FLAGS
088000990311     C                   MOVE      valf          SMPval                         *FLAGS
088100990311     C                   Z-ADD     1             SMPcmb                         *PRZ. MEDIO
088200000000     C                   END                                                    *
088300000000     C*---------- CONVERSIONE QUANTITA' :                  *
088400000000     C     QTAW          MULT(H)   FCVF          QTAANA           15 5          *
088500000000     C*----------------------------------------------------*
088600000000     C*---------- INVENTARIO ------------------------------*
088700000000     C     AZ(15)        IFEQ      '*'                                          *
088800000000     C*---------- ESEGUO DIFFERENZA :                      *
088900000000     C     SMPQGF        CABEQ     QTAANA        SUFOND               9192      *
089000000000     C   91SMPQGF        SUB       QTAANA        QTAANA                         *
089100000000     C   92QTAANA        SUB       SMPQGF        QTAANA                         *
089200000000     C                   MOVEA     *BLANKS       AZ                             *
089300000000     C   92              MOVE      '+'           AZ(5)                          *ENTRATE
089400000000     C   91              MOVE      '+'           AZ(10)                         *USCITE
089500000000     C   92              MOVE      '+'           AZ(15)                         *GIACENZA +
089600000000     C   91              MOVE      '-'           AZ(15)                         *GIACENZA -
089700000000     C   91              MOVE      'RI-'         CAUF                           *
089800000000     C   92              MOVE      'RI+'         CAUF                           *
089900000000     C                   MOVEA     AZ            AZF                            *
090000000000     C                   Z-ADD     0             RELRF                          *
090100000000     C                   Z-ADD     0             RELVF                          *
090200000000     C                   END                                                    *
090300000000     C*--------- AGGIORNAMENTO ENTRATA --------------------*
090400000000     C     1             DO        5             X                 2 0          *
090500000000     C     AZ(X)         COMP      '+'                                    92    *
090600000000     C     AZ(X)         COMP      '-'                                    91    *
090700000000     C   92QTK(X)        ADD       QTAANA        QTK(X)                         *
090800000000     C   91QTK(X)        SUB       QTAANA        QTK(X)                         *
090900000000     C                   END                                                    *
091000000000     C*--------- AGGIORNAMENTO USCITA ---------------------*
091100000000     C     6             DO        10            X                              *
091200000000     C     AZ(X)         COMP      '+'                                    92    *
091300000000     C     AZ(X)         COMP      '-'                                    91    *
091400000000     C   92QTK(X)        ADD       QTAANA        QTK(X)                         *
091500000000     C   91QTK(X)        SUB       QTAANA        QTK(X)                         *
091600000000     C                   END                                                    *
091700000000     C*--------- AGGIORNAMENTO GIACENZA -------------------*
091800000000     C     AZ(15)        COMP      '+'                                    92    *
091900000000     C     AZ(15)        COMP      '-'                                    91    *
092000000000     C   92SMPQGF        ADD       QTAANA        SMPQGF                         *GIACENZA +
092100000000     C   91SMPQGF        SUB       QTAANA        SMPQGF                         *GIACENZA -
092200000000     C*---------- AGGIORNAMENTO SALDI :                    *
092300990120     C   79              WRITE     tmsmp000                                     *UPDATE
092400990120     C  N79              UPDATE    tmsmp000                                     *UPDATE
092500000000     C*---------- CONVERSIONE QUANTITA' :                  *
092600000000     C     QTAANA        DIV(H)    FCVF          QTAW                           * Q.TA' PROMPT
092700870921     C**-----
092800870921     C     KMAT          KLIST
092900870921     C                   KFLD                    SMPMAT
093000870921     C     AZ(2)         IFEQ      '+'
093100990120     C     KMAT          CHAIN     tmamp000                           89
093200870921     C  N89              DO
093300870921     C     AMPTAR        IFLT      DATMOV
093400870921     C                   Z-ADD     DATMOV        AMPTAR
093500870921     C                   END
093600990120     C                   UPDATE    tmamp000
093700870921     C                   END
093800870921     C** SE MOVIMENTO USCITA PER VENDITA
093900870921     C** AGGIORNO DATA ULTIMA USCITA PER VENDITA
094000870921     C** SU ANAGRAFICO MATERIALE
094100870921     C                   END
094200000000     CSR   ENDAGG        ENDSR                                                  *
094300000000     C**--------- AGGIORNAMENTO MOVIMENTI DI MAGAZZINO --***
094400000000     C***------------------------------------------------***
094500000000     CSR   AGGMOV        BEGSR                                                  *
094600000000     C                   MOVE      ' '           MMPATT                         *ATTIVITA'
094700990120     C                   MOVE      xsccut        MMPUTE                         *UTENTE
094800990120     C                   Z-ADD     woggig        MMPDMN                         *DATA MANUT.
094900000000     C                   MOVE      MAGF          MMPMAG                         *MAGAZZINO
095000000000     C                   Z-ADD     ANNO          MMPAAC                         *ANNO
095100000000     C                   Z-ADD     DATMOV        MMPDMV                         *DATA MOVIM.
095200000000     C                   Z-ADD     NUMMOV        MMPNMV                         *NUM. MOVIM.
095300000000     C                   Z-ADD     DDOCK         MMPDDC                         *DATA DOCUM.
095400000000     C                   Z-ADD     NDOCF         MMPNDC                         *NUM. DOCUM.
095500000000     C                   MOVE      CAUF          MMPCAU                         *CAUSALE
095600000000     C                   MOVE      AZF           MMPOPC                         *AZIONI
095700000000     C                   MOVE      CDCF          MMPCDC                         *C/COSTO
095800000000     C                   MOVE      ARTF          MMPMAT                         *PRODOTTO
095900000000     C                   MOVE      AMPUMA        MMPUMA                         *U.M.ANAG.
096000000000     C                   Z-ADD     QTAANA        MMPQTA                         *QUANTITA'
096100860818     C     PRZF          DIV       FCVF          MMPPRZ                         *PREZZO
096200000000     C                   MOVE      MCPF          MMPMCP                         *MAG.C/P.
096300000000     C                   MOVE      QF            MMPFGQ                         *MOV.A QUANTITA
096400000000     C                   MOVE      VF            MMPFGV                         *MOV.A VALORE
096500000000     C                   MOVE      TF            MMPFGT                         *TIPO MAGAZZINO
096600000000     C                   Z-ADD     0             MMPCRG                         *COD.IND.RIGA
096700990311     c                   move      valf          mmpval
096800990311     c                   z-add     1             mmpcmb
096900871201     C** PER COMMESSA SI INCREMENTA COD,IND.RIGA
097000990120     C  N22              WRITE     tmmmp000                                     *WRITE
097100000000     C  N22              Z-ADD     NREL1         RELRF                          *
097200850611     C                   SETON                                            28    *STAMPA
097300000000     CSR                 ENDSR                                                  *
097400000000     C***------------------------------------------------***
097500000000     C**--------- CARICAMENTO DEL VIDEO -----------------***
097600000000     C***------------------------------------------------***
097700000000     CSR   AGGVID        BEGSR                                                  *
097800000000     C     RELVF         COMP      0                                      78    *
097900000000     C   78RECT          ADD       1             RECT                           *
098000000000     C   78              Z-ADD     RECT          REC1                           *
098100000000     C  N78              Z-ADD     RELVF         REC1                           *
098200000000     C  N78REC1          CHAIN     SFL1                               78        *
098300000000     C                   Z-ADD     RELRF         RELREC                         *
098400000000     C                   Z-ADD     REC1          RELVID               71        *
098500000000     C                   Z-ADD     REC1          CURSOR                         *
098600000000     C                   MOVE      FCVF          FCV                            *
098700000000     C     SCEF          IFEQ      'A'                                          *
098800000000     C                   MOVE      SCEF          SCELTW                         *
098900000000     C                   ELSE                                                   *
099000000000     C                   MOVE      *BLANKS       SCELTW                         *
099100000000     C                   END                                                    *
099200000000     C                   MOVE      SCELTW        SCELTA                         *
099300000000     C                   MOVE      CSF           FLGCS                          *
099400000000     C                   MOVEL     DESF          DESART                         *
099500000000     C                   MOVE      UMSF          UMI                            *
099600000000     C                   Z-ADD     QTAW          QTAV                           *
099700000000     C                   MOVE      LISF          LISCOD                         *
099800000000     C                   Z-ADD     COLF          LISCOL                         *
099900000000     C                   Z-ADD     PRZF          PRZV                           *
100000000000     C                   MOVE      CDCF          MMPCDC                         *
100100850314     C                   Z-ADD     DDOCK         MMPDDC                         *
100200850314     C                   Z-ADD     NDOCF         MMPNDC                         *
100300000000     C   78              WRITE     SFL1                                         *
100400000000     C  N78              UPDATE    SFL1                                         *
100500000000     CSR                 ENDSR                                                  *
100600990120     C*----------------------------------------------------*
100700990120     C* Reperimento dati società
100800990120     C*----------------------------------------------------*
100900990120     C     REPSOC        BEGSR
101000990120     C*
101100990120     C                   CALLB     'XSOC'
101200990120     C                   PARM                    TIPXSC            6
101300990120     C                   PARM                    SOCXSC            3
101400990120     C                   PARM                    CDSXSC            9 0
101500990120     C                   PARM                    MODXSC            3
101600990120     C                   PARM      *blanks       RTNXSC            1
101700990120     C                   PARM                    XSOCDS
101800990120     C                   PARM                    KPJBA
101900990120     C*
102000990120     C                   ENDSR
102100990120     Otmmmp000  E            ANNMMP
102200000000     O                       MMPATT
102300910712**    ERR
102400000000*- Anno non VALIDO !!!!!!!! -*      ERR,1
102500000000*- Codice Magazzino  ERRATO -*      ERR,2
102600000000*- Codice Causale    ERRATO -*      ERR,3
102700000000*- Codice C/Costo    ERRATO -*      ERR,4
102800000000*- Lotto o Commessa  ERRATO -*      ERR,5
102900000000*- Listino    non    VALIDO -*      ERR,6
103000000000*- Documento o Data  ERRATI -*      ERR,7
103100000000*-Non Ammesse Causali UGUALI-*      ERR,8
103200000000*- Inserire Dati Mancanti ! -*      ERR,9
103300000000*- Codice Prodotto ERRATO ! -*      ERR,10
103400000000*- Causale non di SCARICO   -*      ERR,11
103500000000*- Causale non di CARICO    -*      ERR,12
103600000000*E' ammesso solo un Magazzino*      ERR,13
103700000000*- Listino/prezzo non Valido-*      ERR,14
103800850403*- Data Movimento ERRATA !! -*      ERR,15
103900850611*- CAMBIO DATA NON AMMESSO !-*      ERR,16
104000871201*- N.COMMESSA OBBLIGATORIO !-*      ERR,17
104100871201*- N.COMMESSA INESISTENTE  !-*      ERR,18
104200871201*- STATO COMMESSA NON VALIDO *      ERR,19
104300871201*- DATA REG < DATA CONG.COM!-*      ERR,20
104400871201*- TIPO ATT.COSTI NON VALIDO-*      ERR,21
104500871202*- C O S T O  OBBLIGATORIO  -*      ERR,22
104600871203*- PREZZO VENDITA = ZERO    -*      ERR,23
104700871214CARICO CLIENTE                          24
104800871214CARICO FORNITORE                        25
104900871214CARICO CONCESSIONARIA                   26
105000871214ASS.CLIENTE                             27
105100871214MANCA DISPONIBILITA MAGAZZINO           28
105200910311ACCESSO NEGATO ALLA CAUSALE !!          29
105300910712*- MATRICOLA OBBLIGATORIA ! -*          30
105400910712 ATTREZZATURA GIA' IN CARICO            31

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
034000910311     C   90              GOTO      EMISPT
034100910311     C*
034200871201     C     AZ(15)        COMP      '*'                                    21
034300871201     C** INVENTARIO
034400871201     C     AZ(15)        IFEQ      '+'
034500871201     C                   MOVEL     'C'           FLGSCA
034600871201     C                   ELSE
034700871201     C                   MOVEL     'S'           FLGSCA
034800871201     C                   END
034900871201     C** CARICO O SCARICO
035000850403     C                   MOVE      OPE152        OPESCA                         *
035100000000     C*---------- C/COSTO DI SCARICO ----------------------*
035200000000     C   05              DO                                                     *
035300990120     C     CDCSCA        COMP      '00000000'                             95    *
035400990120     C  N95CDCSCA        COMP      '99999999'                             95    *
035500000000     C  N95CDCSCA        COMP      *BLANKS                                95    *
035600990120     C   95              MOVE      '?       '    CDCSCA                         *
035700990120     C     CDCSCA        IFEQ      '?       '                                   *
035800000000     C                   MOVE      *BLANKS       CDCSCA                         *
035900000000     C                   MOVE      *ZEROS        CODCDC                         *
036000000000     C                   MOVE      *BLANKS       DESCDC           48            *
036100990317     C                   CALL      'TII102R'                                    *
036200990121     c                   parm                    wsoc
036300000000     C                   PARM                    CODCDC                         *
036400000000     C                   PARM                    DESCDC                         *
036500000000     C                   MOVE      CODCDC        CDCSCA                         *
036600850611     C                   SETON                                            13    *
036700000000     C                   GOTO      EMISPT                                       *
036800000000     C                   END                                                    *
036900990120     C                   END                                                    *
037000860702RIC  C******************************************************
037100871206     C**
037200860702RIC  C                   MOVE      CDCSCA        CODCDC                         *
037300990120RIC  C     KEYCDC        CHAIN     anent01l                           90        *
037400000000     C   90              MOVEL     ERR(4)        ERRORE                         *
037500000000     C   90              SETON                                        13        *
037600000000     C   90              GOTO      EMISPT                                       *
037700000000     C*---------- LISTINO PREZZI --------------------------*
037800871201     C     LISPT         COMP      *BLANKS                                90
037900871201     C   90              MOVEL     ERR(9)        ERRORE
038000871201     C   90              SETON                                        19
038100871201     C   90              GOTO      EMISPT
038200871201     C** LISTINO ERRATO
038300871201     C     COLPT         COMP      0                                      19
038400871201     C   19              SETON                                        90
038500871201     C   90              MOVEL     ERR(9)        ERRORE
038600871201     C   90              GOTO      EMISPT
038700871201     C** COLONNA LISTINO ERRATA
038800000000     C*----------                                          *
038900000000     C     LISPT         IFNE      '999'                                        *
039000000000     C                   MOVE      LISPT         LIS               3            *
039100000000     C                   EXSR      RUTLIS                                       *
039200000000     C  N90              MOVE      LIS           LISPT                          *
039300000000     C  N90COLPT         COMP      0                                      90    *
039400000000     C   90              MOVEL     ERR(6)        ERRORE                         *
039500000000     C   90              SETON                                        19        *
039600000000     C   90              GOTO      EMISPT                                       *
039700000000     C                   END                                                    *
039800000000     C*---------- NUMERO E DATA DOCUMENTO -----------------*
039900000000     C     NUMDOC        COMP      0                                      90    *
040000000000     C  N90DATDOC        COMP      0                                      90    *
040100000000     C                   Z-ADD     NUMDOC        NDOCF                          *
040200000000     C                   Z-ADD     DATDOC        DDOCF                          *
040300000000     C*---------- RICERCA NUMERO MOVIMENTO ----------------*
040400000000     C     SULOCK        TAG                                                    *
040500000000     C     *LOCK         IN        MPHDT1                               90      *
040600000000     C   90              GOTO      SULOCK                                       *
040700850516     C     NUMDTA        ADD       1             NUMDTA                   41    *
040800850516     C   41NUMDTA        ADD       1             NUMDTA                         *
040900990120     C                   MOVE      woggi         DATDTA                         *
041000000000     C                   OUT       MPHDT1                                       *
041100000000     C                   Z-ADD     NUMDTA        NUMMOV                         *
041200850611     C                   MOVE      NUMDTA        MOV116                         *VISUAL.MOV.
041300850403     C                   MOVE      DMVDTA        DAL116                         *
041400850403     C                   MOVE      DMVDTA        AL116                          *
041500850611     C                   MOVE      NUMDTA        MOV119                         *STAMPA MOV.
041600850611     C                   MOVE      DMVDTA        DAL119                         *
041700850611     C                   MOVE      DMVDTA        AL119                          *
041800000000     C*---------- ROVESCIO DATA MOVIMENTO :                *
041900990120     C                   move      DMVDTA        DATAeur                        *
042000990120     C                   move      dataeur       dataiso                        *
042100990120     C                   move      DATAIso       DATMOV            8 0          *
042200000000     C*---------- PULIZIA SUBFILE :                        *
042300000000     C                   SETON                                            70    *PULIZIA
042400000000     C                   WRITE     SFL1CTL                                      *
042500000000     C                   MOVEA     *ZEROS        *IN(10)                        *
042600000000     C                   WRITE     MPH110TS                                     *TESTATA
042700000000     C*---------- EMISSIONE FORMATO -----------------------*
042800000000     C                   Z-ADD     0             REC1              5 0          *
042900000000     C                   Z-ADD     0             RECT              5 0          *
043000000000     C                   Z-ADD     1             CURSOR                         *
043100000000     C     SUFOND        TAG                                                    *
043200000000     C                   SETOFF                                           80    *
043300000000     C                   EXSR      CARFON                                       *
043400871205     C     TIPATC        IFEQ      'N'
043500871205     C                   WRITE     MPH110TS
043600871205     C                   END
043700871205     C** SE EMESSO 3 VIDEATA RIEMETTO LA 1
043800000000     C*---------                                           *
043900000000     C     EMISF1        TAG                                                    *
044000910715     C                   WRITE     MPH110TS                                     *PROVA
044100000000     C                   SETON                                        818283    *
044200000000     C                   SETON                                        848586    *
044300000000     C                   EXFMT     SFL1CTL                                      *
044400000000     C                   MOVEA     *ZEROS        *IN(80)                        *
044500000000     C                   MOVE      *BLANKS       ERRORE                         *
044600850611     C*---------- EMISSIONE STAMPA :                       *
044700990120     C   Kl
044800850611     CAN 28SNSTA         IFEQ      'S'                                          *
044900850611     C                   MOVE      ANNO          AAA119                         *
045000861212     C                   MOVE      NOVE          MAT119                         *
045100850611     C                   MOVEL     TIPUMI        UMI119                         *
045200850611     C                   MOVE      *ALL'9'       CAU119                         *
045300990120     C                   MOVE      '99999999'    CDC119                         *
045400850611     C                   MOVE      'D'           STA119                         *
045500990317     C                   MOVE      'I204'        KCOAZ                          *
045600850614     C                   MOVE      'H'           KJOST                          *CONGELATA
045700850614     C                   MOVEL     'KSNOTTE'     KQEBI                          *
045800850611     C                   MOVEL     MAGSCA        MAG119                         *
045900850611     C                   MOVEL     PH119         KPJBU                          *
046000850611     C                   CALL      'BCH10'                                      *
046100850611     C                   PARM                    KPJBA                          *
046200850611     C                   END                                                    *
046300990120     C   Kl              GOTO      INIZIO                                       *
046400000000     C   KA              GOTO      SUFOND                                       *
046500000000     C*--------- LETTURA RIGA DA SUBFILE ------------------*
046600000000     C   71              READC     SFL1                                   31    *
046700000000     C   71
046800000000     CANN31              DO                                                     *
046900000000     C     SCELTA        COMP      'A'                                    31    *
047000000000     C  N31SCELTA        COMP      'G'                                    31    *
047100000000     C  N31SCELTA        COMP      'D'                                    31    *
047200000000     C  N31SCELTA        COMP      'M'                                    31    *
047300000000     C  N31SCELTA        CABNE     'V'           EMISF1                   31    *
047400000000     C                   Z-ADD     RELVID        REC1                           *
047500000000     C     REC1          CHAIN     SFL1                               31        *SFL
047600000000     C   31              GOTO      SUFOND                                       *
047700000000     C*--------- GIA ANNULLATO :                           *
047800000000     C     SCELTW        IFEQ      'A'                                          *
047900000000     C                   MOVE      'A'           SCELTA                         *
048000000000     C                   UPDATE    SFL1                                         *
048100000000     C                   GOTO      SUFOND                                       *
048200000000     C                   END                                                    *
048300000000     C*--------- ANNULLAMENTO SCELTA :                     *
048400000000     C   KA              MOVE      ' '           SCELTA                         *
048500000000     C   KA              UPDATE    SFL1                                         *
048600000000     C   KA              GOTO      SUFOND                                       *
048700000000     C*--------- CARICO DATI SUL FONDO---------------------*
048800000000     C     SCELTA        CABEQ     'G'           GIATAG                         *
048900000000     C     SCELTA        CABEQ     'D'           GIATAG                         *
049000000000     C     SCELTA        CABEQ     'M'           MOVTAG                         *
049100000000     C                   SETON                                        80  32    *
049200000000     C                   UPDATE    SFL1                                         *
049300000000     C                   SETOFF                                           32    *
049400910712     C                   SETON                                            40    *
049500000000     C                   EXSR      CARFON                                       *
049600000000     C                   GOTO      EMISF1                                       *
049700000000     C*--------- RICHIAMO GIACENZA/DISPONIBILITA'----------*
049800000000     C     GIATAG        TAG                                                    *
049900000000     C                   MOVE      MMPMAG        MAG115                         *
050000000000     C                   MOVE      MMPMAT        MAT115                         *
050100000000     C                   MOVE      TIPUMI        UMI115                         *
050200000000     C                   MOVE      SCELTA        SCE115                         *
050300000000     C                   MOVEL     PH115         KPJBU                          *
050400990317     C                   CALL      'TII202R'                                    *
050500000000     C                   PARM                    KPJBA                          *
050600000000     C                   MOVE      ' '           SCELTA                         *
050700000000     C                   UPDATE    SFL1                                         *
050800000000     C                   GOTO      SUFOND                                       *
050900000000     C*--------- RICHIAMO MOVIMENTI -----------------------*
051000000000     C     MOVTAG        TAG                                                    *
051100000000     C                   MOVE      MMPMAG        MAG116                         *
051200000000     C                   MOVE      MMPMAT        MAT116                         *
051300000000     C                   MOVE      TIPUMI        UMI116                         *
051400000000     C                   MOVE      MMPCAU        CAU116                         *
051500000000     C                   MOVEL     PH116         KPJBU                          *
051600990317     C                   CALL      'TII203R'                                    *
051700000000     C                   PARM                    KPJBA                          *
051800000000     C                   MOVE      ' '           SCELTA                         *
051900000000     C                   UPDATE    SFL1                                         *
052000000000     C                   GOTO      SUFOND                                       *
052100000000     C                   END                                                    *
052200000000     C*--------- CREAZIONE RIGA NUOVA ---------------------*
052300000000     C     RELVF         IFEQ      0                                            *
052400910712     C                   SETOFF                                           40    *
052500000000     C*---------- PRODOTTO :                               *
052600861212     C     ARTF          IFEQ      INTERR                                       *
052700000000     C                   MOVE      *BLANKS       ARTF                           *
052800000000     C                   MOVEL     DESF          DESPRO           30            *
052900990317     C                   CALL      'TII104R'                                    *
053000000000     C                   PARM                    ARTF                           *
053100000000     C                   PARM                    DESPRO                         *
053200000000     C                   MOVEL     DESPRO        DESF                           *
053300990120     C     KEYAMP        CHAIN     tmamp01L                           90        *
053400000000     C   90              SETON                                        96        *
053500000000     C   90              MOVEL     ERR(10)       ERRORE                         *
053600000000     C   90              GOTO      EMISF1                                       *
053700000000     C                   MOVE      AMPUMA        UMAF                           *
053800000000     C                   MOVEL     AMPDMT        DESF                           *
053900000000     C     TIPUMI        COMP      'A'                                    24    *UNITA MISURA
054000000000     C   24              MOVE      AMPUMA        UMSF                           *ANAGRAFICA
054100000000     C     TIPUMI        COMP      'Q'                                    24    *           A
054200000000     C   24              MOVE      AMPUMQ        UMSF                           *ACQUISTO
054300000000     C     TIPUMI        COMP      'S'                                    24    *           A
054400000000     C   24              MOVE      AMPUMS        UMSF                           *STOCCAGGIO
054500000000     C     TIPUMI        COMP      'V'                                    24    *           A
054600000000     C   24              MOVE      AMPUMV        UMSF                           *VENDITA
054700000000     C                   SETON                                        8097      *
054800910712     C*                                                    *
054900000000     C                   GOTO      EMISF1                                       *
055000000000     C                   ELSE                                                   *
055100990120     C     KEYAMP        CHAIN     tmamp01L                           90        *
055200000000     C   90              SETON                                        96        *
055300000000     C   90              MOVEL     ERR(10)       ERRORE                         *
055400000000     C   90              GOTO      EMISF1                                       *
055500000000     C                   MOVE      AMPUMA        UMAF                           *
055600000000     C                   MOVEL     AMPDMT        DESF                           *
055700000000     C     TIPUMI        COMP      'A'                                    24    *UNITA MISURA
055800000000     C   24              MOVE      AMPUMA        UMSF                           *ANAGRAFICA
055900000000     C   24              Z-ADD     1             FCVF                           *
056000000000     C     TIPUMI        COMP      'Q'                                    24    *           A
056100000000     C   24              MOVE      AMPUMQ        UMSF                           *ACQUISTO
056200000000     C   24              Z-ADD     AMPFCQ        FCVF                           *
056300000000     C     TIPUMI        COMP      'S'                                    24    *           A
056400000000     C   24              MOVE      AMPUMS        UMSF                           *STOCCAGGIO
056500000000     C   24              Z-ADD     AMPFCS        FCVF                           *
056600000000     C     TIPUMI        COMP      'V'                                    24    *           A
056700000000     C   24              MOVE      AMPUMV        UMSF                           *VENDITA
056800000000     C   24              Z-ADD     AMPFCV        FCVF                           *
056900000000     C                   END                                                    *
057000000000     C*--------- CREAZIONE RECORD SFL X SCARICO -----------*
057100871203     C                   DO                                                     *
057200000000     C                   MOVE      SNCSCA        SNCF              1            *
057300000000     C                   Z-ADD     QTAF          QTAW             15 2          *
057400850403     C                   MOVE      OPESCA        OPEF                           *
057500000000     C                   MOVE      AZS           AZF                            *
057600000000     C                   MOVEA     AZF           AZ                             *
057700000000     C                   EXSR      CTLFON                                       *
057800000000     C                   SETOFF                                           22    *
057900000000     C                   EXSR      AGGSAL                                       *
058000000000     C                   EXSR      AGGMOV                                       *
058100000000     C                   EXSR      AGGVID                                       *
058200871201     C                   END                                                    *
058300871204     C                   GOTO      SUFOND
058400871204     C                   END                                                    *
058500000000     C*--------- AGGIORNAMENTO RIGA VECCHIA ---------------*
058600000000     C     RELVF         IFNE      0                                            *
058700000000     C                   EXSR      CTLFON                                       *
058800000000     C*--------- ELIMINAZIONE MOVIMENTO PRECEDENTE :       *
058900000000     C     RELRF         IFNE      0                                            *
059000990120     C     RELRF         CHAIN     tmmmp00F                           22        *
059100871203     C  N22              DO
059200871203     C                   MOVE      'A'           MMPATT                         *
059300871203     C                   EXCEPT    ANNMMP                                       *
059400871203     C                   END
059500871203     C** ANNULLO MOV.MAGAZZINO
059600000000     C                   Z-ADD     0             RELRF                          *
059700991102     C*--------- ESEGUO OPERAZIONE INVERSA :               *
059800991102     C     1             DO        15            X
059900991102     C     AZ(X)         COMP      '+'                                    22
060000991102     C     AZ(X)         COMP      '-'                                    23
060100991102     C   22              MOVE      '-'           AZ(X)
060200991102     C   23              MOVE      '+'           AZ(X)
060300991102     C                   END
060400000000     C                   Z-ADD     QTAAF         QTAW                           *
060500000000     C                   SETOFF                                           22    *
060600000000     C                   EXSR      AGGSAL                                       *
060700000000     C                   END                                                    *
060800000000     C*--------- ESEGUO OPERAZIONE VARIATA :               *
060900000000     C     SCELTA        IFEQ      'V'                                          *
061000000000     C                   MOVEA     AZF           AZ                             *
061100000000     C                   Z-ADD     QTAF          QTAW                           *
061200000000     C                   SETOFF                                           22    *
061300000000     C                   EXSR      AGGSAL                                       *
061400000000     C                   EXSR      AGGMOV                                       *
061500000000     C                   END                                                    *
061600000000     C                   EXSR      AGGVID                                       *
061700000000     C                   END                                                    *
061800000000     C                   GOTO      SUFOND                                       *
061900000000     C*----------------------------------------------------*
062000000000     C     FINE          TAG                                                    *
062100000000     C   90              MOVEL     'KPJB207'     KMSMN                          *
062200000000     C                   SETON                                            LR    *
062300990120     C*----------------------------------------------------*
062400871214     C     RICDIS        BEGSR
062500871215     C                   Z-ADD     0             DISPON           15 2
062600990120     C     KEYSMP        CHAIN     tmsmp01L                           90
062700871215     C   90              GOTO      NODSIP
062800871214     C     SMPQGF        SUB       SMPQIM        DISPON
062900871214     C                   SUB       SMPQIP        DISPON
063000871215     C     NODSIP        ENDSR
063100000000     C**--------- CARICAMENTO DEL FONDO -----------------***
063200000000     C***------------------------------------------------***
063300000000     CSR   CARFON        BEGSR                                                  *
063400000000     C                   SETOFF                                       030405    *
063500000000     C                   SETOFF                                       060708    *
063600000000     C                   SETOFF                                       34        *
063700000000     C*---------- POSIZIONAMENTO RIGA :                    *
063800860818     C                   Z-ADD     RELVID        CURSOR                         *
063900000000     C     CURSOR        DIV       10            CURSOR                         *
064000000000     C                   MVR                     RESTO             5 023        *
064100000000     C     CURSOR        MULT      10            CURSOR                         *
064200000000     C   23CURSOR        ADD       1             CURSOR                         *
064300000000     C*---------- CARICAMENTO DA PROMPT :                  *
064400000000     C  N80              DO                                                     *
064500000000     C                   Z-ADD     0             RELRF                          *
064600000000     C                   Z-ADD     0             RELVF                          *
064700000000     C                   Z-ADD     1             FCVF                           *
064800871203     C                   MOVEL     QVMSCA        QF                             *
064900871203     C                   MOVE      QVMSCA        VF                             *
065000871203     C                   MOVE      TIPSCA        TF                             *TIPO MAGAZZINO
065100871203     C                   MOVE      AZS           AZF                            *
065200000000     C                   MOVE      *BLANKS       UMAF                           *
065300000000     C                   MOVE      *BLANKS       MCPF                           *
065400000000     C                   MOVE      *BLANKS       SCEF                           *
065500871203     C                   MOVE      MAGSCA        MAGF                           *
065600871203     C                   MOVE      FLGSCA        CSF                            *
065700861212     C                   MOVE      INTERR        ARTF                           *
065800000000     C                   MOVE      *BLANKS       DESF                           *
065900871203     C                   MOVE      CAUSCA        CAUF                           *
066000871203     C                   MOVE      OPESCA        OPEF                           *
066100000000     C                   MOVE      *BLANKS       UMSF                           *
066200000000     C                   Z-ADD     0             QTAF                           *
066300871203     C                   MOVE      CDCSCA        CDCF                           *
066400000000     C                   MOVE      LISPT         LISF                           *
066500000000     C                   Z-ADD     COLPT         COLF                           *
066600871202     C   02              MOVEL     LISPT         LISV              3
066700871202     C   02              Z-ADD     COLPT         COLV              1 0
066800871202     C** SALVO LISTINO-COLONNA VENDITA
066900871202     C   02              MOVEL     '5'           COLF
067000871201     C** COMMESSA - LISTINO COSTO
067100000000     C                   Z-ADD     0             PRZF                           *
067200000000     C     NDOCF         IFEQ      0                                            *
067300000000     C                   Z-ADD     NUMDOC        NDOCF                          *
067400000000     C                   Z-ADD     DATDOC        DDOCF                          *
067500000000     C                   END                                                    *
067600000000     C                   END                                                    *
067700000000     C*---------- CARICAMENTO DA SUBFILE :                 *
067800000000     C   80              DO                                                     *
067900000000     C                   Z-ADD     RELREC        RELRF                          *
068000000000     C                   Z-ADD     RELVID        RELVF                          *
068100000000     C                   Z-ADD     FCV           FCVF                           *
068200000000     C                   MOVE      MMPFGQ        QF                             *
068300000000     C                   MOVE      MMPFGV        VF                             *
068400000000     C                   MOVE      MMPFGT        TF                             *
068500000000     C                   MOVE      MMPOPC        AZF                            *
068600000000     C                   MOVEA     AZF           AZ                             *
068700000000     C                   MOVE      MMPUMA        UMAF                           *
068800000000     C                   MOVE      MMPMCP        MCPF                           *
068900000000     C                   MOVE      SCELTA        SCEF                           *
069000000000     C                   MOVE      MMPMAG        MAGF                           *
069100000000     C                   MOVE      FLGCS         CSF                            *
069200000000     C                   MOVE      MMPMAT        ARTF                           *
069300000000     C                   MOVEL     DESART        DESF                           *
069400000000     C                   MOVE      MMPCAU        CAUF                           *
069500850403     C                   MOVE      OPESCA        OPEF                           *
069600000000     C                   MOVE      UMI           UMSF                           *
069700000000     C                   Z-ADD     QTAV          QTAF                           *
069800000000     C                   Z-ADD     QTAF          QTAAF                          *
069900000000     C                   MOVE      MMPCDC        CDCF                           *
070000000000     C                   SETON                                            84    *
070100000000     C                   MOVE      LISCOD        LISF                           *
070200000000     C                   Z-ADD     LISCOL        COLF                           *
070300000000     C                   Z-ADD     PRZV          PRZF                           *
070400000000     C                   Z-ADD     MMPNDC        NDOCF                          *
070500990120     C                   move      MMPDDC        dataiso                        *
070600990120     C                   move      dataiso       dataeur                        *
070700990120     C                   move      dataiso       XDTAMG                         *
070800990120     C                   move      dataeur       XDTGMA            8 0          *
070900000000     C                   Z-ADD     XDTGMA        DDOCF                          *
071000000000     C                   END                                                    *
071100860702     C***------------------------------------------------***
071200000000     C*---------- QUANTITA/VALORE :                        *
071300000000     C     QF            COMP      'Q'                                    03    *QUANTITA'
071400000000     C  N03              Z-ADD     1             QTAF                           *SOLO VALORE
071500000000     C     VF            COMP      'V'                                    34    *VALORE
071600000000     C                   SETON                                          0434    *VALORE
071700000000     C*---------- S/N CENTRO DI COSTO :                    *
071800000000     C                   MOVE      SNCSCA        SNCF                           *S/N CDC SCA
071900000000     C     SNCF          COMP      'S'                                    06    *
072000000000     C                   SETOFF                                           90    *
072100000000     C                   MOVE      *BLANKS       ERRORE                         *
072200000000     CSR                 ENDSR                                                  *
072300000000     C***------------------------------------------------***
072400000000     C**--------- CONTROLLI SUL FONDO -------------------***
072500000000     C***------------------------------------------------***
072600000000     CSR   CTLFON        BEGSR                                                  *
072700860702     C*--------- EMISSIONE ERRORI                          *
072800860702     C     EMICTL        TAG                                                    *
072900860702     C   90              SETON                                        818283    *
073000860702     C   90              SETON                                        848586    *
073100860702     C   90              EXFMT     SFL1CTL                                      *
073200910712     C                   SETOFF                                       15        *
073300860702     C                   MOVEA     *ZEROS        *IN(81)                        *
073400860702     C                   MOVE      *BLANKS       ERRORE                         *
073500990120     C   kl              GOTO      INIZIO                                       *
073600860702     C   KA              GOTO      SUFOND                                       *
073700000000     C*---------- QUANTITA :                               *
073800000000     C     AZ(15)        IFNE      '*'                                          *
073900000000     C     QTAF          IFLE      0                                            *
074000000000     C                   SETON                                        9790      *
074100000000     C                   MOVEL     ERR(9)        ERRORE                         *
074200860702     C                   GOTO      EMICTL                                       *
074300860702     C                   ELSE                                                   *
074400860702     C                   Z-ADD     QTAF          QTAW                           *
074500860702     C                   END                                                    *
074600000000     C                   END                                                    *
074700871214     C*---------- QUANTITA :                               *
074800871222     C     AZ(15)        CABEQ     '*'           NOCTRQ
074900871214     C     QTAF          CABEQ     QTAAF         NOCTRQ
075000871215     C                   MOVE      AZF           SEGGIA            1
075100871214     C     SEGGIA        CABEQ     '+'           NOCTRQ
075200871214     C                   Z-ADD     0             DIFGIA           15 2
075300871214     C     QTAF          IFGT      QTAAF
075400871214     C     QTAF          SUB       QTAAF         DIFGIA
075500871214     C                   END
075600871214     C     DIFGIA        CABEQ     0             NOCTRQ
075700871214     C                   EXSR      RICDIS
075800871214     C     DISPON        COMP      DIFGIA                               97
075900871214     C   97              SETON                                        90
076000871214     C   97              MOVEL     ERR(28)       ERRORE
076100871214     C   97              GOTO      EMICTL
076200871214     C** CONTROLLO DISPONIBILITA MAGAZZINO PER PRELIEVO
076300871214     C     NOCTRQ        TAG
076400871214     C*----------------------------------------------------*
076500000000     C*---------- C/COSTO :                                *
076600000000     C     SNCF          IFEQ      'S'                                          *
076700990120     C     CDCF          IFEQ      '?       '                                   *
076800000000     C                   MOVE      *BLANKS       CDCF                           *
076900000000     C                   MOVE      *ZEROS        CODCDC                         *
077000000000     C                   MOVE      *BLANKS       DESCDC                         *
077100990317     C                   CALL      'TII102R'                                    *
077200000000     C                   PARM                    CODCDC                         *
077300000000     C                   PARM                    DESCDC                         *
077400000000     C                   MOVE      CODCDC        CDCF                           *
077500000000     C                   END                                                    *
077600860702RIC  C                   MOVE      CDCF          CODCDC                         *
077700990120RIC  C     KEYCDC        CHAIN     anent01l                           90        *
077800000000     C  N90CDCF          COMP      '9999999'                              90    *
077900000000     C  N90CDCF          COMP      '0000000'                              90    *
078000000000     C  N90CDCF          COMP      *BLANKS                                90    *
078100000000     C   90              SETON                                            98    *
078200000000     C   90              MOVEL     ERR(4)        ERRORE                         *
078300860702     C   90              GOTO      EMICTL                                       *
078400000000     C                   ELSE                                                   *
078500990120     C                   MOVE      '99999999'    CDCF                           *
078600000000     C                   END                                                    *
078700860702     C***------------------------------------------------***
078800000000     C*---------- CONTROLLO NUM. E DATA DOCUMENTO :        *
078900000000     C     NDOCF         COMP      0                                      90    *
079000000000     C  N90DDOCF         COMP      0                                      90    *
079100000000     C  N90              DO                                                     *
079200990120     C                   move      DDOCF         dataeur                        *
079300990120     C                   move      dataeur       dataiso                        *
079400990120     C                   move      dataiso       DDOCK             8 0          *
079500000000     C                   END                                                    *
079600000000     C   90              SETON                                        87        *
079700000000     C   90              MOVEL     ERR(7)        ERRORE                         *
079800860702     C   90              GOTO      EMICTL                                       *
079900860702     C*---------- PREZZO :                                 *
080000880115     C                   SETOFF                                       9099
080100850403     C                   MOVEA     OPEF          FO                             *
080200880115     C     PRZF          IFLE      0                                            *
080300990120     C     KEYLMP        CHAIN     tmlmp01L                           27        *
080400850403     C  N27              Z-ADD     PRE(COLF)     PRZF                           *
080500880115     C                   END                                                    *
080600850403     C*---------                                           *
080700880115     C     FO(3)         IFEQ      'S'                                          *
080800850403     C     PRZF          COMP      0                                    9090    *
080900850403     C   90              MOVEL     ERR(14)       ERRORE                         *
081000850403     C   90              SETON                                        99        *
081100860702     C   90              GOTO      EMICTL                                       *
081200850403     C                   END                                                    *
081300850314     C*--------- RICERCA ANAGRAFICA -----------------------*
081400990120     C     KEYAMP        CHAIN     tmamp01L                           90        *
081500850314     C   90              SETON                                        96        *
081600850314     C   90              MOVEL     ERR(10)       ERRORE                         *
081700860702     C   90              GOTO      EMICTL                                       *
081800850403     CSR                 ENDSR                                                  *
081900000000     C**--------- RICERCA LISTINO -----------------------***
082000000000     C***------------------------------------------------***
082100000000     CSR   RUTLIS        BEGSR                                                  *
082200000000     C     LIS           IFEQ      '?  '                                        *
082300000000     C                   MOVE      *BLANKS       LIS                            *
082400000000     C                   MOVE      *BLANKS       DS161            75            *
082500990317     C                   CALL      'TII116R'                                    *
082600000000     C                   PARM                    LIS                            *
082700000000     C                   PARM                    DS161                          *
082800000000     C                   END                                                    *
082900000000     C                   MOVE      '161'         TABGRU                         *
083000000000     C                   MOVE      *ZEROS        TABCOD                         *
083100000000     C                   MOVE      LIS           TABCOD                         *
083200000000     C     KEYTAB        CHAIN     ANTAB00F                           90        *
083300000000     CSR                 ENDSR                                                  *
083400000000     C***------------------------------------------------***
083500000000     C**--------- AGGIORNAMENTO SALDI DI MAGAZZINO ------***
083600000000     C***------------------------------------------------***
083700000000     CSR   AGGSAL        BEGSR                                                  *
083800000000     C                   SETOFF                                       919293    *
083900000000     C                   SETOFF                                       94        *
084000990120     C     KEYSMP        CHAIN     tmsmp01L                           79        *
084100000000     C   79              MOVE      ' '           SMPATT                         *
084200990120     C   79              move      xsccut        SMPUTE                         *
084300990120     C                   Z-ADD     woggig        SMPDMN                         *DATA MANUT.
084400000000     C   79              DO                                                     *
084500000000     C                   Z-ADD     ANNO          SMPAAC                         *ANNO COMP.
084600000000     C                   MOVE      MAGF          SMPMAG                         *MAGAZZINO
084700000000     C                   MOVE      ARTF          SMPMAT                         *PRODOTTO
084800000000     C                   MOVE      AMPUMA        SMPUMA                         *U.M. ANAGR.
084900000000     C                   Z-ADD     0             SMPPLF                         *LIFO FISC.
085000000000     C                   Z-ADD     0             SMPPLV                         *LIFO VAR.
085100000000     C                   Z-ADD     0             SMPQGI                         *GIAC.INIZ.
085200000000     C                   Z-ADD     0             SMPQOR                         *QTA ORDINI
085300000000     C                   Z-ADD     0             SMPQPR                         *QTA PRODUZ.
085400000000     C                   Z-ADD     0             SMPQIM                         *QTA IMPEG.
085500870518     C                   Z-ADD     0             SMPQIP                         *QTA IMPEG.
085600000000     C                   Z-ADD     0             SMPQGF                         *GIAC.FINALE
085700000000     C                   Z-ADD     0             SMPQE1                         *QTA E-1
085800000000     C                   Z-ADD     0             SMPQE2                         *QTA E-2
085900000000     C                   Z-ADD     0             SMPQE3                         *QTA E-3
086000000000     C                   Z-ADD     0             SMPQE4                         *QTA E-4
086100000000     C                   Z-ADD     0             SMPQE5                         *QTA E-5
086200000000     C                   Z-ADD     0             SMPQU1                         *QTA U-1
086300000000     C                   Z-ADD     0             SMPQU2                         *QTA U-2
086400000000     C                   Z-ADD     0             SMPQU3                         *QTA U-3
086500000000     C                   Z-ADD     0             SMPQU4                         *QTA U-4
086600000000     C                   Z-ADD     0             SMPQU5                         *QTA U-5
086700000000     C                   Z-ADD     DATMOV        SMPDMS                         *DATA SALDO
086800000000     C                   Z-ADD     0             SMPNMS                         *MOV. SALDO
086900000000     C                   Z-ADD     0             SMPPMC                         *PRZ. MEDIO
087000000000     C                   MOVE      *BLANKS       SMPFLG                         *FLAGS
087100990311     C                   MOVE      valf          SMPval                         *FLAGS
087200990311     C                   Z-ADD     1             SMPcmb                         *PRZ. MEDIO
087300000000     C                   END                                                    *
087400000000     C*---------- CONVERSIONE QUANTITA' :                  *
087500000000     C     QTAW          MULT(H)   FCVF          QTAANA           15 5          *
087600000000     C*----------------------------------------------------*
087700000000     C*---------- INVENTARIO ------------------------------*
087800000000     C     AZ(15)        IFEQ      '*'                                          *
087900000000     C*---------- ESEGUO DIFFERENZA :                      *
088000000000     C     SMPQGF        CABEQ     QTAANA        SUFOND               9192      *
088100000000     C   91SMPQGF        SUB       QTAANA        QTAANA                         *
088200000000     C   92QTAANA        SUB       SMPQGF        QTAANA                         *
088300000000     C                   MOVEA     *BLANKS       AZ                             *
088400000000     C   92              MOVE      '+'           AZ(5)                          *ENTRATE
088500000000     C   91              MOVE      '+'           AZ(10)                         *USCITE
088600000000     C   92              MOVE      '+'           AZ(15)                         *GIACENZA +
088700000000     C   91              MOVE      '-'           AZ(15)                         *GIACENZA -
088800000000     C   91              MOVE      'RI-'         CAUF                           *
088900000000     C   92              MOVE      'RI+'         CAUF                           *
089000000000     C                   MOVEA     AZ            AZF                            *
089100000000     C                   Z-ADD     0             RELRF                          *
089200000000     C                   Z-ADD     0             RELVF                          *
089300000000     C                   END                                                    *
089400000000     C*--------- AGGIORNAMENTO ENTRATA --------------------*
089500000000     C     1             DO        5             X                 2 0          *
089600000000     C     AZ(X)         COMP      '+'                                    92    *
089700000000     C     AZ(X)         COMP      '-'                                    91    *
089800000000     C   92QTK(X)        ADD       QTAANA        QTK(X)                         *
089900000000     C   91QTK(X)        SUB       QTAANA        QTK(X)                         *
090000000000     C                   END                                                    *
090100000000     C*--------- AGGIORNAMENTO USCITA ---------------------*
090200000000     C     6             DO        10            X                              *
090300000000     C     AZ(X)         COMP      '+'                                    92    *
090400000000     C     AZ(X)         COMP      '-'                                    91    *
090500000000     C   92QTK(X)        ADD       QTAANA        QTK(X)                         *
090600000000     C   91QTK(X)        SUB       QTAANA        QTK(X)                         *
090700000000     C                   END                                                    *
090800000000     C*--------- AGGIORNAMENTO GIACENZA -------------------*
090900000000     C     AZ(15)        COMP      '+'                                    92    *
091000000000     C     AZ(15)        COMP      '-'                                    91    *
091100000000     C   92SMPQGF        ADD       QTAANA        SMPQGF                         *GIACENZA +
091200000000     C   91SMPQGF        SUB       QTAANA        SMPQGF                         *GIACENZA -
091300000000     C*---------- AGGIORNAMENTO SALDI :                    *
091400990120     C   79              WRITE     tmsmp000                                     *UPDATE
091500990120     C  N79              UPDATE    tmsmp000                                     *UPDATE
091600000000     C*---------- CONVERSIONE QUANTITA' :                  *
091700000000     C     QTAANA        DIV(H)    FCVF          QTAW                           * Q.TA' PROMPT
091800870921     C**-----
091900870921     C     KMAT          KLIST
092000870921     C                   KFLD                    SMPMAT
092100870921     C     AZ(2)         IFEQ      '+'
092200990120     C     KMAT          CHAIN     tmamp000                           89
092300870921     C  N89              DO
092400870921     C     AMPTAR        IFLT      DATMOV
092500870921     C                   Z-ADD     DATMOV        AMPTAR
092600870921     C                   END
092700990120     C                   UPDATE    tmamp000
092800870921     C                   END
092900870921     C** SE MOVIMENTO USCITA PER VENDITA
093000870921     C** AGGIORNO DATA ULTIMA USCITA PER VENDITA
093100870921     C** SU ANAGRAFICO MATERIALE
093200870921     C                   END
093300000000     CSR   ENDAGG        ENDSR                                                  *
093400000000     C**--------- AGGIORNAMENTO MOVIMENTI DI MAGAZZINO --***
093500000000     C***------------------------------------------------***
093600000000     CSR   AGGMOV        BEGSR                                                  *
093700000000     C                   MOVE      ' '           MMPATT                         *ATTIVITA'
093800990120     C                   MOVE      xsccut        MMPUTE                         *UTENTE
093900990120     C                   Z-ADD     woggig        MMPDMN                         *DATA MANUT.
094000000000     C                   MOVE      MAGF          MMPMAG                         *MAGAZZINO
094100000000     C                   Z-ADD     ANNO          MMPAAC                         *ANNO
094200000000     C                   Z-ADD     DATMOV        MMPDMV                         *DATA MOVIM.
094300000000     C                   Z-ADD     NUMMOV        MMPNMV                         *NUM. MOVIM.
094400000000     C                   Z-ADD     DDOCK         MMPDDC                         *DATA DOCUM.
094500000000     C                   Z-ADD     NDOCF         MMPNDC                         *NUM. DOCUM.
094600000000     C                   MOVE      CAUF          MMPCAU                         *CAUSALE
094700000000     C                   MOVE      AZF           MMPOPC                         *AZIONI
094800000000     C                   MOVE      CDCF          MMPCDC                         *C/COSTO
094900000000     C                   MOVE      ARTF          MMPMAT                         *PRODOTTO
095000000000     C                   MOVE      AMPUMA        MMPUMA                         *U.M.ANAG.
095100000000     C                   Z-ADD     QTAANA        MMPQTA                         *QUANTITA'
095200860818     C     PRZF          DIV       FCVF          MMPPRZ                         *PREZZO
095300000000     C                   MOVE      MCPF          MMPMCP                         *MAG.C/P.
095400000000     C                   MOVE      QF            MMPFGQ                         *MOV.A QUANTITA
095500000000     C                   MOVE      VF            MMPFGV                         *MOV.A VALORE
095600000000     C                   MOVE      TF            MMPFGT                         *TIPO MAGAZZINO
095700000000     C                   Z-ADD     0             MMPCRG                         *COD.IND.RIGA
095800990311     c                   move      valf          mmpval
095900990311     c                   z-add     1             mmpcmb
096000871201     C** PER COMMESSA SI INCREMENTA COD,IND.RIGA
096100990120     C  N22              WRITE     tmmmp000                                     *WRITE
096200000000     C  N22              Z-ADD     NREL1         RELRF                          *
096300850611     C                   SETON                                            28    *STAMPA
096400000000     CSR                 ENDSR                                                  *
096500000000     C***------------------------------------------------***
096600000000     C**--------- CARICAMENTO DEL VIDEO -----------------***
096700000000     C***------------------------------------------------***
096800000000     CSR   AGGVID        BEGSR                                                  *
096900000000     C     RELVF         COMP      0                                      78    *
097000000000     C   78RECT          ADD       1             RECT                           *
097100000000     C   78              Z-ADD     RECT          REC1                           *
097200000000     C  N78              Z-ADD     RELVF         REC1                           *
097300000000     C  N78REC1          CHAIN     SFL1                               78        *
097400000000     C                   Z-ADD     RELRF         RELREC                         *
097500000000     C                   Z-ADD     REC1          RELVID               71        *
097600000000     C                   Z-ADD     REC1          CURSOR                         *
097700000000     C                   MOVE      FCVF          FCV                            *
097800000000     C     SCEF          IFEQ      'A'                                          *
097900000000     C                   MOVE      SCEF          SCELTW                         *
098000000000     C                   ELSE                                                   *
098100000000     C                   MOVE      *BLANKS       SCELTW                         *
098200000000     C                   END                                                    *
098300000000     C                   MOVE      SCELTW        SCELTA                         *
098400000000     C                   MOVE      CSF           FLGCS                          *
098500000000     C                   MOVEL     DESF          DESART                         *
098600000000     C                   MOVE      UMSF          UMI                            *
098700000000     C                   Z-ADD     QTAW          QTAV                           *
098800000000     C                   MOVE      LISF          LISCOD                         *
098900000000     C                   Z-ADD     COLF          LISCOL                         *
099000000000     C                   Z-ADD     PRZF          PRZV                           *
099100000000     C                   MOVE      CDCF          MMPCDC                         *
099200850314     C                   Z-ADD     DDOCK         MMPDDC                         *
099300850314     C                   Z-ADD     NDOCF         MMPNDC                         *
099400000000     C   78              WRITE     SFL1                                         *
099500000000     C  N78              UPDATE    SFL1                                         *
099600000000     CSR                 ENDSR                                                  *
099700990120     C*----------------------------------------------------*
099800990120     C* Reperimento dati società
099900990120     C*----------------------------------------------------*
100000990120     C     REPSOC        BEGSR
100100990120     C*
100200990120     C                   CALLB     'XSOC'
100300990120     C                   PARM                    TIPXSC            6
100400990120     C                   PARM                    SOCXSC            3
100500990120     C                   PARM                    CDSXSC            9 0
100600990120     C                   PARM                    MODXSC            3
100700990120     C                   PARM      *blanks       RTNXSC            1
100800990120     C                   PARM                    XSOCDS
100900990120     C                   PARM                    KPJBA
101000990120     C*
101100990120     C                   ENDSR
101200990120     Otmmmp000  E            ANNMMP
101300000000     O                       MMPATT
101400910712**    ERR
101500000000*- Anno non VALIDO !!!!!!!! -*      ERR,1
101600000000*- Codice Magazzino  ERRATO -*      ERR,2
101700000000*- Codice Causale    ERRATO -*      ERR,3
101800000000*- Codice C/Costo    ERRATO -*      ERR,4
101900000000*- Lotto o Commessa  ERRATO -*      ERR,5
102000000000*- Listino    non    VALIDO -*      ERR,6
102100000000*- Documento o Data  ERRATI -*      ERR,7
102200000000*-Non Ammesse Causali UGUALI-*      ERR,8
102300000000*- Inserire Dati Mancanti ! -*      ERR,9
102400000000*- Codice Prodotto ERRATO ! -*      ERR,10
102500000000*- Causale non di SCARICO   -*      ERR,11
102600000000*- Causale non di CARICO    -*      ERR,12
102700000000*E' ammesso solo un Magazzino*      ERR,13
102800000000*- Listino/prezzo non Valido-*      ERR,14
102900850403*- Data Movimento ERRATA !! -*      ERR,15
103000850611*- CAMBIO DATA NON AMMESSO !-*      ERR,16
103100871201*- N.COMMESSA OBBLIGATORIO !-*      ERR,17
103200871201*- N.COMMESSA INESISTENTE  !-*      ERR,18
103300871201*- STATO COMMESSA NON VALIDO *      ERR,19
103400871201*- DATA REG < DATA CONG.COM!-*      ERR,20
103500871201*- TIPO ATT.COSTI NON VALIDO-*      ERR,21
103600871202*- C O S T O  OBBLIGATORIO  -*      ERR,22
103700871203*- PREZZO VENDITA = ZERO    -*      ERR,23
103800871214CARICO CLIENTE                          24
103900871214CARICO FORNITORE                        25
104000871214CARICO CONCESSIONARIA                   26
104100871214ASS.CLIENTE                             27
104200871214MANCA DISPONIBILITA MAGAZZINO           28
104300910311ACCESSO NEGATO ALLA CAUSALE !!          29
104400910712*- MATRICOLA OBBLIGATORIA ! -*          30
104500910712 ATTREZZATURA GIA' IN CARICO            31

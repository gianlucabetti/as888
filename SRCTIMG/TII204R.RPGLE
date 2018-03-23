000100990122     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990122     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300850114     F* MPH119R *-MP/38-*--------------------------------------------*
000400850114     F*-----------------*                                            *
000500850114     F*       - STAMPA MOVIMENTI DI MAGAZZINO M.P. -                 *
000600850114     F*       --------------------------------------                 *
000700850114     F*                                                   AZN.: H119 *
000800990122     F*         FILE : MPPRTSTD-tmamp01L-tmmmp01L-ANTAB00F           *
000900850114     F*                                                              *
001000850114     F*         PGM. : RIC001R   - RICERCA INTESTAZIONE DITTA        *
001100850114     F*                                                              *
001200850114     F*--------------------------------------------------------------*
001300990122     Ftmamp01L  IF   E           K DISK
001400990122     Ftmmmp01L  IF   E           K DISK
001500850114     FANTAB00F  IF   E           K DISK
001600850114     FMPPRTSTD  O    F  132        PRINTER OFLIND(*INOF)
001700850114     D TUM             S              1    DIM(4) CTDATA PERRCD(1)              *TIPI U.M.
001800850114     D DUM             S             10    DIM(4) ALT(TUM)
001900850114     D ERR             S             50    DIM(3) CTDATA PERRCD(1)              *ERRORI
002000850114     D AZ              S              1    DIM(15)                              *AZ.CAUSALE   CIT
002100850114     D SK1             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 1
002200850114     D SK2             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 2
002300850114     D SK3             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 3
002400850114     D SK4             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 4
002500850114     D SK5             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 5
002600850114     D SK6             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 6
002700850114     D TB153         E DS                  EXTNAME(ANTAB153)
002800850114     D KPJBA         E DS
002900990122     D PH119           DS
003000990122     D*----- PASSAGGIO PARAMETRI X STAMPA MOVIMENTI :
003100990122     D  AAA119                 1      4  0
003200990122     D  MAG119                 5     11
003300990122     D  DAL119                12     19  0
003400990122     D  AL119                 20     27  0
003500990122     D  UMI119                31     31
003600990122     D  CAU119                32     46
003700990122     D  KAU                   32     46
003800990122     D                                     DIM(5)                               *CAUSALI
003900990122     d  tcau                  32     46
004000990122     D  CDC119                47     54
004100990122     D  STA119                55     55
004200990122     D  MOV119                56     60  0
004300990122     D  MOVk                  56     60
004400990122     D  MAT119                61     72
004500990122     d  desdit               187    206
004600990122     D  MSGERR               207    256
004700990122     d dataiso         s               d   datfmt(*iso)
004800990122     d dataeur         s               d   datfmt(*eur)
004900850114     C*----------------------------------------------------*
005000850114     C     *ENTRY        PLIST                                                  *
005100850114     C                   PARM                    KPJBA                          *
005200990122     c                   movel     kpjbu         ph119
005300850114     C*---------- ANAGRAFICO MATERIE PRIME :               *
005400850114     C     KEYAMP        KLIST                                                  *
005500850114     C                   KFLD                    MMPMAT                         *
005600850114     C*---------- MOVIMENTI DI MAGAZZINO :                 *
005700850114     C     KEYMMP        KLIST                                                  *
005800990122     C                   KFLD                    AAa119                         *
005900851120     C                   KFLD                    MAGAZZ            7            *
006000871014     C                   KFLD                    CODICE           12            *
006100850114     C*---------- ANAGRAFICO TABELLE :                     *
006200850114     C     KEYTAB        KLIST                                                  *
006300850114     C                   KFLD                    TABGRU                         *
006400850114     C                   KFLD                    TABCOD                         *
006500871014     C                   MOVEL     '?'           INTERR           12
006600871014     C                   MOVEL     '99999999'    NOVE             12
006700861230     C                   MOVE      '99999999'    NOVE
006800851120     C*---------- IMPOSTO DATI X POSIZIONAMENTO :          *
006900861230     C     MAG119        IFEQ      '9999999'                                    *
007000851120     C                   MOVE      *BLANKS       MAGAZZ                         *
007100851120     C                   ELSE                                                   *
007200851120     C                   MOVE      MAG119        MAGAZZ                         *
007300851120     C                   END                                                    *
007400851120     C*----------                                          *
007500861230     C     MAT119        IFEQ      NOVE                                         *
007600851120     C                   MOVE      *BLANKS       CODICE                         *
007700851120     C                   ELSE                                                   *
007800851120     C                   MOVE      MAT119        CODICE                         *
007900851120     C                   END                                                    *
008000851120     C*----------                                          *
008100850114     C*----------                                          *
008200850114     C                   Z-ADD     0             PAGE              4 0          *
008300850320     C                   Z-ADD     51            PAGE1             3 0          *
008400850114     C                   MOVE      'XXXXXXX'     OLDMAG            7            *
008500871014     C                   MOVE      'XXXXXXX'     OLDMAT           12            *
008600861230     C                   MOVEL     'XXXXXXX'     OLDMAT
008700850114     C                   MOVE      'XXX'         OLDCAU            3            *
008800850509     C                   MOVE      *ALL'9'       WCAU             15            *
008900850509     C*---------- STAMPA DETTAGLIO :                       *
009000850509     C     STA119        COMP      'D'                                    08    *
009100850509     C     TCAU          COMP      *ALL'9'                                09    *
009200850509     C     CDC119        COMP      '9999999'                              10    *
009300850114     C*---------- INVERSIONE DATA DAL :                    *
009400990122     C                   MOVE      DAL119        DATAeur                        *
009500990122     c                   move      dataeur       dataiso
009600990122     c                   move      dataiso       datdal            8 0
009700850114     C*---------- INVERSIONE DATA AL  :                    *
009800990122     C                   MOVE      AL119         DATAeur                        *
009900990122     c                   move      dataeur       dataiso
010000990122     c                   move      dataiso       datal             8 0
010100850114     C*---------- RICERCA TIPO UNITA DI MISURA :           *
010200850114     C                   Z-ADD     1             X                 3 0          *
010300850114     C     UMI119        LOOKUP    TUM(X)                                 91    *
010400850114     C*----------------------------------------------------*
010500850114     C     MOVK          IFEQ      *BLANKS                                      *
010600850114     C                   MOVE      *ZEROS        MOV119                         *
010700850114     C                   END                                                    *
010800850114     C     MOV119        COMP      0                                  11        *
010900850114     C*---------- RICERCA MOVIMENTI :                      *
011000990122     C     KEYMMP        SETLL     tmmmp01L                           90        *
011100850114     C   90              MOVEL     ERR(2)        MSGERR                         *NO MOVIMENTI
011200850114     C   90              GOTO      FINE                                         *
011300850114     C     RICMOV        TAG                                                    *
011400990122     C                   READ      tmmmp000                               95    *
011500850114     C     ENDMOV        TAG                                                    *
011600850114     C   95
011700850114     CAN 71              EXSR      RUTSAL                                       *
011800850114     C   95
011900850114     CAN 71
012000850114     CANN97              EXCEPT    TOTAL                                        *
012100850114     C   95
012200850114     CAN 71              GOTO      FINE                                         *
012300850114     C   95
012400850114     CANN71              SETON                                        90        *
012500850114     C   90              MOVEL     ERR(2)        MSGERR                         *NO MOVIMENTI
012600850114     C   90              GOTO      FINE                                         *
012700850114     C*---------- CONTROLLO MAGAZZINO :                    *
012800850114     C     MAG119        IFNE      '9999999'                                    *
012900850114     C     MMPMAG        CABGT     MAG119        ENDMOV               95        *
013000850114     C                   END                                                    *
013100850114     C*---------- CONTROLLO PRODOTTO :                     *
013200861230     C     MAT119        IFNE      NOVE                                         *
013300851120     C     MAG119        IFNE      '9999999'                                    *
013400851120     C     MMPMAT        CABGT     MAT119        ENDMOV               95        *
013500851120     C                   END                                                    *
013600851120     C     MAG119        IFEQ      '9999999'                                    *
013700851120     C     MMPMAT        CABNE     MAT119        RICMOV                         *
013800851120     C                   END                                                    *
013900850114     C                   END                                                    *
014000850114     C*---------- CONTROLLO DATA MOVIMENTO :               *
014100850114     C     MMPDMV        CABLT     DATDAL        RICMOV                         *
014200850114     C     MMPDMV        CABGT     DATAL         RICMOV                         *
014300850114     C*---------- CONTROLLO NUM. MOVIMENTO :               *
014400850114     C   11MMPNMV        CABNE     MOV119        RICMOV                         *
014500850114     C*---------- CONTROLLO CAUSALE :                      *
014600850509     C     TCAU          IFNE      WCAU                                         *
014700990122     C     MMPCAU        LOOKUP    KAU                                    27    *
014800850509     C  N27              GOTO      RICMOV                                       *
014900850114     C                   END                                                    *
015000850509     C*---------- CONTROLLO C/COSTO :                      *
015100990122     C     CDC119        IFNE      '99999999'                                   *
015200850509     C     MMPCDC        CABNE     CDC119        RICMOV                         *
015300850509     C                   END                                                    *
015400850320     C*---------- ROTTURA DI MAGAZZINO :                   *
015500850320     C                   MOVEA     MMPOPC        AZ                             *
015600850320     C     MMPMAG        IFNE      OLDMAG                                       *
015700850320     C                   MOVE      'XXXXXXX'     OLDMAT                         *
015800861230     C                   MOVEL     'XXXXXXX'     OLDMAT                         *
015900850320     C                   Z-ADD     0             PAGE2             4 0    26    *
016000850320     C                   END                                                    *
016100850114     C*---------- EMISSIONE MOVIMENTI ---------------------*
016200850114     C*---------- ROTTURA ARTICOLO :                       *
016300850114     C     MMPMAT        IFNE      OLDMAT                                       *
016400850114     C   05              EXSR      RUTSAL                                       *
016500850114     C   05
016600850114     CANN97              EXCEPT    TOTAL                                        *
016700850114     C                   Z-ADD     0             TOTDEN           11 2          *
016800850114     C                   Z-ADD     0             TOTDUS           11 2          *
016900850114     C                   Z-ADD     0             TOTSAL           11 2          *
017000850114     C                   SETOFF                                         0597    *
017100850320     C                   SETON                                            25    *
017200850320     C   26              Z-ADD     50            PAGE1                    26    *
017300850114     C                   END                                                    *
017400850114     C*---------- ROTTURA DI ARTICOLO/VERSIONE :           *
017500861230     C   05              EXSR      RUTSAL                                       *
017600861230     C                   MOVE      MMPMAT        OLDMAT                         *
017700990122     C     KEYAMP        CHAIN     tmamp01L                           92        *
017800861230     C   92              GOTO      RICMOV                                       *
017900861230     C                   MOVE      AMPDMT        DESART           30            *
018000861230     C                   MOVE      AMPCLM        CLASSE            3            *
018100861230     C                   MOVE      AMPSCM        SCLASS            5            *
018200861230     C     UMI119        COMP      'A'                                    95    *UNITA MISURA
018300861230     C   95              MOVE      AMPUMA        OLDUMI            3            *ANAGRAFICA
018400861230     C   95              Z-ADD     1             FCV              11 5    96    *
018500861230     C     UMI119        COMP      'Q'                                    95    *           A
018600861230     C   95              MOVE      AMPUMQ        OLDUMI                         *ACQUISTO
018700861230     C   95              Z-ADD     AMPFCQ        FCV                      96    *
018800861230     C     UMI119        COMP      'S'                                    95    *           A
018900861230     C   95              MOVE      AMPUMS        OLDUMI                         *STOCCAGGIO
019000861230     C   95              Z-ADD     AMPFCS        FCV                      96    *
019100861230     C     UMI119        COMP      'V'                                    95    *           A
019200861230     C   95              MOVE      AMPUMV        OLDUMI                         *VENDITA
019300861230     C   95              Z-ADD     AMPFCV        FCV                      96    *
019400861230     C  N05              MOVE      OLDUMI        UMIT              3            *
019500861230     C     MMPMAG        IFNE      OLDMAG                                       *
019600861230     C                   MOVE      '153'         TABGRU                         *
019700861230     C                   MOVE      *ZEROS        TABCOD                         *
019800861230     C                   MOVE      MMPMAG        TABCOD                         *
019900861230     C     KEYTAB        CHAIN     ANTAB00F                           91        *
020000861230     C                   MOVE      MMPMAG        OLDMAG                         *
020100861230     C   91              MOVEL     *ALL'*'       DESMAG                         *
020200861230     C  N91              MOVEL     TABDES        TB153                          *
020300861230     C  N91              MOVE      DES153        DESMAG           30            *
020400861230     C  N91              MOVE      TIG153        OLDTIG            2            *
020500861230     C  N91              MOVE      TIM153        OLDTIM            1            *
020600990318     C  N91              MOVEl     CDC153        OLDCDC            8            *
020700861230     C                   END                                                    *
020800861230     C                   SETON                                            05    *
020900850114     C*----------------------------------------------------*
021000850114     C/EJECT
021100850114     C*---------- IMPOSTO ENTRATE/USCITE ------------------*
021200850114     C  N96MMPQTA        DIV(H)    FCV           PIPPO            11 2          *
021300850114     C     AZ(15)        IFEQ      '+'                                          *ENTRATE
021400850114     C                   Z-ADD     PIPPO         ENTRAT           11 2          *
021500850114     C                   Z-ADD     0             USCITE           11 2          *
021600850114     C     TOTDEN        ADD       PIPPO         TOTDEN                         *TOT.ENTRATE
021700850114     C     TOTSAL        ADD       PIPPO         TOTSAL                         *
021800850114     C     ENTVER        ADD       PIPPO         ENTVER                         *TOT.ENTRATE
021900850114     C     SALVER        ADD       PIPPO         SALVER                         *
022000850114     C                   END                                                    *
022100850114     C     AZ(15)        IFEQ      '-'                                          *USCITE
022200850114     C                   Z-ADD     PIPPO         USCITE                         *
022300850114     C                   Z-ADD     0             ENTRAT                         *
022400850114     C     TOTDUS        ADD       PIPPO         TOTDUS                         *TOT.USCITE
022500850114     C     TOTSAL        SUB       PIPPO         TOTSAL                         *
022600850114     C     USCVER        ADD       PIPPO         USCVER                         *TOT.ENTRATE
022700850114     C     SALVER        SUB       PIPPO         SALVER                         *
022800850114     C                   END                                                    *
022900850114     C     OLDUMI        IFNE      UMIT                                         *
023000850114     C                   MOVE      '*?*'         UMIT                           *
023100850114     C                   SETON                                            97    *NO TOTALI
023200850114     C                   END                                                    *
023300850114     C*---------- INVERSIONE DATE :                        *
023400990122     c   08              do
023500990122     c                   if        mmpdmv > 0
023600990122     C                   move      MMPDMV        DATAiso                        *DATA MOVIM.
023700990122     C                   move      dataiso       dataeur                        *
023800990122     C                   move      dataeur       datdmv            8 0          *
023900990122     c                   end
024000990122     c                   if        mmpddc > 0
024100990122     C                   move      MMPDDC        DATAiso                        *DATA DOCUM.
024200990122     C                   move      dataiso       dataeur                        *
024300990122     C                   move      DATAeur       DATDDC            8 0          *
024400990122     c                   end
024500850114     C*---------- PREPARAZIONE STAMPA ---------------------*
024600990122     C                   Z-ADD     MMPNMV        NUMNMV            5 0          *
024700990122     C                   Z-ADD     MMPNDC        NUMNDC            5 0          *
024800850114     C*---------- DECODIFICA CAUSALE :                     *
024900990122     C     MMPCAU        IFNE      OLDCAU                                       *
025000850114     C                   MOVE      '152'         TABGRU                         *
025100850114     C                   MOVE      *ZEROS        TABCOD                         *
025200850114     C                   MOVEL     'MP'          CINQUE            5            *
025300850114     C                   MOVE      MMPCAU        CINQUE                         *
025400850114     C                   MOVE      CINQUE        TABCOD                         *
025500850114     C     KEYTAB        CHAIN     ANTAB00F                           92        *
025600990122     C  N92              MOVEL     TABDES        DESCAU           20            *
025700850114     C   92              MOVE      *ALL'*'       DESCAU                         *
025800850114     C                   MOVE      MMPCAU        OLDCAU                         *
025900850114     C                   END                                                    *
026000990122     C                   MOVE      MMPCDC        MCPV              8            *
026100990122     C                   Z-ADD     MMPPRZ        PREZZO           11 2          *
026200990122     C                   EXSR      RUTSTA                                       *
026300990122     c                   enddo
026400850114     C                   SETON                                            71    *
026500850114     C                   GOTO      RICMOV                                       *
026600850114     C*----------                                          *
026700850114     C     FINE          TAG                                                    *
026800850114     C   90              MOVEL     'KPJB207'     KMSMN                          *
026900850114     C                   SETON                                            LR    *
027000850114     C**--------- EMISSIONE SALDO DI VERSIONE -----------***
027100850114     C***------------------------------------------------***
027200850114     CSR   RUTSAL        BEGSR                                                  *
027300850114     C                   SETON                                          98      *
027400850114     C                   Z-ADD     0             DATDMV                         *
027500850114     C                   Z-ADD     0             NUMNMV                         *
027600850114     C                   Z-ADD     0             DATDDC                         *
027700850114     C                   Z-ADD     0             NUMNDC                         *
027800850114     C                   MOVE      *BLANKS       OLDCAU                         *
027900850114     C                   MOVEL     ERR(3)        DESCAU                         *
028000850114     C                   Z-ADD     ENTVER        ENTRAT                         *
028100850114     C                   Z-ADD     USCVER        USCITE                         *
028200850114     C*----------                                          *
028300850509     C                   SETOFF                                         98      *
028400850114     C                   Z-ADD     0             ENTVER           11 2          *
028500850114     C                   Z-ADD     0             USCVER           11 2          *
028600850114     C                   Z-ADD     0             SALVER           11 2          *
028700850114     C                   MOVE      'KKK'         OLDCAU                         *
028800850114     CSR                 ENDSR                                                  *
028900850114     C**--------- RUTINE EMISSIONE STAMPA ---------------***
029000850114     C***------------------------------------------------***
029100850114     CSR   RUTSTA        BEGSR                                                  *
029200850320     C     PAGE1         IFGE      50                                           *
029300850114     C                   EXCEPT    TESTA                                        *
029400850320     C                   EXCEPT    TESTA1                                       *
029500850320     C                   Z-ADD     0             PAGE1                25        *
029600850114     C                   END                                                    *
029700850320     C   25              EXCEPT    TESTA1                                       *
029800850114     C                   EXCEPT    DETAIL                                       *
029900850320     C                   SETOFF                                           25    *
030000850114     CSR                 ENDSR                                                  *
030100850114     O*--------------------------------------------------------------*
030200850114     O*     TESTATA                                                  *
030300850114     O*--------------------------------------------------------------*
030400850114     OMPPRTSTD  E            TESTA            01
030500850114     O                       SK1                132
030600850114     O                       DESDIT              20
030700850114     O                       UDATE         Y    118
030800850114     O                       PAGE          Z    132
030900850114     O          E            TESTA       1
031000850114     O                       SK2                132
031100990122     O                       DAL119              89 '  /  /    '
031200990122     O                       AL119              103 '  /  /    '
031300850114     O          E            TESTA       1
031400850114     O                       SK3                132
031500850114     O                       KNMUS              132
031600850509     O          E            TESTA       1
031700850320     O                                            4 'Pag.'
031800850320     O                       PAGE2         Z      8
031900990122     O                                           41 'Anno      Magazzino :'
032000850114     O                       AAA119              29
032100850114     O                       OLDMAG              49
032200850114     O                       DESMAG              80
032300850114     O                       OLDTIG              84
032400850114     O                       OLDTIM              86
032500850114     O                                           92 'CdC.:'
032600850114     O                       OLDCDC             100
032700850320     O                                          119 'U.Mis.:'
032800850320     O                       UMI119             121
032900850320     O                                          122 '-'
033000850320     O                       DUM(X)             132
033100850509     O          E            TESTA       1
033200850509     O                                            9 'CAUSALI :'
033300850509     O               09                        +  2 'Tutte'
033400990122     O              N09      kau(1)            +  2
033500990122     O              N09      kau(2)            +  2
033600990122     O              N09      kau(3)            +  2
033700990122     O              N09      kau(4)            +  2
033800990122     O              N09      kau(5)            +  2
033900850509     O                                          119 'C/COSTO :'
034000990122     O              N10      CDC119             128
034100850509     O               10                         125 'Tutti'
034200850509     O          E            TESTA1      2
034300871014     O                       PAGE1                9
034400871014     O                                            9 'Prodotto:'
034500861230     O                       OLDMAT              22
034600861230     O                       DESART            +  1
034700850509     O                                          109 'Classe :'
034800850509     O                       CLASSE             113
034900850509     O                                          126 'S/Classe :'
035000850509     O                       SCLASS             132
035100850509     O          E            TESTA1      0  0
035200850320     O                       PAGE1               19
035300861230     O                       OLDMAT              22
035400861230     O                       DESART            +  1
035500850509     O          E            TESTA1      1
035600850320     O                       PAGE1              132
035700850114     O                       SK4                132
035800850509     O                  08                       13 'Movimento'
035900850509     O                  08                       30 'Documento'
036000990122     O                  08                       42 'Cod.'
036100850509     O                  08                       53 'Causale'
036200850509     O                  08                      130 'C/Costo'
036300850114     O*--------------------------------------------------------------*
036400850114     O*     DETTAGLIO                                                *
036500850114     O*--------------------------------------------------------------*
036600850509     O          E    98 08   DETAIL      1
036700850114     O                       PAGE1                9
036800850114     O                       SK6                132
036900850114     O          E            DETAIL      1
037000850114     O                       SK5                132
037100850114     O                       PAGE1                9
037200990122     O                       DATDMV         B    10 '  /  /    '
037300990122     O                       NUMNMV        ZB    16
037400990122     O               98                          18 ' '
037500990122     O                       DATDDC         B    29 '  /  /    '
037600990122     O                       NUMNDC        ZB    35
037700990122     O               98                          37 ' '
037800990122     O                       OLDCAU              41
037900990122     O                       DESCAU              63
038000990122     O               98      DESCAU         B    63
038100990122     O                       OLDUMI              74
038200990122     O                       ENTRAT         B    90 '   .   .   ,  -'
038300990122     O                       USCITE         B   106 '   .   .   ,  -'
038400990122     O              N98      PREZZO         B   121 '   .   .   ,  '
038500990122     O               98      SALVER             122 '   .   . 0 ,  -'
038600990122     O               98                         129 'Saldo'
038700990122     O              N98      MCPV           B   131
038800850509     O          E    98 08   DETAIL      1
038900850114     O                       PAGE1                9
039000850114     O                       SK6                132
039100850114     O*--------------------------------------------------------------*
039200850114     O*     TOTALE                                                   *
039300850114     O*--------------------------------------------------------------*
039400850509     O          E      N08   TOTAL       1
039500850509     O                       PAGE1                9
039600850509     O                       SK6                132
039700850114     O          E            TOTAL       1
039800850320     O                       PAGE1              132
039900850114     O                       SK5                132
040000850509     O                                           17 ' '
040100850509     O                                           34 ' '
040200850509     O                                           58 '******* T O T A L E'
040300850114     O                                           70 ':     '
040400850114     O                       UMIT                74
040500850114     O                       TOTDEN              90 '   .   . 0 ,  -'
040600850114     O                       TOTDUS             106 '   .   . 0 ,  -'
040700850130     O                       TOTSAL             122 '   .   . 0 ,  -'
040800850130     O                                          129 'Saldo'
040900850320     O          E            TOTAL       0  0
041000850509     O                                           58 '******* T O T A L E'
041100850320     O                       UMIT                74
041200850320     O                       TOTDEN              90 '   .   . 0 ,  -'
041300850320     O                       TOTDUS             106 '   .   . 0 ,  -'
041400850320     O                       TOTSAL             122 '   .   . 0 ,  -'
041500850320     O                                          129 'Saldo'
041600850114     O          E            TOTAL       1
041700850320     O                       PAGE1              132
041800850114     O                       SK6                132
041900850114     O*--------------------------------------------------------------*
042000850114**    ERR                       ERRORI
042100850114AANAGRAFICA
042200850114QACQUISTO
042300850114SSTOCCAGGIO
042400850114VVENDITA
042500850114**    ERR                       ERRORI
042600850114*- Cod. DITTA non Valido - Fare inserimento     -* Err,1
042700850114*- Non  Trovati Movimenti riferentesi a Richiesta* Err,2
042800850114******* SALDO Versione  :                       -* Err,3
042900850114**   SK1
043000850114*-DESDIT-----------****-------------------------------------------SK01
043100850114-------------------------------------*** il XX/XX/XX      Pag. XXXSK01
043200850114**   SK2
043300850509                    ** Gest.Magazz. -M.P.- STAMPA dei MOVIMENTI diSK02
043400990317 MAGAZZINO dal XX.XX.XX al XX.XX.XX   **        PGM.:   TII204R   SK02
043500850114**   SK3
043600850114                    ***-------------------------------------------SK03
043700850114-------------------------------------***        Utente  XXXXXXXXXXSK03
043800850114**   SK4
043900850509*---------------*----------------*------------------------------*VSK04
044000850509er.*-UM-*----ENTRATE----*----USCITE-----*----Prezzo----*---------*SK04
044100850114**   SK5
044200850114*               *                *                              * SK05
044300850114   *    :               *               *              *         *SK05
044400850114**   SK6
044500850114*---------------*----------------*------------------------------*-SK06
044600850114---*----*---------------*---------------*--------------*---------*SK06

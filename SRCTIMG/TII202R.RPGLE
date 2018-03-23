000100990121     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990121     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300861212     F* MPH115R  *-MP/38-*-------------------------------------------*
000400861212     F*------------------*                                           *
000500861212     F*       - INTERROGAZIONE GIACENZA E DISPONIB. M.P. -           *
000600861212     F*       --------------------------------------------           *
000700861212     F*                                                   AZN.: H115 *
000800990121     F*         FILE : MPH115D--ANTAB00F-tmamp01L-tmsmp02L-          *
000900861212     F*                                                              *
001000861212     F*         PGM. : RIC001R   - RICERCA INTESTAZIONE DITTA        *
001100990317     F*         PGM. : tii104R   - RICERCA PRODOTTO                  *
001200990317     F*         PGM. : tii108R   - RICERCA MAGAZZINO                 *
001300861212     F*--------------------------------------------------------------*
001400861212     F*    IND.  01 - RICHIESTA GIACENZA                             *
001500861212     F*     "    02 - RICHIESTA DISPONIBILITA                        *
001600861212     F*     "    03 - RICHIESTO UNA      VERSIONE                    *
001700861212     F*     "   N03 - RICHIESTO TUTTE LE VERSIONI                    *
001800861212     F*     "    04 - RICHIESTO UN       MAGAZZINO                   *
001900861212     F*     "   N04 - RICHIESTO TUTTI I  MAGAZZINI                   *
002000861212     F*                                                              *
002100861212     F*--------------------------------------------------------------*
002200990317     FTII202D   CF   E             WORKSTN
002300861212     F                                     SFILE(SFL1:REC1)
002400861212     F                                     SFILE(SFL2:REC1)
002500861212     F                                     SFILE(SFL3:REC2)
002600861212     FANTAB00F  IF   E           K DISK
002700990121     Ftmamp01L  IF   E           K DISK
002800990121     Ftmsmp02L  IF   E           K DISK
002900861212     F*--------------------------------------------------------------*
003000861212     D TEN             S             11  2 DIM(5)                               *TOT.ENT.FILE
003100861212     D TTEN            S             11  2 DIM(5)                               *TTT.ENT.FILE
003200861212     D TUS             S             11  2 DIM(5)                               *TOT.USC.FILE
003300861212     D TTUS            S             11  2 DIM(5)                               *TTT.USC.FILE
003400870519     D TDS             S             11  2 DIM(5)                               *TOT.DIS.FILE
003500870519     D TTDS            S             11  2 DIM(6)                               *TTT.DIS.FILE
003600861212     D ERR             S             30    DIM(5) CTDATA PERRCD(1)              *ERRORI
003700861212     D*--------------------------------------------------------------*
003900861212     D                 DS
004000861212     D*------------- CHIAVE TABELLA 151 :
004100861212     D  PROCED                 1      2
004200861212     D  EU                     3      3
004300861212     D  DESPRC                 1      3
004400861212     D DESENT          DS
004500861212     D*------------- DESCRIZIONI COLONNE ENTRATE :
004600861212     D  DE1                    1      9
004700861212     D  DE2                   10     18
004800861212     D  DE3                   19     27
004900861212     D  DE4                   28     36
005000861212     D  DE5                   37     45
005100861212     D DESUSC          DS
005200861212     D*------------- DESCRIZIONI COLONNE USCITE :
005300861212     D  DU1                    1      9
005400861212     D  DU2                   10     18
005500861212     D  DU3                   19     27
005600861212     D  DU4                   28     36
005700861212     D  DU5                   37     45
005800861212     D TB153         E DS                  EXTNAME(ANTAB153)
005900990121     D RECSAL        E DS                  EXTNAME(tmsmp00F)
006000990211     D  DS                    56     85P 2
006100870519     D                                     DIM(5)                               *DISPON. FILE
006200990211     D  EN                    86    115P 2
006300861212     D                                     DIM(5)                               *ENTRATE FILE
006400990211     D  US                   116    145P 2
006500861212     D                                     DIM(5)                               *USCITE  FILE
006600861212     D ENVID           DS
006700861212     D*------------- ENTRATE DETTAGLIO A VIDEO :
006800861212     D  ENT1                   1      7  1
006900861212     D  ENT2                   8     14  1
007000861212     D  ENT3                  15     21  1
007100861212     D  ENT4                  22     28  1
007200861212     D  ENT5                  29     35  1
007300861212     D  ENV                    1     35  1
007400861212     D                                     DIM(5)                               *ENTRATE VIDEO
007500861212     D USVID           DS
007600861212     D*------------- USCITE  DETTAGLIO A VIDEO :
007700861212     D  USC1                   1      7  1
007800861212     D  USC2                   8     14  1
007900861212     D  USC3                  15     21  1
008000861212     D  USC4                  22     28  1
008100861212     D  USC5                  29     35  1
008200861212     D  USV                    1     35  1
008300861212     D                                     DIM(5)                               *USCITE  VIDEO
008400861212     D DSVID           DS
008500861212     D*------------- DISPON. DETTAGLIO A VIDEO :
008600861212     D  ORDIN                  1     10  2
008700861212     D  PRODUZ                11     20  2
008800861212     D  IMPEGN                21     30  2
008900870519     D  ASSPRO                31     40  2
009000870519     D  GIACE2                41     50  2
009100870519     D  DSV                    1     50  2
009200870519     D                                     DIM(5)                               *DISPON. VIDEO
009300861212     D TENVID          DS
009400861212     D*------------- ENTRATE TOTALE A VIDEO :
009500861212     D  TOTE01                 1      7  1
009600861212     D  TOTE02                 8     14  1
009700861212     D  TOTE03                15     21  1
009800861212     D  TOTE04                22     28  1
009900861212     D  TOTE05                29     35  1
010000861212     D  TENV                   1     35  1
010100861212     D                                     DIM(5)                               *TOT.ENTR.VIDEO
010200871121     D MPHDT1          DS           512
010300871121     D*-----DATA AREA NUMERATORE MOVIM. MAGAZZINO
010400871121     D  NUMDTA                 1      5  0
010500871121     D  FILL1                  6      8
010600871121     D  DATDTA                 9     14  0
010700871121     D  FILL2                 15     17
010800871121     D  MPHAA                 18     19  0
010900871121     D  FIL03                 20     50
011000871121     D  DGIO                  51     56  0
011100871121     D  FIL04                 57    100
011200871121     D  FILL5                101    300
011300871121     D  GIOTOL               301    302  0
011400871121     D  FILL6                303    512
011500861212     D TUSVID          DS
011600861212     D*------------- USCITE  TOTALE A VIDEO :
011700861212     D  TOTU01                 1      7  1
011800861212     D  TOTU02                 8     14  1
011900861212     D  TOTU03                15     21  1
012000861212     D  TOTU04                22     28  1
012100861212     D  TOTU05                29     35  1
012200861212     D  TUSV                   1     35  1
012300861212     D                                     DIM(5)                               *TOT.USCI.VIDEO
012400861212     D TDSVID          DS
012500861212     D*------------- DISPON. TOTALE A VIDEO :
012600861212     D  TOTORD                 1     10  2
012700861212     D  TOTPRD                11     20  2
012800861212     D  TOTIMP                21     30  2
012900870519     D  TOTASP                31     40  2
013000870519     D  TOTGIF                41     50  2
013100870519     D  TDSV                   1     50  2
013200870519     D                                     DIM(5)                               *TOT.DISP.VIDEO
013300861212     D KPJBA         E DS
013400861212     D*----- PASSAGGIO PARAMETRI
013500990121     D PH115           DS
013600990121     D*----- PASSAGGIO PARAMETRI X VISUALIZZ. GIACENZA :
013700990121     D  Anno                   1      4
013800990121     D  Aaa115                 1      4  0
013900990121     D  MAG115                 5     11
014000990121     D  MAT115                12     23
014100990121     D  UMI115                24     24
014200990121     D  SCE115                25     25
014300990121     D  MSGERR               207    256
014400990121     D soc001        E DS                  EXTNAME(xsoc001ds)
014500990121     D xsocds          DS          1000
014600861212     C*----------------------------------------------------*
014700861212     C     *ENTRY        PLIST                                                  *
014800861212     C                   PARM                    KPJBA                          *
014900990121     c                   movel     kpjbu         ph115
015000990121     C*---------- RICERCA DITTA :
015100990121     C                   MOVEL     'SOC001'      TIPXSC
015200990121     C                   MOVEL     *blank        SOCXSC
015300990121     C                   EXSR      REPSOC
015400990121     C     RTNXSC        IFNE      '1'
015500990121     C                   MOVEL     XSOCDS        SOC001
015600990121     C                   MOVEL     xscrgs        desdit
015700990121     c                   end
015800990121     c
015900871121     C     *DTAARA       DEFINE                  MPHDT1                         *
016000871005     C                   MOVEL     '?'           INTERR           12
016100861212     C*---------- ANAGRAFICO MATERIE PRIME :               *
016200861212     C     KEYAMP        KLIST                                                  *
016300861212     C                   KFLD                    MAT115                         *
016400861212     C*---------- SALDI DI MAGAZZINO :                     *
016500861212     C     KEYMG1        KLIST                                                  *
016600990121     C                   KFLD                    AAa115                         *
016700861212     C                   KFLD                    MAT115                         *
016800861212     C     KEYMG2        KLIST                                                  *
016900990121     C                   KFLD                    AAa115                         *
017000861212     C                   KFLD                    MAT115                         *
017100861212     C                   KFLD                    MAG115                         *
017200861212     C*---------- ANAGRAFICO TABELLE :                     *
017300861212     C     KEYTAB        KLIST                                                  *
017400861212     C                   KFLD                    TABGRU                         *
017500861212     C                   KFLD                    TABCOD                         *
017600861212     C*----------                                          *
017700871121     C                   IN        MPHDT1
017800990121     C     Anno          COMP      *blank                                 90    *
017900990121     c   90              do
018000990121     C                   z-add     mphaa         AAA115            4 0          *
018100990121     c                   if        aaa115  > 40
018200990121     c                   add       1900          aaa115
018300990121     c                   else
018400990121     c                   add       2000          aaa115
018500990121     c                   end
018600990121     c                   end
018700861212     C     MAT115        COMP      *BLANKS                                90    *
018800861212     C   90              MOVE      INTERR        MAT115                         *
018900861212     C     MAG115        COMP      *BLANKS                                90    *
019000871121     C   90              MOVE      '1      '     MAG115                         *
019100861212     C     INIZIO        TAG                                                    *
019200861212     C     UMI115        COMP      *BLANKS                                90    *
019300861212     C   90              MOVE      'A'           UMI115                         *
019400861212     C     SCE115        COMP      *BLANKS                                90    *
019500870519     C   90              MOVE      'D'           SCE115                         *
019600861212     C                   MOVEA     *ZEROS        *IN(1)                         *
019700861212     C                   MOVE      *BLANKS       ERRORE                         *
019800861212     C*--------- CONTROLLI SI PROMPT ----------------------*
019900861212     C     EMISPT        TAG                                                    *
020000990412     C     MAT115        COMP      *BLANKS                                90    *
020100990412     C   90              MOVE      INTERR        MAT115                         *
020200861212     C                   SETOFF                                       010203    *
020300861212     C                   SETOFF                                       0405      *
020400861212     C                   WRITE     MPH115TS                                     *
020500861212     C                   EXFMT     MPH115PT                                     *
020600990412     C   Kc              GOTO      FINE                                         *
020700861212     C                   MOVEA     *ZEROS        *IN(10)                        *
020800861212     C                   MOVE      *BLANKS       ERRORE                         *
020900861212     C     SCE115        COMP      'G'                                    01    *GIACENZA
021000861212     C     SCE115        COMP      'D'                                    02    *DISPONIB.
021100861212     C*---------- PRODOTTO :                               *
021200861212     C     MAT115        IFEQ      INTERR                                       *
021300861212     C                   MOVE      *BLANKS       MAT115                         *
021400990412     C                   MOVE      *blank        desart                         *
021500990317     C                   CALL      'TII104R'                                    *
021600861212     C                   PARM                    MAT115                         *
021700861212     C                   PARM                    DESART                         *
021800861212     C                   GOTO      EMISPT                                       *
021900861212     C                   END                                                    *
022000990121     C     KEYAMP        CHAIN     tmamp01L                           90        *
022100861212     C   90              MOVEL     ERR(1)        ERRORE                         *
022200861212     C   90              GOTO      EMISPT                                       *
022300861212     C                   MOVEL     AMPDMT        DESART                         *
022400861212     C*---------- MAGAZZINO :                              *
022500861212     C     MAG115        IFEQ      INTERR
022600861212     C                   MOVE      *BLANKS       MAG115                         *
022700861212     C                   MOVE      *BLANKS       DAT153                         *
022800990317     C                   CALL      'TII108R'                                    *
022900861212     C                   PARM                    MAG115                         *
023000861212     C                   PARM                    DAT153           75            *
023100861212     C                   GOTO      EMISPT                                       *
023200861212     C                   END                                                    *
023300861212     C*----------------------------------------------------*
023400861212     C     MAG115        IFNE      '9999999'                                    *
023500861212     C                   SETON                                            04    *
023600861212     C                   MOVE      '153'         TABGRU                         *
023700861212     C                   MOVE      *ZEROS        TABCOD                         *
023800861212     C                   MOVE      MAG115        TABCOD                         *
023900861212     C     KEYTAB        CHAIN     ANTAB00F                           90        *
024000861212     C   90              MOVEL     ERR(2)        ERRORE                         *
024100861212     C   90              GOTO      EMISPT                                       *
024200861212     C                   MOVEL     TABDES        TB153                          *
024300861212     C                   ELSE                                                   *
024400861212     C                   MOVE      *ALL'*'       DES153                         *
024500861212     C                   MOVEL     'TUTTI'       DES153                         *
024600861212     C                   END                                                    *
024700990121     C                   MOVEL     *BLANKS       CDC153v
024800131030     C                   MOVEL     AMPfil        CDC153v
024900861212     C                   Z-ADD     0             REC1              5 0          *
025000861212     C                   Z-ADD     0             REC2              5 0          *
025100861212     C                   Z-ADD     0             TTEN                           *
025200861212     C                   Z-ADD     0             TTUS                           *
025300861212     C                   Z-ADD     0             TTDS                           *
025400861212     C                   MOVEA     *ZEROS        *IN(10)                        *
025500861212     C                   MOVE      *ZEROS        ENVID                          *
025600861212     C                   MOVE      *ZEROS        USVID                          *
025700861212     C                   MOVE      *ZEROS        DSVID                          *
025800861212     C                   MOVE      *ZEROS        TENVID                         *
025900861212     C                   MOVE      *ZEROS        TUSVID                         *
026000861212     C                   MOVE      *ZEROS        TDSVID                         *
026100861212     C*---------- PULIZIA SUBFILE :                        *
026200861212     C                   SETON                                            70    *PULIZIA
026300861212     C   01              WRITE     SFL1CTL                                      *
026400861212     C   01              WRITE     SFL2CTL                                      *
026500861212     C   02              WRITE     SFL3CTL                                      *
026600861212     C                   SETOFF                                           70    *
026700861212     C*---------- RICERCA DES. ENTRATE/USCITE :            *
026800861212     C   01              DO                                                     *
026900861212     C                   MOVE      'MP'          PROCED                         *
027000861212     C                   MOVE      '151'         TABGRU                         *
027100861212     C                   MOVE      *ZEROS        TABCOD                         *
027200861212     C*---------- DESCRIZIONE ENTRATE :                    *
027300861212     C                   MOVE      'E'           EU                             *
027400861212     C                   MOVE      DESPRC        TABCOD                         *
027500861212     C     KEYTAB        CHAIN     ANTAB00F                           85        *
027600861212     C   85              MOVE      *ALL'*'       DESENT                         *
027700861212     C  N85              MOVEL     TABDES        DESENT                         *ENTRATE
027800861212     C*---------- DESCRIZIONE ENTRATE :                    *
027900861212     C                   MOVE      'U'           EU                             *
028000861212     C                   MOVE      DESPRC        TABCOD                         *
028100861212     C     KEYTAB        CHAIN     ANTAB00F                           85        *
028200861212     C   85              MOVE      *ALL'*'       DESUSC                         *
028300861212     C  N85              MOVEL     TABDES        DESUSC                         *USCITE
028400861212     C                   END                                                    *
028500861212     C*---------- LETTURA SALDI DI MAGAZZINO --------------*
028600861212     C  N03
028700861212     CAN 04                                                                     *
028800861212     CORN03
028900861212     CANN04                                                                     *
029000861212     COR 03
029100990121     CANN04KEYMG1        SETLL     tmsmp02L                           90  91    *
029200861212     C   03
029300990121     CAN 04KEYMG2        SETLL     tmsmp02L                           90  91    *
029400861212     C  N91              SETON                                        90        *
029500861212     C   90
029600861212     CAN 01              MOVEL     ERR(3)        ERRORE                         *NO GIACENZA
029700861212     C   90
029800861212     CAN 02              MOVEL     ERR(4)        ERRORE                         *NO DISPONIBIL.
029900861212     C   90              GOTO      EMISPT                                       *
030000861212     C     SUSALD        TAG                                                    *
030100861212     C  N03
030200861212     CAN 04                                                                     *
030300861212     CORN03
030400861212     CANN04                                                                     *
030500861212     COR 03
030600990121     CANN04KEYMG1        READE     tmsmp000                               91    *
030700861212     C   03
030800990121     CAN 04KEYMG2        READE     tmsmp000                               91    *
030900861212     C     ENDSAL        TAG                                                    *
031000861212     C   91
031100861212     CAN 05              EXSR      CARVID                                       *
031200861212     C   91
031300861212     CAN 05              GOTO      EMISSF                                       *
031400861212     C   91
031500861212     CANN05              SETON                                        90        *
031600861212     C   90
031700861212     CAN 01              MOVEL     ERR(3)        ERRORE                         *NO GIACENZA
031800861212     C   90
031900861212     CAN 02              MOVEL     ERR(4)        ERRORE                         *NO DISPONIBIL.
032000861212     C   90              GOTO      EMISPT                                       *
032100861212     C*---------- CONTROLLO ROTTURA  MAGAZZINO :           *
032200861212     C     MAG115        IFNE      '9999999'                                    *
032300861212     C     SMPMAG        CABNE     MAG115        SUSALD                         *
032400861212     C                   END                                                    *
032500861212     C*---------- ROTTURA DI VERSIONE :                    *
032600861212     C   05              EXSR      CARVID                                       *
032700990121     C     KEYAMP        CHAIN     tmamp01L                           90        *
032800861212     C   90              GOTO      SUSALD                                       *
032900861212     C     UMI115        COMP      'A'                                    85    *UNITA MISURA
033000861212     C   85              MOVE      AMPUMA        UMIQ              3            *ANAGRAFICA
033100861212     C   85              Z-ADD     1             FCVF             11 5    86    *
033200861212     C     UMI115        COMP      'Q'                                    85    *           A
033300861212     C   85              MOVE      AMPUMQ        UMIQ                           *ACQUISTO
033400861212     C   85              Z-ADD     AMPFCQ        FCVF                     86    *
033500861212     C     UMI115        COMP      'S'                                    85    *           A
033600861212     C   85              MOVE      AMPUMS        UMIQ                           *STOCCAGGIO
033700861212     C   85              Z-ADD     AMPFCS        FCVF                     86    *
033800861212     C     UMI115        COMP      'V'                                    85    *           A
033900861212     C   85              MOVE      AMPUMV        UMIQ                           *VENDITA
034000861212     C   85              Z-ADD     AMPFCV        FCVF                     86    *
034100861212     C  N05              MOVE      UMIQ          UMIT                           *
034200861212     C                   SETON                                            05    *
034300861212     C                   Z-ADD     0             TEN                            *
034400861212     C                   Z-ADD     0             TUS                            *
034500861212     C                   Z-ADD     0             TDS                            *
034600861212     C*----------------------------------------------------*
034700861212     C  N86EN            DIV(H)    FCVF          EN                             *
034800861212     C  N86US            DIV(H)    FCVF          US                             *
034900861212     C  N86DS            DIV(H)    FCVF          DS                             *
035000861212     C   01TEN           ADD       EN            TEN                            *ENTRATE
035100861212     C   01TTEN          ADD       EN            TTEN                           *TOT.ENTRATE
035200861212     C   01TUS           ADD       US            TUS                            *USCITE
035300861212     C   01TTUS          ADD       US            TTUS                           *TOT.USCITE
035400861212     C   02TDS           ADD       DS            TDS                            *DISPON.
035500861212     C   02TTDS          ADD       DS            TTDS                           *TOT.DISPON.
035600861212     C                   GOTO      SUSALD                                       *
035700861212     C     EMISSF        TAG                                                    *
035800861212     C*---------- EMISSIONE GIACENZA :                     *
035900861212     C   01              DO                                                     *
036000861212     C                   Z-ADD     TTEN          TENV                           *
036100861212     C                   Z-ADD     TTUS          TUSV                           *
036200861212     C                   XFOOT     TTEN          TOTENT                         *
036300861212     C                   XFOOT     TTUS          TOTUSC                         *
036400861212     C     TOTENT        SUB       TOTUSC        TOTGIA                         *
036500861212     C     SU10          TAG                                                    *
036600861212     C                   SETON                                            10    *
036700861212     C                   WRITE     MPH115TS                                     *
036800861212     C                   EXFMT     SFL1CTL                                      *
036900990121     C   Kl              GOTO      INIZIO                                       *
037000861212     C                   SETOFF                                           10    *
037100861212     C                   SETON                                            11    *
037200861212     C                   WRITE     MPH115TS                                     *
037300861212     C                   EXFMT     SFL2CTL                                      *
037400990121     C   Kl              GOTO      INIZIO                                       *
037500861212     C                   SETOFF                                           11    *
037600861212     C                   GOTO      SU10                                         *
037700861212     C                   END                                                    *
037800861212     C*---------- EMISSIONE DISPONIBILITA' ----------------*
037900990412     C   02              DO        *hival                                       *
038000861212     C                   Z-ADD     TTDS          TDSV                           *
038100880118     C     TTDS(5)       ADD       TTDS(1)       TOTDIS                         *
038200880118     C     TOTDIS        SUB       TTDS(4)       TOTDIS                         *
038300880118     C     TOTDIS        SUB       TTDS(3)       TOTDIS                         *
038400880118     C     TOTDIS        SUB       TTDS(2)       TOTDIS                         *
038500861212     C                   SETOFF                                         1011    *
038600861212     C                   WRITE     MPH115TS                                     *
038700861212     C                   EXFMT     SFL3CTL                                      *
038800990412     C*  Kl              GOTO      FINE                                         *
038900990412     C   kl              GOTO      INIZIO                                       *
039000861212     C                   END                                                    *
039100861212     C*----------------------------------------------------*
039200861212     C     FINE          TAG                                                    *
039300861212     C   90              MOVEL     'KPJB207'     KMSMN                          *
039400861212     C                   SETON                                            LR    *
039500861212     C**--------- CARICAMENTO DATI A VIDEO ---------------**
039600861212     C***------------------------------------------------***
039700861212     CSR   CARVID        BEGSR                                                  *
039800861212     C                   MOVE      UMIQ          UMIV                           *
039900861212     C     UMIV          IFNE      UMIT                                         *
040000861212     C                   MOVE      '*?*'         UMIT                           *
040100861212     C                   SETON                                            92    *NO TOTALI
040200861212     C                   END                                                    *
040300861212     C   01              DO                                                     *
040400861212     C                   Z-ADD     TEN           ENV                            *
040500861212     C                   Z-ADD     TUS           USV                            *
040600861212     C                   XFOOT     TEN           ENTRAT                         *
040700861212     C                   XFOOT     TUS           USCITE                         *
040800861212     C     ENTRAT        SUB       USCITE        GIACE1                         *
040900861212     C     REC1          ADD       1             REC1                 71        *
041000861212     C                   WRITE     SFL1                                         *
041100861212     C                   WRITE     SFL2                                         *
041200861212     C                   END                                                    *
041300861212     C*----------                                          *
041400861212     C   02              DO                                                     *
041500861212     C                   Z-ADD     TDS           DSV                            *
041600880113     C     TDS(5)        SUB       TDS(4)        DISPON                         *
041700880113     C     DISPON        SUB       TDS(3)        DISPON                         *
041800880113     C** DISPONIBILITA= GIACENZA--SS.TO CLIENTE-ASS.TO PROVA
041900861212     C     REC2          ADD       1             REC2                 71        *
042000861212     C                   WRITE     SFL3                                         *
042100861212     C                   END                                                    *
042200861212     CSR                 ENDSR                                                  *
042300990121     C*----------------------------------------------------*
042400990121     C* Reperimento dati società
042500990121     C*----------------------------------------------------*
042600990121     C     REPSOC        BEGSR
042700990121     C*
042800990121     C                   CALLB     'XSOC'
042900990121     C                   PARM                    TIPXSC            6
043000990121     C                   PARM                    SOCXSC            3
043100990121     C                   PARM                    CDSXSC            9 0
043200990121     C                   PARM                    MODXSC            3
043300990121     C                   PARM      *blanks       RTNXSC            1
043400990121     C                   PARM                    XSOCDS
043500990121     C                   PARM                    KPJBA
043600990121     C*
043700990121     C                   ENDSR
043800861212     C***------------------------------------------------***
043900861212**    ERR
044000861212*- Codice Prodotto ERRATO ! -*      ERR,1
044100861212*- Codice Magazzino  ERRATO -*      ERR,2
044200861212*- Non esiste Giacenza  !!! -*      ERR,3
044300861212*- Non esiste Disponibilità -*      ERR,4
044400861212*-                          -*      ERR,5

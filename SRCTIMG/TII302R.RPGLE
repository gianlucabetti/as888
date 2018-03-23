000100990126     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990126     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300880919     F*-OCO170R-*-OC/38-*--------------------------------------------*
000400000000     F*-----------------*                                            *
000500880919     F*           INTERROG. ARCHIVIO ORDINI CLIENTI X DATA BOLLA     *
000600880919     F*           ----------------------------------------------     *
000700880919     F*                                                    AZ.: O170 *
000800000000     F*                                                              *
000900990126     F*         FILE : OCO170D  - tmclp01L - ANTAB00F -              *
001000990126     F*                tmsocXXL - tmamp0XL - OCSOFXXL - OCODT2       *
001100000000     F*--------------------------------------------------------------*
001200880919     F*  INDIC.: 01 - GESTIONE X CLIENTE                             *
001300000000     F*          02 - GESTIONE X PRODOTTO                            *
001400000000     F*                                                              *
001500000000     F*  ORDIN.: 12 - SETTORE MERCEOLOGICO     22 - S/CLASSE MERC.   *
001600000000     F*          11 - GRUPPO  MERCEOLOGICO     21 - CLASSE   MERC.   *
001700880919     F*          17 - CLIENTE                  26 - PRODOTTO         *
001800000000     F*--------------------------------------------------------------*
001900000000     F*         PGM. : RIC001R - RICERCA DITTA                       *
002000000000     F*                RIC002C - RECLAIM RESURCES                    *
002100880919     F*                ANU005R - RICERCA ALFABETICA MATERIE PRIME    *
002200990317     F*                TII119R - RICERCA ALFABETICA CLIENTE          *
002300990317     F*                TII150R - RICERCA CLASSE   MERCEOLOGICA       *
002400990317     F*                TII111R - RICERCA S/CLASSE MERCEOLOGICA       *
002500990317     F*                TII114R - RICERCA GRUPPO   MERCEOLOGICO       *
002600990317     F*                TII115R - RICERCA SETTORE  MERCEOLOGICO       *
002700000000     F*                RICDATE - CONTROLLO INVERSIONE DATA           *
002800000000     F*--------------------------------------------------------------*
002900990126     Ftmamp01L  IF   E           K DISK
003000990126     F                                     RENAME(tmamp000:ARTCOD)
003100990126     Ftmamp03L  IF   E           K DISK    USROPN
003200990126     F                                     RENAME(tmamp000:ARTCLM)
003300990126     Ftmamp04L  IF   E           K DISK    USROPN
003400990126     F                                     RENAME(tmamp000:ARTSCM)
003500000000     F*----------------------------------------------------------------
003600990126     Ftmsoc23L  IF   E           K DISK    USROPN
003700990126     F                                     RENAME(tmsoc000:SOGPM)
003800990126     F                                     RENAME(tmmoc000:MOGPM)
003900990126     Ftmsoc24L  IF   E           K DISK    USROPN
004000990126     F                                     RENAME(tmsoc000:SOSTM)
004100990126     F                                     RENAME(tmmoc000:MOSTM)
004200990126     Ftmsoc25L  IF   E           K DISK    USROPN
004300990126     F                                     RENAME(tmsoc000:SOART)
004400990126     F                                     RENAME(tmmoc000:MOART)
004500990126     Ftmsoc26L  IF   E           K DISK    USROPN
004600990126     F                                     RENAME(tmsoc000:SOFOR)
004700990126     F                                     RENAME(tmmoc000:MOFOR)
004800000000     F*----------------------------------------------------------------
004900990126     Ftmclp01L  IF   E           K DISK
005000000000     FANTAB00F  IF   E           K DISK
005100990317     FTII302D   CF   E             WORKSTN
005200000000     F                                     SFILE(SFL1:REC1)
005300000000     FOFPRTSTD  O    F  132        PRINTER OFLIND(*INOF)
005400000000     F                                     USROPN
005500000000     F*----------------------------------------------------------------
005600000000     D TES             S             15    DIM(6) CTDATA PERRCD(1)              *TESTATE
005700000000     D ERR             S             30    DIM(12) CTDATA PERRCD(1)             * ERRORI
005800000000     D SK1             S             66    DIM(2) CTDATA PERRCD(1)              *FINCATURA 1
005900000000     D SK2             S             66    DIM(2) CTDATA PERRCD(1)              *FINCATURA 2
006000000000     D SK3             S             66    DIM(2) CTDATA PERRCD(1)              *FINCATURA 3
006100000000     D SK4             S             66    DIM(2) CTDATA PERRCD(1)              *FINCATURA 4
006200000000     D SK5             S             66    DIM(2) CTDATA PERRCD(1)              *FINCATURA 5
006300000000     D SK6             S             66    DIM(2) CTDATA PERRCD(1)              *FINCATURA 6
006400000000     D ARTWW1          DS            12
006500000000     D*----- SCOMPOSIZIONE ARTICOLO :
006600880919     D  MOCMAT                 1     12
006700880920     D OCODT2          DS           512
006800000000     D*-----DATA AREA ORDINI FORNIT. -2- :
006900990127     D  DT2DUA               151    156  0
007000000000     D                 DS
007100000000     D*-----TIPO RIGHE :
007200000000     D  TG1170                 1      1
007300000000     D  TG2170                 2      2
007400000000     D  TG3170                 3      3
007500000000     D  TG4170                 4      4
007600000000     D  TG5170                 5      5
007700000000     D  TG6170                 6      6
007800000000     D  TG7170                 7      7
007900000000     D  TRG                    1      7
008000000000     D                                     DIM(7)                               *TIPO RIGHE
008100000000     D KPJBA         E DS
008200000000     D*--- PASSAGGIO PARAMETRI
008300000000     D  MSGERR               453    502
008400990126     D soc001        E DS                  EXTNAME(xsoc001ds)
008500990126     D xsocds          DS          1000
008600990126     d dataiso         s               d   datfmt(*iso)
008700990126     d dataeur         s               d   datfmt(*eur)
008800000000     C*----------------------------------------------------*
008900000000     C     *ENTRY        PLIST                                                  *
009000000000     C                   PARM                    KPJBA                          *
009100990126     C*---------- RICERCA DITTA :
009200990126     C                   MOVEL     'SOC001'      TIPXSC
009300990126     C                   MOVEL     *blank        SOCXSC
009400990126     C                   EXSR      REPSOC
009500990126     C     RTNXSC        IFNE      '1'
009600990126     C                   MOVEL     XSOCDS        SOC001
009700990126     C                   MOVEL     xscrgs        desdit
009800990126     C                   MOVEL     xscsoc        wsoc              3
009900990126     c                   end
010000990126     c                   time                    como14           14 0
010100990126     c                   move      como14        woggi             8 0
010200990126     c                   move      woggi         dataeur
010300990126     c                   move      dataeur       dataiso
010400990126     c                   move      dataiso       woggig            8 0
010500990126     C*                                                    *
010600000000     C*--- FILE ANAGRAFICO MATERIE PRIME :                 *
010700000000     C     KEYAMP        KLIST                                                  *
010800000000     C                   KFLD                    AMPMAT                         *
010900000000     C*--- FILE ANAGRAFICO TABELLE "ANTAB00F"              *
011000000000     C     KEYTAB        KLIST                                                  *
011100000000     C                   KFLD                    TABGRU                         *
011200000000     C                   KFLD                    TABCOD                         *
011300000000     C*---------- RICERCA X FORNITORE ---------------------*
011400000000     C     KFOR          KLIST                                                  *
011500000000     C                   KFLD                    FOR170                         *
011600000000     C*---------- RICERCA X GRUPPO ------------------------*
011700000000     C     KGPM          KLIST                                                  *
011800000000     C                   KFLD                    GPM170                         *
011900000000     C*---------- RICERCA X SETTORE -----------------------*
012000000000     C     KSTM          KLIST                                                  *
012100000000     C                   KFLD                    STM170                         *
012200000000     C*---------- RICERCA X ARTICOLO ----------------------*
012300000000     C     KART          KLIST                                                  *
012400000000     C                   KFLD                    AMPMAT                         *
012500000000     C     KART1         KLIST                                                  *
012600000000     C                   KFLD                    AMPMAT                         *
012700000000     C                   MOVE      *ZEROS        ARTWW1                         *
012800000000     C*---------- RICERCA DATA AREA ARCHIVIAZIONE ---------*
012900880920     C     *DTAARA       DEFINE                  OCODT2                         *
013000000000     C     SUDT2         TAG                                                    *
013100880920     C     *LOCK         IN        OCODT2                               90      *
013200000000     C   90              GOTO      SUDT2                                        *
013300880920     C                   OUT       OCODT2                                       *
013400990127     C     *dmy          MOVE      DT2DUA        DATAeur                        *
013500990127     C                   move      dataeur       dataiso                        *
013600990127     C                   move      DATAIso       DATARC            8 0          *DATA ARCHIVIO
013700000000     C*---------- IMPOSTAZIONE PROMPT ---------------------*
013800990127     C                   move      woggi         DAL
013900000000     C                   MOVEL     '0101'        DAL                            *
014000990127     C                   move      woggi         AL                             *
014100000000     C                   MOVE      '99'          TIP170                         *
014200000000     C                   MOVEA     '123CDIS'     TRG                            *
014300000000     C                   MOVE      '99'          FAS170                         *
014400000000     C     START         TAG                                                    *
014500000000     C                   MOVE      *BLANKS       ORD170                         *
014600000000     C                   MOVE      *BLANKS       DESORD                         *
014700000000     C                   MOVE      *BLANKS       CODORD                         *
014800000000     C                   MOVE      *BLANKS       DES170                         *
014900000000     C                   MOVE      *ALL'X'       FOR170                         *
015000000000     C                   MOVE      *BLANKS       DESFOR                         *
015100000000     C                   MOVE      'XX'          GPM170                         *
015200000000     C                   MOVE      *BLANKS       DESGPM                         *
015300000000     C                   MOVE      'XX'          STM170                         *
015400000000     C                   MOVE      *BLANKS       DESSTM                         *
015500000000     C                   MOVE      *ALL'X'       ART170                         *
015600000000     C                   MOVE      *BLANKS       DESART                         *
015700000000     C                   MOVE      'XXX'         CLM170                         *
015800000000     C                   MOVE      *BLANKS       DESCLM                         *
015900000000     C                   MOVE      'XXXXX'       SCM170                         *
016000000000     C                   MOVE      *BLANKS       DESSCM                         *
016100990127     C                   Z-ADD     0             DAL170            8 0          *
016200990127     C                   Z-ADD     0             AL170             8 0          *
016300990127     C                   Z-ADD     0             TOTVAL           15 5          * VAL.ORDINI
016400000000     C                   Z-ADD     0             TOTQTA           12 2          * QTA.ORDINI
016500000000     C                   Z-ADD     0             PAGINE                         * PAGINE VIDEO
016600000000     C                   Z-ADD     0             PAGE              3 0          *
016700000000     C                   Z-ADD     50            PAGE1             3 0          *
016800000000     C  N90              MOVEA     *ZEROS        *IN(1)                         *
016900000000     C*----------------------------------------------------*
017000000000     C     EMISPT        TAG                                                    *
017100880920     C                   EXFMT     OCO170PT                                     * PROMPT
017200000000     C                   MOVEA     *ZEROS        *IN(1)                         *
017300000000     C                   MOVE      *BLANKS       ERRORE                         *
017400990409     C   Kc              GOTO      FINE                                         *
017500000000     C*---------                                           *
017600000000     C     ORD170        COMP      '1'                                029001    *
017700000000     C   90              SETON                                        9060      *
017800000000     C   90              MOVEL     ERR(10)       ERRORE                         *
017900000000     C   90              GOTO      EMISPT                                       *
018000880920     C*---------- RICERCA CLIENTE -------------------------*
018100990126     C     FOR170        IFNE      'XXXXXXXX'                                   *
018200990126     C     FOR170        IFEQ      '?       '                                   *
018300000000     C                   MOVE      *ZEROS        FOR170                         *
018400990317     C                   CALL      'TII119R'                                    *
018500000000     C                   PARM                    FOR170                         *
018600000000     C                   PARM                    DESFOR                         *
018700000000     C                   SETON                                            61    *
018800990126     C     FOR170        COMP      '99999999'                             90    *
018900000000     C   90              MOVEL     ERR(2)        ERRORE                         *
019000000000     C                   GOTO      EMISPT                                       *
019100000000     C                   END                                                    *
019200000000     C*----------                                          *
019300000000     C                   MOVE      *BLANKS       DESFOR                         *
019400990126     C     FOR170        CHAIN     tmclp01L                           90        *
019500000000     C   90              SETON                                          61      *
019600000000     C   90              MOVEL     ERR(2)        ERRORE                         *
019700000000     C   90              GOTO      EMISPT                                       *
019800880919     C                   MOVEL     CLPRSI        DESFOR                         *
019900000000     C                   END                                                    *
020000000000     C*---------- RICERCA ORDINAMENTO SETTORE -------------*
020100000000     C     STM170        IFNE      'XX'                                         *
020200000000     C     STM170        IFEQ      '? '                                         *
020300000000     C                   MOVE      *BLANKS       STM170                         *
020400990317     C                   CALL      'TII115R'                                    *
020500000000     C                   PARM                    STM170                         *
020600000000     C                   PARM                    DESSTM                         *
020700000000     C                   SETON                                        62        *
020800000000     C     STM170        COMP      '99'                                   90    *
020900000000     C   90              MOVEL     ERR(3)        ERRORE                         *
021000000000     C   90              MOVE      *BLANKS       STM170                         *
021100000000     C                   GOTO      EMISPT                                       *
021200000000     C                   END                                                    *
021300000000     C*----------                                          *
021400000000     C                   MOVE      '160'         TABGRU                         *
021500000000     C                   MOVE      *ZEROS        TABCOD                         *
021600000000     C                   MOVE      STM170        TABCOD                         *
021700000000     C     KEYTAB        CHAIN     ANTAB00F                           90        *
021800000000     C   90              SETON                                        62        *
021900000000     C   90              MOVEL     ERR(3)        ERRORE                         *
022000000000     C   90              GOTO      EMISPT                                       *
022100000000     C                   MOVEL     TABDES        DESSTM                         *
022200000000     C                   END                                                    *
022300000000     C*---------- RICERCA ORDINAMENTO GRUPPO --------------*
022400000000     C     GPM170        IFNE      'XX'                                         *
022500000000     C     GPM170        IFEQ      '? '                                         *
022600000000     C                   MOVE      *BLANKS       GPM170                         *
022700990317     C                   CALL      'TII114R'                                    *
022800000000     C                   PARM                    GPM170                         *
022900000000     C                   PARM                    DESGPM                         *
023000000000     C                   SETON                                        63        *
023100000000     C     GPM170        COMP      '99'                                   90    *
023200000000     C   90              MOVEL     ERR(4)        ERRORE                         *
023300000000     C   90              MOVE      *BLANKS       GPM170                         *
023400000000     C                   GOTO      EMISPT                                       *
023500000000     C                   END                                                    *
023600000000     C*----------                                          *
023700000000     C                   MOVE      '159'         TABGRU                         *
023800000000     C                   MOVE      *ZEROS        TABCOD                         *
023900000000     C                   MOVE      GPM170        TABCOD                         *
024000000000     C     KEYTAB        CHAIN     ANTAB00F                           90        *
024100000000     C   90              SETON                                        63        *
024200000000     C   90              MOVEL     ERR(4)        ERRORE                         *
024300000000     C   90              GOTO      EMISPT                                       *
024400000000     C                   MOVEL     TABDES        DESGPM                         *
024500000000     C                   END                                                    *
024600000000     C*---------- RICERCA PRODOTTO ------------------------*
024700000000     C                   MOVEL     'XXXXXXX'     ICS              12
024800000000     C                   MOVE      'XXXXXXX'     ICS
024900990127     C                   MOVE      *all'9'       NOVE             12
025000000000     C                   MOVEL     '?'           INTERR           12
025100000000     C     ART170        IFNE      ICS                                          *
025200000000     C     ART170        IFEQ      INTERR                                       *
025300000000     C                   MOVE      *BLANKS       ART170                         *
025400990317     C                   CALL      'TII104R'                                    *
025500000000     C                   PARM                    ART170                         *
025600000000     C                   PARM                    DESART                         *
025700000000     C                   SETON                                        65        *
025800000000     C     ART170        COMP      NOVE                                   90    *
025900000000     C   90              MOVEL     ERR(5)        ERRORE                         *
026000000000     C                   GOTO      EMISPT                                       *
026100000000     C                   END                                                    *
026200000000     C*----------                                          *
026300000000     C                   MOVE      ART170        AMPMAT                         *
026400990126     C     KEYAMP        CHAIN     tmamp01L                           90        *
026500000000     C   90              SETON                                        65        *
026600000000     C   90              MOVEL     ERR(5)        ERRORE                         *
026700000000     C   90              GOTO      EMISPT                                       *
026800000000     C                   MOVEL     AMPDMT        DESART                         *
026900000000     C                   END                                                    *
027000000000     C*---------- RICERCA ORDINAMENTO S/CLASSE ------------*
027100000000     C     SCM170        IFNE      'XXXXX'                                      *
027200000000     C     SCM170        IFEQ      '?    '                                      *
027300000000     C                   MOVE      *BLANKS       SCM170                         *
027400990317     C                   CALL      'TII111R'                                    *
027500000000     C                   PARM                    SCM170                         *
027600000000     C                   PARM                    DESSCM                         *
027700000000     C                   SETON                                        66        *
027800000000     C     SCM170        COMP      '99999'                                90    *
027900000000     C   90              MOVEL     ERR(6)        ERRORE                         *
028000000000     C   90              MOVE      *BLANKS       SCM170                         *
028100000000     C                   GOTO      EMISPT                                       *
028200000000     C                   END                                                    *
028300000000     C*----------                                          *
028400000000     C                   MOVE      '156'         TABGRU                         *
028500000000     C                   MOVE      *ZEROS        TABCOD                         *
028600000000     C                   MOVE      SCM170        TABCOD                         *
028700000000     C     KEYTAB        CHAIN     ANTAB00F                           90        *
028800000000     C   90              SETON                                        66        *
028900000000     C   90              MOVEL     ERR(6)        ERRORE                         *
029000000000     C   90              GOTO      EMISPT                                       *
029100000000     C                   MOVEL     TABDES        DESSCM                         *
029200000000     C                   END                                                    *
029300000000     C*---------- RICERCA CLASSE --------------------------*
029400000000     C     CLM170        IFNE      'XXX'                                        *
029500000000     C     CLM170        IFEQ      '?  '                                        *
029600000000     C                   MOVE      *BLANKS       CLM170                         *
029700990317     C                   CALL      'TII110R'                                    *
029800000000     C                   PARM                    CLM170                         *
029900000000     C                   PARM                    DESCLM                         *
030000000000     C                   SETON                                        67        *
030100000000     C     CLM170        COMP      '999'                                  90    *
030200000000     C   90              MOVEL     ERR(7)        ERRORE                         *
030300000000     C   90              MOVE      *BLANKS       CLM170                         *
030400000000     C                   GOTO      EMISPT                                       *
030500000000     C                   END                                                    *
030600000000     C*----------                                          *
030700000000     C                   MOVE      '155'         TABGRU                         *
030800000000     C                   MOVE      *ZEROS        TABCOD                         *
030900000000     C                   MOVE      CLM170        TABCOD                         *
031000000000     C     KEYTAB        CHAIN     ANTAB00F                           90        *
031100000000     C   90              SETON                                        67        *
031200000000     C   90              MOVEL     ERR(7)        ERRORE                         *
031300000000     C   90              GOTO      EMISPT                                       *
031400000000     C                   MOVEL     TABDES        DESCLM                         *
031500000000     C                   END                                                    *
031600000000     C*---- CONTROLLO DATA DAL SOLO SE DIVERSO DA ZERO ----*
031700000000     C                   Z-ADD     0             DAL170                         *
031800000000     C                   Z-ADD     0             AL170                          *
031900000000     C     DAL           IFNE      *ZEROS                                       *
032000990127     c     *eur          test(d)                 dal                    90
032100990127     c                   if        *in90
032200990127     c     *dmy          test(d)                 dal                    90
032300990127     c  n90*dmy          move      dal           dataeur
032400990127     c  n90              move      dataeur       dal
032500990127     c                   else
032600990127     c                   move      dal           dataeur
032700990127     c                   end
032800990127     C   90              SETON                                        74        *
032900990127     C   90              MOVEL     ERR(8)        ERRORE                         *
033000990127     c   90              goto      emispt
033100990127     c                   move      dataeur       dataiso
033200990127     c                   move      dataiso       dal170
033300000000     C                   END                                                    *
033400000000     C*---------- CONTROLLO DATA AL  ----------------------*
033500990127     c     *eur          test(d)                 al                     90
033600990127     c                   if        *in90
033700990127     c     *dmy          test(d)                 al                     90
033800990127     c  n90*dmy          move      al            dataeur
033900990127     c  n90              move      dataeur       al
034000990127     c                   else
034100990127     c                   move      al            dataeur
034200990127     c                   end
034300990127     C   90              SETON                                        74        *
034400990127     C   90              MOVEL     ERR(8)        ERRORE                         *
034500990127     c   90              goto      emispt
034600990127     c                   move      dataeur       dataiso
034700990127     c                   move      dataiso       al170
034800000000     C*---------- CONTROLLO INCROCIATO FRA DATE -----------*
034900000000     C     DAL170        IFGT      AL170                                        *
035000000000     C                   SETON                                        9074      *
035100000000     C                   MOVEL     ERR(8)        ERRORE                         *
035200000000     C                   GOTO      EMISPT                                       *
035300000000     C                   END                                                    *
035400000000     C*---------- RICERCA TIPO ORDINE ---------------------*
035500000000     C                   MOVE      *BLANKS       DESTIP                         *
035600000000     C     TIP170        IFNE      '99'                                         *
035700000000     C     TIP170        IFEQ      '? '                                         *
035800000000     C                   MOVE      *BLANKS       TIP170                         *
035900000000     C                   MOVE      *BLANKS       DESTIP           25            *
036000990317     C                   CALL      'TII117R'                                    *
036100000000     C                   PARM                    TIP170                         *
036200000000     C                   PARM                    DESTIP                         *
036300000000     C                   SETON                                        75        *
036400000000     C     TIP170        COMP      '99'                                   90    *
036500000000     C   90              MOVEL     ERR(9)        ERRORE                         *
036600000000     C                   GOTO      EMISPT                                       *
036700000000     C                   END                                                    *
036800000000     C                   MOVE      '162'         TABGRU                         *
036900000000     C                   MOVE      *ZEROS        TABCOD                         *
037000000000     C                   MOVE      TIP170        TABCOD                         *
037100000000     C     KEYTAB        CHAIN     ANTAB00F                           90        *
037200000000     C   90              SETON                                        75        *
037300000000     C   90              MOVEL     ERR(9)        ERRORE                         *
037400000000     C   90              GOTO      EMISPT                                       *
037500000000     C                   MOVEL     TABDES        DESTIP                         *
037600000000     C                   END                                                    *
037700880919     C*---------- IMPOSTO TIPO LAVORO X CLIENTE  ----------*
037800000000     C   01              DO                                                     *
037900880919     C*----------                                          *CLIENTE
038000990126     C     FOR170        IFNE      'XXXXXXXX'                                   *
038100000000     C                   MOVE      TES(1)        DESORD                         *
038200000000     C                   MOVEL     FOR170        CODORD                         *
038300000000     C                   MOVEL     DESFOR        DES170                         *
038400990126     C                   OPEN      tmsoc26L                             94      *
038500000000     C                   SETON                                            17    *
038600000000     C                   GOTO      LANCIO                                       *
038700000000     C                   END                                                    *
038800000000     C*---------- IMPOSTO TIPO LAVORO X SETTORE -----------*
038900000000     C     STM170        IFNE      'XX'                                         *
039000000000     C                   MOVE      TES(2)        DESORD                         *
039100000000     C                   MOVEL     STM170        CODORD                         *
039200000000     C                   MOVEL     DESSTM        DES170                         *
039300990126     C                   OPEN      tmsoc24L                             94      *
039400000000     C                   SETON                                            12    *
039500000000     C                   GOTO      LANCIO                                       *
039600000000     C                   END                                                    *
039700000000     C*---------- IMPOSTO TIPO LAVORO X GRUPPO ------------*
039800000000     C     GPM170        IFNE      'XX'                                         *
039900000000     C                   MOVE      TES(3)        DESORD                         *
040000000000     C                   MOVEL     GPM170        CODORD                         *
040100000000     C                   MOVEL     DESGPM        DES170                         *
040200990126     C                   OPEN      tmsoc23L                             94      *
040300000000     C                   SETON                                            11    *
040400000000     C                   GOTO      LANCIO                                       *
040500000000     C                   END                                                    *
040600000000     C                   END                                                    *
040700000000     C*---------- IMPOSTO TIPO LAVORO X ARTICOLO ----------*
040800000000     C   02              DO                                                     *
040900990126     C                   OPEN      tmsoc25L                             94      *
041000000000     C*----------                                          *ARTICOLO
041100000000     C     ART170        IFNE      ICS                                          *
041200000000     C                   MOVE      TES(4)        DESORD                         *
041300000000     C                   MOVEL     ART170        CODORD                         *
041400000000     C                   MOVEL     DESART        DES170                         *
041500000000     C                   SETON                                            26    *
041600000000     C                   GOTO      LANCIO                                       *
041700000000     C                   END                                                    *
041800000000     C*---------- IMPOSTO TIPO LAVORO X S/CLASSE ----------*
041900000000     C     SCM170        IFNE      'XXXXX'                                      *
042000000000     C                   MOVE      TES(5)        DESORD                         *
042100000000     C                   MOVEL     SCM170        CODORD                         *
042200000000     C                   MOVEL     DESSCM        DES170                         *
042300990126     C                   OPEN      tmamp04L                             94      *
042400000000     C                   SETON                                            22    *
042500000000     C                   GOTO      LANCIO                                       *
042600000000     C                   END                                                    *
042700000000     C*---------- IMPOSTO TIPO LAVORO X CLASSE  -----------*
042800000000     C     CLM170        IFNE      'XXX'                                        *
042900000000     C                   MOVE      TES(6)        DESORD                         *
043000000000     C                   MOVEL     CLM170        CODORD                         *
043100000000     C                   MOVEL     DESCLM        DES170                         *
043200990126     C                   OPEN      tmamp03L                             94      *
043300000000     C                   SETON                                            21    *
043400000000     C                   GOTO      LANCIO                                       *
043500000000     C                   END                                                    *
043600000000     C                   END                                                    *
043700000000     C*----------- LANCIO ELABORAZIONE --------------------*
043800000000     C     LANCIO        TAG                                                    *
043900000000     C  N11
044000000000     CANN12
044100000000     CANN17                                                                     *
044200000000     CANN21
044300000000     CANN22
044400000000     CANN26              SETON                                          6090    *
044500000000     C   90              MOVEL     ERR(1)        ERRORE                         *
044600000000     C   90              GOTO      EMISPT                                       *
044700000000     C*----------- X FORNITORE ----------------------------*
044800000000     C     GPM170        IFEQ      'XX'                                         *
044900000000     C                   MOVE      '99'          GPM170                         *
045000000000     C                   END                                                    *
045100000000     C     STM170        IFEQ      'XX'                                         *
045200000000     C                   MOVE      '99'          STM170                         *
045300000000     C                   END                                                    *
045400990126     C     FOR170        IFEQ      'XXXXXXXX'                                   *
045500990126     C                   MOVE      '99999999'    FOR170                         *
045600000000     C                   END                                                    *
045700000000     C*---------- X PRODOTTO ------------------------------*
045800000000     C     CLM170        IFEQ      'XXX'                                        *
045900000000     C                   MOVE      '999'         CLM170                         *
046000000000     C                   END                                                    *
046100000000     C     SCM170        IFEQ      'XXXXX'                                      *
046200000000     C                   MOVE      '99999'       SCM170                         *
046300000000     C                   END                                                    *
046400000000     C     ART170        IFEQ      ICS                                          *
046500000000     C                   MOVE      NOVE          ART170                         *
046600000000     C                   END                                                    *
046700000000     C*---------- PULIZIA SUBFILE -------------------------*
046800000000     C                   Z-ADD     1             CURSOR                         *
046900000000     C                   SETON                                            80    *
047000000000     C                   WRITE     SFL1CT                                       *
047100000000     C                   SETOFF                                           80    *
047200000000     C                   Z-ADD     0             REC1              5 081        *
047300000000     C                   Z-ADD     0             RECT              5 0          *
047400000000     C**------- LETTURA FILE ARCHIVIO STORICO ------------**
047500000000     C*---------- RICERCA X FORNITORE ---------------------*
047600000000     C   01              DO                                                     *
047700000000     C                   SETON                                            92    *
047800990126     C   11KGPM          SETLL     tmsoc23L                               94    *
047900990126     C   12KSTM          SETLL     tmsoc24L                               94    *
048000990126     C   17KFOR          SETLL     tmsoc26L                               94    *
048100880919     C  N94              SETON                                        90        *
048200880919     C   90              MOVEL     ERR(11)       ERRORE                         *
048300880919     C   90              GOTO      START                                        *
048400000000     C                   EXSR      RUTARC                                       *
048500000000     C                   SETOFF                                           92    *
048600000000     C                   GOTO      ENDRIC                                       *
048700000000     C                   END                                                    *
048800000000     C*---------- RICERCA X ARTICOLO ----------------------*
048900000000     C   02              DO                                                     *
049000990126     C   21CLM170        SETLL     tmamp03L                           94        *
049100990126     C   22SCM170        SETLL     tmamp04L                           94        *
049200990126     C   26ART170        SETLL     tmamp01L                           94        *
049300000000     C   94              SETON                                            90    *
049400000000     C   90              MOVEL     ERR(11)       ERRORE                         *
049500000000     C   90              GOTO      START                                        *
049600000000     C*-----------                                         *
049700000000     C     LET1          TAG                                                    *
049800000000     C   21CLM170        READE     ARTCLM                                 98    *
049900000000     C   22SCM170        READE     ARTSCM                                 98    *
050000000000     C   26ART170        READE     ARTCOD                                 98    *
050100000000     C   98              GOTO      ENDRIC                                       *
050200000000     C                   EXSR      CTLART                                       *
050300000000     C   94              GOTO      LET1                                         *
050400000000     C*                                                    *
050500990126     C     KART          SETLL     tmsoc25L                               98    *
050600000000     C  N98              GOTO      LET1                                         *
050700000000     C                   EXSR      RUTARC                                       *
050800000000     C                   GOTO      LET1                                         *
050900000000     C                   END                                                    *
051000000000     C*---------- EMISSIONE SUBFILE -----------------------*
051100000000     C     ENDRIC        TAG                                                    *
051200000000     C   99
051300000000     CANN81              SETON                                            90    *
051400000000     C   90              MOVEL     ERR(11)       ERRORE                         *
051500000000     C   90              GOTO      START                                        *
051600000000     C                   Z-ADD     REC1          RECT                           *
051700000000     C*--------- NUMERAZIONE PAGINE :                      *
051800000000     C     REC1          DIV       12            PAGINE                         *
051900000000     C                   MVR                     RESTO             5 052        *
052000000000     C   52PAGINE        ADD       1             PAGINE                         *
052100880920     C                   WRITE     OCO170TS                                     *
052200000000     C*---------                                           *
052300000000     C     EMISF1        TAG                                                    *
052400000000     C                   SETOFF                                           94    *
052500000000     C                   EXFMT     SFL1CT                                       *
052600990126     C   Kl              GOTO      START                                        *
052700000000     C*---------- EMISSIONE STAMPA ----------------------***
052800990409     C   Kh              DO                                                     *
052900000000     C                   OPEN      OFPRTSTD                             94      *
053000000000     C     1             DO        RECT          REC1                           *
053100000000     C     REC1          CHAIN     SFL1                               83        *
053200000000     C*---------                                           *
053300000000     C  N83              DO                                                     *
053400990127     c                   if        mocdor > 0
053500990127     C                   move      MOCDOR        DATAIso                        *
053600990127     C                   move      dataiso       dataeur                        *
053700990127     C                   move      DATAeur       dormof                         *
053800990127     c                   end
053900990127     c                   if        mocdbl > 0
054000990127     C                   move      MOCDbl        DATAIso                        *
054100990127     C                   move      dataiso       dataeur                        *
054200990127     C                   move      DATAeur       dblmof                         *
054300990127     c                   end
054400990127     c                   if        mocdft > 0
054500990127     C                   move      MOCDft        DATAIso                        *
054600990127     C                   move      dataiso       dataeur                        *
054700990127     C                   move      DATAeur       dftmof                         *
054800990127     c                   end
054900990127     c                   if        mocdpc > 0
055000990127     C                   move      MOCDpc        DATAIso                        *
055100990127     C                   move      dataiso       dataeur                        *
055200990127     C                   move      DATAeur       dptmof                         *
055300990127     c                   end
055400880919     C     MOCCMB        COMP      1                                  8484      *
055500000000     C     PAGE1         IFGE      50                                           *
055600000000     C                   EXCEPT    TESTA                                        *
055700000000     C                   Z-ADD     0             PAGE1                          *
055800000000     C                   END                                                    *
055900000000     C                   EXCEPT    DETAIL                                       *
056000000000     C                   END                                                    *
056100000000     C*---------                                           *
056200000000     C                   END                                                    *
056300000000     C                   EXCEPT    TOTALE                                       *
056400000000     C                   GOTO      EMISF1                                       *
056500000000     C                   END                                                    *
056600000000     C*---------  LETTURA VARIATI -------------------------*
056700000000     C                   READC     SFL1                                   83    *
056800000000     C  N83              DO                                                     *
056900000000     C     VISUAL        CABNE     'V'           EMISF1                         *
057000000000     C                   MOVE      *BLANKS       VISUAL                         *
057100000000     C                   SETON                                        91        *
057200000000     C*--------- POSIZIONAMENTO PAGINA :                   *
057300000000     C                   Z-ADD     NRELV         CURSOR                         *
057400000000     C     CURSOR        DIV       12            CURSOR                         *
057500000000     C                   MVR                     RESTO             5 052        *
057600000000     C     CURSOR        MULT      12            CURSOR                         *
057700000000     C   52CURSOR        ADD       1             CURSOR                         *
057800000000     C                   UPDATE    SFL1                                         *
057900990127     c                   if        mocdor > 0
058000990127     C                   move      MOCDOR        DATAIso                        *
058100990127     C                   move      dataiso       dataeur                        *
058200990127     C                   move      DATAeur       dormof                         *
058300990127     c                   end
058400990127     c                   if        mocdbl > 0
058500990127     C                   move      MOCDbl        DATAIso                        *
058600990127     C                   move      dataiso       dataeur                        *
058700990127     C                   move      DATAeur       dblmof                         *
058800990127     c                   end
058900990127     c                   if        mocdft > 0
059000990127     C                   move      MOCDft        DATAIso                        *
059100990127     C                   move      dataiso       dataeur                        *
059200990127     C                   move      DATAeur       dftmof                         *
059300990127     c                   end
059400990127     c                   if        mocdpc > 0
059500990127     C                   move      MOCDpc        DATAIso                        *
059600990127     C                   move      dataiso       dataeur                        *
059700990127     C                   move      DATAeur       dptmof                         *
059800990127     c                   end
059900990127     c                   if        mocdpc > 0
060000990127     C                   move      MOCDpc        DATAIso                        *
060100990127     C                   move      dataiso       dataeur                        *
060200990127     C                   move      DATAeur       dcnmof                         *
060300990127     c                   end
060400000000     C                   GOTO      EMISF1                                       *
060500000000     C                   END                                                    *
060600000000     C                   SETOFF                                       91        *
060700000000     C                   GOTO      EMISF1                                       *
060800000000     C*--- FINE LAVORO ------------------------------------*
060900000000     C     FINE          TAG                                                    *
061000000000     C                   SETON                                            LR    *
061100000000     C*--- ROUTINE CARICAMENTO SUBFILE DA ARCHIVIO --------*
061200000000     C***------------------------------------------------***
061300000000     CSR   RUTARC        BEGSR                                                  *
061400000000     C     RICARC        TAG                                                    *
061500990126     C   11GPM170        READE     tmsoc23L                               99    *
061600990126     C   12STM170        READE     tmsoc24L                               99    *
061700990126     C   17FOR170        READE     tmsoc26L                               99    *
061800990126     C   02KART1         READE     tmsoc25L                               99    *
061900000000     C   99              GOTO      ENDARC                                       *
062000880919     C                   MOVEL     MOCDMT        DESAR1                         *
062100000000     C*---------- CONTROLLO MOVIMENTI ---------------------*
062200000000     C                   EXSR      CTLMOV                                       *
062300000000     C   94              GOTO      RICARC                                       *
062400000000     C*---------- CONTROLLO DATA BOLLA --------------------*
062500880919     C     MOCDBL        CABLT     DAL170        RICARC                         *
062600880919     C     MOCDBL        CABGT     AL170         ENDARC                         *
062700000000     C                   Z-ADD     0             PRZMOF                         *
062800000000     C*---------- RIGHE PRODOTTO --------------------------*
062900000000     C   97              DO                                                     *
063000000000     C   01              EXSR      CTLART                                       *
063100000000     C   01
063200000000     CAN 94              GOTO      RICARC                                       *
063300990126     C                   Z-ADD     MOCPRZ        PRENET           14 5          *
063400990126     C     PRENET        MULT      MOCSC1        SCONTO           14 5          *
063500880926     C     SCONTO        DIV       100           SCONTO                         *
063600880926     C     PRENET        SUB       SCONTO        PRENET                         *
063700880926     C     PRENET        MULT      MOCSC2        SCONTO                         *
063800880926     C     SCONTO        DIV       100           SCONTO                         *
063900880926     C     PRENET        SUB       SCONTO        PRENET                         *
064000880926     C     PRENET        MULT      MOCSC3        SCONTO
064100880926     C     SCONTO        DIV       100           SCONTO                         *
064200880926     C     PRENET        SUB       SCONTO        PRENET                         *
064300990126     C     PRENET        MULT(H)   MOCQTA        PRZVAL           17 5          *
064400990126     C                   Z-ADD     PRZVAL        VALORE           17 5          *
064500000000     C     TOTVAL        ADD       VALORE        TOTVAL                         *
064600880919     C   02TOTQTA        ADD       MOCQTA        TOTQTA                         *
064700000000     C                   END                                                    *
064800000000     C*---------- ALTRE RIGHE -----------------------------*
064900000000     C   96              DO                                                     *
065000880919     C     MOCTRG        IFEQ      'D'                                          *DEBITO  FORNIT.
065100880919     C     MOCPRZ        MULT      -1            MOCPRZ                         *
065200000000     C                   END                                                    *
065300880919     C     MOCTRG        IFEQ      'C'                                          *CREDITO FORNIT.
065400880919     C                   Z-ADD     MOCPRZ        PRZVAL                         *
065500880919     C     MOCQTA        MULT      PRZVAL        VALORE                         *
065600000000     C     TOTVAL        ADD       VALORE        TOTVAL                         *
065700000000     C                   END                                                    *
065800880919     C                   MOVE      *BLANKS       MOCMAT                         *
065900880919     C                   MOVE      *BLANKS       MOCUMQ                         *
066000000000     C                   END                                                    *
066100000000     C*---------- IMPOSTO DATI X SUBFILE ------------------*
066200000000     C                   MOVE      *BLANKS       VISUAL                         *
066300000000     C     REC1          ADD       1             REC1                 81        *
066400000000     C                   Z-ADD     REC1          NRELV                          *
066500881025     C                   Z-ADD     PRZVAL        PRZMOF                         *
066600000000     C                   WRITE     SFL1                                         *
066700000000     C     REC1          COMP      9999                                   90    *
066800000000     C   90              MOVEL     ERR(12)       ERRORE                         *
066900000000     C   90              GOTO      START                                        *
067000000000     C                   GOTO      RICARC                                       *
067100000000     CSR   ENDARC        ENDSR                                                  *
067200000000     C*------- ROUTINE SELEZIONE DATI DAI MOVIMENTI -------*
067300000000     C***------------------------------------------------***
067400000000     CSR   CTLMOV        BEGSR                                                  *
067500000000     C                   SETOFF                                       9495      *
067600000000     C                   SETOFF                                         9697    *
067700000000     C*---------- CONTROLLO TIPO ORDINE -------------------*
067800000000     C     TIP170        IFNE      '99'                                         *
067900880919     C     MOCTOR        CABNE     TIP170        ENDMOV               9494      *
068000000000     C                   END                                                    *
068100000000     C*---------- CONTROLLO TIPO RIGA ---------------------*
068200880919     C     MOCTRG        LOOKUP    TRG                                    95    *
068300000000     C  N951             CABEQ     1             ENDMOV                   94    *
068400880919     C     MOCTRG        COMP      '1'                                    97    *
068500880919     C  N97MOCTRG        COMP      '2'                                    97    *
068600880919     C  N97MOCTRG        COMP      '3'                                    97    *
068700880919     C     MOCTRG        COMP      'C'                                    96    *
068800880919     C  N96MOCTRG        COMP      'D'                                    96    *
068900880919     C  N96MOCTRG        COMP      'I'                                    96    *
069000880919     C  N96MOCTRG        COMP      'S'                                    96    *
069100000000     C  N96
069200000000     CANN971             CABEQ     1             ENDMOV                   94    *
069300000000     C*---------- CONTROLLO FASE --------------------------*
069400000000     C     FAS170        IFNE      '99'                                         *
069500880919     C     MOCFAS        CABNE     FAS170        ENDMOV               9494      *
069600000000     C                   END                                                    *
069700880919     C     MOCFAS        COMP      'BB'                               9494      *
069800880919     C   94MOCFAS        COMP      'FA'                               9494      *
069900000000     C   94              GOTO      ENDMOV                                       *
070000000000     C*---------- GRUPPO MERCEOLOGICO ---------------------*
070100000000     C     GPM170        IFNE      '99'                                         *
070200880919     C     MOCGPM        CABNE     GPM170        ENDMOV               9494      *
070300000000     C                   END                                                    *
070400000000     C*---------- SETTORE MERCEOLOGICO---------------------*
070500000000     C     STM170        IFNE      '99'                                         *
070600880919     C     MOCSTM        CABNE     STM170        ENDMOV               9494      *
070700000000     C                   END                                                    *
070800000000     C*---------- FORNITORE -------------------------------*
070900990909     C     FOR170        IFNE      '99999999'                                   *
071000880919     C     MOCCLI        CABNE     FOR170        ENDMOV               9494      *
071100000000     C                   END                                                    *
071200990126     C     MOCCLI        CHAIN     tmclp01L                           51        *
071300000000     C   51              MOVEL     *ALL'*'       DESFO1                         *
071400880919     C   51              MOVE      MOCCLI        DESFO1                         *
071500880920     C  N51              MOVEL     CLPRSI        DESFO1                         *
071600880919     C                   MOVE      MOCCLI        OLDFOR            7            *
071700000000     C*----------                                          *
071800000000     CSR   ENDMOV        ENDSR                                                  *
071900000000     C*------- ROUTINE SELEZIONE DATI X ARTICOLO ----------*
072000000000     C***------------------------------------------------***
072100000000     CSR   CTLART        BEGSR                                                  *
072200000000     C                   SETOFF                                           94    *
072300000000     C   01              DO                                                     *
072400000000     C     ARTWW1        IFNE      ARTWW2                                       *
072500880919     C                   MOVE      MOCMAT        AMPMAT                         *ARTICOLO
072600990126     C     KEYAMP        CHAIN     tmamp01L                           51        *
072700000000     C   51              MOVE      *ALL'*'       DESAR1                         *
072800000000     C   51              MOVE      ARTWW1        DESAR1                         *
072900000000     C   51              GOTO      ENDART                                       *
073000000000     C                   MOVE      ARTWW1        ARTWW2           12            *
073100000000     C                   END                                                    *
073200000000     C                   END                                                    *
073300000000     C*---------- ARTICOLO --------------------------------*
073400000000     C     ART170        IFNE      NOVE                                         *
073500000000     C     AMPMAT        CABNE     ART170        ENDART               9494      *
073600000000     C                   END                                                    *
073700000000     C*---------- CLASSE ----------------------------------*
073800000000     C     CLM170        IFNE      '999'                                        *
073900000000     C     AMPCLM        CABNE     CLM170        ENDART               9494      *
074000000000     C                   END                                                    *
074100000000     C*---------- S/CLASSE --------------------------------*
074200000000     C     SCM170        IFNE      '99999'                                      *
074300000000     C     AMPSCM        CABNE     SCM170        ENDART               9494      *
074400000000     C                   END                                                    *
074500000000     CSR   ENDART        ENDSR                                                  *
074600990126     C*----------------------------------------------------*
074700990126     C* Reperimento dati società
074800990126     C*----------------------------------------------------*
074900990126     C     REPSOC        BEGSR
075000990126     C*
075100990126     C                   CALLB     'XSOC'
075200990126     C                   PARM                    TIPXSC            6
075300990126     C                   PARM                    SOCXSC            3
075400990126     C                   PARM                    CDSXSC            9 0
075500990126     C                   PARM                    MODXSC            3
075600990126     C                   PARM      *blanks       RTNXSC            1
075700990126     C                   PARM                    XSOCDS
075800990126     C                   PARM                    KPJBA
075900990126     C*
076000990126     C                   ENDSR
076100000000     O*--------------------------------------------------------------*
076200000000     O*     TESTATA                                                  *
076300000000     O*--------------------------------------------------------------*
076400000000     OOFPRTSTD  E            TESTA            01
076500000000     O                       SK1                132
076600000000     O                       DESDIT              20
076700990311     O                       woggi              121 '  /  /    '
076800000000     O                       PAGE          Z    132
076900000000     O          E            TESTA       1
077000000000     O                       SK2                132
077100990127     O                       DAL                 93 '  /  /    '
077200990311     O                       AL                 107 '  /  /    '
077300000000     O          E            TESTA       1
077400000000     O                       SK3                132
077500000000     O                       KNMUS              132
077600000000     O          E            TESTA       2
077700000000     O                                           99 'Selez.:'
077800000000     O                                          110 'Gruppo..xx'
077900000000     O                       GPM170             110
078000000000     O                                          130 'Settore....xx'
078100000000     O                       STM170             130
078200000000     O          E            TESTA       1
078300000000     O                       DESORD              36
078400000000     O                       CODORD              49
078500000000     O                       DES170              82
078600000000     O                                          110 'Classe.xxx'
078700000000     O                       CLM170             110
078800000000     O                                          130 'S/Classe.xxxxx'
078900000000     O                       SCM170             130
079000000000     O          E            TESTA       0  0
079100000000     O                       CODORD              49
079200000000     O                       DES170              82
079300000000     O          E            TESTA       1
079400000000     O                                          110 'Tipo..XX'
079500000000     O                       TIP170             110
079600000000     O                                          118 'Fase.xx'
079700000000     O                       FAS170             118
079800000000     O                                          123 'Tr.'
079900000000     O                       TRG                130
080000000000     O          E            TESTA       1
080100000000     O                       SK4                132
080200000000     O*--------------------------------------------------------------*
080300000000     O*     DETTAGLIO                                                *
080400000000     O*--------------------------------------------------------------*
080500000000     O          E            DETAIL      1
080600000000     O                       PAGE1              132
080700000000     O                       SK5                132
080800880919     O                       MOCNOR        Z      5
080900880919     O                       MOCTOR               8
081000990127     O                       DORMOF        Z     17
081100990127     O                       MOCNBL        Z     25
081200990127     O                       DBLMOF        Z     34
081300990311     O                       MOCNPC        Z     44
081400990311     O                       DPTMOF        Z     55
081500990127     O                       MOCMAT              68
081600000000     O                       DESAR1              86
081700880919     O                       MOCFAS              89
081800880919     O                       MOCUMQ              93
081900880919     O                       MOCQTA             105 '     0 ,  -'
082000880919     O                       MOCTRG             106
082100880919     O                       MOCVAL             111
082200990127     O                       MOCPRZ             128 '       0 ,     -'
082300990127     O                       MOCCPG             132
082400000000     O          E            DETAIL      1
082500000000     O                       PAGE1              132
082600000000     O                       SK5                132
082700990127     O                       MOCNFT        Z     25
082800990127     O                       DFTMOF        Z     34
082900000000     O                       DESFO1              82
083000990311     O                  84                      118 'cmb.'
083100990311     O                  84   MOCCMB             132 '     0 ,      '
083200000000     O/EJECT
083300000000     O*--------------------------------------------------------------*
083400000000     O*     TOTALE                                                   *
083500000000     O*--------------------------------------------------------------*
083600000000     O          E            TOTALE      1
083700000000     O                       SK6                132
083800000000     O                     02                   103 '.Quantità.'
083900000000     O                                          121 '.Valore.'
084000000000     O          E            TOTALE      1
084100000000     O                                           86 'Totale........:'
084200000000     O                     02TOTQTA             104 ' .   .   . 0 ,  -'
084300990127     O                       TOTVAL             128 ' .   .   . 0 ,     -'
084400000000     O          E            TOTALE      0  0
084500000000     O                     02TOTQTA             104 ' .   .   . 0 ,  -'
084600990127     O                       TOTVAL             128 ' .   .   . 0 ,     -'
084700000000     O          E            TOTALE      1
084800000000     O                       SK6                132
084900000000**   TES
085000880919Cliente.......:      1
085100000000Settore Merc..:      2
085200000000Gruppo..Merc..:      3
085300000000Prodotto......:      4
085400000000S/Classe.Merc.:      5
085500000000Classe...Merc.:      6
085600000000**   ERR
085700000000*- Ins. Param. Ordinamento  -*   ERR,1
085800880919*- Cod. CLIENTE      ERRATO -*   ERR,2
085900000000*- Cod. SETTORE MERC.ERRATO -*   ERR,3
086000000000*- Cod. GRUPPO  MERC.ERRATO -*   ERR,4
086100000000*- Cod. PRODOTTO     ERRATO -*   ERR,5
086200000000*- Cod. S/CLASSE     ERRATO -*   ERR,6
086300000000*- Cod. Classe       ERRATO -*   ERR,7
086400000000*- Date Limite       ERRATE -*   ERR,8
086500000000*- Tipo Ordine       ERRATO -*   ERR,9
086600000000*- Eseguire Scelta ORDINAM. -*   ERR,10
086700000000*- Non Trovati Movimenti !! -*   ERR,11
086800000000*- Righe Ordini oltre 9999 !-*   ERR,12
086900000000**   SK1
087000000000*-DESDIT-----------* ***------------------------------------------SK01
087100990311--------------------------------------*** il  X/XX/XX     Pag. XXXSK01
087200000000**   SK2
087300880919                     ** Gest.Ord.FORNIT. : Stampa ORDINI CLIENTI  SK02
087400990317 X DATA BOLLA dal xx.xx.xx  al  xx.xx.xx        PGM.:   TII302R   SK02
087500000000**   SK3
087600000000                     ***------------------------------------------SK03
087700000000--------------------------------------***       Utente  XXXXXXXXXXSK03
087800000000**   SK4
087900990127*------Ordine----*-Bol/Fat Cli.-*--Ns.Fattura----------*---Cod.---SK04
088000990127--Prodotto.Cliente  -:Fa-Um.--Quantità-Tr*Val.----Prezzo-------PagSK04
088100000000**   SK5
088200990127                 :              :                      :          SK05
088300990127                     :                   :                        SK05
088400000000**   SK6
088500000000*--------------*--------------*----------------------*------------SK06
088600000000-------------------*-------------------*-------------------------*SK06

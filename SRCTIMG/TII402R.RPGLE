000100990204     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990204     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300990204     F*-OFN170R-*-OF/38-*--------------------------------------------*
000400851218     F*-----------------*                                            *
000500860401     F*              - INTERROG. ARCHIVIO STORICO ORDINI FORNIT. -   *
000600851218     F*              ---------------------------------------------   *
000700860401     F*                                                    AZ.: N170 *
000800851218     F*                                                              *
000900990204     F*         FILE : OFN170D  - tmfop01L - ANTAB00F -              *
001000990204     F*                tmsofXXL - tmamp0XL - tmsofXXL - OFNDT2       *
001100851218     F*--------------------------------------------------------------*
001200860401     F*  INDIC.: 01 - GESTIONE X FORNITORE                           *
001300851218     F*          02 - GESTIONE X PRODOTTO                            *
001400851218     F*                                                              *
001500860401     F*  ORDIN.: 12 - SETTORE MERCEOLOGICO     22 - S/CLASSE MERC.   *
001600860401     F*          11 - GRUPPO  MERCEOLOGICO     21 - CLASSE   MERC.   *
001700860401     F*          17 - FORNITORE                26 - PRODOTTO         *
001800851218     F*--------------------------------------------------------------*
001900990211     F*         PGM. :
002000860401     F*                ANU005R - RICERCA ALFABETICA MATERIE PRIME    *
002100990317     F*                tii103R - RICERCA ALFABETICA FORNITORI        *
002200990317     F*                TII110R - RICERCA CLASSE   MERCEOLOGICA       *
002300990317     F*                TII111R - RICERCA S/CLASSE MERCEOLOGICA       *
002400990317     F*                TII114R - RICERCA GRUPPO   MERCEOLOGICO       *
002500990317     F*                TII115R - RICERCA SETTORE  MERCEOLOGICO       *
002600851218     F*--------------------------------------------------------------*
002700990204     Ftmamp01L  IF   E           K DISK
002800990204     F                                     RENAME(tmamp000:ARTCOD)
002900990204     Ftmamp03L  IF   E           K DISK    USROPN
003000990204     F                                     RENAME(tmamp000:ARTCLM)
003100990204     Ftmamp04L  IF   E           K DISK    USROPN
003200990204     F                                     RENAME(tmamp000:ARTSCM)
003300851218     F*----------------------------------------------------------------
003400990204     Ftmsof23L  IF   E           K DISK    USROPN
003500990204     F                                     RENAME(tmsof000:SOGPM)
003600990204     F                                     RENAME(tmMOF000:MOGPM)
003700990204     Ftmsof24L  IF   E           K DISK    USROPN
003800990204     F                                     RENAME(tmsof000:SOSTM)
003900990204     F                                     RENAME(tmMOF000:MOSTM)
004000990204     Ftmsof25L  IF   E           K DISK    USROPN
004100990204     F                                     RENAME(tmsof000:SOART)
004200990204     F                                     RENAME(tmMOF000:MOART)
004300990204     Ftmsof26L  IF   E           K DISK    USROPN
004400990204     F                                     RENAME(tmsof000:SOFOR)
004500990204     F                                     RENAME(tmMOF000:MOFOR)
004600851218     F*----------------------------------------------------------------
004700990204     Ftmfop01L  IF   E           K DISK
004800851218     FANTAB00F  IF   E           K DISK
004900990317     FTII402D   CF   E             WORKSTN
005000851218     F                                     SFILE(SFL1:REC1)
005100860401     FOFPRTSTD  O    F  132        PRINTER OFLIND(*INOF)
005200860401     F                                     USROPN
005300851218     F*----------------------------------------------------------------
005400860401     D TES             S             15    DIM(6) CTDATA PERRCD(1)              *TESTATE
005500860401     D ERR             S             30    DIM(12) CTDATA PERRCD(1)             * ERRORI
005600851218     D SK1             S             66    DIM(2) CTDATA PERRCD(1)              *FINCATURA 1
005700851218     D SK2             S             66    DIM(2) CTDATA PERRCD(1)              *FINCATURA 2
005800851218     D SK3             S             66    DIM(2) CTDATA PERRCD(1)              *FINCATURA 3
005900851218     D SK4             S             66    DIM(2) CTDATA PERRCD(1)              *FINCATURA 4
006000851218     D SK5             S             66    DIM(2) CTDATA PERRCD(1)              *FINCATURA 5
006100851218     D SK6             S             66    DIM(2) CTDATA PERRCD(1)              *FINCATURA 6
006200871006     D ARTWW1          DS            12
006300860401     D*----- SCOMPOSIZIONE ARTICOLO :
006400871006     D  MOFMAT                 1     12
006500860401     D OFNDT2          DS           512
006600860401     D*-----DATA AREA ORDINI FORNIT. -2- :
006700860401     D  DT2050                 1     50
006800860401     D  DT2DUA                51     56  0
006900860402     D  DT2256                57    256
007000860402     D  DT2512               257    512
007100860402     D                 DS
007200860402     D*-----TIPO RIGHE :
007300860402     D  TG1170                 1      1
007400860402     D  TG2170                 2      2
007500860402     D  TG3170                 3      3
007600860402     D  TG4170                 4      4
007700860402     D  TG5170                 5      5
007800860402     D  TG6170                 6      6
007900860402     D  TG7170                 7      7
008000860402     D  TRG                    1      7
008100860402     D                                     DIM(7)                               *TIPO RIGHE
008200851218     D KPJBA         E DS
008300851218     D  MSGERR               453    502
008400990204     D soc001        E DS                  EXTNAME(xsoc001ds)
008500990204     D xsocds          DS          1000
008600990204     d dataiso         s               d   datfmt(*iso)
008700990204     d dataeur         s               d   datfmt(*eur)
008800851218     C*----------------------------------------------------*
008900851218     C     *ENTRY        PLIST                                                  *
009000851218     C                   PARM                    KPJBA                          *
009100990204     C*---------- RICERCA DITTA :
009200990204     C                   MOVEL     'SOC001'      TIPXSC
009300990204     C                   MOVEL     *blank        SOCXSC
009400990204     C                   EXSR      REPSOC
009500990204     C     RTNXSC        IFNE      '1'
009600990204     C                   MOVEL     XSOCDS        SOC001
009700990204     C                   MOVEL     xscrgs        desdit
009800990204     C                   MOVEL     xscsoc        wsoc              3
009900990204     c                   end
010000990204     c                   time                    como14           14 0
010100990204     c                   move      como14        woggi             8 0
010200990204     c                   move      woggi         dataeur
010300990204     c                   move      dataeur       dataiso
010400990204     c                   move      dataiso       woggig            8 0
010500860401     C*--- FILE ANAGRAFICO MATERIE PRIME :                 *
010600860401     C     KEYAMP        KLIST                                                  *
010700860401     C                   KFLD                    AMPMAT                         *
010800851218     C*--- FILE ANAGRAFICO TABELLE "ANTAB00F"              *
010900851218     C     KEYTAB        KLIST                                                  *
011000851218     C                   KFLD                    TABGRU                         *
011100851218     C                   KFLD                    TABCOD                         *
011200860401     C*---------- RICERCA X FORNITORE ---------------------*
011300860401     C     KFOR          KLIST                                                  *
011400860401     C                   KFLD                    FOR170                         *
011500860401     C*---------- RICERCA X GRUPPO ------------------------*
011600860401     C     KGPM          KLIST                                                  *
011700860401     C                   KFLD                    GPM170                         *
011800860401     C*---------- RICERCA X SETTORE -----------------------*
011900860401     C     KSTM          KLIST                                                  *
012000860401     C                   KFLD                    STM170                         *
012100851218     C*---------- RICERCA X ARTICOLO ----------------------*
012200851218     C     KART          KLIST                                                  *
012300860401     C                   KFLD                    AMPMAT                         *
012400851218     C     KART1         KLIST                                                  *
012500860401     C                   KFLD                    AMPMAT                         *
012600851218     C*---------- RICERCA DATA AREA ARCHIVIAZIONE ---------*
012700860401     C     *DTAARA       DEFINE                  OFNDT2                         *
012800851218     C     SUDT2         TAG                                                    *
012900860401     C     *LOCK         IN        OFNDT2                               90      *
013000851218     C   90              GOTO      SUDT2                                        *
013100860401     C                   OUT       OFNDT2                                       *
013200990204     C     *dmy          MOVE      DT2DUA        DATAeur                        *
013300990204     C                   move      dataeur       dataiso                        *
013400990204     C                   move      DATAIso       DATARC            8 0          *DATA ARCHIVIO
013500851218     C*---------- IMPOSTAZIONE PROMPT ---------------------*
013600990204     C                   Z-ADD     woggi         DAL
013700851218     C                   MOVEL     '0101'        DAL                            *
013800990204     C                   Z-ADD     woggi         AL                             *
013900860401     C                   MOVE      '99'          TIP170                         *
014000860402     C                   MOVEA     '123CDIS'     TRG                            *
014100860401     C                   MOVE      '99'          FAS170                         *
014200851218     C     START         TAG                                                    *
014300990204     C                   MOVE      *ALL'X'       FOR170                         *
014400990204     C                   MOVE      'XX'          GPM170                         *
014500990204     C                   MOVE      'XX'          STM170                         *
014600990204     C                   MOVE      *ALL'X'       ART170                         *
014700990204     C                   MOVE      'XXX'         CLM170                         *
014800990204     C                   MOVE      'XXXXX'       SCM170                         *
014900860401     C                   MOVE      *BLANKS       ORD170                         *
015000860402     C                   MOVE      *BLANKS       DESORD                         *
015100860402     C                   MOVE      *BLANKS       CODORD                         *
015200860402     C                   MOVE      *BLANKS       DES170                         *
015300860402     C                   MOVE      *BLANKS       DESFOR                         *
015400860402     C                   MOVE      *BLANKS       DESGPM                         *
015500860402     C                   MOVE      *BLANKS       DESSTM                         *
015600860402     C                   MOVE      *BLANKS       DESART                         *
015700860402     C                   MOVE      *BLANKS       DESCLM                         *
015800860402     C                   MOVE      *BLANKS       DESSCM                         *
015900990204     C                   Z-ADD     0             DAL170            8 0          *
016000990204     C                   Z-ADD     0             AL170             8 0          *
016100990204     C                   Z-ADD     0             TOTVAL                         * VAL.ORDINI
016200860401     C                   Z-ADD     0             TOTQTA           12 2          * QTA.ORDINI
016300851218     C                   Z-ADD     0             PAGINE                         * PAGINE VIDEO
016400851218     C                   Z-ADD     0             PAGE              3 0          *
016500851218     C                   Z-ADD     50            PAGE1             3 0          *
016600851218     C  N90              MOVEA     *ZEROS        *IN(1)                         *
016700851218     C*----------------------------------------------------*
016800851218     C     EMISPT        TAG                                                    *
016900860401     C                   EXFMT     OFN170PT                                     * PROMPT
017000851218     C                   MOVEA     *ZEROS        *IN(1)                         *
017100851218     C                   MOVE      *BLANKS       ERRORE                         *
017200990409     C   Kc              GOTO      FINE                                         *
017300851218     C*---------                                           *
017400860401     C     ORD170        COMP      '1'                                029001    *
017500851218     C   90              SETON                                        9060      *
017600860402     C   90              MOVEL     ERR(10)       ERRORE                         *
017700851218     C   90              GOTO      EMISPT                                       *
017800860401     C*---------- RICERCA FORNITORE -----------------------*
017900990204     C     FOR170        IFNE      'XXXXXXXX'                                   *
018000990204     C     FOR170        IFEQ      '?       '                                   *
018100860401     C                   MOVE      *ZEROS        FOR170                         *
018200990317     C                   CALL      'TII103R'                                    *
018300860401     C                   PARM                    FOR170                         *
018400860401     C                   PARM                    DESFOR                         *
018500851218     C                   SETON                                            61    *
018600990204     C     FOR170        COMP      '99999999'                             90    *
018700851218     C   90              MOVEL     ERR(2)        ERRORE                         *
018800851218     C                   GOTO      EMISPT                                       *
018900851218     C                   END                                                    *
019000851218     C*----------                                          *
019100860401     C                   MOVE      *BLANKS       DESFOR                         *
019200990204     C     FOR170        CHAIN     tmfop01L                           90        *
019300851218     C   90              SETON                                          61      *
019400851218     C   90              MOVEL     ERR(2)        ERRORE                         *
019500851218     C   90              GOTO      EMISPT                                       *
019600860401     C                   MOVEL     FOPRSI        DESFOR                         *
019700851218     C                   END                                                    *
019800860401     C*---------- RICERCA ORDINAMENTO SETTORE -------------*
019900860401     C     STM170        IFNE      'XX'                                         *
020000860401     C     STM170        IFEQ      '? '                                         *
020100860401     C                   MOVE      *BLANKS       STM170                         *
020200990317     C                   CALL      'TII115R'                                    *
020300860401     C                   PARM                    STM170                         *
020400860401     C                   PARM                    DESSTM                         *
020500860401     C                   SETON                                        62        *
020600860401     C     STM170        COMP      '99'                                   90    *
020700860401     C   90              MOVEL     ERR(3)        ERRORE                         *
020800860401     C   90              MOVE      *BLANKS       STM170                         *
020900851218     C                   GOTO      EMISPT                                       *
021000851218     C                   END                                                    *
021100851218     C*----------                                          *
021200860401     C                   MOVE      '160'         TABGRU                         *
021300851218     C                   MOVE      *ZEROS        TABCOD                         *
021400860401     C                   MOVE      STM170        TABCOD                         *
021500851218     C     KEYTAB        CHAIN     ANTAB00F                           90        *
021600860401     C   90              SETON                                        62        *
021700860401     C   90              MOVEL     ERR(3)        ERRORE                         *
021800851218     C   90              GOTO      EMISPT                                       *
021900860401     C                   MOVEL     TABDES        DESSTM                         *
022000851218     C                   END                                                    *
022100860401     C*---------- RICERCA ORDINAMENTO GRUPPO --------------*
022200860401     C     GPM170        IFNE      'XX'                                         *
022300860401     C     GPM170        IFEQ      '? '                                         *
022400860401     C                   MOVE      *BLANKS       GPM170                         *
022500990317     C                   CALL      'TII114R'                                    *
022600860401     C                   PARM                    GPM170                         *
022700860401     C                   PARM                    DESGPM                         *
022800860401     C                   SETON                                        63        *
022900860401     C     GPM170        COMP      '99'                                   90    *
023000860401     C   90              MOVEL     ERR(4)        ERRORE                         *
023100860401     C   90              MOVE      *BLANKS       GPM170                         *
023200851218     C                   GOTO      EMISPT                                       *
023300851218     C                   END                                                    *
023400851218     C*----------                                          *
023500860401     C                   MOVE      '159'         TABGRU                         *
023600851218     C                   MOVE      *ZEROS        TABCOD                         *
023700860401     C                   MOVE      GPM170        TABCOD                         *
023800851218     C     KEYTAB        CHAIN     ANTAB00F                           90        *
023900860401     C   90              SETON                                        63        *
024000860401     C   90              MOVEL     ERR(4)        ERRORE                         *
024100851218     C   90              GOTO      EMISPT                                       *
024200860401     C                   MOVEL     TABDES        DESGPM                         *
024300851218     C                   END                                                    *
024400860401     C*---------- RICERCA PRODOTTO ------------------------*
024500871006     C                   MOVEL     'XXXXXXX'     ICS              12
024600870105     C                   MOVE      'XXXXXXX'     ICS
024700990204     C                   MOVEL     *all'9'       NOVE             12
024800871006     C                   MOVEL     '?'           INTERR           12
024900870105     C     ART170        IFNE      ICS                                          *
025000870105     C     ART170        IFEQ      INTERR                                       *
025100860401     C                   MOVE      *BLANKS       ART170                         *
025200990317     C                   CALL      'TII104R'                                    *
025300860401     C                   PARM                    ART170                         *
025400860401     C                   PARM                    DESART                         *
025500860401     C                   SETON                                        65        *
025600870105     C     ART170        COMP      NOVE                                   90    *
025700860401     C   90              MOVEL     ERR(5)        ERRORE                         *
025800851218     C                   GOTO      EMISPT                                       *
025900851218     C                   END                                                    *
026000851218     C*----------                                          *
026100860401     C                   MOVE      ART170        AMPMAT                         *
026200990204     C     KEYAMP        CHAIN     tmamp01L                           90        *
026300860401     C   90              SETON                                        65        *
026400860401     C   90              MOVEL     ERR(5)        ERRORE                         *
026500851218     C   90              GOTO      EMISPT                                       *
026600860401     C                   MOVEL     AMPDMT        DESART                         *
026700851218     C                   END                                                    *
026800860401     C*---------- RICERCA ORDINAMENTO S/CLASSE ------------*
026900860401     C     SCM170        IFNE      'XXXXX'                                      *
027000860401     C     SCM170        IFEQ      '?    '                                      *
027100860401     C                   MOVE      *BLANKS       SCM170                         *
027200990317     C                   CALL      'TII111R'                                    *
027300860401     C                   PARM                    SCM170                         *
027400860401     C                   PARM                    DESSCM                         *
027500860401     C                   SETON                                        66        *
027600860401     C     SCM170        COMP      '99999'                                90    *
027700860401     C   90              MOVEL     ERR(6)        ERRORE                         *
027800860401     C   90              MOVE      *BLANKS       SCM170                         *
027900860401     C                   GOTO      EMISPT                                       *
028000860401     C                   END                                                    *
028100860401     C*----------                                          *
028200860401     C                   MOVE      '156'         TABGRU                         *
028300860401     C                   MOVE      *ZEROS        TABCOD                         *
028400860401     C                   MOVE      SCM170        TABCOD                         *
028500860401     C     KEYTAB        CHAIN     ANTAB00F                           90        *
028600860401     C   90              SETON                                        66        *
028700860401     C   90              MOVEL     ERR(6)        ERRORE                         *
028800860401     C   90              GOTO      EMISPT                                       *
028900860401     C                   MOVEL     TABDES        DESSCM                         *
029000860401     C                   END                                                    *
029100851218     C*---------- RICERCA CLASSE --------------------------*
029200860401     C     CLM170        IFNE      'XXX'                                        *
029300860401     C     CLM170        IFEQ      '?  '                                        *
029400860401     C                   MOVE      *BLANKS       CLM170                         *
029500990317     C                   CALL      'TII110R'                                    *
029600860401     C                   PARM                    CLM170                         *
029700860401     C                   PARM                    DESCLM                         *
029800860401     C                   SETON                                        67        *
029900860401     C     CLM170        COMP      '999'                                  90    *
030000860401     C   90              MOVEL     ERR(7)        ERRORE                         *
030100860401     C   90              MOVE      *BLANKS       CLM170                         *
030200851218     C                   GOTO      EMISPT                                       *
030300851218     C                   END                                                    *
030400851218     C*----------                                          *
030500860401     C                   MOVE      '155'         TABGRU                         *
030600851218     C                   MOVE      *ZEROS        TABCOD                         *
030700860401     C                   MOVE      CLM170        TABCOD                         *
030800851218     C     KEYTAB        CHAIN     ANTAB00F                           90        *
030900860401     C   90              SETON                                        67        *
031000860401     C   90              MOVEL     ERR(7)        ERRORE                         *
031100851218     C   90              GOTO      EMISPT                                       *
031200860401     C                   MOVEL     TABDES        DESCLM                         *
031300851218     C                   END                                                    *
031400851218     C*---- CONTROLLO DATA DAL SOLO SE DIVERSO DA ZERO ----*
031500860401     C                   Z-ADD     0             DAL170                         *
031600860401     C                   Z-ADD     0             AL170                          *
031700851218     C     DAL           IFNE      *ZEROS                                       *
031800990204     c     *eur          test(d)                 dal                    90
031900990204     c                   if        *in90
032000990204     c     *dmy          test(d)                 dal                    90
032100990204     c  n90*dmy          move      dal           dataeur
032200990204     c  n90              move      dataeur       dal
032300990204     c                   else
032400990204     c                   move      dal           dataeur
032500990204     c                   end
032600990204     C   90              SETON                                        74        *
032700990204     C   90              MOVEL     ERR(8)        ERRORE                         *
032800990204     c   90              goto      emispt
032900990204     c                   move      dataeur       dataiso
033000990204     c                   move      dataiso       dal170
033100851218     C                   END                                                    *
033200851218     C*---------- CONTROLLO DATA AL  ----------------------*
033300990204     c     *eur          test(d)                 al                     90
033400990204     c                   if        *in90
033500990204     c     *dmy          test(d)                 al                     90
033600990204     c  n90*dmy          move      al            dataeur
033700990204     c  n90              move      dataeur       al
033800990204     c                   else
033900990204     c                   move      al            dataeur
034000990204     c                   end
034100990204     C   90              SETON                                        74        *
034200990204     C   90              MOVEL     ERR(8)        ERRORE                         *
034300990204     c   90              goto      emispt
034400990204     c                   move      dataeur       dataiso
034500990204     c                   move      dataiso       al170
034600860401     C*---------- CONTROLLO INCROCIATO FRA DATE -----------*
034700860401     C     DAL170        IFGT      AL170                                        *
034800851218     C                   SETON                                        9074      *
034900860401     C                   MOVEL     ERR(8)        ERRORE                         *
035000851218     C                   GOTO      EMISPT                                       *
035100851218     C                   END                                                    *
035200851218     C*---------- RICERCA TIPO ORDINE ---------------------*
035300851218     C                   MOVE      *BLANKS       DESTIP                         *
035400860401     C     TIP170        IFNE      '99'                                         *
035500860401     C     TIP170        IFEQ      '? '                                         *
035600860401     C                   MOVE      *BLANKS       TIP170                         *
035700851218     C                   MOVE      *BLANKS       DESTIP           25            *
035800990317     C                   CALL      'TII117R'                                    *
035900860401     C                   PARM                    TIP170                         *
036000851218     C                   PARM                    DESTIP                         *
036100851218     C                   SETON                                        75        *
036200860401     C     TIP170        COMP      '99'                                   90    *
036300860401     C   90              MOVEL     ERR(9)        ERRORE                         *
036400851218     C                   GOTO      EMISPT                                       *
036500851218     C                   END                                                    *
036600851218     C                   MOVE      '162'         TABGRU                         *
036700851218     C                   MOVE      *ZEROS        TABCOD                         *
036800860401     C                   MOVE      TIP170        TABCOD                         *
036900851218     C     KEYTAB        CHAIN     ANTAB00F                           90        *
037000851218     C   90              SETON                                        75        *
037100860401     C   90              MOVEL     ERR(9)        ERRORE                         *
037200851218     C   90              GOTO      EMISPT                                       *
037300851218     C                   MOVEL     TABDES        DESTIP                         *
037400851218     C                   END                                                    *
037500860401     C*---------- IMPOSTO TIPO LAVORO X FORNITORE----------*
037600851218     C   01              DO                                                     *
037700860401     C*----------                                          *FORNITORE
037800990204     C     FOR170        IFNE      'XXXXXXXX'                                   *
037900851218     C                   MOVE      TES(1)        DESORD                         *
038000860401     C                   MOVEL     FOR170        CODORD                         *
038100860401     C                   MOVEL     DESFOR        DES170                         *
038200990204     C                   OPEN      tmsof26L                             94      *
038300851218     C                   SETON                                            17    *
038400851218     C                   GOTO      LANCIO                                       *
038500851218     C                   END                                                    *
038600860401     C*---------- IMPOSTO TIPO LAVORO X SETTORE -----------*
038700860401     C     STM170        IFNE      'XX'                                         *
038800860401     C                   MOVE      TES(2)        DESORD                         *
038900860401     C                   MOVEL     STM170        CODORD                         *
039000860401     C                   MOVEL     DESSTM        DES170                         *
039100990204     C                   OPEN      tmsof24L                             94      *
039200851218     C                   SETON                                            12    *
039300851218     C                   GOTO      LANCIO                                       *
039400851218     C                   END                                                    *
039500860401     C*---------- IMPOSTO TIPO LAVORO X GRUPPO ------------*
039600860401     C     GPM170        IFNE      'XX'                                         *
039700860401     C                   MOVE      TES(3)        DESORD                         *
039800860401     C                   MOVEL     GPM170        CODORD                         *
039900860401     C                   MOVEL     DESGPM        DES170                         *
040000990204     C                   OPEN      tmsof23L                             94      *
040100851218     C                   SETON                                            11    *
040200851218     C                   GOTO      LANCIO                                       *
040300851218     C                   END                                                    *
040400851218     C                   END                                                    *
040500851218     C*---------- IMPOSTO TIPO LAVORO X ARTICOLO ----------*
040600851218     C   02              DO                                                     *
040700990204     C                   OPEN      tmsof25L                             94      *
040800851218     C*----------                                          *ARTICOLO
040900871006     C     ART170        IFNE      ICS                                          *
041000860401     C                   MOVE      TES(4)        DESORD                         *
041100860401     C                   MOVEL     ART170        CODORD                         *
041200860401     C                   MOVEL     DESART        DES170                         *
041300851218     C                   SETON                                            26    *
041400851218     C                   GOTO      LANCIO                                       *
041500851218     C                   END                                                    *
041600860401     C*---------- IMPOSTO TIPO LAVORO X S/CLASSE ----------*
041700860401     C     SCM170        IFNE      'XXXXX'                                      *
041800860401     C                   MOVE      TES(5)        DESORD                         *
041900860401     C                   MOVEL     SCM170        CODORD                         *
042000860401     C                   MOVEL     DESSCM        DES170                         *
042100990204     C                   OPEN      tmamp04L                             94      *
042200851218     C                   SETON                                            22    *
042300851218     C                   GOTO      LANCIO                                       *
042400851218     C                   END                                                    *
042500860401     C*---------- IMPOSTO TIPO LAVORO X CLASSE  -----------*
042600860401     C     CLM170        IFNE      'XXX'                                        *
042700860401     C                   MOVE      TES(6)        DESORD                         *
042800860401     C                   MOVEL     CLM170        CODORD                         *
042900860401     C                   MOVEL     DESCLM        DES170                         *
043000990204     C                   OPEN      tmamp03L                             94      *
043100851218     C                   SETON                                            21    *
043200851218     C                   GOTO      LANCIO                                       *
043300851218     C                   END                                                    *
043400851218     C                   END                                                    *
043500851218     C*----------- LANCIO ELABORAZIONE --------------------*
043600851218     C     LANCIO        TAG                                                    *
043700860401     C  N11
043800860401     CANN12
043900860401     CANN17                                                                     *
044000860401     CANN21
044100860401     CANN22
044200860401     CANN26              SETON                                          6090    *
044300860402     C   90              MOVEL     ERR(1)        ERRORE                         *
044400851218     C   90              GOTO      EMISPT                                       *
044500860401     C*----------- X FORNITORE ----------------------------*
044600860401     C     GPM170        IFEQ      'XX'                                         *
044700860401     C                   MOVE      '99'          GPM170                         *
044800851218     C                   END                                                    *
044900860401     C     STM170        IFEQ      'XX'                                         *
045000860401     C                   MOVE      '99'          STM170                         *
045100851218     C                   END                                                    *
045200990204     C     FOR170        IFEQ      'XXXXXXXX'                                   *
045300990204     C                   MOVE      '99999999'    FOR170                         *
045400851218     C                   END                                                    *
045500860401     C*---------- X PRODOTTO ------------------------------*
045600860401     C     CLM170        IFEQ      'XXX'                                        *
045700860401     C                   MOVE      '999'         CLM170                         *
045800851218     C                   END                                                    *
045900860401     C     SCM170        IFEQ      'XXXXX'                                      *
046000860401     C                   MOVE      '99999'       SCM170                         *
046100851218     C                   END                                                    *
046200870105     C     ART170        IFEQ      ICS                                          *
046300870105     C                   MOVE      NOVE          ART170                         *
046400851218     C                   END                                                    *
046500851218     C*---------- PULIZIA SUBFILE -------------------------*
046600851218     C                   Z-ADD     1             CURSOR                         *
046700851218     C                   SETON                                            80    *
046800851218     C                   WRITE     SFL1CT                                       *
046900851218     C                   SETOFF                                           80    *
047000851218     C                   Z-ADD     0             REC1              5 081        *
047100851218     C                   Z-ADD     0             RECT              5 0          *
047200851218     C**------- LETTURA FILE ARCHIVIO STORICO ------------**
047300860401     C*---------- RICERCA X FORNITORE ---------------------*
047400880915     C   01              DO                                                     *
047500860401     C                   SETON                                            92    *
047600990204     C   11KGPM          SETLL     tmsof23L                               94    *
047700990204     C   12KSTM          SETLL     tmsof24L                               94    *
047800990204     C   17KFOR          SETLL     tmsof26L                               94    *
047900880915     C  N94              SETON                                        90
048000880915     C   90              MOVEL     ERR(11)       ERRORE
048100880915     C   90              GOTO      START
048200851218     C                   EXSR      RUTARC                                       *
048300860401     C                   SETOFF                                           92    *
048400880915     C                   GOTO      ENDRIC                                       *
048500851218     C                   END                                                    *
048600851218     C*---------- RICERCA X ARTICOLO ----------------------*
048700880915     C   02              DO                                                     *
048800990204     C   21CLM170        SETLL     tmamp03L                           94        *
048900990204     C   22SCM170        SETLL     tmamp04L                           94        *
049000990204     C   26ART170        SETLL     tmamp01L                           94        *
049100880915     C   94              SETON                                            90    *
049200860401     C   90              MOVEL     ERR(11)       ERRORE                         *
049300851218     C   90              GOTO      START                                        *
049400851218     C*-----------                                         *
049500851218     C     LET1          TAG                                                    *
049600860401     C   21CLM170        READE     ARTCLM                                 98    *
049700860401     C   22SCM170        READE     ARTSCM                                 98    *
049800860401     C   26ART170        READE     ARTCOD                                 98    *
049900880915     C   98              GOTO      ENDRIC                                       *
050000851218     C                   EXSR      CTLART                                       *
050100851218     C   94              GOTO      LET1                                         *
050200851218     C*                                                    *
050300990204     C     KART          SETLL     tmsof25L                               98    *
050400880915     C  N98              GOTO      LET1                                         *
050500851218     C                   EXSR      RUTARC                                       *
050600851218     C                   GOTO      LET1                                         *
050700851218     C                   END                                                    *
050800860401     C*---------- EMISSIONE SUBFILE -----------------------*
050900860401     C     ENDRIC        TAG                                                    *
051000851218     C   99
051100851218     CANN81              SETON                                            90    *
051200860401     C   90              MOVEL     ERR(11)       ERRORE                         *
051300851218     C   90              GOTO      START                                        *
051400860402     C                   Z-ADD     REC1          RECT                           *
051500851218     C*--------- NUMERAZIONE PAGINE :                      *
051600851218     C     REC1          DIV       12            PAGINE                         *
051700851218     C                   MVR                     RESTO             5 052        *
051800851218     C   52PAGINE        ADD       1             PAGINE                         *
051900860401     C                   WRITE     OFN170TS                                     *
052000851218     C*---------                                           *
052100851218     C     EMISF1        TAG                                                    *
052200860401     C                   SETOFF                                           94    *
052300851218     C                   EXFMT     SFL1CT                                       *
052400990204     C   Kl              GOTO      START                                        *
052500851218     C*---------- EMISSIONE STAMPA ----------------------***
052600990409     C   Kh              DO                                                     *
052700860401     C                   OPEN      OFPRTSTD                             94      *
052800860401     C     1             DO        RECT          REC1                           *
052900851218     C     REC1          CHAIN     SFL1                               83        *
053000851218     C*---------                                           *
053100860401     C  N83              DO                                                     *
053200990204     c                   if        mofdor > 0
053300990204     C                   move      MOfDOR        DATAIso                        *
053400990204     C                   move      dataiso       dataeur                        *
053500990204     C                   move      DATAeur       dormof                         *
053600990204     c                   end
053700990204     c                   if        mofdbl > 0
053800990204     C                   move      MOfDbl        DATAIso                        *
053900990204     C                   move      dataiso       dataeur                        *
054000990204     C                   move      DATAeur       dblmof                         *
054100990204     c                   end
054200990204     c                   if        mofdft > 0
054300990204     C                   move      MOfDft        DATAIso                        *
054400990204     C                   move      dataiso       dataeur                        *
054500990204     C                   move      DATAeur       dftmof                         *
054600990204     c                   end
054700990204     c                   if        mofdpt > 0
054800990204     C                   move      MOfDpt        DATAIso                        *
054900990204     C                   move      dataiso       dataeur                        *
055000990204     C                   move      DATAeur       dptmof                         *
055100990204     c                   end
055200860402     C     MOFCMB        COMP      1                                  8484      *
055300851218     C     PAGE1         IFGE      50                                           *
055400851218     C                   EXCEPT    TESTA                                        *
055500851218     C                   Z-ADD     0             PAGE1                          *
055600851218     C                   END                                                    *
055700851218     C                   EXCEPT    DETAIL                                       *
055800851218     C                   END                                                    *
055900851218     C*---------                                           *
056000851218     C                   END                                                    *
056100860401     C                   EXCEPT    TOTALE                                       *
056200860401     C                   GOTO      EMISF1                                       *
056300860401     C                   END                                                    *
056400851218     C*---------  LETTURA VARIATI -------------------------*
056500990913     c                   if        rec1 > 0
056600860402     C                   READC     SFL1                                   83    *
056700860402     C  N83              DO                                                     *
056800851218     C     VISUAL        CABNE     'V'           EMISF1                         *
056900851218     C                   MOVE      *BLANKS       VISUAL                         *
057000851218     C                   SETON                                        91        *
057100851218     C*--------- POSIZIONAMENTO PAGINA :                   *
057200851218     C                   Z-ADD     NRELV         CURSOR                         *
057300851218     C     CURSOR        DIV       12            CURSOR                         *
057400851218     C                   MVR                     RESTO             5 052        *
057500851218     C     CURSOR        MULT      12            CURSOR                         *
057600851218     C   52CURSOR        ADD       1             CURSOR                         *
057700851218     C                   UPDATE    SFL1                                         *
057800990204     c                   if        mofdor > 0
057900990204     C                   move      MOfDOR        DATAIso                        *
058000990204     C                   move      dataiso       dataeur                        *
058100990204     C                   move      DATAeur       dormof                         *
058200990204     c                   end
058300990204     c                   if        mofdbl > 0
058400990204     C                   move      MOfDbl        DATAIso                        *
058500990204     C                   move      dataiso       dataeur                        *
058600990204     C                   move      DATAeur       dblmof                         *
058700990204     c                   end
058800990204     c                   if        mofdft > 0
058900990204     C                   move      MOfDft        DATAIso                        *
059000990204     C                   move      dataiso       dataeur                        *
059100990204     C                   move      DATAeur       dftmof                         *
059200990204     c                   end
059300990204     c                   if        mofdpt > 0
059400990204     C                   move      MOfDpt        DATAIso                        *
059500990204     C                   move      dataiso       dataeur                        *
059600990204     C                   move      DATAeur       dptmof                         *
059700990204     c                   end
059800990204     c                   if        mofdcn > 0
059900990204     C                   move      MOfDcn        DATAIso                        *
060000990204     C                   move      dataiso       dataeur                        *
060100990204     C                   move      DATAeur       dcnmof                         *
060200990204     c                   end
060300851218     C                   GOTO      EMISF1                                       *
060400851218     C                   END                                                    *
060500990913     C                   endif                                                  *
060600860401     C                   SETOFF                                       91        *
060700851218     C                   GOTO      EMISF1                                       *
060800851218     C*--- FINE LAVORO ------------------------------------*
060900851218     C     FINE          TAG                                                    *
061000851218     C                   SETON                                            LR    *
061100851218     C*--- ROUTINE CARICAMENTO SUBFILE DA ARCHIVIO --------*
061200851218     C***------------------------------------------------***
061300851218     CSR   RUTARC        BEGSR                                                  *
061400860401     C     RICARC        TAG                                                    *
061500990204     C   11GPM170        READE     tmsof23L                               99    *
061600990204     C   12STM170        READE     tmsof24L                               99    *
061700990204     C   17FOR170        READE     tmsof26L                               99    *
061800990204     C   02KART1         READE     tmsof25L                               99    *
061900851218     C   99              GOTO      ENDARC                                       *
062000860402     C                   MOVEL     MOFDMT        DESAR1                         *
062100860401     C*---------- CONTROLLO MOVIMENTI ---------------------*
062200860401     C                   EXSR      CTLMOV                                       *
062300860401     C   94              GOTO      RICARC                                       *
062400880915     C*---------- CONTROLLO DATA BOLLA --------------------*
062500880915     C     MOFDBL        CABLT     DAL170        RICARC                         *
062600880916     C     MOFDBL        CABGT     AL170         ENDARC                         *
062700860402     C                   Z-ADD     0             PRZMOF                         *
062800860402     C*---------- RIGHE PRODOTTO --------------------------*
062900860401     C   97              DO                                                     *
063000860402     C   01              EXSR      CTLART                                       *
063100860402     C   01
063200860402     CAN 94              GOTO      RICARC                                       *
063300880926     C                   Z-ADD     MOFPRZ        PRENET           11 2          *
063400880926     C     PRENET        MULT      MOFSC1        SCONTO           11 2          *
063500880926     C     SCONTO        DIV       100           SCONTO                         *
063600880926     C     PRENET        SUB       SCONTO        PRENET                         *
063700880926     C     PRENET        MULT      MOFSC2        SCONTO                         *
063800880926     C     SCONTO        DIV       100           SCONTO                         *
063900880926     C     PRENET        SUB       SCONTO        PRENET                         *
064000880926     C     PRENET        MULT      MOFSC3        SCONTO
064100880926     C     SCONTO        DIV       100           SCONTO                         *
064200880926     C     PRENET        SUB       SCONTO        PRENET                         *
064300880926     C** SOTTRAE SCONTO 1,2,3
064400880926     C     PRENET        MULT(H)   MOFQTA        PRZVAL           15 2          *
064500880915     C                   Z-ADD     PRZVAL        VALORE           15 2          *
064600860401     C     TOTVAL        ADD       VALORE        TOTVAL                         *
064700860402     C   02TOTQTA        ADD       MOFQTA        TOTQTA                         *
064800860401     C                   END                                                    *
064900860401     C*---------- ALTRE RIGHE -----------------------------*
065000860402     C   96              DO                                                     *
065100860402     C     MOFTRG        IFEQ      'D'                                          *DEBITO  FORNIT.
065200860401     C     MOFPRZ        MULT      -1            MOFPRZ                         *
065300860401     C                   END                                                    *
065400860402     C     MOFTRG        IFEQ      'C'                                          *CREDITO FORNIT.
065500880915     C                   Z-ADD     MOFPRZ        PRZVAL                         *
065600860402     C     MOFQTA        MULT      PRZVAL        VALORE                         *
065700860402     C     TOTVAL        ADD       VALORE        TOTVAL                         *
065800860402     C                   END                                                    *
065900860402     C                   MOVE      *BLANKS       MOFMAT                         *
066000860402     C                   MOVE      *BLANKS       MOFUMQ                         *
066100860402     C                   END                                                    *
066200851218     C*---------- IMPOSTO DATI X SUBFILE ------------------*
066300851218     C                   MOVE      *BLANKS       VISUAL                         *
066400851218     C     REC1          ADD       1             REC1                 81        *
066500860402     C                   Z-ADD     REC1          NRELV                          *
066600881025     C                   Z-ADD     PRZVAL        PRZMOF                         *
066700851218     C                   WRITE     SFL1                                         *
066800851218     C     REC1          COMP      9999                                   90    *
066900860401     C   90              MOVEL     ERR(12)       ERRORE                         *
067000851218     C   90              GOTO      START                                        *
067100860401     C                   GOTO      RICARC                                       *
067200851218     CSR   ENDARC        ENDSR                                                  *
067300860401     C*------- ROUTINE SELEZIONE DATI DAI MOVIMENTI -------*
067400860401     C***------------------------------------------------***
067500860401     CSR   CTLMOV        BEGSR                                                  *
067600860402     C                   SETOFF                                       9495      *
067700860402     C                   SETOFF                                         9697    *
067800860401     C*---------- CONTROLLO TIPO ORDINE -------------------*
067900860401     C     TIP170        IFNE      '99'                                         *
068000860401     C     MOFTOR        CABNE     TIP170        ENDMOV               9494      *
068100860401     C                   END                                                    *
068200860401     C*---------- CONTROLLO TIPO RIGA ---------------------*
068300860402     C     MOFTRG        LOOKUP    TRG                                    95    *
068400860402     C  N951             CABEQ     1             ENDMOV                   94    *
068500860401     C     MOFTRG        COMP      '1'                                    97    *
068600860401     C  N97MOFTRG        COMP      '2'                                    97    *
068700860401     C  N97MOFTRG        COMP      '3'                                    97    *
068800860401     C     MOFTRG        COMP      'C'                                    96    *
068900860401     C  N96MOFTRG        COMP      'D'                                    96    *
069000860401     C  N96MOFTRG        COMP      'I'                                    96    *
069100860401     C  N96MOFTRG        COMP      'S'                                    96    *
069200860401     C  N96
069300860401     CANN971             CABEQ     1             ENDMOV                   94    *
069400860401     C*---------- CONTROLLO FASE --------------------------*
069500860401     C     FAS170        IFNE      '99'                                         *
069600860401     C     MOFFAS        CABNE     FAS170        ENDMOV               9494      *
069700860401     C                   END                                                    *
069800860401     C     MOFFAS        COMP      'BB'                               9494      *
069900860401     C   94MOFFAS        COMP      'FA'                               9494      *
070000860402     C   94              GOTO      ENDMOV                                       *
070100860401     C*---------- GRUPPO MERCEOLOGICO ---------------------*
070200860401     C     GPM170        IFNE      '99'                                         *
070300860402     C     MOFGPM        CABNE     GPM170        ENDMOV               9494      *
070400851218     C                   END                                                    *
070500860401     C*---------- SETTORE MERCEOLOGICO---------------------*
070600860401     C     STM170        IFNE      '99'                                         *
070700860402     C     MOFSTM        CABNE     STM170        ENDMOV               9494      *
070800851218     C                   END                                                    *
070900860402     C*---------- FORNITORE -------------------------------*
071000990204     C     FOR170        IFNE      '99999999'                                   *
071100860402     C     MOFFOR        CABNE     FOR170        ENDMOV               9494      *
071200860402     C                   END                                                    *
071300990204     C     MOFFOR        CHAIN     tmfop01L                           51        *
071400860402     C   51              MOVEL     *ALL'*'       DESFO1                         *
071500860402     C   51              MOVE      MOFFOR        DESFO1                         *
071600860402     C  N51              MOVEL     FOPRSI        DESFO1                         *
071700990204     C                   MOVE      MOFFOR        OLDFOR            8            *
071800860402     C*----------                                          *
071900860402     CSR   ENDMOV        ENDSR                                                  *
072000851218     C*------- ROUTINE SELEZIONE DATI X ARTICOLO ----------*
072100851218     C***------------------------------------------------***
072200851218     CSR   CTLART        BEGSR                                                  *
072300851218     C                   SETOFF                                           94    *
072400860401     C   01              DO                                                     *
072500860401     C     ARTWW1        IFNE      ARTWW2                                       *
072600860401     C                   MOVE      MOFMAT        AMPMAT                         *ARTICOLO
072700990204     C     KEYAMP        CHAIN     tmamp01L                           51        *
072800860402     C   51              MOVE      *ALL'*'       DESAR1                         *
072900860402     C   51              MOVE      ARTWW1        DESAR1                         *
073000860402     C   51              GOTO      ENDART                                       *
073100871006     C                   MOVE      ARTWW1        ARTWW2           12            *
073200860401     C                   END                                                    *
073300860401     C                   END                                                    *
073400851218     C*---------- ARTICOLO --------------------------------*
073500871006     C     ART170        IFNE      NOVE                                         *
073600860401     C     AMPMAT        CABNE     ART170        ENDART               9494      *
073700851218     C                   END                                                    *
073800851218     C*---------- CLASSE ----------------------------------*
073900860401     C     CLM170        IFNE      '999'                                        *
074000860401     C     AMPCLM        CABNE     CLM170        ENDART               9494      *
074100851218     C                   END                                                    *
074200860401     C*---------- S/CLASSE --------------------------------*
074300860401     C     SCM170        IFNE      '99999'                                      *
074400860401     C     AMPSCM        CABNE     SCM170        ENDART               9494      *
074500851218     C                   END                                                    *
074600851218     CSR   ENDART        ENDSR                                                  *
074700990204     C*----------------------------------------------------*
074800990204     C* Reperimento dati società
074900990204     C*----------------------------------------------------*
075000990204     C     REPSOC        BEGSR
075100990204     C*
075200990204     C                   CALLB     'XSOC'
075300990204     C                   PARM                    TIPXSC            6
075400990204     C                   PARM                    SOCXSC            3
075500990204     C                   PARM                    CDSXSC            9 0
075600990204     C                   PARM                    MODXSC            3
075700990204     C                   PARM      *blanks       RTNXSC            1
075800990204     C                   PARM                    XSOCDS
075900990204     C                   PARM                    KPJBA
076000990204     C*
076100990204     C                   ENDSR
076200851218     O*--------------------------------------------------------------*
076300851218     O*     TESTATA                                                  *
076400851218     O*--------------------------------------------------------------*
076500860401     OOFPRTSTD  E            TESTA            01
076600851218     O                       SK1                132
076700851218     O                       DESDIT              20
076800990204     O                       woggi              122 '  /  /    '
076900851218     O                       PAGE          Z    132
077000851218     O          E            TESTA       1
077100851218     O                       SK2                132
077200990204     O                       DAL                 93 '  /  /    '
077300990204     O                       AL                 108 '  /  /    '
077400851218     O          E            TESTA       1
077500851218     O                       SK3                132
077600851218     O                       KNMUS              132
077700860402     O          E            TESTA       2
077800860402     O                                           99 'Selez.:'
077900860402     O                                          110 'Gruppo..xx'
078000860402     O                       GPM170             110
078100860402     O                                          130 'Settore....xx'
078200860402     O                       STM170             130
078300851218     O          E            TESTA       1
078400851218     O                       DESORD              36
078500871006     O                       CODORD              49
078600871006     O                       DES170              82
078700860402     O                                          110 'Classe.xxx'
078800860402     O                       CLM170             110
078900860402     O                                          130 'S/Classe.xxxxx'
079000860402     O                       SCM170             130
079100851218     O          E            TESTA       0  0
079200871006     O                       CODORD              49
079300871006     O                       DES170              82
079400851218     O          E            TESTA       1
079500860402     O                                          110 'Tipo..XX'
079600860402     O                       TIP170             110
079700860402     O                                          118 'Fase.xx'
079800860402     O                       FAS170             118
079900860402     O                                          123 'Tr.'
080000860402     O                       TRG                130
080100851218     O          E            TESTA       1
080200851218     O                       SK4                132
080300851218     O*--------------------------------------------------------------*
080400851218     O*     DETTAGLIO                                                *
080500851218     O*--------------------------------------------------------------*
080600851218     O          E            DETAIL      1
080700860402     O                       PAGE1              132
080800851218     O                       SK5                132
080900860401     O                       MOFNOR        Z      5
081000860402     O                       MOFTOR               8
081100990204     O                       DORMOF        Z     17
081200990204     O                       MOFNBL        Z     25
081300990204     O                       DBLMOF        Z     34
081400990311     O                       MOFNPT        Z     44
081500990311     O                       DPTMOF        Z     55
081600990204     O                       MOFMAT              68
081700871006     O                       DESAR1              86
081800871006     O                       MOFFAS              89
081900871006     O                       MOFUMQ              93
082000871006     O                       MOFQTA             105 '     0 ,  -'
082100871006     O                       MOFTRG             106
082200871006     O                       MOFVAL             111
082300990204     O                       MOFPRZ             128 '       0 ,     -'
082400990204     O                       MOFCPG             132
082500851218     O          E            DETAIL      1
082600851218     O                       PAGE1              132
082700851218     O                       SK5                132
082800990204     O                       MOFNFT        Z     25
082900990204     O                       DFTMOF        Z     34
083000860402     O                       DESFO1              82
083100990311     O                  84                      118 'cmb.'
083200990311     O                  84   MOFCMB             132 '     0 ,      '
083300851218     O/EJECT
083400851218     O*--------------------------------------------------------------*
083500851218     O*     TOTALE                                                   *
083600851218     O*--------------------------------------------------------------*
083700851218     O          E            TOTALE      1
083800851218     O                       SK6                132
083900860402     O                     02                   103 '.Quantità.'
084000860402     O                                          121 '.Valore.'
084100851218     O          E            TOTALE      1
084200860402     O                                           86 'Totale........:'
084300860402     O                     02TOTQTA             104 ' .   .   . 0 ,  -'
084400990204     O                       TOTVAL             128 ' .   .   . 0 ,     -'
084500851218     O          E            TOTALE      0  0
084600860402     O                     02TOTQTA             104 ' .   .   . 0 ,  -'
084700990204     O                       TOTVAL             128 ' .   .   . 0 ,     -'
084800851218     O          E            TOTALE      1
084900851218     O                       SK6                132
085000860402**   TES
085100860401Fornitore.....:      1
085200860401Settore Merc..:      2
085300860401Gruppo..Merc..:      3
085400860401Prodotto......:      4
085500860401S/Classe.Merc.:      5
085600860401Classe...Merc.:      6
085700851218**   ERR
085800851218*- Ins. Param. Ordinamento  -*   ERR,1
085900860401*- Cod. FORNITORE    ERRATO -*   ERR,2
086000860401*- Cod. SETTORE MERC.ERRATO -*   ERR,3
086100860401*- Cod. GRUPPO  MERC.ERRATO -*   ERR,4
086200860401*- Cod. PRODOTTO     ERRATO -*   ERR,5
086300860401*- Cod. S/CLASSE     ERRATO -*   ERR,6
086400860401*- Cod. Classe       ERRATO -*   ERR,7
086500860401*- Date Limite       ERRATE -*   ERR,8
086600860401*- Tipo Ordine       ERRATO -*   ERR,9
086700860401*- Eseguire Scelta ORDINAM. -*   ERR,10
086800860401*- Non Trovati Movimenti !! -*   ERR,11
086900860401*- Righe Ordini oltre 9999 !-*   ERR,12
087000851218**   SK1
087100851218*-DESDIT-----------* ***------------------------------------------SK01
087200851218--------------------------------------*** il XX/XX/XX     Pag. XXXSK01
087300851218**   SK2
087400880915                     ** Gest.Ord.FORNIT. : Stampa ORDINI FORNITORISK02
087500990317 X DATA BOLLA dal xx.xx.xx   al xx.xx.xx        PGM.:   TII402R   SK02
087600851218**   SK3
087700851218                     ***------------------------------------------SK03
087800851218--------------------------------------***       Utente  XXXXXXXXXXSK03
087900851218**   SK4
088000990204*------Ordine----*-Bol/Fat For.-*--Ns.Fattura----------*---Cod.---SK04
088100990204--Prodotto.Fornitore-:Fa-Um.--Quantità-Tr*Val.----Prezzo-------PagSK04
088200851218**   SK5
088300990204                 :              :                      :          SK05
088400990204                     :                   :                        SK05
088500851218**   SK6
088600860402*--------------*--------------*----------------------*------------SK06
088700860402-------------------*-------------------*-------------------------*SK06

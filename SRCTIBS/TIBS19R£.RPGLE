000100011206?     * TIBS19R *-----------------------------------------------------*
000200011206?     *       PULIZIA NUMERATORI DI PO CHE NON ESISTONO PIU'          *
000300011206?     *---------------------------------------------------------------*
000400011206     H DECEDIT('0,') DATEDIT(*DMY.)
000500931222     FAZORG01L  IF   E           K DISK
000600011206     FAZNUM02L  UF   E           K DISK
000700000113     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000800011206?     *---------------------------------------------------------------*
000900931222     D KPJBA         E DS
001000011207     D*
001100011207     D UT§DSE0F      E DS
001200011206     D*
001300011206     D* DS PER TIBS02R - Gestione ricerca / controllo tabelle
001400011206     D TIBS02DS      E DS                  INZ
001500011206     D*
001600011206     D DCNU          E DS                  INZ
001700011206     D*
001800000113     D PARAM           DS
001900000113     D  PARDEL                 1      1
002000011206     D*
002100011206     D $DEL            S              1    INZ(*OFF)
002200011206     D CONT            S              4  0 INZ
002300011206     D DESCNU          S             20    INZ
002400020114     D SAVcnu          s                   like(NUMcnu) inz
002500011206?     *****************************************************************
002600011206?     * RIEPILOGO INDICATORI
002700011206?     *****************************************************************
002800020114      *  01 - MANCA TABELLA "CNU"
002900020114      *  02 - P.O.1 INESISTENTE
003000020114      *  03 - P.O.2 INESISTENTE
003100011206?     *****************************************************************
003200931222     C     *ENTRY        PLIST
003300931222     C                   PARM                    KPJBA
003400000113     C                   MOVEL     KPJBU         PARAM
003500011207     C                   movel     *on           *inOF
003600011206     C*
003700011206     C     *LOVAL        SETLL     AZNUM02L
003800011206     C                   READ      AZNUM02L
003900000113     C*
004000011206DO  1C                   DOW       not %eof(AZNUM02L)
004100011206     C*
004200011206     C                   movel     *off          $DEL
004300011206     C                   clear                   DEScnu
004400011206     C                   setoff                                       010203
004500000112     C* CERCO CODICE NUMERATORE IN TABELLA: SE NON ESISTE ANNULLO
004600020114     C                   if        NUMcnu <> SAVcnu
004700020114     C                   z-add     NUMcnu        SAVcnu
004800011206     C                   reset                   TIBS02DS
004900011206     C                   movel     'C'           t02mod
005000011206     C                   movel     knsif         t02sif
005100011206     C                   movel     'CNU'         t02cod
005200011206     C                   movel(p)  NUMcnu        t02ke1
005300011206     C                   call      'TIBS02R'
005400011206     C                   parm                    KPJBA
005500011206     C                   parm                    TIBS02DS
005600020114     C                   endif
005700011206     C*
005800011206IF  2C                   IF        T02err <> *blanks
005900011206     C*
006000011206     C*- DELETO
006100011206IF  3C                   if        PARDEL = 'S'
006200011206     C                   DELETE    AZNUM000
006300011206     C                   else
006400011207     C                   exsr      TestOF
006500011206     C                   movel     *on           *in01
006600011206     C                   except    STAMP1
006700011206     C                   add       1             CONT
006800011206     C                   movel     *off          *in01
006900011206E   3C                   endif
007000011206     C*
007100011206X   2C                   else
007200011206     C*
007300011206     C                   movel     T02uni        DCNU
007400011206     C                   movel     §cnuDES       DESCNU
007500000112     C*
007600011206     C* SE NUMPO1 E' UNA FILIALE
007700011206IF  3C                   IF            §cnuP1N <> 'S'
007800011206     C                             and NUMpo1  >  0
007900011206     C     NUMpo1        chain     AZORG01L
008000011206IF  4C                   if        not %found(AZORG01L)
008100011206     C                             or  ORGfva <> *blanks
008200011206     C                             or (ORGfag <> 'A'
008300011206     C                             and ORGfag <> 'F')
008400011206     C*- DELETO
008500011206IF  5C                   if        PARDEL = 'S'
008600011206     C                   DELETE    AZNUM000
008700011206     C                   else
008800011207     C                   exsr      TestOF
008900011206     C                   movel     *on           *in02
009000011206     C                   except    STAMP1
009100011206     C                   add       1             cont
009200011206     C                   movel     *off          *in02
009300011206E   5C                   endif
009400011206     C                   movel     *on           $DEL
009500011206E   4C                   endif
009600011206E   3C                   ENDIF
009700000113     C*
009800011206     C* SE NUMPO2 E' PIENO CONTROLLO SE E' P.O. ESISTENTE
009900011206IF  3C                   IF            NUMpo2  > 0
010000011206     C                             and $DEL    = *off
010100000113     C*
010200011206IF  4C                   if            §cnuPO2 = 'TUT'
010300011206     C                             or  §cnuPO2 = 'FIL'
010400011206     C     NUMpo2        chain     AZORG01L
010500011206IF  5C                   if        not %found(AZORG01L)
010600011206     C                             or  ORGfva <> *blanks
010700011206     C                             or (ORGfag <> 'A'
010800011206     C                             and ORGfag <> 'F')
010900011206     C*- DELETO
011000011206IF  6C                   if        PARDEL = 'S'
011100011206     C                   DELETE    AZNUM000
011200011206X   6C                   else
011300011207     C                   exsr      TestOF
011400011206     C                   movel     *on           *in03
011500011206     C                   except    STAMP1
011600011206     C                   add       1             CONT
011700011206     C                   movel     *off          *in03
011800011206E   6C                   endif
011900011206E   5C                   endif
012000011206E   4C                   endif
012100011206E   3C                   ENDIF
012200011206     C*
012300011206E   2C                   ENDIF
012400931222     C*
012500011206     C                   READ      AZNUM02L
012600011206E   1C                   ENDDO
012700011206     C*
012800011206     C                   if        CONT > 0
012900011207     C                   except    TOTAL1
013000011206     C                   endif
013100931222     C*
013200011206     C                   movel     *on           *INLR
013300011207     C*
013400011207      *****************************************************************
013500011207      * TEST OVERFLOW
013600011207      *****************************************************************
013700011207     C     TESTOF        BEGSR
013800011207     C                   if        *inOF
013900011207     C                   time                    wTIME             6 0
014000011207     C                   except    TESTA1
014100011207     C                   movel     *off          *inOF
014200011207     C                   endif
014300011207     C                   ENDSR
014400011207     C*
014500011207      *****************************************************************
014600011207      * OPERAZIONI INIZIALI
014700011207      *****************************************************************
014800011207     C     *INZSR        BEGSR
014900011207     C                   EVAL      CODUT = 1
015000011207     C                   CALL      'X§PARUT'
015100011207     C                   PARM                    UT§DSE0F
015200011207     C                   ENDSR
015300011206?     *
015400011207?    OQSYSPRT   E            TESTA1            2
015500011207     O                       RAGUT
015600011207     O                                         +  5 ' PULIZIA NUMERATORI '
015700011207     O                       *DATE             + 30 '  /  /    '
015800011207     O                                         +  2 'PAG.'
015900011207     O                       PAGE          Z   +  0
016000011207     O          E            TESTA1      1
016100011207     O                       KNSIF
016200011207     O                       KNMUS             +  2
016300011207     O                                           45 ' '
016400011207     O                                         +  5 ' ------------------ '
016500011207     O                       wTIME             + 30 '  :  :  '
016600011207     O                                         +  4 'TIBS19R'
016700011207     O          E            TESTA1      2
016800011207     O                                              '    '
016900011207     O                                         +  1 '                        '
017000011207     O                                         +  1 '   '
017100011207     O                                         +  1 '   '
017200011207     O                                         +  1 '  Ultimo Numero'
017300011207     O                                         +  1 '               '
017400011207     O                                         +  1 '               '
017500011207     O                                         +  1 '   Data   '
017600011207     O          E            TESTA1      1
017700020206     O                                              'Società'
017800020206     O                                         +  1 'Anno'
017900011207     O                                         +  1 'Numeratore              '
018000011207     O                                         +  1 'PO1'
018100011207     O                                         +  1 'PO2'
018200011207     O                                         +  1 '      Prelevato'
018300011207     O                                         +  1 '   Inizio Range'
018400011207     O                                         +  1 '     Fine Range'
018500011207     O                                         +  1 ' Creazione'
018600011207     O          E            TESTA1      1  1
018700020206     O                                              '-------'
018800020206     O                                         +  1 '----'
018900011207     O                                         +  1 '------------------------'
019000011207     O                                         +  1 '---'
019100011207     O                                         +  1 '---'
019200011207     O                                         +  1 '---------------'
019300011207     O                                         +  1 '---------------'
019400011207     O                                         +  1 '---------------'
019500011207     O                                         +  1 '----------'
019600011207     O          E            STAMP1      1
019700020206     O                       NUMsoc               5
019800020206     O                       NUMaaa            +  3
019900011206     O                       NUMcnu            +  1
020000011206     O                       DEScnu            +  1
020100011206     O                       NUMpo1        Z   +  1
020200011206     O                       NUMpo2        Z   +  1
020300011206     O                       NUMunp        Z   +  1
020400011206     O                       NUMrin        Z   +  1
020500000113     O                                         +  0 '/'
020600011206     O                       NUMrfi        Z   +  0
020700011206     O                       NUMdac            +  1 '    /  /  '
020800011206     O               01                         132 'MANCA TABELLA "CNU"    '
020900020114     O               02                         132 'P.O.1 INESISTENTE      '
021000020114     O               03                         132 'P.O.2 INESISTENTE      '
021100011207     O          E            TOTAL1      2
021200020206     O                                         +  8 'TOTALE RECORD DA CANCELLAR-
021300011207     O                                              E:'
021400011207     O                       CONT          2   +  1

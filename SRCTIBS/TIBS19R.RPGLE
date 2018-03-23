000100011206?     * TIBS19R *-----------------------------------------------------*
000200011206?     *       PULIZIA NUMERATORI DI PO CHE NON ESISTONO PIU'          *
000300011206?     *---------------------------------------------------------------*
000400011206     H DECEDIT('0,') DATEDIT(*DMY.)
000500931222     FAZORG01L  IF   E           K DISK
000600011206     FAZNUM02L  UF   E           K DISK
000700000113     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000800011206?     *---------------------------------------------------------------*
000900931222     D KPJBA         E DS
001000011207     D UT§DSE0F      E DS
001100011206     D* DS PER TIBS02R - Gestione ricerca / controllo tabelle
001200011206     D TIBS02DS      E DS                  INZ
001300011206     D DCNU          E DS                  INZ
001400041026      *
001500000113     D PARAM           DS
001600000113     D  PARDEL                 1      1
001700011206     D*
001800011206     D $DEL            S              1    INZ(*OFF)
001900011206     D CONT            S              4  0 INZ
002000011206     D DESCNU          S             20    INZ
002100020114     D SAVcnu          s                   like(NUMcnu) inz
002200011206?     *****************************************************************
002300011206?     * RIEPILOGO INDICATORI
002400011206?     *****************************************************************
002500020114      *  01 - MANCA TABELLA "CNU"
002600020114      *  02 - P.O.1 INESISTENTE
002700020114      *  03 - P.O.2 INESISTENTE
002800011206?     *****************************************************************
002900931222     C     *ENTRY        PLIST
003000931222     C                   PARM                    KPJBA
003100000113     C                   MOVEL     KPJBU         PARAM
003200011207     C                   movel     *on           *inOF
003300011206     C*
003400011206     C     *LOVAL        SETLL     AZNUM02L
003500011206     C                   READ      AZNUM02L
003600000113     C*
003700011206DO  1C                   DOW       not %eof(AZNUM02L)
003800011206     C*
003900011206     C                   movel     *off          $DEL
004000011206     C                   clear                   DEScnu
004100011206     C                   setoff                                       010203
004200000112     C* CERCO CODICE NUMERATORE IN TABELLA: SE NON ESISTE ANNULLO
004300020114     C                   if        NUMcnu <> SAVcnu
004400020114     C                   z-add     NUMcnu        SAVcnu
004500011206     C                   reset                   TIBS02DS
004600011206     C                   movel     'C'           t02mod
004700011206     C                   movel     knsif         t02sif
004800011206     C                   movel     'CNU'         t02cod
004900011206     C                   movel(p)  NUMcnu        t02ke1
005000011206     C                   call      'TIBS02R'
005100011206     C                   parm                    KPJBA
005200011206     C                   parm                    TIBS02DS
005300020114     C                   endif
005400011206     C*
005500011206IF  2C                   IF        T02err <> *blanks
005600011206     C*
005700011206     C*- DELETO
005800011206IF  3C                   if        PARDEL = 'S'
005900011206     C                   DELETE    AZNUM000
006000011206     C                   else
006100011207     C                   exsr      TestOF
006200011206     C                   movel     *on           *in01
006300011206     C                   except    STAMP1
006400011206     C                   add       1             CONT
006500011206     C                   movel     *off          *in01
006600011206E   3C                   endif
006700011206     C*
006800011206X   2C                   else
006900011206     C*
007000011206     C                   movel     T02uni        DCNU
007100011206     C                   movel     §cnuDES       DESCNU
007200000112     C*
007300011206     C* SE NUMPO1 E' UNA FILIALE
007400011206IF  3C                   IF            §cnuP1N <> 'S'
007500011206     C                             and NUMpo1  >  0
007600011206     C     NUMpo1        chain     AZORG01L
007700011206IF  4C                   if        not %found(AZORG01L)
007800011206     C                             or  ORGfva <> *blanks
007900011206     C                             or (ORGfag <> 'A'
008000041026     C                             and ORGfag <> 'F' and ORGFIl <> 046)
008100011206     C*- DELETO
008200011206IF  5C                   if        PARDEL = 'S'
008300011206     C                   DELETE    AZNUM000
008400011206     C                   else
008500011207     C                   exsr      TestOF
008600011206     C                   movel     *on           *in02
008700011206     C                   except    STAMP1
008800011206     C                   add       1             cont
008900011206     C                   movel     *off          *in02
009000011206E   5C                   endif
009100011206     C                   movel     *on           $DEL
009200011206E   4C                   endif
009300011206E   3C                   ENDIF
009400000113     C*
009500011206     C* SE NUMPO2 E' PIENO CONTROLLO SE E' P.O. ESISTENTE
009600011206IF  3C                   IF            NUMpo2  > 0
009700011206     C                             and $DEL    = *off
009800000113     C*
009900011206IF  4C                   if            §cnuPO2 = 'TUT'
010000011206     C                             or  §cnuPO2 = 'FIL'
010100011206     C     NUMpo2        chain     AZORG01L
010200011206IF  5C                   if        not %found(AZORG01L)
010300011206     C                             or  ORGfva <> *blanks
010400011206     C                             or (ORGfag <> 'A'
010500041026     C                             and ORGfag <> 'F' and ORGFIl <> 046)
010600011206     C*- DELETO
010700011206IF  6C                   if        PARDEL = 'S'
010800011206     C                   DELETE    AZNUM000
010900011206X   6C                   else
011000011207     C                   exsr      TestOF
011100011206     C                   movel     *on           *in03
011200011206     C                   except    STAMP1
011300011206     C                   add       1             CONT
011400011206     C                   movel     *off          *in03
011500011206E   6C                   endif
011600011206E   5C                   endif
011700011206E   4C                   endif
011800011206E   3C                   ENDIF
011900011206     C*
012000011206E   2C                   ENDIF
012100931222     C*
012200011206     C                   READ      AZNUM02L
012300011206E   1C                   ENDDO
012400011206     C*
012500011206     C                   if        CONT > 0
012600011207     C                   except    TOTAL1
012700011206     C                   endif
012800931222     C*
012900011206     C                   movel     *on           *INLR
013000011207     C*
013100011207      *****************************************************************
013200011207      * TEST OVERFLOW
013300011207      *****************************************************************
013400011207     C     TESTOF        BEGSR
013500011207     C                   if        *inOF
013600011207     C                   time                    wTIME             6 0
013700011207     C                   except    TESTA1
013800011207     C                   movel     *off          *inOF
013900011207     C                   endif
014000011207     C                   ENDSR
014100011207     C*
014200011207      *****************************************************************
014300011207      * OPERAZIONI INIZIALI
014400011207      *****************************************************************
014500011207     C     *INZSR        BEGSR
014600011207     C                   EVAL      CODUT = 1
014700011207     C                   CALL      'X§PARUT'
014800011207     C                   PARM                    UT§DSE0F
014900011207     C                   ENDSR
015000011206?     *
015100011207?    OQSYSPRT   E            TESTA1            2
015200011207     O                       RAGUT
015300011207     O                                         +  5 ' PULIZIA NUMERATORI '
015400011207     O                       *DATE             + 30 '  /  /    '
015500011207     O                                         +  2 'PAG.'
015600011207     O                       PAGE          Z   +  0
015700011207     O          E            TESTA1      1
015800011207     O                       KNSIF
015900011207     O                       KNMUS             +  2
016000011207     O                                           45 ' '
016100011207     O                                         +  5 ' ------------------ '
016200011207     O                       wTIME             + 30 '  :  :  '
016300011207     O                                         +  4 'TIBS19R'
016400011207     O          E            TESTA1      2
016500011207     O                                              '    '
016600011207     O                                         +  1 '                        '
016700011207     O                                         +  1 '   '
016800011207     O                                         +  1 '   '
016900011207     O                                         +  1 '  Ultimo Numero'
017000011207     O                                         +  1 '               '
017100011207     O                                         +  1 '               '
017200011207     O                                         +  1 '   Data   '
017300011207     O          E            TESTA1      1
017400020206     O                                              'Società'
017500020206     O                                         +  1 'Anno'
017600011207     O                                         +  1 'Numeratore              '
017700011207     O                                         +  1 'PO1'
017800011207     O                                         +  1 'PO2'
017900011207     O                                         +  1 '      Prelevato'
018000011207     O                                         +  1 '   Inizio Range'
018100011207     O                                         +  1 '     Fine Range'
018200011207     O                                         +  1 ' Creazione'
018300011207     O          E            TESTA1      1  1
018400020206     O                                              '-------'
018500020206     O                                         +  1 '----'
018600011207     O                                         +  1 '------------------------'
018700011207     O                                         +  1 '---'
018800011207     O                                         +  1 '---'
018900011207     O                                         +  1 '---------------'
019000011207     O                                         +  1 '---------------'
019100011207     O                                         +  1 '---------------'
019200011207     O                                         +  1 '----------'
019300011207     O          E            STAMP1      1
019400020206     O                       NUMsoc               5
019500020206     O                       NUMaaa            +  3
019600011206     O                       NUMcnu            +  1
019700011206     O                       DEScnu            +  1
019800011206     O                       NUMpo1        Z   +  1
019900011206     O                       NUMpo2        Z   +  1
020000011206     O                       NUMunp        Z   +  1
020100011206     O                       NUMrin        Z   +  1
020200000113     O                                         +  0 '/'
020300011206     O                       NUMrfi        Z   +  0
020400011206     O                       NUMdac            +  1 '    /  /  '
020500011206     O               01                         132 'MANCA TABELLA "CNU"    '
020600020114     O               02                         132 'P.O.1 INESISTENTE      '
020700020114     O               03                         132 'P.O.2 INESISTENTE      '
020800011207     O          E            TOTAL1      2
020900020206     O                                         +  8 'TOTALE RECORD DA CANCELLAR-
021000011207     O                                              E:'
021100011207     O                       CONT          2   +  1

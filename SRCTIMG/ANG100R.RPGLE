000100000000     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200000000     F* ANG100R * ANA38*-----------------------------------*
000300000000     F*----------------*                                   *
000400000000     F*          MANUTENZIONE TABELLE RICCHETTI            *
000500000000     F*          ------------------------------   AZ. G100 *
000600000000     F*          FILE : ANTAB00F-ANG100D                   *
000700000000     F*          FMT. : ANG1001 ANG10CF1 ANG10SC1          *
000800000000     F*                 ANG1002 ANG10CF2 ANG10SC2          *
000900000000     F*          PGM.:  ANG1152R - TABELLA 152             *
001000850708     F*          PGM.:  ANG1153R - TABELLA 153             *
001100850708     F*          PGM.:  ANG164R  - TABELLA 164             *
001200000000     F*----------------------------------------------------*
001300000000     FANTAB00F  UF A E           K DISK
001400000000     FANG100D   CF   E             WORKSTN
001500000000     F                                     SFILE(ANG10SF1:RRN1)
001600000000     F                                     SFILE(ANG10SF2:RRN2)
001700000000     D KPJBA         E DS
001800000000     D  EDP                    1      3
001900000000     C/EJECT
002000000000     C*----------------------------------------------------*
002100000000     C     *ENTRY        PLIST                                                  *
002200000000     C                   PARM                    KPJBA                          *
002300891123     C                   CALL      'CNC0M0R'                                    *
002400000000     C                   PARM                    KPJBA                          *
002500000000     C                   PARM                    CODDIT            3 0          *COD.DITTA
002600000000     C                   PARM                    DESDIT           20            *DESC.DITTA
002700000000     C*--------- SE DITTA INESISTENTE FINE LAVORO          *
002800000000     C     CODDIT        CABEQ     -1            FINE                           *
002900000000     C*---------                                           *
003000000000     C     KEY000        KLIST                                                  *
003100000000     C                   KFLD                    GRUTAB                         *
003200000000     C                   KFLD                    SETTE             7            *
003300000000     C*-------- CLEAR SUBFILE INDICE TABELLA --------------*
003400000000     C     INIZIO        TAG                                                    *
003500000000     C                   SETON                                        70        *
003600000000     C                   WRITE     ANG10SC1                                     *
003700000000     C                   SETOFF                                       147072    *
003800000000     C*--------                                            *
003900000000     C                   Z-ADD     0             RRN1              3 0          *
004000000000     C                   Z-ADD     1             CURSOR            3 0          *
004100000000     C                   MOVE      *ZEROS        SETTE                          *
004200000000     C                   MOVE      *ZEROS        COMODO            3            *
004300000000     C     COMODO        SETLL     ANTAB00F                                     *
004400000000     C*-------- CARICA SUBFILE INDICE TABELLA -------------*
004500000000     C     SUIND         TAG                                                    *
004600000000     C     COMODO        READE     ANTAB000                               99    *
004700000000     C  N99              DO                                                     *
004800860507     C     EDP           IFNE      'EDP'                                        *
004900860507     C     TABCOD        CABEQ     '0000201'     SUIND                          *
005000860507     C                   END                                                    *
005100000000     C     RRN1          ADD       1             RRN1                 71        *
005200000000     C   71              SETON                                        72        *
005300000000     C     RRN1          COMP      14                                 14  14    *
005400000000     C                   MOVE      TABCOD        TABELA                         *
005500000000     C                   MOVEL     TABDES        DESTAB                         *
005600000000     C   01              Z-ADD     RRN1          CURSOR                         *
005700000000     C                   WRITE     ANG10SF1                                     *
005800000000     C                   GOTO      SUIND                                        *
005900000000     C                   END                                                    *
006000000000     C                   SETOFF                                       01        *
006100000000     C*-------- EMISSIONE TESTATA INDICE ------------------*
006200000000     C                   WRITE     ANG1001                                      *
006300000000     C*-------- EMISSIONE SUBFILE INDICE ------------------*
006400000000     C     ERSFL1        TAG                                                    *
006500000000     C   91                                                                     *
006600000000     COR 92                                                                     *
006700000000     COR 93              SETON                                        90        *
006800000000     C                   EXFMT     ANG10SC1                                     *
006900000000     C                   SETOFF                                       71        *
007000000000     C                   MOVEA     *ZEROS        *IN(90)                        *
007100000000     C   KJ              GOTO      FINE                                         *
007200000000     C*-------- RICHIESTA STAMPA TABELLE :                 *
007300000000     C   KH              CALL      'ANG910R'                                    *
007400000000     C                   PARM                    KPJBA                          *
007500000000     C   KH              GOTO      INIZIO                                       *
007600000000     C*-------- CONTROLLO CREAZIONE NUOVA TABELLA ---------*
007700000000     C   KA              DO                                                     *
007800000000     C                   SETON                                        01        *
007900000000     C                   MOVE      NEWGRU        GRUNEW            3            *
008000000000     C     GRUNEW        CABEQ     *BLANKS       ERSFL1                   92    *
008100000000     C     NEWDES        CABEQ     *BLANKS       ERSFL1                   93    *
008200000000     C                   MOVE      COMODO        GRUTAB                         *
008300000000     C                   MOVE      NEWGRU        SETTE                          *
008400000000     C     KEY000        CHAIN     ANTAB00F                           99        *
008500000000     C     *IN(99)       CABEQ     '0'           ERSFL1                   91    *
008600000000     C                   MOVE      *ZEROS        TABGRU                         *
008700000000     C                   MOVE      *ZEROS        TABCOD                         *
008800000000     C                   MOVE      NEWGRU        TABCOD                         *
008900000000     C                   MOVEL     NEWDES        TABDES                         *
009000000000     C                   WRITE     ANTAB000                                     *
009100000000     C                   MOVE      *BLANKS       NEWGRU                         *
009200000000     C                   MOVE      *BLANKS       NEWDES                         *
009300000000     C                   GOTO      INIZIO                                       *
009400000000     C                   END                                                    *
009500000000     C*-------- SCELTA TABELLA DA AGGIORNARE --------------*
009600000000     C     SUREAD        TAG                                                    *
009700000000     C   72              READC     ANG10SF1                               99    *
009800000000     C   72
009900000000     CANN99SCELTA        CABEQ     *BLANKS       SUREAD                         *
010000000000     C  N72                                                                     *
010100000000     COR 99              GOTO      INIZIO                                       *
010200000000     C*-------- RICHIAMO PROGRAMMI PERSONALIZZATI :        *
010300000000     C                   MOVE      TABELA        GRUTAB                         *
010400000000     C*-------- TABELLA 152 'CAUSALI MAGAZZINO' :          *
010500000000     C     GRUTAB        IFEQ      '152'                                        *
010600000000     C     EDP           IFNE      'EDP'                                        *
010700000000     C                   MOVE      *BLANKS       PRC152                         *
010800000000     C                   CALL      'ANG1152R'                                   *
010900000000     C                   PARM                    KPJBA                          *
011000000000     C                   PARM                    PRC152            2            *
011100000000     C                   PARM                    CAU152            3            *
011200000000     C                   PARM                    DES152           25            *
011300000000     C                   PARM                    AZI152           15            *
011400000000     C                   GOTO      INIZIO                                       *
011500000000     C                   END                                                    *
011600000000     C                   END                                                    *
011700850708     C*-------- TABELLA 153 'CODIFICA MAGAZZINI' :         *
011800850708     C     GRUTAB        IFEQ      '153'                                        *
011900850708     C     EDP           IFNE      'EDP'                                        *
012000850708     C                   CALL      'ANG1153R'                                   *
012100850708     C                   PARM                    KPJBA                          *
012200850708     C                   GOTO      INIZIO                                       *
012300850708     C                   END                                                    *
012400850708     C                   END                                                    *
012500850320     C*-------- TABELLA 164 'SCHEDE TECNICHE' :            *
012600850320     C     GRUTAB        IFEQ      '164'                                        *
012700850320     C     EDP           IFNE      'EDP'                                        *
012800850320     C                   CALL      'ANG164R'                                    *
012900850320     C                   PARM                    KPJBA                          *
013000850320     C                   GOTO      INIZIO                                       *
013100850320     C                   END                                                    *
013200850320     C                   END                                                    *
013300000000     C*-------- EMISSIONE REC.TESTATA TABELLA -------------*
013400000000     C                   WRITE     ANG1002                                      *
013500000000     C*-------- CARICAMENTO SUBFILE ELEMENTI TABELLA ------*
013600000000     C     SU            TAG                                                    *
013700000000     C                   Z-ADD     1             RRN2                           *
013800000000     C                   MOVE      *BLANKS       SETTE                          *
013900000000     C     KEY000        SETLL     ANTAB00F                           72        *
014000000000     C                   MOVE      *ZEROS        SETTE                          *
014100000000     C*-------- PULIZIA RK CONTROLLO SUB. ELEM. TABELLA ---*
014200000000     C     SU1           TAG                                                    *
014300000000     C                   SETON                                        70        *
014400000000     C                   WRITE     ANG10SC2                                     *
014500000000     C                   SETOFF                                       70        *
014600000000     C   72              GOTO      RIEMPI                                       *
014700000000     C*----------------------------------------------------*
014800000000      /EJECT
014900000000     C*-------- LETTURA FILE E CARIC.SUBFILE ELEM.TAB. ----*
015000170428     C     1             DO        *hival        RRN2              3 0          *
015100000000     C     GRUTAB        READE     ANTAB00F                               72    *
015200000000     C   72              GOTO      RIEMPI                                       *
015300000000     C                   MOVEL     TABDES        DEST75           75            *
015400000000     C                   MOVE      TABCOD        TABK                           *
015500000000     C                   MOVE      TABCOD        TABKP                          *
015600000000     C                   MOVE      *BLANKS       ANN                            *
015700000000     C                   WRITE     ANG10SF2                                     *
015800000000     C                   END                                                    *
015900000000     C*-------- ABBLENCAMENTO SUB. ELEMENTI TAB. ----------*
016000000000     C     RIEMPI        TAG                                                    *
016100170428     c                   clear                   pagina
016200170428     c                   clear                   resto
016300170428     c                   clear                   aggiungi
016400170428     C     RRN2          Div       7             pagina            3 0          *
016500170428     c                   mvr                     resto             3 0
016600170428     c                   if        resto > 0
016700170428     c     7             sub       resto         aggiungi          1 0
016800170428     c                   else
016900170428     c                   z-add     7             aggiungi
017000170428     c                   endif
017100170428     c     rrn2          add       aggiungi      rrn2W             4 0
017200170428     C     rrn2          DO        rrn2w         RRN2                           *
017300170428     C                   MOVE      *ZEROS        TABK                           *
017400170428     C                   MOVE      *ZEROS        TABKP                          *
017500170428     C                   MOVE      *BLANKS       DEST75                         *
017600170428     C                   MOVE      *BLANKS       ANN                            *
017700170428     C                   WRITE     ANG10SF2                                     *
017800170428     C                   END                                                    *
017900000000     C*-------- EMISSIONE SUBFILE ELEMENTI TABELLA --------*
018000000000     C                   EXFMT     ANG10SC2                                     *
018100000000     C*--------                                            *
018200000000     C   KJ              GOTO      INIZIO                                       *
018300000000     C   KG              GOTO      SU                                           *
018400000000     C*----------------------------------------------------*
018500000000      /EJECT
018600000000     C*-------- LEGGO RECORD VARIATI ----------------------*
018700000000     C     SU2           TAG                                                    *
018800000000     C                   SETOFF                                       747576    *
018900000000     C                   SETOFF                                       77        *
019000000000     C                   MOVE      *ZEROS        SETTE                          *
019100000000     C                   READC     ANG10SF2                               74    *
019200000000     C   74
019300000000     CAN 72              Z-ADD     1             RRN2                           *
019400000000     C   74              GOTO      SU1                                          *
019500000000     C*-------- AGGIORNAMENTO :                            *
019600000000     C     TABKP         COMP      SETTE                              7575      *
019700000000     C   75              MOVE      TABKP         SETTE                          *
019800000000     C   75KEY000        CHAIN     ANTAB00F                           76        *
019900000000     C   75
020000000000     CANN76ANN           IFEQ      'A'                                          *CANCELLAZIONE
020100000000     C                   DELETE    ANTAB000                                     *
020200000000     C                   ELSE                                                   *UPDATE
020300000000     C                   MOVEL     DEST75        TABDES                         *
020400000000     C                   UPDATE    ANTAB000                                     *
020500000000     C                   END                                                    *
020600000000     C   75              GOTO      SU2                                          *
020700000000     C*-------- CONTROLLO NUOVO RECORD --------------------*
020800000000     C     TABK          CABEQ     SETTE         SU2                            *
020900000000     C                   MOVE      TABK          SETTE                          *
021000000000     C     KEY000        CHAIN     ANTAB00F                           76        *
021100000000     C  N76              GOTO      SU2                                          *
021200000000     C                   MOVEL     GRUTAB        TABGRU                         *
021300000000     C                   MOVE      TABK          TABCOD                         *
021400000000     C                   MOVE      DEST75        TABDES                         *
021500000000     C                   WRITE     ANTAB000                                     *
021600000000     C                   GOTO      SU2                                          *
021700000000     C*-------- CANCELLO SUBFILE --------------------------*
021800000000     C     FINE          TAG                                                    *
021900000000     C                   SETON                                        70        *
022000000000     C                   WRITE     ANG10SC1                                     *
022100000000     C                   SETON                                        LR        *
022200000000     C*----------------------------------------------------*
022300000000      /EJECT

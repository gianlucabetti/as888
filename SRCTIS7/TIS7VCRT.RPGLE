000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200890921     H*----------------------------------------------------*
000300890921     F*
000400170510
000500170510     D FNVACDS       E DS                  EXTNAME(FNVAC00T)
000600170510
000700170510     D*--------------------
000800170510     D* DS DI RIDEFINIZIOINE PARAMETRI IN INPUT
000900170510     D*--------------------
001000170510     D DSINPUT         DS
001100170510     D  DATADA                        8  0
001200170510     D  DATAA                         8  0
001300170510     D  CODCLI                        7  0
001400170510     D  FLGUNI                        1
001500170510     D  FLGCONS                       1
001600170510     D  FLGVAS                        1
001700170510     D  FLGOPZ                        1
001800170510     D  CODCLIVAS                     7
001900170510     D  TIPFILE                       2
002000170510     D  FLGEXE                        1
002100170510     D  TIPCLI                        1
002200170510     D  FLGNOE                        1
002300170510     D  KAAS                          4  0
002400170510     D  KLNP                          3  0
002500170510     D  KNRS                          2  0
002600170510     D  KNSP                          7  0
002700170510     D  DACMDA                        8  0
002800170510     D  DACMA                         8  0
002900170510     D*--------------------
003000010402
003100010402
003200100512     C*
003300100512     C* Calcola la data corrente
003400100512     C                   z-add     *zeros        datcor            8 0
003500100512     C                   eval      datcor = %dec(%date() : *ISO)
003600100512     C*
003700100512     C* Qunidi richiamo il *pgm standard
003800100512     C                   exsr      EXEELAB
003900991110     C*
004000100512     C                   seton                                        LR
004100100512
004200100512
004300100512
004400100512     C*----------------------------------------------------*
004500100512     C     EXEELAB       BEGSR
004600100512     C*
004700100512     C* Valorizzo la DS d passaggio parametri
004800170510     C                   clear                   DSINPUT
004900100512     C*
005000170510     C                   eval      DATADA  = 20170219
005100170510     C                   eval      DATAA   = 20170219
005200170510     C                   eval      CODCLI  = 1662139
005300170510     C                   eval      FLGUNI  = 'S'
005400170510     C                   eval      FLGCONS = 'N'
005500170510     C                   eval      TIPCLI  = 'E'
005600170510     C                   eval      FLGNOE  = 'S'
005700170510     C                   eval      KAAS    = 2017
005800170510     C                   eval      KLNP    = 166
005900170510     C                   eval      KNRS    = 03
006000170510     C                   eval      KNSP    = 1271224
006100100512     C*
006200100512     C* Quindi elabora
006300170510     C                   EVAL      PARAM = DSINPUT
006400170510     C                   CALL      'TIS7VCR'
006500170510     C                   PARM                    PARAM            71
006600170510     C                   PARM                    FNVACDS
006700100512     C*
006800100512     C                   ENDSR
006900100512     C*----------------------------------------------------*
007000100512
007100100512
007200100512
007300090424     C*----------------------------------------------------*
007400090424     C     *INZSR        BEGSR
007500090424     C*
007600100512     C*
007700100512     C                   ENDSR

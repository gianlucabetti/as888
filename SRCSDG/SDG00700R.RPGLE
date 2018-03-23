000100100120     H*BIND
000200100120     H*PARMS OPTION(*NOXREF *NODEBUGIO) CVTOPT(*NONE)
000300100120     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000400100120      *
000500100120      *           -----------------------------------------
000600170727      *                Spesometro trimestrale
000700170727      *            Esegue in *share l'ovrprtf di SDGSI52P1
000800100120      *           -----------------------------------------
000900170727      *
001000170727R258  * Questo pgm è stato creato con il task R258 Comunicaz. Spesometro Trimestrale
001100100120      *
001200100120     D*-------------
001300100120     D* Passaggio Parametri
001400100120     D KPJBA         E DS
001500100120     D*-------------
001600100120     D* Parametri ricevuti
001700170727     D SDG00700DS    E DS
001800100120     D*-------------
001900100120     D* campi di lavoro
002000100120     D WErrori         S              1A   Inz('0')
002100100120     D*----------
002200100120     D* appoggio a chiamata QCMDEXC
002300100120     D Cmd             S            100
002400100120     D Len             S             15  5 Inz(100)
002500100120     C************************************************************
002600100120     C*                MAIN LINE PROGRAM
002700100120     C************************************************************
002800100120     C*
002900100120     C* inizializzazione variabili
003000100120     C                   EXSR      INZVAR
003100100120     C*
003200100120     C* eseguo l'ovrprtf in *share del file di stampa
003300100120     C                   EXSR      OvrPrtF
003400100120     C*
003500100120     C* fine programma
003600100120     C                   EXSR      ENDPGM
003700100120     C*
003800100120      /EJECT
003900100120     C************************************************************
004000100120     C* OPERAZIONI INIZIALI
004100100120     C************************************************************
004200100120     C     *INZSR        BEGSR
004300100120     C*
004400100120     C* Reperimento parametri
004500100120     C*
004600100120     C     *ENTRY        PLIST
004700100120     C                   PARM                    KPJBA
004800100120     C*
004900100120     C*
005000100120     C                   ENDSR
005100100120     C/EJECT
005200100120     C************************************************************
005300100120     C* Inizializzazione variabili
005400100120     C************************************************************
005500100120     C     INZVAR        BEGSR
005600100120     C*
005700170727     C                   MoveL     KPJBU         SDG00700DS
005800100120     C*
005900170727     C                   Eval      Esito0700   = *Off
006000100120     C*
006100100120     C                   Eval      WErrori     = *Off
006200100120     C*
006300100120     C                   ENDSR
006400100120      /EJECT
006500100120     C************************************************************
006600110720     C* OVRPRTF    FILE(SDGSD52P1) OVRSCOPE(*JOB) SHARE(*YES)
006700100120     C************************************************************
006800100120     C     OvrPrtF       BEGSR
006900100120     C*
007000100120     C                   Clear                   Cmd
007100100120     C     'OVRPRTF'     Cat       'FILE(':1     Cmd
007200130826     C                   Cat       'SDGSG52P1':0 Cmd
007300100120     C                   Cat       ')':0         Cmd
007400100120     C*
007500100120     C                   Cat       'OVRSCOPE(':1 Cmd
007600100120     C                   Cat       '*JOB':0      Cmd
007700100120     C                   Cat       ')':0         Cmd
007800100120     C*
007900100120     C                   Cat       'SHARE(':1    Cmd
008000100120     C                   Cat       '*YES':0      Cmd
008100100120     C                   Cat       ')':0         Cmd
008200100120     C*
008300100120     C                   Call      'QCMDEXC'                            24
008400100120     C                   Parm                    Cmd
008500100120     C                   Parm                    LeN
008600100120     C*
008700100120     C* Fallito l'ovrprtf del file di spool
008800100120     C                   if        *In24       = *On
008900100120     C                   Eval      WErrori     = *On
009000100120     C                   endif
009100100120     C*
009200100120     C                   ENDSR
009300100120      /EJECT
009400100120     C************************************************************
009500100120     C* FINE PROGRAMMA
009600100120     C************************************************************
009700100120     C     ENDPGM        BEGSR
009800100120     C*
009900100120     C                   if        WErrori     = *On
010000170727     C                   Eval      Esito0700   = *On
010100100120     C                   endif
010200100120     C*
010300170727     C                   MoveL     SDG00700DS    KPJBU
010400100120     C*
010500100120     C                   SetOn                                        RT
010600100120     C                   Return
010700100120     C*
010800100120     C                   ENDSR
010900100120      /EJECT

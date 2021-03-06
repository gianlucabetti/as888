000100100120     H*BIND
000200100120     H*PARMS OPTION(*NOXREF *NODEBUGIO) CVTOPT(*NONE)
000300100120     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000400100120      *
000500100120      *           -----------------------------------------
000501130826      *                Spesometro clienti/fornitori
000700110720      *            Esegue in *share l'ovrprtf di SDGSD52P1
000800100120      *           -----------------------------------------
000900100120      *
001000100120     D*-------------
001100100120     D* Passaggio Parametri
001200100120     D KPJBA         E DS
001300100120     D*-------------
001400100120     D* Parametri ricevuti
001500130826     D SDG00500DS    E DS
001600100120     D*-------------
001700100120     D* campi di lavoro
001800100120     D WErrori         S              1A   Inz('0')
001900100120     D*----------
002000100120     D* appoggio a chiamata QCMDEXC
002100100120     D Cmd             S            100
002200100120     D Len             S             15  5 Inz(100)
002300100120     C************************************************************
002400100120     C*                MAIN LINE PROGRAM
002500100120     C************************************************************
002600100120     C*
002700100120     C* inizializzazione variabili
002800100120     C                   EXSR      INZVAR
002900100120     C*
003000100120     C* eseguo l'ovrprtf in *share del file di stampa
003100100120     C                   EXSR      OvrPrtF
003200100120     C*
003300100120     C* fine programma
003400100120     C                   EXSR      ENDPGM
003500100120     C*
003600100120      /EJECT
003700100120     C************************************************************
003800100120     C* OPERAZIONI INIZIALI
003900100120     C************************************************************
004000100120     C     *INZSR        BEGSR
004100100120     C*
004200100120     C* Reperimento parametri
004300100120     C*
004400100120     C     *ENTRY        PLIST
004500100120     C                   PARM                    KPJBA
004600100120     C*
004700100120     C*
004800100120     C                   ENDSR
004900100120     C/EJECT
005000100120     C************************************************************
005100100120     C* Inizializzazione variabili
005200100120     C************************************************************
005300100120     C     INZVAR        BEGSR
005400100120     C*
005500130826     C                   MoveL     KPJBU         SDG00500DS
005600100120     C*
005700130826     C                   Eval      Esito0500   = *Off
005800100120     C*
005900100120     C                   Eval      WErrori     = *Off
006000100120     C*
006100100120     C                   ENDSR
006200100120      /EJECT
006300100120     C************************************************************
006400110720     C* OVRPRTF    FILE(SDGSD52P1) OVRSCOPE(*JOB) SHARE(*YES)
006500100120     C************************************************************
006600100120     C     OvrPrtF       BEGSR
006700100120     C*
006800100120     C                   Clear                   Cmd
006900100120     C     'OVRPRTF'     Cat       'FILE(':1     Cmd
007000130826     C                   Cat       'SDGSG52P1':0 Cmd
007100100120     C                   Cat       ')':0         Cmd
007200100120     C*
007300100120     C                   Cat       'OVRSCOPE(':1 Cmd
007400100120     C                   Cat       '*JOB':0      Cmd
007500100120     C                   Cat       ')':0         Cmd
007600100120     C*
007700100120     C                   Cat       'SHARE(':1    Cmd
007800100120     C                   Cat       '*YES':0      Cmd
007900100120     C                   Cat       ')':0         Cmd
008000100120     C*
008100100120     C                   Call      'QCMDEXC'                            24
008200100120     C                   Parm                    Cmd
008300100120     C                   Parm                    LeN
008400100120     C*
008500100120     C* Fallito l'ovrprtf del file di spool
008600100120     C                   if        *In24       = *On
008700100120     C                   Eval      WErrori     = *On
008800100120     C                   endif
008900100120     C*
009000100120     C                   ENDSR
009100100120      /EJECT
009200100120     C************************************************************
009300100120     C* FINE PROGRAMMA
009400100120     C************************************************************
009500100120     C     ENDPGM        BEGSR
009600100120     C*
009700100120     C                   if        WErrori     = *On
009800130826     C                   Eval      Esito0500   = *On
009900100120     C                   endif
010000100120     C*
010100130826     C                   MoveL     SDG00500DS    KPJBU
010200100120     C*
010300100120     C                   SetOn                                        RT
010400100120     C                   Return
010500100120     C*
010600100120     C                   ENDSR
010700100120      /EJECT

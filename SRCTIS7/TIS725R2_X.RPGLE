000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*--------------------------------------------------------------*
000300001218
000400100913     D tivssds       e ds                  extname(tivss00f)
000500100913     D tivirds       e ds                  extname(tivss00f)
000600100914     D tis725ds21    e ds
000700100217     D kpjba         e ds
000800001218
000900100217
001000920812     C*---------------------------------------------------------------*
001100001218     C* MAIN
001200001218     C*---------------------------------------------------------------*
001300100914     C*
001400100914     C* Verifico il tipo passaggio parametri ricevuto
001500100914     C                   select
001600100914     C                   when      ds_typ = '1'
001700100914     C                   eval      tis725ds21 = ds_param
001800100914     C                   endsl
001900100913     C*
002000100913     C* Verifico l'opzione d entrata richiesta
002100100913     C                   select
002200100914     C*
002300100913     C* Verifica/inserimento UTENTE
002400100914     C                   when      §21_FILE   = 'VSS' and
002500100914     C                             §21_OPERAZ = 'INS'
002600100914     C                   exsr      insVSS
002700100914     C*
002800100913     C* Verifica/inserimento TIPI FILE
002900100914     C                   when      §21_FILE   = 'VIR' and
003000100914     C                             §21_OPERAZ = 'INS'
003100100914     C                   exsr      insVIR
003200100914     C*
003300100913     C* Verifica/inserimento UTENTE + TIPI FILE
003400100914     C                   when      §21_FILE   = 'ESY'
003500100914     C                   exsr      insESYSP
003600100914     C*
003700100913     C                   endsl
003800001218     C*
003900001218     C                   seton                                        LR
004000001218
004100001218
004200001218
004300001218     C*------------------------------------------------------------------------*
004400100914     C* EXEVSS - Verifica/inserimento UTENTE
004500001218     C*------------------------------------------------------------------------*
004600100914     C     EXEVSS        BEGSR
004700040705     C*
004800100914     C* Verifica/inserimento
004900100914     C                   if        IN_OPERAZ = 'INS'
005000100913     C                   eval      TIPFIL   = 'VSS'
005100100913     C                   eval      UTENTE   = %subst(vssKSU:2:7)
005200100913     C                   eval      DESCRIZ  = IN_DESCRIZ
005300100913     C                   eval      PASSWORD = IN_PASSWORD
005400100913     C                   call      'TIS725R3'
005500100913     C                   parm                    TIPFIL
005600100914     C                   parm                    IN_OPERAZ
005700100913     C                   parm                    UTENTE
005800100913     C                   parm                    DESCRIZ
005900100913     C                   parm                    PASSWORD
006000100913     C                   parm                    TIVSSDS
006100100913     C                   parm                    TIVIRDS
006200100913     C                   parm                    FLG_ERR
006300100913     C                   parm                    FLG_MSG
006400001218     C*
006500001218     C                   ENDSR
006600001218     C*------------------------------------------------------------------------*
006700100913
006800100913
006900100913
007000100913     C*------------------------------------------------------------------------*
007100100913     C* INSVIR_ESYSP - Verifica/inserimento TIPI FILE "Easy Sped"
007200100913     C*------------------------------------------------------------------------*
007300100913     C     INSVIR_ESYSP  BEGSR
007400100913     C*
007500100913     C                   clear                   tivirds
007600100913     C*
007700100913     C                   eval      TIPFIL   = 'VIR'
007800100913     C                   eval      TIPOPE   = 'INS'
007900100913     C                   eval      UTENTE   = %subst(vssKSU:2:7)
008000100913     C                   eval      DESCRIZ  = IN_DESCRIZ
008100100913     C                   eval      PASSWORD = IN_PASSWORD
008200100913     C                   call      'TIS725R3'
008300100913     C                   parm                    TIPFIL
008400100913     C                   parm                    TIPOPE
008500100913     C                   parm                    UTENTE
008600100913     C                   parm                    DESCRIZ
008700100913     C                   parm                    PASSWORD
008800100913     C                   parm                    TIVSSDS
008900100913     C                   parm                    TIVIRDS
009000100913     C                   parm                    FLG_ERR
009100100913     C                   parm                    FLG_MSG
009200100913     C*
009300100913     C                   ENDSR
009400100913     C*------------------------------------------------------------------------*
009500100913
009600100913
009700100913
009800100913     C*------------------------------------------------------------------------*
009900100913     C* INSVIR - Verifica/inserimento TIPI FILE
010000100913     C*------------------------------------------------------------------------*
010100100913     C     INSVIR        BEGSR
010200100913     C*
010300100913     C                   eval      TIPFIL   = 'VIR'
010400100913     C                   eval      TIPOPE   = 'INS'
010500100913     C                   eval      UTENTE   = %subst(vssKSU:2:7)
010600100913     C                   eval      DESCRIZ  = IN_DESCRIZ
010700100913     C                   eval      PASSWORD = IN_PASSWORD
010800100913     C                   call      'TIS725R3'
010900100913     C                   parm                    TIPFIL
011000100913     C                   parm                    TIPOPE
011100100913     C                   parm                    UTENTE
011200100913     C                   parm                    DESCRIZ
011300100913     C                   parm                    PASSWORD
011400100913     C                   parm                    TIVSSDS
011500100913     C                   parm                    TIVIRDS
011600100913     C                   parm                    FLG_ERR
011700100913     C                   parm                    FLG_MSG
011800100913     C*
011900100913     C                   ENDSR
012000100913     C*------------------------------------------------------------------------*
012100001218
012200001218
012300001218
012400001218
012500001218     C*------------------------------------------------------------------------*
012600001218     C* *INZSR - ROUTINE INIZIALE
012700001218     C*------------------------------------------------------------------------*
012800001218     C     *INZSR        BEGSR
012900100217     C*
013000100217     C     *ENTRY        PLIST
013100100914     C                   PARM                    ds_typ            1
013200100914     C                   PARM                    ds_param       1024
013300100914     C                   PARM                    tivssds
013400100914     C                   PARM                    tivirds
013500001218     C*
013600001218     C                   ENDSR

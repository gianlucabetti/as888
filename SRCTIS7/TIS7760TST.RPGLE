000100040409      /TITLE Test TIS7760R.
000200040407
000300040407     D TIS776DSI     E DS
000400040407     D                                     INZ
000500040407     D TIS173DSO     E DS
000600040407     D                                     INZ
000700040407
000800040408     D TIS173          S
000900040408     D                                     LIKE(TIS173DSO)
001000040408     D                                     DIM(20)
001100040408
001200040407     D Operazione      S              1
001300040407     D                                     INZ('1')
001400040407     D RigheRic        S              5S 0
001500040407     D                                     INZ(20)
001600040408     D DatiOut         S           9999
001700040408     D RigheOut        S              5S 0
001800040407     D Esito           S              1
001900040407     D PrmAnonimo      S
002000040407     D                                     LIKE(AnonimoI76)
002100040409     D PrmKSU          S
002200040409     D                                     LIKE(KSUI76)
002300040409     D PrmCCM          S
002400040409     D                                     LIKE(CCMI76)
002500040407     D PrmChiSono      S
002600040407     D                                     LIKE(ChiSonoI76)
002700040407     D PrmParzial      S
002800040407     D                                     LIKE(ParzialI76)
002900040407     D PrmDIn          S
003000040407     D                                     LIKE(DInI76)
003100040407     D PrmDFi          S
003200040407     D                                     LIKE(DFiI76)
003300040407
003400040407     ***********************************************************************************************
003500040407     **
003600040407     **
003700040407     **
003800040407     ***********************************************************************************************
003900040407     C     *ENTRY        PLIST
004000040407     C     AnonimoI76    PARM                    PrmAnonimo
004100040409     C     KSUI76        PARM                    PrmKSU
004200040409     C     CCMI76        PARM                    PrmCCM
004300040407     C     ChiSonoI76    PARM                    PrmChiSono
004400040407     C     ParzialI76    PARM                    PrmParzial
004500040407     C     DInI76        PARM                    PrmDIn
004600040407     C     DFiI76        PARM                    PrmDFi
004700040407
004800040408     C                   DO        *HIVAL
004900040407
005000040409     C                   CALL      'TIS7760R'
005100040407     C                   PARM                    TIS776DSI
005200040407     C                   PARM                    Operazione
005300040407     C                   PARM                    RigheRic
005400040408     C                   PARM      *BLANK        DatiOut
005500040407     C                   PARM      *ZERO         RigheOut
005600040407     C                   PARM      *BLANK        Esito
005700040408
005800040408     C                   IF        Operazione = '9' OR Esito = '1'
005900040408     C                   LEAVE
006000040408     C                   ENDIF
006100040408
006200040408     C                   MOVEA     DatiOut       TIS173
006300040408
006400040408     C                   EVAL      Operazione = '2'
006500040408
006600040408     C                   ENDDO
006700040407
006800040407     C                   EVAL      *INLR = *ON

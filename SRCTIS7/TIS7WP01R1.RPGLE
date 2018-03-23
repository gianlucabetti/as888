000100000000     H Option( *SrcStmt )
000200160804
000300161011      /COPY STRABASRC/SRCWSER,WPER0010D
000400161011      /COPY GAITRASRC/SRCPROTOPI,UBISVPWDR
000500160924      /COPY GAITRASRC/SRCPROTOPI,TIS7WP01R
000600090521
000700160922
000800160922     C
000900170302     C                   eval      TIS7WP01DI.APIVersion = 4
001000161010     C                   eval      TIS7WP01DI.CliUnif    = '1662139'
001100161010     C                   eval      TIS7WP01DI.TipoLegame = 'WW'
001200170302     C                   eval      TIS7WP01DI.TrkNumber  = '166011033334968'
001300161011     C*
001400161011     C                   eval      Request = TIS7WP01DI
001500090520     C*
001600160922     C                   Call      'TIS7WP01R'
001700161011     C                   Parm                    Request
001800161011     C                   Parm                    Response
001900160924     C*
002000161011     C                   eval      TIS7WP01DO =  Response
002100160924     C*
002200161010     C                   movel     *blanks       wDisplay         52
002300161010     C                   eval      wDisplay = %char(TIS7WP01DO.EventCount)
002400160924     C*
002500161010     C     wDisplay      DSPLY
002600160924     C*
002700090520     C                   Seton                                        LR
002800000000     **

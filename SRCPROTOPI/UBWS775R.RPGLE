000100081118     D*------------------
000200170508     D* PASSAGGIO PARAMETRI A PROCEDURE UBWS775R
000300081118     D*------------------
000400170511     D iWS775TIP       S              1a
000500170509     D iWS775AAS       S              4s 0
000600170508     D iWS775RMA       S             15a
000700170508     D iWS775RMN       S             15s 0
000800170511     D iWS775CLI       S              7s 0
000900170515     D iWS775TLE       S              2a
001000170508     D iWS775LANG      S              2a
001100160916     D*
001200170508     D oWS775Esito     S              1s 0                                       0 = OK
001300170509     D*                                                                         -1 = Generic Err
001400170509     D*                                                                         -2 = Parameter Err
001500170509     D*                                                                         -3 = Sped Not Found
001600170509     D*                                                                         -4 = Sped Not Unique
001700170508     D oWS775AAS       S              4s 0
001800170508     D oWS775LNP       S              3s 0
001900170508     D oWS775NRS       S              2s 0
002000170508     D oWS775NSP       S              7s 0
002100170515     D oWS775DSP       S              8s 0
002200170515     D oWS775CLI       S              7s 0
002300170515     D oWS775COP       S              7s 0
002400170515     D oWS775Msg       S            128a

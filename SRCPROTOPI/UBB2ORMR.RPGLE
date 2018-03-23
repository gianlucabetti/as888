000100081118     D*------------------
000200170728     D* PASSAGGIO PARAMETRI A PROCEDURE UBB2ORMR
000300081118     D*------------------
000400170728     D iB2ORMSUN       S              9a
000500170728     D iB2ORMKSU       S              7a
000600170728     D iB2ORMISV       S              2a
000700170728     D iB2ORMAAS       S              4s 0
000800170728     D iB2ORMSPE       S             12s 0
000900170728     D iB2ORMCRC       S             10s 0
001000170728     D iB2ORMDPM       S              8s 0
001100170728     D iB2ORMORARD     S              4s 0
001200170728     D iB2ORMORARA     S              4s 0
001300170728     D iB2ORMRFA       S             15a
001400170728     D iB2ORMEMLR      S             60a
001500170728     D iB2ORMSMSR      S             16a
001600170728     D iB2ORMNOTE      S             70a
001700170728     D iB2ORMIP        S             15a
001800160916     D*
001900170728     D oB2ORMEsito     S              1s 0                                       0 = OK
002000170728     D*                                                                         -1 = Generic Err
002100170509     D*                                                                         -2 = Parameter Err
002200170509     D*                                                                         -3 = Sped Not Found
002300170804     D*                                                                         -4 = Cod.cons.NotFnd
002400170728     D oB2ORMNPR       S             10a

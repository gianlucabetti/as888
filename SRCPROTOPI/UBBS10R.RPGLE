000100081118     D*------------------
000200170515     D* PASSAGGIO PARAMETRI A PROCEDURE UBBS10R
000300081118     D*------------------
000400170515     D iBS10DRF        S              8s 0
000500170515     D iBS10TLE        S              2a
000600170515     D iBS10CLI        S              7s 0
000700160916     D*
000800170515     D oBS10Esito      S              1s 0                                       0 = OK
000900170509     D*                                                                         -1 = Generic Err
001000170515     D*                                                                         -2 = Err Parametro
001100170515     D oBS10COP        S              7s 0
001200170515     D oBS10TIP        S              1a
001300170515     D*                                                                          P = Padre
001400170515     D*                                                                          F = Padre
001500170515     D*                                                                          S = Singolo

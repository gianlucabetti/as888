000100081118     D*------------------
000200170803     D* PASSAGGIO PARAMETRI A PROCEDURE UBDKC2BR
000300081118     D*------------------
000400170803     D iDKC2BKSU       S              7a
000500170803     D iDKC2BTLE       S              2a
000600170803     D iDKC2BWHO       S             35a
000700160916     D*
000800170803     D oDKC2BEsito     S              1s 0                                       0 = OK
000900170728     D*                                                                         -1 = Generic Err
001000170509     D*                                                                         -2 = Parameter Err
001100170509     D*                                                                         -3 = Sped Not Found
001200170803     D oDKC2BVAB       S                   like(EDIVABDS)

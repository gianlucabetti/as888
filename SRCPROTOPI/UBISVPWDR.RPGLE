000100081118     D*------------------
000200160916     D* PASSAGGIO PARAMETRI A PROCEDURE UBISVPWDR
000300081118     D*------------------
000400160916     D iISVPWDUTE      S              7a
000500160916     D iISVPWDSUN      S              9a
000600160916     D iISVPWDISV      S              2a
000700161017     D iISVPWDTIP      S              3a
000800160916     D iISVPWDPWD      S             20a
000900160916     D iISVPWDLANG     S              2a
001000160916     D*
001100160923     D oISVPWDEsito    S              1s 0                                      0=Err 1=OK 2=KO
001200161108     D oISVPWDUTE      S              7a
001300161108     D oISVPWDSUN      S              9a
001400160923     D oISVPWDTLE      S              2a
001500160916     D oISVPWDRSP      S             40a
001600160923     D oISVPWDEML      S            100a
001700160923     D oISVPWDTEL      S             20a
001800160916     D oISVPWDMsg      S            128a   varying

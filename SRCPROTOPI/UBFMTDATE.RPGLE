000100081118     D*------------------
000200120104     D* PASSAGGIO PARAMETRI A PROCEDURE UBFMTDATE
000300081118     D*------------------
000400120104     D pInDateIn       S          32767A   VARYING
000500120104     D pInMaskDateIn   S          32767A   VARYING
000600120104     D pInMaskDateOut  S          32767A   VARYING
000700120104     D pInFormat       S              2A   VARYING
000800120104     D pOutREDS        DS
000900120104     D  pOutREDateOut                64A
001000120104     D  pOutREError                   2A
001100120104     D  pOutREDescr                  34A

000100081118     D*------------------
000200170518     D* PASSAGGIO PARAMETRI A PROCEDURE UBGETIFSR
000300081118     D*------------------
000400170518     D iGETIFSfile     S           1024a                                        Percorso e nome file
000500170518     D iGETIFSbinTxt   S              3a                                        BIN = Output binario
000600170518     D*                                                                         TXT = Output test
000700170518     D iGETIFSbase64   S              1a                                        Y = Output in Base64
000800170518     D iGETIFSmaxLen   S             10i 0                                      Lunghezza max dati
000900160916     D*
001000170518     D oGETIFSesito    S              1s 0                                       0 = OK
001100170509     D*                                                                         -1 = Generic Err
001200170509     D*                                                                         -2 = Parameter Err
001300170518     D*                                                                         -3 = File too large
001400170518     D oGETIFScodPage  S              5u 0                                      CCSID file letto
001500170518     D oGETIFS_CCSID   S              5u 0                                      CodePage file letto
001600170518     D oGETIFSlen      S             10i 0                                      Lunghezza dati out
001700170518     D oGETIFSdati4MB  S               a   VARYING LEN(4194304)                 Dati di ouput

000100160901     d UBVMSDDER...
000200160901     d                 pr                  extpgm('UBVMSDDER')
000300160901     d pIn_Opz                        1a
000400160901       // Procedura
000500160901     d pIn_Proc                      10a
000600160901       // Cliente unificante
000700160901     d pIn_KSU                        8a
000800160901       // Data di riferimento
000900160901     d pIn_DtRif                      8s 0
001000160901       // Data decorrenza
001100160901     d pOut_DDE                       8s 0
001200160901       // esito: 1=OK  I=Err parametri input  2=KO SQL  3=non trovato periodo  4=trovati + periodi
001300160901     d pOut_Esito                     1a

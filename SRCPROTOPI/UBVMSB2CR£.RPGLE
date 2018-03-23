000100160920     d UBVMSB2CR...
000200160920     d                 pr                  extpgm('UBVMSB2CR')
000300160901     d pIn_Opz                        1a
000400160920       // Data di riferimento
000500160920     d pIn_DtRif                      8s 0
000600160920       // Lingua
000700160920     d pIn_Lingua                     1
000800160920       // Procedura
000900160901     d pIn_Proc                      10a
001000160901       // Cliente unificante
001100160901     d pIn_KSU                        8a
001200160920       // Causale BRT
001300160920     d pIn_CauBRT                    15a
001400160901       // esito: 1=OK  I=Err parametri input  2=KO SQL  3=non trovato periodo  4=trovati + periodi
001500160901     d pOut_Esito                     1a
001600160920       // Tipo Causale
001700160920     d pOut_CauTip                    3a
001800160920       // Causale T&T
001900160920     d pOut_CauTT                    15a
002000160920       // Stato cliente
002100160920     d pOut_StsCli                   15a
002200160920       // Stato opzioni
002300160920     d pOut_StsOpz                    5a
002400160920       // Fase operativa
002500160920     d pOut_FasOp                    10a
002600160920       // Data decorrenza
002700160920     d pOut_DDE                       8s 0
002800160920       // Descrizione
002900160920     d pOut_Desc                    200a   varying

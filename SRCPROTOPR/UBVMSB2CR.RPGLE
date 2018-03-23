000100160922       // pgm
000200160922     d UBVMSB2CR_P...
000300160920     d                 pr                  extpgm('UBVMSB2CR')
000400160921     d iVMSB2COpz                     1a   const
000500160920       // Data di riferimento
000600160921     d iVMSB2CDtRif                   8s 0 const
000700160920       // Lingua
000800160921     d iVMSB2CLingua                  2A   const
000900160920       // Procedura
001000160921     d iVMSB2CProc                   10a   const
001100160901       // Cliente unificante
001200160921     d iVMSB2CKSU                     8a   const
001300160920       // Causale BRT
001400160921     d iVMSB2CCauBRT                 15a   const
001500160901       // esito: 1=OK  I=Err parametri input  2=KO SQL  3=non trovato periodo  4=trovati + periodi
001600160921     d oVMSB2CEsito                   1a
001700160920       // Tipo Causale
001800160921     d oVMSB2CCauTip                  3a
001900160920       // Causale T&T
002000160921     d oVMSB2CCauTT                  15a
002100160920       // Stato cliente
002200160921     d oVMSB2CStsCli                 15a
002300160920       // Stato opzioni
002400160921     d oVMSB2CStsOpz                  5a
002500160920       // Fase operativa
002600160921     d oVMSB2CFasOp                  10a
002700160920       // Data decorrenza
002800160921     d oVMSB2CDDE                     8s 0
002900160920       // Descrizione
003000160921     d oVMSB2CDesc                  200a   varying
003100160922
003200160922       // procedura da srvpgm
003300160922     d UBVMSB2CR_S...
003400160922     d                 pr                  extproc('UBVMSB2CR')
003500160922     d iVMSB2COpz                     1a   const
003600160922       // Data di riferimento
003700160922     d iVMSB2CDtRif                   8s 0 const
003800160922       // Lingua
003900160922     d iVMSB2CLingua                  2A   const
004000160922       // Procedura
004100160922     d iVMSB2CProc                   10a   const
004200160922       // Cliente unificante
004300160922     d iVMSB2CKSU                     8a   const
004400160922       // Causale BRT
004500160922     d iVMSB2CCauBRT                 15a   const
004600160922       // esito: 1=OK  I=Err parametri input  2=KO SQL  3=non trovato periodo  4=trovati + periodi
004700160922     d oVMSB2CEsito                   1a
004800160922       // Tipo Causale
004900160922     d oVMSB2CCauTip                  3a
005000160922       // Causale T&T
005100160922     d oVMSB2CCauTT                  15a
005200160922       // Stato cliente
005300160922     d oVMSB2CStsCli                 15a
005400160922       // Stato opzioni
005500160922     d oVMSB2CStsOpz                  5a
005600160922       // Fase operativa
005700160922     d oVMSB2CFasOp                  10a
005800160922       // Data decorrenza
005900160922     d oVMSB2CDDE                     8s 0
006000160922       // Descrizione
006100160922     d oVMSB2CDesc                  200a   varying

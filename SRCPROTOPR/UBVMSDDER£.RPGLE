000100160901     d TIVMS_ds      e ds                  extname(TIVMS00F:TIVMS000)
000200160901
000300160901       // - Parametri:
000400160901       // opz: blank=elabora  C=chiudi
000500160901     d pIn_Opz         s              1a
000600160901       // Procedura
000700160901     d pIn_Proc        s                   like(VMSProc)
000800160901       // Cliente unificante
000900160901     d pIn_KSU         s                   like(VMSKSU)
001000160901       // Data di riferimento
001100160901     d pIn_DtRif       s              8s 0
001200160901       // Data decorrenza
001300160901     d pOut_DDE        s                   like(VMSDDE)
001400160901       // esito: 1=OK  I=Err parametri input  2=KO SQL  3=non trovato periodo  4=trovati + periodi
001500160901     d pOut_Esito      s              1a

000100160921     d TIVMS_VMSDDE  e ds                  extname(TIVMS00F:TIVMS000)
000200160921     d                                     prefix('D_')
000300160901
000400160901       // - Parametri:
000500160901       // opz: blank=elabora  C=chiudi
000600160921     d iVMSDDEOpz      s              1a
000700160921       // Procedura
000800160921     d iVMSDDEProc     s                   like(D_VMSProc)
000900160921       // Cliente unificante
001000160921     d iVMSDDEKSU      s                   like(D_VMSKSU)
001100160921       // Data di riferimento
001200160921     d iVMSDDEDtRif    s              8s 0
001300160921       // Data decorrenza
001400160921     d oVMSDDEDDE      s                   like(D_VMSDDE)
001500160921       // esito: 1=OK  I=Err parametri input  2=KO SQL  3=non trovato periodo  4=trovati + periodi
001600160921     d oVMSDDEEsito    s              1a

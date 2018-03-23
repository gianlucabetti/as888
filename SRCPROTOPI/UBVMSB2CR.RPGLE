000100160921     d TIVMS_VMSB2C  e ds                  extname(TIVMS00F:TIVMS000)
000200160921     d                                     prefix('B_')
000300160920
000400160920       // - Parametri:
000500160920       // opz: blank=elabora  C=chiudi
000600160921     d iVMSB2COpz      s              1a
000700160921       // Data di riferimento
000800160921     d iVMSB2CDtRif    s              8s 0
000900160921       // Lingua
001000160921     d iVMSB2CLingua   s              2
001100160921       // Procedura
001200160921     d iVMSB2CProc     s                   like(B_VMSProc)
001300160921       // Cliente unificante
001400160921     d iVMSB2CKSU      s                   like(B_VMSKSU)
001500160921       // Causale BRT
001600160921     d iVMSB2CCauBRT   s                   like(B_VMSCAUBRT)
001700160921       // esito: 1=OK  I=Err parametri input  2=KO SQL  3=non trovato periodo  4=trovati + periodi
001800160921     d oVMSB2CEsito    s              1a
001900160921       // Tipo Causale
002000160921     d oVMSB2CCauTip   s                   like(B_VMSCAUTIP)
002100160921       // Causale T&T
002200160921     d oVMSB2CCauTT    s                   like(B_VMSCAUTT)
002300160921       // Stato cliente
002400160921     d oVMSB2CStsCli   s                   like(B_VMSStsCli)
002500160921       // Stato opzioni
002600160921     d oVMSB2CStsOpz   s                   like(B_VMSStsOpz)
002700160921       // Fase operativa
002800160921     d oVMSB2CFasOp    s                   like(B_VMSFasOp)
002900160921       // Data decorrenza
003000160921     d oVMSB2CDDE      s              8s 0
003100160921       // Descrizione
003200160921     d oVMSB2CDesc     s                   like(B_VMSDescriz)

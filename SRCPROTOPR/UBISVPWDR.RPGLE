000100081003     P*--------------------------------------------------
000200160916     P* Procedure name: UBISVPWDR...
000300160916     P* Purpose:        Verifica credenziali per accesso servizi VAS
000400160916     P* Returns:        -------
000500160916     P* Parameter:      iISVPWDUTE     => Utente (Cliente Unificante)
000600160916     P* Parameter:      iISVPWDSUN     => Strategi User Number
000700160916     P* Parameter:      iISVPWDISV     => Servizio Internet
000800161017     P* Parameter:      iISVPWDTIP     => Tipo Servizio Internet
000900160916     P* Parameter:      iISVPWDPWD     => Passwrod
001000160916     P* Parameter:      iISVPWDLANG    => Lingua
001100160916     P* Parameter:      oISVPWDEsito   => Utente (Cliente Unificante)
001200161108     P* Parameter:      oISVPWDUTE     => Utente (Cliente Unificante)
001300161108     P* Parameter:      oISVPWDSUN     => Strategi User Number
001400160923     P* Parameter:      oISVPWDTLE     => Tipo Legame
001500160923     P* Parameter:      oISVPWDRSP     => Responsabile del Servizio
001600160923     P* Parameter:      oISVPWDEML     => Email Resp. de Servizio
001700160923     P* Parameter:      oISVPWDTEL     => Teledono Resp. del Servizio
001800160923     P* Parameter:      oISVPWDMsg     => Messaggio esito
001900081003     P*--------------------------------------------------
002000081003     P*
002100160916     D UBISVPWDR...
002200160916     D                 PR
002300160916     D   iISVPWDUTE                   7a
002400160916     D   iISVPWDSUN                   9a
002500160916     D   iISVPWDISV                   2a
002600161017     D   iISVPWDTIP                   3a
002700160916     D   iISVPWDPWD                  20a
002800160916     D   iISVPWDLANG                  2a
002900080611     D*
003000160923     D   oISVPWDEsito                 1s 0                                      0=Err 1=OK 2=KO
003100161108     D   oISVPWDUTE                   7a
003200161108     D   oISVPWDSUN                   9a
003300160923     D   oISVPWDTLE                   2a
003400160923     D   oISVPWDRSP                  40a
003500160923     D   oISVPWDEML                 100a
003600160923     D   oISVPWDTEL                  20a
003700160916     D   oISVPWDMsg                 128a   varying
003800160916     D*

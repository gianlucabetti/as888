000100090609     P*--------------------------------------------------
000200090609     P* Procedure name: UBUTISGI_Init
000300090609     P* Purpose:        Operazioni di inizializzazione modulo
000400090609     P* Returns:        Integer
000500090609     P*--------------------------------------------------
000600090609     D*
000700090609     D UBUTISGI_Init...
000800090609     D                 PR            10I 0
000900090609     D*
001000090609     P*--------------------------------------------------
001100090609     P* Procedure name: UBUTISGI_Finalize
001200090609     P* Purpose:        Operazioni di inizializzazione modulo
001300090609     P* Returns:        Integer
001400090609     P*--------------------------------------------------
001500090609     D*
001600090609     D UBUTISGI_Finalize...
001700090609     D                 PR            10I 0
001800090609     D*
001900071129     P*--------------------------------------------------
002000090609     P* Procedure name: UBUTISGI_RtvSrvSts
002100090611     P* Purpose:        Verifica esistenza servizi attivi del tipo procedura legata
002200090609     P* Returns:        Integer
002300090609     P* Parameter:      pInSUN        => Strategi User Number
002400090609     P* Parameter:      pInKSU        => Cliente Unificante
002500090611     P* Parameter:      pInRGR        => Tipo procedura legata (S=SGI F="FTP")
002600090609     P* Parameter:      pOutEsito     => Esito (N=No Srv 0=No srv attivi 1=Sì srv attivi)
002700071129     P*--------------------------------------------------
002800071129     D*
002900090609     D UBUTISGI_RtvSrvSts...
003000071129     D                 PR            10I 0
003100090609     D  pInSUN                        9A   CONST
003200090609     D  pInKSU                        8A   CONST
003300090609     D  pInRGR                        1A   CONST
003400090609     D  pOutEsito                     1A   OPTIONS(*NOPASS)
003500071129     D*
003600090610     P*--------------------------------------------------
003700090610     P* Procedure name: UBUTISGI_DisableSgiUsr
003800090610     P* Purpose:        Disabilita l'utente Strategi richiesto
003900090610     P* Returns:        Integer
004000090610     P* Parameter:      pInSUN        => Strategi User Number
004100090610     P* Parameter:      pInKSU        => Cliente Unificante
004200090610     P* Parameter:      pOutEsito     => Esito (N=Usr nn trovato  E=Errore  1=Ok usr disabilitato)
004300090610     P*--------------------------------------------------
004400090610     D*
004500090610     D UBUTISGI_DisableSgiUsr...
004600090610     D                 PR            10I 0
004700090610     D  pInSUN                        9A   CONST
004800090610     D  pInKSU                        8A   CONST
004900090610     D  pOutEsito                     1A   OPTIONS(*NOPASS)
005000090610     D*
005100090610     P*--------------------------------------------------
005200090610     P* Procedure name: UBUTISGI_EnableSgiUsr
005300090610     P* Purpose:        Abilita l'utente Strategi richiesto
005400090610     P* Returns:        Integer
005500090610     P* Parameter:      pInSUN        => Strategi User Number
005600090610     P* Parameter:      pInKSU        => Cliente Unificante
005700090610     P* Parameter:      pOutEsito     => Esito (N=Usr nn trovato  E=Errore  1=Ok usr abilitato)
005800090610     P*--------------------------------------------------
005900090610     D*
006000090610     D UBUTISGI_EnableSgiUsr...
006100090610     D                 PR            10I 0
006200090610     D  pInSUN                        9A   CONST
006300090610     D  pInKSU                        8A   CONST
006400090610     D  pOutEsito                     1A   OPTIONS(*NOPASS)
006500090610     D*
006600100827     P*--------------------------------------------------
006700100827     P* Procedure name: UBUTISGI_DeleteSgiUsr
006800100827     P* Purpose:        Elimina l'utente Strategi richiesto
006900100827     P* Returns:        Integer
007000100827     P* Parameter:      pInSUN        => Strategi User Number
007100100827     P* Parameter:      pOutEsito     => Esito (N=Usr nn trovato  E=Errore  1=Ok usr disabilitato)
007200100827     P*--------------------------------------------------
007300100827     D*
007400100827     D UBUTISGI_DeleteSgiUsr...
007500100827     D                 PR            10I 0
007600100827     D  pInSUN                        9A   CONST
007700100827     D  pOutEsito                     1A   OPTIONS(*NOPASS)
007800100827     D*

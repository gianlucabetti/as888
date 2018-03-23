000100110616     P*--------------------------------------------------
000200110616     P* Procedure name: BartMail_Insert
000300110616     P* Purpose:        Alimenta BartMail by-passando lo spool
000400110616     P* Returns:        Integer
000500110616     P* Parameter:      pInTags       => Elenco dei tags e dei relativi valori
000600110616     P* Parameter:      pInDMRABYPS   => DS di procedura
000700110616     P* Parameter:      pInTRTCM1DS   => DS di procedura
000800110620     P* Parameter:      pInLungh      => Lunghezza singolo "tag" (totale)
000900110616     P* Parameter:      pOutEsito     => Esito
001000110616     P*--------------------------------------------------
001100110617     D*
001200110617     D InDMRABYPS    E DS                  extname(DMRABYPS)
001300110617     D InTRTCM1DS    E DS                  extname(TRTCM1DS)
001400110616     D*
001500110616     D BartMail_Insert...
001600110616     D                 PR            10I 0
001700110616     D  pInTags                   65535A   CONST VARYING
001800110616     D  pInDMRABYPS                        CONST LIKEDS(InDMRABYPS)
001900110616     D  pInTRTCM1DS                              LIKEDS(InTRTCM1DS)
002000110620     D  pInLungh                      5S 0 CONST
002100110616     D  pOutEsito                    10I 0 OPTIONS(*NOPASS)

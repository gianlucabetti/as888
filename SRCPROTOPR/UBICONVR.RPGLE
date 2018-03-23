000100161021     P*--------------------------------------------------
000200161021     P* Procedure name: UBICONVR_CvtFromToCCSID
000300161021     P* Purpose:        Converte dati DA/A CCSID
000400161021     P* Returns:        Integer (Status_Code)
000500161021     P* Parameter:      iICONVinput     => Dati da convertire
000600161021     P* Parameter:      iICONVfromCCSID => CCSID origine
000700161021     P* Parameter:      iICONVtoCCSID   => CCSID destinazione
000800161021     P* Parameter:      oICONVoutput    => Dati convertiti
000900161021     P*--------------------------------------------------
001000161021     D UBICONVR_CvtFromToCCSID...
001100161024     D                 PR            10I 0 EXTPROC('UBICONVR')
001200161021     DiICONVinput                 65535A   VARYING OPTIONS(*VARSIZE) CONST
001300161021     DiICONVfromCCSID                10I 0 CONST
001400161021     DiICONVtoCCSID                  10I 0 CONST
001500161021     DoICONVoutput                65535A   VARYING OPTIONS(*VARSIZE)
001600161018     D*

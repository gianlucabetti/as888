000100161018     P*--------------------------------------------------
000200161020     P* Procedure name: UBBASE64R_Encode_Binary
000300161020     P* Purpose:        Converte dati in BASE64
000400161020     P* Returns:        Integer (Status_Code)
000500161020     P* Parameter:      oBASE64enc_dta  => Dati convertiti (encodati)
000600161020     P* Parameter:      oBASE64enc_len  => Lunghezza dati convertiti (encodati)
000700161020     P* Parameter:      iBASE64src_dta  => Dati da convertire
000800161020     P* Parameter:      iBASE64src_len  => Lunghezza dati da convertire
000900161018     P*--------------------------------------------------
001000161019     D UBBASE64R_Encode_Binary...
001100161021     D                 PR            10I 0
001200161019     DoBASE64enc_dta              65535A   VARYING OPTIONS(*VARSIZE)
001300161020     DoBASE64enc_len                 10I 0
001400161030     DiBASE64src_dta              65535A   CONST VARYING OPTIONS(*VARSIZE)
001500161021     DiBASE64src_len                 10I 0 CONST
001600161018     D*
001700161019     P*--------------------------------------------------
001800161019     P* Procedure name: UBBASE64R_Encode
001900161019     P* Purpose:        Converte dati in BASE64
002000161019     P* Returns:        Integer (Status_Code)
002100161019     P* Parameter:      oBASE64enc_dta  => Dati convertiti (encodati)
002200161020     P* Parameter:      oBASE64enc_len  => Lunghezza dati convertiti (encodati)
002300161019     P* Parameter:      iBASE64src_ptr  => Puntatore dati da convertire
002400161020     P* Parameter:      iBASE64src_len  => Lunghezza dati da convertire
002500161019     P*--------------------------------------------------
002600161019     D UBBASE64R_Encode...
002700161021     D                 PR            10I 0
002800161019     DoBASE64enc_dta              65535A   VARYING OPTIONS(*VARSIZE)
002900161020     DoBASE64enc_len                 10I 0
003000161019     DiBASE64src_ptr                   *   VALUE OPTIONS(*STRING)
003100161021     DiBASE64src_len                 10I 0 CONST
003200161019     D*
003300161019     P*--------------------------------------------------
003400161019     P* Procedure name: UBBASE64R_Decode
003500170516     P* Purpose:        Converte dati da BASE64 fino a 64K
003600161019     P* Returns:        Integer (Status_Code)
003700161019     P* Parameter:      oBASE64dec_dta  => Dati convertiti (decodati)
003800161019     P* Parameter:      iBASE64src_ptr  => Puntatore dati da convertire
003900161020     P* Parameter:      oBASE64dec_len  => Lunghezza dati convertiti (decodati)
004000161019     P*--------------------------------------------------
004100161019     D UBBASE64R_Decode...
004200161021     D                 PR            10I 0
004300170516     DoBASE64dec_dta              65535A   OPTIONS(*VARSIZE)
004400161019     DiBASE64src_ptr                   *   VALUE OPTIONS(*STRING)
004500161020     DoBASE64dec_len                 10I 0
004600161019     D*
004700170516     P*--------------------------------------------------
004800170517     P* Procedure name: UBBASE64R_EncodeLarge
004900170517     P* Purpose:        Converte dati in BASE64 anche oltre i 64K
005000170517     P* Returns:        Lunghezza dati encodati (se valore > oBASE64b64_siz possibile troncamento)
005100170517     P* Parameter:      iBASE64bin_ptr  => Puntatore a dati da encodare in Base64
005200170517     P* Parameter:      iBASE64bin_len  => Lunghezza dati da convertire
005300170517     P* Parameter:      oBASE64b64_ptr  => Puntatore a dati di output (encodati)
005400170517     P* Parameter:      oBASE64b64_siz  => Size are di memoria puntatore precenete
005500170516     P*--------------------------------------------------
005600170517     D UBBASE64R_EncodeLarge...
005700170517     D                 PR            10U 0
005800170517     DiBASE64bin_ptr                   *   VALUE
005900170517     DiBASE64bin_len                 10U 0 VALUE
006000170517     DoBASE64b64_ptr                   *   VALUE
006100170517     DoBASE64b64_siz                 10U 0 VALUE
006200170516     D*
006300170517     P*--------------------------------------------------
006400170517     P* Procedure name: UBBASE64R_DecodeLarge
006500170517     P* Purpose:        Converte dati da BASE64 a Binary anche oltre i 64K
006600170517     P* Returns:        Lunghezza dati decodati (se valore > oBASE64bin_siz possibile troncamento)
006700170517     P* Parameter:      iBASE64b64_ptr  => Puntatore a dati in Base64 da decodare
006800170517     P* Parameter:      iBASE64b64_len  => Lunghezza dati in Base64
006900170517     P* Parameter:      oBASE64bin_ptr  => Puntatore a dati di output (decodati)
007000170517     P* Parameter:      oBASE64bin_siz  => Size are di memoria puntatore precenete
007100170517     P*--------------------------------------------------
007200170517     D UBBASE64R_DecodeLarge...
007300170517     D                 PR            10U 0
007400170517     DiBASE64b64_ptr                   *   VALUE
007500170517     DiBASE64b64_len                 10U 0 VALUE
007600170517     DoBASE64bin_ptr                   *   VALUE
007700170517     DoBASE64bin_siz                 10U 0 VALUE
007800170517     D*

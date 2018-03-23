000100080612     P*--------------------------------------------------
000200111221     P* Procedure name: UBFMTDATE_Convert
000300111222     P* Purpose:        Questo modulo fornisce la funzionalità per manipolare una data in qualsiasi
000400111222     P*                 formato e restituirla in un formato diverso.
000500111222     P* Returns:        Alfanumerico di 64 contenente la data nel formato richiesto
000600111222     P* Parameter:      pInNumDate      => Data di partenza
000700111221     P*                 pInMaskNumDate  => Formato della data di partenza
000800111221     P*                 pInMaskStrDate  => Formato della data di arrivo
000900120103     P*                 pInFormat       => Codice richiesta
001000080612     P*--------------------------------------------------
001100080612     D*
001200111221     D UBFMTDATE_Convert...
001300120102     D                 PR           100A
001400111227     D pInDateIn                  32767A   CONST VARYING OPTIONS(*VARSIZE)
001500111227     D pInMaskDateIn              32767A   CONST VARYING OPTIONS(*VARSIZE)
001600111227     D pInMaskDateOut             32767A   CONST VARYING OPTIONS(*VARSIZE)
001700120103     D pInFormat                      2A   CONST OPTIONS(*OMIT:*NOPASS)
001800080612     D*

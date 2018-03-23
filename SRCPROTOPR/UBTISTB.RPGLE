000100080612     P*--------------------------------------------------
000200120110     P* Procedure name: UBTISTB_Ctrl_Shipment_Sent
000300120110     P* Purpose:        Controlla l'esistenza di almeno un rcd col flag trasmissione passato nella
000400120110     P*                 spedizione passata;
000500120110     P*                 se non viene passato il flag, si controlla con blank.
000600120110     P* Returns:        Alfanumerico di 1 contenente:
000700120110     P*                 '1' = trovato
000800120110     P*                 '0' = non trivato
000900120110     P*                 'E' = errore
001000120110     P* Parameter:      pInAnno         => Anno spedizione
001100120110     P*                 pInLineaPar     => Linea di partenza
001200120110     P*                 pInSerie        => Serie
001300120110     P*                 pInNumSped      => Numero spedizione
001400120110     P*                 pInFlagTrasm    => Flag trasmissione
001500080612     P*--------------------------------------------------
001600080612     D*
001700120110     D UBTISTB_Ctrl_Shipment_Sent...
001800120110     D                 PR             1
001900120110     D pInAnno                        4s 0 CONST
002000120110     D pInLineaPar                    3s 0 CONST
002100120110     D pInSerie                       2s 0 CONST
002200120110     D pInNumSped                     7s 0 CONST
002300120110     D pInFlagTrasm                   1A   CONST OPTIONS(*OMIT:*NOPASS)
002400080612     D*
002500120110     P*--------------------------------------------------
002600120110     P* Procedure name: UBTISTB_Upd_Shipment_Sent
002700120110     P* Purpose:        Aggiorna tutti i rcd su TISTB col flag trasmissione passato, per la
002800120110     P*                 spedizione passata;
002900120110     P*                 se non viene passato il flag, si imposta a blank.
003000120110     P* Returns:        Alfanumerico di 1 contenente:
003100120110     P*                 '1' = trovato
003200120110     P*                 '0' = non trivato
003300120110     P*                 'E' = errore
003400120110     P* Parameter:      pInAnno         => Anno spedizione
003500120110     P*                 pInLineaPar     => Linea di partenza
003600120110     P*                 pInSerie        => Serie
003700120110     P*                 pInNumSped      => Numero spedizione
003800120110     P*                 pInFlagTrasm    => Flag trasmissione
003900120110     P*--------------------------------------------------
004000120110     D*
004100120110     D UBTISTB_Upd_Shipment_Sent...
004200120110     D                 PR             1
004300120110     D pInAnno                        4s 0 CONST
004400120110     D pInLineaPar                    3s 0 CONST
004500120110     D pInSerie                       2s 0 CONST
004600120110     D pInNumSped                     7s 0 CONST
004700120110     D pInFlagTrasm                   1A   CONST OPTIONS(*OMIT:*NOPASS)
004800120110     D*

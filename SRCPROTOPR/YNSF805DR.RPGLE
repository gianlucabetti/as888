000100141015     D*--------------------------------------------------
000101141015     D* Procedure name: ChkImponibileFattura
000102141015     D* Purpose:        Controlla se il totale imponibile delle note accred...
000103141015     D*                          ito è maggiore dell'imponibile della fattura.
000104141015     D* Returns:        Esito.
000105141015     D* Parameter:      prmSocieta => ID società
000106141015     D* Parameter:      prmCtb => Contabilità
000107141015     D* Parameter:      prmKcc => Capo conto cliente
000108141015     D* Parameter:      prmScf => Sotto conto cliente fattura
000109141015     D* Parameter:      prmDataPartita => Data partita
000110141015     D* Parameter:      prmNumeroPartita => Numero partita.
000111141015     D* Parameter:      prmSeriePartita => Serie partita
000112141015     D* Parameter:      prmImponibileInserendo => Imponibile della n.a. ins...
000113141015     D*                          erenda.
000114141015     D*--------------------------------------------------
000115141015     D ChkImponibileFattura...
000116141015     D                 PR            10I 0
000117141015     D  prmSocieta                    3A
000118141015     D  prmCtb                        2A   VALUE
000119141015     D  prmKcc                        6A   VALUE
000120141015     D  prmScf                        8A
000121141015     D prmDataPartita                  D   DATFMT(*ISO)
000122141015     D  prmNumeroPartita...
000123141015     D                                9P 0
000124141015     D  prmSeriePartita...
000125141015     D                                4A
000126141024     D  prmImponibileNaInserenda...
000127141024     D                               15P 2 CONST
000128141024     D  prmImpostaNaInserenda...
000129141015     D                               15P 2 CONST
000130141029     D  prmSys                        3P 0 CONST
000131141029     D  prmNrAsReg                    9P 0 CONST

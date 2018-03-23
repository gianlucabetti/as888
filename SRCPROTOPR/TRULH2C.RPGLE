000100140318     D*--------------------------------------------------
000101140318     D* Program name: TrulH2C
000102140318     D* Purpose:        Converte i caratteri HTML nel corrispondente valore...
000103140318     D*                           EBCDIC.
000104140318     D* Returns:
000105140319     D* Parameter:      prmCodiceOperativo => 0 = da HTML a CHAR
000106140319     D*                                       1 = da CHAR a HTML
000107140319     D*                                     100 = chiusura (*INLR = *ON)
000108140319     D* Parameter:      prmStringaInput => Stringa di input.
000109140318     D* Parameter:      prmStringaInputSize => Dimensione stringa input.
000110140318     D* Parameter:      prmStringaOutput => Stringa di output.
000111140318     D* Parameter:      prmStringaOutputSize => Dimensione stringa output.
000112140318     D* Parameter:      prmEsito => Esito.
000113140318     D*--------------------------------------------------
000114140318     D TrulH2C         PR                  EXTPGM('TRULH2C')
000115140320     D  prmCodiceOperativo...
000116140320     D                               10I 0 CONST
000117140320     D  prmEsito...
000118140320     D                               10I 0 OPTIONS(*NOPASS)
000119140320     D  prmStringaInput...
000120140320     D                            32767A   OPTIONS(*VARSIZE:*NOPASS)
000121140320     D  prmStringaInputSize...
000122140320     D                               10I 0 CONST OPTIONS(*NOPASS)
000123140320     D  prmStringaOutput...
000124140320     D                            32767A   OPTIONS(*VARSIZE:*NOPASS)
000125140320     D  prmStringaOutputSize...
000126140320     D                               10I 0 CONST OPTIONS(*NOPASS)

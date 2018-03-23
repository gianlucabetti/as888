000100091218     D*--------------------------------------------------
000200091218     D* Procedure name: GetContratto
000300091218     D* Purpose:        Reperisce anagrafica contratto.
000400091218     D* Returns:
000500091218     D*--------------------------------------------------
000600091218     D GetContratto    PR                  EXTPGM('YCOYNFEAR')
000700091218     D  tipoFornitore...
000800091218     D                                     LIKE(ynfea00f.feaTpFor)
000900091218     D                                     CONST
001000091218     D  idContratto...
001100091218     D                                     LIKE(ynfea00f.feaContra)
001200091218     D                                     CONST
001300091218     D  dataDocumento...
001400091218     D                                     LIKE(ynfea00f.feaDtInCo)
001500091218     D  esito...
001600091218     D                                1A
001700091218     D  ynfea00f...
001800091218     D                                     LIKEDS(ynfea00f)
001801160323     D  ynfea00fSize                 10I 0 CONST OPTIONS(*NOPASS)

000100130307     D TIVLRds       e ds                  extname(TIVLR00F)
000200130307     D FIVABds       e ds                  extname(FIVAB00F)
000300130307       // - Parametri
000400130403     d pIn_FolderPDF   s            256a
000500130403     d pIn_NamePDF     s            256a
000600130403       // '0'=OK '1'=errore
000700130403     d pOut_Esito      s              1A
000800130307
000900130403     c     1             do        3
001000130403     c                   eval       pIn_FolderPDF = 'FOLDER'
001100130403     c                   eval       pIn_NamePDF = 'NAME'
001200130403     c                   call      'TIS7P2SR3'
001300130403     c                   parm                    pIn_FolderPDF
001400130403     c                   parm                    pIn_NamePDF
001500130403     c                   parm                    pOut_Esito
001600130403     c                   enddo
001700130307
001800130307     c                   seton                                        lr

000100021017     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000400960124      *--------------------------------------------------------------*
000500101029     fungio01l  if   e           k Disk
000501101029     funana02l  uf   e           k Disk
005600970227     C**
005700101029     C*
005701101029     c     kana          klist
005702101029     c                   kfld                    ungtir
005703101029     c                   kfld                    ungmatr
005704101029     c
005800101029     C     *loval        SETll     ungio01l
005801101029     C                   read      ungio01l
005802101029     c
005803101029     c                   dow       not %eof(ungio01l)
005804101029     c                   if        ungtir>0 and ungmatr<>*blanks and ungfmv='S'
005805101029     c
005806101029     c     kana          chain     unana02l
005807101029     c                   if        %found(unana02l)
005808101029     c                   eval      unadtr=ungdtm
005809101029     c                   update    unana000
005810101029     c                   endif
005811101029     c                   endif
005812101029     c
005813101029     C                   read      ungio01l
005814101029     c                   enddo
009500960124     C                   SETON                                        RT
010100960124     C*

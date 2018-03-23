001700091215     Faiats01l  UF   E           k DISK    commit
002100091215     Faiatm01l  UF   E           k DISK    commit
002500091215     Faiata01l  UF   E           k DISK    commit
004000000000      *
004001091215     c     kats          klist
004002091215     c                   kfld                    atssoc
004003091215     c                   kfld                    atsksc
004100000000      *****************************************************************
004103091215     C     248           SETLL     aiats01l
004104091215     c                   do        *hival
004105091215     C     248           READe     aiats01L
004106091215     c                   if        %eof(aiats01l)
004107091215     c                   leave
004108091215     c                   end
004109091215     C                   IF        ATSDTFIC <> 0
004111091215     C                   iter
004112090114     C                   ENDIF
004200000000      *
004400091215     C     kats          SETLL     aiatm01L
004401091215     c                   do        *hival
004402091215     C     kats          reade     aiatm01L
004403091215     c                   if        %eof(aiatm01l)
004404091215     c                   leave
004405091215     c                   end
004406091215     c                   eval      atmsoc = 260
004407091215     c                   update    aiatm000
005700000000     C                   ENDDO
005800000000      *
005802091215     C     kats          SETLL     aiata01L
005803091215     c                   do        *hival
005804091215     C     kats          reade     aiata01L
005805091215     c                   if        %eof(aiata01l)
005806091215     c                   leave
005807091215     c                   end
005808091215     c                   eval      atasoc = 260
005809091215     c                   update    aiata000
005810091215     C                   ENDDO
005811091215      *
005812091215     c                   eval      atssoc = 260
005813091215     c                   update    aiats000
008902090706     c                   commit
008903091215     C                   ENDDO
008910090624      *
009100000000     C                   SETON                                        LR

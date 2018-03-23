000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200070104?     * FNLSA2R8 ----------------------------------------------------*
000300070104      *        Richiamo pgm lancio lsa2 per fogli viaggio 9999
000400971121?     *--------------------------------------------------------------*
000500070104     d kpjba         e ds
000600070104     D dataiso         S               d   datfmt(*iso)
000700070104     c     *entry        plist
000800070104     c                   parm                    kpjba
000900070104     C* Lancio azione "LSR9" solo nel mese di gennaio
001000070104     c                   time                    dataiso
001100070104     c                   if        %subdt(dataiso:*months)=1
001200070104     c                   call      'FNLSA2R9'
001300070104     c                   parm                    kpjba
001400070104     c                   endif
001500070104     c*
001600070104     c                   seton                                        lr

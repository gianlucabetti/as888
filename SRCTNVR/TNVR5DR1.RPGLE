000100020819     H/TITLE  Lancio trascodifica AZUTE00F x profili in sede
000200020819     H DEBUG DECEDIT('0,') DATEDIT(*ymd.)
000400021105     Fazorg01l  IF   E           K DISK
001500020819     D ut§dse0f      E DS
001600020829     D cncr80        E DS
001601020829     D og143         E DS
001602021107     D comfel          s              3
001700951009?     *--------------------------------------------------------------*
001800951009?     *  CICLO PRINCIPALE                                            *
001801020903?     *  questo programma deve girare in GAITRA201                   *
001900951009?     *--------------------------------------------------------------*
001901021107     c                   move      simfel        comfel
001902020829     c                   call      'TNVR5DC'
001903021107     c                   parm                    comfel
005000020820     c* reperisco la SDI
005001021105     c                   clear                   orgfil
005002021105     c     orgfil        setll     azorg01l
005003021105     c                   do        *hival
005004021105     c                   read      azorg01l
005005021105     c                   if        %eof(azorg01l)
005006021105     c                   leave
005007021105     c                   end
005008021105     c                   if        orgfel <> simfel
005009021105     c                   iter
005010021105     c                   end
005011021105     c                   if        %subst(orgdes: 1: 1) = '*'
005012021105     c                   iter
005013021105     c                   end
005014021105     c                   if        orgdit <> 'SDI'
005015021105     c                   iter
005016021105     c                   end
005017021105     c                   movel     orgde3        og143
005018021105     c                   if        §ogas <> simfel
005019021107     c                   move      orgfil        comfel
005602020829     c                   call      'TNVR5DC'
005603021107     c                   parm                    comfel
005604020902     c                   leave
005605020902     c                   end
005700020902     c                   enddo
005701020829     c*
005702020829     c                   seton                                        lr
008500951011?     *--------------------------------------------------------------*
008600951011?     *  *INZSR: Operazioni di inizializzazione dati                 *
008700951011?     *--------------------------------------------------------------*
008800951011     C     *INZSR        BEGSR
008900951011      *
009200020702     C                   Z-ADD     1             CODUT
009300020702     C                   CALL      'X§PARUT'
009400020702     C                   PARM                    UT§DSE0F
009401020829     c                   movel     rec80         cncr80
009402020902     c                   if        simfel = 0
009403020902     c                   z-add     046           simfel
009404020902     c                   end
011100951012     C                   ENDSR

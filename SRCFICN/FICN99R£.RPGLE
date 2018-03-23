000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200900606?     *--------------------------------------------------------------*
000300020507      * FICN56R - reperisce OUTQ in base all'unità di registrazione  *
000400020423      *           ricevo l'unità di registrazione e mi compongo OUTQ *
000500900606?     *--------------------------------------------------------------*
000700020423     D com3            s              3
000800020423     D uni             s              8
000900020423     D outq            s             10
000910021007
000911021008     D PrmFine         S              1    inz('L')
000913021008     D PrmTpOutQ       S              1    inz('0')
000915021007     D PrmFilS         S              3S 0
000916021007     D PrmOutQ         S             10
000917021007     D PrmOutQLib      S             10
000918021007     D PrmEsito        S              1
001100020423     C     *ENTRY        PLIST                                                  *
001200020423     C                   PARM                    uni                            *
001300020423     C                   PARM                    outq                           *
001400020423     c                   clear                   outq
001401021007     C                   eval      com3 = %subst(uni: 1: 3)
001402021007     c                   move      com3          prmfils
001403021007     ** Reperisco la coda delle stampe amministrative.
001404021007     C                   CALL      'TRULOUTQ'
001405021007     ** Parametri di input.
001406021007     C                   PARM                    PrmFine
001407021007     C                   PARM                    PrmFilS
001408021007     C                   PARM                    PrmTpOutQ
001409021007     ** Parametri di output.
001410021008     C                   PARM      *BLANK        PrmOutQ
001411021008     C                   PARM      *BLANK        PrmOutQLib
001412021007     C                   PARM      *BLANK        PrmEsito
001413021007     ** Se il reperimento della coda non è riuscito, creo lo spool in QDKT.
001414021008     C                   IF        PrmEsito = '1' or prmoutq = ' '
001415021008     C                   EVAL      outq = 'QDKT'
001417021007     c                   else
001418021008     c                   eval      outq = prmoutq
001419021007     C                   ENDIF
001500020423     C*
003200020423     c                   EVAL      *INLR = *ON

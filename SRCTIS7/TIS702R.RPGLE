000200990908     H dftactgrp(*yes)
000300000313     F*
000500990910     Ftivin00r  uF   E             DISK    usropn
000501040226
000503040226     D TxtInOut        S           2048
000504040226     D ElencoChar      S            256
000505040226     D TipoElenco      S              1
000506040226     D CharSost        S              1
000507040226     D UpperCase       S              1
000508040226     D ChkNull         S              1
000509040226     D CharNull        S              1
000510040226     D Esito           S              1
004100010330
004300010330
004900010601     C*
004902040226     C                   if        not %open(tivin00r)
004903040226     C                   open      tivin00r
004904040226     C                   endif
004905040226     C*
004906040226     C* Scorro tutti i record del membro corrente d TIVIN...
004907040226     C* ...e "bonifico" i dati da caratteri indesiderati
004908040226     C                   DO        *HIVAL
004909040226     C*
004910040226     C                   READ      tivin00r                               70
004911040226     C*
004912040226     C* Imposto i parametri x il *pgm d "bonifica"
004913040226     C                   EVAL      TxtInOut = vinDTA
004914040226     C                   EVAL      ElencoChar = *blanks
004915040226     C                   EVAL      TipoElenco = *blanks
004916040226     C                   EVAL      CharSost   = *blanks
004917040226     C                   EVAL      UpperCase  = '1'
004918040226     C                   EVAL      ChkNull    = '1'
004919040226     C                   EVAL      CharNull   = *blanks
004920040226     C                   EVAL      Esito      = *blanks
004921040226     C*
004922040226     C* Effettuo la chiamata al *pgm d "bonifica"
004923040226     C                   CALL      'XCHKCHAR'
004924040226     C                   PARM                    TxtInOut
004925040226     C                   PARM                    ElencoChar
004926040226     C                   PARM                    TipoElenco
004927040226     C                   PARM                    CharSost
004928040226     C                   PARM                    UpperCase
004929040226     C                   PARM                    ChkNull
004930040226     C                   PARM                    CharNull
004931040226     C                   PARM                    Esito
004932040226     C*
004933040226     C* Re-imposto il campo dati d TIVIN con la stringa così "bonificata"
004934040226     C                   EVAL      vinDTA = TxtInOut
004935040226     C*
004936040226     C  N70              UPDATE    tivin000
004937040226     C*
004938040226     C  N70              ENDDO
004939040226     C*
004940040226     C                   if        %open(tivin00r)
004941040226     C                   close     tivin00r
004942040226     C                   endif
005200010601     C*
005300040226     C                   SETON                                        LR
005400010601
005500010601
063700010330
063800000613     C     *inzsr        BEGSR
063900990910     C*
065500000613     C*
065600000613     C                   ENDSR

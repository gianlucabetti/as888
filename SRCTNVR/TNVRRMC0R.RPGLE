000100980521      ****************************************************************
000200150211      *  Elenco località presenti solo su CAP con viario
000300981008      ****************************************************************
000400990603      * 01 -
000500990317      ****************************************************************
000600980521     H DECEDIT('0,') DATEDIT(*DMY.)
000700980623
000800990602      *------------------------------------------------------------------------*
000901150211     Fazcpc01l  IF   E           K DISK
000902150211     Fazcpl01l  IF   E           K DISK
000903150211     Fwflvi00f  o  a E             DISK
001000980521      *------------------------------------------------------------------------*
001100980702
001200990602      * Definizione campi di work ------------------------------------------
001201150211     D kver            S                   LIKE(cpcver) inz(45)
001900990602      * Schiere ------------------------------------------------------------
002300990602      * DS esterne ---------------------------------------------------------
002301150211     D TISI95DS      E DS                  INZ
003700990602     D KPJBA         E DS
003800990602      * DS interne ---------------------------------------------------------
004800990602
004900980525      *
004901150211
004902150211     C     kver          setll     azcpc01l
004903150211     c     kver          reade     azcpc01l
004904150211     c                   dow       not %eof(azcpc01l)
004905150211     c* considero solo se cap con viario
004906150211     c                   if        cpcflg='S'
004907150211     c     kloc          setll     azcpl01l
004908150211     c     kloc          reade     azcpl01l
004909150211     c                   dow       not %eof(azcpl01l)
004910150211     c* per la località letta verifico se presente su altro CAP
004911150211     c                   clear                   tisi95ds
004912150211     C                   MOVEL     *blanks       I95TLA                         *tipo lancio
004913150211     C                   MOVEL     '7'           I95TCN                         *tipo controllo
004914150211     C                   MOVEL     cplNAr        I95NAR                         *nazione
004916150211     C                   MOVEL     cplLOC        I95LOC                         *località
004917150211     C                   MOVEL     cpcprv        I95PRV                         *provincia
004919150211     C*
004920150211     C                   CALL      'TISI95R'
004921150211     C                   PARM                    TISI95DS
004922150211     c                   if        o95err='B' or o95flg='S'
004924150211     c                   eval      lvinar=cplnar
004925150211     c                   eval      lvicap=cplcap
004927150211     c                   eval      lviprv=cpcprv
004928150211     c                   eval      lviloc=cplloc
004929150211     c                   write     wflvi000
004930150211     c                   endif
004931150211     c     kloc          reade     azcpl01l
004932150211     c                   enddo
004933150211     c                   endif
004934150211     c     kver          reade     azcpc01l
004935150211     c                   enddo
007300990601
007301150211     C                   CLEAR                   TISI95DS                       *azzera i parametri
007302150211     C                   MOVEL     'C'           I95TLA                         *tipo lancio
007304150211     C                   CALL      'TISI95R'
007305150211     C                   PARM                    TISI95DS
007306150211
011400990603     c                   seton                                        lr
011500990602
011600990603     c**********************************************************************
011700990603     c     *inzsr        begsr
011800990603
011900990603     C     *entry        plist
012000990603     C                   parm                    kpjba
012100990603
012101150211     C     kloc          klist
012102150211     C                   kfld                    cpcver
012103150211     C                   kfld                    cpcnar
012104150211     C                   kfld                    cpccap
016200990603
016300990603     c                   endsr
016400990603

000100970117     H*------------------------------------------------------------------------*
000200091103     H* restituisce campi del file TNVIS/TIVIS                                 *
000300970117     H*------------------------------------------------------------------------*
003400091103     FTNVIS05L  IF   E           K DISK    usropn prefix(£)
003401100420     FTIVIS05L  IF   E           K DISK
009400091103     D kpjba         E DS                                                       *Profili utente
009401091103     D TNTE04DS      E DS                                                       *Profili utente
292000970117     C*--------------------
292100970117     C* RICEVIMENTO PARAMETRI
292200970117     C*--------------------
292300970117     C     *ENTRY        PLIST
292400970117     C                   PARM                    KPJBA
292401091126     C                   PARM                    tnte04ds
292402091103     c*
292502091103     c                   clear                   d04ksc
292503091103     c                   clear                   d04cmm
292504091103     c                   clear                   d04rag
292505091103     c                   clear                   d04err
292506091103     c*
293017091103     C     d04nrv        CHAIN     TIVIS05L                                     *VISITE
293018091103IF  1C                   IF        %found
293019091123     C                   eval      d04ksc = visksc
293020091103     C                   eval      d04cmm = viscmm
293021091103     C                   eval      d04rag = visrag
293022100420     c                   else
293023100420     c                   if        d04dsf = ' '
293024100420     c                   open      tnvis05l
293025100420     C     d04nrv        CHAIN     TNVIS05L                                     *VISITE
293026100420IF  1C                   IF        %found
293027100420     C                   eval      d04ksc = £visksc
293028100420     C                   eval      d04cmm = £viscmm
293029100420     C                   eval      d04rag = £visrag
293030100420     c                   end
293031091103     c                   end
293032100420     c                   end
293033091103     c*
309502091103IF  1C                   IF        not %found
309503091103     c                   eval      d04err= '1'
309600970414E   1C                   ENDIF
309700091103     c                   seton                                        lr

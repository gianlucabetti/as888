000100970117     H*------------------------------------------------------------------------*
000200091103     H* restituisce campi del file TNVIS/TIVIS                                 *
000300970117     H*------------------------------------------------------------------------*
000400100420     FTIVIS05L  IF   E           K DISK
000500091103     D kpjba         E DS                                                       *Profili utente
000600091103     D TNTE04DS      E DS                                                       *Profili utente
000700970117     C*--------------------
000800970117     C* RICEVIMENTO PARAMETRI
000900970117     C*--------------------
001000970117     C     *ENTRY        PLIST
001100970117     C                   PARM                    KPJBA
001200091126     C                   PARM                    tnte04ds
001300091103     c*
001400091103     c                   clear                   d04ksc
001500091103     c                   clear                   d04cmm
001600091103     c                   clear                   d04rag
001700091103     c                   clear                   d04err
001800091103     c*
001900091103     C     d04nrv        CHAIN     TIVIS05L                                     *VISITE
002000091103IF  1C                   IF        %found
002100091123     C                   eval      d04ksc = visksc
002200091103     C                   eval      d04cmm = viscmm
002300091103     C                   eval      d04rag = visrag
002400110104     c**                 else
002500110104     c**                 if        d04dsf = ' '
002600110104     c**                 open      tnvis05l
002700110104     C**   d04nrv        CHAIN     TNVIS05L                                     *VISITE
002800110104IF  1C**                 IF        %found
002900110104     C**                 eval      d04ksc = £visksc
003000110104     C**                 eval      d04cmm = £viscmm
003100110104     C**                 eval      d04rag = £visrag
003200110104     c**                 end
003300110104     c**                 end
003400110104     c
003500100420     c                   end
003600091103     c*
003700091103IF  1C                   IF        not %found
003800091103     c                   eval      d04err= '1'
003900970414E   1C                   ENDIF
004000091103     c                   seton                                        lr

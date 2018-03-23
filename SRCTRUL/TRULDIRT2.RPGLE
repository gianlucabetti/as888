000100960926     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200980910     H*PARMS
000300960926     H DECEDIT('0,') DATEDIT(*DMY.)
000400161007      *---------------------------------------------------------------*
000500161012      *             tunnel per trukdirt
000600161007      *---------------------------------------------------------------*
000700161007     D kpjba         E DS                  QUALIFIED
000800161007
000801161007          *inlr = *on;
000802161007
000803161007     C     *ENTRY        PLIST
000804161007     C                   PARM                    kpjba
000805161007
000807161007     C                   call      'TIS7CALLR'
000808161007     C                   parm      '*LIBL'       lib              10
000809161012     C                   parm      'TRULDIRT'    obj              10
000810161007     C                   parm      kpjba         params        10240
000811161007     C                   parm      *blank        esito             1
000812161012
000814161007

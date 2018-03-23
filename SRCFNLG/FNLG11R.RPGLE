000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300051202      * ?_____________________________________________________________      */
000400051202      * COMPILARE CON
000500051202      *               OVRDBF FILE(TITAS30C) TOFILE(GAITRAGRPS/TITAS30C)
000600051202      * ?_____________________________________________________________      */
000700051202     ftitas30c  if   e           k disk    usropn
000800051202     d kpjba         e ds
000900051202     d fnlg11ds      e ds
001000051202     d cmd             s             50    dim(01) ctdata perrcd(1)
001100051202     d cmdp            s             50    dim(01) ctdata perrcd(1)
001200051202     d comman          s             80
001300051202     d lenght          s             15  5 inz(80)
001400051202      * ?_____________________________________________________________      */
001500051202      * ?sistema informativo filiale reale(SETRAS)                          */
001600051202     c                   if        knsif = 'FILTRA201'
001700051202     c                   Movel(p)  cmd(1)        comman
001800051202     c                   else
001900051202      * ?sistema informativo filiale prova(AS888)                           */
002000051202     c                   Movel(p)  cmdp(1)       comman
002100051202     c                   endif
002200051202     c                   eval      lenght = %len(%trim(comman))
002300051202     c                   Call      'QCMDEXC'                            99
002400051202     c                   Parm                    comman
002500051202     c                   Parm                    lenght
002600051202     c  n99              Open      Titas30c
002700051202
002800051202     c     kspe          chain     titas30c
002900051202     c                   if        %found(titas30c)
003000051202      *non fatturato
003100051202     c                   if        tasfic = *blank
003200051202     c                   move      'N'           o11fat
003300051202     c                   eval      o11des = 'NON FATTURATO'
003400051202     c                   else
003500051202      * fatturato
003600051202     c                   move      'S'           o11fat
003700051202     c                   eval      o11des = 'FATTURATO'
003800051202     c                   endif
003900051202     c                   else
004000051205      *non trovato rekord richiesto su titas
004100051202     c                   move      'E'           o11fat
004200051202     c                   eval      o11des = 'NON TROVATO'
004300051202     c                   endif
004400000607
004500051202     c                   movel     fnlg11ds      kpjbu
004600000607     c                   seton                                        lr
004700051202      * ?________________________________________                           */
004800051202     c     *inzsr        begsr
004900051202      * ?________________________________________                           */
005000051202     c     *entry        plist
005100051202     c                   parm                    kpjba
005200051202     c                   movel     kpjbu         fnlg11ds
005300051130      * originale
005400051130     c     kspe          klist
005500051202     c                   kfld                    i11aas
005600051202     c                   kfld                    i11lnp
005700051202     c                   kfld                    i11nrs
005800051202     c                   kfld                    i11nsp
005900051202     c                   kfld                    i11tbl
006000051202
006100051202     c                   endsr
006200051202      *________________________________________________________
006300051202** cmd
006400051202OVRDBF FILE(TITAS30C) TOFILE(GAITRAGRU/TITAS30C)
006500051202** cmdp
006600051202OVRDBF FILE(TITAS30C) TOFILE(GAITRAGRPS/TITAS30C)

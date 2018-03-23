000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300040831     ffictd01l  uf   e           k disk
000400040831     ffictt01l  uf   e           k disk
000500030414      *
000600040831     D  parpo          s              3s 0
000700040831     D  datadal        s              8s 0
000800040831     D  dataal         s              8s 0
000900040831     D  keypo          s                   like(cttfgs)
001000030109     d
001100000607      *--------------------------------------------------------------------
001200040831     c     *entry        plist
001300040831     c                   parm                    parpo
001400040831     c                   parm                    datadal
001500040831     c                   parm                    dataal
001600040831     c                   z-add     parpo         keypo
001700000607     c
001800040831     c     kctd          klist
001900040831     c                   kfld                    cttfgs
002000040831     c                   kfld                    cttddc
002100040831     c     keypo         comp      *zeros                                 90
002200040831      * verifico se le testate sono già state confermate
002300040831     c  n90keypo         setll     fictt01l
002400040831     c   90*loval        setll     fictt01l
002500040831     c                   do        *hival
002600040831     c   90              read      fictt01l
002700040831     c  n90keypo         reade     fictt01l
002800040831     c                   if        %eof(fictt01l)
002900040831     c                   leave
003000040831     c                   endif
003100050407      *elimino tutte le righe senza tipo servizio
003200050407     c                   if        ctttsr = *blank
003300050407     c                   delete    fictt000
003400050407     c                   iter
003500050407     c                   endif
003600040831      *verifico il range di date
003700040831     c                   if        cttddc < datadal or cttddc > dataal
003800040903     c                             or ctttsr <> 'X'
003900040831     c                   iter
004000040831     c                   endif
004100040831      *dettaglio
004200040831     c     kctd          setll     fictd01l
004300040831     c                   do        *hival
004400040831     c     kctd          reade     fictd01l
004500040831     c                   if        %eof(fictd01l)
004600040831     c                   leave
004700040831     c                   endif
004800040831      * se confermata azzera il campo per il conto economico dinamico
004900040831      * altrimenti cancella il record
005000040831     c                   if        cttfvl = 'C'
005100040831     c                   z-add     0             ctdied
005200040831     c                   update    fictd000
005300040831     c                   else
005400040831     c                   delete    fictd000
005500040831     c                   endif
005600040831     c
005700040831     c                   enddo
005800040831     c                   if        cttfvl = 'C'
005900040831     c                   z-add     0             cttied
006000040831     c                   update    fictt000
006100040831     c                   else
006200040831     c                   delete    fictt000
006300040831     c                   endif
006400040831     c                   enddo
006500020517
006600020521     c                   seton                                        lr
006700030414      *-------------------------------------------------

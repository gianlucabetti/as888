000100020415     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200020603     Fqsysprt   o    f  132        printer
000300020603     Ffnar401l  IF   E           K DISK
000400020603      *  lettura FNAR401l
000500020603     c                   z-add     2002          ar4aas
000600020603     c     ar4aas        setll     fnar401l
000700020603     c                   do        *hival
000800020603     c     ar4aas        reade     fnar401l
000900020603     c                   if        %eof(fnar401l)
001000020603     c                   leave
001100020603     c                   end
001200020603     c                   if        ar4trc <> 'A'
001300020603     c                   iter
001400020603     c                   end
001500020603     c                   if        %subst(ar4not: 1: 2) = 'Y '
001600020603     c                   except    riga
001700020603     c                   end
001800020603     c                   enddo
001900020603     c                   seton                                        lr
002000020603     OQSYSPRT   E            riga        1
002100020603     o                                           +0 'Spedizione'
002200020603     o                       ar4aas              +1
002300020603     o                       ar4lnp              +1
002400020603     o                       ar4nrs              +1
002500020603     o                       ar4nsp              +1
002600020603     o                                           +1 'AR4NOT:'
002700020603     o                       ar4not              +1

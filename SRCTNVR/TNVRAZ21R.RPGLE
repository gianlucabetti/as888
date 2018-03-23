000100171128     h Decedit('0,') Datedit(*ymd.) option(*nodebugio)
000200171128      *-----------------------------------------------------------------*
000300171128      *             Aut. Visualizza Peso/Volume Desunto                 *
000400171128      *-----------------------------------------------------------------*
000500171128     fKpprf01l  if   e           K Disk
000600171128     fAzute01l  uf   e           k Disk
000700171128
000800171128     d dUTE01        e ds
000900171128
001300171128     c                   eval      *InLr = *On
001900171128
002000171128     c     *loval        setll     KPPRF01L
002100171128     c                   DO        *hival
002200171128     c                   read      KPPRF01L
002300171128      * fine file
002400171128     c                   IF        %eof(KPPRF01L)
002500171128     c                   leave
002600171128     c                   ENDIF
002800171128      * Aggiorno solo utenti EDP
003000171128     c                   IF        %subst(PRFnmu:1:3) = 'EDP'
003300171128
003400171128     c     PRFnmu        chain     AZUTE01L
003500171128     c                   IF        not %found(AZUTE01L)
003600171128     c                   iter
003700171128     c                   ENDIF
004300171128
004400171128      * scaduto non aggiorno
004500171128     c                   IF        UTEdsc <= *date
004600171128     c                   iter
004700171128     c                   ENDIF
004800171128
004900171128     c                   eval      dUTE01= UTEfaf
005000171128
005100171128      * solo se non già impostato
005200171128     c                   IF        §UTEdesun = *blanks
005300171128     c                   eval      §UTEdesun = 'S'
005400171128     c                   ENDIF
005800171128     c                   eval      UTEfaf = dUTE01
005900171128     c                   UPDATE    AZUTE000
006000171128     c                   ENDIF
006100171128
006200171128     c                   ENDDO

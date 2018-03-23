000100931029     H DECEDIT('0,') DATEDIT(*DMY.)
000200020529      *--------------------------------------------------------------*
000300040330      *         - esegue cpysplf normalizzato                        *
000400020529      *--------------------------------------------------------------*
000500030509     D trulfcds      E DS
000600030509      *--------------------------------------------------------------*
000700931102      *  CICLO PRINCIPALE:                                           *
000800931102      *--------------------------------------------------------------*
000900931104     C     *ENTRY        PLIST
001000030509     C                   PARM                    prtf             10
001100030509     C                   PARM                    file             10
001200030509     C                   PARM                    job              10
001300030509     C                   PARM                    usr              10
001400030509     C                   PARM                    nbr               6
001500030509     c*
001600030509     c                   clear                   trulfcds
001700030509     c                   eval      dfcfnam = prtf
001800030509     c                   eval      dfcjnam = job
001900030509     c                   eval      dfcunam = usr
002000030509     c                   eval      dfcjnum = nbr
002100030509     c                   eval      dfcfnum = '*LAST'
002200030509     c                   eval      dfcfout = file
002300040330     c                   eval      dfclout = 'LIBASP4'
002400030509     c                   eval      dfcfopt = 'A'
002500030509     c                   eval      dfcfolen = '200'
002600030509     c                   call      'TRULFCC'
002700030509     c                   parm                    trulfcds
002800030515     c* cpf3309= non trovato file spool non è un errore altrimenti sì
002900040330     c                   if        dfcerr = '1'
003000030509     c                   seton                                        h1
003100030509     c                   end
003200030509     c                   seton                                        lr

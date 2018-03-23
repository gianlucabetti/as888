000100931029     H DECEDIT('0,') DATEDIT(*DMY.)
000200020529      *--------------------------------------------------------------*
000300030509      * FICN72R *                                                    *
000400030509      *         - esegue cpysplf normalizzato                        *
000500020529      *--------------------------------------------------------------*
000600030509     D trulfcds      E DS
000700030509      *--------------------------------------------------------------*
000800931102      *  CICLO PRINCIPALE:                                           *
000900931102      *--------------------------------------------------------------*
001000931104     C     *ENTRY        PLIST
001100030509     C                   PARM                    prtf             10
001200030509     C                   PARM                    file             10
001300030509     C                   PARM                    job              10
001400030509     C                   PARM                    usr              10
001500030509     C                   PARM                    nbr               6
001600030509     c*
001700030509     c                   clear                   trulfcds
001800030509     c                   eval      dfcfnam = prtf
001900030509     c                   eval      dfcjnam = job
002000030509     c                   eval      dfcunam = usr
002100030509     c                   eval      dfcjnum = nbr
002200030509     c                   eval      dfcfnum = '*LAST'
002300030509     c                   eval      dfcfout = file
002400030509     c                   eval      dfclout = 'LIBASP5'
002500030509     c                   eval      dfcfopt = 'A'
002600030509     c                   eval      dfcfolen = '200'
002700030509     c                   call      'TRULFCC'
002800030509     c                   parm                    trulfcds
002900030515     c* cpf3309= non trovato file spool non è un errore altrimenti sì
003000030515     c                   if        dfcerr = '1' and
003100030515     c                             %subst(dfcmsg: 1: 7) <> 'CPF3309'
003200030509     c                   seton                                        h1
003300030509     c                   end
003400030509     c                   seton                                        lr

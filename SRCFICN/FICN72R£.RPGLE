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
002200030509     c                   eval      dfcfout = file
002300030509     c                   eval      dfclout = 'LIBASP5'
002400030509     c                   eval      dfcfopt = 'A'
002500030509     c                   eval      dfcfolen = '200'
002600030509     c                   call      'TRULFCC'
002700030509     c                   parm                    trulfcds
002800030509     c                   if        dfcerr = '1'
002900030509     c                   seton                                        h1
003000030509     c                   end
003100030509     c                   seton                                        lr

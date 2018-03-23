000100050502     FRM_triang1IF   E             DISK
000200050502     F                                     rename(RM_triang1:triang1f)
000300050502     FTITAA30C  IF   E           K DISK
000400050502     Ftnvrtrip  O    E             PRINTER oflind(*IN99)
000500050502     d TIBS69DS      E DS                  INZ
000600050502     d DS_cnaco      E DS                  extname(CNACO00F)
000700050502     d DS_cnind      E DS                  extname(CNIND00F)
000800050502     d DS_cnclp      E DS                  extname(CNCLP00F)
000900050502     d DS_fncls      E DS                  extname(FNCLS00F)
001000020607     c*
001100050502     c     ktaa          klist
001200050502     c                   kfld                    tasaas
001300050502     c                   kfld                    taslnp
001400050502     c                   kfld                    tasnrs
001500050502     c                   kfld                    tasnsp
001600050502     c                   kfld                    ktrc              1
001700050502     c                   movel     'M'           ktrc
001800050502     c* stampo intestazione
001900050502     c                   write     rcd001
002000050502     c                   setoff                                       99
002100020607     c*
002200050502     c                   do        *hival
002300050502     C                   read      rm_triang1
002400050502      * fine file
002500050502     c                   if        %eof(rm_triang1)
002600050502     c                   leave
002700050502     c                   endif
002800050502     c* decodifico tasccm
002900050502     c                   move      tasccm        w0040             4 0
003000050502     c                   clear                   pccmrsc
003100050502     c                   if        w0040=8888
003200050502     c     ktaa          chain     titaa30c
003300050502     c                   if        %found(titaa30c)
003400050502     c                   eval      pccmrsc = taarsc
003500050502     c                   endif
003600050502     c                   else
003700050502     C                   clear                   DS_cnaco
003800050502     C                   clear                   DS_cnind
003900050502     C                   clear                   DS_cnclp
004000050502     C                   clear                   DS_fncls
004100050502     c                   eval      I69kac = tasccm
004200050502     C                   CALL      'TIBS69R'
004300050502     C                   PARM                    tibs69DS
004400050502     C                   PARM                    DS_cnaco
004500050502     C                   PARM                    DS_cnind
004600050502     C                   PARM                    DS_cnclp
004700050502     C                   PARM                    DS_fncls
004800050502     c                   eval      pccmrsc = acorag
004900050502     c                   endif
005000050502     c* decodifico tasksc
005100050502     c                   clear                   pkscrsc
005200050502     C                   clear                   DS_cnaco
005300050502     C                   clear                   DS_cnind
005400050502     C                   clear                   DS_cnclp
005500050502     C                   clear                   DS_fncls
005600050502     c                   eval      I69kac = tasksc
005700050502     C                   CALL      'TIBS69R'
005800050502     C                   PARM                    tibs69DS
005900050502     C                   PARM                    DS_cnaco
006000050502     C                   PARM                    DS_cnind
006100050502     C                   PARM                    DS_cnclp
006200050502     C                   PARM                    DS_fncls
006300050502     c                   eval      pkscrsc = acorag
006400050502     c* decodifico csbdti
006500050502     c                   clear                   pdtirsc
006600050502     c                   move      csbcdi        w0040
006700050502     c                   if        csbcdi <> 9999
006800050502     C                   clear                   DS_cnaco
006900050502     C                   clear                   DS_cnind
007000050502     C                   clear                   DS_cnclp
007100050502     C                   clear                   DS_fncls
007200050502     c                   eval      I69kac = csbcdi
007300050502     C                   CALL      'TIBS69R'
007400050502     C                   PARM                    tibs69DS
007500050502     C                   PARM                    DS_cnaco
007600050502     C                   PARM                    DS_cnind
007700050502     C                   PARM                    DS_cnclp
007800050502     C                   PARM                    DS_fncls
007900050502     c                   eval      pdtirsc = acorag
008000050502     c                   endif
008100050502     c* Stampo riga
008200050502     c                   eval      pormrsr = ormrsr
008300050502     c                   eval      pormrsc = ormrsc
008400050502     c                   eval      pormrso = ormrso
008500050502     c                   eval      ptasrsd = tasrsd
008600050502     c                   if        *in99
008700050502     c                   write     rcd001
008800050502     c                   setoff                                       99
008900050502     c                   endif
009000050502     c                   write     rcd002
009100050502     c
009200020607     c*
009300020607     c*
009400020607     c                   enddo
009500020607     c*
009600020607     c                   seton                                        lr
009700050502     c*
009800050502     C                   eval      I69TLA  = 'C'
009900050502     C                   call      'TIBS69R'
010000050502     C                   parm                    TIBS69DS

000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300010404     frinom1d   cf   e             workstn
000400010404     ffileobjd  if   e             disk    usropn
000500010404     ffiledbrf  if   e             disk    usropn
000600010405     fqsysprt   o    f  132        printer infds(stamp)
000700000607     D kpjba         e ds
000800010404     d lib             s             10    dim(10)
000900010504     d nomfile         s             10    inz
001000010504     d nomfiler        s             10    inz
001100010404     d nomlib          s             10    inz
001200010404     d newfile         s             10    inz
001300010404     d newfile12       s             12    inz
001400010404     d lib7            s              7
001500010404     d const1          c                   const('GAITRA2')
001600010404     d const2          c                   const('SDITRA1')
001700010405     d stamp           ds
001800010405     d  lin                  367    368b 0
001900010405      *---------------------------------------------------
002000010404     ivideo1
002100010404     i              v1lib1                      lib(1)
002200010404     i              v1lib2                      lib(2)
002300010404     i              v1lib3                      lib(3)
002400010404     i              v1lib4                      lib(4)
002500010404     i              v1lib5                      lib(5)
002600010404     i              v1lib6                      lib(6)
002700010404     i              v1lib7                      lib(7)
002800010404     i              v1lib8                      lib(8)
002900010404     i              v1lib9                      lib(9)
003000010404     i              v1lib10                     lib(10)
003100000607      *--------------------------------------------------------------------
003200000607     c
003300000607     c     *entry        plist
003400000607     c                   parm                    kpjba
003500010404     c                   move      *blank        v1cnmf
003600010404     c                   moveL(P)  'GAITRA201'   lib(1)
003700010404     c                   moveL(P)  'SDITRA101'   lib(2)
003800010404     c
003900010405     c                   z-add     66            lin
004000000607
004100000607     c                   do        *hival
004200000607     c                   exfmt     video1
004300000607     c   kc              leave
004400010404     c                   if        *inkf and v1cnmf <> *blank
004500010404      *lancio programma per dspobjd su outfile del file richiesto
004600010404     c                   call      'RINOM1C'
004700010404     c                   parm      v1cnmf        nomfile
004800000607     c                   leave
004900000607     c                   endif
005000000607     c                   enddo
005100010404     C                   exsr      analisi
005200010404     c
005300000607     c                   seton                                        lr
005400010404      *----------------------------------------------------
005500010404     c     analisi       begsr
005600010404      *----------------------------------------------------
005700010404
005800010404      *lettura outfile
005900010404     c                   open      fileobjd
006000010404
006100010404     c                   do        *hival
006200010404     c                   read      fileobjd
006300010404     c                   if        %eof (fileobjd)
006400010404     c                   leave
006500010404     c                   end
006600010404     c                   movel     odlbnm        lib7
006700010404      *controllo ramo libreria che sia o gaitra2 o sditra1
006800010404      *
006900010404     c                   if        lib7 = const1 or lib7 = const2
007000010404      *controllo libreria che non sia fra quelle da escludere
007100010404     c     odlbnm        lookup    lib                                    99
007200010404     c   99              iter
007300010404     c                   call      'RINOM1C2'
007400010404     c                   parm                    nomfile
007500010404     c                   parm      odlbnm        nomlib
007600010404     c                   exsr      ridenomina
007700010404     c                   else
007800010404     c                   iter
007900010404     c                   endif
008000010404     c                   enddo
008100010404     c                   endsr
008200010404      *----------------------------------------------------
008300010404     c     ridenomina    begsr
008400010404      *----------------------------------------------------
008500010404
008600010404      *lettura outfile
008700010404     c                   open      filedbrf
008800010404
008900010404     c                   do        *hival
009000010404     c                   read      filedbrf
009100010404     c                   if        %eof (filedbrf)
009200010404     c                   leave
009300010404     c                   end
009400010404      *richiamo ridenominazione oggetti relazionati
009500010404      *
009600010404     c                   if        whrefi <> *blank
009700010404     c                   move      whrefi        newfile12
009800010404     c                   movel     'EX'          newfile12
009900010404     c                   movel     newfile12     newfile
010000010405     c                   call      'RINOM1C3'
010100010504     c                   parm      whrefi        nomfiler
010200010405     c                   parm      odlbnm        nomlib
010300010405     c                   parm                    newfile
010400010405     c                   if        lin > 60
010500010405     c                   except    testa
010600010405     c                   end
010700010405     c                   except    detta
010800010404     c                   else
010900010404     c                   iter
011000010404     c                   endif
011100010404     c                   enddo
011200010404      * cancello il file fisico/libreria
011300010404     c                   move      nomfile       newfile12
011400010404     c                   movel     'EX'          newfile12
011500010404     c                   movel     newfile12     newfile
011600010405     c                   call      'RINOM1C3'
011700010504     c                   parm      nomfile       nomfiler
011800010405     c                   parm      odlbnm        nomlib
011900010405     c                   parm                    newfile
012000010405     c                   if        lin > 60
012100010405     c                   except    testa
012200010405     c                   end
012300010405     c                   except    detta
012400010405     c                   except    dettaB
012500010404     c                   close     filedbrf
012600010404     c                   endsr
012700010405     O*------------------------------------------------------*
012800010405     OQSYSPRT   e            TESTA             2
012900010405     O                                           53 '***  STAMPA FILE RIDENOM'
013000010405     O                                              'INATI DAL PROGRAMMA     '
013100010405     O                                          111 'RINOM1R'
013200010405     O                                          127 'Pag.'
013300010405     O                       page               132
013400010405     O          e            TESTA       1
013500010405     O                       KNSIF               10
013600010405     O                       KNMUS               21
013700010405     O          e            TESTA       2
013800010405     O                                           20 'Nome file origine'
013900010405     O                                           40 'Nuovo Nome file'
014000010405     O                                           55 'Libreria '
014100010405     o          e            detta       1
014200010504     O                       nomfiler            18
014300010405     O                       newfile             35
014400010405     O                       nomlib              52
014500010405     o          e            dettab      0
014600010504     O                       nomfiler            18
014700010405     O                       newfile             35
014800010405     O                       nomlib              52
014900010405     o          e            dettab      0
015000010504     O                       nomfiler            18
015100010405     O                       newfile             35
015200010405     O                       nomlib              52

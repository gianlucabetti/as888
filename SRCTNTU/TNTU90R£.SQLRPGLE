000100051229      *PARMS CLOSQLCSR(*ENDMOD)
000200051229      *PARMS DYNUSRPRF(*OWNER)
000300051227      *-----------------------------------------------------------------*
000400060104      *  Leggo file reclami
000500051227      *-----------------------------------------------------------------*
000600051227     ftntud01l  uf a e           k disk
000700051229     fqsysprt   o    f  132        printer oflind(*inof) usropn
000800051227
000900051229     d conta           s                   like(tudqta)
001000051229     d dataalfa        s              6
001100051227     d dtaiso          s               d   inz
001200051229     d mult            s                   like(tudmult)
001300060104     d savdim          s                   like(tgddac)
001400060104     d savfgs          s                   like(tgdpot)
001500060104     d keytud          ds                  likerec(tntud000:*key)
001600051229     d wrkgetlista     s           4096    varying
001700051227
001800051229     d dtup          e ds
001900060104     d fitgdds       e ds                  extname(fitgd00f)
002000051227     d kpjba         e ds
002100051227     d tntu05ds      e ds
002200060104     dtntudtads      e ds
002300051227
002400051227      *-----------------------------------------------------------------*
002500051227      * Moltiplicatori calcolati:
002600060104      *    370 - telefonale di reclamo
002700051227      *-----------------------------------------------------------------*
002800051229
002900060104     c                   exsr      PREPASQL
003000060104     c
003100060104     c*  ?Aggiorno data ultima elaborazione in tabella TUP
003200060104     c                   if        t05mult=*blanks and t05fil=0
003300060104     c                   clear                   tntudtads
003400060104     c                   eval      §DTAazn = kcdaz
003500060104     c                   eval      §DTAdta = t05dtf
003600060104     c                   call      'TNTUDTAR'
003700060104     c                   parm                    tntudtads
003800060104     c                   endif
003900051229
004000051229     c                   Eval      *InLr = *On
004100051229
004200051229      *-----------------------------------------------------------------*
004300060104      * Lettura file fasi
004400051229      *-----------------------------------------------------------------*
004500060104     c     prepasql      begsr
004600051229
004700051229      *?Imposto la parte fissa dell'istruzione sql
004800051229     c                   eval      wrkgetlista =
004900060104     c                             'select tgdpot, tgddac, -
005000060104     c                             count(*) from fitgd00f where -
005100060104     c                             tgddac between '+
005200051229     c                             %editc(t05dti:'X') + ' and ' +
005300051229     c                             %editc(t05dtf:'X')
005400051229      *?Se passato un p.o.
005500051229     c                   if        t05fil <> 0
005600051229     c                   eval      wrkgetlista = wrkgetlista +
005700060104     c                             ' and digits(tgdpot) = ' +
005800051229     c                             %triml(%editc(t05fil:'Z'))
005900051229     c                   endif
006000051229      *?Ordinamento
006100051229     c                   eval      wrkgetlista = wrkgetlista +
006200060104     c                             ' group by tgdpot, tgddac'
006300051229      *?Solo lettura
006400051229     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
006500051229
006600051229     C/EXEC SQL
006700051229     C+ PREPARE s1 FROM :wrkgetlista
006800051229     C/END-EXEC
006900051229     C
007000051229     C/EXEC SQL
007100051229     C+ DECLARE A1 CURSOR FOR S1
007200051229     C/END-EXEC
007300051229     C
007400051229     C/EXEC SQL
007500051229     C+ OPEN a1
007600051229     C/END-EXEC
007700051229
007800051229     c                   clear                   conta
007900051229      *?Leggo il file
008000051229     c                   do        *hival
008100051229     C/EXEC SQL
008200060104     C+ FETCH NEXT FROM a1 INTO : savfgs, :savdim, :conta
008300051229     C/END-EXEC
008400051229
008500051229     c                   select
008600051229
008700051229     c                   when      sqlcod = 100
008800051229     c                   leave
008900051229     c                   when      sqlcod < 0
009000051229     c                   seton                                        H1
009100051229     c                   exsr      srfine
009200051229     c                   other
009300060104     c
009400060104     c                   eval      mult = '370'
009500060104     c
009600051229     c                   exsr      scrivitntu
009700060104     c
009800051229     c                   endsl
009900051229
010000051229     c                   enddo
010100051229
010200051229     C/EXEC SQL
010300051229     C+ CLOSE a1
010400051229     C/END-EXEC
010500051229
010600051229     c                   endsr
010700051227      *-----------------------------------------------------------------*
010800051228      * Scrivo/Aggiorno TNTUD
010900051227      *-----------------------------------------------------------------*
011000051227     c     scrivitntu    begsr
011100051229
011200051229      /free
011300051229
011400051229       //?imposto la data iso
011500051229       dataalfa = %subst(%char(savdim):3:6);
011600051229       dtaiso = %date(dataalfa:*ymd0);
011700051229
011800051229       //?imposto la chiave per tntud01l
011900051229       keytud.tudmult = mult;
012000051229       keytud.tudfil = savfgs;
012100051229       keytud.tuddta = dtaiso;
012200051229
012300051229       //?aggancio tntud01l
012400051229       chain %kds(keytud:3) tntud01l;
012500051229
012600051229       //?trovo rcd aggiorno se è un ricalcolo
012700051229       if %found(tntud01l);
012800051229       if t05mult <> *blanks or t05fil <> *zeros;
012900051229       tudqta = conta;
013000051229       update tntud000;
013100051229       else;
013200051229       if not%open(qsysprt);
013300051229       open qsysprt;
013400051229       except testa;
013500051229       endif;
013600051229       if *inof;
013700051229       except testa;
013800051229       endif;
013900051229       except riga;
014000051229       endif;
014100051229
014200051229       //?non trovo rcd scrivo
014300051229       else;
014400051229       clear tntud000;
014500051229       tudmult = mult;
014600051229       tuddta = dtaiso;
014700051229       tudfil = savfgs;
014800051229       tudcli = 0;
014900051229       tudqta = conta;
015000051229       write tntud000;
015100051229       endif;
015200051229
015300051229      /end-free
015400051227
015500051229     c                   endsr
015600051229
015700051229      *-----------------------------------------------------------------*
015800051229      * Fine programmma con codice errore
015900051229      *-----------------------------------------------------------------*
016000051229     c     srfine        begsr
016100051229
016200051229     c                   eval      *InLr = *On
016300051229     c                   return
016400051229
016500051229     c                   endsr
016600051227
016700051227      *-----------------------------------------------------------------*
016800051227      * Routine iniziale
016900051227      *-----------------------------------------------------------------*
017000051227     c     *inzsr        begsr
017100051227
017200051227     c     *entry        plist
017300051227     c                   parm                    kpjba
017400051227     c                   eval      tntu05ds = kpjbu
017500051227
017600051229     c                   endsr
017700051229
017800051227      *-----------------------------------------------------------------*
017900051229     oQsysprt   e            Testa          2 02
018000051229     o                                         +  5 'Rcd già esistente in'
018100051229     o                                         +  1 'TNTUD00F'
018200060104     o                                           85 'TNTU92R'
018300051229     o                       *Date         y    110
018400051229     o                                          128 'Pag.'
018500051229     o                       Page          z    132
018600051229     o          e            Riga           1
018700051229     o                                           +1 'Rcd:'
018800051229     o                       tudmult             +1
018900051229     o                       tudfil              +1
019000051229     o                       tuddta              +1
019100051229     o                       tudqta        4     +1

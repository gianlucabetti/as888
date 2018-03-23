000100051229      *PARMS CLOSQLCSR(*ENDMOD)
000200051229      *PARMS DYNUSRPRF(*OWNER)
000300051227      *-----------------------------------------------------------------*
000400060320      *  Leggo file reclami x data chiusura
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
002600060320      *    375 - imputazione reclami x data chiusura
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
004900060320     c                             'select tgdpor, tgddcd, -
005000060104     c                             count(*) from fitgd00f where -
005100060320     c                             tgddcd between '+
005200051229     c                             %editc(t05dti:'X') + ' and ' +
005300060320     c                             %editc(t05dtf:'X') + ' and ' +
005400060320     c                             'tgdpor>0'
005500051229      *?Se passato un p.o.
005600051229     c                   if        t05fil <> 0
005700051229     c                   eval      wrkgetlista = wrkgetlista +
005800060320     c                             ' and digits(tgdpor) = ' +
005900051229     c                             %triml(%editc(t05fil:'Z'))
006000051229     c                   endif
006100051229      *?Ordinamento
006200051229     c                   eval      wrkgetlista = wrkgetlista +
006300060320     c                             ' group by tgdpor, tgddcd'
006400051229      *?Solo lettura
006500051229     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
006600051229
006700051229     C/EXEC SQL
006800051229     C+ PREPARE s1 FROM :wrkgetlista
006900051229     C/END-EXEC
007000051229     C
007100051229     C/EXEC SQL
007200051229     C+ DECLARE A1 CURSOR FOR S1
007300051229     C/END-EXEC
007400051229     C
007500051229     C/EXEC SQL
007600051229     C+ OPEN a1
007700051229     C/END-EXEC
007800051229
007900051229     c                   clear                   conta
008000051229      *?Leggo il file
008100051229     c                   do        *hival
008200051229     C/EXEC SQL
008300060104     C+ FETCH NEXT FROM a1 INTO : savfgs, :savdim, :conta
008400051229     C/END-EXEC
008500051229
008600051229     c                   select
008700051229
008800051229     c                   when      sqlcod = 100
008900051229     c                   leave
009000051229     c                   when      sqlcod < 0
009100051229     c                   seton                                        H1
009200051229     c                   exsr      srfine
009300051229     c                   other
009400060104     c
009500060320     c                   eval      mult = '375'
009600060104     c
009700051229     c                   exsr      scrivitntu
009800060104     c
009900051229     c                   endsl
010000051229
010100051229     c                   enddo
010200051229
010300051229     C/EXEC SQL
010400051229     C+ CLOSE a1
010500051229     C/END-EXEC
010600051229
010700051229     c                   endsr
010800051227      *-----------------------------------------------------------------*
010900051228      * Scrivo/Aggiorno TNTUD
011000051227      *-----------------------------------------------------------------*
011100051227     c     scrivitntu    begsr
011200051229
011300051229      /free
011400051229
011500051229       //?imposto la data iso
011600051229       dataalfa = %subst(%char(savdim):3:6);
011700051229       dtaiso = %date(dataalfa:*ymd0);
011800051229
011900051229       //?imposto la chiave per tntud01l
012000051229       keytud.tudmult = mult;
012100051229       keytud.tudfil = savfgs;
012200051229       keytud.tuddta = dtaiso;
012300051229
012400051229       //?aggancio tntud01l
012500051229       chain %kds(keytud:3) tntud01l;
012600051229
012700051229       //?trovo rcd aggiorno se è un ricalcolo
012800051229       if %found(tntud01l);
012900051229       if t05mult <> *blanks or t05fil <> *zeros;
013000051229       tudqta = conta;
013100051229       update tntud000;
013200051229       else;
013300051229       if not%open(qsysprt);
013400051229       open qsysprt;
013500051229       except testa;
013600051229       endif;
013700051229       if *inof;
013800051229       except testa;
013900051229       endif;
014000051229       except riga;
014100051229       endif;
014200051229
014300051229       //?non trovo rcd scrivo
014400051229       else;
014500051229       clear tntud000;
014600051229       tudmult = mult;
014700051229       tuddta = dtaiso;
014800051229       tudfil = savfgs;
014900051229       tudcli = 0;
015000051229       tudqta = conta;
015100051229       write tntud000;
015200051229       endif;
015300051229
015400051229      /end-free
015500051227
015600051229     c                   endsr
015700051229
015800051229      *-----------------------------------------------------------------*
015900051229      * Fine programmma con codice errore
016000051229      *-----------------------------------------------------------------*
016100051229     c     srfine        begsr
016200051229
016300051229     c                   eval      *InLr = *On
016400051229     c                   return
016500051229
016600051229     c                   endsr
016700051227
016800051227      *-----------------------------------------------------------------*
016900051227      * Routine iniziale
017000051227      *-----------------------------------------------------------------*
017100051227     c     *inzsr        begsr
017200051227
017300051227     c     *entry        plist
017400051227     c                   parm                    kpjba
017500051227     c                   eval      tntu05ds = kpjbu
017600051227
017700051229     c                   endsr
017800051229
017900051227      *-----------------------------------------------------------------*
018000051229     oQsysprt   e            Testa          2 02
018100051229     o                                         +  5 'Rcd già esistente in'
018200051229     o                                         +  1 'TNTUD00F'
018300060320     o                                           85 'TNTU88R'
018400051229     o                       *Date         y    110
018500051229     o                                          128 'Pag.'
018600051229     o                       Page          z    132
018700051229     o          e            Riga           1
018800051229     o                                           +1 'Rcd:'
018900051229     o                       tudmult             +1
019000051229     o                       tudfil              +1
019100051229     o                       tuddta              +1
019200051229     o                       tudqta        4     +1

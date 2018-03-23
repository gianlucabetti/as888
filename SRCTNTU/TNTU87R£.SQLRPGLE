000100051229      *PARMS CLOSQLCSR(*ENDMOD)
000200051229      *PARMS DYNUSRPRF(*OWNER)
000300051227      *-----------------------------------------------------------------*
000400060104      *  Leggo file reclami
000500051227      *-----------------------------------------------------------------*
000600051227     ftntud01l  uf a e           k disk
000700051229     fqsysprt   o    f  132        printer oflind(*inof) usropn
000800051227
000900060322     d tgdconta        s                   like(tudqta)
001000060322     d conta           s                   like(tudqta)
001100060322     d containfo       s                   like(tudqta)
001200051229     d dataalfa        s              6
001300051227     d dtaiso          s               d   inz
001400051229     d mult            s                   like(tudmult)
001500060104     d savdim          s                   like(tgddac)
001600060104     d savfgs          s                   like(tgdpot)
001700060104     d keytud          ds                  likerec(tntud000:*key)
001800051229     d wrkgetlista     s           4096    varying
001900051227
002000051229     d dtup          e ds
002100060104     d fitgdds       e ds                  extname(fitgd00f)
002200051227     d kpjba         e ds
002300051227     d tntu05ds      e ds
002400060104     dtntudtads      e ds
002500051227
002600051227      *-----------------------------------------------------------------*
002700051227      * Moltiplicatori calcolati:
002800090925      *    374 - Richieste Info no contatti
002900051227      *-----------------------------------------------------------------*
003000051229
003100060104     c                   exsr      PREPASQL
003200060104     c
003300060104     c*  ?Aggiorno data ultima elaborazione in tabella TUP
003400060104     c                   if        t05mult=*blanks and t05fil=0
003500060104     c                   clear                   tntudtads
003600060104     c                   eval      §DTAazn = kcdaz
003700060104     c                   eval      §DTAdta = t05dtf
003800060104     c                   call      'TNTUDTAR'
003900060104     c                   parm                    tntudtads
004000060104     c                   endif
004100051229
004200051229     c                   Eval      *InLr = *On
004300051229
004400051229      *-----------------------------------------------------------------*
004500060104      * Lettura file fasi
004600051229      *-----------------------------------------------------------------*
004700060104     c     prepasql      begsr
004800051229
004900051229      *?Imposto la parte fissa dell'istruzione sql
005000051229     c                   eval      wrkgetlista =
005100060321     c                             'select tgdpot, tgddac, tgdcdt, -
005200060104     c                             count(*) from fitgd00f where -
005300060104     c                             tgddac between '+
005400051229     c                             %editc(t05dti:'X') + ' and ' +
005500090925     c                             %editc(t05dtf:'X') + ' and +
005600090925     c                             substr(tgdflo, 1, 1) =  ''' +
005700090925     c                             ''''
005800051229      *?Se passato un p.o.
005900051229     c                   if        t05fil <> 0
006000051229     c                   eval      wrkgetlista = wrkgetlista +
006100060104     c                             ' and digits(tgdpot) = ' +
006200051229     c                             %triml(%editc(t05fil:'Z'))
006300051229     c                   endif
006400051229      *?Ordinamento
006500051229     c                   eval      wrkgetlista = wrkgetlista +
006600090925     c                             ' group by tgdpot, tgddac, tgdcdt'
006700060322      *?Ordinamento
006800060322     c                   eval      wrkgetlista = wrkgetlista +
006900060322     c                             ' order by tgdpot, tgddac, tgdcdt'
007000051229      *?Solo lettura
007100051229     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
007200051229
007300051229     C/EXEC SQL
007400051229     C+ PREPARE s1 FROM :wrkgetlista
007500051229     C/END-EXEC
007600051229     C
007700051229     C/EXEC SQL
007800051229     C+ DECLARE A1 CURSOR FOR S1
007900051229     C/END-EXEC
008000051229     C
008100051229     C/EXEC SQL
008200051229     C+ OPEN a1
008300051229     C/END-EXEC
008400051229
008500051229     c                   clear                   conta
008600051229      *?Leggo il file
008700051229     c                   do        *hival
008800051229     C/EXEC SQL
008900090925     C+ FETCH NEXT FROM a1 INTO :tgdpot, :tgddac, :tgdcdt,
009000090925     c+                         :tgdconta
009100051229     C/END-EXEC
009200051229
009300051229     c                   select
009400051229
009500051229     c                   when      sqlcod = 100
009600051229     c                   leave
009700051229     c                   when      sqlcod < 0
009800051229     c                   exsr      srfine
009900051229     c                   other
010000060321     c
010100060321     c* Sommo e a cambio p.o. o data scrivo
010200060322     c                   EXSR      Contatgd
010300060104     c
010400051229     c                   endsl
010500051229
010600051229     c                   enddo
010700051229
010800051229     C/EXEC SQL
010900051229     C+ CLOSE a1
011000051229     C/END-EXEC
011100051229
011200051229     c                   endsr
011300060321      *-----------------------------------------------------------------*
011400060321      * Sommo INformative dalle altre e a rottura aggiorno
011500060321      *-----------------------------------------------------------------*
011600060322     c     contatgd      begsr
011700060321
011800060321      /free
011900060321
012000060321       //?a rottura di data aggiorno
012100060321       if tgddac <> savdim;
012200060321       if savdim > *zeros;
012300060321       exsr aggiorna;
012400060321       containfo= 0;
012500060321       endif;
012600060321       savdim = tgddac;
012700060321       savfgs = tgdpot;
012800060321       endif;
012900060321
013000060321       //?a rottura di p.o. aggiorno
013100060321       if tgdpot <> savfgs;
013200060321       if savfgs > *zeros;
013300060321       exsr aggiorna;
013400060321       containfo= 0;
013500060321       endif;
013600060321       savfgs = tgdpot;
013700060321       endif;
013800060321
013900060321       //?sommo le informative
014000060322       if tgdcdt = '  I';
014100060322       containfo= containfo+ tgdconta;
014200060322       endif;
014300060321
014400060321      /end-free
014500060321
014600060321     c                   endsr
014700060321      *-----------------------------------------------------------------*
014800060321      * Imposto cosa devo aggiornare
014900060321      *-----------------------------------------------------------------*
015000060321     c     aggiorna      begsr
015100060321
015200060321      /free
015300060321
015400060321       //?se passato il moltiplicatore aggiorno quello passato
015500060321       if t05mult <> *blanks;
015600060321       mult = t05mult;
015700060321
015800090925       if t05mult='374';
015900060321       conta = containfo;
016000060321       containfo= 0;
016100060321       endif;
016200060321
016300060321       if conta > 0;
016400060322       exsr scrivitud;
016500060321       conta = 0;
016600060321       endif;
016700060321
016800060321       //?moltiplicatore non passato aggiorno tutti quelli previsti
016900060321       else;
017000060321
017100060321       //?richeiste solo informative
017200060321       if containfo> 0;
017300090925       mult = '374';
017400060321       conta = containfo;
017500060322       exsr scrivitud;
017600060321       endif;
017700060321
017800060321       endif;
017900060321
018000060321      /end-free
018100060321
018200060321     c                   endsr
018300051227      *-----------------------------------------------------------------*
018400051228      * Scrivo/Aggiorno TNTUD
018500051227      *-----------------------------------------------------------------*
018600060321     c     SCrivitud     begsr
018700051229
018800051229      /free
018900051229
019000051229       //?imposto la data iso
019100051229       dataalfa = %subst(%char(savdim):3:6);
019200051229       dtaiso = %date(dataalfa:*ymd0);
019300051229
019400051229       //?imposto la chiave per tntud01l
019500051229       keytud.tudmult = mult;
019600051229       keytud.tudfil = savfgs;
019700051229       keytud.tuddta = dtaiso;
019800051229
019900051229       //?aggancio tntud01l
020000051229       chain %kds(keytud:3) tntud01l;
020100051229
020200051229       //?trovo rcd aggiorno se è un ricalcolo
020300051229       if %found(tntud01l);
020400051229       if t05mult <> *blanks or t05fil <> *zeros;
020500051229       tudqta = conta;
020600051229       update tntud000;
020700051229       else;
020800051229       if not%open(qsysprt);
020900051229       open qsysprt;
021000051229       except testa;
021100051229       endif;
021200051229       if *inof;
021300051229       except testa;
021400051229       endif;
021500051229       except riga;
021600051229       endif;
021700051229
021800051229       //?non trovo rcd scrivo
021900051229       else;
022000051229       clear tntud000;
022100051229       tudmult = mult;
022200051229       tuddta = dtaiso;
022300051229       tudfil = savfgs;
022400051229       tudcli = 0;
022500051229       tudqta = conta;
022600051229       write tntud000;
022700051229       endif;
022800051229
022900051229      /end-free
023000051227
023100051229     c                   endsr
023200051229
023300051229      *-----------------------------------------------------------------*
023400051229      * Fine programmma con codice errore
023500051229      *-----------------------------------------------------------------*
023600051229     c     srfine        begsr
023700051229
023800051229     c                   eval      *InLr = *On
023900051229     c                   return
024000051229
024100051229     c                   endsr
024200051227
024300051227      *-----------------------------------------------------------------*
024400051227      * Routine iniziale
024500051227      *-----------------------------------------------------------------*
024600051227     c     *inzsr        begsr
024700051227
024800051227     c     *entry        plist
024900051227     c                   parm                    kpjba
025000051227     c                   eval      tntu05ds = kpjbu
025100051227
025200051229     c                   endsr
025300051229
025400051227      *-----------------------------------------------------------------*
025500051229     oQsysprt   e            Testa          2 02
025600051229     o                                         +  5 'Rcd già esistente in'
025700051229     o                                         +  1 'TNTUD00F'
025800090925     o                                           85 'TNTU87R'
025900051229     o                       *Date         y    110
026000051229     o                                          128 'Pag.'
026100051229     o                       Page          z    132
026200051229     o          e            Riga           1
026300051229     o                                           +1 'Rcd:'
026400051229     o                       tudmult             +1
026500051229     o                       tudfil              +1
026600051229     o                       tuddta              +1
026700051229     o                       tudqta        4     +1

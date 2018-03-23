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
001200060321     d contanoinfo     s                   like(tudqta)
001300051229     d dataalfa        s              6
001400051227     d dtaiso          s               d   inz
001500051229     d mult            s                   like(tudmult)
001600060104     d savdim          s                   like(tgddac)
001700060104     d savfgs          s                   like(tgdpot)
001800060104     d keytud          ds                  likerec(tntud000:*key)
001900051229     d wrkgetlista     s           4096    varying
002000051227
002100051229     d dtup          e ds
002200060104     d fitgdds       e ds                  extname(fitgd00f)
002300051227     d kpjba         e ds
002400051227     d tntu05ds      e ds
002500060104     dtntudtads      e ds
002600051227
002700051227      *-----------------------------------------------------------------*
002800051227      * Moltiplicatori calcolati:
002900060104      *    370 - telefonale di reclamo
003000060321      *    372 - telefonale di reclamo
003100051227      *-----------------------------------------------------------------*
003200051229
003300060104     c                   exsr      PREPASQL
003400060104     c
003500060104     c*  ?Aggiorno data ultima elaborazione in tabella TUP
003600060104     c                   if        t05mult=*blanks and t05fil=0
003700060104     c                   clear                   tntudtads
003800060104     c                   eval      §DTAazn = kcdaz
003900060104     c                   eval      §DTAdta = t05dtf
004000060104     c                   call      'TNTUDTAR'
004100060104     c                   parm                    tntudtads
004200060104     c                   endif
004300051229
004400051229     c                   Eval      *InLr = *On
004500051229
004600051229      *-----------------------------------------------------------------*
004700060104      * Lettura file fasi
004800051229      *-----------------------------------------------------------------*
004900060104     c     prepasql      begsr
005000051229
005100051229      *?Imposto la parte fissa dell'istruzione sql
005200051229     c                   eval      wrkgetlista =
005300060321     c                             'select tgdpot, tgddac, tgdcdt, -
005400060104     c                             count(*) from fitgd00f where -
005500060104     c                             tgddac between '+
005600051229     c                             %editc(t05dti:'X') + ' and ' +
005700051229     c                             %editc(t05dtf:'X')
005800051229      *?Se passato un p.o.
005900051229     c                   if        t05fil <> 0
006000051229     c                   eval      wrkgetlista = wrkgetlista +
006100060104     c                             ' and digits(tgdpot) = ' +
006200051229     c                             %triml(%editc(t05fil:'Z'))
006300051229     c                   endif
006400051229      *?Ordinamento
006500051229     c                   eval      wrkgetlista = wrkgetlista +
006600060321     c                             ' group by tgdpot, tgddac, tgdcdt'
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
008900060322     C+ FETCH NEXT FROM a1 INTO : tgdpot, :tgddac, :tgdcdt, :tgdconta
009000051229     C/END-EXEC
009100051229
009200051229     c                   select
009300051229
009400051229     c                   when      sqlcod = 100
009500051229     c                   leave
009600051229     c                   when      sqlcod < 0
009700051229     c                   seton                                        H1
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
012500060321       contanoinfo= 0;
012600060321       endif;
012700060321       savdim = tgddac;
012800060321       savfgs = tgdpot;
012900060321       endif;
013000060321
013100060321       //?a rottura di p.o. aggiorno
013200060321       if tgdpot <> savfgs;
013300060321       if savfgs > *zeros;
013400060321       exsr aggiorna;
013500060321       containfo= 0;
013600060321       contanoinfo= 0;
013700060321       endif;
013800060321       savfgs = tgdpot;
013900060321       endif;
014000060321
014100060321       //?sommo le informative
014200060322       if tgdcdt = '  I';
014300060322       containfo= containfo+ tgdconta;
014400060322       endif;
014500060322       if tgdcdt <> '  I';
014600060322       contanoinfo= contanoinfo+ tgdconta;
014700060321       endif;
014800060321
014900060321      /end-free
015000060321
015100060321     c                   endsr
015200060321      *-----------------------------------------------------------------*
015300060321      * Imposto cosa devo aggiornare
015400060321      *-----------------------------------------------------------------*
015500060321     c     aggiorna      begsr
015600060321
015700060321      /free
015800060321
015900060321       //?se passato il moltiplicatore aggiorno quello passato
016000060321       if t05mult <> *blanks;
016100060321       mult = t05mult;
016200060321
016300060321       if t05mult='370';
016400060321       conta = containfo;
016500060321       containfo= 0;
016600060321       endif;
016700060321       if t05mult='372';
016800060321       conta = contanoinfo;
016900060321       contanoinfo= 0;
017000060321       endif;
017100060321
017200060321       if conta > 0;
017300060322       exsr scrivitud;
017400060321       conta = 0;
017500060321       endif;
017600060321
017700060321       //?moltiplicatore non passato aggiorno tutti quelli previsti
017800060321       else;
017900060321
018000060321       //?richeiste solo informative
018100060321       if containfo> 0;
018200060321       mult = '370';
018300060321       conta = containfo;
018400060322       exsr scrivitud;
018500060321       endif;
018600060321       if contanoinfo> 0;
018700060321       mult = '372';
018800060321       conta = contanoinfo;
018900060322       exsr scrivitud;
019000060321       endif;
019100060321
019200060321       endif;
019300060321
019400060321      /end-free
019500060321
019600060321     c                   endsr
019700051227      *-----------------------------------------------------------------*
019800051228      * Scrivo/Aggiorno TNTUD
019900051227      *-----------------------------------------------------------------*
020000060321     c     SCrivitud     begsr
020100051229
020200051229      /free
020300051229
020400051229       //?imposto la data iso
020500051229       dataalfa = %subst(%char(savdim):3:6);
020600051229       dtaiso = %date(dataalfa:*ymd0);
020700051229
020800051229       //?imposto la chiave per tntud01l
020900051229       keytud.tudmult = mult;
021000051229       keytud.tudfil = savfgs;
021100051229       keytud.tuddta = dtaiso;
021200051229
021300051229       //?aggancio tntud01l
021400051229       chain %kds(keytud:3) tntud01l;
021500051229
021600051229       //?trovo rcd aggiorno se è un ricalcolo
021700051229       if %found(tntud01l);
021800051229       if t05mult <> *blanks or t05fil <> *zeros;
021900051229       tudqta = conta;
022000051229       update tntud000;
022100051229       else;
022200051229       if not%open(qsysprt);
022300051229       open qsysprt;
022400051229       except testa;
022500051229       endif;
022600051229       if *inof;
022700051229       except testa;
022800051229       endif;
022900051229       except riga;
023000051229       endif;
023100051229
023200051229       //?non trovo rcd scrivo
023300051229       else;
023400051229       clear tntud000;
023500051229       tudmult = mult;
023600051229       tuddta = dtaiso;
023700051229       tudfil = savfgs;
023800051229       tudcli = 0;
023900051229       tudqta = conta;
024000051229       write tntud000;
024100051229       endif;
024200051229
024300051229      /end-free
024400051227
024500051229     c                   endsr
024600051229
024700051229      *-----------------------------------------------------------------*
024800051229      * Fine programmma con codice errore
024900051229      *-----------------------------------------------------------------*
025000051229     c     srfine        begsr
025100051229
025200051229     c                   eval      *InLr = *On
025300051229     c                   return
025400051229
025500051229     c                   endsr
025600051227
025700051227      *-----------------------------------------------------------------*
025800051227      * Routine iniziale
025900051227      *-----------------------------------------------------------------*
026000051227     c     *inzsr        begsr
026100051227
026200051227     c     *entry        plist
026300051227     c                   parm                    kpjba
026400051227     c                   eval      tntu05ds = kpjbu
026500051227
026600051229     c                   endsr
026700051229
026800051227      *-----------------------------------------------------------------*
026900051229     oQsysprt   e            Testa          2 02
027000051229     o                                         +  5 'Rcd già esistente in'
027100051229     o                                         +  1 'TNTUD00F'
027200060104     o                                           85 'TNTU92R'
027300051229     o                       *Date         y    110
027400051229     o                                          128 'Pag.'
027500051229     o                       Page          z    132
027600051229     o          e            Riga           1
027700051229     o                                           +1 'Rcd:'
027800051229     o                       tudmult             +1
027900051229     o                       tudfil              +1
028000051229     o                       tuddta              +1
028100051229     o                       tudqta        4     +1

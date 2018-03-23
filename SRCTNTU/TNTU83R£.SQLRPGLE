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
001100090925     d contalav        s                   like(tudqta)
001200090925     d contacas        s                   like(tudqta)
001300090925     d contaapp        s                   like(tudqta)
001400090925     d contasup        s                   like(tudqta)
001500090925     d contaffd        s                   like(tudqta)
001600051229     d dataalfa        s              6
001700051227     d dtaiso          s               d   inz
001800051229     d mult            s                   like(tudmult)
001900060104     d savdim          s                   like(tgddac)
002000060104     d savfgs          s                   like(tgdpot)
002100060104     d keytud          ds                  likerec(tntud000:*key)
002200051229     d wrkgetlista     s           4096    varying
002300051227
002400051229     d dtup          e ds
002500060104     d fitgdds       e ds                  extname(fitgd00f)
002600051227     d kpjba         e ds
002700051227     d tntu05ds      e ds
002800060104     dtntudtads      e ds
002900051227
003000051227      *-----------------------------------------------------------------*
003100051227      * Moltiplicatori calcolati:
003200090925      *    630 - Telefonate x lasciato avviso
003300090925      *    640 - Telefonate x c/assegno
003400090925      *    650 - Telefonate x appuntamento
003500090925      *    660 - Telefonate x supermercato
003600090925      *    670 - Telefonate x fermo deposito
003700051227      *-----------------------------------------------------------------*
003800051229
003900060104     c                   exsr      PREPASQL
004000060104     c
004100060104     c*  ?Aggiorno data ultima elaborazione in tabella TUP
004200060104     c                   if        t05mult=*blanks and t05fil=0
004300060104     c                   clear                   tntudtads
004400060104     c                   eval      §DTAazn = kcdaz
004500060104     c                   eval      §DTAdta = t05dtf
004600060104     c                   call      'TNTUDTAR'
004700060104     c                   parm                    tntudtads
004800060104     c                   endif
004900051229
005000051229     c                   Eval      *InLr = *On
005100051229
005200051229      *-----------------------------------------------------------------*
005300060104      * Lettura file fasi
005400051229      *-----------------------------------------------------------------*
005500060104     c     prepasql      begsr
005600051229
005700051229      *?Imposto la parte fissa dell'istruzione sql
005800051229     c                   eval      wrkgetlista =
005900060321     c                             'select tgdpot, tgddac, tgdcdt, -
006000090925     c                             tgdmad, -
006100060104     c                             count(*) from fitgd00f where -
006200060104     c                             tgddac between '+
006300051229     c                             %editc(t05dti:'X') + ' and ' +
006400090925     c                             %editc(t05dtf:'X') + ' and +
006500090925     c                             substr(tgdflo, 1, 1) = +
006600090925     c                             ''C'' '
006700051229      *?Se passato un p.o.
006800051229     c                   if        t05fil <> 0
006900051229     c                   eval      wrkgetlista = wrkgetlista +
007000060104     c                             ' and digits(tgdpot) = ' +
007100051229     c                             %triml(%editc(t05fil:'Z'))
007200051229     c                   endif
007300051229      *?Ordinamento
007400051229     c                   eval      wrkgetlista = wrkgetlista +
007500090925     c                             ' group by tgdpot, tgddac, tgdcdt, +
007600091006     c                             tgdmad'
007700060322      *?Ordinamento
007800060322     c                   eval      wrkgetlista = wrkgetlista +
007900090925     c                             ' order by tgdpot, tgddac, tgdcdt, +
008000091006     c                             tgdmad'
008100051229      *?Solo lettura
008200051229     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
008300051229
008400051229     C/EXEC SQL
008500051229     C+ PREPARE s1 FROM :wrkgetlista
008600051229     C/END-EXEC
008700051229     C
008800051229     C/EXEC SQL
008900051229     C+ DECLARE A1 CURSOR FOR S1
009000051229     C/END-EXEC
009100051229     C
009200051229     C/EXEC SQL
009300051229     C+ OPEN a1
009400051229     C/END-EXEC
009500051229
009600051229     c                   clear                   conta
009700051229      *?Leggo il file
009800051229     c                   do        *hival
009900051229     C/EXEC SQL
010000090925     C+ FETCH NEXT FROM a1 INTO :tgdpot, :tgddac, :tgdcdt, :tgdmad,
010100090925     c+                         :tgdconta
010200051229     C/END-EXEC
010300051229
010400051229     c                   select
010500051229
010600051229     c                   when      sqlcod = 100
010700051229     c                   leave
010800051229     c                   when      sqlcod < 0
010900051229     c                   exsr      srfine
011000051229     c                   other
011100060321     c
011200060321     c* Sommo e a cambio p.o. o data scrivo
011300060322     c                   EXSR      Contatgd
011400060104     c
011500051229     c                   endsl
011600051229
011700051229     c                   enddo
011800051229
011900051229     C/EXEC SQL
012000051229     C+ CLOSE a1
012100051229     C/END-EXEC
012200051229
012300051229     c                   endsr
012400060321      *-----------------------------------------------------------------*
012500060321      * Sommo INformative dalle altre e a rottura aggiorno
012600060321      *-----------------------------------------------------------------*
012700060322     c     contatgd      begsr
012800060321
012900060321      /free
013000060321
013100060321       //?a rottura di data aggiorno
013200060321       if tgddac <> savdim;
013300060321       if savdim > *zeros;
013400060321       exsr aggiorna;
013500090925       contalav= 0;
013600090925       contacas= 0;
013700090925       contasup= 0;
013800090925       contaapp= 0;
013900090925       contaffd= 0;
014000060321       endif;
014100060321       savdim = tgddac;
014200060321       savfgs = tgdpot;
014300060321       endif;
014400060321
014500060321       //?a rottura di p.o. aggiorno
014600060321       if tgdpot <> savfgs;
014700060321       if savfgs > *zeros;
014800060321       exsr aggiorna;
014900090925       contalav= 0;
015000090925       contacas= 0;
015100090925       contasup= 0;
015200090925       contaapp= 0;
015300090925       contaffd= 0;
015400060321       endif;
015500060321       savfgs = tgdpot;
015600060321       endif;
015700060321
015800090925       //?sommo lasciati avviso
015900090925       if tgdmad = '  2';
016000090925       contalav= contalav+ tgdconta;
016100060322       endif;
016200090925       //?sommo c/assegni
016300090925       if tgdmad = ' 10';
016400090925       contacas= contacas+ tgdconta;
016500090925       endif;
016600090925       //?sommo supermercato
016700090925       if tgdmad = ' 14';
016800090925       contasup= contasup+ tgdconta;
016900090925       endif;
017000090925       //?sommo appuntamento
017100090925       if tgdmad = ' 15';
017200090925       contaapp= contaapp+ tgdconta;
017300090925       endif;
017400090925       //?sommo fermo deposito
017500090925       if tgdmad = ' 16';
017600090925       contaffd= contaffd+ tgdconta;
017700090925       endif;
017800060321
017900060321      /end-free
018000060321
018100060321     c                   endsr
018200060321      *-----------------------------------------------------------------*
018300060321      * Imposto cosa devo aggiornare
018400060321      *-----------------------------------------------------------------*
018500060321     c     aggiorna      begsr
018600060321
018700060321      /free
018800060321
018900060321       //?se passato il moltiplicatore aggiorno quello passato
019000060321       if t05mult <> *blanks;
019100060321       mult = t05mult;
019200060321
019300090925       if t05mult='630';
019400090925       conta = contalav;
019500090925       contalav= 0;
019600060321       endif;
019700090925       if t05mult='640';
019800090925       conta = contacas;
019900090925       contacas= 0;
020000090925       endif;
020100091014       if t05mult='660';
020200090925       conta = contasup;
020300090925       contasup= 0;
020400090925       endif;
020500091014       if t05mult='650';
020600090925       conta = contaapp;
020700090925       contaapp= 0;
020800090925       endif;
020900090925       if t05mult='670';
021000090925       conta = contaffd;
021100090925       contaffd= 0;
021200090925       endif;
021300060321
021400060321       if conta > 0;
021500060322       exsr scrivitud;
021600060321       conta = 0;
021700060321       endif;
021800060321
021900060321       //?moltiplicatore non passato aggiorno tutti quelli previsti
022000060321       else;
022100060321
022200090925       //?richieste no rapide
022300090925       if contalav> 0;
022400090925       mult = '630';
022500090925       conta = contalav;
022600060322       exsr scrivitud;
022700060321       endif;
022800090925       if contacas> 0;
022900090925       mult = '640';
023000090925       conta = contacas;
023100090925       exsr scrivitud;
023200090925       endif;
023300090925       if contasup> 0;
023400091014       mult = '660';
023500090925       conta = contasup;
023600090925       exsr scrivitud;
023700090925       endif;
023800090925       if contaapp> 0;
023900091014       mult = '650';
024000090925       conta = contaapp;
024100090925       exsr scrivitud;
024200090925       endif;
024300090925       if contaffd> 0;
024400090925       mult = '670';
024500090925       conta = contaffd;
024600090925       exsr scrivitud;
024700090925       endif;
024800060321
024900060321       endif;
025000060321
025100060321      /end-free
025200060321
025300060321     c                   endsr
025400051227      *-----------------------------------------------------------------*
025500051228      * Scrivo/Aggiorno TNTUD
025600051227      *-----------------------------------------------------------------*
025700060321     c     SCrivitud     begsr
025800051229
025900051229      /free
026000051229
026100051229       //?imposto la data iso
026200051229       dataalfa = %subst(%char(savdim):3:6);
026300051229       dtaiso = %date(dataalfa:*ymd0);
026400051229
026500051229       //?imposto la chiave per tntud01l
026600051229       keytud.tudmult = mult;
026700051229       keytud.tudfil = savfgs;
026800051229       keytud.tuddta = dtaiso;
026900051229
027000051229       //?aggancio tntud01l
027100051229       chain %kds(keytud:3) tntud01l;
027200051229
027300051229       //?trovo rcd aggiorno se è un ricalcolo
027400051229       if %found(tntud01l);
027500051229       if t05mult <> *blanks or t05fil <> *zeros;
027600051229       tudqta = conta;
027700051229       update tntud000;
027800051229       else;
027900051229       if not%open(qsysprt);
028000051229       open qsysprt;
028100051229       except testa;
028200051229       endif;
028300051229       if *inof;
028400051229       except testa;
028500051229       endif;
028600051229       except riga;
028700051229       endif;
028800051229
028900051229       //?non trovo rcd scrivo
029000051229       else;
029100051229       clear tntud000;
029200051229       tudmult = mult;
029300051229       tuddta = dtaiso;
029400051229       tudfil = savfgs;
029500051229       tudcli = 0;
029600051229       tudqta = conta;
029700051229       write tntud000;
029800051229       endif;
029900051229
030000051229      /end-free
030100051227
030200051229     c                   endsr
030300051229
030400051229      *-----------------------------------------------------------------*
030500051229      * Fine programmma con codice errore
030600051229      *-----------------------------------------------------------------*
030700051229     c     srfine        begsr
030800051229
030900051229     c                   eval      *InLr = *On
031000051229     c                   return
031100051229
031200051229     c                   endsr
031300051227
031400051227      *-----------------------------------------------------------------*
031500051227      * Routine iniziale
031600051227      *-----------------------------------------------------------------*
031700051227     c     *inzsr        begsr
031800051227
031900051227     c     *entry        plist
032000051227     c                   parm                    kpjba
032100051227     c                   eval      tntu05ds = kpjbu
032200051227
032300051229     c                   endsr
032400051229
032500051227      *-----------------------------------------------------------------*
032600051229     oQsysprt   e            Testa          2 02
032700051229     o                                         +  5 'Rcd già esistente in'
032800051229     o                                         +  1 'TNTUD00F'
032900090925     o                                           85 'TNTU83R'
033000051229     o                       *Date         y    110
033100051229     o                                          128 'Pag.'
033200051229     o                       Page          z    132
033300051229     o          e            Riga           1
033400051229     o                                           +1 'Rcd:'
033500051229     o                       tudmult             +1
033600051229     o                       tudfil              +1
033700051229     o                       tuddta              +1
033800051229     o                       tudqta        4     +1

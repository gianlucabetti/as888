000100051227      *-----------------------------------------------------------------*
000200051227      *  Leggo ORM x calcolo tempi
000300051227      *-----------------------------------------------------------------*
000400051227
000500051227     ftntud01l  uf a e           k disk
000600051229     ftntbe01l  uf   e           k disk
000700051229     fqsysprt   o    f  132        printer oflind(*inof) usropn
000800051227
000900051227     d conta           s              5  0 inz
001000051227     d contaass        s              5  0 inz
001100051227     d contanoass      s              5  0 inz
001200051227     d comfgs          s                   like(orfpog) inz
001300051229     d dataalfa        s              6
001400051227     d dtaiso          s               d   inz
001500051229     d mult            s                   like(tudmult)
001600051227     d savddc          s                   like(orfddc)
001700051227     d savpog          s                   like(orfpog)
001800051229     d wrkgetlista     s           4096    varying
001900051228
002000051228     d keytud          ds                  likerec(tntud000:*key)
002100051229     d keytbe          ds                  likerec(tntbe000:*key)
002200051227
002300051229     d dtup          e ds
002400051229     d fnorfds       e ds                  extname(fnorf00f)
002500051227     d kpjba         e ds
002600051227     d tntu05ds      e ds
002700051227
002800051227      *-----------------------------------------------------------------*
002900051227      * Moltiplicatori calcolati:
003000051227      *    470 - ORM Assegnati/Quadrati
003100051227      *-----------------------------------------------------------------*
003200051229
003300051229      *?Imposto la parte fissa dell'istruzione sql
003400051229     c                   eval      wrkgetlista =
003500051229     c                             'select orfddc, orfpog, orffar -
003600051229     c                             from fnorf00f where -
003700051229     c                             orffar in(''390'', ''400'') -
003800051229     c                             and orfddc between '+
003900051229     c                             %editc(t05dti:'X') + ' and ' +
004000051229     c                             %editc(t05dtf:'X')
004100051229      *?Ordinamento
004200051229     c                   eval      wrkgetlista = wrkgetlista +
004300051229     c                             ' order by orfddc, orfpog, orffar'
004400051229      *?Solo lettura
004500051229     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
004600051229
004700051229     C/EXEC SQL
004800051229     C+ PREPARE s1 FROM :wrkgetlista
004900051229     C/END-EXEC
005000051229     C
005100051229     C/EXEC SQL
005200051229     C+ DECLARE A1 CURSOR FOR S1
005300051229     C/END-EXEC
005400051229     C
005500051229     C/EXEC SQL
005600051229     C+ OPEN a1
005700051229     C/END-EXEC
005800051229
005900051229      *?Leggo il file
006000051229     c                   do        *hival
006100051229     C/EXEC SQL
006200051229     C+ FETCH NEXT FROM a1 INTO :orfddc, :orfpog, :orffar
006300051229     C/END-EXEC
006400051229
006500051229     c                   select
006600051229
006700051229     c                   when      sqlcod = 100
006800051229     c                   exsr      aggiorna
006900051229     c                   leave
007000051229     c                   when      sqlcod < 0
007100051229     c                   seton                                        H1
007200051229     c                   exsr      srfine
007300051229     c                   other
007400051229     c                   exsr      srconta
007500051229     c                   endsl
007600051229
007700051229     c                   enddo
007800051229
007900051229     C/EXEC SQL
008000051229     C+ CLOSE a1
008100051229     C/END-EXEC
008200051229
008300051229      *?Aggiorno data ultima elaborazione in tabella TUP
008400051229
008500051229      /free
008600051229
008700051229       //?imposto la chiave per tntbe01l
008800051229       keytbe.tbecod = 'TUP';
008900051229       keytbe.tbeke1 = kcdaz;
009000051229
009100051229       //?aggancio tntbe01l
009200051229       chain %kds(keytbe:2) tntbe01l;
009300051229
009400051229       //?trovo rcd aggiorno
009500051229       if %found(tntbe01l);
009600051229       dtup = tbeuni;
009700051229       §tupdta = t05dtf;
009800051229       tbeuni = dtup;
009900051229       update tntbe000;
010000051229       endif;
010100051229
010200051229      /end-free
010300051229
010400051229     c                   Eval      *InLr = *On
010500051229
010600051229      *-----------------------------------------------------------------*
010700051229      * Conto gli ORM
010800051229      *-----------------------------------------------------------------*
010900051229     c     srconta       begsr
011000051227
011100051227      /free
011200051227
011300051227       //?a rottura di data aggiorno
011400051227       if orfddc <> savddc;
011500051227       if savddc > *zeros;
011600051227       exsr aggiorna;
011700051227       contaass = 0;
011800051227       contanoass = 0;
011900051227       endif;
012000051227       savddc = orfddc;
012100051227       savpog = orfpog;
012200051227       endif;
012300051227
012400051227       //?a rottura di p.o. aggiorno
012500051227       if orfpog <> savpog;
012600051227       if savpog > *zeros;
012700051227       exsr aggiorna;
012800051227       contaass = 0;
012900051227       contanoass = 0;
013000051227       endif;
013100051227       savpog = orfpog;
013200051227       endif;
013300051227
013400051227       //?se richiesto un solo p.o. sommo solo se uguale
013500051227       if t05fil <> *zeros and orfpog = t05fil;
013600051227
013700051227       //?sommo gli orm assegnati
013800051227       if orffar = 400;
013900051227       contaass = contaass + 1;
014000051227       endif;
014100051227
014200051227       //?sommo gli orm disassegnati
014300051227       if orffar = 390;
014400051227       contanoass = contanoass + 1;
014500051227       endif;
014600051227
014700051227       endif;
014800051227
014900051227       //?se non è stato richiesto un solo p.o. sommo sempre
015000051227       if t05fil = *zeros;
015100051227
015200051227       //?sommo gli orm assegnati
015300051227       if orffar = 400;
015400051227       contaass = contaass + 1;
015500051227       endif;
015600051227
015700051227       //?sommo gli orm disassegnati
015800051227       if orffar = 390;
015900051227       contanoass = contanoass + 1;
016000051227       endif;
016100051227
016200051227       endif;
016300051227
016400051227      /end-free
016500051227
016600051229     c                   endsr
016700051227
016800051227      *-----------------------------------------------------------------*
016900051227      * Imposto cosa devo aggiornare
017000051227      *-----------------------------------------------------------------*
017100051227     c     aggiorna      begsr
017200051227
017300051227      /free
017400051228
017500051228       //?se passato il moltiplicatore aggiorno quello passato
017600051228       if t05mult <> *blanks;
017700051228       mult = t05mult;
017800051228       conta = contaass;
017900051228       contaass = 0;
018000051228       if contanoass > 0;
018100051228       conta = conta - contanoass;
018200051228       endif;
018300051228       if conta > 0;
018400051228       exsr scrivitntu;
018500051228       conta = 0;
018600051228       endif;
018700051228
018800051229       //?moltiplicatore non passato aggiorno tutti quelli previsti
018900051228       else;
019000051227
019100051227       //?orm assegnati
019200051227       if contaass > 0;
019300051227       mult = '470';
019400051227       conta = contaass;
019500051227       //?sottaggro gli orm disassegnati
019600051227       if contanoass > 0;
019700051227       conta = conta - contanoass;
019800051227       endif;
019900051227       if conta > 0;
020000051227       exsr scrivitntu;
020100051227       endif;
020200051227       endif;
020300051227
020400051229       endif;
020500051229
020600051227      /end-free
020700051227
020800051229     c                   endsr
020900051227      *-----------------------------------------------------------------*
021000051228      * Scrivo/Aggiorno TNTUD
021100051227      *-----------------------------------------------------------------*
021200051227     c     scrivitntu    begsr
021300051229
021400051229      /free
021500051229
021600051229       //?imposto la data iso
021700051229       dataalfa = %subst(%char(savddc):3:6);
021800051229       dtaiso = %date(dataalfa:*ymd0);
021900051229
022000051229       //?imposto la chiave per tntud01l
022100051229       keytud.tudmult = mult;
022200051229       keytud.tudfil = savpog;
022300051229       keytud.tuddta = dtaiso;
022400051229
022500051229       //?aggancio tntud01l
022600051229       chain %kds(keytud:3) tntud01l;
022700051229
022800051229       //?trovo rcd aggiorno se è un ricalcolo
022900051229       if %found(tntud01l);
023000051229       if t05mult <> *blanks or t05fil <> *zeros;
023100051229       tudqta = conta;
023200051229       update tntud000;
023300051229       else;
023400051229       if not%open(qsysprt);
023500051229       open qsysprt;
023600051229       except testa;
023700051229       endif;
023800051229       if *inof;
023900051229       except testa;
024000051229       endif;
024100051229       except riga;
024200051229       endif;
024300051229
024400051229       //?non trovo rcd scrivo
024500051229       else;
024600051229       clear tntud000;
024700051229       tudmult = mult;
024800051229       tuddta = dtaiso;
024900051229       tudfil = savpog;
025000051229       tudcli = 0;
025100051229       tudqta = conta;
025200051229       write tntud000;
025300051229       endif;
025400051229
025500051229      /end-free
025600051227
025700051229     c                   endsr
025800051229
025900051229      *-----------------------------------------------------------------*
026000051229      * Fine programmma con codice errore
026100051229      *-----------------------------------------------------------------*
026200051229     c     srfine        begsr
026300051229     c                   eval      *InLr = *On
026400051229     c                   return
026500051229
026600051229     c                   endsr
026700051227
026800051227      *-----------------------------------------------------------------*
026900051227      * Routine iniziale
027000051227      *-----------------------------------------------------------------*
027100051227     c     *inzsr        begsr
027200051227
027300051227     c     *entry        plist
027400051227     c                   parm                    kpjba
027500051227     c                   eval      tntu05ds = kpjbu
027600051227
027700051229     c                   endsr
027800051229
027900051227      *-----------------------------------------------------------------*
028000051229     oQsysprt   e            Testa          2 02
028100051229     o                                         +  5 'Rcd già esistente in'
028200051229     o                                         +  1 'TNTUD00F'
028300051229     o                                           85 'TNTU95R'
028400051229     o                       *Date         y    110
028500051229     o                                          128 'Pag.'
028600051229     o                       Page          z    132
028700051229     o          e            Riga           1
028800051229     o                                           +1 'Rcd:'
028900051229     o                       tudmult             +1
029000051229     o                       tudfil              +1
029100051229     o                       tuddta              +1
029200051229     o                       tudqta        4     +1

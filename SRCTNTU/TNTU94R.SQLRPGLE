000100051229      *PARMS CLOSQLCSR(*ENDMOD)
000200051229      *PARMS DYNUSRPRF(*OWNER)
000300051227      *-----------------------------------------------------------------*
000400090427      *  Leggo FNRIG x calcolo tempi
000500051227      *-----------------------------------------------------------------*
000600051227
000700051227     ftntud01l  uf a e           k disk
000800051229     ftntbe01l  uf   e           k disk
000900051229     fqsysprt   o    f  132        printer oflind(*inof) usropn
001000051227
001100051229     d conta           s                   like(tudqta)
001200051229     d comfgs          s                   like(riglnp) inz
001300051229     d dataalfa        s              6
001400051227     d dtaiso          s               d   inz
001500051229     d mult            s                   like(tudmult)
001600090427     d savdim          s                   like(rigdata)
001700051229     d savfgs          s                   like(riglnp)
001800051229     d wrkgetlista     s           4096    varying
001900051228
002000051228     d keytud          ds                  likerec(tntud000:*key)
002100051229     d keytbe          ds                  likerec(tntbe000:*key)
002200051227
002300051229     d dtup          e ds
002400090427     d fnrigds       e ds                  extname(fnrig00f)
002500051227     d kpjba         e ds
002600051227     d tntu05ds      e ds
002700051227
002800051227      *-----------------------------------------------------------------*
002900051227      * Moltiplicatori calcolati:
003000051229      *    038 - Bolle firmate non scannerizzate (FO)
003100051229      *    110 - Bolle firmate scannerizzate (FF)
003200051227      *-----------------------------------------------------------------*
003300051229
003400051229      /free
003500051229
003600051229       //?calcolo bolle firmate no scannerizzate (FO)
003700051229       if t05mult = *blanks or t05mult = '038';
003800051229       exsr srnoscan;
003900051229       endif;
004000051229
004100051229       //?calcolo bolle firmate scannerizzate (FF)
004200051229       if t05mult = *blanks or t05mult = '110';
004300051229       exsr srscan;
004400051229       endif;
004500051229
004600051229       //?aggiorno data ultima elaborazione in tabella TUP
004700051229
004800051229       //?imposto la chiave per tntbe01l
004900051229       keytbe.tbecod = 'TUP';
005000051229       keytbe.tbeke1 = kcdaz;
005100051229
005200051229       //?aggancio tntbe01l
005300051229       chain %kds(keytbe:2) tntbe01l;
005400051229
005500060103       //?trovo rcd aggiorno solo se ultima elaborazione > data tabella
005600051229       if %found(tntbe01l);
005700051229       dtup = tbeuni;
005800060103       if t05dtf >= §tupdta;
005900051229       §tupdta = t05dtf;
006000051229       tbeuni = dtup;
006100051229       update tntbe000;
006200060103       endif;
006300051229       endif;
006400051229
006500051229      /end-free
006600051229
006700051229     c                   Eval      *InLr = *On
006800051229
006900051229      *-----------------------------------------------------------------*
007000051229      * Bolle firmate non scannerizzate
007100051229      *-----------------------------------------------------------------*
007200051229     c     srnoscan      begsr
007300051229
007400051229      *?Imposto la parte fissa dell'istruzione sql
007500051229     c                   eval      wrkgetlista =
007600090427     c                             'select riglna, rigdata, -
007700090427     c                             count(*) from fnrig00f where -
007800090427     c                             rigcau = ''F'' -
007900090427     c                             and rigdata between '+
008000051229     c                             %editc(t05dti:'X') + ' and ' +
008100051229     c                             %editc(t05dtf:'X')
008200051229      *?Se passato un p.o.
008300051229     c                   if        t05fil <> 0
008400051229     c                   eval      wrkgetlista = wrkgetlista +
008500051229     c                             ' and digits(riglna) = ' +
008600051229     c                             %triml(%editc(t05fil:'Z'))
008700051229     c                   endif
008800051229      *?Ordinamento
008900051229     c                   eval      wrkgetlista = wrkgetlista +
009000090427     c                             ' group by riglna, rigdata'
009100051229      *?Solo lettura
009200051229     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
009300051229
009400051229     C/EXEC SQL
009500051229     C+ PREPARE s1 FROM :wrkgetlista
009600051229     C/END-EXEC
009700051229     C
009800051229     C/EXEC SQL
009900051229     C+ DECLARE A1 CURSOR FOR S1
010000051229     C/END-EXEC
010100051229     C
010200051229     C/EXEC SQL
010300051229     C+ OPEN a1
010400051229     C/END-EXEC
010500051229
010600051229     c                   clear                   conta
010700051229      *?Leggo il file
010800051229     c                   do        *hival
010900051229     C/EXEC SQL
011000051229     C+ FETCH NEXT FROM a1 INTO :savfgs, :savdim, :conta
011100051229     C/END-EXEC
011200051229
011300051229     c                   select
011400051229
011500051229     c                   when      sqlcod = 100
011600051229     c                   leave
011700051229     c                   when      sqlcod < 0
011800051229     c                   seton                                        H1
011900051229     c                   exsr      srfine
012000051229     c                   other
012100051229     c                   eval      mult = '038'
012200051229     c                   exsr      scrivitntu
012300051229     c                   endsl
012400051229
012500051229     c                   enddo
012600051229
012700051229     C/EXEC SQL
012800051229     C+ CLOSE a1
012900051229     C/END-EXEC
013000051229
013100051229     c                   endsr
013200051229
013300051229      *-----------------------------------------------------------------*
013400051229      * Bolle firmate scannerizzate
013500051229      *-----------------------------------------------------------------*
013600051229     c     srscan        begsr
013700051229
013800051229      *?Imposto la parte fissa dell'istruzione sql
013900051229     c                   eval      wrkgetlista =
014000090427     c                             'select rigfil, rigdata, -
014100090427     c                             count(*) from fnrig00f where -
014200090427     c                             rigcau = ''S'' -
014300090427     c                             and rigdata between '+
014400051229     c                             %editc(t05dti:'X') + ' and ' +
014500051229     c                             %editc(t05dtf:'X')
014600051229      *?Se passato un p.o.
014700051229     c                   if        t05fil <> 0
014800051229     c                   eval      wrkgetlista = wrkgetlista +
014900090427     c                             ' and digits(rigfil) = ' +
015000051229     c                             %triml(%editc(t05fil:'Z'))
015100051229     c                   endif
015200051229      *?Ordinamento
015300051229     c                   eval      wrkgetlista = wrkgetlista +
015400090427     c                             ' group by rigfil, rigdata'
015500051229      *?Solo lettura
015600051229     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
015700051229
015800051229     C/EXEC SQL
015900051229     C+ PREPARE s2 FROM :wrkgetlista
016000051229     C/END-EXEC
016100051229     C
016200051229     C/EXEC SQL
016300051229     C+ DECLARE A2 CURSOR FOR S2
016400051229     C/END-EXEC
016500051229     C
016600051229     C/EXEC SQL
016700051229     C+ OPEN a2
016800051229     C/END-EXEC
016900051229
017000051229     c                   clear                   conta
017100051229      *?Leggo il file
017200051229     c                   do        *hival
017300051229     C/EXEC SQL
017400051229     C+ FETCH NEXT FROM a2 INTO :savfgs, :savdim, :conta
017500051229     C/END-EXEC
017600051229
017700051229     c                   select
017800051229
017900051229     c                   when      sqlcod = 100
018000051229     c                   leave
018100051229     c                   when      sqlcod < 0
018200051229     c                   seton                                        H1
018300051229     c                   exsr      srfine
018400051229     c                   other
018500051229     c                   eval      mult = '110'
018600051229     c                   exsr      scrivitntu
018700051229     c                   endsl
018800051229
018900051229     c                   enddo
019000051229
019100051229     C/EXEC SQL
019200051229     C+ CLOSE a2
019300051229     C/END-EXEC
019400051229
019500051229     c                   endsr
019600051229
019700051227      *-----------------------------------------------------------------*
019800051228      * Scrivo/Aggiorno TNTUD
019900051227      *-----------------------------------------------------------------*
020000051227     c     scrivitntu    begsr
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
027200051229     o                                           85 'TNTU94R'
027300051229     o                       *Date         y    110
027400051229     o                                          128 'Pag.'
027500051229     o                       Page          z    132
027600051229     o          e            Riga           1
027700051229     o                                           +1 'Rcd:'
027800051229     o                       tudmult             +1
027900051229     o                       tudfil              +1
028000051229     o                       tuddta              +1
028100051229     o                       tudqta        4     +1

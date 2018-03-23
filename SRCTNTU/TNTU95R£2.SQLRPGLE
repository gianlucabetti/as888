000100051229      *PARMS CLOSQLCSR(*ENDMOD)
000200051229      *PARMS DYNUSRPRF(*OWNER)
000300051227      *-----------------------------------------------------------------*
000400051227      *  Leggo ORM x calcolo tempi
000500051227      *-----------------------------------------------------------------*
000600051227
000700051227     ftntud01l  uf a e           k disk
000800051229     ftntbe01l  uf   e           k disk
000900051229     fqsysprt   o    f  132        printer oflind(*inof) usropn
001000051227
001100051229     d conta           s                   like(tudqta)
001200051229     d contaass        s                   like(tudqta)
001300051229     d contanoass      s                   like(tudqta)
001400051227     d comfgs          s                   like(orfpog) inz
001500051229     d dataalfa        s              6
001600051227     d dtaiso          s               d   inz
001700051229     d mult            s                   like(tudmult)
001800051227     d savddc          s                   like(orfddc)
001900051227     d savpog          s                   like(orfpog)
002000051229     d wrkgetlista     s           4096    varying
002100051228
002200051228     d keytud          ds                  likerec(tntud000:*key)
002300051229     d keytbe          ds                  likerec(tntbe000:*key)
002400051227
002500051229     d dtup          e ds
002600051229     d fnorfds       e ds                  extname(fnorf00f)
002700051227     d kpjba         e ds
002800051227     d tntu05ds      e ds
002900051227
003000051227      *-----------------------------------------------------------------*
003100051227      * Moltiplicatori calcolati:
003200071025      *    460 - ORM Stampa
003300051227      *    470 - ORM Assegnati/Quadrati
003400051227      *-----------------------------------------------------------------*
003500051229
003600051229      *?Imposto la parte fissa dell'istruzione sql
003700051229     c                   eval      wrkgetlista =
003800051229     c                             'select orfddc, orfpog, orffar' +
003900051229     c                             ' from fnorf00f where' +
004000051229     c                             ' orffar in(390, 400)' +
004100051229     c                             ' and orfddc between '+
004200051229     c                             %editc(t05dti:'X') + ' and ' +
004300051229     c                             %editc(t05dtf:'X')
004400051229      *?Ordinamento
004500051229     c                   eval      wrkgetlista = wrkgetlista +
004600051229     c                             ' order by orfddc, orfpog, orffar'
004700051229      *?Solo lettura
004800051229     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
004900051229
005000051229     C/EXEC SQL
005100051229     C+ PREPARE s1 FROM :wrkgetlista
005200051229     C/END-EXEC
005300051229     C
005400051229     C/EXEC SQL
005500051229     C+ DECLARE A1 CURSOR FOR S1
005600051229     C/END-EXEC
005700051229     C
005800051229     C/EXEC SQL
005900051229     C+ OPEN a1
006000051229     C/END-EXEC
006100051229
006200051229      *?Leggo il file
006300051229     c                   do        *hival
006400051229     C/EXEC SQL
006500051229     C+ FETCH NEXT FROM a1 INTO :orfddc, :orfpog, :orffar
006600051229     C/END-EXEC
006700051229
006800051229     c                   select
006900051229
007000051229     c                   when      sqlcod = 100
007100051229     c                   exsr      aggiorna
007200051229     c                   leave
007300051229     c                   when      sqlcod < 0
007400051229     c                   seton                                        H1
007500051229     c                   exsr      srfine
007600051229     c                   other
007700051229     c                   exsr      srconta
007800051229     c                   endsl
007900051229
008000051229     c                   enddo
008100051229
008200051229     C/EXEC SQL
008300051229     C+ CLOSE a1
008400051229     C/END-EXEC
008500051229
008600051229      *?Aggiorno data ultima elaborazione in tabella TUP
008700051229
008800051229      /free
008900051229
009000051229       //?imposto la chiave per tntbe01l
009100051229       keytbe.tbecod = 'TUP';
009200051229       keytbe.tbeke1 = kcdaz;
009300051229
009400051229       //?aggancio tntbe01l
009500051229       chain %kds(keytbe:2) tntbe01l;
009600051229
009700060103       //?trovo rcd aggiorno solo se ultima elaborazione > data tabella
009800051229       if %found(tntbe01l);
009900051229       dtup = tbeuni;
010000060103       if t05dtf >= §tupdta;
010100051229       §tupdta = t05dtf;
010200051229       tbeuni = dtup;
010300051229       update tntbe000;
010400060103       endif;
010500051229       endif;
010600051229
010700051229      /end-free
010800051229
010900051229     c                   Eval      *InLr = *On
011000051229
011100051229      *-----------------------------------------------------------------*
011200051229      * Conto gli ORM
011300051229      *-----------------------------------------------------------------*
011400051229     c     srconta       begsr
011500051227
011600051227      /free
011700051227
011800051227       //?a rottura di data aggiorno
011900051229       if orfddc <> savddc;
012000051227       if savddc > *zeros;
012100051227       exsr aggiorna;
012200051227       contaass = 0;
012300051227       contanoass = 0;
012400051227       endif;
012500051229       savddc = orfddc;
012600051229       savpog = orfpog;
012700051227       endif;
012800051227
012900051227       //?a rottura di p.o. aggiorno
013000051229       if orfpog <> savpog;
013100051227       if savpog > *zeros;
013200051227       exsr aggiorna;
013300051227       contaass = 0;
013400051227       contanoass = 0;
013500051227       endif;
013600051229       savpog = orfpog;
013700051227       endif;
013800051227
013900051227       //?se richiesto un solo p.o. sommo solo se uguale
014000051229       if t05fil <> *zeros and orfpog = t05fil;
014100051227
014200071025       //?sommo gli orm assegnati/stampati
014300051229       if orffar = 400;
014400051227       contaass = contaass + 1;
014500051227       endif;
014600051227
014700051227       //?sommo gli orm disassegnati
014800051229       if orffar = 390;
014900051227       contanoass = contanoass + 1;
015000051227       endif;
015100051227
015200051227       endif;
015300051227
015400051227       //?se non è stato richiesto un solo p.o. sommo sempre
015500051227       if t05fil = *zeros;
015600051227
015700071025       //?sommo gli orm assegnati/stampati
015800051229       if orffar = 400;
015900051227       contaass = contaass + 1;
016000051227       endif;
016100051227
016200051227       //?sommo gli orm disassegnati
016300051229       if orffar = 390;
016400051227       contanoass = contanoass + 1;
016500051227       endif;
016600051227
016700051227       endif;
016800051227
016900051227      /end-free
017000051227
017100051229     c                   endsr
017200051227
017300051227      *-----------------------------------------------------------------*
017400051227      * Imposto cosa devo aggiornare
017500051227      *-----------------------------------------------------------------*
017600051227     c     aggiorna      begsr
017700051227
017800051227      /free
017900051228
018000051228       //?se passato il moltiplicatore aggiorno quello passato
018100051228       if t05mult <> *blanks;
018200051228       mult = t05mult;
018300071029       conta = contaass;
018400071029       contaass = 0;
018500071029       if contanoass > 0;
018600071029       conta = conta - contanoass;
018700071029       endif;
018800051228       if conta > 0;
018900051228       exsr scrivitntu;
019000051228       conta = 0;
019100051228       endif;
019200051228
019300051229       //?moltiplicatore non passato aggiorno tutti quelli previsti
019400051228       else;
019500071029
019600071029       //?orm assegnati
019700071029       if contaass > 0;
019800071029       conta = contaass;
019900071029       //?sottaggro gli orm disassegnati
020000071029       if contanoass > 0;
020100071029       conta = conta - contanoass;
020200071029       endif;
020300051227
020400071029       if conta > 0;
020500071029
020600051227       //?orm assegnati
020700051227       mult = '470';
020800051227       exsr scrivitntu;
020900071025
021000071025       //?orm stampati
021100071025       mult = '460';
021200071025       exsr scrivitntu;
021300071029
021400051229       endif;
021500071029       endif;
021600071029       endif;
021700051229
021800051227      /end-free
021900051227
022000051229     c                   endsr
022100051227      *-----------------------------------------------------------------*
022200051228      * Scrivo/Aggiorno TNTUD
022300051227      *-----------------------------------------------------------------*
022400051227     c     scrivitntu    begsr
022500051229
022600051229      /free
022700051229
022800051229       //?imposto la data iso
022900051229       dataalfa = %subst(%char(savddc):3:6);
023000051229       dtaiso = %date(dataalfa:*ymd0);
023100051229
023200051229       //?imposto la chiave per tntud01l
023300051229       keytud.tudmult = mult;
023400051229       keytud.tudfil = savpog;
023500051229       keytud.tuddta = dtaiso;
023600051229
023700051229       //?aggancio tntud01l
023800051229       chain %kds(keytud:3) tntud01l;
023900051229
024000051229       //?trovo rcd aggiorno se è un ricalcolo
024100051229       if %found(tntud01l);
024200051229       if t05mult <> *blanks or t05fil <> *zeros;
024300051229       tudqta = conta;
024400051229       update tntud000;
024500051229       else;
024600051229       if not%open(qsysprt);
024700051229       open qsysprt;
024800051229       except testa;
024900051229       endif;
025000051229       if *inof;
025100051229       except testa;
025200051229       endif;
025300051229       except riga;
025400051229       endif;
025500051229
025600051229       //?non trovo rcd scrivo
025700051229       else;
025800051229       clear tntud000;
025900051229       tudmult = mult;
026000051229       tuddta = dtaiso;
026100051229       tudfil = savpog;
026200051229       tudcli = 0;
026300051229       tudqta = conta;
026400051229       write tntud000;
026500051229       endif;
026600051229
026700051229      /end-free
026800051227
026900051229     c                   endsr
027000051229
027100051229      *-----------------------------------------------------------------*
027200051229      * Fine programmma con codice errore
027300051229      *-----------------------------------------------------------------*
027400051229     c     srfine        begsr
027500051229
027600051229     c                   eval      *InLr = *On
027700051229     c                   return
027800051229
027900051229     c                   endsr
028000051227
028100051227      *-----------------------------------------------------------------*
028200051227      * Routine iniziale
028300051227      *-----------------------------------------------------------------*
028400051227     c     *inzsr        begsr
028500051227
028600051227     c     *entry        plist
028700051227     c                   parm                    kpjba
028800051227     c                   eval      tntu05ds = kpjbu
028900051227
029000051229     c                   endsr
029100051229
029200051227      *-----------------------------------------------------------------*
029300051229     oQsysprt   e            Testa          2 02
029400051229     o                                         +  5 'Rcd già esistente in'
029500051229     o                                         +  1 'TNTUD00F'
029600051229     o                                           85 'TNTU95R'
029700051229     o                       *Date         y    110
029800051229     o                                          128 'Pag.'
029900051229     o                       Page          z    132
030000051229     o          e            Riga           1
030100051229     o                                           +1 'Rcd:'
030200051229     o                       tudmult             +1
030300051229     o                       tudfil              +1
030400051229     o                       tuddta              +1
030500051229     o                       tudqta        4     +1

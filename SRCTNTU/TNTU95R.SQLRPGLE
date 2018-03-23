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
003200051227      *    470 - ORM Assegnati/Quadrati
003300051227      *-----------------------------------------------------------------*
003400051229
003500051229      *?Imposto la parte fissa dell'istruzione sql
003600051229     c                   eval      wrkgetlista =
003700051229     c                             'select orfddc, orfpog, orffar' +
003800051229     c                             ' from fnorf00f where' +
003900051229     c                             ' orffar in(390, 400)' +
004000051229     c                             ' and orfddc between '+
004100051229     c                             %editc(t05dti:'X') + ' and ' +
004200051229     c                             %editc(t05dtf:'X')
004300051229      *?Ordinamento
004400051229     c                   eval      wrkgetlista = wrkgetlista +
004500051229     c                             ' order by orfddc, orfpog, orffar'
004600051229      *?Solo lettura
004700051229     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
004800051229
004900051229     C/EXEC SQL
005000051229     C+ PREPARE s1 FROM :wrkgetlista
005100051229     C/END-EXEC
005200051229     C
005300051229     C/EXEC SQL
005400051229     C+ DECLARE A1 CURSOR FOR S1
005500051229     C/END-EXEC
005600051229     C
005700051229     C/EXEC SQL
005800051229     C+ OPEN a1
005900051229     C/END-EXEC
006000051229
006100051229      *?Leggo il file
006200051229     c                   do        *hival
006300051229     C/EXEC SQL
006400051229     C+ FETCH NEXT FROM a1 INTO :orfddc, :orfpog, :orffar
006500051229     C/END-EXEC
006600051229
006700051229     c                   select
006800051229
006900051229     c                   when      sqlcod = 100
007000051229     c                   exsr      aggiorna
007100051229     c                   leave
007200051229     c                   when      sqlcod < 0
007300051229     c                   seton                                        H1
007400051229     c                   exsr      srfine
007500051229     c                   other
007600051229     c                   exsr      srconta
007700051229     c                   endsl
007800051229
007900051229     c                   enddo
008000051229
008100051229     C/EXEC SQL
008200051229     C+ CLOSE a1
008300051229     C/END-EXEC
008400051229
008500051229      *?Aggiorno data ultima elaborazione in tabella TUP
008600051229
008700051229      /free
008800051229
008900051229       //?imposto la chiave per tntbe01l
009000051229       keytbe.tbecod = 'TUP';
009100051229       keytbe.tbeke1 = kcdaz;
009200051229
009300051229       //?aggancio tntbe01l
009400051229       chain %kds(keytbe:2) tntbe01l;
009500051229
009600060103       //?trovo rcd aggiorno solo se ultima elaborazione > data tabella
009700051229       if %found(tntbe01l);
009800051229       dtup = tbeuni;
009900060103       if t05dtf >= §tupdta;
010000051229       §tupdta = t05dtf;
010100051229       tbeuni = dtup;
010200051229       update tntbe000;
010300060103       endif;
010400051229       endif;
010500051229
010600051229      /end-free
010700051229
010800051229     c                   Eval      *InLr = *On
010900051229
011000051229      *-----------------------------------------------------------------*
011100051229      * Conto gli ORM
011200051229      *-----------------------------------------------------------------*
011300051229     c     srconta       begsr
011400051227
011500051227      /free
011600051227
011700051227       //?a rottura di data aggiorno
011800051229       if orfddc <> savddc;
011900051227       if savddc > *zeros;
012000051227       exsr aggiorna;
012100051227       contaass = 0;
012200051227       contanoass = 0;
012300051227       endif;
012400051229       savddc = orfddc;
012500051229       savpog = orfpog;
012600051227       endif;
012700051227
012800051227       //?a rottura di p.o. aggiorno
012900051229       if orfpog <> savpog;
013000051227       if savpog > *zeros;
013100051227       exsr aggiorna;
013200051227       contaass = 0;
013300051227       contanoass = 0;
013400051227       endif;
013500051229       savpog = orfpog;
013600051227       endif;
013700051227
013800051227       //?se richiesto un solo p.o. sommo solo se uguale
013900051229       if t05fil <> *zeros and orfpog = t05fil;
014000051227
014100071025       //?sommo gli orm assegnati/stampati
014200051229       if orffar = 400;
014300051227       contaass = contaass + 1;
014400051227       endif;
014500051227
014600051227       //?sommo gli orm disassegnati
014700051229       if orffar = 390;
014800051227       contanoass = contanoass + 1;
014900051227       endif;
015000051227
015100051227       endif;
015200051227
015300051227       //?se non è stato richiesto un solo p.o. sommo sempre
015400051227       if t05fil = *zeros;
015500051227
015600071025       //?sommo gli orm assegnati/stampati
015700051229       if orffar = 400;
015800051227       contaass = contaass + 1;
015900051227       endif;
016000051227
016100051227       //?sommo gli orm disassegnati
016200051229       if orffar = 390;
016300051227       contanoass = contanoass + 1;
016400051227       endif;
016500051227
016600051227       endif;
016700051227
016800051227      /end-free
016900051227
017000051229     c                   endsr
017100051227
017200051227      *-----------------------------------------------------------------*
017300051227      * Imposto cosa devo aggiornare
017400051227      *-----------------------------------------------------------------*
017500051227     c     aggiorna      begsr
017600071029
017700051227      /free
017800051228
017900051228       //?se passato il moltiplicatore aggiorno quello passato
018000051228       if t05mult <> *blanks;
018100051228       mult = t05mult;
018200071029       conta = contaass;
018300071029       contaass = 0;
018400071029       if contanoass > 0;
018500071029       conta = conta - contanoass;
018600071029       endif;
018700071029
018800051228       if conta > 0;
018900071029
019000071029       //?orm assegnati
019100071029       if mult = '470';
019200051228       exsr scrivitntu;
019300071029       endif;
019400071029
019500051228       conta = 0;
019600051228       endif;
019700051228
019800051229       //?moltiplicatore non passato aggiorno tutti quelli previsti
019900051228       else;
020000071029
020100071029       //?orm assegnati
020200071029       if contaass > 0;
020300071029       conta = contaass;
020400071029       //?sottaggro gli orm disassegnati
020500071029       if contanoass > 0;
020600071029       conta = conta - contanoass;
020700071029       endif;
020800051227
020900071029       if conta > 0;
021000071029
021100051227       //?orm assegnati
021200051227       mult = '470';
021300051227       exsr scrivitntu;
021400071029
021500051229       endif;
021600071029       endif;
021700071029       endif;
021800051229
021900051227      /end-free
022000051227
022100051229     c                   endsr
022200051227      *-----------------------------------------------------------------*
022300051228      * Scrivo/Aggiorno TNTUD
022400051227      *-----------------------------------------------------------------*
022500051227     c     scrivitntu    begsr
022600051229
022700051229      /free
022800051229
022900051229       //?imposto la data iso
023000051229       dataalfa = %subst(%char(savddc):3:6);
023100051229       dtaiso = %date(dataalfa:*ymd0);
023200051229
023300051229       //?imposto la chiave per tntud01l
023400051229       keytud.tudmult = mult;
023500051229       keytud.tudfil = savpog;
023600051229       keytud.tuddta = dtaiso;
023700051229
023800051229       //?aggancio tntud01l
023900051229       chain %kds(keytud:3) tntud01l;
024000051229
024100051229       //?trovo rcd aggiorno se è un ricalcolo
024200051229       if %found(tntud01l);
024300051229       if t05mult <> *blanks or t05fil <> *zeros;
024400051229       tudqta = conta;
024500051229       update tntud000;
024600051229       else;
024700051229       if not%open(qsysprt);
024800051229       open qsysprt;
024900051229       except testa;
025000051229       endif;
025100051229       if *inof;
025200051229       except testa;
025300051229       endif;
025400051229       except riga;
025500051229       endif;
025600051229
025700051229       //?non trovo rcd scrivo
025800051229       else;
025900051229       clear tntud000;
026000051229       tudmult = mult;
026100051229       tuddta = dtaiso;
026200051229       tudfil = savpog;
026300051229       tudcli = 0;
026400051229       tudqta = conta;
026500051229       write tntud000;
026600051229       endif;
026700051229
026800051229      /end-free
026900051227
027000051229     c                   endsr
027100051229
027200051229      *-----------------------------------------------------------------*
027300051229      * Fine programmma con codice errore
027400051229      *-----------------------------------------------------------------*
027500051229     c     srfine        begsr
027600051229
027700051229     c                   eval      *InLr = *On
027800051229     c                   return
027900051229
028000051229     c                   endsr
028100051227
028200051227      *-----------------------------------------------------------------*
028300051227      * Routine iniziale
028400051227      *-----------------------------------------------------------------*
028500051227     c     *inzsr        begsr
028600051227
028700051227     c     *entry        plist
028800051227     c                   parm                    kpjba
028900051227     c                   eval      tntu05ds = kpjbu
029000051227
029100051229     c                   endsr
029200051229
029300051227      *-----------------------------------------------------------------*
029400051229     oQsysprt   e            Testa          2 02
029500051229     o                                         +  5 'Rcd già esistente in'
029600051229     o                                         +  1 'TNTUD00F'
029700051229     o                                           85 'TNTU95R'
029800051229     o                       *Date         y    110
029900051229     o                                          128 'Pag.'
030000051229     o                       Page          z    132
030100051229     o          e            Riga           1
030200051229     o                                           +1 'Rcd:'
030300051229     o                       tudmult             +1
030400051229     o                       tudfil              +1
030500051229     o                       tuddta              +1
030600051229     o                       tudqta        4     +1

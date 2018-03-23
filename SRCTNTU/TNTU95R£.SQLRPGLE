000100051229      *PARMS CLOSQLCSR(*ENDMOD)
000200051229      *PARMS DYNUSRPRF(*OWNER)
000300051227      *-----------------------------------------------------------------*
000400051227      *  Leggo ORM x calcolo tempi
000500051227      *-----------------------------------------------------------------*
000600051227
000700051227     ftntud01l  uf a e           k disk
000800051229     ftntbe01l  uf   e           k disk
000900071029     fazorg01l  if   e           k disk
001000051229     fqsysprt   o    f  132        printer oflind(*inof) usropn
001100051227
001200051229     d conta           s                   like(tudqta)
001300051229     d contaass        s                   like(tudqta)
001400051229     d contanoass      s                   like(tudqta)
001500051227     d comfgs          s                   like(orfpog) inz
001600051229     d dataalfa        s              6
001700071029     d data_org        s              8  0 inz
001800051227     d dtaiso          s               d   inz
001900051229     d mult            s                   like(tudmult)
002000051227     d savddc          s                   like(orfddc)
002100051227     d savpog          s                   like(orfpog)
002200051229     d wrkgetlista     s           4096    varying
002300071029     d $geoorm         s              1    inz(*off)
002400051228
002500051228     d keytud          ds                  likerec(tntud000:*key)
002600051229     d keytbe          ds                  likerec(tntbe000:*key)
002700051227
002800051229     d dtup          e ds
002900051229     d fnorfds       e ds                  extname(fnorf00f)
003000051227     d kpjba         e ds
003100051227     d tntu05ds      e ds
003200071029     d og147         e ds
003300051227
003400051227      *-----------------------------------------------------------------*
003500051227      * Moltiplicatori calcolati:
003600071025      *    460 - ORM Stampa
003700051227      *    470 - ORM Assegnati/Quadrati
003800051227      *-----------------------------------------------------------------*
003900051229
004000051229      *?Imposto la parte fissa dell'istruzione sql
004100051229     c                   eval      wrkgetlista =
004200051229     c                             'select orfddc, orfpog, orffar' +
004300051229     c                             ' from fnorf00f where' +
004400051229     c                             ' orffar in(390, 400)' +
004500051229     c                             ' and orfddc between '+
004600051229     c                             %editc(t05dti:'X') + ' and ' +
004700051229     c                             %editc(t05dtf:'X')
004800051229      *?Ordinamento
004900051229     c                   eval      wrkgetlista = wrkgetlista +
005000051229     c                             ' order by orfddc, orfpog, orffar'
005100051229      *?Solo lettura
005200051229     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
005300051229
005400051229     C/EXEC SQL
005500051229     C+ PREPARE s1 FROM :wrkgetlista
005600051229     C/END-EXEC
005700051229     C
005800051229     C/EXEC SQL
005900051229     C+ DECLARE A1 CURSOR FOR S1
006000051229     C/END-EXEC
006100051229     C
006200051229     C/EXEC SQL
006300051229     C+ OPEN a1
006400051229     C/END-EXEC
006500051229
006600051229      *?Leggo il file
006700051229     c                   do        *hival
006800051229     C/EXEC SQL
006900051229     C+ FETCH NEXT FROM a1 INTO :orfddc, :orfpog, :orffar
007000051229     C/END-EXEC
007100051229
007200051229     c                   select
007300051229
007400051229     c                   when      sqlcod = 100
007500051229     c                   exsr      aggiorna
007600051229     c                   leave
007700051229     c                   when      sqlcod < 0
007800051229     c                   seton                                        H1
007900051229     c                   exsr      srfine
008000051229     c                   other
008100051229     c                   exsr      srconta
008200051229     c                   endsl
008300051229
008400051229     c                   enddo
008500051229
008600051229     C/EXEC SQL
008700051229     C+ CLOSE a1
008800051229     C/END-EXEC
008900051229
009000051229      *?Aggiorno data ultima elaborazione in tabella TUP
009100051229
009200051229      /free
009300051229
009400051229       //?imposto la chiave per tntbe01l
009500051229       keytbe.tbecod = 'TUP';
009600051229       keytbe.tbeke1 = kcdaz;
009700051229
009800051229       //?aggancio tntbe01l
009900051229       chain %kds(keytbe:2) tntbe01l;
010000051229
010100060103       //?trovo rcd aggiorno solo se ultima elaborazione > data tabella
010200051229       if %found(tntbe01l);
010300051229       dtup = tbeuni;
010400060103       if t05dtf >= §tupdta;
010500051229       §tupdta = t05dtf;
010600051229       tbeuni = dtup;
010700051229       update tntbe000;
010800060103       endif;
010900051229       endif;
011000051229
011100051229      /end-free
011200051229
011300051229     c                   Eval      *InLr = *On
011400051229
011500051229      *-----------------------------------------------------------------*
011600051229      * Conto gli ORM
011700051229      *-----------------------------------------------------------------*
011800051229     c     srconta       begsr
011900051227
012000051227      /free
012100051227
012200051227       //?a rottura di data aggiorno
012300051229       if orfddc <> savddc;
012400051227       if savddc > *zeros;
012500051227       exsr aggiorna;
012600051227       contaass = 0;
012700051227       contanoass = 0;
012800051227       endif;
012900051229       savddc = orfddc;
013000051229       savpog = orfpog;
013100051227       endif;
013200051227
013300051227       //?a rottura di p.o. aggiorno
013400051229       if orfpog <> savpog;
013500051227       if savpog > *zeros;
013600051227       exsr aggiorna;
013700051227       contaass = 0;
013800051227       contanoass = 0;
013900051227       endif;
014000051229       savpog = orfpog;
014100051227       endif;
014200051227
014300051227       //?se richiesto un solo p.o. sommo solo se uguale
014400051229       if t05fil <> *zeros and orfpog = t05fil;
014500051227
014600071025       //?sommo gli orm assegnati/stampati
014700051229       if orffar = 400;
014800051227       contaass = contaass + 1;
014900051227       endif;
015000051227
015100051227       //?sommo gli orm disassegnati
015200051229       if orffar = 390;
015300051227       contanoass = contanoass + 1;
015400051227       endif;
015500051227
015600051227       endif;
015700051227
015800051227       //?se non è stato richiesto un solo p.o. sommo sempre
015900051227       if t05fil = *zeros;
016000051227
016100071025       //?sommo gli orm assegnati/stampati
016200051229       if orffar = 400;
016300051227       contaass = contaass + 1;
016400051227       endif;
016500051227
016600051227       //?sommo gli orm disassegnati
016700051229       if orffar = 390;
016800051227       contanoass = contanoass + 1;
016900051227       endif;
017000051227
017100051227       endif;
017200051227
017300051227      /end-free
017400051227
017500051229     c                   endsr
017600051227
017700051227      *-----------------------------------------------------------------*
017800051227      * Imposto cosa devo aggiornare
017900051227      *-----------------------------------------------------------------*
018000051227     c     aggiorna      begsr
018100051227
018200071029      *?aggancio organigramma per stabilire se filiale
018300071029      *?ha la procedura GEO ORM attiva
018400071029     c                   clear                   og147
018500071029     c                   eval      $geoorm = *off
018600071029     c     savpog        chain     azorg01l
018700071029     c                   if        %found(azorg01l)
018800071029     c                   eval      og147 = orgde7
018900071029     c                   endif
019000071029      * controllo se attivo con GEO ORM
019100071029     c                   if        §ogcgio = 'S' and §ogddao > *zeros
019200071029     c                   move      §ogddao       data_org
019300071029     c                   if        savddc >= data_org
019400071029     c                   eval      $geoorm = *on
019500071029     c                   endif
019600071029     c                   endif
019700071029
019800051227      /free
019900051228
020000051228       //?se passato il moltiplicatore aggiorno quello passato
020100051228       if t05mult <> *blanks;
020200051228       mult = t05mult;
020300071029       conta = contaass;
020400071029       contaass = 0;
020500071029       if contanoass > 0;
020600071029       conta = conta - contanoass;
020700071029       endif;
020800071029
020900051228       if conta > 0;
021000071029
021100071029       //?orm assegnati
021200071029       if mult = '470';
021300051228       exsr scrivitntu;
021400071029       endif;
021500071029
021600071029       //?orm stampati
021700071029       //?se GEO ORM attiva
021800071029       if mult = '460' and $geoorm = *on;
021900071029       exsr scrivitntu;
022000071029       endif;
022100071029
022200051228       conta = 0;
022300051228       endif;
022400051228
022500051229       //?moltiplicatore non passato aggiorno tutti quelli previsti
022600051228       else;
022700071029
022800071029       //?orm assegnati
022900071029       if contaass > 0;
023000071029       conta = contaass;
023100071029       //?sottaggro gli orm disassegnati
023200071029       if contanoass > 0;
023300071029       conta = conta - contanoass;
023400071029       endif;
023500051227
023600071029       if conta > 0;
023700071029
023800051227       //?orm assegnati
023900051227       mult = '470';
024000051227       exsr scrivitntu;
024100071025
024200071025       //?orm stampati
024300071029       //?se GEO ORM attiva
024400071029       if $geoorm = *on;
024500071025       mult = '460';
024600071025       exsr scrivitntu;
024700071029       endif;
024800071029
024900051229       endif;
025000071029       endif;
025100071029       endif;
025200051229
025300051227      /end-free
025400051227
025500051229     c                   endsr
025600051227      *-----------------------------------------------------------------*
025700051228      * Scrivo/Aggiorno TNTUD
025800051227      *-----------------------------------------------------------------*
025900051227     c     scrivitntu    begsr
026000051229
026100051229      /free
026200051229
026300051229       //?imposto la data iso
026400051229       dataalfa = %subst(%char(savddc):3:6);
026500051229       dtaiso = %date(dataalfa:*ymd0);
026600051229
026700051229       //?imposto la chiave per tntud01l
026800051229       keytud.tudmult = mult;
026900051229       keytud.tudfil = savpog;
027000051229       keytud.tuddta = dtaiso;
027100051229
027200051229       //?aggancio tntud01l
027300051229       chain %kds(keytud:3) tntud01l;
027400051229
027500051229       //?trovo rcd aggiorno se è un ricalcolo
027600051229       if %found(tntud01l);
027700051229       if t05mult <> *blanks or t05fil <> *zeros;
027800051229       tudqta = conta;
027900051229       update tntud000;
028000051229       else;
028100051229       if not%open(qsysprt);
028200051229       open qsysprt;
028300051229       except testa;
028400051229       endif;
028500051229       if *inof;
028600051229       except testa;
028700051229       endif;
028800051229       except riga;
028900051229       endif;
029000051229
029100051229       //?non trovo rcd scrivo
029200051229       else;
029300051229       clear tntud000;
029400051229       tudmult = mult;
029500051229       tuddta = dtaiso;
029600051229       tudfil = savpog;
029700051229       tudcli = 0;
029800051229       tudqta = conta;
029900051229       write tntud000;
030000051229       endif;
030100051229
030200051229      /end-free
030300051227
030400051229     c                   endsr
030500051229
030600051229      *-----------------------------------------------------------------*
030700051229      * Fine programmma con codice errore
030800051229      *-----------------------------------------------------------------*
030900051229     c     srfine        begsr
031000051229
031100051229     c                   eval      *InLr = *On
031200051229     c                   return
031300051229
031400051229     c                   endsr
031500051227
031600051227      *-----------------------------------------------------------------*
031700051227      * Routine iniziale
031800051227      *-----------------------------------------------------------------*
031900051227     c     *inzsr        begsr
032000051227
032100051227     c     *entry        plist
032200051227     c                   parm                    kpjba
032300051227     c                   eval      tntu05ds = kpjbu
032400051227
032500051229     c                   endsr
032600051229
032700051227      *-----------------------------------------------------------------*
032800051229     oQsysprt   e            Testa          2 02
032900051229     o                                         +  5 'Rcd già esistente in'
033000051229     o                                         +  1 'TNTUD00F'
033100051229     o                                           85 'TNTU95R'
033200051229     o                       *Date         y    110
033300051229     o                                          128 'Pag.'
033400051229     o                       Page          z    132
033500051229     o          e            Riga           1
033600051229     o                                           +1 'Rcd:'
033700051229     o                       tudmult             +1
033800051229     o                       tudfil              +1
033900051229     o                       tuddta              +1
034000051229     o                       tudqta        4     +1

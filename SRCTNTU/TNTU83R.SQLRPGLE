000100140211     /*PRM dbgview(*source)
000200140211     /*END
000300140211     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000400140211     h alwnull(*inputonly)
000500051227      *-----------------------------------------------------------------*
000600060104      *  Leggo file reclami
000700051227      *-----------------------------------------------------------------*
000800051227     ftntud01l  uf a e           k disk
000900051229     fqsysprt   o    f  132        printer oflind(*inof) usropn
001000051227
001100060322     d tgdconta        s                   like(tudqta)
001200060322     d conta           s                   like(tudqta)
001300090925     d contalav        s                   like(tudqta)
001400090925     d contacas        s                   like(tudqta)
001500090925     d contaapp        s                   like(tudqta)
001600090925     d contasup        s                   like(tudqta)
001700090925     d contaffd        s                   like(tudqta)
001800051229     d dataalfa        s              6
001900051227     d dtaiso          s               d   inz
002000051229     d mult            s                   like(tudmult)
002100060104     d savdim          s                   like(tgddac)
002200060104     d savfgs          s                   like(tgdpot)
002300060104     d keytud          ds                  likerec(tntud000:*key)
002400140211     d w_MAR_1         s                   like(TGDmad)  inz
002500140211     d w_MARect        s           1024    varying       inz
002600140211     d wrkGetMARect    s           1024    varying       inz
002700051229     d wrkgetlista     s           4096    varying
002800051227
002900051229     d dtup          e ds
003000060104     d fitgdds       e ds                  extname(fitgd00f)
003100051227     d kpjba         e ds
003200051227     d tntu05ds      e ds
003300060104     dtntudtads      e ds
003400140211
003500140211       //--------------------------------------------------------------
003600140211       //?Definizione prototipi procedure.                             ?
003700140211       //--------------------------------------------------------------
003800140211
003900140211       // -?Parametri API QCAPCMD (Process Commands)?
004000140211     d Qcmd            s           2048    inz  varying
004100140211      /copy qSysInc/qRpgleSrc,QCAPCMD
004200140211       // -?API QCAPCMD (Process Commands)?
004300140211      /copy gaitrasrc/srcProtoPR,QCAPCMD
004400140211
004500140211       // -?Parametri gestione errori API.?
004600140211      /copy qsysinc/qrpglesrc,QUSEC
004700051227
004800051227      *-----------------------------------------------------------------*
004900051227      * Moltiplicatori calcolati:
005000090925      *    630 - Telefonate x lasciato avviso
005100090925      *    640 - Telefonate x c/assegno
005200090925      *    650 - Telefonate x appuntamento
005300090925      *    660 - Telefonate x supermercato
005400090925      *    670 - Telefonate x fermo deposito
005500051227      *-----------------------------------------------------------------*
005600051229
005700060104     c                   exsr      PREPASQL
005800060104     c
005900060104     c*  ?Aggiorno data ultima elaborazione in tabella TUP
006000060104     c                   if        t05mult=*blanks and t05fil=0
006100060104     c                   clear                   tntudtads
006200060104     c                   eval      §DTAazn = kcdaz
006300060104     c                   eval      §DTAdta = t05dtf
006400060104     c                   call      'TNTUDTAR'
006500060104     c                   parm                    tntudtads
006600060104     c                   endif
006700051229
006800051229     c                   Eval      *InLr = *On
006900051229
007000051229      *-----------------------------------------------------------------*
007100060104      * Lettura file fasi
007200051229      *-----------------------------------------------------------------*
007300060104     c     prepasql      begsr
007400051229
007500140211      * -?Impostazione opzioni per SQL?
007600140211     c/exec sql
007700140211     c+  set  option  DynUsrPrf = *Owner,
007800140211     c+               CloSqlCsr = *EndMod
007900140211     c/end-exec
008000140211      *
008100140211      *?Caricamento dei motivi apertura Richiesta Assistenza da escludere
008200140211      *?da queste statistiche
008300140211     c                   exsr      sr_MAR_escl
008400140211      *
008500051229      *?Imposto la parte fissa dell'istruzione sql
008600051229     c                   eval      wrkgetlista =
008700060321     c                             'select tgdpot, tgddac, tgdcdt, -
008800090925     c                             tgdmad, -
008900060104     c                             count(*) from fitgd00f where -
009000060104     c                             tgddac between '+
009100051229     c                             %editc(t05dti:'X') + ' and ' +
009200090925     c                             %editc(t05dtf:'X') + ' and +
009300090925     c                             substr(tgdflo, 1, 1) = +
009400090925     c                             ''C'' '
009500140211      *?Esclusione dei motivi apertura Richiesta Assistenza
009600140211     c                   if        w_MARect <> *blank
009700140211     c                   eval      wrkgetlista = wrkgetlista +
009800140211     c                                 ' and TGDmad NOT in ' + w_MARect
009900140211     c                   endif
010000051229      *?Se passato un p.o.
010100051229     c                   if        t05fil <> 0
010200051229     c                   eval      wrkgetlista = wrkgetlista +
010300060104     c                             ' and digits(tgdpot) = ' +
010400051229     c                             %triml(%editc(t05fil:'Z'))
010500051229     c                   endif
010600051229      *?Ordinamento
010700051229     c                   eval      wrkgetlista = wrkgetlista +
010800090925     c                             ' group by tgdpot, tgddac, tgdcdt, +
010900091006     c                             tgdmad'
011000060322      *?Ordinamento
011100060322     c                   eval      wrkgetlista = wrkgetlista +
011200090925     c                             ' order by tgdpot, tgddac, tgdcdt, +
011300091006     c                             tgdmad'
011400051229      *?Solo lettura
011500051229     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
011600051229
011700051229     C/EXEC SQL
011800051229     C+ PREPARE s1 FROM :wrkgetlista
011900051229     C/END-EXEC
012000051229     C
012100051229     C/EXEC SQL
012200051229     C+ DECLARE A1 CURSOR FOR S1
012300051229     C/END-EXEC
012400051229     C
012500051229     C/EXEC SQL
012600051229     C+ OPEN a1
012700051229     C/END-EXEC
012800051229
012900051229     c                   clear                   conta
013000051229      *?Leggo il file
013100051229     c                   do        *hival
013200051229     C/EXEC SQL
013300090925     C+ FETCH NEXT FROM a1 INTO :tgdpot, :tgddac, :tgdcdt, :tgdmad,
013400090925     c+                         :tgdconta
013500051229     C/END-EXEC
013600051229
013700051229     c                   select
013800051229
013900051229     c                   when      sqlcod = 100
014000051229     c                   leave
014100051229     c                   when      sqlcod < 0
014200051229     c                   exsr      srfine
014300051229     c                   other
014400140217     c*
014500060321     c* Sommo e a cambio p.o. o data scrivo
014600060322     c                   EXSR      Contatgd
014700140217     c*
014800051229     c                   endsl
014900051229
015000051229     c                   enddo
015100140217
015200140217      * Scrivo ultimi totali
015300140217     c                   if        savdim > *zeros
015400140217     c                   exsr      aggiorna
015500140217     c                   endif
015600051229
015700051229     C/EXEC SQL
015800051229     C+ CLOSE a1
015900051229     C/END-EXEC
016000051229
016100051229     c                   endsr
016200060321      *-----------------------------------------------------------------*
016300060321      * Sommo INformative dalle altre e a rottura aggiorno
016400060321      *-----------------------------------------------------------------*
016500060322     c     contatgd      begsr
016600060321
016700060321      /free
016800060321
016900060321       //?a rottura di data aggiorno
017000060321       if tgddac <> savdim;
017100060321       if savdim > *zeros;
017200060321       exsr aggiorna;
017300090925       contalav= 0;
017400090925       contacas= 0;
017500090925       contasup= 0;
017600090925       contaapp= 0;
017700090925       contaffd= 0;
017800060321       endif;
017900060321       savdim = tgddac;
018000060321       savfgs = tgdpot;
018100060321       endif;
018200060321
018300060321       //?a rottura di p.o. aggiorno
018400060321       if tgdpot <> savfgs;
018500060321       if savfgs > *zeros;
018600060321       exsr aggiorna;
018700090925       contalav= 0;
018800090925       contacas= 0;
018900090925       contasup= 0;
019000090925       contaapp= 0;
019100090925       contaffd= 0;
019200060321       endif;
019300060321       savfgs = tgdpot;
019400060321       endif;
019500060321
019600090925       //?sommo lasciati avviso
019700090925       if tgdmad = '  2';
019800090925       contalav= contalav+ tgdconta;
019900060322       endif;
020000090925       //?sommo c/assegni
020100090925       if tgdmad = ' 10';
020200090925       contacas= contacas+ tgdconta;
020300090925       endif;
020400090925       //?sommo supermercato
020500090925       if tgdmad = ' 14';
020600090925       contasup= contasup+ tgdconta;
020700090925       endif;
020800090925       //?sommo appuntamento
020900090925       if tgdmad = ' 15';
021000090925       contaapp= contaapp+ tgdconta;
021100090925       endif;
021200090925       //?sommo fermo deposito
021300090925       if tgdmad = ' 16';
021400090925       contaffd= contaffd+ tgdconta;
021500090925       endif;
021600060321
021700060321      /end-free
021800060321
021900060321     c                   endsr
022000060321      *-----------------------------------------------------------------*
022100060321      * Imposto cosa devo aggiornare
022200060321      *-----------------------------------------------------------------*
022300060321     c     aggiorna      begsr
022400060321
022500060321      /free
022600060321
022700060321       //?se passato il moltiplicatore aggiorno quello passato
022800060321       if t05mult <> *blanks;
022900060321       mult = t05mult;
023000060321
023100090925       if t05mult='630';
023200090925       conta = contalav;
023300090925       contalav= 0;
023400060321       endif;
023500090925       if t05mult='640';
023600090925       conta = contacas;
023700090925       contacas= 0;
023800090925       endif;
023900091014       if t05mult='660';
024000090925       conta = contasup;
024100090925       contasup= 0;
024200090925       endif;
024300091014       if t05mult='650';
024400090925       conta = contaapp;
024500090925       contaapp= 0;
024600090925       endif;
024700090925       if t05mult='670';
024800090925       conta = contaffd;
024900090925       contaffd= 0;
025000090925       endif;
025100060321
025200060321       if conta > 0;
025300060322       exsr scrivitud;
025400060321       conta = 0;
025500060321       endif;
025600060321
025700060321       //?moltiplicatore non passato aggiorno tutti quelli previsti
025800060321       else;
025900060321
026000090925       //?richieste no rapide
026100090925       if contalav> 0;
026200090925       mult = '630';
026300090925       conta = contalav;
026400060322       exsr scrivitud;
026500060321       endif;
026600090925       if contacas> 0;
026700090925       mult = '640';
026800090925       conta = contacas;
026900090925       exsr scrivitud;
027000090925       endif;
027100090925       if contasup> 0;
027200091014       mult = '660';
027300090925       conta = contasup;
027400090925       exsr scrivitud;
027500090925       endif;
027600090925       if contaapp> 0;
027700091014       mult = '650';
027800090925       conta = contaapp;
027900090925       exsr scrivitud;
028000090925       endif;
028100090925       if contaffd> 0;
028200090925       mult = '670';
028300090925       conta = contaffd;
028400090925       exsr scrivitud;
028500090925       endif;
028600060321
028700060321       endif;
028800060321
028900060321      /end-free
029000060321
029100060321     c                   endsr
029200051227      *-----------------------------------------------------------------*
029300051228      * Scrivo/Aggiorno TNTUD
029400051227      *-----------------------------------------------------------------*
029500060321     c     SCrivitud     begsr
029600051229
029700051229      /free
029800051229
029900051229       //?imposto la data iso
030000051229       dataalfa = %subst(%char(savdim):3:6);
030100051229       dtaiso = %date(dataalfa:*ymd0);
030200051229
030300051229       //?imposto la chiave per tntud01l
030400051229       keytud.tudmult = mult;
030500051229       keytud.tudfil = savfgs;
030600051229       keytud.tuddta = dtaiso;
030700051229
030800051229       //?aggancio tntud01l
030900051229       chain %kds(keytud:3) tntud01l;
031000051229
031100051229       //?trovo rcd aggiorno se è un ricalcolo
031200051229       if %found(tntud01l);
031300051229       if t05mult <> *blanks or t05fil <> *zeros;
031400051229       tudqta = conta;
031500051229       update tntud000;
031600051229       else;
031700051229       if not%open(qsysprt);
031800051229       open qsysprt;
031900051229       except testa;
032000051229       endif;
032100051229       if *inof;
032200051229       except testa;
032300051229       endif;
032400051229       except riga;
032500051229       endif;
032600051229
032700051229       //?non trovo rcd scrivo
032800051229       else;
032900051229       clear tntud000;
033000051229       tudmult = mult;
033100051229       tuddta = dtaiso;
033200051229       tudfil = savfgs;
033300051229       tudcli = 0;
033400051229       tudqta = conta;
033500051229       write tntud000;
033600051229       endif;
033700051229
033800051229      /end-free
033900051227
034000051229     c                   endsr
034100140211
034200140211      /free
034300140211
034400140211       //----------------------------------------------------------------
034500140211       //?Caricamento Motivi Apertura R.A. da ESCLUDERE nel calcolo dei
034600140211       //?Tempi Ufficio
034700140211       //----------------------------------------------------------------
034800140211       BEGSR  sr_MAR_escl;
034900140211
035000140211         // -?Preparazione stringa SQL?
035100140211         wrkGetMARect = 'select substr(TBEke1, 1, 3) +
035200140211                           from TNTBE00F +
035300140211                          where TBEcod = ''MAR'' +
035400140211                            and TBEatb = '' '' +
035500140211                            and substr(TBEuni, 38, 1) = ''S'' +
035600140211                          order by TBEke1 +
035700140211                            for fetch only';
035800140211
035900140211         // -?Dichiarazione cursore?
036000140211         exec sql   prepare S2   from :wrkGetMARect;
036100140211         exec sql   declare C2   cursor for S2;
036200140211         if  SQLcode < *zero;
036300140211           exsr  sr_PrintERR;
036400140211         endif;
036500140211
036600140211         // -?Apertura del cursore?
036700140211         exec sql   open C2;
036800140211         if  SQLcode < *zero;
036900140211           exsr  sr_PrintERR;
037000140211         endif;
037100140211
037200140211         // -?Lettura cursore?
037300140211         exec sql   fetch next   from C2   into :w_MAR_1;
037400140211         DoW  SQLcode <> 100;
037500140211           // -?Rilevato errore SQL?
037600140211           if  SQLcode < *zero;
037700140211             exsr  sr_PrintERR;
037800140211           endif;
037900140211           // -?Memorizzazione Motivo Apertura R.A.?
038000140211           if  w_MARect = *blank;
038100140211             w_MARect = '(''';
038200140211           else;
038300140211             w_MARect += ', ''';
038400140211           endif;
038500140211           w_MARect += w_MAR_1 + '''';
038600140211           // -?Lettura rec. successivo?
038700140211           exec sql   fetch next   from C2   into :w_MAR_1;
038800140211         EndDo;
038900140211         if  w_MARect <> *blank;
039000140211           w_MARect += ')';
039100140211         endif;
039200140211
039300140211         // -?Chiusura cursore?
039400140211         exec sql   close C2;
039500140211
039600140211       ENDSR;
039700140211
039800140211       //--------------------------------------------------------------
039900140211       //?Stampa segnalazione dell'errore rilevato via SQL             ?
040000140211       //--------------------------------------------------------------
040100140211       BEGSR  sr_PrintERR;
040200140211
040300140211         // -?Stampa del Dump?
040400140211         Dump(A);
040500140211
040600140211         // -?Stampa del Job-Log?
040700140211         Qcmd = 'DSPJOBLOG job(*) output(*print)';
040800140211         exsr  sr_ExecCmd;
040900140211
041000140211         // -?Chiusra *pgm?
041100140211         exsr  srFine;
041200140211
041300140211       ENDSR;
041400140211
041500140211       //--------------------------------------------------------------
041600140211       //?Esecuzione del comando (già impostato).                      ?
041700140211       //--------------------------------------------------------------
041800140211       BEGSR  sr_ExecCmd;
041900140211
042000140211         clear Qcap0100;
042100140211         Qcabcsdh = *off;
042200140211         Qcapa    = *off;
042300140211         Qcacmdss = *off;
042400140211         Qcaerved = *allX'00';
042500140211
042600140211         clear Qusec;
042700140211         Qusbprv  = %size(Qusec);
042800140211
042900140211         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
043000140211                           %size(Qcap0100) : 'CPOP0100' : *omit :
043100140211                           0 : 0 : Qusec);
043200140211
043300140211         //if  Qusei <> *blank;
043400140211         //  ...;
043500140211         //endif;
043600140211
043700140211       ENDSR;
043800140211
043900140211      /end-free
044000051229
044100051229      *-----------------------------------------------------------------*
044200051229      * Fine programmma con codice errore
044300051229      *-----------------------------------------------------------------*
044400051229     c     srfine        begsr
044500051229
044600051229     c                   eval      *InLr = *On
044700051229     c                   return
044800051229
044900051229     c                   endsr
045000051227
045100051227      *-----------------------------------------------------------------*
045200051227      * Routine iniziale
045300051227      *-----------------------------------------------------------------*
045400051227     c     *inzsr        begsr
045500051227
045600051227     c     *entry        plist
045700051227     c                   parm                    kpjba
045800051227     c                   eval      tntu05ds = kpjbu
045900051227
046000051229     c                   endsr
046100051229
046200051227      *-----------------------------------------------------------------*
046300051229     oQsysprt   e            Testa          2 02
046400051229     o                                         +  5 'Rcd già esistente in'
046500051229     o                                         +  1 'TNTUD00F'
046600090925     o                                           85 'TNTU83R'
046700051229     o                       *Date         y    110
046800051229     o                                          128 'Pag.'
046900051229     o                       Page          z    132
047000051229     o          e            Riga           1
047100051229     o                                           +1 'Rcd:'
047200051229     o                       tudmult             +1
047300051229     o                       tudfil              +1
047400051229     o                       tuddta              +1
047500051229     o                       tudqta        4     +1

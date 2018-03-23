000100140213     /*PRM dbgview(*source)
000200140213     /*END
000300140213     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000400140213     h alwnull(*inputonly)
000500051227      *-----------------------------------------------------------------*
000600060104      *  Leggo file reclami
000700051227      *-----------------------------------------------------------------*
000800051227     ftntud01l  uf a e           k disk
000900051229     fqsysprt   o    f  132        printer oflind(*inof) usropn
001000051227
001100060322     d tgdconta        s                   like(tudqta)
001200060322     d conta           s                   like(tudqta)
001300060322     d containfo       s                   like(tudqta)
001400051229     d dataalfa        s              6
001500051227     d dtaiso          s               d   inz
001600051229     d mult            s                   like(tudmult)
001700060104     d savdim          s                   like(tgddac)
001800060104     d savfgs          s                   like(tgdpot)
001900060104     d keytud          ds                  likerec(tntud000:*key)
002000140213     d w_MAR_1         s                   like(TGDmad)  inz
002100140213     d w_MARect        s           1024    varying       inz
002200140213     d wrkGetMARect    s           1024    varying       inz
002300051229     d wrkgetlista     s           4096    varying
002400051227
002500051229     d dtup          e ds
002600060104     d fitgdds       e ds                  extname(fitgd00f)
002700051227     d kpjba         e ds
002800051227     d tntu05ds      e ds
002900060104     dtntudtads      e ds
003000140213
003100140213       //--------------------------------------------------------------
003200140213       //?Definizione prototipi procedure.                             ?
003300140213       //--------------------------------------------------------------
003400140213
003500140213       // -?Parametri API QCAPCMD (Process Commands)?
003600140213     d Qcmd            s           2048    inz  varying
003700140213      /copy qSysInc/qRpgleSrc,QCAPCMD
003800140213       // -?API QCAPCMD (Process Commands)?
003900140213      /copy gaitrasrc/srcProtoPR,QCAPCMD
004000140213
004100140213       // -?Parametri gestione errori API.?
004200140213      /copy qsysinc/qrpglesrc,QUSEC
004300051227
004400051227      *-----------------------------------------------------------------*
004500051227      * Moltiplicatori calcolati:
004600090925      *    374 - Richieste Info no contatti
004700051227      *-----------------------------------------------------------------*
004800051229
004900060104     c                   exsr      PREPASQL
005000060104     c
005100060104     c*  ?Aggiorno data ultima elaborazione in tabella TUP
005200060104     c                   if        t05mult=*blanks and t05fil=0
005300060104     c                   clear                   tntudtads
005400060104     c                   eval      §DTAazn = kcdaz
005500060104     c                   eval      §DTAdta = t05dtf
005600060104     c                   call      'TNTUDTAR'
005700060104     c                   parm                    tntudtads
005800060104     c                   endif
005900051229
006000051229     c                   Eval      *InLr = *On
006100051229
006200051229      *-----------------------------------------------------------------*
006300060104      * Lettura file fasi
006400051229      *-----------------------------------------------------------------*
006500060104     c     prepasql      begsr
006600051229
006700140213      * -?Impostazione opzioni per SQL?
006800140213     c/exec sql
006900140213     c+  set  option  DynUsrPrf = *Owner,
007000140213     c+               CloSqlCsr = *EndMod
007100140213     c/end-exec
007200140213      *
007300140213      *?Caricamento dei motivi apertura Richiesta Assistenza da escludere
007400140213      *?da queste statistiche
007500140213     c                   exsr      sr_MAR_escl
007600140213      *
007700051229      *?Imposto la parte fissa dell'istruzione sql
007800051229     c                   eval      wrkgetlista =
007900060321     c                             'select tgdpot, tgddac, tgdcdt, -
008000060104     c                             count(*) from fitgd00f where -
008001150626      * aggiungere selezione per chiuse TGDDCD <> 0
008100060104     c                             tgddac between '+
008200051229     c                             %editc(t05dti:'X') + ' and ' +
008300090925     c                             %editc(t05dtf:'X') + ' and +
008400090925     c                             substr(tgdflo, 1, 1) =  ''' +
008500090925     c                             ''''
008600140213      *?Esclusione dei motivi apertura Richiesta Assistenza
008700140213     c                   if        w_MARect <> *blank
008800140213     c                   eval      wrkgetlista = wrkgetlista +
008900140213     c                                 ' and TGDmad NOT in ' + w_MARect
009000140213     c                   endif
009100051229      *?Se passato un p.o.
009200051229     c                   if        t05fil <> 0
009300051229     c                   eval      wrkgetlista = wrkgetlista +
009400060104     c                             ' and digits(tgdpot) = ' +
009500051229     c                             %triml(%editc(t05fil:'Z'))
009600051229     c                   endif
009700051229      *?Ordinamento
009800051229     c                   eval      wrkgetlista = wrkgetlista +
009900090925     c                             ' group by tgdpot, tgddac, tgdcdt'
010000060322      *?Ordinamento
010100060322     c                   eval      wrkgetlista = wrkgetlista +
010200060322     c                             ' order by tgdpot, tgddac, tgdcdt'
010300051229      *?Solo lettura
010400051229     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
010500051229
010600051229     C/EXEC SQL
010700051229     C+ PREPARE s1 FROM :wrkgetlista
010800051229     C/END-EXEC
010900051229     C
011000051229     C/EXEC SQL
011100051229     C+ DECLARE A1 CURSOR FOR S1
011200051229     C/END-EXEC
011300051229     C
011400051229     C/EXEC SQL
011500051229     C+ OPEN a1
011600051229     C/END-EXEC
011700051229
011800051229     c                   clear                   conta
011900051229      *?Leggo il file
012000051229     c                   do        *hival
012100051229     C/EXEC SQL
012200090925     C+ FETCH NEXT FROM a1 INTO :tgdpot, :tgddac, :tgdcdt,
012300090925     c+                         :tgdconta
012400051229     C/END-EXEC
012500051229
012600051229     c                   select
012700051229
012800051229     c                   when      sqlcod = 100
012900051229     c                   leave
013000051229     c                   when      sqlcod < 0
013100051229     c                   exsr      srfine
013200051229     c                   other
013300140217     c*
013400060321     c* Sommo e a cambio p.o. o data scrivo
013500060322     c                   EXSR      Contatgd
013600140217     c*
013700051229     c                   endsl
013800051229
013900051229     c                   enddo
014000140217
014100140217      * Scrivo ultimi totali
014200140217     c                   if        savdim > *zeros
014300140217     c                   exsr      aggiorna
014400140217     c                   endif
014500051229
014600051229     C/EXEC SQL
014700051229     C+ CLOSE a1
014800051229     C/END-EXEC
014900051229
015000051229     c                   endsr
015100060321      *-----------------------------------------------------------------*
015200060321      * Sommo INformative dalle altre e a rottura aggiorno
015300060321      *-----------------------------------------------------------------*
015400060322     c     contatgd      begsr
015500060321
015600060321      /free
015700060321
015800060321       //?a rottura di data aggiorno
015900060321       if tgddac <> savdim;
016000060321       if savdim > *zeros;
016100060321       exsr aggiorna;
016200060321       containfo= 0;
016300060321       endif;
016400060321       savdim = tgddac;
016500060321       savfgs = tgdpot;
016600060321       endif;
016700060321
016800060321       //?a rottura di p.o. aggiorno
016900060321       if tgdpot <> savfgs;
017000060321       if savfgs > *zeros;
017100060321       exsr aggiorna;
017200060321       containfo= 0;
017300060321       endif;
017400060321       savfgs = tgdpot;
017500060321       endif;
017600060321
017700060321       //?sommo le informative
017800060322       if tgdcdt = '  I';
017900060322       containfo= containfo+ tgdconta;
018000060322       endif;
018100060321
018200060321      /end-free
018300060321
018400060321     c                   endsr
018500060321      *-----------------------------------------------------------------*
018600060321      * Imposto cosa devo aggiornare
018700060321      *-----------------------------------------------------------------*
018800060321     c     aggiorna      begsr
018900060321
019000060321      /free
019100060321
019200060321       //?se passato il moltiplicatore aggiorno quello passato
019300060321       if t05mult <> *blanks;
019400060321       mult = t05mult;
019500060321
019600090925       if t05mult='374';
019700060321       conta = containfo;
019800060321       containfo= 0;
019900060321       endif;
020000060321
020100060321       if conta > 0;
020200060322       exsr scrivitud;
020300060321       conta = 0;
020400060321       endif;
020500060321
020600060321       //?moltiplicatore non passato aggiorno tutti quelli previsti
020700060321       else;
020800060321
020900060321       //?richeiste solo informative
021000060321       if containfo> 0;
021100090925       mult = '374';
021200060321       conta = containfo;
021300060322       exsr scrivitud;
021400060321       endif;
021500060321
021600060321       endif;
021700060321
021800060321      /end-free
021900060321
022000060321     c                   endsr
022100051227      *-----------------------------------------------------------------*
022200051228      * Scrivo/Aggiorno TNTUD
022300051227      *-----------------------------------------------------------------*
022400060321     c     SCrivitud     begsr
022500051229
022600051229      /free
022700051229
022800051229       //?imposto la data iso
022900051229       dataalfa = %subst(%char(savdim):3:6);
023000051229       dtaiso = %date(dataalfa:*ymd0);
023100051229
023200051229       //?imposto la chiave per tntud01l
023300051229       keytud.tudmult = mult;
023400051229       keytud.tudfil = savfgs;
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
026100051229       tudfil = savfgs;
026200051229       tudcli = 0;
026300051229       tudqta = conta;
026400051229       write tntud000;
026500051229       endif;
026600051229
026700051229      /end-free
026800051227
026900051229     c                   endsr
027000140213
027100140213      /free
027200140213
027300140213       //----------------------------------------------------------------
027400140213       //?Caricamento Motivi Apertura R.A. da ESCLUDERE nel calcolo dei
027500140213       //?Tempi Ufficio
027600140213       //----------------------------------------------------------------
027700140213       BEGSR  sr_MAR_escl;
027800140213
027900140213         // -?Preparazione stringa SQL?
028000140213         wrkGetMARect = 'select substr(TBEke1, 1, 3) +
028100140213                           from TNTBE00F +
028200140213                          where TBEcod = ''MAR'' +
028300140213                            and TBEatb = '' '' +
028400140213                            and substr(TBEuni, 38, 1) = ''S'' +
028500140213                          order by TBEke1 +
028600140213                            for fetch only';
028700140213
028800140213         // -?Dichiarazione cursore?
028900140213         exec sql   prepare S2   from :wrkGetMARect;
029000140213         exec sql   declare C2   cursor for S2;
029100140213         if  SQLcode < *zero;
029200140213           exsr  sr_PrintERR;
029300140213         endif;
029400140213
029500140213         // -?Apertura del cursore?
029600140213         exec sql   open C2;
029700140213         if  SQLcode < *zero;
029800140213           exsr  sr_PrintERR;
029900140213         endif;
030000140213
030100140213         // -?Lettura cursore?
030200140213         exec sql   fetch next   from C2   into :w_MAR_1;
030300140213         DoW  SQLcode <> 100;
030400140213           // -?Rilevato errore SQL?
030500140213           if  SQLcode < *zero;
030600140213             exsr  sr_PrintERR;
030700140213           endif;
030800140213           // -?Memorizzazione Motivo Apertura R.A.?
030900140213           if  w_MARect = *blank;
031000140213             w_MARect = '(''';
031100140213           else;
031200140213             w_MARect += ', ''';
031300140213           endif;
031400140213           w_MARect += w_MAR_1 + '''';
031500140213           // -?Lettura rec. successivo?
031600140213           exec sql   fetch next   from C2   into :w_MAR_1;
031700140213         EndDo;
031800140213         if  w_MARect <> *blank;
031900140213           w_MARect += ')';
032000140213         endif;
032100140213
032200140213         // -?Chiusura cursore?
032300140213         exec sql   close C2;
032400140213
032500140213       ENDSR;
032600140213
032700140213       //--------------------------------------------------------------
032800140213       //?Stampa segnalazione dell'errore rilevato via SQL             ?
032900140213       //--------------------------------------------------------------
033000140213       BEGSR  sr_PrintERR;
033100140213
033200140213         // -?Stampa del Dump?
033300140213         Dump(A);
033400140213
033500140213         // -?Stampa del Job-Log?
033600140213         Qcmd = 'DSPJOBLOG job(*) output(*print)';
033700140213         exsr  sr_ExecCmd;
033800140213
033900140213         // -?Chiusra *pgm?
034000140213         exsr  srFine;
034100140213
034200140213       ENDSR;
034300140213
034400140213       //--------------------------------------------------------------
034500140213       //?Esecuzione del comando (già impostato).                      ?
034600140213       //--------------------------------------------------------------
034700140213       BEGSR  sr_ExecCmd;
034800140213
034900140213         clear Qcap0100;
035000140213         Qcabcsdh = *off;
035100140213         Qcapa    = *off;
035200140213         Qcacmdss = *off;
035300140213         Qcaerved = *allX'00';
035400140213
035500140213         clear Qusec;
035600140213         Qusbprv  = %size(Qusec);
035700140213
035800140213         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
035900140213                           %size(Qcap0100) : 'CPOP0100' : *omit :
036000140213                           0 : 0 : Qusec);
036100140213
036200140213         //if  Qusei <> *blank;
036300140213         //  ...;
036400140213         //endif;
036500140213
036600140213       ENDSR;
036700140213
036800140213      /end-free
036900051229
037000051229      *-----------------------------------------------------------------*
037100051229      * Fine programmma con codice errore
037200051229      *-----------------------------------------------------------------*
037300051229     c     srfine        begsr
037400051229
037500051229     c                   eval      *InLr = *On
037600051229     c                   return
037700051229
037800051229     c                   endsr
037900051227
038000051227      *-----------------------------------------------------------------*
038100051227      * Routine iniziale
038200051227      *-----------------------------------------------------------------*
038300051227     c     *inzsr        begsr
038400051227
038500051227     c     *entry        plist
038600051227     c                   parm                    kpjba
038700051227     c                   eval      tntu05ds = kpjbu
038800051227
038900051229     c                   endsr
039000051229
039100051227      *-----------------------------------------------------------------*
039200051229     oQsysprt   e            Testa          2 02
039300051229     o                                         +  5 'Rcd già esistente in'
039400051229     o                                         +  1 'TNTUD00F'
039500090925     o                                           85 'TNTU87R'
039600051229     o                       *Date         y    110
039700051229     o                                          128 'Pag.'
039800051229     o                       Page          z    132
039900051229     o          e            Riga           1
040000051229     o                                           +1 'Rcd:'
040100051229     o                       tudmult             +1
040200051229     o                       tudfil              +1
040300051229     o                       tuddta              +1
040400051229     o                       tudqta        4     +1

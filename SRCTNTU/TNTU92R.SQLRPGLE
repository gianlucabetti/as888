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
001400060321     d contanoinfo     s                   like(tudqta)
001500051229     d dataalfa        s              6
001600051227     d dtaiso          s               d   inz
001700051229     d mult            s                   like(tudmult)
001800060104     d savdim          s                   like(tgddac)
001900060104     d savfgs          s                   like(tgdpot)
002000060104     d keytud          ds                  likerec(tntud000:*key)
002100140213     d w_MAR_1         s                   like(TGDmad)  inz
002200140213     d w_MARect        s           1024    varying       inz
002300140213     d wrkGetMARect    s           1024    varying       inz
002400051229     d wrkgetlista     s           4096    varying
002500051227
002600051229     d dtup          e ds
002700060104     d fitgdds       e ds                  extname(fitgd00f)
002800051227     d kpjba         e ds
002900051227     d tntu05ds      e ds
003000060104     dtntudtads      e ds
003100140213
003200140213       //--------------------------------------------------------------
003300140213       //?Definizione prototipi procedure.                             ?
003400140213       //--------------------------------------------------------------
003500140213
003600140213       // -?Parametri API QCAPCMD (Process Commands)?
003700140213     d Qcmd            s           2048    inz  varying
003800140213      /copy qSysInc/qRpgleSrc,QCAPCMD
003900140213       // -?API QCAPCMD (Process Commands)?
004000140213      /copy gaitrasrc/srcProtoPR,QCAPCMD
004100140213
004200140213       // -?Parametri gestione errori API.?
004300140213      /copy qsysinc/qrpglesrc,QUSEC
004400051227
004500051227      *-----------------------------------------------------------------*
004600051227      * Moltiplicatori calcolati:
004700060104      *    370 - telefonale di reclamo
004800060321      *    372 - telefonale di reclamo
004900051227      *-----------------------------------------------------------------*
005000051229
005100060104     c                   exsr      PREPASQL
005200060104     c
005300060104     c*  ?Aggiorno data ultima elaborazione in tabella TUP
005400060104     c                   if        t05mult=*blanks and t05fil=0
005500060104     c                   clear                   tntudtads
005600060104     c                   eval      §DTAazn = kcdaz
005700060104     c                   eval      §DTAdta = t05dtf
005800060104     c                   call      'TNTUDTAR'
005900060104     c                   parm                    tntudtads
006000060104     c                   endif
006100051229
006200051229     c                   Eval      *InLr = *On
006300051229
006400051229      *-----------------------------------------------------------------*
006500060104      * Lettura file fasi
006600051229      *-----------------------------------------------------------------*
006700060104     c     prepasql      begsr
006800051229
006900140213      * -?Impostazione opzioni per SQL?
007000140213     c/exec sql
007100140213     c+  set  option  DynUsrPrf = *Owner,
007200140213     c+               CloSqlCsr = *EndMod
007300140213     c/end-exec
007400140213      *
007500140213      *?Caricamento dei motivi apertura Richiesta Assistenza da escludere
007600140213      *?da queste statistiche
007700140213     c                   exsr      sr_MAR_escl
007800140213      *
007900051229      *?Imposto la parte fissa dell'istruzione sql
008000051229     c                   eval      wrkgetlista =
008100060321     c                             'select tgdpot, tgddac, tgdcdt, -
008200060104     c                             count(*) from fitgd00f where -
008300060104     c                             tgddac between '+
008400051229     c                             %editc(t05dti:'X') + ' and ' +
008500051229     c                             %editc(t05dtf:'X')
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
009900060321     c                             ' group by tgdpot, tgddac, tgdcdt'
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
012200060322     C+ FETCH NEXT FROM a1 INTO : tgdpot, :tgddac, :tgdcdt, :tgdconta
012300051229     C/END-EXEC
012400051229
012500051229     c                   select
012600051229
012700051229     c                   when      sqlcod = 100
012800051229     c                   leave
012900051229     c                   when      sqlcod < 0
013000051229     c                   seton                                        H1
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
016300060321       contanoinfo= 0;
016400060321       endif;
016500060321       savdim = tgddac;
016600060321       savfgs = tgdpot;
016700060321       endif;
016800060321
016900060321       //?a rottura di p.o. aggiorno
017000060321       if tgdpot <> savfgs;
017100060321       if savfgs > *zeros;
017200060321       exsr aggiorna;
017300060321       containfo= 0;
017400060321       contanoinfo= 0;
017500060321       endif;
017600060321       savfgs = tgdpot;
017700060321       endif;
017800060321
017900060321       //?sommo le informative
018000060322       if tgdcdt = '  I';
018100060322       containfo= containfo+ tgdconta;
018200060322       endif;
018300060322       if tgdcdt <> '  I';
018400060322       contanoinfo= contanoinfo+ tgdconta;
018500060321       endif;
018600060321
018700060321      /end-free
018800060321
018900060321     c                   endsr
019000060321      *-----------------------------------------------------------------*
019100060321      * Imposto cosa devo aggiornare
019200060321      *-----------------------------------------------------------------*
019300060321     c     aggiorna      begsr
019400060321
019500060321      /free
019600060321
019700060321       //?se passato il moltiplicatore aggiorno quello passato
019800060321       if t05mult <> *blanks;
019900060321       mult = t05mult;
020000060321
020100060321       if t05mult='370';
020200060321       conta = containfo;
020300060321       containfo= 0;
020400060321       endif;
020500060321       if t05mult='372';
020600060321       conta = contanoinfo;
020700060321       contanoinfo= 0;
020800060321       endif;
020900060321
021000060321       if conta > 0;
021100060322       exsr scrivitud;
021200060321       conta = 0;
021300060321       endif;
021400060321
021500060321       //?moltiplicatore non passato aggiorno tutti quelli previsti
021600060321       else;
021700060321
021800060321       //?richeiste solo informative
021900060321       if containfo> 0;
022000060321       mult = '370';
022100060321       conta = containfo;
022200060322       exsr scrivitud;
022300060321       endif;
022400060321       if contanoinfo> 0;
022500060321       mult = '372';
022600060321       conta = contanoinfo;
022700060322       exsr scrivitud;
022800060321       endif;
022900060321
023000060321       endif;
023100060321
023200060321      /end-free
023300060321
023400060321     c                   endsr
023500051227      *-----------------------------------------------------------------*
023600051228      * Scrivo/Aggiorno TNTUD
023700051227      *-----------------------------------------------------------------*
023800060321     c     SCrivitud     begsr
023900051229
024000051229      /free
024100051229
024200051229       //?imposto la data iso
024300051229       dataalfa = %subst(%char(savdim):3:6);
024400051229       dtaiso = %date(dataalfa:*ymd0);
024500051229
024600051229       //?imposto la chiave per tntud01l
024700051229       keytud.tudmult = mult;
024800051229       keytud.tudfil = savfgs;
024900051229       keytud.tuddta = dtaiso;
025000051229
025100051229       //?aggancio tntud01l
025200051229       chain %kds(keytud:3) tntud01l;
025300051229
025400051229       //?trovo rcd aggiorno se è un ricalcolo
025500051229       if %found(tntud01l);
025600051229       if t05mult <> *blanks or t05fil <> *zeros;
025700051229       tudqta = conta;
025800051229       update tntud000;
025900051229       else;
026000051229       if not%open(qsysprt);
026100051229       open qsysprt;
026200051229       except testa;
026300051229       endif;
026400051229       if *inof;
026500051229       except testa;
026600051229       endif;
026700051229       except riga;
026800051229       endif;
026900051229
027000051229       //?non trovo rcd scrivo
027100051229       else;
027200051229       clear tntud000;
027300051229       tudmult = mult;
027400051229       tuddta = dtaiso;
027500051229       tudfil = savfgs;
027600051229       tudcli = 0;
027700051229       tudqta = conta;
027800051229       write tntud000;
027900051229       endif;
028000051229
028100051229      /end-free
028200051227
028300051229     c                   endsr
028400140213
028500140213      /free
028600140213
028700140213       //----------------------------------------------------------------
028800140213       //?Caricamento Motivi Apertura R.A. da ESCLUDERE nel calcolo dei
028900140213       //?Tempi Ufficio
029000140213       //----------------------------------------------------------------
029100140213       BEGSR  sr_MAR_escl;
029200140213
029300140213         // -?Preparazione stringa SQL?
029400140213         wrkGetMARect = 'select substr(TBEke1, 1, 3) +
029500140213                           from TNTBE00F +
029600140213                          where TBEcod = ''MAR'' +
029700140213                            and TBEatb = '' '' +
029800140213                            and substr(TBEuni, 38, 1) = ''S'' +
029900140213                          order by TBEke1 +
030000140213                            for fetch only';
030100140213
030200140213         // -?Dichiarazione cursore?
030300140213         exec sql   prepare S2   from :wrkGetMARect;
030400140213         exec sql   declare C2   cursor for S2;
030500140213         if  SQLcode < *zero;
030600140213           exsr  sr_PrintERR;
030700140213         endif;
030800140213
030900140213         // -?Apertura del cursore?
031000140213         exec sql   open C2;
031100140213         if  SQLcode < *zero;
031200140213           exsr  sr_PrintERR;
031300140213         endif;
031400140213
031500140213         // -?Lettura cursore?
031600140213         exec sql   fetch next   from C2   into :w_MAR_1;
031700140213         DoW  SQLcode <> 100;
031800140213           // -?Rilevato errore SQL?
031900140213           if  SQLcode < *zero;
032000140213             exsr  sr_PrintERR;
032100140213           endif;
032200140213           // -?Memorizzazione Motivo Apertura R.A.?
032300140213           if  w_MARect = *blank;
032400140213             w_MARect = '(''';
032500140213           else;
032600140213             w_MARect += ', ''';
032700140213           endif;
032800140213           w_MARect += w_MAR_1 + '''';
032900140213           // -?Lettura rec. successivo?
033000140213           exec sql   fetch next   from C2   into :w_MAR_1;
033100140213         EndDo;
033200140213         if  w_MARect <> *blank;
033300140213           w_MARect += ')';
033400140213         endif;
033500140213
033600140213         // -?Chiusura cursore?
033700140213         exec sql   close C2;
033800140213
033900140213       ENDSR;
034000140213
034100140213       //--------------------------------------------------------------
034200140213       //?Stampa segnalazione dell'errore rilevato via SQL             ?
034300140213       //--------------------------------------------------------------
034400140213       BEGSR  sr_PrintERR;
034500140213
034600140213         // -?Stampa del Dump?
034700140213         Dump(A);
034800140213
034900140213         // -?Stampa del Job-Log?
035000140213         Qcmd = 'DSPJOBLOG job(*) output(*print)';
035100140213         exsr  sr_ExecCmd;
035200140213
035300140213         // -?Chiusra *pgm?
035400140213         exsr  srFine;
035500140213
035600140213       ENDSR;
035700140213
035800140213       //--------------------------------------------------------------
035900140213       //?Esecuzione del comando (già impostato).                      ?
036000140213       //--------------------------------------------------------------
036100140213       BEGSR  sr_ExecCmd;
036200140213
036300140213         clear Qcap0100;
036400140213         Qcabcsdh = *off;
036500140213         Qcapa    = *off;
036600140213         Qcacmdss = *off;
036700140213         Qcaerved = *allX'00';
036800140213
036900140213         clear Qusec;
037000140213         Qusbprv  = %size(Qusec);
037100140213
037200140213         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
037300140213                           %size(Qcap0100) : 'CPOP0100' : *omit :
037400140213                           0 : 0 : Qusec);
037500140213
037600140213         //if  Qusei <> *blank;
037700140213         //  ...;
037800140213         //endif;
037900140213
038000140213       ENDSR;
038100140213
038200140213      /end-free
038300051229
038400051229      *-----------------------------------------------------------------*
038500051229      * Fine programmma con codice errore
038600051229      *-----------------------------------------------------------------*
038700051229     c     srfine        begsr
038800051229
038900051229     c                   eval      *InLr = *On
039000051229     c                   return
039100051229
039200051229     c                   endsr
039300051227
039400051227      *-----------------------------------------------------------------*
039500051227      * Routine iniziale
039600051227      *-----------------------------------------------------------------*
039700051227     c     *inzsr        begsr
039800051227
039900051227     c     *entry        plist
040000051227     c                   parm                    kpjba
040100051227     c                   eval      tntu05ds = kpjbu
040200051227
040300051229     c                   endsr
040400051229
040500051227      *-----------------------------------------------------------------*
040600051229     oQsysprt   e            Testa          2 02
040700051229     o                                         +  5 'Rcd già esistente in'
040800051229     o                                         +  1 'TNTUD00F'
040900060104     o                                           85 'TNTU92R'
041000051229     o                       *Date         y    110
041100051229     o                                          128 'Pag.'
041200051229     o                       Page          z    132
041300051229     o          e            Riga           1
041400051229     o                                           +1 'Rcd:'
041500051229     o                       tudmult             +1
041600051229     o                       tudfil              +1
041700051229     o                       tuddta              +1
041800051229     o                       tudqta        4     +1

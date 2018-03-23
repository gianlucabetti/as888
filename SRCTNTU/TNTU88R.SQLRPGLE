000100140213     /*PRM dbgview(*source)
000200140213     /*END
000300140213     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000400140213     h alwnull(*inputonly)
000500051227      *-----------------------------------------------------------------*
000600060320      *  Leggo file reclami x data chiusura
000700051227      *-----------------------------------------------------------------*
000800051227     ftntud01l  uf a e           k disk
000900051229     fqsysprt   o    f  132        printer oflind(*inof) usropn
001000051227
001100051229     d conta           s                   like(tudqta)
001200051229     d dataalfa        s              6
001300051227     d dtaiso          s               d   inz
001400051229     d mult            s                   like(tudmult)
001500060104     d savdim          s                   like(tgddac)
001600060104     d savfgs          s                   like(tgdpot)
001700060104     d keytud          ds                  likerec(tntud000:*key)
001800140213     d w_MAR_1         s                   like(TGDmad)  inz
001900140213     d w_MARect        s           1024    varying       inz
002000140213     d wrkGetMARect    s           1024    varying       inz
002100051229     d wrkgetlista     s           4096    varying
002200051227
002300051229     d dtup          e ds
002400060104     d fitgdds       e ds                  extname(fitgd00f)
002500051227     d kpjba         e ds
002600051227     d tntu05ds      e ds
002700060104     dtntudtads      e ds
002800140213
002900140213       //--------------------------------------------------------------
003000140213       //?Definizione prototipi procedure.                             ?
003100140213       //--------------------------------------------------------------
003200140213
003300140213       // -?Parametri API QCAPCMD (Process Commands)?
003400140213     d Qcmd            s           2048    inz  varying
003500140213      /copy qSysInc/qRpgleSrc,QCAPCMD
003600140213       // -?API QCAPCMD (Process Commands)?
003700140213      /copy gaitrasrc/srcProtoPR,QCAPCMD
003800140213
003900140213       // -?Parametri gestione errori API.?
004000140213      /copy qsysinc/qrpglesrc,QUSEC
004100051227
004200051227      *-----------------------------------------------------------------*
004300051227      * Moltiplicatori calcolati:
004400060320      *    375 - imputazione reclami x data chiusura
004500051227      *-----------------------------------------------------------------*
004600051229
004700060104     c                   exsr      PREPASQL
004800060104     c
004900060104     c*  ?Aggiorno data ultima elaborazione in tabella TUP
005000060104     c                   if        t05mult=*blanks and t05fil=0
005100060104     c                   clear                   tntudtads
005200060104     c                   eval      §DTAazn = kcdaz
005300060104     c                   eval      §DTAdta = t05dtf
005400060104     c                   call      'TNTUDTAR'
005500060104     c                   parm                    tntudtads
005600060104     c                   endif
005700051229
005800051229     c                   Eval      *InLr = *On
005900051229
006000051229      *-----------------------------------------------------------------*
006100060104      * Lettura file fasi
006200051229      *-----------------------------------------------------------------*
006300060104     c     prepasql      begsr
006400051229
006500140213      * -?Impostazione opzioni per SQL?
006600140213     c/exec sql
006700140213     c+  set  option  DynUsrPrf = *Owner,
006800140213     c+               CloSqlCsr = *EndMod
006900140213     c/end-exec
007000140213      *
007100140213      *?Caricamento dei motivi apertura Richiesta Assistenza da escludere
007200140213      *?da queste statistiche
007300140213     c                   exsr      sr_MAR_escl
007400140213      *
007500051229      *?Imposto la parte fissa dell'istruzione sql
007600051229     c                   eval      wrkgetlista =
007700060320     c                             'select tgdpor, tgddcd, -
007800060104     c                             count(*) from fitgd00f where -
007900060320     c                             tgddcd between '+
008000051229     c                             %editc(t05dti:'X') + ' and ' +
008100060320     c                             %editc(t05dtf:'X') + ' and ' +
008200060320     c                             'tgdpor>0'
008300140213      *?Esclusione dei motivi apertura Richiesta Assistenza
008400140213     c                   if        w_MARect <> *blank
008500140213     c                   eval      wrkgetlista = wrkgetlista +
008600140213     c                                 ' and TGDmad NOT in ' + w_MARect
008700140213     c                   endif
008800051229      *?Se passato un p.o.
008900051229     c                   if        t05fil <> 0
009000051229     c                   eval      wrkgetlista = wrkgetlista +
009100060320     c                             ' and digits(tgdpor) = ' +
009200051229     c                             %triml(%editc(t05fil:'Z'))
009300051229     c                   endif
009400051229      *?Ordinamento
009500051229     c                   eval      wrkgetlista = wrkgetlista +
009600060320     c                             ' group by tgdpor, tgddcd'
009700051229      *?Solo lettura
009800051229     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
009900051229
010000051229     C/EXEC SQL
010100051229     C+ PREPARE s1 FROM :wrkgetlista
010200051229     C/END-EXEC
010300051229     C
010400051229     C/EXEC SQL
010500051229     C+ DECLARE A1 CURSOR FOR S1
010600051229     C/END-EXEC
010700051229     C
010800051229     C/EXEC SQL
010900051229     C+ OPEN a1
011000051229     C/END-EXEC
011100051229
011200051229     c                   clear                   conta
011300051229      *?Leggo il file
011400051229     c                   do        *hival
011500051229     C/EXEC SQL
011600060104     C+ FETCH NEXT FROM a1 INTO : savfgs, :savdim, :conta
011700051229     C/END-EXEC
011800051229
011900051229     c                   select
012000051229
012100051229     c                   when      sqlcod = 100
012200051229     c                   leave
012300051229     c                   when      sqlcod < 0
012400051229     c                   seton                                        H1
012500051229     c                   exsr      srfine
012600051229     c                   other
012700060104     c
012800060320     c                   eval      mult = '375'
012900060104     c
013000051229     c                   exsr      scrivitntu
013100060104     c
013200051229     c                   endsl
013300051229
013400051229     c                   enddo
013500051229
013600051229     C/EXEC SQL
013700051229     C+ CLOSE a1
013800051229     C/END-EXEC
013900051229
014000051229     c                   endsr
014100051227      *-----------------------------------------------------------------*
014200051228      * Scrivo/Aggiorno TNTUD
014300051227      *-----------------------------------------------------------------*
014400051227     c     scrivitntu    begsr
014500051229
014600051229      /free
014700051229
014800051229       //?imposto la data iso
014900051229       dataalfa = %subst(%char(savdim):3:6);
015000051229       dtaiso = %date(dataalfa:*ymd0);
015100051229
015200051229       //?imposto la chiave per tntud01l
015300051229       keytud.tudmult = mult;
015400051229       keytud.tudfil = savfgs;
015500051229       keytud.tuddta = dtaiso;
015600051229
015700051229       //?aggancio tntud01l
015800051229       chain %kds(keytud:3) tntud01l;
015900051229
016000051229       //?trovo rcd aggiorno se è un ricalcolo
016100051229       if %found(tntud01l);
016200051229       if t05mult <> *blanks or t05fil <> *zeros;
016300051229       tudqta = conta;
016400051229       update tntud000;
016500051229       else;
016600051229       if not%open(qsysprt);
016700051229       open qsysprt;
016800051229       except testa;
016900051229       endif;
017000051229       if *inof;
017100051229       except testa;
017200051229       endif;
017300051229       except riga;
017400051229       endif;
017500051229
017600051229       //?non trovo rcd scrivo
017700051229       else;
017800051229       clear tntud000;
017900051229       tudmult = mult;
018000051229       tuddta = dtaiso;
018100051229       tudfil = savfgs;
018200051229       tudcli = 0;
018300051229       tudqta = conta;
018400051229       write tntud000;
018500051229       endif;
018600051229
018700051229      /end-free
018800051227
018900051229     c                   endsr
019000140213
019100140213      /free
019200140213
019300140213       //----------------------------------------------------------------
019400140213       //?Caricamento Motivi Apertura R.A. da ESCLUDERE nel calcolo dei
019500140213       //?Tempi Ufficio
019600140213       //----------------------------------------------------------------
019700140213       BEGSR  sr_MAR_escl;
019800140213
019900140213         // -?Preparazione stringa SQL?
020000140213         wrkGetMARect = 'select substr(TBEke1, 1, 3) +
020100140213                           from TNTBE00F +
020200140213                          where TBEcod = ''MAR'' +
020300140213                            and TBEatb = '' '' +
020400140213                            and substr(TBEuni, 38, 1) = ''S'' +
020500140213                          order by TBEke1 +
020600140213                            for fetch only';
020700140213
020800140213         // -?Dichiarazione cursore?
020900140213         exec sql   prepare S2   from :wrkGetMARect;
021000140213         exec sql   declare C2   cursor for S2;
021100140213         if  SQLcode < *zero;
021200140213           exsr  sr_PrintERR;
021300140213         endif;
021400140213
021500140213         // -?Apertura del cursore?
021600140213         exec sql   open C2;
021700140213         if  SQLcode < *zero;
021800140213           exsr  sr_PrintERR;
021900140213         endif;
022000140213
022100140213         // -?Lettura cursore?
022200140213         exec sql   fetch next   from C2   into :w_MAR_1;
022300140213         DoW  SQLcode <> 100;
022400140213           // -?Rilevato errore SQL?
022500140213           if  SQLcode < *zero;
022600140213             exsr  sr_PrintERR;
022700140213           endif;
022800140213           // -?Memorizzazione Motivo Apertura R.A.?
022900140213           if  w_MARect = *blank;
023000140213             w_MARect = '(''';
023100140213           else;
023200140213             w_MARect += ', ''';
023300140213           endif;
023400140213           w_MARect += w_MAR_1 + '''';
023500140213           // -?Lettura rec. successivo?
023600140213           exec sql   fetch next   from C2   into :w_MAR_1;
023700140213         EndDo;
023800140213         if  w_MARect <> *blank;
023900140213           w_MARect += ')';
024000140213         endif;
024100140213
024200140213         // -?Chiusura cursore?
024300140213         exec sql   close C2;
024400140213
024500140213       ENDSR;
024600140213
024700140213       //--------------------------------------------------------------
024800140213       //?Stampa segnalazione dell'errore rilevato via SQL             ?
024900140213       //--------------------------------------------------------------
025000140213       BEGSR  sr_PrintERR;
025100140213
025200140213         // -?Stampa del Dump?
025300140213         Dump(A);
025400140213
025500140213         // -?Stampa del Job-Log?
025600140213         Qcmd = 'DSPJOBLOG job(*) output(*print)';
025700140213         exsr  sr_ExecCmd;
025800140213
025900140213         // -?Chiusra *pgm?
026000140213         exsr  srFine;
026100140213
026200140213       ENDSR;
026300140213
026400140213       //--------------------------------------------------------------
026500140213       //?Esecuzione del comando (già impostato).                      ?
026600140213       //--------------------------------------------------------------
026700140213       BEGSR  sr_ExecCmd;
026800140213
026900140213         clear Qcap0100;
027000140213         Qcabcsdh = *off;
027100140213         Qcapa    = *off;
027200140213         Qcacmdss = *off;
027300140213         Qcaerved = *allX'00';
027400140213
027500140213         clear Qusec;
027600140213         Qusbprv  = %size(Qusec);
027700140213
027800140213         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
027900140213                           %size(Qcap0100) : 'CPOP0100' : *omit :
028000140213                           0 : 0 : Qusec);
028100140213
028200140213         //if  Qusei <> *blank;
028300140213         //  ...;
028400140213         //endif;
028500140213
028600140213       ENDSR;
028700140213
028800140213      /end-free
028900051229
029000051229      *-----------------------------------------------------------------*
029100051229      * Fine programmma con codice errore
029200051229      *-----------------------------------------------------------------*
029300051229     c     srfine        begsr
029400051229
029500051229     c                   eval      *InLr = *On
029600051229     c                   return
029700051229
029800051229     c                   endsr
029900051227
030000051227      *-----------------------------------------------------------------*
030100051227      * Routine iniziale
030200051227      *-----------------------------------------------------------------*
030300051227     c     *inzsr        begsr
030400051227
030500051227     c     *entry        plist
030600051227     c                   parm                    kpjba
030700051227     c                   eval      tntu05ds = kpjbu
030800051227
030900051229     c                   endsr
031000051229
031100051227      *-----------------------------------------------------------------*
031200051229     oQsysprt   e            Testa          2 02
031300051229     o                                         +  5 'Rcd già esistente in'
031400051229     o                                         +  1 'TNTUD00F'
031500060320     o                                           85 'TNTU88R'
031600051229     o                       *Date         y    110
031700051229     o                                          128 'Pag.'
031800051229     o                       Page          z    132
031900051229     o          e            Riga           1
032000051229     o                                           +1 'Rcd:'
032100051229     o                       tudmult             +1
032200051229     o                       tudfil              +1
032300051229     o                       tuddta              +1
032400051229     o                       tudqta        4     +1

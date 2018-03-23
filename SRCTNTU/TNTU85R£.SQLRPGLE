000100051229      *PARMS CLOSQLCSR(*ENDMOD)
000200051229      *PARMS DYNUSRPRF(*OWNER)
000300051227      *-----------------------------------------------------------------*
000400060509      *  Legge responsabilità danni
000500051227      *-----------------------------------------------------------------*
000600051227     ftntud01l  uf a e           k disk
000700051229     fqsysprt   o    f  132        printer oflind(*inof) usropn
000800051227
000900051229     d conta           s                   like(tudqta)
001000051229     d dataalfa        s              6
001100051227     d dtaiso          s               d   inz
001200060509     d Dataiso         s               d   datfmt(*iso)
001300060509     d Datafine        s               d   datfmt(*iso)
001400051229     d mult            s                   like(tudmult)
001500060509     d kmult           s                   like(tudmult)
001600060104     d savdim          s                   like(dcfdfc)
001700060104     d savfgs          s                   like(dcffev)
001800060104     d keytud          ds                  likerec(tntud000:*key)
001900051229     d wrkgetlista     s           4096    varying
002000051227
002100051229     d dtup          e ds
002200060104     d fndcfds       e ds                  extname(fndcf00f)
002300051227     d kpjba         e ds
002400051227     d tntu05ds      e ds
002500060104     dtntudtads      e ds
002600051227
002700051227      *-----------------------------------------------------------------*
002800051227      * Moltiplicatori calcolati:
002900060509      *    505 - numero c.a. per p.o. responsabile
003000051227      *-----------------------------------------------------------------*
003100060509     c* Cancello se già trovo il numeratore
003200060509     c* loop sulla data
003300060509     c                   eval      kmult='505'
003400060509     c     *iso          movel     t05dti        dataiso
003500060509     c     *iso          movel     t05dtf        datafine
003600060509    2c                   dow       dataiso<=datafine
003700060509     c                   exsr      CANCMULT
003800060509     c
003900060509     c                   adddur    1:*D          dataiso
004000060509    2c                   enddo
004100051229
004200060104     c                   exsr      PREPASQL
004300060104     c
004400060104     c*  ?Aggiorno data ultima elaborazione in tabella TUP
004500060104     c                   if        t05mult=*blanks and t05fil=0
004600060104     c                   clear                   tntudtads
004700060104     c                   eval      §DTAazn = kcdaz
004800060104     c                   eval      §DTAdta = t05dtf
004900060104     c                   call      'TNTUDTAR'
005000060104     c                   parm                    tntudtads
005100060104     c                   endif
005200051229
005300051229     c                   Eval      *InLr = *On
005400051229
005500051229      *-----------------------------------------------------------------*
005600060104      * Lettura file fasi
005700051229      *-----------------------------------------------------------------*
005800060104     c     prepasql      begsr
005900060509     c                   movel     t05dti        annodti           4 0
006000060509     c                   move      t05dti        mmggdti           4 0
006100060509     c
006200060509     c                   movel     t05dfi        annodfi           4 0
006300060509     c                   move      t05dfi        mmggdfi           4 0
006400051229
006500051229      *?Imposto la parte fissa dell'istruzione sql
006600051229     c                   eval      wrkgetlista =
006700060509     c                             'select dcrpor, dcraac, dctmgc, dctfil, -
006800060509     c                             dctnca  from fndcr00f, fndct00f where -
006900060509     c                             and dcraac between '+
007000060509     c                             %editc(annodti:'X') + ' and ' +
007100060509     c                             %editc(annodtf:'X') +
007200060509     c                             ' and dctmgc between '+
007300060509     c                             %editc(mmggdti:'X') + ' and ' +
007400060509     c                             %editc(mmggdtf:'X')
007500051229      *?Se passato un p.o.
007600051229     c                   if        t05fil <> 0
007700051229     c                   eval      wrkgetlista = wrkgetlista +
007800060509     c                             ' and digits(dcrpor) = ' +
007900051229     c                             %triml(%editc(t05fil:'Z'))
008000051229     c                   endif
008100060509      *?Unione dei 2 files
008200060509     c                   eval      wrkgetlista = wrkgetlista +
008300060509     c                             ' and dcrpor=dctpor and dcraac=dctaac ' +
008400060509     c                             ' and dcrnca=dctnca'
008500060509     c                   endif
008600051229      *?Solo lettura
008700051229     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
008800051229
008900051229     C/EXEC SQL
009000051229     C+ PREPARE s1 FROM :wrkgetlista
009100051229     C/END-EXEC
009200051229     C
009300051229     C/EXEC SQL
009400051229     C+ DECLARE A1 CURSOR FOR S1
009500051229     C/END-EXEC
009600051229     C
009700051229     C/EXEC SQL
009800051229     C+ OPEN a1
009900051229     C/END-EXEC
010000051229
010100051229     c                   clear                   conta
010200051229      *?Leggo il file
010300051229     c                   do        *hival
010400051229     C/EXEC SQL
010500060104     C+ FETCH NEXT FROM a1 INTO :dcffca, :savfgs, :savdim, :conta
010600051229     C/END-EXEC
010700051229
010800051229     c                   select
010900051229
011000051229     c                   when      sqlcod = 100
011100051229     c                   leave
011200051229     c                   when      sqlcod < 0
011300051229     c                   seton                                        H1
011400051229     c                   exsr      srfine
011500051229     c                   other
011600060104     c
011700060104     c                   select
011800060104     c                   when      dcffca=10
011900060104     c                   eval      mult = '500'
012000060104     c                   endsl
012100060104     c
012200060104     c                   if        t05mult=*blanks or t05mult=mult
012300051229     c                   exsr      scrivitntu
012400060104     c                   endif
012500060104     c
012600051229     c                   endsl
012700051229
012800051229     c                   enddo
012900051229
013000051229     C/EXEC SQL
013100051229     C+ CLOSE a1
013200051229     C/END-EXEC
013300051229
013400051229     c                   endsr
013500051227      *-----------------------------------------------------------------*
013600051228      * Scrivo/Aggiorno TNTUD
013700051227      *-----------------------------------------------------------------*
013800051227     c     scrivitntu    begsr
013900051229
014000051229      /free
014100051229
014200051229       //?imposto la data iso
014300051229       dataalfa = %subst(%char(savdim):3:6);
014400051229       dtaiso = %date(dataalfa:*ymd0);
014500051229
014600051229       //?imposto la chiave per tntud01l
014700051229       keytud.tudmult = mult;
014800051229       keytud.tudfil = savfgs;
014900051229       keytud.tuddta = dtaiso;
015000051229
015100051229       //?aggancio tntud01l
015200051229       chain %kds(keytud:3) tntud01l;
015300051229
015400051229       //?trovo rcd aggiorno se è un ricalcolo
015500051229       if %found(tntud01l);
015600051229       if t05mult <> *blanks or t05fil <> *zeros;
015700051229       tudqta = conta;
015800051229       update tntud000;
015900051229       else;
016000051229       if not%open(qsysprt);
016100051229       open qsysprt;
016200051229       except testa;
016300051229       endif;
016400051229       if *inof;
016500051229       except testa;
016600051229       endif;
016700051229       except riga;
016800051229       endif;
016900051229
017000051229       //?non trovo rcd scrivo
017100051229       else;
017200051229       clear tntud000;
017300051229       tudmult = mult;
017400051229       tuddta = dtaiso;
017500051229       tudfil = savfgs;
017600051229       tudcli = 0;
017700051229       tudqta = conta;
017800051229       write tntud000;
017900051229       endif;
018000051229
018100051229      /end-free
018200051227
018300051229     c                   endsr
018400051229
018500051229      *-----------------------------------------------------------------*
018600051229      * Fine programmma con codice errore
018700051229      *-----------------------------------------------------------------*
018800051229     c     srfine        begsr
018900051229
019000051229     c                   eval      *InLr = *On
019100051229     c                   return
019200051229
019300051229     c                   endsr
019400051227
019500060509     C**************************************************************************
019600060509     c* Cancella il moltiplicatore se rielaborazione
019700060509     C**************************************************************************
019800060509     C     CANCMULT      BEGSR
019900060509     c     ktud2         setll     tntud01l
020000060509     c
020100060509     c                   setoff                                       31
020200060509     c                   dow       not *in31
020300060509     c     ktud2         delete    tntud01l                           31
020400060509     c                   enddo
020500060509     c
020600060509     c                   ENDSR
020700051227      *-----------------------------------------------------------------*
020800051227      * Routine iniziale
020900051227      *-----------------------------------------------------------------*
021000051227     c     *inzsr        begsr
021100051227
021200051227     c     *entry        plist
021300051227     c                   parm                    kpjba
021400051227     c                   eval      tntu05ds = kpjbu
021500060509     c
021600060509     c     ktud2         Klist
021700060509     c                   Kfld                    kmult
021800060509     c                   Kfld                    dataiso
021900060509     c
022000051227
022100051229     c                   endsr
022200051229
022300051227      *-----------------------------------------------------------------*
022400051229     oQsysprt   e            Testa          2 02
022500051229     o                                         +  5 'Rcd già esistente in'
022600051229     o                                         +  1 'TNTUD00F'
022700060104     o                                           85 'TNTU93R'
022800051229     o                       *Date         y    110
022900051229     o                                          128 'Pag.'
023000051229     o                       Page          z    132
023100051229     o          e            Riga           1
023200051229     o                                           +1 'Rcd:'
023300051229     o                       tudmult             +1
023400051229     o                       tudfil              +1
023500051229     o                       tuddta              +1
023600051229     o                       tudqta        4     +1

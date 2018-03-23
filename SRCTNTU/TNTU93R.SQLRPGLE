000100051229      *PARMS CLOSQLCSR(*ENDMOD)
000200051229      *PARMS DYNUSRPRF(*OWNER)
000300051227      *-----------------------------------------------------------------*
000400060104      *  Leggo FASI C.A.
000500051227      *-----------------------------------------------------------------*
000600051227     ftntud01l  uf a e           k disk
000700051229     fqsysprt   o    f  132        printer oflind(*inof) usropn
000800051227
000900051229     d conta           s                   like(tudqta)
001000051229     d dataalfa        s              6
001100051227     d dtaiso          s               d   inz
001200051229     d mult            s                   like(tudmult)
001300060104     d savdim          s                   like(dcfdfc)
001400060104     d savfgs          s                   like(dcffev)
001500060104     d keytud          ds                  likerec(tntud000:*key)
001600051229     d wrkgetlista     s           4096    varying
001700051227
001800051229     d dtup          e ds
001900060104     d fndcfds       e ds                  extname(fndcf00f)
002000051227     d kpjba         e ds
002100051227     d tntu05ds      e ds
002200060104     dtntudtads      e ds
002300051227
002400051227      *-----------------------------------------------------------------*
002500051227      * Moltiplicatori calcolati:
002600060104      *    500 - apeprtua c.a.
002700060104      *    510 - conferma c.a.
002800060104      *    520 - liquidazione transattiva
002900051227      *-----------------------------------------------------------------*
003000051229
003100060104     c                   exsr      PREPASQL
003200060104     c
003300060104     c*  ?Aggiorno data ultima elaborazione in tabella TUP
003400060104     c                   if        t05mult=*blanks and t05fil=0
003500060104     c                   clear                   tntudtads
003600060104     c                   eval      §DTAazn = kcdaz
003700060104     c                   eval      §DTAdta = t05dtf
003800060104     c                   call      'TNTUDTAR'
003900060104     c                   parm                    tntudtads
004000060104     c                   endif
004100051229
004200051229     c                   Eval      *InLr = *On
004300051229
004400051229      *-----------------------------------------------------------------*
004500060104      * Lettura file fasi
004600051229      *-----------------------------------------------------------------*
004700060104     c     prepasql      begsr
004800051229
004900051229      *?Imposto la parte fissa dell'istruzione sql
005000051229     c                   eval      wrkgetlista =
005100060104     c                             'select dcffca, dcffev, dcfdfc, -
005200060104     c                             count(*) from fndcf00f where -
005300060104     c                             dcffca in(010, 100, 215) -
005400060104     c                             and dcfdfc between '+
005500051229     c                             %editc(t05dti:'X') + ' and ' +
005600051229     c                             %editc(t05dtf:'X')
005700051229      *?Se passato un p.o.
005800051229     c                   if        t05fil <> 0
005900051229     c                   eval      wrkgetlista = wrkgetlista +
006000060104     c                             ' and digits(dcffev) = ' +
006100051229     c                             %triml(%editc(t05fil:'Z'))
006200051229     c                   endif
006300051229      *?Ordinamento
006400051229     c                   eval      wrkgetlista = wrkgetlista +
006500060104     c                             ' group by dcffca, dcffev, dcfdfc'
006600051229      *?Solo lettura
006700051229     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
006800051229
006900051229     C/EXEC SQL
007000051229     C+ PREPARE s1 FROM :wrkgetlista
007100051229     C/END-EXEC
007200051229     C
007300051229     C/EXEC SQL
007400051229     C+ DECLARE A1 CURSOR FOR S1
007500051229     C/END-EXEC
007600051229     C
007700051229     C/EXEC SQL
007800051229     C+ OPEN a1
007900051229     C/END-EXEC
008000051229
008100051229     c                   clear                   conta
008200051229      *?Leggo il file
008300051229     c                   do        *hival
008400051229     C/EXEC SQL
008500060104     C+ FETCH NEXT FROM a1 INTO :dcffca, :savfgs, :savdim, :conta
008600051229     C/END-EXEC
008700051229
008800051229     c                   select
008900051229
009000051229     c                   when      sqlcod = 100
009100051229     c                   leave
009200051229     c                   when      sqlcod < 0
009300051229     c                   seton                                        H1
009400051229     c                   exsr      srfine
009500051229     c                   other
009600060104     c
009700060104     c                   select
009800060104     c                   when      dcffca=10
009900060104     c                   eval      mult = '500'
010000060104
010100060104     c                   when      dcffca=100
010200060104     c                   eval      mult = '510'
010300060104
010400060104     c                   when      dcffca=215
010500060104     c                   eval      mult = '520'
010600060104     c                   endsl
010700060104     c
010800060104     c                   if        t05mult=*blanks or t05mult=mult
010900051229     c                   exsr      scrivitntu
011000060104     c                   endif
011100060104     c
011200051229     c                   endsl
011300051229
011400051229     c                   enddo
011500051229
011600051229     C/EXEC SQL
011700051229     C+ CLOSE a1
011800051229     C/END-EXEC
011900051229
012000051229     c                   endsr
012100051227      *-----------------------------------------------------------------*
012200051228      * Scrivo/Aggiorno TNTUD
012300051227      *-----------------------------------------------------------------*
012400051227     c     scrivitntu    begsr
012500051229
012600051229      /free
012700051229
012800051229       //?imposto la data iso
012900051229       dataalfa = %subst(%char(savdim):3:6);
013000051229       dtaiso = %date(dataalfa:*ymd0);
013100051229
013200051229       //?imposto la chiave per tntud01l
013300051229       keytud.tudmult = mult;
013400051229       keytud.tudfil = savfgs;
013500051229       keytud.tuddta = dtaiso;
013600051229
013700051229       //?aggancio tntud01l
013800051229       chain %kds(keytud:3) tntud01l;
013900051229
014000051229       //?trovo rcd aggiorno se è un ricalcolo
014100051229       if %found(tntud01l);
014200051229       if t05mult <> *blanks or t05fil <> *zeros;
014300051229       tudqta = conta;
014400051229       update tntud000;
014500051229       else;
014600051229       if not%open(qsysprt);
014700051229       open qsysprt;
014800051229       except testa;
014900051229       endif;
015000051229       if *inof;
015100051229       except testa;
015200051229       endif;
015300051229       except riga;
015400051229       endif;
015500051229
015600051229       //?non trovo rcd scrivo
015700051229       else;
015800051229       clear tntud000;
015900051229       tudmult = mult;
016000051229       tuddta = dtaiso;
016100051229       tudfil = savfgs;
016200051229       tudcli = 0;
016300051229       tudqta = conta;
016400051229       write tntud000;
016500051229       endif;
016600051229
016700051229      /end-free
016800051227
016900051229     c                   endsr
017000051229
017100051229      *-----------------------------------------------------------------*
017200051229      * Fine programmma con codice errore
017300051229      *-----------------------------------------------------------------*
017400051229     c     srfine        begsr
017500051229
017600051229     c                   eval      *InLr = *On
017700051229     c                   return
017800051229
017900051229     c                   endsr
018000051227
018100051227      *-----------------------------------------------------------------*
018200051227      * Routine iniziale
018300051227      *-----------------------------------------------------------------*
018400051227     c     *inzsr        begsr
018500051227
018600051227     c     *entry        plist
018700051227     c                   parm                    kpjba
018800051227     c                   eval      tntu05ds = kpjbu
018900051227
019000051229     c                   endsr
019100051229
019200051227      *-----------------------------------------------------------------*
019300051229     oQsysprt   e            Testa          2 02
019400051229     o                                         +  5 'Rcd già esistente in'
019500051229     o                                         +  1 'TNTUD00F'
019600060104     o                                           85 'TNTU93R'
019700051229     o                       *Date         y    110
019800051229     o                                          128 'Pag.'
019900051229     o                       Page          z    132
020000051229     o          e            Riga           1
020100051229     o                                           +1 'Rcd:'
020200051229     o                       tudmult             +1
020300051229     o                       tudfil              +1
020400051229     o                       tuddta              +1
020500051229     o                       tudqta        4     +1

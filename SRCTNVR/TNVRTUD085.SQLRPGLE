000100051229      *PARMS CLOSQLCSR(*ENDMOD)
000200051229      *PARMS DYNUSRPRF(*OWNER)
000300051227      *-----------------------------------------------------------------*
000400051227     ftntud01l  uf a e           k disk
000500060323     ftitas00f  uf   e           k disk
000600051227
000700060509     d conta           s                   like(tudqta)
000800051228     d dataalfa        s              6
000900051228     d dtaiso          s               d   inz(d'0001-01-01')
001000060322     d Dataiso         s               d   datfmt(*iso)
001100060322     d Datafine        s               d   datfmt(*iso)
001200051227     d mult            s                   like(tudmult)
001300051228     d wrkgetlista     s           4096    varying
001400060323     d wrkgetlista2    s           4096    varying
001500051228
001600051228     d keytud          ds                  likerec(tntud000:*key)
001700051227
001800051229     d dtup          e ds
001900060116     d DS3A          e ds
002000051227     d kpjba         e ds
002100051227     d tntu05ds      e ds
002200051227
002300051227      *-----------------------------------------------------------------*
002400060323     c
002500051228      *?Imposto la parte fissa dell'istruzione sql
002600051228     c                   eval      wrkgetlista =
002700060509     c                             'select taslna, tasdti, count(*) ' +
002800060323     c                             ' from titas00f  where' +
002900060323     c                             ' tasdti between '+
003000051229     c                              %editc(t05dti:'X') + ' and ' +
003100060116     c                              %editc(t05dtf:'X') +' and ' +
003200060509     c                             'tascbo =''A '''
003300060103     c
003400060323      *?Ordinamento
003500060323     c                   eval      wrkgetlista = wrkgetlista +
003600060323     c                             ' group by taslna, tasdti '
003700051228      *?Ordinamento
003800051228     c                   eval      wrkgetlista = wrkgetlista +
003900060323     c                             ' order by taslna, tasdti '
004000051228      *?Solo lettura
004100051228     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
004200060323     c
004300060323     c
004400060323      *?Imposto la parte fissa dell'istruzione sql
004500060323     c                   eval      wrkgetlista2 =
004600060509     c                             'select taslna, tasdti, count(*)' +
004700060323     c                             ' from titas10f  where' +
004800060323     c                             ' tasdti between '+
004900060323     c                              %editc(t05dti:'X') + ' and ' +
005000060323     c                              %editc(t05dtf:'X') +' and ' +
005100060509     c                             'tascbo = ''A '''
005200060323      *?Ordinamento
005300060323     c                   eval      wrkgetlista2 = wrkgetlista2 +
005400060323     c                             ' group by taslna, tasdti '
005500060323      *?Ordinamento
005600060323     c                   eval      wrkgetlista2 = wrkgetlista2 +
005700060323     c                             ' order by taslna, tasdti '
005800060323      *?Solo lettura
005900060323     c                   eval      wrkgetlista2= wrkgetlista2+ ' for read only'
006000051228
006100051228     C/EXEC SQL
006200051228     C+ PREPARE s1 FROM :wrkgetlista
006300051228     C/END-EXEC
006400051228     C
006500060323     C
006600051228     C/EXEC SQL
006700051228     C+ DECLARE A1 CURSOR FOR S1
006800051228     C/END-EXEC
006900051228     C
007000051228     C/EXEC SQL
007100051228     C+ OPEN a1
007200051228     C/END-EXEC
007300051228
007400051228      *?Leggo il file
007500060509     c                   clear                   conta
007600051228     c                   do        *hival
007700051228     C/EXEC SQL
007800060509     C+ FETCH NEXT FROM a1 INTO :taslna, :tasdti, :conta
007900051228     C/END-EXEC
008000051228
008100051228     c                   select
008200051228
008300051228     c                   when      sqlcod = 100
008400051228     c                   leave
008500051228     c                   when      sqlcod < 0
008600051228     c                   seton                                        H1
008700051228     c                   exsr      srfine
008800051228     c                   other
008900060323     c                   exsr      scrivitntu
009000051228     c                   endsl
009100051228
009200051228     c                   enddo
009300051228
009400051228     C/EXEC SQL
009500051228     C+ CLOSE a1
009600051228     C/END-EXEC
009700060323
009800060323     C/EXEC SQL
009900060323     C+ PREPARE s2 FROM :wrkgetlista2
010000060323     C/END-EXEC
010100060323     C
010200060323     C
010300060323     C/EXEC SQL
010400060323     C+ DECLARE A2 CURSOR FOR S2
010500060323     C/END-EXEC
010600060323     C
010700060323     C/EXEC SQL
010800060323     C+ OPEN a2
010900060323     C/END-EXEC
011000060323
011100060323      *?Leggo il file
011200060323     c                   do        *hival
011300060323     C/EXEC SQL
011400060509     C+ FETCH NEXT FROM a2 INTO :taslna, :tasdti, :conta
011500060323     C/END-EXEC
011600060323
011700060323     c                   select
011800060323
011900060323     c                   when      sqlcod = 100
012000060323     c                   leave
012100060323     c                   when      sqlcod < 0
012200060323     c                   seton                                        H1
012300060323     c                   exsr      srfine
012400060323     c                   other
012500060323     c                   exsr      scrivitntu
012600060323     c                   endsl
012700060323
012800060323     c                   enddo
012900060323
013000060323     C/EXEC SQL
013100060323     C+ CLOSE a2
013200060323     C/END-EXEC
013300051228
013400051228     c                   Eval      *InLr = *On
013500051227
013600051227      *-----------------------------------------------------------------*
013700051228      * Scrivo/Aggiorno TNTUD
013800051227      *-----------------------------------------------------------------*
013900051227     c     scrivitntu    begsr
014000051227
014100051228      /free
014200051228
014300051228       //?imposto la data iso
014400060323       dataalfa = %subst(%char(tasdti):3:6);
014500051228       dtaiso = %date(dataalfa:*ymd0);
014600051228
014700051228       //?imposto la chiave per tntud01l
014800060509       keytud.tudmult = '085';
014900060323       keytud.tudfil = taslna;
015000051228       keytud.tuddta = dtaiso;
015100051228
015200051228       //?aggancio tntud01l
015300051228       chain %kds(keytud:3) tntud01l;
015400051228
015500051228       //?trovo rcd aggiorno se è un ricalcolo
015600051228       if %found(tntud01l);
015700060509       tudqta = tudqta+conta;
015800051228       update tntud000;
015900051228
016000051228       //?non trovo rcd scrivo
016100051228       else;
016200051228       clear tntud000;
016300060509       tudmult = '085';
016400051228       tuddta = dtaiso;
016500060323       tudfil = taslna;
016600051228       tudcli = 0;
016700060509       tudqta = conta;
016800051228       write tntud000;
016900051228       endif;
017000051228
017100051228      /end-free
017200051227
017300051229     c                   endsr
017400051228
017500051228      *-----------------------------------------------------------------*
017600051228      * Fine programmma con codice errore
017700051228      *-----------------------------------------------------------------*
017800051228     c     srfine        begsr
017900051228     c                   eval      *InLr = *On
018000051228     c                   return
018100051228
018200051228     c                   endsr
018300051227
018400051227      *-----------------------------------------------------------------*
018500051227      * Routine iniziale
018600051227      *-----------------------------------------------------------------*
018700051227     c     *inzsr        begsr
018800051227
018900051227     c     *entry        plist
019000051227     c                   parm                    kpjba
019100051227     c                   eval      tntu05ds = kpjbu
019200051227
019300051229     c                   endsr

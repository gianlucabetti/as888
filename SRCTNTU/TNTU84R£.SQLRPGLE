000100051229      *PARMS CLOSQLCSR(*ENDMOD)
000200051229      *PARMS DYNUSRPRF(*OWNER)
000300051227      *-----------------------------------------------------------------*
000400070112      *  Leggo le bolle dirottate cioè con consegna anomala='1',
000500051227      *-----------------------------------------------------------------*
000600051227     ftntud01l  uf a e           k disk
000700060111     d
000800051227
000900051229     d conta           s                   like(tudqta)
001000051229     d dataalfa        s              6
001100051227     d dtaiso          s               d   inz
001200051229     d mult            s                   like(tudmult)
001300060111     d savdim          s                   like(arbdcm)
001400060111     d savfgs          s                   like(arblna)
001500060104     d keytud          ds                  likerec(tntud000:*key)
001600051229     d wrkgetlista     s           4096    varying
001700051227
001800051229     d dtup          e ds
001900060111     d fnarbds       e ds                  extname(fnarb00f)
002000051227     d kpjba         e ds
002100051227     d tntu05ds      e ds
002200051227
002300051227      *-----------------------------------------------------------------*
002400060111      * Moltiplicatore calcolato in aggiunta al tntu91r
002500070112      *    072 - spedizioni dirottate
002600051227      *-----------------------------------------------------------------*
002700051229
002800060104     c                   exsr      PREPASQL
002900051229
003000051229     c                   Eval      *InLr = *On
003100051229
003200051229      *-----------------------------------------------------------------*
003300060104      * Lettura file fasi
003400051229      *-----------------------------------------------------------------*
003500060104     c     prepasql      begsr
003600051229
003700051229      *?Imposto la parte fissa dell'istruzione sql
003800051229     c                   eval      wrkgetlista =
003900060111     c                             'select arblna, arbdcm, -
004000060111     c                             count(*) from fnarb00f where -
004100060111     c                             arbdcm between '+
004200051229     c                             %editc(t05dti:'X') + ' and ' +
004300070112     c                             %editc(t05dtf:'X') + ' and arbcca=''1'''
004400051229      *?Se passato un p.o.
004500051229     c                   if        t05fil <> 0
004600051229     c                   eval      wrkgetlista = wrkgetlista +
004700060111     c                             ' and digits(arblna) = ' +
004800051229     c                             %triml(%editc(t05fil:'Z'))
004900051229     c                   endif
005000051229      *?Ordinamento
005100051229     c                   eval      wrkgetlista = wrkgetlista +
005200060111     c                             ' group by arblna, arbdcm'
005300051229      *?Solo lettura
005400051229     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
005500051229
005600051229     C/EXEC SQL
005700051229     C+ PREPARE s1 FROM :wrkgetlista
005800051229     C/END-EXEC
005900051229     C
006000051229     C/EXEC SQL
006100051229     C+ DECLARE A1 CURSOR FOR S1
006200051229     C/END-EXEC
006300051229     C
006400051229     C/EXEC SQL
006500051229     C+ OPEN a1
006600051229     C/END-EXEC
006700051229
006800051229     c                   clear                   conta
006900051229      *?Leggo il file
007000051229     c                   do        *hival
007100051229     C/EXEC SQL
007200060104     C+ FETCH NEXT FROM a1 INTO : savfgs, :savdim, :conta
007300051229     C/END-EXEC
007400051229
007500051229     c                   select
007600051229
007700051229     c                   when      sqlcod = 100
007800051229     c                   leave
007900051229     c                   when      sqlcod < 0
008000051229     c                   seton                                        H1
008100051229     c                   exsr      srfine
008200051229     c                   other
008300060104     c
008400070112     c                   eval      mult = '072'
008500060104     c
008600051229     c                   exsr      scrivitntu
008700060104     c
008800051229     c                   endsl
008900051229
009000051229     c                   enddo
009100051229
009200051229     C/EXEC SQL
009300051229     C+ CLOSE a1
009400051229     C/END-EXEC
009500051229
009600051229     c                   endsr
009700051227      *-----------------------------------------------------------------*
009800051228      * Scrivo/Aggiorno TNTUD
009900051227      *-----------------------------------------------------------------*
010000051227     c     scrivitntu    begsr
010100051229
010200051229      /free
010300051229
010400051229       //?imposto la data iso
010500051229       dataalfa = %subst(%char(savdim):3:6);
010600051229       dtaiso = %date(dataalfa:*ymd0);
010700051229
010800051229       //?imposto la chiave per tntud01l
010900051229       keytud.tudmult = mult;
011000051229       keytud.tudfil = savfgs;
011100051229       keytud.tuddta = dtaiso;
011200051229
011300051229       //?aggancio tntud01l
011400051229       chain %kds(keytud:3) tntud01l;
011500051229
011600051229       //?trovo rcd aggiorno se è un ricalcolo
011700051229       if %found(tntud01l);
011800060111       tudqta = tudqta + conta;
011900051229       update tntud000;
012000051229
012100051229       //?non trovo rcd scrivo
012200051229       else;
012300051229       clear tntud000;
012400051229       tudmult = mult;
012500051229       tuddta = dtaiso;
012600051229       tudfil = savfgs;
012700051229       tudcli = 0;
012800051229       tudqta = conta;
012900051229       write tntud000;
013000051229       endif;
013100051229
013200051229      /end-free
013300051227
013400051229     c                   endsr
013500051229
013600051229      *-----------------------------------------------------------------*
013700051229      * Fine programmma con codice errore
013800051229      *-----------------------------------------------------------------*
013900051229     c     srfine        begsr
014000051229
014100051229     c                   eval      *InLr = *On
014200051229     c                   return
014300051229
014400051229     c                   endsr
014500051227
014600051227      *-----------------------------------------------------------------*
014700051227      * Routine iniziale
014800051227      *-----------------------------------------------------------------*
014900051227     c     *inzsr        begsr
015000051227
015100051227     c     *entry        plist
015200051227     c                   parm                    kpjba
015300070112     c                   eval      tntu05ds = kpjbu
015400070112     c                   endsr

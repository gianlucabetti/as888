000100051229      *PARMS CLOSQLCSR(*ENDMOD)
000200051229      *PARMS DYNUSRPRF(*OWNER)
000300051227      *-----------------------------------------------------------------*
000400060111      *  Leggo solo le bolle di recupero in arrivo con data consegna merce
000500060111      *  impostata e data arrivo =0 - Sono solo SEGUE FATTURA in quanto
000600060111      *  le fatture immediate, venendo messe in distinta per l'incasso
000700060111      *  dei soldi, una volta incassate un chiusura distinta viene impostat
000800060111      *  sia la data di consegna che la data di arrivo della bolla per
000900060111      *  cui vengono letti nel pgm tntu91r
001000051227      *-----------------------------------------------------------------*
001100051227     ftntud01l  uf a e           k disk
001200060111     d
001300060111     D REC             S              2    DIM(50)
001400051227
001500051229     d conta           s                   like(tudqta)
001600051229     d dataalfa        s              6
001700051227     d dtaiso          s               d   inz
001800051229     d mult            s                   like(tudmult)
001900060111     d savdim          s                   like(arbdcm)
002000060111     d savfgs          s                   like(arblna)
002100060104     d keytud          ds                  likerec(tntud000:*key)
002200051229     d wrkgetlista     s           4096    varying
002300051227
002400051229     d dtup          e ds
002500060111     d fnarbds       e ds                  extname(fnarb00f)
002600051227     d kpjba         e ds
002700051227     d tntu05ds      e ds
002800051227
002900051227      *-----------------------------------------------------------------*
003000060111      * Moltiplicatore calcolato in aggiunta al tntu91r
003100060111      *    080 - assegnati segue fattura
003200051227      *-----------------------------------------------------------------*
003300051229
003400060104     c                   exsr      PREPASQL
003500051229
003600051229     c                   Eval      *InLr = *On
003700051229
003800051229      *-----------------------------------------------------------------*
003900060104      * Lettura file fasi
004000051229      *-----------------------------------------------------------------*
004100060104     c     prepasql      begsr
004200051229
004300051229      *?Imposto la parte fissa dell'istruzione sql
004400051229     c                   eval      wrkgetlista =
004500060111     c                             'select arblna, arbdcm, -
004600060111     c                             count(*) from fnarb00f where -
004700060111     c                             arbdcm between '+
004800051229     c                             %editc(t05dti:'X') + ' and ' +
004900060111     c                             %editc(t05dtf:'X') + ' and arbdam=0 and '
005000060111     c                             + 'arbcbo in('
005100060111      * ciclo sulla sk dei cod bolla di recupero
005200060111     c                   Do        50            XX                3 0
005300060111     c                   If        rec(XX) = *blanks
005400060111     c                   Leave
005500060111     c                   EndIf
005600060111     c
005700060111     c                   If        XX >1
005800060111     c                   eval      wrkgetlista =
005900060111     c                             %trim(wrkgetlista) + ','
006000060111     c                   EndIf
006100060111     c
006200060111     c                   eval      wrkgetlista =
006300060111     c                             %trim(wrkgetlista) + ' ''' +
006400060111     c                             rec(xx) + ''''
006500060111     c                   EndDo
006600060111     c
006700060111     c                   Eval      wrkgetlista =
006800060111     c                             %trim(wrkgetlista) + ')'
006900051229      *?Se passato un p.o.
007000051229     c                   if        t05fil <> 0
007100051229     c                   eval      wrkgetlista = wrkgetlista +
007200060111     c                             ' and digits(arblna) = ' +
007300051229     c                             %triml(%editc(t05fil:'Z'))
007400051229     c                   endif
007500051229      *?Ordinamento
007600051229     c                   eval      wrkgetlista = wrkgetlista +
007700060111     c                             ' group by arblna, arbdcm'
007800051229      *?Solo lettura
007900051229     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
008000051229
008100051229     C/EXEC SQL
008200051229     C+ PREPARE s1 FROM :wrkgetlista
008300051229     C/END-EXEC
008400051229     C
008500051229     C/EXEC SQL
008600051229     C+ DECLARE A1 CURSOR FOR S1
008700051229     C/END-EXEC
008800051229     C
008900051229     C/EXEC SQL
009000051229     C+ OPEN a1
009100051229     C/END-EXEC
009200051229
009300051229     c                   clear                   conta
009400051229      *?Leggo il file
009500051229     c                   do        *hival
009600051229     C/EXEC SQL
009700060104     C+ FETCH NEXT FROM a1 INTO : savfgs, :savdim, :conta
009800051229     C/END-EXEC
009900051229
010000051229     c                   select
010100051229
010200051229     c                   when      sqlcod = 100
010300051229     c                   leave
010400051229     c                   when      sqlcod < 0
010500051229     c                   seton                                        H1
010600051229     c                   exsr      srfine
010700051229     c                   other
010800060104     c
010900060111     c                   eval      mult = '080'
011000060104     c
011100051229     c                   exsr      scrivitntu
011200060104     c
011300051229     c                   endsl
011400051229
011500051229     c                   enddo
011600051229
011700051229     C/EXEC SQL
011800051229     C+ CLOSE a1
011900051229     C/END-EXEC
012000051229
012100051229     c                   endsr
012200051227      *-----------------------------------------------------------------*
012300051228      * Scrivo/Aggiorno TNTUD
012400051227      *-----------------------------------------------------------------*
012500051227     c     scrivitntu    begsr
012600051229
012700051229      /free
012800051229
012900051229       //?imposto la data iso
013000051229       dataalfa = %subst(%char(savdim):3:6);
013100051229       dtaiso = %date(dataalfa:*ymd0);
013200051229
013300051229       //?imposto la chiave per tntud01l
013400051229       keytud.tudmult = mult;
013500051229       keytud.tudfil = savfgs;
013600051229       keytud.tuddta = dtaiso;
013700051229
013800051229       //?aggancio tntud01l
013900051229       chain %kds(keytud:3) tntud01l;
014000051229
014100051229       //?trovo rcd aggiorno se è un ricalcolo
014200051229       if %found(tntud01l);
014300060111       tudqta = tudqta + conta;
014400051229       update tntud000;
014500051229
014600051229       //?non trovo rcd scrivo
014700051229       else;
014800051229       clear tntud000;
014900051229       tudmult = mult;
015000051229       tuddta = dtaiso;
015100051229       tudfil = savfgs;
015200051229       tudcli = 0;
015300051229       tudqta = conta;
015400051229       write tntud000;
015500051229       endif;
015600051229
015700051229      /end-free
015800051227
015900051229     c                   endsr
016000051229
016100051229      *-----------------------------------------------------------------*
016200051229      * Fine programmma con codice errore
016300051229      *-----------------------------------------------------------------*
016400051229     c     srfine        begsr
016500051229
016600051229     c                   eval      *InLr = *On
016700051229     c                   return
016800051229
016900051229     c                   endsr
017000051227
017100051227      *-----------------------------------------------------------------*
017200051227      * Routine iniziale
017300051227      *-----------------------------------------------------------------*
017400051227     c     *inzsr        begsr
017500051227
017600051227     c     *entry        plist
017700051227     c                   parm                    kpjba
017800060111     c                   parm                    codbollarec     100
017900060111     c
018000051227     c                   eval      tntu05ds = kpjbu
018100060111     c                   movea     codbollarec   rec
018200051227
018300051229     c                   endsr
018400051229

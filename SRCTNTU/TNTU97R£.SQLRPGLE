000100051229      *PARMS CLOSQLCSR(*ENDMOD)
000200051229      *PARMS DYNUSRPRF(*OWNER)
000300051227      *-----------------------------------------------------------------*
000400060103      *  Leggo Bolle partenza fnblp
000500051227      *-----------------------------------------------------------------*
000600051227     ftntud01l  uf a e           k disk
000700051229     ftntbe01l  uf   e           k disk
000800060103     ftabel00f  if   e           k disk
000900060104     ffiar501l  if   e           k disk
001000051228     fqsysprt   o    f  132        printer oflind(*inof) usropn
001100060103     d
001200060116     D REC             S              2    DIM(50)
001300060103     D TMC             S              2    DIM(100)
001400071022     D OPE             S              1    DIM(50)
001500071022     D OPEDACLI        S              1    DIM(50)
001600051227
001700060103     d conta           s                   like(tudqta)
001800060103     d conta040        s                   like(tudqta)
001900060103     d conta090        s                   like(tudqta)
002000060103     d conta100        s                   like(tudqta)
002100060323     d conta105        s                   like(tudqta)
002200070104     d conta107        s                   like(tudqta)
002300060103     d conta200        s                   like(tudqta)
002400070104     d conta202        s                   like(tudqta)
002500070104     d conta204        s                   like(tudqta)
002600070104     d conta206        s                   like(tudqta)
002700051228     d dataalfa        s              6
002800051228     d dtaiso          s               d   inz(d'0001-01-01')
002900060322     d Dataiso         s               d   datfmt(*iso)
003000060322     d Datafine        s               d   datfmt(*iso)
003100051227     d mult            s                   like(tudmult)
003200060323     d kmult           s                   like(tudmult)
003300060103     d savdim          s                   like(blpdim)
003400060103     d savpog          s                   like(blplnp)
003500060322     d wpog            s                   like(blplnp)
003600060322     d savtfp          s                   like(blptfp)
003700060103     d ktbkut          s                   like(tblkut) inz(1)
003800060103     d ktbcod          s                   like(tblcod)
003900060104     d KAr5Trd         s                   Like(Ar5Trd)
004000051228     d wrkgetlista     s           4096    varying
004100051228
004200051228     d keytud          ds                  likerec(tntud000:*key)
004300051229     d keytbe          ds                  likerec(tntbe000:*key)
004400051227
004500051229     d dtup          e ds
004600060103     d DS1B          e ds
004700060116     d DS3A          e ds
004800071022     d DS3M          e ds
004900060103     d fnblpds       e ds                  extname(fnblp00f)
005000051227     d kpjba         e ds
005100051227     d tntu05ds      e ds
005200060104     d dar5bnb       e ds
005300051227
005400051227      *-----------------------------------------------------------------*
005500051227      * Moltiplicatori calcolati:
005600060322     c*    040 - DDT SI x lnp
005700060322     c*    042 - DDT SI x TFP
005800071022     c*    090 - spedizioni no da cliente - bollettate
005900071022     c*    100 - spedizioni    da clienti - solo confermate
006000060323     c*    105 - spedizioni
006100070104     c*    107 - spedizioni over 20 con particolarità varia "O" senza serie
006200060103     c*    200 - colli ritirati
006300070104     c*    202 - colli disk B
006400070104     c*    204 - colli disk C
006500070104     c*    206 - colli no disk B/C
006600051227      *-----------------------------------------------------------------*
006700060322     c* Qui non è possibile mai rielaborare solo un p.o. azzero
006800060322     c                   clear                   t05fil
006900060322     c
007000060103     c* Carico i ctm del disk C
007100060103     c                   exsr      carCTM
007200060323     c
007300060322    1c                   if        t05mult='040' or t05mult=*blanks
007400060322     c                             or t05mult='042'
007500060323     c                   eval      kmult='040'
007600060322     c* loop sulla data
007700060322     c     *iso          movel     t05dti        dataiso
007800060322     c     *iso          movel     t05dtf        datafine
007900060322    2c                   dow       dataiso<=datafine
008000060322     c                   exsr      CANCMULT
008100060322     c
008200060322     c                   adddur    1:*D          dataiso
008300060322    2c                   enddo
008400060322     c
008500060323     c                   eval      kmult='042'
008600060322     c* loop sulla data
008700060322     c     *iso          movel     t05dti        dataiso
008800060322     c     *iso          movel     t05dtf        datafine
008900060322    2c                   dow       dataiso<=datafine
009000060322     c                   exsr      CANCMULT
009100060322     c
009200060322     c                   adddur    1:*D          dataiso
009300060322    2c                   enddo
009400060322    1c                   endif
009500051228
009600051228      *?Imposto la parte fissa dell'istruzione sql
009700051228     c                   eval      wrkgetlista =
009800060103     c                             'select blplnp, blpdim, blpfst, blpctm,' +
009900060104     c                             ' blpsop, blpncl, blpgva, blpaas, '+
010000060323     c                             ' blpnrs, blpnsp, blptfp '+
010100060103     c                             ' from fnblp00f  where' +
010200060103     c                             ' blpdim between '+
010300051229     c                              %editc(t05dti:'X') + ' and ' +
010400060116     c                              %editc(t05dtf:'X') +' and ' +
010500060116     c                             'blpcbo not in('
010600060116      * ciclo sulla sk dei cod bolla di recupero
010700060116     c                   Do        50            XX                3 0
010800060116     c                   If        rec(XX) = *blanks
010900060116     c                   Leave
011000060116     c                   EndIf
011100060116     c
011200060116     c                   If        XX >1
011300060116     c                   eval      wrkgetlista =
011400060116     c                             %trim(wrkgetlista) + ','
011500060116     c                   EndIf
011600060116     c
011700060116     c                   eval      wrkgetlista =
011800060116     c                             %trim(wrkgetlista) + ' ''' +
011900060116     c                             rec(xx) + ''''
012000060116     c                   EndDo
012100060116     c
012200060116     c                   Eval      wrkgetlista =
012300060116     c                             %trim(wrkgetlista) + ')'
012400060103     c* solo un p.o.
012500060103     c                   if        t05fil <> 0
012600060103     c                   eval      wrkgetlista = wrkgetlista +
012700060103     c                             ' and digits(blplnp) = ' +
012800060103     c                             %triml(%editc(t05fil:'Z'))
012900060103     c                   endif
013000060103     c
013100060103     c
013200051228      *?Ordinamento
013300051228     c                   eval      wrkgetlista = wrkgetlista +
013400060103     c                             ' order by blplnp, blpdim '
013500051228      *?Solo lettura
013600051228     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
013700051228
013800051228     C/EXEC SQL
013900051228     C+ PREPARE s1 FROM :wrkgetlista
014000051228     C/END-EXEC
014100051228     C
014200051228     C/EXEC SQL
014300051228     C+ DECLARE A1 CURSOR FOR S1
014400051228     C/END-EXEC
014500051228     C
014600051228     C/EXEC SQL
014700051228     C+ OPEN a1
014800051228     C/END-EXEC
014900051228
015000051228      *?Leggo il file
015100051228     c                   do        *hival
015200051228     C/EXEC SQL
015300060103     C+ FETCH NEXT FROM a1 INTO :blplnp, :blpdim, :blpfst, :blpctm, :blpsop,
015400060323     C+                         :blpncl, :blpgva, :blpaas, :blpnrs, :blpnsp,
015500060323     C+                         :blptfp
015600051228     C/END-EXEC
015700051228
015800051228     c                   select
015900051228
016000051228     c                   when      sqlcod = 100
016100051228     c                   exsr      aggiorna
016200051228     c                   leave
016300051228     c                   when      sqlcod < 0
016400051228     c                   seton                                        H1
016500051228     c                   exsr      srfine
016600051228     c                   other
016700051228     c                   exsr      srconta
016800051228     c                   endsl
016900051228
017000051228     c                   enddo
017100051228
017200051228     C/EXEC SQL
017300051228     C+ CLOSE a1
017400051228     C/END-EXEC
017500051229
017600051229      *?Aggiorno data ultima elaborazione in tabella TUP
017700060103      *? solo se non richiesto un moltiplicatore o un p.o.
017800051229
017900051229      /free
018000060103
018100060103       if t05mult=*blanks and t05fil=0;
018200051229
018300051229       //?imposto la chiave per tntbe01l
018400051229       keytbe.tbecod = 'TUP';
018500051229       keytbe.tbeke1 = kcdaz;
018600051229
018700051229       //?aggancio tntbe01l
018800051229       chain %kds(keytbe:2) tntbe01l;
018900051229
019000051229       //?trovo rcd aggiorno
019100060103       if %found(tntbe01l);
019200051229       dtup = tbeuni;
019300060103       //? solo se la data presente è < data che devo aggiornare
019400060103       if §tupdta<t05dtf;
019500051229       §tupdta = t05dtf;
019600051229       tbeuni = dtup;
019700060103       endif;
019800051229       update tntbe000;
019900051229       endif;
020000060103       endif;
020100051229
020200051229      /end-free
020300051228
020400051228     c                   Eval      *InLr = *On
020500051227
020600051228      *-----------------------------------------------------------------*
020700060103      * Conta i moltiplicatori previsti
020800051228      *-----------------------------------------------------------------*
020900051228     c     srconta       begsr
021000051228
021100051227      /free
021200051227
021300051227       //?a rottura di data aggiorno
021400060103       if blpdim <> savdim;
021500060103       if savdim > *zeros;
021600051227       exsr aggiorna;
021700060103       conta040 = 0;
021800060103       conta090 = 0;
021900060103       conta100 = 0;
022000060323       conta105 = 0;
022100070104       conta107 = 0;
022200060103       conta200 = 0;
022300070104       conta202 = 0;
022400070104       conta204 = 0;
022500070104       conta206 = 0;
022600051227       endif;
022700060103       savdim = blpdim;
022800060322       savtfp = blptfp;
022900060103       savpog = blplnp;
023000051227       endif;
023100051227
023200051227       //?a rottura di p.o. aggiorno
023300060103       if blplnp <> savpog;
023400051227       if savpog > *zeros;
023500051227       exsr aggiorna;
023600060103       conta040 = 0;
023700060103       conta090 = 0;
023800060103       conta100 = 0;
023900060323       conta105 = 0;
024000070104       conta107 = 0;
024100060103       conta200 = 0;
024200070104       conta202 = 0;
024300070104       conta204 = 0;
024400070104       conta206 = 0;
024500051227       endif;
024600060103       savpog = blplnp;
024700060322       savtfp = blptfp;
024800060103       savdim = blpdim;
024900051227       endif;
025000051227
025100060103       //?sommo i DDT SI
025200060103       if t05mult=*blanks or t05mult='040';
025300060103       if blpfst = 'S';
025400060103       conta040=conta040 + 1;
025500051227       endif;
025600060103       endif;
025700060104
025800060104      /end-free
025900060104
026000060104     c* Verifico se presenti bancali
026100060104     c                   if        t05mult=*blanks or t05mult='200'
026200060104    2c                   if        %subst(blpgva:1:1)<>'O'
026300060104     c                   add       blpncl        conta200
026400060322   x2c                   else
026500060104     c* considerazioni particolari per sped. con particolarità varia "O"
026600060104     c                   Clear                   DAr5Bnb
026700060104     c                   Eval      Kar5Trd = 'BNB'
026800060104     c     KFiar5        Chain     Fiar501l
026900060104    3c                   If        %Found(Fiar501l)
027000060104     c                   Movel     Ar5Uni        DAr5Bnb
027100070104     c                   else
027200070104     c                   z-add     blpncl        §ar5bcor
027300060104    3c                   EndIf
027400070104     c
027500060104     c                   add       §AR5BCOR      conta200
027600060104     c                   endif
027700060104     c                   endif
027800070104     c
027900070104     c* Somma colli DISK B C Altro
028000070104     c     blpctm        lookup    tmc                                    90
028100070104     c                   select
028200070104     c                   when      *in90
028300070104     c                   add       blpncl        conta204
028400070104     c
028500070104     c                   when      blpsop='%'
028600070104     c                   add       blpncl        conta202
028700070104     c                   other
028800070104     c                   add       blpncl        conta206
028900070104     c                   endsl
029000070104     c
029100051227
029200060323     c* Sommo tutte le spedizioni
029300060323     c                   if        t05mult=*blanks or t05mult='105'
029400060323     c                   eval      conta105=conta105 + 1
029500060323     c                   endif
029600070104     c* Sommo spedizioni over 20
029700070104     c                   if        t05mult=*blanks or t05mult='107'
029800070104    2c                   if        %subst(blpgva:1:1)='O' and blpnrs=0
029900070104     c                   eval      conta107=conta107 + 1
030000070104   x2c                   endif
030100070104   x2c                   endif
030200060103
030300060103     c* Sommo i disk e i no disk
030400060103     c                   if        t05mult=*blanks or t05mult='100' or
030500071022     c                             t05mult='090'
030600071022     c
030700071022     c***  blpctm        lookup    tmc                                    90
030800071022     c***                if        not *in90   and
030900071022     c***                          blpsop<>'§' and blpsop<>'%'
031000071022     c***                eval      conta090=conta090 + 1
031100071022     c***                else
031200071022     c***                eval      conta100=conta100 + 1
031300071022     c***                endif
031400071022     c
031500071022     c* Verifico se sigla operatore da scartare
031600071022     c                   z-add     1             II
031700071022     c     blpsop        lookup    ope(II)                                90
031800071022     c* non trovata sigla operatore--> bolla bollettata
031900071022     c                   if        not *in90 or blpsop=' '
032000071022     c                   eval      conta090=conta090 + 1
032100071022     c                   else
032200071022     c                   if        opedacli(II)='S'
032300071022     c                   eval      conta100=conta100 + 1
032400071022     c                   endif
032500071022     c                   endif
032600071022     c
032700060103     c                   endif
032800051227
032900051229     c                   endsr
033000051227
033100051227      *-----------------------------------------------------------------*
033200051227      * Imposto cosa devo aggiornare
033300051227      *-----------------------------------------------------------------*
033400051227     c     aggiorna      begsr
033500051227
033600051227      /free
033700051227
033800051228       //?se passato il moltiplicatore aggiorno quello passato
033900051228       if t05mult <> *blanks;
034000051228       mult = t05mult;
034100051228       select;
034200060323       when mult = '040'or mult='042';
034300060323       mult='040';
034400060103       conta = conta040;
034500060103       conta040 = 0;
034600060103       when mult = '090';
034700060103       conta = conta090;
034800060103       conta090 = 0;
034900060103       when mult = '100';
035000060103       conta = conta100;
035100060103       conta100 = 0;
035200060323       when mult = '105';
035300060323       conta = conta105;
035400060323       conta105 = 0;
035500070104       when mult = '107';
035600070104       conta = conta107;
035700070104       conta107 = 0;
035800060103       when mult = '200';
035900060103       conta = conta200;
036000060103       conta200 = 0;
036100070104       when mult = '202';
036200070104       conta = conta202;
036300070104       conta202 = 0;
036400070104       when mult = '204';
036500070104       conta = conta204;
036600070104       conta204 = 0;
036700070104       when mult = '206';
036800070104       conta = conta206;
036900070104       conta206 = 0;
037000051228       endsl;
037100060103
037200051228       if conta > 0;
037300051228       exsr scrivitntu;
037400060323
037500060323       if mult='040';
037600060323       wpog = savpog;
037700060323       mult = '042';
037800060323       savpog = savtfp;
037900060323       exsr scrivitntu;
038000060323       savpog = wpog;
038100060323       endif;
038200051228       conta = 0;
038300051228       endif;
038400051228
038500051229       //?moltiplicatore non passato aggiorno tutti quelli previsti
038600051228       else;
038700051228
038800060103       //?ddt si
038900060103       if conta040 > 0;
039000060103       mult = '040';
039100060103       conta = conta040;
039200051227       exsr scrivitntu;
039300060322       //?spedizioni DDT SI anche per terminal di partenza
039400060322       wpog = savpog;
039500060322       mult = '042';
039600060322       savpog = savtfp;
039700060322       exsr scrivitntu;
039800060322       savpog = wpog;
039900060103       conta040 = 0;
040000051227       endif;
040100051227
040200060103       //?spedizioni no disk
040300060103       if conta090 > 0;
040400060103       mult = '090';
040500060103       conta = conta090;
040600051227       exsr scrivitntu;
040700060103       conta090 = 0;
040800051227       endif;
040900051227
041000060103       //?spedizioni    disk
041100060103       if conta100 > 0;
041200060103       mult = '100';
041300060103       conta = conta100;
041400051227       exsr scrivitntu;
041500060103       conta100 = 0;
041600051227       endif;
041700060323
041800060323       //?spedizioni
041900060323       if conta105 > 0;
042000060323       mult = '105';
042100060323       conta = conta105;
042200060323       exsr scrivitntu;
042300060323       conta105 = 0;
042400060323       endif;
042500060103
042600070104       //?spedizioni over 20 senza serie
042700070104       if conta107 > 0;
042800070104       mult = '107';
042900070104       conta = conta107;
043000070104       exsr scrivitntu;
043100070104       conta107 = 0;
043200070104       endif;
043300070104
043400060103       //?colli ritirati
043500060103       if conta200 > 0;
043600060103       mult = '200';
043700060103       conta = conta200;
043800060103       exsr scrivitntu;
043900060103       conta200 = 0;
044000060103       endif;
044100070104
044200070104       //?colli disk B
044300070104       if conta202 > 0;
044400070104       mult = '202';
044500070104       conta = conta202;
044600070104       exsr scrivitntu;
044700070104       conta202 = 0;
044800070104       endif;
044900070104
045000070104       //?colli disk C
045100070104       if conta204 > 0;
045200070104       mult = '204';
045300070104       conta = conta204;
045400070104       exsr scrivitntu;
045500070104       conta204 = 0;
045600070104       endif;
045700070104
045800070104       //?colli no disk B/C
045900070104       if conta206 > 0;
046000070104       mult = '206';
046100070104       conta = conta206;
046200070104       exsr scrivitntu;
046300070104       conta206 = 0;
046400070104       endif;
046500051228
046600051228       endif;
046700051227
046800051227      /end-free
046900051227
047000051229     c                   endsr
047100051227      *-----------------------------------------------------------------*
047200051228      * Scrivo/Aggiorno TNTUD
047300051227      *-----------------------------------------------------------------*
047400051227     c     scrivitntu    begsr
047500051227
047600051228      /free
047700051228
047800051228       //?imposto la data iso
047900060103       dataalfa = %subst(%char(savdim):3:6);
048000051228       dtaiso = %date(dataalfa:*ymd0);
048100051228
048200051228       //?imposto la chiave per tntud01l
048300051228       keytud.tudmult = mult;
048400051228       keytud.tudfil = savpog;
048500051228       keytud.tuddta = dtaiso;
048600051228
048700051228       //?aggancio tntud01l
048800051228       chain %kds(keytud:3) tntud01l;
048900051228
049000051228       //?trovo rcd aggiorno se è un ricalcolo
049100051228       if %found(tntud01l);
049200060323       if t05mult <> *blanks or t05fil <> *zeros or tudmult='042';
049300060322       if tudmult<>'042';
049400060322       tudqta = conta;
049500060322       else;
049600060322       tudqta = tudqta+conta;
049700060322       endif;
049800060322
049900051228       update tntud000;
050000060322
050100051228       else;
050200051228       if not%open(qsysprt);
050300051228       open qsysprt;
050400051228       except testa;
050500051228       endif;
050600051228       if *inof;
050700051228       except testa;
050800051228       endif;
050900051228       except riga;
051000051228       endif;
051100051228
051200051228       //?non trovo rcd scrivo
051300051228       else;
051400051228       clear tntud000;
051500051228       tudmult = mult;
051600051228       tuddta = dtaiso;
051700051228       tudfil = savpog;
051800051228       tudcli = 0;
051900051228       tudqta = conta;
052000051228       write tntud000;
052100051228       endif;
052200051228
052300051228      /end-free
052400051227
052500051229     c                   endsr
052600051228
052700051228      *-----------------------------------------------------------------*
052800051228      * Fine programmma con codice errore
052900051228      *-----------------------------------------------------------------*
053000051228     c     srfine        begsr
053100051228     c                   eval      *InLr = *On
053200051228     c                   return
053300051228
053400051228     c                   endsr
053500051227
053600051227      *-----------------------------------------------------------------*
053700051227      * Routine iniziale
053800051227      *-----------------------------------------------------------------*
053900051227     c     *inzsr        begsr
054000051227
054100051227     c     *entry        plist
054200051227     c                   parm                    kpjba
054300051227     c                   eval      tntu05ds = kpjbu
054400051227
054500060103     C     KE2TAB        KLIST
054600060103     C                   KFLD                    KTBKUT
054700060103     C                   KFLD                    KTBCOD
054800060104     c     KFiar5        Klist
054900060104     c                   Kfld                    BlpAas
055000060104     c                   Kfld                    BlpLnp
055100060104     c                   Kfld                    BlpNrs
055200060104     c                   Kfld                    BlpNsp
055300060104     c                   Kfld                    KAr5Trd
055400060322     c     ktud2         Klist
055500060322     c                   Kfld                    kmult
055600060322     c                   Kfld                    dataiso
055700060103     c
055800051229     c                   endsr
055900060103      *-----------------------------------------------------------------*
056000060103     c* Carico cod trattamento merce disk  C
056100060103      *-----------------------------------------------------------------*
056200060103     c     CarCTM        begsr
056300060103     C* CARICO TABELLA CTM DISK C
056400060103     C                   Z-ADD     1             KTBKUT
056500060103     C                   MOVE      '1B'          KTBCOD
056600060103     C                   CLEAR                   II                4 0
056700060103     C     KE2TAB        SETLL     TABEL00F
056800060103     C     KE2TAB        READE     TABEL00F                               98
056900060103     C     *IN98         DOWEQ     *OFF
057000060103     C                   MOVEL     TBLUNI        DS1B
057100060103     C     §1BDKC        IFEQ      'B'
057200060103     C     §1BDKC        OREQ      'C'
057300060103     C                   ADD       1             II
057400060103     C                   MOVEL     TBLKEY        TMC(II)
057500060103     C                   ENDIF
057600060103     C     KE2TAB        READE     TABEL00F                               98
057700060103     C                   ENDDO
057800060116     c
057900060116     C                   MOVE      '3A'          KTBCOD
058000060116     C                   CLEAR                   II                4 0
058100060116     C     KE2TAB        SETLL     TABEL00F
058200060116     C     KE2TAB        READE     TABEL00F                               98
058300060116     C     *IN98         DOWEQ     *OFF
058400060116     C                   MOVEL     TBLUNI        DS3a
058500060116     C     §3arbl        IFEQ      'R'
058600060116     C                   ADD       1             II
058700060116     C                   MOVEL     TBLKEY        rec(II)
058800060116     C                   ENDIF
058900060116     C     KE2TAB        READE     TABEL00F                               98
059000060116     C                   ENDDO
059100060116     c
059200071022     c* Carico sigle operatori da consdierare come bolle da cliente
059300071022     c*  e quello da non conteggiare
059400071022     C                   MOVE      '3M'          KTBCOD
059500071022     C                   CLEAR                   II                4 0
059600071022     C     KE2TAB        SETLL     TABEL00F
059700071022     C     KE2TAB        READE     TABEL00F                               98
059800071022     C     *IN98         DOWEQ     *OFF
059900071022     c                   if        tblflg=' '
060000071023     C                   ADD       1             II
060100071022     C                   MOVEL     TBLUNI        DS3M
060200071022     C                   MOVEL     TBLKEY        ope(II)
060300071022     C                   MOVEL     §3mdacli      opedacli(II)
060400071022     c                   endif
060500071022     c
060600071022     C     KE2TAB        READE     TABEL00F                               98
060700071022     C                   ENDDO
060800071022     c
060900060103     C*
061000060103     C                   ENDSR
061100060322     C**************************************************************************
061200060322     c* Cancella il moltiplicatore se rielaborazione
061300060322     C**************************************************************************
061400060322     C     CANCMULT      BEGSR
061500060322     c     ktud2         setll     tntud01l
061600060322     c
061700060322     c                   setoff                                       31
061800060322     c                   dow       not *in31
061900060322     c     ktud2         delete    tntud01l                           31
062000060322     c                   enddo
062100060322     c
062200060322     c                   ENDSR
062300051228      *-----------------------------------------------------------------*
062400051228     oQsysprt   e            Testa          2 02
062500051228     o                                         +  5 'Rcd già esistente in'
062600051228     o                                         +  1 'TNTUD00F'
062700060103     o                                           85 'TNTU97R'
062800051228     o                       *Date         y    110
062900051228     o                                          128 'Pag.'
063000051228     o                       Page          z    132
063100051228     o          e            Riga           1
063200051228     o                                           +1 'Rcd:'
063300051228     o                       tudmult             +1
063400051228     o                       tudfil              +1
063500051228     o                       tuddta              +1
063600051228     o                       tudqta        4     +1

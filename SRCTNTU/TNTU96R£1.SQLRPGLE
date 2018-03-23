000100051229      *PARMS CLOSQLCSR(*ENDMOD)
000200051229      *PARMS DYNUSRPRF(*OWNER)
000300051227      *-----------------------------------------------------------------*
000400051227      *  Leggo ORM x calcolo tempi
000500051227      *-----------------------------------------------------------------*
000600051227
000700051227     ftntud01l  uf a e           k disk
000800051229     ftntbe01l  uf   e           k disk
000900071029     fazorg01l  if   e           k disk
001000051228     fqsysprt   o    f  132        printer oflind(*inof) usropn
001100051227
001200051229     d conta           s                   like(tudqta)
001300051229     d contacomm       s                   like(tudqta)
001400051229     d contafissi      s                   like(tudqta)
001500051229     d contaman        s                   like(tudqta)
001600071029     d contastp        s                   like(tudqta)
001700051229     d contavas        s                   like(tudqta)
001800051227     d comfgs          s                   like(orfpog) inz
001900051228     d dataalfa        s              6
002000071029     d data_org        s              8  0 inz
002100051228     d dtaiso          s               d   inz(d'0001-01-01')
002200051227     d mult            s                   like(tudmult)
002300051228     d savdae          s                   like(orfdae)
002400080109     d savddc          s                   like(orfddc)
002500051227     d savpog          s                   like(orfpog)
002600051228     d wrkgetlista     s           4096    varying
002700071029     d $geoorm         s              1    inz(*off)
002800140619     d wOkAlert        s               n   inz(*off)
002900051228
003000051228     d keytud          ds                  likerec(tntud000:*key)
003100051229     d keytbe          ds                  likerec(tntbe000:*key)
003200051227
003300051229     d dtup          e ds
003400051228     d fnorfds       e ds                  extname(fnorf00f)
003500051228     d fnormds       e ds                  extname(fnorm00f)
003600051227     d kpjba         e ds
003700051227     d tntu05ds      e ds
003800071029     d og147         e ds
003900140619
004000140619      // - Driver per reperire mail/sms
004100140619     d FIOR900DS     e ds                  qualified inz
004200140619
004300140619      //---------------------------------------------------------------
004400140619      //?Definizione procedure utilizzate.
004500140619      //---------------------------------------------------------------
004600140619      // - Driver per reperire mail/sms
004700140619     d FIOR900R        pr                  extpgm('FIOR900R')
004800140619     d  fior900ds                          likeds(fior900ds)
004900051227
005000051227      *-----------------------------------------------------------------*
005100051227      * Moltiplicatori calcolati:
005200051227      *    420 - ORM Fissi
005300051227      *    430 - ORM VAS
005400051227      *    440 - ORM Commissionati
005500051227      *    450 - ORM Manuali
005600071029      *    460 - ORM Stampa
005700051227      *-----------------------------------------------------------------*
005800080109
005900080109      *?prima elaborazione --> TUTTO
006000051228
006100051228      *?Imposto la parte fissa dell'istruzione sql
006200051228     c                   eval      wrkgetlista =
006300051229     c                             'select orfdae, orfpog, orffar, orfpoe,' +
006400140619     c                             ' ormtco, ormflo, orfnsr, orfnor, orfnrv'+
006500051229     c                             ' from fnorf00f, fnorm00f where' +
006600051229     c                             ' orfpoe = ormpoe and orfnsr = ormnsr' +
006700051229     c                             ' and orfnor = ormnor and orfnrv = ormnrv'+
006800071029     c                             ' and orffar in(050, 100, 300)' +
006900051229     c                             'and orfnsr = 00 and orfdae between '+
007000051229     c                              %editc(t05dti:'X') + ' and ' +
007100051229     c                              %editc(t05dtf:'X')
007200051228      *?Ordinamento
007300051228     c                   eval      wrkgetlista = wrkgetlista +
007400051228     c                             ' order by orfdae, orfpog, orffar'
007500051228      *?Solo lettura
007600051228     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
007700051228
007800051228     C/EXEC SQL
007900051228     C+ PREPARE s1 FROM :wrkgetlista
008000051228     C/END-EXEC
008100051228     C
008200051228     C/EXEC SQL
008300051228     C+ DECLARE A1 CURSOR FOR S1
008400051228     C/END-EXEC
008500051228     C
008600051228     C/EXEC SQL
008700051228     C+ OPEN a1
008800051228     C/END-EXEC
008900051228
009000051228      *?Leggo il file
009100051228     c                   do        *hival
009200051228     C/EXEC SQL
009300051228     C+ FETCH NEXT FROM a1 INTO :orfdae, :orfpog, :orffar, :orfpoe, :ormtco,
009400140619     C+                         :ormflo, :orfnsr, :orfnor, :orfnrv
009500051228     C/END-EXEC
009600051228
009700051228     c                   select
009800051228
009900051228     c                   when      sqlcod = 100
010000051228     c                   exsr      aggiorna
010100051228     c                   leave
010200051228     c                   when      sqlcod < 0
010300051228     c                   seton                                        H1
010400051228     c                   exsr      srfine
010500051228     c                   other
010600051228     c                   exsr      srconta
010700051228     c                   endsl
010800051228
010900051228     c                   enddo
011000051228
011100051228     C/EXEC SQL
011200051228     C+ CLOSE a1
011300051228     C/END-EXEC
011400080109
011500080109      *?seconda elaborazione --> STAMPATI per GEO ORM
011600080109      *?Imposto la parte fissa dell'istruzione sql
011700080109     c                   clear                   wrkgetlista
011800080109     c                   eval      wrkgetlista =
011900080109     c                             'select orfddc, orfpog' +
012000080109     c                             ' from fnorf00f where' +
012100080109     c                             ' orffar = 400' +
012200080109     c                             ' and orfddc between '+
012300080109     c                             %editc(t05dti:'X') + ' and ' +
012400080109     c                             %editc(t05dtf:'X')
012500080109      *?Ordinamento
012600080109     c                   eval      wrkgetlista = wrkgetlista +
012700080109     c                             ' order by orfddc, orfpog'
012800080109      *?Solo lettura
012900080109     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
013000080109
013100080109     C/EXEC SQL
013200080109     C+ PREPARE s2 FROM :wrkgetlista
013300080109     C/END-EXEC
013400080109     C
013500080109     C/EXEC SQL
013600080109     C+ DECLARE A2 CURSOR FOR S2
013700080109     C/END-EXEC
013800080109     C
013900080109     C/EXEC SQL
014000080109     C+ OPEN a2
014100080109     C/END-EXEC
014200080109
014300080109      *?Leggo il file
014400080109     c                   do        *hival
014500080109     C/EXEC SQL
014600080109     C+ FETCH NEXT FROM a2 INTO :orfddc, :orfpog
014700080109     C/END-EXEC
014800080109
014900080109     c                   select
015000080109
015100080109     c                   when      sqlcod = 100
015200080109     c                   exsr      aggiorna2
015300080109     c                   leave
015400080109     c                   when      sqlcod < 0
015500080109     c                   seton                                        H1
015600080109     c                   exsr      srfine
015700080109     c                   other
015800080109     c                   exsr      srconta2
015900080109     c                   endsl
016000080109
016100080109     c                   enddo
016200080109
016300080109     C/EXEC SQL
016400080109     C+ CLOSE a2
016500080109     C/END-EXEC
016600051229
016700051229      *?Aggiorno data ultima elaborazione in tabella TUP
016800051229
016900051229      /free
017000051229
017100051229       //?imposto la chiave per tntbe01l
017200051229       keytbe.tbecod = 'TUP';
017300051229       keytbe.tbeke1 = kcdaz;
017400051229
017500051229       //?aggancio tntbe01l
017600051229       chain %kds(keytbe:2) tntbe01l;
017700051229
017800060103       //?trovo rcd aggiorno solo se ultima elaborazione > data tabella
017900051229       if %found(tntbe01l);
018000051229       dtup = tbeuni;
018100060103       if t05dtf >= §tupdta;
018200051229       §tupdta = t05dtf;
018300051229       tbeuni = dtup;
018400051229       update tntbe000;
018500060103       endif;
018600051229       endif;
018700051229
018800051229      /end-free
018900051228
019000051228     c                   Eval      *InLr = *On
019100051227
019200051228      *-----------------------------------------------------------------*
019300051228      * Conto gli ORM
019400051228      *-----------------------------------------------------------------*
019500051228     c     srconta       begsr
019600051228
019700051227      /free
019800051227
019900051227       //?a rottura di data aggiorno
020000051227       if orfdae <> savdae;
020100051227       if savdae > *zeros;
020200051227       exsr aggiorna;
020300051227       contafissi = 0;
020400051227       contavas = 0;
020500051227       contacomm = 0;
020600051227       contaman = 0;
020700071029       contastp = 0;
020800051227       endif;
020900051227       savdae = orfdae;
021000051227       savpog = orfpog;
021100051227       endif;
021200051227
021300051227       //?a rottura di p.o. aggiorno
021400051227       if orfpog <> savpog;
021500051227       if savpog > *zeros;
021600051227       exsr aggiorna;
021700051227       contafissi = 0;
021800051227       contavas = 0;
021900051227       contacomm = 0;
022000051227       contaman = 0;
022100071029       contastp = 0;
022200051227       endif;
022300051227       savpog = orfpog;
022400051227       endif;
022500140619
022600140619       //?Controllo se l'ORM che sto leggendo ha Alert
022700140619         wOkalert = *off;
022800140619         fior900ds.IOR900trc = 'A';
022900140619         fior900ds.IOR900rcdm = 'MA';
023000140619         fior900ds.IOR900rcds = 'S ';
023100140619         fior900ds.IOR900poe = ORFpoe;
023200140619         fior900ds.IOR900nsr = ORFnsr;
023300140619         fior900ds.IOR900nor = ORFnor;
023400140619         fior900ds.IOR900nrv = ORFnrv;
023500140619         fior900r (fior900ds);
023600140619       //?Se torna che OK dati NON devo sommarlo ai commissionati
023700140619         IF  fior900ds.OOR900err = *blanks;
023800140619           wOkalert = *on;
023900140619         ENDIF;
024000051227
024100051227       //?se richiesto un solo p.o. sommo solo se uguale
024200051228       if t05fil <> *zeros and orfpog = t05fil;
024300051227
024400060103       //?sommo gli orm fissi confermati (anche fase 50 xchè fino al 03/01/06
024500060103       //?possibili orm fissi commissionati)
024600060103       if ormtco = 'S' and (orffar = 50 or orffar = 100) and orfpog = orfpoe;
024700051227       contafissi=contafissi + 1;
024800051227       endif;
024900051227
025000051227       //?sommo gli orm vas confermati
025100051227       if (ormtco = 'F' or ormtco = 'I') and ((orffar = 50 and orfpog = orfpoe)
025200051227                                         or (orffar = 100 and orfpog = orfpoe));
025300051227       contavas=contavas + 1;
025400051227       endif;
025500051227
025600051227       //?sommo gli orm commissionati confermati
025700140619       //?ma non se hanno alert
025800140619       if %subst(ormflo:6:1) = 'S' and orffar = 100 and not wOkAlert;
025900051227       contacomm=contacomm + 1;
026000051227       endif;
026100051227
026200051227       //?sommo gli orm immessi manualmente
026300141212       if (ormtco = 'M' or ormtco = 'P' or ormtco = 'E')
026400141211                                         and ((orffar = 50 and orfpog = orfpoe)
026500051227                                         or (orffar = 100 and orfpog = orfpoe));
026600051227       contaman=contaman + 1;
026700051227       endif;
026800071029
026900071029       //?sommo gli orm stampati
027000071029       if orffar = 300;
027100071029       contastp=contastp + 1;
027200071029       endif;
027300051227
027400051228       endif;
027500051227
027600051227       //?se non è stato richiesto un solo p.o. sommo sempre
027700051227       if t05fil = *zeros;
027800051227
027900060103       //?sommo gli orm fissi confermati (anche fase 50 xchè fino al 03/01/06
028000060103       //?possibili orm fissi commissionati)
028100060103       if ormtco = 'S' and (orffar = 50 or orffar = 100) and orfpog = orfpoe;
028200051227       contafissi=contafissi + 1;
028300051227       endif;
028400051227
028500051227       //?sommo gli orm vas confermati
028600051227       if (ormtco = 'F' or ormtco = 'I') and ((orffar = 50 and orfpog = orfpoe)
028700051227                                         or (orffar = 100 and orfpog = orfpoe));
028800051227       contavas=contavas + 1;
028900051227       endif;
029000051227
029100051227       //?sommo gli orm commissionati confermati
029200140619       //?ma non se hanno alert
029300140619       if %subst(ormflo:6:1) = 'S' and orffar = 100 and not wOkAlert;
029400051227       contacomm=contacomm + 1;
029500051227       endif;
029600051227
029700051227       //?sommo gli orm immessi manualmente
029800141212       if (ormtco = 'M' or ormtco = 'P' or ormtco = 'E')
029900141211                                         and ((orffar = 50 and orfpog = orfpoe)
030000051227                                         or (orffar = 100 and orfpog = orfpoe));
030100051227       contaman=contaman + 1;
030200051227       endif;
030300071029
030400071029       //?sommo gli orm stampati
030500071029       if orffar = 300;
030600071029       contastp=contastp + 1;
030700071029       endif;
030800051227
030900051227       endif;
031000051227
031100051227      /end-free
031200051227
031300051229     c                   endsr
031400051227
031500051227      *-----------------------------------------------------------------*
031600051227      * Imposto cosa devo aggiornare
031700051227      *-----------------------------------------------------------------*
031800051227     c     aggiorna      begsr
031900071029
032000071029      *?aggancio organigramma per stabilire se filiale
032100071029      *?ha la procedura GEO ORM attiva
032200071029     c                   clear                   og147
032300071029     c                   eval      $geoorm = *off
032400071029     c     savpog        chain     azorg01l
032500071029     c                   if        %found(azorg01l)
032600071029     c                   eval      og147 = orgde7
032700071029     c                   endif
032800071029      * controllo se attivo con GEO ORM
032900071029     c                   if        §ogcgio = 'S' and §ogddao > *zeros
033000071029     c                   move      §ogddao       data_org
033100071029     c                   if        savdae >= data_org
033200071029     c                   eval      $geoorm = *on
033300071029     c                   endif
033400071029     c                   endif
033500051227
033600051227      /free
033700051227
033800051228       //?se passato il moltiplicatore aggiorno quello passato
033900051228       if t05mult <> *blanks;
034000051228       mult = t05mult;
034100051228       select;
034200051228       when mult = '420';
034300051228       conta = contafissi;
034400051228       contafissi = 0;
034500051228       when mult = '430';
034600051228       conta = contavas;
034700051228       contavas = 0;
034800051228       when mult = '440';
034900051228       conta = contacomm;
035000051228       contacomm = 0;
035100051228       when mult = '450';
035200051228       conta = contaman;
035300051228       contaman = 0;
035400071029       when mult = '460' and $geoorm = *off;
035500071029       conta = contastp;
035600071029       contastp = 0;
035700051228       endsl;
035800051228       if conta > 0;
035900051228       exsr scrivitntu;
036000051228       conta = 0;
036100051228       endif;
036200051228
036300051229       //?moltiplicatore non passato aggiorno tutti quelli previsti
036400051228       else;
036500051228
036600051227       //?orm fissi
036700051227       if contafissi > 0;
036800051227       mult = '420';
036900051227       conta = contafissi;
037000051227       exsr scrivitntu;
037100051227       contafissi = 0;
037200051227       endif;
037300051227
037400051227       //?orm vas
037500051227       if contavas > 0;
037600051227       mult = '430';
037700051227       conta = contavas;
037800051227       exsr scrivitntu;
037900051227       contavas = 0;
038000051227       endif;
038100051227
038200051227       //?orm commissionati
038300051227       if contacomm > 0;
038400051227       mult = '440';
038500051227       conta = contacomm;
038600051227       exsr scrivitntu;
038700051227       contacomm = 0;
038800051227       endif;
038900051227
039000051227       //?orm manuali
039100051227       if contaman > 0;
039200051227       mult = '450';
039300051227       conta = contaman;
039400051227       exsr scrivitntu;
039500051227       contaman = 0;
039600051227       endif;
039700071029
039800071029       //?orm stampati
039900071029       if contastp > 0 and $geoorm = *off;
040000071029       mult = '460';
040100071029       conta = contastp;
040200071029       exsr scrivitntu;
040300071029       contastp = 0;
040400071029       endif;
040500051228
040600051228       endif;
040700051227
040800051227      /end-free
040900051227
041000051229     c                   endsr
041100080109
041200080109      *-----------------------------------------------------------------*
041300080109      * Conto gli ORM stampati
041400080109      *-----------------------------------------------------------------*
041500080109     c     srconta2      begsr
041600080109
041700080109      /free
041800080109
041900080109       //?a rottura di data aggiorno
042000080109       if orfddc <> savddc;
042100080109       if savddc > *zeros;
042200080109       exsr aggiorna2;
042300080109       contastp = 0;
042400080109       endif;
042500080109       savddc = orfddc;
042600080109       savpog = orfpog;
042700080109       endif;
042800080109
042900080109       //?a rottura di p.o. aggiorno
043000080109       if orfpog <> savpog;
043100080109       if savpog > *zeros;
043200080109       exsr aggiorna2;
043300080109       contastp = 0;
043400080109       endif;
043500080109       savpog = orfpog;
043600080109       endif;
043700080109
043800080109       //?se richiesto un solo p.o. sommo solo se uguale
043900080109       if t05fil <> *zeros and orfpog = t05fil;
044000080109
044100080109       //?sommo gli orm fissi stampati
044200080109       contastp=contastp + 1;
044300080109
044400080109       endif;
044500080109
044600080109       //?se non è stato richiesto un solo p.o. sommo sempre
044700080109       if t05fil = *zeros;
044800080109
044900080109       //?sommo gli orm stampati
045000080109       contastp=contastp + 1;
045100080109
045200080109       endif;
045300080109
045400080109      /end-free
045500080109
045600080109     c                   endsr
045700080109
045800080109      *-----------------------------------------------------------------*
045900080109      * Imposto cosa devo aggiornare
046000080109      *-----------------------------------------------------------------*
046100080109     c     aggiorna2     begsr
046200080109
046300080109      *?aggancio organigramma per stabilire se filiale
046400080109      *?ha la procedura GEO ORM attiva
046500080109     c                   clear                   og147
046600080109     c                   eval      $geoorm = *off
046700080109     c     savpog        chain     azorg01l
046800080109     c                   if        %found(azorg01l)
046900080109     c                   eval      og147 = orgde7
047000080109     c                   endif
047100080109      * controllo se attivo con GEO ORM
047200080109     c                   if        §ogcgio = 'S' and §ogddao > *zeros
047300080109     c                   move      §ogddao       data_org
047400080109     c                   if        savddc >= data_org
047500080109     c                   eval      $geoorm = *on
047600080109     c                   endif
047700080109     c                   endif
047800080109
047900080109      /free
048000080109
048100080109       //?se passato il moltiplicatore aggiorno quello passato
048200080109       if t05mult <> *blanks;
048300080109       mult = t05mult;
048400080109       select;
048500080109       when mult = '460' and $geoorm = *on;
048600080109       conta = contastp;
048700080109       contastp = 0;
048800080109       endsl;
048900080109       if conta > 0;
049000080109       exsr scrivitntu2;
049100080109       conta = 0;
049200080109       endif;
049300080109
049400080109       //?moltiplicatore non passato aggiorno tutti quelli previsti
049500080109       else;
049600080109
049700080109       //?orm stampati
049800080109       if contastp > 0 and $geoorm = *on;
049900080109       mult = '460';
050000080109       conta = contastp;
050100080109       exsr scrivitntu2;
050200080109       contastp = 0;
050300080109       endif;
050400080109
050500080109       endif;
050600080109
050700080109      /end-free
050800080109
050900080109     c                   endsr
051000080109
051100051227      *-----------------------------------------------------------------*
051200051228      * Scrivo/Aggiorno TNTUD
051300051227      *-----------------------------------------------------------------*
051400051227     c     scrivitntu    begsr
051500051227
051600051228      /free
051700051228
051800051228       //?imposto la data iso
051900051228       dataalfa = %subst(%char(savdae):3:6);
052000051228       dtaiso = %date(dataalfa:*ymd0);
052100051228
052200051228       //?imposto la chiave per tntud01l
052300051228       keytud.tudmult = mult;
052400051228       keytud.tudfil = savpog;
052500051228       keytud.tuddta = dtaiso;
052600051228
052700051228       //?aggancio tntud01l
052800051228       chain %kds(keytud:3) tntud01l;
052900051228
053000051228       //?trovo rcd aggiorno se è un ricalcolo
053100051228       if %found(tntud01l);
053200051228       if t05mult <> *blanks or t05fil <> *zeros;
053300051228       tudqta = conta;
053400051228       update tntud000;
053500051228       else;
053600051228       if not%open(qsysprt);
053700051228       open qsysprt;
053800051228       except testa;
053900051228       endif;
054000051228       if *inof;
054100051228       except testa;
054200051228       endif;
054300051228       except riga;
054400051228       endif;
054500051228
054600051228       //?non trovo rcd scrivo
054700051228       else;
054800051228       clear tntud000;
054900051228       tudmult = mult;
055000051228       tuddta = dtaiso;
055100051228       tudfil = savpog;
055200051228       tudcli = 0;
055300051228       tudqta = conta;
055400051228       write tntud000;
055500051228       endif;
055600051228
055700051228      /end-free
055800051227
055900051229     c                   endsr
056000080109
056100080109      *-----------------------------------------------------------------*
056200080109      * Scrivo/Aggiorno TNTUD
056300080109      *-----------------------------------------------------------------*
056400080109     c     scrivitntu2   begsr
056500080109
056600080109      /free
056700080109
056800080109       //?imposto la data iso
056900080109       dataalfa = %subst(%char(savddc):3:6);
057000080109       dtaiso = %date(dataalfa:*ymd0);
057100080109
057200080109       //?imposto la chiave per tntud01l
057300080109       keytud.tudmult = mult;
057400080109       keytud.tudfil = savpog;
057500080109       keytud.tuddta = dtaiso;
057600080109
057700080109       //?aggancio tntud01l
057800080109       chain %kds(keytud:3) tntud01l;
057900080109
058000080109       //?trovo rcd aggiorno se è un ricalcolo
058100080109       if %found(tntud01l);
058200080109       if t05mult <> *blanks or t05fil <> *zeros;
058300080109       tudqta = conta;
058400080109       update tntud000;
058500080109       else;
058600080109       if not%open(qsysprt);
058700080109       open qsysprt;
058800080109       except testa;
058900080109       endif;
059000080109       if *inof;
059100080109       except testa;
059200080109       endif;
059300080109       except riga;
059400080109       endif;
059500080109
059600080109       //?non trovo rcd scrivo
059700080109       else;
059800080109       clear tntud000;
059900080109       tudmult = mult;
060000080109       tuddta = dtaiso;
060100080109       tudfil = savpog;
060200080109       tudcli = 0;
060300080109       tudqta = conta;
060400080109       write tntud000;
060500080109       endif;
060600080109
060700080109      /end-free
060800080109
060900080109     c                   endsr
061000051228
061100051228      *-----------------------------------------------------------------*
061200051228      * Fine programmma con codice errore
061300051228      *-----------------------------------------------------------------*
061400051228     c     srfine        begsr
061500051228     c                   eval      *InLr = *On
061600051228     c                   return
061700051228
061800051228     c                   endsr
061900051227
062000051227      *-----------------------------------------------------------------*
062100051227      * Routine iniziale
062200051227      *-----------------------------------------------------------------*
062300051227     c     *inzsr        begsr
062400051227
062500051227     c     *entry        plist
062600051227     c                   parm                    kpjba
062700051227     c                   eval      tntu05ds = kpjbu
062800051227
062900051229     c                   endsr
063000051229
063100051228      *-----------------------------------------------------------------*
063200051228     oQsysprt   e            Testa          2 02
063300051228     o                                         +  5 'Rcd già esistente in'
063400051228     o                                         +  1 'TNTUD00F'
063500051228     o                                           85 'TNTU96R'
063600051228     o                       *Date         y    110
063700051228     o                                          128 'Pag.'
063800051228     o                       Page          z    132
063900051228     o          e            Riga           1
064000051228     o                                           +1 'Rcd:'
064100051228     o                       tudmult             +1
064200051228     o                       tudfil              +1
064300051228     o                       tuddta              +1
064400051228     o                       tudqta        4     +1

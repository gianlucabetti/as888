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
001600150626     d contatel        s                   like(tudqta)
001700150626     d contamail       s                   like(tudqta)
001800071029     d contastp        s                   like(tudqta)
001900051229     d contavas        s                   like(tudqta)
002000051227     d comfgs          s                   like(orfpog) inz
002100051228     d dataalfa        s              6
002200071029     d data_org        s              8  0 inz
002300051228     d dtaiso          s               d   inz(d'0001-01-01')
002400051227     d mult            s                   like(tudmult)
002500051228     d savdae          s                   like(orfdae)
002600080109     d savddc          s                   like(orfddc)
002700051227     d savpog          s                   like(orfpog)
002800051228     d wrkgetlista     s           4096    varying
002900071029     d $geoorm         s              1    inz(*off)
003000140619     d wOkAlert        s               n   inz(*off)
003100051228
003200051228     d keytud          ds                  likerec(tntud000:*key)
003300051229     d keytbe          ds                  likerec(tntbe000:*key)
003400051227
003500051229     d dtup          e ds
003600051228     d fnorfds       e ds                  extname(fnorf00f)
003700051228     d fnormds       e ds                  extname(fnorm00f)
003800051227     d kpjba         e ds
003900051227     d tntu05ds      e ds
004000071029     d og147         e ds
004100140619
004200140619      // - Driver per reperire mail/sms
004300140619     d FIOR900DS     e ds                  qualified inz
004400140619
004500140619      //---------------------------------------------------------------
004600140619      //?Definizione procedure utilizzate.
004700140619      //---------------------------------------------------------------
004800140619      // - Driver per reperire mail/sms
004900140619     d FIOR900R        pr                  extpgm('FIOR900R')
005000140619     d  fior900ds                          likeds(fior900ds)
005100051227
005200051227      *-----------------------------------------------------------------*
005300051227      * Moltiplicatori calcolati:
005400051227      *    420 - ORM Fissi
005500051227      *    430 - ORM VAS
005600051227      *    440 - ORM Commissionati
005700150626      *    450 - ORM Manuali/Mail
005800150626      *    452 - ORM Telefonici
005900150626      *    454 - ORM Mail/Fax
006000071029      *    460 - ORM Stampa
006100051227      *-----------------------------------------------------------------*
006200080109
006300080109      *?prima elaborazione --> TUTTO
006400051228
006500051228      *?Imposto la parte fissa dell'istruzione sql
006600051228     c                   eval      wrkgetlista =
006700051229     c                             'select orfdae, orfpog, orffar, orfpoe,' +
006800140619     c                             ' ormtco, ormflo, orfnsr, orfnor, orfnrv'+
006900051229     c                             ' from fnorf00f, fnorm00f where' +
007000051229     c                             ' orfpoe = ormpoe and orfnsr = ormnsr' +
007100051229     c                             ' and orfnor = ormnor and orfnrv = ormnrv'+
007200071029     c                             ' and orffar in(050, 100, 300)' +
007300051229     c                             'and orfnsr = 00 and orfdae between '+
007400051229     c                              %editc(t05dti:'X') + ' and ' +
007500051229     c                              %editc(t05dtf:'X')
007600051228      *?Ordinamento
007700051228     c                   eval      wrkgetlista = wrkgetlista +
007800051228     c                             ' order by orfdae, orfpog, orffar'
007900051228      *?Solo lettura
008000051228     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
008100051228
008200051228     C/EXEC SQL
008300051228     C+ PREPARE s1 FROM :wrkgetlista
008400051228     C/END-EXEC
008500051228     C
008600051228     C/EXEC SQL
008700051228     C+ DECLARE A1 CURSOR FOR S1
008800051228     C/END-EXEC
008900051228     C
009000051228     C/EXEC SQL
009100051228     C+ OPEN a1
009200051228     C/END-EXEC
009300051228
009400051228      *?Leggo il file
009500051228     c                   do        *hival
009600051228     C/EXEC SQL
009700051228     C+ FETCH NEXT FROM a1 INTO :orfdae, :orfpog, :orffar, :orfpoe, :ormtco,
009800140619     C+                         :ormflo, :orfnsr, :orfnor, :orfnrv
009900051228     C/END-EXEC
010000051228
010100051228     c                   select
010200051228
010300051228     c                   when      sqlcod = 100
010400051228     c                   exsr      aggiorna
010500051228     c                   leave
010600051228     c                   when      sqlcod < 0
010700051228     c                   seton                                        H1
010800051228     c                   exsr      srfine
010900051228     c                   other
011000051228     c                   exsr      srconta
011100051228     c                   endsl
011200051228
011300051228     c                   enddo
011400051228
011500051228     C/EXEC SQL
011600051228     C+ CLOSE a1
011700051228     C/END-EXEC
011800080109
011900080109      *?seconda elaborazione --> STAMPATI per GEO ORM
012000080109      *?Imposto la parte fissa dell'istruzione sql
012100080109     c                   clear                   wrkgetlista
012200080109     c                   eval      wrkgetlista =
012300080109     c                             'select orfddc, orfpog' +
012400080109     c                             ' from fnorf00f where' +
012500080109     c                             ' orffar = 400' +
012600080109     c                             ' and orfddc between '+
012700080109     c                             %editc(t05dti:'X') + ' and ' +
012800080109     c                             %editc(t05dtf:'X')
012900080109      *?Ordinamento
013000080109     c                   eval      wrkgetlista = wrkgetlista +
013100080109     c                             ' order by orfddc, orfpog'
013200080109      *?Solo lettura
013300080109     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
013400080109
013500080109     C/EXEC SQL
013600080109     C+ PREPARE s2 FROM :wrkgetlista
013700080109     C/END-EXEC
013800080109     C
013900080109     C/EXEC SQL
014000080109     C+ DECLARE A2 CURSOR FOR S2
014100080109     C/END-EXEC
014200080109     C
014300080109     C/EXEC SQL
014400080109     C+ OPEN a2
014500080109     C/END-EXEC
014600080109
014700080109      *?Leggo il file
014800080109     c                   do        *hival
014900080109     C/EXEC SQL
015000080109     C+ FETCH NEXT FROM a2 INTO :orfddc, :orfpog
015100080109     C/END-EXEC
015200080109
015300080109     c                   select
015400080109
015500080109     c                   when      sqlcod = 100
015600080109     c                   exsr      aggiorna2
015700080109     c                   leave
015800080109     c                   when      sqlcod < 0
015900080109     c                   seton                                        H1
016000080109     c                   exsr      srfine
016100080109     c                   other
016200080109     c                   exsr      srconta2
016300080109     c                   endsl
016400080109
016500080109     c                   enddo
016600080109
016700080109     C/EXEC SQL
016800080109     C+ CLOSE a2
016900080109     C/END-EXEC
017000051229
017100051229      *?Aggiorno data ultima elaborazione in tabella TUP
017200051229
017300051229      /free
017400051229
017500051229       //?imposto la chiave per tntbe01l
017600051229       keytbe.tbecod = 'TUP';
017700051229       keytbe.tbeke1 = kcdaz;
017800051229
017900051229       //?aggancio tntbe01l
018000051229       chain %kds(keytbe:2) tntbe01l;
018100051229
018200060103       //?trovo rcd aggiorno solo se ultima elaborazione > data tabella
018300051229       if %found(tntbe01l);
018400051229       dtup = tbeuni;
018500060103       if t05dtf >= §tupdta;
018600051229       §tupdta = t05dtf;
018700051229       tbeuni = dtup;
018800051229       update tntbe000;
018900060103       endif;
019000051229       endif;
019100051229
019200051229      /end-free
019300051228
019400051228     c                   Eval      *InLr = *On
019500051227
019600051228      *-----------------------------------------------------------------*
019700051228      * Conto gli ORM
019800051228      *-----------------------------------------------------------------*
019900051228     c     srconta       begsr
020000051228
020100051227      /free
020200051227
020300051227       //?a rottura di data aggiorno
020400051227       if orfdae <> savdae;
020500051227       if savdae > *zeros;
020600051227       exsr aggiorna;
020700051227       contafissi = 0;
020800051227       contavas = 0;
020900051227       contacomm = 0;
021000051227       contaman = 0;
021100150626       contatel = 0;
021200150626       contamail = 0;
021300071029       contastp = 0;
021400051227       endif;
021500051227       savdae = orfdae;
021600051227       savpog = orfpog;
021700051227       endif;
021800051227
021900051227       //?a rottura di p.o. aggiorno
022000051227       if orfpog <> savpog;
022100051227       if savpog > *zeros;
022200051227       exsr aggiorna;
022300051227       contafissi = 0;
022400051227       contavas = 0;
022500051227       contacomm = 0;
022600051227       contaman = 0;
022700150626       contatel = 0;
022800150626       contamail = 0;
022900071029       contastp = 0;
023000051227       endif;
023100051227       savpog = orfpog;
023200051227       endif;
023300140619
023400140619       //?Controllo se l'ORM che sto leggendo ha Alert
023500140619         wOkalert = *off;
023600140619         fior900ds.IOR900trc = 'A';
023700140619         fior900ds.IOR900rcdm = 'MA';
023800140619         fior900ds.IOR900rcds = 'S ';
023900140619         fior900ds.IOR900poe = ORFpoe;
024000140619         fior900ds.IOR900nsr = ORFnsr;
024100140619         fior900ds.IOR900nor = ORFnor;
024200140619         fior900ds.IOR900nrv = ORFnrv;
024300140619         fior900r (fior900ds);
024400140619       //?Se torna che OK dati NON devo sommarlo ai commissionati
024500140619         IF  fior900ds.OOR900err = *blanks;
024600140619           wOkalert = *on;
024700140619         ENDIF;
024800051227
024900051227       //?se richiesto un solo p.o. sommo solo se uguale
025000051228       if t05fil <> *zeros and orfpog = t05fil;
025100051227
025200060103       //?sommo gli orm fissi confermati (anche fase 50 xchè fino al 03/01/06
025300060103       //?possibili orm fissi commissionati)
025400060103       if ormtco = 'S' and (orffar = 50 or orffar = 100) and orfpog = orfpoe;
025500051227       contafissi=contafissi + 1;
025600051227       endif;
025700051227
025800051227       //?sommo gli orm vas confermati
025900051227       if (ormtco = 'F' or ormtco = 'I') and ((orffar = 50 and orfpog = orfpoe)
026000051227                                         or (orffar = 100 and orfpog = orfpoe));
026100051227       contavas=contavas + 1;
026200051227       endif;
026300051227
026400051227       //?sommo gli orm commissionati confermati
026500140619       //?ma non se hanno alert
026600140619       if %subst(ormflo:6:1) = 'S' and orffar = 100 and not wOkAlert;
026700051227       contacomm=contacomm + 1;
026800051227       endif;
026900051227
027000051227       //?sommo gli orm immessi manualmente
027100141212       if (ormtco = 'M' or ormtco = 'P' or ormtco = 'E')
027200141211                                         and ((orffar = 50 and orfpog = orfpoe)
027300051227                                         or (orffar = 100 and orfpog = orfpoe));
027400051227       contaman=contaman + 1;
027500051227       endif;
027600150626
027700150626       //?sommo gli orm immessi TELEFONICI (i prepagati fanno parte dei TEL)
027800150626       if (ormtco = 'M' or ormtco = 'P') and ((orffar = 50 and orfpog = orfpoe)
027900150626                                         or (orffar = 100 and orfpog = orfpoe));
028000150626       contatel=contatel + 1;
028100150626       endif;
028200150626
028300150626       //?sommo gli orm immessi MAIL/FAX
028400150629       if ormtco = 'E'                   and ((orffar = 50 and orfpog = orfpoe)
028500150626                                         or (orffar = 100 and orfpog = orfpoe));
028600150626       contamail=contamail + 1;
028700150626       endif;
028800071029
028900071029       //?sommo gli orm stampati
029000071029       if orffar = 300;
029100071029       contastp=contastp + 1;
029200071029       endif;
029300051227
029400051228       endif;
029500051227
029600051227       //?se non è stato richiesto un solo p.o. sommo sempre
029700051227       if t05fil = *zeros;
029800051227
029900060103       //?sommo gli orm fissi confermati (anche fase 50 xchè fino al 03/01/06
030000060103       //?possibili orm fissi commissionati)
030100060103       if ormtco = 'S' and (orffar = 50 or orffar = 100) and orfpog = orfpoe;
030200051227       contafissi=contafissi + 1;
030300051227       endif;
030400051227
030500051227       //?sommo gli orm vas confermati
030600051227       if (ormtco = 'F' or ormtco = 'I') and ((orffar = 50 and orfpog = orfpoe)
030700051227                                         or (orffar = 100 and orfpog = orfpoe));
030800051227       contavas=contavas + 1;
030900051227       endif;
031000051227
031100051227       //?sommo gli orm commissionati confermati
031200140619       //?ma non se hanno alert
031300140619       if %subst(ormflo:6:1) = 'S' and orffar = 100 and not wOkAlert;
031400051227       contacomm=contacomm + 1;
031500051227       endif;
031600051227
031700051227       //?sommo gli orm immessi manualmente
031800141212       if (ormtco = 'M' or ormtco = 'P' or ormtco = 'E')
031900141211                                         and ((orffar = 50 and orfpog = orfpoe)
032000051227                                         or (orffar = 100 and orfpog = orfpoe));
032100051227       contaman=contaman + 1;
032200051227       endif;
032300150626
032400150626       //?sommo gli orm immessi TELEFONICI (i prepagati fanno parte dei TEL)
032500150626       if (ormtco = 'M' or ormtco = 'P') and ((orffar = 50 and orfpog = orfpoe)
032600150626                                         or (orffar = 100 and orfpog = orfpoe));
032700150626       contatel=contatel + 1;
032800150626       endif;
032900150626
033000150626       //?sommo gli orm immessi MAIL/FAX
033100150629       if ormtco = 'E'                   and ((orffar = 50 and orfpog = orfpoe)
033200150626                                         or (orffar = 100 and orfpog = orfpoe));
033300150626       contamail=contamail + 1;
033400150626       endif;
033500071029
033600071029       //?sommo gli orm stampati
033700071029       if orffar = 300;
033800071029       contastp=contastp + 1;
033900071029       endif;
034000051227
034100051227       endif;
034200051227
034300051227      /end-free
034400051227
034500051229     c                   endsr
034600051227
034700051227      *-----------------------------------------------------------------*
034800051227      * Imposto cosa devo aggiornare
034900051227      *-----------------------------------------------------------------*
035000051227     c     aggiorna      begsr
035100071029
035200071029      *?aggancio organigramma per stabilire se filiale
035300071029      *?ha la procedura GEO ORM attiva
035400071029     c                   clear                   og147
035500071029     c                   eval      $geoorm = *off
035600071029     c     savpog        chain     azorg01l
035700071029     c                   if        %found(azorg01l)
035800071029     c                   eval      og147 = orgde7
035900071029     c                   endif
036000071029      * controllo se attivo con GEO ORM
036100071029     c                   if        §ogcgio = 'S' and §ogddao > *zeros
036200071029     c                   move      §ogddao       data_org
036300071029     c                   if        savdae >= data_org
036400071029     c                   eval      $geoorm = *on
036500071029     c                   endif
036600071029     c                   endif
036700051227
036800051227      /free
036900051227
037000051228       //?se passato il moltiplicatore aggiorno quello passato
037100051228       if t05mult <> *blanks;
037200051228       mult = t05mult;
037300051228       select;
037400051228       when mult = '420';
037500051228       conta = contafissi;
037600051228       contafissi = 0;
037700051228       when mult = '430';
037800051228       conta = contavas;
037900051228       contavas = 0;
038000051228       when mult = '440';
038100051228       conta = contacomm;
038200051228       contacomm = 0;
038300051228       when mult = '450';
038400051228       conta = contaman;
038500051228       contaman = 0;
038600150626       when mult = '452';
038700150626       conta = contatel;
038800150626       contatel = 0;
038900150626       when mult = '454';
039000150626       conta = contamail;
039100150626       contamail = 0;
039200071029       when mult = '460' and $geoorm = *off;
039300071029       conta = contastp;
039400071029       contastp = 0;
039500051228       endsl;
039600051228       if conta > 0;
039700051228       exsr scrivitntu;
039800051228       conta = 0;
039900051228       endif;
040000051228
040100051229       //?moltiplicatore non passato aggiorno tutti quelli previsti
040200051228       else;
040300051228
040400051227       //?orm fissi
040500051227       if contafissi > 0;
040600051227       mult = '420';
040700051227       conta = contafissi;
040800051227       exsr scrivitntu;
040900051227       contafissi = 0;
041000051227       endif;
041100051227
041200051227       //?orm vas
041300051227       if contavas > 0;
041400051227       mult = '430';
041500051227       conta = contavas;
041600051227       exsr scrivitntu;
041700051227       contavas = 0;
041800051227       endif;
041900051227
042000051227       //?orm commissionati
042100051227       if contacomm > 0;
042200051227       mult = '440';
042300051227       conta = contacomm;
042400051227       exsr scrivitntu;
042500051227       contacomm = 0;
042600051227       endif;
042700051227
042800051227       //?orm manuali
042900051227       if contaman > 0;
043000051227       mult = '450';
043100051227       conta = contaman;
043200051227       exsr scrivitntu;
043300051227       contaman = 0;
043400051227       endif;
043500150626
043600150626       //?orm Telefonici
043700150626       if contatel > 0;
043800150626       mult = '452';
043900150626       conta = contatel;
044000150626       exsr scrivitntu;
044100150626       contatel = 0;
044200150626       endif;
044300150626
044400150626       //?orm Mail/Fax
044500150626       if contamail > 0;
044600150626       mult = '454';
044700150626       conta = contamail;
044800150626       exsr scrivitntu;
044900150626       contamail = 0;
045000150626       endif;
045100071029
045200071029       //?orm stampati
045300071029       if contastp > 0 and $geoorm = *off;
045400071029       mult = '460';
045500071029       conta = contastp;
045600071029       exsr scrivitntu;
045700071029       contastp = 0;
045800071029       endif;
045900051228
046000051228       endif;
046100051227
046200051227      /end-free
046300051227
046400051229     c                   endsr
046500080109
046600080109      *-----------------------------------------------------------------*
046700080109      * Conto gli ORM stampati
046800080109      *-----------------------------------------------------------------*
046900080109     c     srconta2      begsr
047000080109
047100080109      /free
047200080109
047300080109       //?a rottura di data aggiorno
047400080109       if orfddc <> savddc;
047500080109       if savddc > *zeros;
047600080109       exsr aggiorna2;
047700080109       contastp = 0;
047800080109       endif;
047900080109       savddc = orfddc;
048000080109       savpog = orfpog;
048100080109       endif;
048200080109
048300080109       //?a rottura di p.o. aggiorno
048400080109       if orfpog <> savpog;
048500080109       if savpog > *zeros;
048600080109       exsr aggiorna2;
048700080109       contastp = 0;
048800080109       endif;
048900080109       savpog = orfpog;
049000080109       endif;
049100080109
049200080109       //?se richiesto un solo p.o. sommo solo se uguale
049300080109       if t05fil <> *zeros and orfpog = t05fil;
049400080109
049500080109       //?sommo gli orm fissi stampati
049600080109       contastp=contastp + 1;
049700080109
049800080109       endif;
049900080109
050000080109       //?se non è stato richiesto un solo p.o. sommo sempre
050100080109       if t05fil = *zeros;
050200080109
050300080109       //?sommo gli orm stampati
050400080109       contastp=contastp + 1;
050500080109
050600080109       endif;
050700080109
050800080109      /end-free
050900080109
051000080109     c                   endsr
051100080109
051200080109      *-----------------------------------------------------------------*
051300080109      * Imposto cosa devo aggiornare
051400080109      *-----------------------------------------------------------------*
051500080109     c     aggiorna2     begsr
051600080109
051700080109      *?aggancio organigramma per stabilire se filiale
051800080109      *?ha la procedura GEO ORM attiva
051900080109     c                   clear                   og147
052000080109     c                   eval      $geoorm = *off
052100080109     c     savpog        chain     azorg01l
052200080109     c                   if        %found(azorg01l)
052300080109     c                   eval      og147 = orgde7
052400080109     c                   endif
052500080109      * controllo se attivo con GEO ORM
052600080109     c                   if        §ogcgio = 'S' and §ogddao > *zeros
052700080109     c                   move      §ogddao       data_org
052800080109     c                   if        savddc >= data_org
052900080109     c                   eval      $geoorm = *on
053000080109     c                   endif
053100080109     c                   endif
053200080109
053300080109      /free
053400080109
053500080109       //?se passato il moltiplicatore aggiorno quello passato
053600080109       if t05mult <> *blanks;
053700080109       mult = t05mult;
053800080109       select;
053900080109       when mult = '460' and $geoorm = *on;
054000080109       conta = contastp;
054100080109       contastp = 0;
054200080109       endsl;
054300080109       if conta > 0;
054400080109       exsr scrivitntu2;
054500080109       conta = 0;
054600080109       endif;
054700080109
054800080109       //?moltiplicatore non passato aggiorno tutti quelli previsti
054900080109       else;
055000080109
055100080109       //?orm stampati
055200080109       if contastp > 0 and $geoorm = *on;
055300080109       mult = '460';
055400080109       conta = contastp;
055500080109       exsr scrivitntu2;
055600080109       contastp = 0;
055700080109       endif;
055800080109
055900080109       endif;
056000080109
056100080109      /end-free
056200080109
056300080109     c                   endsr
056400080109
056500051227      *-----------------------------------------------------------------*
056600051228      * Scrivo/Aggiorno TNTUD
056700051227      *-----------------------------------------------------------------*
056800051227     c     scrivitntu    begsr
056900051227
057000051228      /free
057100051228
057200051228       //?imposto la data iso
057300051228       dataalfa = %subst(%char(savdae):3:6);
057400051228       dtaiso = %date(dataalfa:*ymd0);
057500051228
057600051228       //?imposto la chiave per tntud01l
057700051228       keytud.tudmult = mult;
057800051228       keytud.tudfil = savpog;
057900051228       keytud.tuddta = dtaiso;
058000051228
058100051228       //?aggancio tntud01l
058200051228       chain %kds(keytud:3) tntud01l;
058300051228
058400051228       //?trovo rcd aggiorno se è un ricalcolo
058500051228       if %found(tntud01l);
058600051228       if t05mult <> *blanks or t05fil <> *zeros;
058700051228       tudqta = conta;
058800051228       update tntud000;
058900051228       else;
059000051228       if not%open(qsysprt);
059100051228       open qsysprt;
059200051228       except testa;
059300051228       endif;
059400051228       if *inof;
059500051228       except testa;
059600051228       endif;
059700051228       except riga;
059800051228       endif;
059900051228
060000051228       //?non trovo rcd scrivo
060100051228       else;
060200051228       clear tntud000;
060300051228       tudmult = mult;
060400051228       tuddta = dtaiso;
060500051228       tudfil = savpog;
060600051228       tudcli = 0;
060700051228       tudqta = conta;
060800051228       write tntud000;
060900051228       endif;
061000051228
061100051228      /end-free
061200051227
061300051229     c                   endsr
061400080109
061500080109      *-----------------------------------------------------------------*
061600080109      * Scrivo/Aggiorno TNTUD
061700080109      *-----------------------------------------------------------------*
061800080109     c     scrivitntu2   begsr
061900080109
062000080109      /free
062100080109
062200080109       //?imposto la data iso
062300080109       dataalfa = %subst(%char(savddc):3:6);
062400080109       dtaiso = %date(dataalfa:*ymd0);
062500080109
062600080109       //?imposto la chiave per tntud01l
062700080109       keytud.tudmult = mult;
062800080109       keytud.tudfil = savpog;
062900080109       keytud.tuddta = dtaiso;
063000080109
063100080109       //?aggancio tntud01l
063200080109       chain %kds(keytud:3) tntud01l;
063300080109
063400080109       //?trovo rcd aggiorno se è un ricalcolo
063500080109       if %found(tntud01l);
063600080109       if t05mult <> *blanks or t05fil <> *zeros;
063700080109       tudqta = conta;
063800080109       update tntud000;
063900080109       else;
064000080109       if not%open(qsysprt);
064100080109       open qsysprt;
064200080109       except testa;
064300080109       endif;
064400080109       if *inof;
064500080109       except testa;
064600080109       endif;
064700080109       except riga;
064800080109       endif;
064900080109
065000080109       //?non trovo rcd scrivo
065100080109       else;
065200080109       clear tntud000;
065300080109       tudmult = mult;
065400080109       tuddta = dtaiso;
065500080109       tudfil = savpog;
065600080109       tudcli = 0;
065700080109       tudqta = conta;
065800080109       write tntud000;
065900080109       endif;
066000080109
066100080109      /end-free
066200080109
066300080109     c                   endsr
066400051228
066500051228      *-----------------------------------------------------------------*
066600051228      * Fine programmma con codice errore
066700051228      *-----------------------------------------------------------------*
066800051228     c     srfine        begsr
066900051228     c                   eval      *InLr = *On
067000051228     c                   return
067100051228
067200051228     c                   endsr
067300051227
067400051227      *-----------------------------------------------------------------*
067500051227      * Routine iniziale
067600051227      *-----------------------------------------------------------------*
067700051227     c     *inzsr        begsr
067800051227
067900051227     c     *entry        plist
068000051227     c                   parm                    kpjba
068100051227     c                   eval      tntu05ds = kpjbu
068200051227
068300051229     c                   endsr
068400051229
068500051228      *-----------------------------------------------------------------*
068600051228     oQsysprt   e            Testa          2 02
068700051228     o                                         +  5 'Rcd già esistente in'
068800051228     o                                         +  1 'TNTUD00F'
068900051228     o                                           85 'TNTU96R'
069000051228     o                       *Date         y    110
069100051228     o                                          128 'Pag.'
069200051228     o                       Page          z    132
069300051228     o          e            Riga           1
069400051228     o                                           +1 'Rcd:'
069500051228     o                       tudmult             +1
069600051228     o                       tudfil              +1
069700051228     o                       tuddta              +1
069800051228     o                       tudqta        4     +1

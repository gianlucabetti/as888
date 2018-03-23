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
002000151006     d contavasm       s                   like(tudqta)
002100051227     d comfgs          s                   like(orfpog) inz
002200051228     d dataalfa        s              6
002300071029     d data_org        s              8  0 inz
002400051228     d dtaiso          s               d   inz(d'0001-01-01')
002500051227     d mult            s                   like(tudmult)
002600051228     d savdae          s                   like(orfdae)
002700080109     d savddc          s                   like(orfddc)
002800051227     d savpog          s                   like(orfpog)
002900051228     d wrkgetlista     s           4096    varying
003000071029     d $geoorm         s              1    inz(*off)
003100140619     d wOkAlert        s               n   inz(*off)
003200151005     d wOkAuto         s               n   inz(*off)
003300151007     d wEndA3          s               n   inz(*off)
003400051228
003500051228     d keytud          ds                  likerec(tntud000:*key)
003600051229     d keytbe          ds                  likerec(tntbe000:*key)
003700051227
003800051229     d dtup          e ds
003900051228     d fnorfds       e ds                  extname(fnorf00f)
004000051228     d fnormds       e ds                  extname(fnorm00f)
004100051227     d kpjba         e ds
004200051227     d tntu05ds      e ds
004300071029     d og147         e ds
004400140619
004500140619      // - Driver per reperire mail/sms
004600140619     d FIOR900DS     e ds                  qualified inz
004700151006
004800151006      // - Driver per reperire se conf. automatica
004900151006     d FIOR901DS     e ds                  qualified inz
005000140619
005100140619      //---------------------------------------------------------------
005200140619      //?Definizione procedure utilizzate.
005300140619      //---------------------------------------------------------------
005400140619      // - Driver per reperire mail/sms
005500140619     d FIOR900R        pr                  extpgm('FIOR900R')
005600140619     d  fior900ds                          likeds(fior900ds)
005700051227
005800151006      // - Driver per reperire se conferma automatica
005900151006     d FIOR901R        pr                  extpgm('FIOR901R')
006000151006     d  fior901ds                          likeds(fior901ds)
006100151006
006200051227      *-----------------------------------------------------------------*
006300051227      * Moltiplicatori calcolati:
006400051227      *    420 - ORM Fissi
006500151006      *    430 - ORM VAS
006600151006      *    431 - ORM VAS (solo confermati a mano)
006700051227      *    440 - ORM Commissionati
006800150626      *    450 - ORM Manuali/Mail
006900150626      *    452 - ORM Telefonici
007000150626      *    454 - ORM Mail/Fax
007100071029      *    460 - ORM Stampa
007200051227      *-----------------------------------------------------------------*
007300080109
007400151007      *?prima elaborazione --> TUTTO
007500151007      *?                       non conto ORM VAS
007600051228
007700051228      *?Imposto la parte fissa dell'istruzione sql
007800051228     c                   eval      wrkgetlista =
007900051229     c                             'select orfdae, orfpog, orffar, orfpoe,' +
008000140619     c                             ' ormtco, ormflo, orfnsr, orfnor, orfnrv'+
008100051229     c                             ' from fnorf00f, fnorm00f where' +
008200051229     c                             ' orfpoe = ormpoe and orfnsr = ormnsr' +
008300051229     c                             ' and orfnor = ormnor and orfnrv = ormnrv'+
008400071029     c                             ' and orffar in(050, 100, 300)' +
008500051229     c                             'and orfnsr = 00 and orfdae between '+
008600051229     c                              %editc(t05dti:'X') + ' and ' +
008700051229     c                              %editc(t05dtf:'X')
008800051228      *?Ordinamento
008900051228     c                   eval      wrkgetlista = wrkgetlista +
009000051228     c                             ' order by orfdae, orfpog, orffar'
009100051228      *?Solo lettura
009200051228     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
009300051228
009400051228     C/EXEC SQL
009500051228     C+ PREPARE s1 FROM :wrkgetlista
009600051228     C/END-EXEC
009700051228     C
009800051228     C/EXEC SQL
009900051228     C+ DECLARE A1 CURSOR FOR S1
010000051228     C/END-EXEC
010100051228     C
010200051228     C/EXEC SQL
010300051228     C+ OPEN a1
010400051228     C/END-EXEC
010500051228
010600051228      *?Leggo il file
010700051228     c                   do        *hival
010800051228     C/EXEC SQL
010900051228     C+ FETCH NEXT FROM a1 INTO :orfdae, :orfpog, :orffar, :orfpoe, :ormtco,
011000140619     C+                         :ormflo, :orfnsr, :orfnor, :orfnrv
011100051228     C/END-EXEC
011200051228
011300051228     c                   select
011400051228
011500051228     c                   when      sqlcod = 100
011600051228     c                   exsr      aggiorna
011700051228     c                   leave
011800051228     c                   when      sqlcod < 0
011900051228     c                   seton                                        H1
012000051228     c                   exsr      srfine
012100051228     c                   other
012200051228     c                   exsr      srconta
012300051228     c                   endsl
012400051228
012500051228     c                   enddo
012600051228
012700051228     C/EXEC SQL
012800051228     C+ CLOSE a1
012900051228     C/END-EXEC
013000080109
013100080109      *?seconda elaborazione --> STAMPATI per GEO ORM
013200080109      *?Imposto la parte fissa dell'istruzione sql
013300080109     c                   clear                   wrkgetlista
013400080109     c                   eval      wrkgetlista =
013500080109     c                             'select orfddc, orfpog' +
013600080109     c                             ' from fnorf00f where' +
013700080109     c                             ' orffar = 400' +
013800080109     c                             ' and orfddc between '+
013900080109     c                             %editc(t05dti:'X') + ' and ' +
014000080109     c                             %editc(t05dtf:'X')
014100080109      *?Ordinamento
014200080109     c                   eval      wrkgetlista = wrkgetlista +
014300080109     c                             ' order by orfddc, orfpog'
014400080109      *?Solo lettura
014500080109     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
014600080109
014700080109     C/EXEC SQL
014800080109     C+ PREPARE s2 FROM :wrkgetlista
014900080109     C/END-EXEC
015000080109     C
015100080109     C/EXEC SQL
015200080109     C+ DECLARE A2 CURSOR FOR S2
015300080109     C/END-EXEC
015400080109     C
015500080109     C/EXEC SQL
015600080109     C+ OPEN a2
015700080109     C/END-EXEC
015800080109
015900080109      *?Leggo il file
016000080109     c                   do        *hival
016100080109     C/EXEC SQL
016200080109     C+ FETCH NEXT FROM a2 INTO :orfddc, :orfpog
016300080109     C/END-EXEC
016400080109
016500080109     c                   select
016600080109
016700080109     c                   when      sqlcod = 100
016800080109     c                   exsr      aggiorna2
016900080109     c                   leave
017000080109     c                   when      sqlcod < 0
017100080109     c                   seton                                        H1
017200080109     c                   exsr      srfine
017300080109     c                   other
017400080109     c                   exsr      srconta2
017500080109     c                   endsl
017600080109
017700080109     c                   enddo
017800080109
017900080109     C/EXEC SQL
018000080109     C+ CLOSE a2
018100080109     C/END-EXEC
018200151006
018300151006      /free
018400151007       //?terza elaborazione --> conto SOLO ORM VAS
018500151007       wEndA3 = *off;
018600151007       clear savdae;
018700151007       clear savpog;
018800151006       exec sql
018900151007       declare A3 cursor for
019000151007       with F1 as (select ORFpoe, ORFnsr, ORFnor, ORFnrv,
019100151007                   ORFdae, min(ORFore) as ORFore
019200151007       from FNORF00F
019300151007       where ORFdae between :T05dti and :T05dtf and
019400151007             ORFfar in(050, 100) and ORFnsr = 0
019500151007       group by ORFpoe, ORFnsr, ORFnor, ORFnrv, ORFdae),
019600151007            F2 as (select FNORF00F.*
019700151007       from FNORF00F inner join F1 on
019800151007            FNORF00F.ORFpoe = F1.ORFpoe and
019900151007            FNORF00F.ORFnsr = F1.ORFnsr and
020000151007            FNORF00F.ORFnor = F1.ORFnor and
020100151007            FNORF00F.ORFnrv = F1.ORFnrv and
020200151007            FNORF00F.ORFdae = F1.ORFdae and
020300151007            FNORF00F.ORFore = F1.ORFore)
020400151007       select F2.ORFdae, F2.ORFpog, F2.ORFfar,
020500151007              ORMpoe, ORMnsr, ORMnor, ORMnrv
020600151007       from F2 inner join FNORM00F on
020700151007            F2.ORFpoe = FNORM00F.ORMpoe and
020800151007            F2.ORFnsr = FNORM00F.ORMnsr and
020900151007            F2.ORFnor = FNORM00F.ORMnor and
021000151007            F2.ORFnrv = FNORM00F.ORMnrv
021100151007       where FNORM00F.ORMtco in('I', 'F') and
021200151007             F2.ORFpog = F2.ORFpoe
021300151007       order by F2.ORFdae, F2.ORFpog, F2.ORFfar;
021400151006
021500151007       //?Apertura del cursore
021600151007       exec sql
021700151007       OPEN A3;
021800151006
021900151007       IF  sqlcode < 0;
022000151007         exec sql
022100151007         CLOSE A3;
022200151007         wEndA3 = *on;
022300151007       ENDIF;
022400151006
022500151007       DOW  not wEndA3;
022600151007         exec sql
022700151007         FETCH NEXT from A3 into :ORFdae, :ORFpog, :ORFfar,
022800151007                                 :ORMpoe, :ORMnsr, :ORMnor,
022900151007                                 :ORMnrv;
023000151007         SELECT;
023100151007         WHEN  sqlcod = 100;
023200151007           exsr Aggiorna3;
023300151007           wEndA3 = *on;
023400151007           leave;
023500151007         WHEN  sqlcod < 0;
023600151007           wEndA3 = *on;
023700151007           exsr srfine;
023800151007         OTHER;
023900151007           exsr Conta3;
024000151007         ENDSL;
024100151007         IF  sqlcod < 0;
024200151007           wEndA3 = *on;
024300151007           leave;
024400151007         ENDIF;
024500151007       ENDDO;
024600151006
024700151007       //?Chiusura del cursore
024800151007       exec sql
024900151007       CLOSE A3;
025000051229
025100151007       //?Aggiorno data ultima elaborazione in tabella TUP
025200051229       //?imposto la chiave per tntbe01l
025300051229       keytbe.tbecod = 'TUP';
025400051229       keytbe.tbeke1 = kcdaz;
025500051229
025600051229       //?aggancio tntbe01l
025700051229       chain %kds(keytbe:2) tntbe01l;
025800051229
025900060103       //?trovo rcd aggiorno solo se ultima elaborazione > data tabella
026000051229       if %found(tntbe01l);
026100051229       dtup = tbeuni;
026200060103       if t05dtf >= §tupdta;
026300051229       §tupdta = t05dtf;
026400051229       tbeuni = dtup;
026500051229       update tntbe000;
026600060103       endif;
026700051229       endif;
026800051229
026900051229      /end-free
027000051228
027100051228     c                   Eval      *InLr = *On
027200051227
027300051228      *-----------------------------------------------------------------*
027400051228      * Conto gli ORM
027500051228      *-----------------------------------------------------------------*
027600051228     c     srconta       begsr
027700051228
027800051227      /free
027900051227
028000051227       //?a rottura di data aggiorno
028100051227       if orfdae <> savdae;
028200051227       if savdae > *zeros;
028300051227       exsr aggiorna;
028400051227       contafissi = 0;
028500051227       contacomm = 0;
028600051227       contaman = 0;
028700150626       contatel = 0;
028800150626       contamail = 0;
028900071029       contastp = 0;
029000051227       endif;
029100051227       savdae = orfdae;
029200051227       savpog = orfpog;
029300051227       endif;
029400051227
029500051227       //?a rottura di p.o. aggiorno
029600051227       if orfpog <> savpog;
029700051227       if savpog > *zeros;
029800051227       exsr aggiorna;
029900051227       contafissi = 0;
030000051227       contacomm = 0;
030100051227       contaman = 0;
030200150626       contatel = 0;
030300150626       contamail = 0;
030400071029       contastp = 0;
030500051227       endif;
030600051227       savpog = orfpog;
030700051227       endif;
030800140619
030900140619       //?Controllo se l'ORM che sto leggendo ha Alert
031000140619         wOkalert = *off;
031100140619         fior900ds.IOR900trc = 'A';
031200140619         fior900ds.IOR900rcdm = 'MA';
031300140619         fior900ds.IOR900rcds = 'S ';
031400140619         fior900ds.IOR900poe = ORFpoe;
031500140619         fior900ds.IOR900nsr = ORFnsr;
031600140619         fior900ds.IOR900nor = ORFnor;
031700140619         fior900ds.IOR900nrv = ORFnrv;
031800140619         fior900r (fior900ds);
031900140619       //?Se torna che OK dati NON devo sommarlo ai commissionati
032000140619         IF  fior900ds.OOR900err = *blanks;
032100140619           wOkalert = *on;
032200140619         ENDIF;
032300051227
032400051227       //?se richiesto un solo p.o. sommo solo se uguale
032500051228       if t05fil <> *zeros and orfpog = t05fil;
032600051227
032700060103       //?sommo gli orm fissi confermati (anche fase 50 xchè fino al 03/01/06
032800060103       //?possibili orm fissi commissionati)
032900060103       if ormtco = 'S' and (orffar = 50 or orffar = 100) and orfpog = orfpoe;
033000051227       contafissi=contafissi + 1;
033100051227       endif;
033200151006
033300051227       //?sommo gli orm commissionati confermati
033400140619       //?ma non se hanno alert
033500140619       if %subst(ormflo:6:1) = 'S' and orffar = 100 and not wOkAlert;
033600051227       contacomm=contacomm + 1;
033700051227       endif;
033800051227
033900051227       //?sommo gli orm immessi manualmente
034000141212       if (ormtco = 'M' or ormtco = 'P' or ormtco = 'E')
034100141211                                         and ((orffar = 50 and orfpog = orfpoe)
034200051227                                         or (orffar = 100 and orfpog = orfpoe));
034300051227       contaman=contaman + 1;
034400051227       endif;
034500150626
034600150626       //?sommo gli orm immessi TELEFONICI (i prepagati fanno parte dei TEL)
034700150626       if (ormtco = 'M' or ormtco = 'P') and ((orffar = 50 and orfpog = orfpoe)
034800150626                                         or (orffar = 100 and orfpog = orfpoe));
034900150626       contatel=contatel + 1;
035000150626       endif;
035100150626
035200150626       //?sommo gli orm immessi MAIL/FAX
035300150629       if ormtco = 'E'                   and ((orffar = 50 and orfpog = orfpoe)
035400150626                                         or (orffar = 100 and orfpog = orfpoe));
035500150626       contamail=contamail + 1;
035600150626       endif;
035700071029
035800071029       //?sommo gli orm stampati
035900071029       if orffar = 300;
036000071029       contastp=contastp + 1;
036100071029       endif;
036200051227
036300051228       endif;
036400051227
036500051227       //?se non è stato richiesto un solo p.o. sommo sempre
036600051227       if t05fil = *zeros;
036700051227
036800060103       //?sommo gli orm fissi confermati (anche fase 50 xchè fino al 03/01/06
036900060103       //?possibili orm fissi commissionati)
037000060103       if ormtco = 'S' and (orffar = 50 or orffar = 100) and orfpog = orfpoe;
037100051227       contafissi=contafissi + 1;
037200051227       endif;
037300051227
037400051227       //?sommo gli orm commissionati confermati
037500140619       //?ma non se hanno alert
037600140619       if %subst(ormflo:6:1) = 'S' and orffar = 100 and not wOkAlert;
037700051227       contacomm=contacomm + 1;
037800051227       endif;
037900051227
038000051227       //?sommo gli orm immessi manualmente
038100141212       if (ormtco = 'M' or ormtco = 'P' or ormtco = 'E')
038200141211                                         and ((orffar = 50 and orfpog = orfpoe)
038300051227                                         or (orffar = 100 and orfpog = orfpoe));
038400051227       contaman=contaman + 1;
038500051227       endif;
038600150626
038700150626       //?sommo gli orm immessi TELEFONICI (i prepagati fanno parte dei TEL)
038800150626       if (ormtco = 'M' or ormtco = 'P') and ((orffar = 50 and orfpog = orfpoe)
038900150626                                         or (orffar = 100 and orfpog = orfpoe));
039000150626       contatel=contatel + 1;
039100150626       endif;
039200150626
039300150626       //?sommo gli orm immessi MAIL/FAX
039400150629       if ormtco = 'E'                   and ((orffar = 50 and orfpog = orfpoe)
039500150626                                         or (orffar = 100 and orfpog = orfpoe));
039600150626       contamail=contamail + 1;
039700150626       endif;
039800071029
039900071029       //?sommo gli orm stampati
040000071029       if orffar = 300;
040100071029       contastp=contastp + 1;
040200071029       endif;
040300051227
040400051227       endif;
040500051227
040600051227      /end-free
040700051227
040800051229     c                   endsr
040900051227
041000051227      *-----------------------------------------------------------------*
041100051227      * Imposto cosa devo aggiornare
041200051227      *-----------------------------------------------------------------*
041300051227     c     aggiorna      begsr
041400071029
041500071029      *?aggancio organigramma per stabilire se filiale
041600071029      *?ha la procedura GEO ORM attiva
041700071029     c                   clear                   og147
041800071029     c                   eval      $geoorm = *off
041900071029     c     savpog        chain     azorg01l
042000071029     c                   if        %found(azorg01l)
042100071029     c                   eval      og147 = orgde7
042200071029     c                   endif
042300071029      * controllo se attivo con GEO ORM
042400071029     c                   if        §ogcgio = 'S' and §ogddao > *zeros
042500071029     c                   move      §ogddao       data_org
042600071029     c                   if        savdae >= data_org
042700071029     c                   eval      $geoorm = *on
042800071029     c                   endif
042900071029     c                   endif
043000051227
043100051227      /free
043200051227
043300051228       //?se passato il moltiplicatore aggiorno quello passato
043400051228       if t05mult <> *blanks;
043500051228       mult = t05mult;
043600051228       select;
043700051228       when mult = '420';
043800051228       conta = contafissi;
043900051228       contafissi = 0;
044000051228       when mult = '440';
044100051228       conta = contacomm;
044200051228       contacomm = 0;
044300051228       when mult = '450';
044400051228       conta = contaman;
044500051228       contaman = 0;
044600150626       when mult = '452';
044700150626       conta = contatel;
044800150626       contatel = 0;
044900150626       when mult = '454';
045000150626       conta = contamail;
045100150626       contamail = 0;
045200071029       when mult = '460' and $geoorm = *off;
045300071029       conta = contastp;
045400071029       contastp = 0;
045500051228       endsl;
045600051228       if conta > 0;
045700051228       exsr scrivitntu;
045800051228       conta = 0;
045900051228       endif;
046000051228
046100051229       //?moltiplicatore non passato aggiorno tutti quelli previsti
046200051228       else;
046300051228
046400051227       //?orm fissi
046500051227       if contafissi > 0;
046600051227       mult = '420';
046700051227       conta = contafissi;
046800051227       exsr scrivitntu;
046900051227       contafissi = 0;
047000051227       endif;
047100051227
047200051227       //?orm commissionati
047300051227       if contacomm > 0;
047400051227       mult = '440';
047500051227       conta = contacomm;
047600051227       exsr scrivitntu;
047700051227       contacomm = 0;
047800051227       endif;
047900051227
048000051227       //?orm manuali
048100051227       if contaman > 0;
048200051227       mult = '450';
048300051227       conta = contaman;
048400051227       exsr scrivitntu;
048500051227       contaman = 0;
048600051227       endif;
048700150626
048800150626       //?orm Telefonici
048900150626       if contatel > 0;
049000150626       mult = '452';
049100150626       conta = contatel;
049200150626       exsr scrivitntu;
049300150626       contatel = 0;
049400150626       endif;
049500150626
049600150626       //?orm Mail/Fax
049700150626       if contamail > 0;
049800150626       mult = '454';
049900150626       conta = contamail;
050000150626       exsr scrivitntu;
050100150626       contamail = 0;
050200150626       endif;
050300071029
050400071029       //?orm stampati
050500071029       if contastp > 0 and $geoorm = *off;
050600071029       mult = '460';
050700071029       conta = contastp;
050800071029       exsr scrivitntu;
050900071029       contastp = 0;
051000071029       endif;
051100051228
051200051228       endif;
051300051227
051400051227      /end-free
051500051227
051600051229     c                   endsr
051700080109
051800080109      *-----------------------------------------------------------------*
051900080109      * Conto gli ORM stampati
052000080109      *-----------------------------------------------------------------*
052100080109     c     srconta2      begsr
052200080109
052300080109      /free
052400080109
052500080109       //?a rottura di data aggiorno
052600080109       if orfddc <> savddc;
052700080109       if savddc > *zeros;
052800080109       exsr aggiorna2;
052900080109       contastp = 0;
053000080109       endif;
053100080109       savddc = orfddc;
053200080109       savpog = orfpog;
053300080109       endif;
053400080109
053500080109       //?a rottura di p.o. aggiorno
053600080109       if orfpog <> savpog;
053700080109       if savpog > *zeros;
053800080109       exsr aggiorna2;
053900080109       contastp = 0;
054000080109       endif;
054100080109       savpog = orfpog;
054200080109       endif;
054300080109
054400080109       //?se richiesto un solo p.o. sommo solo se uguale
054500080109       if t05fil <> *zeros and orfpog = t05fil;
054600080109
054700080109       //?sommo gli orm fissi stampati
054800080109       contastp=contastp + 1;
054900080109
055000080109       endif;
055100080109
055200080109       //?se non è stato richiesto un solo p.o. sommo sempre
055300080109       if t05fil = *zeros;
055400080109
055500080109       //?sommo gli orm stampati
055600080109       contastp=contastp + 1;
055700080109
055800080109       endif;
055900080109
056000080109      /end-free
056100080109
056200080109     c                   endsr
056300080109
056400080109      *-----------------------------------------------------------------*
056500080109      * Imposto cosa devo aggiornare
056600080109      *-----------------------------------------------------------------*
056700080109     c     aggiorna2     begsr
056800080109
056900080109      *?aggancio organigramma per stabilire se filiale
057000080109      *?ha la procedura GEO ORM attiva
057100080109     c                   clear                   og147
057200080109     c                   eval      $geoorm = *off
057300080109     c     savpog        chain     azorg01l
057400080109     c                   if        %found(azorg01l)
057500080109     c                   eval      og147 = orgde7
057600080109     c                   endif
057700080109      * controllo se attivo con GEO ORM
057800080109     c                   if        §ogcgio = 'S' and §ogddao > *zeros
057900080109     c                   move      §ogddao       data_org
058000080109     c                   if        savddc >= data_org
058100080109     c                   eval      $geoorm = *on
058200080109     c                   endif
058300080109     c                   endif
058400080109
058500080109      /free
058600080109
058700080109       //?se passato il moltiplicatore aggiorno quello passato
058800080109       if t05mult <> *blanks;
058900080109       mult = t05mult;
059000080109       select;
059100080109       when mult = '460' and $geoorm = *on;
059200080109       conta = contastp;
059300080109       contastp = 0;
059400080109       endsl;
059500080109       if conta > 0;
059600080109       exsr scrivitntu2;
059700080109       conta = 0;
059800080109       endif;
059900080109
060000080109       //?moltiplicatore non passato aggiorno tutti quelli previsti
060100080109       else;
060200080109
060300080109       //?orm stampati
060400080109       if contastp > 0 and $geoorm = *on;
060500080109       mult = '460';
060600080109       conta = contastp;
060700080109       exsr scrivitntu2;
060800080109       contastp = 0;
060900080109       endif;
061000080109
061100080109       endif;
061200080109
061300080109      /end-free
061400080109
061500080109     c                   endsr
061600151007
061700151007      /free
061800151007       //-----------------------------------------------------------------*
061900151007       // Conto gli ORM VAS
062000151007       //-----------------------------------------------------------------*
062100151007       BEGSR Conta3;
062200151007
062300151007       //?a rottura di data aggiorno
062400151007         IF  ORFdae <> savdae;
062500151007           IF  savdae > *zeros;
062600151007             exsr Aggiorna3;
062700151007             contavas = 0;
062800151007             contavasm = 0;
062900151007           ENDIF;
063000151007           savdae = ORFdae;
063100151007           savpog = ORFpog;
063200151007         ENDIF;
063300151007
063400151007       //?a rottura di p.o. aggiorno
063500151007         IF  ORFpog <> savpog;
063600151007           IF  savpog > *zeros;
063700151007             exsr Aggiorna3;
063800151007             contavas = 0;
063900151007             contavasm = 0;
064000151007           ENDIF;
064100151007           savpog = ORFpog;
064200151007         ENDIF;
064300151007
064400151007       //?Controllo se ORM confermato automaticamente in caso di ORM da VAS
064500151007         wOkAuto = *off;
064600151007         fior901ds.IOR901poe = ORMpoe;
064700151007         fior901ds.IOR901nsr = ORMnsr;
064800151007         fior901ds.IOR901nor = ORMnor;
064900151007         fior901ds.IOR901nrv = ORMnrv;
065000151013         fior901ds.IOR901far = ORFfar;
065100151007         fior901r (fior901ds);
065200151013       //?Se torna 'E' non trova ORM
065300151013         IF  fior901ds.OOR901flg = 'E';
065400151013           leavesr;
065500151013         ENDIF;
065600151013       //?Se torna 'N' non è la fase immissione
065700151013         IF  fior901ds.OOR901flg = 'N';
065800151013           leavesr;
065900151013         ENDIF;
066000151013       //?Se torna 'M' è la fase immissione MANUALE
066100151013         IF  fior901ds.OOR901flg = 'M';
066200151013           wOkAuto = *off;
066300151013         ENDIF;
066400151013       //?Se torna ' ' è la fase immissione AUTOMATICA
066500151013         IF  fior901ds.OOR901flg = *blanks;
066600151013           wOkAuto = *on;
066700151007         ENDIF;
066800151007
066900151007       //?se richiesto un solo p.o. sommo solo se uguale
067000151007         IF  T05fil <> *zeros and ORFpog = T05fil;
067100151007         //?sommo gli orm vas
067200151007           contavas += 1;
067300151007         //?sommo gli orm vas confermati manualmente
067400151007           IF  not wOkAuto;
067500151007             contavasm += 1;
067600151007           ENDIF;
067700151007         ENDIF;
067800151007
067900151007       //?se non è stato richiesto un solo p.o. sommo sempre
068000151007         IF  T05fil = *zeros;
068100151007         //?sommo gli orm vas
068200151007           contavas += 1;
068300151007         //?sommo gli orm vas confermati manualmente
068400151007           IF  not wOkAuto;
068500151007             contavasm += 1;
068600151007           ENDIF;
068700151007         ENDIF;
068800151007
068900151007       ENDSR;
069000151007
069100151007       //-----------------------------------------------------------------*
069200151007       // Imposto cosa devo aggiornare
069300151007       //-----------------------------------------------------------------*
069400151007       BEGSR Aggiorna3;
069500151007
069600151007       //?se passato il moltiplicatore aggiorno quello passato
069700151007         IF  T05mult <> *blanks;
069800151007           mult = T05mult;
069900151007           SELECT;
070000151007           WHEN  mult = '430';
070100151007             conta = contavas;
070200151007             contavas = 0;
070300151007           WHEN  mult = '431';
070400151007             conta = contavasm;
070500151007             contavasm = 0;
070600151007           ENDSL;
070700151007           IF  conta > 0;
070800151007             exsr scrivitntu;
070900151007             conta = 0;
071000151007           ENDIF;
071100151007
071200151007       //?moltiplicatore non passato aggiorno tutti quelli previsti
071300151007         ELSE;
071400151007
071500151007         //?orm vas
071600151007           IF  contavas > 0;
071700151007             mult = '430';
071800151007             conta = contavas;
071900151007             exsr scrivitntu;
072000151007             contavas = 0;
072100151007           ENDIF;
072200151007
072300151007         //?orm vas manuali
072400151007           IF  contavasm > 0;
072500151007             mult = '431';
072600151007             conta = contavasm;
072700151007             exsr scrivitntu;
072800151007             contavasm = 0;
072900151007           ENDIF;
073000151007
073100151007         ENDIF;
073200151007
073300151007       ENDSR;
073400080109
073500151007      /end-free
073600051227      *-----------------------------------------------------------------*
073700051228      * Scrivo/Aggiorno TNTUD
073800051227      *-----------------------------------------------------------------*
073900051227     c     scrivitntu    begsr
074000051227
074100051228      /free
074200051228
074300051228       //?imposto la data iso
074400051228       dataalfa = %subst(%char(savdae):3:6);
074500051228       dtaiso = %date(dataalfa:*ymd0);
074600051228
074700051228       //?imposto la chiave per tntud01l
074800051228       keytud.tudmult = mult;
074900051228       keytud.tudfil = savpog;
075000051228       keytud.tuddta = dtaiso;
075100051228
075200051228       //?aggancio tntud01l
075300051228       chain %kds(keytud:3) tntud01l;
075400051228
075500051228       //?trovo rcd aggiorno se è un ricalcolo
075600051228       if %found(tntud01l);
075700051228       if t05mult <> *blanks or t05fil <> *zeros;
075800051228       tudqta = conta;
075900051228       update tntud000;
076000051228       else;
076100051228       if not%open(qsysprt);
076200051228       open qsysprt;
076300051228       except testa;
076400051228       endif;
076500051228       if *inof;
076600051228       except testa;
076700051228       endif;
076800051228       except riga;
076900051228       endif;
077000051228
077100051228       //?non trovo rcd scrivo
077200051228       else;
077300051228       clear tntud000;
077400051228       tudmult = mult;
077500051228       tuddta = dtaiso;
077600051228       tudfil = savpog;
077700051228       tudcli = 0;
077800051228       tudqta = conta;
077900051228       write tntud000;
078000051228       endif;
078100051228
078200051228      /end-free
078300051227
078400051229     c                   endsr
078500080109
078600080109      *-----------------------------------------------------------------*
078700080109      * Scrivo/Aggiorno TNTUD
078800080109      *-----------------------------------------------------------------*
078900080109     c     scrivitntu2   begsr
079000080109
079100080109      /free
079200080109
079300080109       //?imposto la data iso
079400080109       dataalfa = %subst(%char(savddc):3:6);
079500080109       dtaiso = %date(dataalfa:*ymd0);
079600080109
079700080109       //?imposto la chiave per tntud01l
079800080109       keytud.tudmult = mult;
079900080109       keytud.tudfil = savpog;
080000080109       keytud.tuddta = dtaiso;
080100080109
080200080109       //?aggancio tntud01l
080300080109       chain %kds(keytud:3) tntud01l;
080400080109
080500080109       //?trovo rcd aggiorno se è un ricalcolo
080600080109       if %found(tntud01l);
080700080109       if t05mult <> *blanks or t05fil <> *zeros;
080800080109       tudqta = conta;
080900080109       update tntud000;
081000080109       else;
081100080109       if not%open(qsysprt);
081200080109       open qsysprt;
081300080109       except testa;
081400080109       endif;
081500080109       if *inof;
081600080109       except testa;
081700080109       endif;
081800080109       except riga;
081900080109       endif;
082000080109
082100080109       //?non trovo rcd scrivo
082200080109       else;
082300080109       clear tntud000;
082400080109       tudmult = mult;
082500080109       tuddta = dtaiso;
082600080109       tudfil = savpog;
082700080109       tudcli = 0;
082800080109       tudqta = conta;
082900080109       write tntud000;
083000080109       endif;
083100080109
083200080109      /end-free
083300080109
083400080109     c                   endsr
083500051228
083600051228      *-----------------------------------------------------------------*
083700051228      * Fine programmma con codice errore
083800051228      *-----------------------------------------------------------------*
083900051228     c     srfine        begsr
084000051228     c                   eval      *InLr = *On
084100051228     c                   return
084200051228
084300051228     c                   endsr
084400051227
084500051227      *-----------------------------------------------------------------*
084600051227      * Routine iniziale
084700051227      *-----------------------------------------------------------------*
084800051227     c     *inzsr        begsr
084900051227
085000051227     c     *entry        plist
085100051227     c                   parm                    kpjba
085200051227     c                   eval      tntu05ds = kpjbu
085300051227
085400051229     c                   endsr
085500051229
085600051228      *-----------------------------------------------------------------*
085700051228     oQsysprt   e            Testa          2 02
085800051228     o                                         +  5 'Rcd già esistente in'
085900051228     o                                         +  1 'TNTUD00F'
086000051228     o                                           85 'TNTU96R'
086100051228     o                       *Date         y    110
086200051228     o                                          128 'Pag.'
086300051228     o                       Page          z    132
086400051228     o          e            Riga           1
086500051228     o                                           +1 'Rcd:'
086600051228     o                       tudmult             +1
086700051228     o                       tudfil              +1
086800051228     o                       tuddta              +1
086900051228     o                       tudqta        4     +1

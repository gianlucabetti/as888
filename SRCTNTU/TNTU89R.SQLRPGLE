000100150629     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200150629     h alwnull(*inputonly)
000300150629      *-----------------------------------------------------------------*
000400150629      *  Leggo file reclami
000500150629      *-----------------------------------------------------------------*
000600150629     ftntud01l  uf a e           k disk
000700150629     ftntbe01l  if   e           k disk
000800150629     fqsysprt   o    f  132        printer oflind(*inof) usropn
000900150629
001000150629     d conta           s                   like(tudqta)
001100150629     d contaFA1        s                   like(tudqta)
001200150629     d contaFA2        s                   like(tudqta)
001300150629     d contaBA1        s                   like(tudqta)
001400150629     d contaBA2        s                   like(tudqta)
001500150629     d contaBC1        s                   like(tudqta)
001600150629     d contaBC2        s                   like(tudqta)
001700150629     d dataalfa        s              6
001800150629     d dtaiso          s               d   inz
001900150629     d filTP           s               n   inz
002000150629     d mult            s                   like(tudmult)
002100150629     d primo           s               n   inz
002200150629     d savdac          s                   like(tgddac)
002300150629     d savpot          s                   like(tgdpot)
002400150629     d savpct          s                   like(tgdpct)
002500150629     d wEoF            s               n   inz
002600150629     d wfil            s                   like(T05fil)
002700150629     d wrkgetlista     s           4096    varying
002800150629     d xx              s              4  0 inz
002900150629
003000150629     d keytud          ds                  likerec(tntud000:*key)
003100150629
003200150629     d dMAR          e ds
003300150629     d fitgd00f      e ds                  extname(fitgd00f)
003400150629     d fnlv55ds      e ds
003500150629     d kpjba         e ds
003600150629     d tntu05ds      e ds
003700150629     d tntudtads     e ds
003800150629
003900150629      // - Motivi Apertura da escludere
004000150629     d skMARect        s              3    dim(999)
004100150629
004200150629      //--------------------------------------------------------------
004300150629      //?Definizione prototipi procedure.                             ?
004400150629      //--------------------------------------------------------------
004500150629       // - Controllo terminal
004600150629     d FNLV55R         pr                  extpgm('FNLV55R')
004700150629     d  fnlv55ds                           likeds(FNLV55DS)
004800150629
004900150629       // - Aggiorna Tabella TUP
005000150629     d TNTUDTAR        pr                  extpgm('TNTUDTAR')
005100150629     d  tntudtads                          likeds(TNTUDTADS)
005200150629
005300150629      *-----------------------------------------------------------------*
005400150629      * Moltiplicatori calcolati:
005500150629      *    340 - FRONT - Apertura - 1° Livello RAC/GIA 2° livello TUTTI
005600150629      *    342 - FRONT - Apertura - 1° Livello NO RAC/GIA
005700150629      *    344 - BACK  - Apertura - 1° Livello RAC/GIA 2° livello TUTTI
005800150629      *    346 - BACK  - Apertura - 1° Livello NO RAC/GIA
005900150629      *    348 - BACK  - Chiusura - 1° Livello RAC/GIA 2° livello TUTTI
006000150629      *    350 - BACK  - Chiusura - 1° Livello NO RAC/GIA
006100150629      *-----------------------------------------------------------------*
006200150629
006300150629     c     *entry        plist
006400150629     c                   parm                    kpjba
006500150629
006600150629      /free
006700150629        tntu05ds = kpjbu;
006800150629
006900150629        exec sql
007000150629        set option DynUsrPrf = *Owner,
007100150629                   CloSqlCsr = *EndMod;
007200150629
007300150629       //?Caricamento dei motivi apertura Richiesta Assistenza da escludere
007400150629       //?da queste statistiche
007500150629        exsr sr_MAR_escl;
007600150629
007700150629       //?Elabora per filiale apertura
007800150629        exsr ela_POT;
007900150629
008000150629       //?Elabora per filiale chiusura
008100150629        exsr ela_PCT;
008200150629
008300150629       //?Aggiorno data ultima elaborazione in tabella TUP
008400150629         IF  T05mult = *blanks and T05fil = 0;
008500150629           clear TNTUDTADS;
008600150629           §DTAazn = kcdaz;
008700150629           §DTAdta = T05dtf;
008800150629           tntudtar (TNTUDTADS);
008900150629         ENDIF;
009000150629
009100150629       //?Fine
009200150629         exsr RoutEnd;
009300150629
009400150629       //----------------------------------------------------------------
009500150629       //?Caricamento Motivi Apertura R.A. da ESCLUDERE nel calcolo dei
009600150629       //?Tempi Ufficio
009700150629       //----------------------------------------------------------------
009800150629       BEGSR  sr_MAR_escl;
009900150629
010000150629         clear skMARect;
010100150629
010200150629       //?Metto in SK i motivi apertura da scartare
010300150629         xx = 0;
010400150629         TBEcod = 'MAR';
010500150629         setll TBEcod TNTBE01L;
010600150629         reade TBEcod TNTBE01L;
010700150629         DOW not %eof(TNTBE01L);
010800150629           dMAR = TBEuni;
010900150629           IF  §MARect = 'S';
011000150629             xx += 1;
011100150629             skMARect(xx) = %subst(TBEke1:1:3);
011200150629           ENDIF;
011300150629           reade TBEcod TNTBE01L;
011400150629         ENDDO;
011500150629
011600150629       ENDSR;
011700150629
011800150629       //----------------------------------------------------------------
011900150629       //?Elabora per filiale apertura
012000150629       //----------------------------------------------------------------
012100150629       BEGSR  ela_POT;
012200150629
012300150629         wEoF = *off;
012400150629
012500150629       //?Imposto la parte fissa dell'istruzione sql
012600150629         wrkgetlista =
012700150629         'SELECT * from FITGD00F -
012800150629          WHERE TGDdcd <> 0  and -
012900150629                TGDdac between ' +
013000150629          %editc(T05dti:'X') + ' and ' +
013100150629          %editc(T05dtf:'X') + ' and ' +
013200150629         'substr(TGDflo, 1, 1) = '' ''';
013300150629
013400150629       //?Esclusione dei motivi apertura Richiesta Assistenza
013500150629         IF  skMARect(01) <> *blanks;
013600150629           primo = *off;
013700150629           xx = 1;
013800150629           wrkgetlista +=
013900150629           ' and TGDmad not in(';
014000150629           FOR xx by 1 to %elem(skMARect);
014100150629             IF  skMARect(xx) <> *blanks;
014200150629               IF primo;
014300150629                 wrkgetlista += ', ';
014400150629               ELSE;
014500150629                 primo = *on;
014600150629               ENDIF;
014700150629               wrkgetlista += '''';
014800150629               wrkgetlista += skMARect(xx);
014900150629               wrkgetlista += '''';
015000150629             ENDIF;
015100150629           ENDFOR;
015200150629           wrkgetlista += ')';
015300150629         ENDIF;
015400150629
015500150629       //?Se passato un p.o.
015600150629         IF  T05fil <> *zeros;
015700150629           wrkgetlista += ' and digits(TGDpot) = ' +
015800150629           %triml(%editc(T05fil:'Z'));
015900150629         ENDIF;
016000150629
016100150629       //?Ordinamento
016200150629         wrkgetlista += ' order by tgdpot, tgddac, tgdcdt';
016300150629         wrkgetlista += ' for read only';
016400150629
016500150629         exec sql
016600150629         PREPARE S1 from :wrkgetlista;
016700150629
016800150629         exec sql
016900150629         DECLARE D1 cursor for S1;
017000150629
017100150629         exec sql
017200150629         open D1;
017300150629
017400150629         IF  sqlcode < 0;
017500150629           exec sql
017600150629           CLOSE D1;
017700150629           exsr RoutEnd;
017800150629         ENDIF;
017900150629
018000150629         DOW  not wEoF;
018100150629           exec sql
018200150629           FETCH NEXT from D1 into :FITGD00F;
018300150629
018400150629           SELECT;
018500150629           WHEN  sqlcod = 100;
018600150629             exsr AggiornaPOT;
018700150629             wEof = *on;
018800150629           WHEN  sqlcod < 0;
018900150629             exsr RoutEnd;
019000150629           OTHER;
019100150629             exsr ContaPOT;
019200150629           ENDSL;
019300150629
019400150629         ENDDO;
019500150629
019600150629         exec sql
019700150629         CLOSE D1;
019800150629
019900150629       ENDSR;
020000150629
020100150629       //----------------------------------------------------------------
020200150629       //?Conta le RA per filiale apertura
020300150629       //----------------------------------------------------------------
020400150629       BEGSR  ContaPOT;
020500150629
020600150629       //?a rottura di p.o. calcolo Primo/Secondo livello
020700150629         IF  TGDpot <> savpot;
020800150629           wfil = TGDpot;
020900150629           exsr callLV55;
021000150629         ENDIF;
021100150629
021200150629       //?a rottura di data aggiorno
021300150629         IF  TGDdac <> savdac;
021400150629           IF  savdac > *zeros;
021500150629             exsr aggiornaPOT;
021600150629             clear contaFA1;
021700150629             clear contaFA2;
021800150629             clear contaBA1;
021900150629             clear contaBA2;
022000150629           ENDIF;
022100150629           savdac = TGDdac;
022200150629           savpot = TGDpot;
022300150629         ENDIF;
022400150629
022500150629       //?a rottura di p.o. aggiorno
022600150629         IF  TGDpot <> savpot;
022700150629           IF  savpot > *zeros;
022800150629             exsr aggiornaPOT;
022900150629             clear contaFA1;
023000150629             clear contaFA2;
023100150629             clear contaBA1;
023200150629             clear contaBA2;
023300150629           ENDIF;
023400150629           savpot = TGDpot;
023500150629         ENDIF;
023600150629
023700150629       //?sommo RA FRONT (STATO 9)
023800150629         IF  TGDprt = '9';
023900150629         //?Se Filiale di primo livello
024000150629         //? e Utente apertura RA è RAC o GIA
024100150630         //? o Filiale di Secondo livello
024200150630           IF  (filTP and
024300150629               (%subst(TGDpru:1:3) = 'RAC' or
024400150630                %subst(TGDpru:1:3) = 'GIA')) or
024500150630                not filTP;
024600150629           //?sommo al contatore FA1
024700150629             contaFA1 += 1;
024800150629         //?Se Filiale di primo livello
024900150629         //? e Utente apertura RA non è RAC o GIA
025000150629           ELSE;
025100150629           //?sommo al contatore FA2
025200150629             contaFA2 += 1;
025300150629           ENDIF;
025400150629         ENDIF;
025500150629
025600150629       //?sommo RA BACK (STATO 8)
025700150629         IF  TGDPRT = '8';
025800150629         //?Se Filiale di primo livello
025900150629         //? e Utente apertura RA è RAC o GIA
026000150630         //? o Filiale di Secondo livello
026100150630           IF  (filTP and
026200150629               (%subst(TGDpru:1:3) = 'RAC' or
026300150630                %subst(TGDpru:1:3) = 'GIA')) or
026400150630                not filTP;
026500150629           //?sommo al contatore BA1
026600150629             contaBA1 += 1;
026700150629         //?Se Filiale di primo livello
026800150629         //? e Utente apertura RA non è RAC o GIA
026900150629           ELSE;
027000150629           //?sommo al contatore BA2
027100150629             contaBA2 += 1;
027200150629           ENDIF;
027300150629         ENDIF;
027400150629
027500150629       ENDSR;
027600150629
027700150629       //----------------------------------------------------------------
027800150629       //?Richiamo FNLV55R
027900150629       //----------------------------------------------------------------
028000150629       BEGSR  CallLV55;
028100150629
028200150629       //?controllo se Primo/Secondo livello
028300150629         clear FNLV55DS;
028400150629         D55tpt = 'P';
028500150629         D55lin = wfil;
028600150629         D55drf = %dec(%date());
028700150629         fnlv55r (FNLV55DS);
028800150629         IF  D55err <> *blanks;
028900150629           leavesr;
029000150629         ENDIF;
029100150629         IF  D55tfp = D55lin;
029200150629           filTP = *on;
029300150629         ELSE;
029400150629           filTP = *off;
029500150629         ENDIF;
029600150629
029700150629       ENDSR;
029800150629
029900150629       //----------------------------------------------------------------
030000150629       //?Aggiorna Moltiplicatori
030100150629       //----------------------------------------------------------------
030200150629       BEGSR  aggiornaPOT;
030300150629
030400150629       //?se passato il moltiplicatore aggiorno quello passato
030500150629         IF  T05mult <> *blanks;
030600150629           mult = T05mult;
030700150629
030800150629           SELECT;
030900150629           WHEN  mult = '340';
031000150629             conta = contaFA1;
031100150629             clear contaFA1;
031200150629           WHEN  mult = '342';
031300150629             conta = contaFA2;
031400150629             clear contaFA2;
031500150629           WHEN  mult = '344';
031600150629             conta = contaBA2;
031700150629             clear contaBA2;
031800150629           WHEN  mult = '346';
031900150629             conta = contaBA2;
032000150629             clear contaBA2;
032100150629           ENDSL;
032200150629
032300150629           IF  conta > 0;
032400150629             exsr wrttudPOT;
032500150629             clear conta;
032600150629           ENDIF;
032700150629
032800150629       //?moltiplicatore non passato aggiorno tutti quelli previsti
032900150629         ELSE;
033000150629
033100150629           IF  contaFA1 > 0;
033200150629             mult = '340';
033300150629             conta = contaFA1;
033400150629             exsr wrttudPOT;
033500150629           ENDIF;
033600150629           IF  contaFA2 > 0;
033700150629             mult = '342';
033800150629             conta = contaFA2;
033900150629             exsr wrttudPOT;
034000150629           ENDIF;
034100150629           IF  contaBA1 > 0;
034200150629             mult = '344';
034300150629             conta = contaBA1;
034400150629             exsr wrttudPOT;
034500150629           ENDIF;
034600150629           IF  contaBA2 > 0;
034700150629             mult = '346';
034800150629             conta = contaBA2;
034900150629             exsr wrttudPOT;
035000150629           ENDIF;
035100150629
035200150629         ENDIF;
035300150629
035400150629       ENDSR;
035500150629
035600150629       //----------------------------------------------------------------
035700150629       //?Aggiorna Moltiplicatori
035800150629       //----------------------------------------------------------------
035900150629       BEGSR  wrttudPOT;
036000150629
036100150629       //?imposto la data iso
036200150629         dataalfa = %subst(%char(savdac):3:6);
036300150629         dtaiso = %date(dataalfa:*ymd0);
036400150629
036500150629       //?imposto la chiave per tntud01l
036600150629         keytud.tudmult = mult;
036700150629         keytud.tudfil = savpot;
036800150629         keytud.tuddta = dtaiso;
036900150629
037000150629         //?aggancio tntud01l
037100150629         chain %kds(keytud:3) tntud01l;
037200150629
037300150629         //?trovo rcd aggiorno se è un ricalcolo
037400150629         IF  %found(tntud01l);
037500150629           IF  T05mult <> *blanks or T05fil <> *zeros;
037600150629             tudqta = conta;
037700150629             update tntud000;
037800150629           ELSE;
037900150629           IF  not %open(qsysprt);
038000150629             open qsysprt;
038100150629             except testa;
038200150629           ENDIF;
038300150629           IF   *inof;
038400150629             except testa;
038500150629           ENDIF;
038600150629           except riga;
038700150629           ENDIF;
038800150629
038900150629       //?non trovo rcd scrivo
039000150629         ELSE;
039100150629           clear tntud000;
039200150629           tudmult = mult;
039300150629           tuddta = dtaiso;
039400150629           tudfil = savpot;
039500150629           tudcli = 0;
039600150629           tudqta = conta;
039700150629           write tntud000;
039800150629         ENDIF;
039900150629
040000150629       ENDSR;
040100150629
040200150629       //----------------------------------------------------------------
040300150629       //?Elabora per filiale chiusura
040400150629       //----------------------------------------------------------------
040500150629       BEGSR  ela_PCT;
040600150629
040700150629         wEoF = *off;
040800150629
040900150629       //?Imposto la parte fissa dell'istruzione sql
041000150629         wrkgetlista =
041100150629         'SELECT * from FITGD00F -
041200150629          WHERE TGDdcd <> 0  and -
041300150629                TGDdac between ' +
041400150629          %editc(T05dti:'X') + ' and ' +
041500150629          %editc(T05dtf:'X') + ' and ' +
041600150629         'substr(TGDflo, 1, 1) = '' ''';
041700150629
041800150629       //?Esclusione dei motivi apertura Richiesta Assistenza
041900150629         IF  skMARect(01) <> *blanks;
042000150629           primo = *off;
042100150629           xx = 1;
042200150629           wrkgetlista +=
042300150629           ' and TGDmad not in(';
042400150629           FOR xx by 1 to %elem(skMARect);
042500150629             IF  skMARect(xx) <> *blanks;
042600150629               IF primo;
042700150629                 wrkgetlista += ', ';
042800150629               ELSE;
042900150629                 primo = *on;
043000150629               ENDIF;
043100150629               wrkgetlista += '''';
043200150629               wrkgetlista += skMARect(xx);
043300150629               wrkgetlista += '''';
043400150629             ENDIF;
043500150629           ENDFOR;
043600150629           wrkgetlista += ')';
043700150629         ENDIF;
043800150629
043900150629       //?Se passato un p.o.
044000150629         IF  T05fil <> *zeros;
044100150629           wrkgetlista += ' and digits(TGDpct) = ' +
044200150629           %triml(%editc(T05fil:'Z'));
044300150629         ENDIF;
044400150629
044500150629       //?Ordinamento
044600150629         wrkgetlista += ' order by tgdpct, tgddac, tgdcdt';
044700150629         wrkgetlista += ' for read only';
044800150629
044900150629         exec sql
045000150629         PREPARE S2 from :wrkgetlista;
045100150629
045200150629         exec sql
045300150629         DECLARE D2 cursor for S2;
045400150629
045500150629         exec sql
045600150629         open D2;
045700150629
045800150629         IF  sqlcode < 0;
045900150629           exec sql
046000150629           CLOSE D2;
046100150629           exsr RoutEnd;
046200150629         ENDIF;
046300150629
046400150629         DOW  not wEoF;
046500150629           exec sql
046600150629           FETCH NEXT from D2 into :FITGD00F;
046700150629
046800150629           SELECT;
046900150629           WHEN  sqlcod = 100;
047000150629             exsr AggiornaPCT;
047100150629             wEof = *on;
047200150629           WHEN  sqlcod < 0;
047300150629             exsr RoutEnd;
047400150629           OTHER;
047500150629             exsr ContaPCT;
047600150629           ENDSL;
047700150629
047800150629         ENDDO;
047900150629
048000150629         exec sql
048100150629         CLOSE D2;
048200150629
048300150629       ENDSR;
048400150629
048500150629       //----------------------------------------------------------------
048600150629       //?Conta le RA per filiale chiusura
048700150629       //----------------------------------------------------------------
048800150629       BEGSR  ContaPCT;
048900150629
049000150629       //?a rottura di p.o. calcolo Primo/Secondo livello
049100150629         IF  TGDpct <> savpct;
049200150629           wfil = TGDpct;
049300150629           exsr callLV55;
049400150629         ENDIF;
049500150629
049600150629       //?a rottura di data aggiorno
049700150629         IF  TGDdac <> savdac;
049800150629           IF  savdac > *zeros;
049900150629             exsr aggiornaPCT;
050000150629             clear contaBC1;
050100150629             clear contaBC2;
050200150629           ENDIF;
050300150629           savdac = TGDdac;
050400150629           savpct = TGDpct;
050500150629         ENDIF;
050600150629
050700150629       //?a rottura di p.o. aggiorno
050800150629         IF  TGDpct <> savpct;
050900150629           IF  savpct > *zeros;
051000150629             exsr aggiornaPCT;
051100150629             clear contaBC1;
051200150629             clear contaBC2;
051300150629           ENDIF;
051400150629           savpct = TGDpct;
051500150629         ENDIF;
051600150629
051700150629       //?sommo RA BACK (STATO 8)
051800150629         IF  TGDPRT = '8';
051900150629         //?Se Filiale di primo livello
052000150629         //? e Utente chiusura RA è RAC o GIA
052100150630         //? o Filiale di Secondo livello
052200150630           IF  (filTP and
052300150629               (%subst(TGDutc:1:3) = 'RAC' or
052400150630                %subst(TGDutc:1:3) = 'GIA')) or
052500150630                not filTP;
052600150629           //?sommo al contatore BC1
052700150629             contaBC1 += 1;
052800150629         //?Se Filiale di primo livello
052900150629         //? e Utente chiusura RA non è RAC o GIA
053000150629           ELSE;
053100150629           //?sommo al contatore BC2
053200150629             contaBC2 += 1;
053300150629           ENDIF;
053400150629         ENDIF;
053500150629
053600150629       ENDSR;
053700150629
053800150629       //----------------------------------------------------------------
053900150629       //?Aggiorna Moltiplicatori
054000150629       //----------------------------------------------------------------
054100150629       BEGSR  aggiornaPCT;
054200150629
054300150629       //?se passato il moltiplicatore aggiorno quello passato
054400150629         IF  T05mult <> *blanks;
054500150629           mult = T05mult;
054600150629
054700150629           SELECT;
054800150629           WHEN  mult = '348';
054900150629             conta = contaBC1;
055000150629             clear contaBC1;
055100150629           WHEN  mult = '350';
055200150629             conta = contaBC2;
055300150629             clear contaBC2;
055400150629           ENDSL;
055500150629
055600150629           IF  conta > 0;
055700150629             exsr wrttudPCT;
055800150629             clear conta;
055900150629           ENDIF;
056000150629
056100150629       //?moltiplicatore non passato aggiorno tutti quelli previsti
056200150629         ELSE;
056300150629
056400150629           IF  contaBC1 > 0;
056500150629             mult = '348';
056600150629             conta = contaBC1;
056700150629             exsr wrttudPCT;
056800150629           ENDIF;
056900150629           IF  contaBC2 > 0;
057000150629             mult = '350';
057100150629             conta = contaBC2;
057200150629             exsr wrttudPCT;
057300150629           ENDIF;
057400150629
057500150629         ENDIF;
057600150629
057700150629       ENDSR;
057800150629
057900150629       //----------------------------------------------------------------
058000150629       //?Aggiorna Moltiplicatori
058100150629       //----------------------------------------------------------------
058200150629       BEGSR  wrttudPCT;
058300150629
058400150629       //?imposto la data iso
058500150629         dataalfa = %subst(%char(savdac):3:6);
058600150629         dtaiso = %date(dataalfa:*ymd0);
058700150629
058800150629       //?imposto la chiave per tntud01l
058900150629         keytud.tudmult = mult;
059000150629         keytud.tudfil = savpct;
059100150629         keytud.tuddta = dtaiso;
059200150629
059300150629         //?aggancio tntud01l
059400150629         chain %kds(keytud:3) tntud01l;
059500150629
059600150629         //?trovo rcd aggiorno se è un ricalcolo
059700150629         IF  %found(tntud01l);
059800150629           IF  T05mult <> *blanks or T05fil <> *zeros;
059900150629             tudqta = conta;
060000150629             update tntud000;
060100150629           ELSE;
060200150629           IF  not %open(qsysprt);
060300150629             open qsysprt;
060400150629             except testa;
060500150629           ENDIF;
060600150629           IF   *inof;
060700150629             except testa;
060800150629           ENDIF;
060900150629           except riga;
061000150629           ENDIF;
061100150629
061200150629       //?non trovo rcd scrivo
061300150629         ELSE;
061400150629           clear tntud000;
061500150629           tudmult = mult;
061600150629           tuddta = dtaiso;
061700150629           tudfil = savpct;
061800150629           tudcli = 0;
061900150629           tudqta = conta;
062000150629           write tntud000;
062100150629         ENDIF;
062200150629
062300150629       ENDSR;
062400150629
062500150629       //----------------------------------------------------------------
062600150629       //?Fine
062700150629       //----------------------------------------------------------------
062800150629       BEGSR  RoutEnd;
062900150629
063000150629         *inLR = *on;
063100150629         return;
063200150629
063300150629       ENDSR;
063400150629
063500150629      /end-free
063600150629
063700150629      *-----------------------------------------------------------------*
063800150629     oQsysprt   e            Testa          2 02
063900150629     o                                         +  5 'Rcd già esistente in'
064000150629     o                                         +  1 'TNTUD00F'
064100150629     o                                           85 'TNTU89R'
064200150629     o                       *Date         y    110
064300150629     o                                          128 'Pag.'
064400150629     o                       Page          z    132
064500150629     o          e            Riga           1
064600150629     o                                           +1 'Rcd:'
064700150629     o                       tudmult             +1
064800150629     o                       tudfil              +1
064900150629     o                       tuddta              +1
065000150629     o                       tudqta        4     +1

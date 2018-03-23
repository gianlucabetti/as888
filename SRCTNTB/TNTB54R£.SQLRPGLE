000100100201       //==============================================================
000200100209       //?TNTB54R * Attribuzione serie parziale libera per filiale     ?
000300100201       //==============================================================
000400100201
000500100201       //--------------------------------------------------------------
000600100201       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700100201       //--------------------------------------------------------------
000800100201
000900100201     /*PRM closqlcsr(*endmod) dbgview(*source) dynusrprf(*owner)
001000100201     /*PRM commit(*none)
001100100201     /*END
001200100201
001300100201       //--------------------------------------------------------------
001400100201       //?Specifiche di controllo.                                     ?
001500100201       //--------------------------------------------------------------
001600100201
001700100201     h decedit('0,') datedit(*ymd/) option(*nodebugio)
001800100201     h dftactgrp(*no) actgrp(*caller)
001900100201     h alwnull(*inputonly)
002000100201
002100100201       //--------------------------------------------------------------
002200100201       //?Dichiarazione file.                                          ?
002300100201       //--------------------------------------------------------------
002400100201
002500100201
002600100201       //--------------------------------------------------------------
002700100201       //?Definizione costanti.                                        ?
002800100201       //--------------------------------------------------------------
002900100201
003000100201       // -?Numero segnacollo massimo?
003100100201     d c_NscMAX        c                   const(9999999)
003200120113
003300120113       // -?Numero serie da escludere (ALFAnumerico)?
003400120113     d c_NrsERR        c                   const('99')
003500100201
003600100201       //--------------------------------------------------------------
003700100201       //?Definizione schiere.                                         ?
003800100201       //--------------------------------------------------------------
003900100201
004000100201
004100100201       //--------------------------------------------------------------
004200100201       //?Definizione aree dati.                                       ?
004300100201       //--------------------------------------------------------------
004400100201
004500100201       // -?Dati utente?
004600100201     d §AzUte        e ds                  extname(AZUTE00F)
004700100201     d                                     dtaara
004800100201     d §DatiUte      e ds                  extname(dDatiUte)
004900100201     d                                     dtaara
005000100201
005100100201       //--------------------------------------------------------------
005200100201       //?Definizione strutture dati.                                  ?
005300100201       //--------------------------------------------------------------
005400100201
005500100201       // -?Parametri ricevuti?
005600100201     d KPJBA         e ds
005700100201     d TNTB54ds      e ds                  inz
005800100201
005900100201       // -?Parametri per Reperimento dati utente?
006000100201     d TIBS34ds      e ds                  inz
006100100201
006200100201       // -?Tabelle usate:?
006300100201       // ·?"3CP" = Serie parziali assegnate a clienti
006400100201     d d3CP          e ds                  inz
006500100201       // ·?"3EW" = LNP e dati segnacollo assegnati alla "postazione"?
006600100201     d d3EW          e ds                  inz
006700100201
006800100201       // -?Dati estratti via SQL?
006900100201     d dsTNTBE       e ds                  extname(TNTBE00F)  inz
007000100201     d ds_TAB          ds                  inz
007100100201     d  SQL_tab                            like(TBEcod)      inz                  3=> 1-  3
007200100201     d  SQL_atb                            like(TBEatb)      inz                  1=> 4-  4
007300100201     d  SQL_ke1                            like(TBEke1)      inz                 15=> 5- 19
007400100201     d  SQL_ssi                       9a                                          9=>20- 28
007500100201     d  SQL_uni                            like(TBEuni)      inz                256=>29-284
007600100201
007700100201       // -?Ridefinizione chiave tabella TBEKE2 per tab. "3CP"?
007800100201     d ds_Nrs_Nsi      ds             9    inz
007900100201     d   wNRS                         2s 0 inz
008000100201     d   wNSI                         7s 0 inz(*zero)
008100100201
008200100201       //--------------------------------------------------------------
008300100201       //?Definizione variabili globali.                               ?
008400100201       //--------------------------------------------------------------
008500110209
008600110209       // -?Parametri ricevuti?
008700110209     d P_ubLeg3C_SKC   s           3200
008800100201
008900100201       // -?Flags booleani?
009000100201     d $Fine           s               n   inz
009100120111     d $SerieCompl     s               n   inz
009200100201
009300100201       // -?Stringa SQL da eseguire?
009400100201     d wrkGetLista     s           4096    varying  inz
009500100201
009600100201       // -?Campi di comodo?
009700100201     d w_Dal           s              8s 0 inz
009800100201     d w_Al            s              8s 0 inz
009900100201     d SAVnrs          s                   like(wNrs)  inz
010000100201
010100100201       //--------------------------------------------------------------
010200100201       //?Definizione prototipi procedure.                             ?
010300100201       //--------------------------------------------------------------
010400100201
010500100201       // -?Reperimento dati utente?
010600100201      /copy gaitrasrc/srcProtoPR,TIBS34R
010700110208
010800110208       // -?Parametri per reperimento legami in tab. "3C"?
010900110208      /copy gaitrasrc/srcProtoPI,UBLEG3C
011000110208     ***  // -?Reperimento legami in tab. "3C"?
011100110208     *** /copy gaitrasrc/srcProtoPR,UBLEG3C
011200100201
011300100201       //--------------------------------------------------------------
011400100201       //?Definizione key-list.                                        ?
011500100201       //--------------------------------------------------------------
011600100201
011700100201
011800100201       //--------------------------------------------------------------
011900100201       //?M A I N - L I N E                                            ?
012000100201       //--------------------------------------------------------------
012100100201
012200100201     c     *Entry        plist
012300100201     c                   parm                    KPJBA
012400110209     c                   parm                    p_ubLeg3C_SKC
012500100201
012600100201      /free
012700100201
012800100201       // -?Operazioni iniziali?
012900100201       exsr  sr_RoutInz;
013000100201
013100100201       // -?Elaborazione SQL?
013200100201       doW  Not $Fine;
013300100201         exsr  sr_Elab;
013400100201       enddo;
013500051228
013600100201       // -?Operazioni finali?
013700100201       exsr  sr_RoutEnd;
013800100201
013900100201       //--------------------------------------------------------------
014000100201       //?Operazioni iniziali.                                         ?
014100100201       //--------------------------------------------------------------
014200100201       BEGSR  sr_RoutInz;
014300100201
014400100201         exec sql   set option dynusrprf = *owner, closqlcsr = *endmod;
014500100201         *inLR = *on;
014600100201
014700100201         TNTB54ds = KPJBU;
014800110209
014900110209         if  %parms() > 1;
015000110209           ubLeg3C_skC = P_ubLeg3C_skC;
015100110209         endif;
015200100201
015300100201         // -?Pulizia parametri di output?
015400100201         clear  O54nrs;
015500100201         clear  O54dal;
015600100201         clear  O54al;
015700100201         clear  O54err;
015800100201         clear  O54prz;
015900100201         clear  O54ksc;
016000100201
016100100201         // -?Reperimento dati job?
016200100201         exsr  sr_DatiJob;
016300120113
016400120113         // -?Controllo parameetri ricevuti?
016500120113         if  I54nrs = c_NrsERR;
016600120113           O54err = '9';
016700120113           $Fine = *on;
016800120113           leavesr;
016900120113         endif;
017000120110
017100120110         // -?Compilazione stringa SQL?
017200120110         wrkGetLista = 'with +
017300120110
017400120110         // -?- selezione da tab. "3CP"
017500120110                        Tab_3CP as +
017600120110                        (select TBEcod as SQL_cod, TBEatb, +
017700120110                                substr(TBEke1, 1, 7) as SQL_ksc, +
017800120110                                substr(TBEke2, 1, 9) as SQL_ssi, +
017900120110                                TBEuni +
018000120110                           from TNTBE00F +
018100120110                          where TBEcod = ''3CP'' +
018200120113                            and TBEatb = '' '' +
018300120110         // -?- - filiale segnacollo?
018400120110                            and substr(TBEke1, 1, 3) = ''' +
018500120110                                I54fil + '''';
018600120110         // -?- - serie segnacollo?
018700120110         if  I54nrs > *zero;
018800120110           wrkGetLista += ' and substr(TBEke2, 1, 2) = ''' +
018900120110                                I54nrs + '''';
019000120110         endif;
019100120110         wrkGetLista += '), +
019200120110
019300120110         // -?- selezione da tab. "3EW"?
019400120110                        Tab_3EW as +
019500120110                        (select TBEcod as SQL_cod, TBEatb, +
019600120110                                substr(TBEke1, 2, 7) as SQL_ksc, +
019700120110                                substr(TBEuni, 7, 9) as SQL_ssi, +
019800120110                                TBEuni +
019900120110                           from TNTBE00F +
020000120110                          where TBEcod = ''3EW'' +
020100120113                            and TBEatb = '' '' +
020200120110         // -?- - filiale segnacollo?
020300120110                            and substr(TBEuni, 4, 3) = ''' +
020400120110                                I54fil + '''';
020500120110         // -?- - serie segnacollo?
020600120110         if  I54nrs > *zero;
020700120110           wrkGetLista += ' and substr(TBEuni, 7, 2) = ''' +
020800120110                                I54nrs + '''';
020900120110         endif;
021000120110         wrkGetLista += '), +
021100120110
021200120110         // -?- selezione da tab. "3C" (senza range segnacollo)?
021300120110                         Tab_3C as +
021400120110                        (select TBLcod as SQL_cod, TBLflg, +
021500120110                                substr(TBLkey, 1, 7) as SQL_ksc, +
021600120110                                (substr(TBLuni, 1, 2) concat ''0000001'') +
021700120110                                   as SQL_ssi, ''9999999'' +
021800120110                           from TABEL00F exception join Tab_3CP +
021900120110                             on TBLcod = ''3C'' +
022000120110                            and TBLkey = Tab_3CP.SQL_ksc +
022100120110                            and substr(TBLuni, 1, 2) = +
022200120110                                substr(Tab_3CP.SQL_ssi, 1, 2) +
022300120110                                         exception join Tab_3EW +
022400120110                             on TBLcod = ''3C'' +
022500120110                            and TBLkey = Tab_3EW.SQL_ksc +
022600120110                            and substr(TBLuni, 1, 2) = +
022700120110                                substr(Tab_3EW.SQL_ssi, 1, 2) +
022800120113                          where TBLcod = ''3C'' +
022900120113                            and TBLflg = '' '' +
023000120110         // -?- - filiale segnacollo?
023100120113                            and substr(TBLuni, 36, 3) = ''' +
023200120110                                I54fil + '''';
023300120110         // -?- - serie segnacollo?
023400120110         if  I54nrs > *zero;
023500120110           wrkGetLista += ' and substr(TBLuni, 1, 2) = ''' +
023600120110                                I54nrs + '''';
023700120110         endif;
023800120110         wrkGetLista += ') +
023900120110
024000120110         // -?- estrazione dati
024100120110                         select * from Tab_3CP +
024200120110                          union +
024300120110                         select * from Tab_3EW +
024400120110                          union +
024500120110                         select * from Tab_3C +
024600120110
024700120110         // -?- impostazioni finali?
024800120110                          order by SQL_ssi, SQL_cod +
024900100202                            for read only';
025000100201
025100100201         // -?Start SQL?
025200100201         exec sql   prepare S1   from :wrkGetLista;
025300100201         exec sql   declare C1   cursor for S1;
025400100201         exec sql   open C1;
025500100201
025600100201       ENDSR;
025700100201
025800100201       //--------------------------------------------------------------
025900100201       //?Reperimento Dati del job (Utente/Operativi).                 ?
026000100201       //--------------------------------------------------------------
026100100201       BEGSR  sr_DatiJob;
026200100201
026300100201         in(e) §AzUte;
026400100201         if NOT %error;
026500100201           in(e) §DatiUte;
026600100201         endif;
026700100201         if %error or RSut = *blank;
026800100201           tibs34r ( tibs34ds );
026900100201           in §AzUte;
027000100201           in §DatiUte;
027100100201         endif;
027200100201
027300100201       ENDSR;
027400100201
027500100201       //--------------------------------------------------------------
027600100201       //?Elaborazione singola estrazione SQL.                         ?
027700100201       //--------------------------------------------------------------
027800100201       BEGSR  sr_Elab;
027900100201
028000100201         exec sql   fetch next   from C1   into :ds_TAB;
028100100201
028200100201         select;
028300100201           when  SQLcode = 100;
028400100201             $Fine = *on;
028500100201             leavesr;
028600100201           when  SQLcode < *zero;
028700100201             dump(a);
028800100201             O54err = 'S';
028900100201             $Fine  = *on;
029000100201             leavesr;
029100100201           other;
029200100201             exsr  sr_ElabRec;
029300100201         endsl;
029400100201
029500100201       ENDSR;
029600100201
029700100201       //--------------------------------------------------------------
029800100201       //?Elaborazione singolo record estratto via SQL.                ?
029900100201       //--------------------------------------------------------------
030000100201       BEGSR  sr_ElabRec;
030100120111
030200120111         if  I54tel = 'C'   or   I54nrs > '00';
030300120111           O54prz = 'P';
030400120111         endif;
030500100201
030600120111         select;
030700120111
030800120111           // -?SE richiesto CONTROLLO:?
030900120111           //  ?si scartano i record annullati o del cliente ricevuto?
031000120111           when  I54tel = 'C'   and  ( SQL_atb = 'A'    or
031100120111                                       I54ksc  = %trimr(SQL_ke1) );
031200120111             leavesr;
031300120111
031400120111           // -?SE richiesto RICERCA:?
031500120111           //  ?inutile elaborare i record con range di una serie per?
031600120111           //  ?la quale è già stato rilevato un range totale?
031700120111           when  I54tel = 'R'   and  %subst( SQL_ssi : 1 : 2 ) =
031800120111                                     %editc( SAVnrs : 'X' )
031900120111                                and  $SerieCompl;
032000120111             leavesr;
032100120111
032200120111         endsl;
032300100201
032400100201         // -?Impostazione dati letti?
032500100201         clear  ds_Nrs_Nsi;
032600100201         ds_Nrs_Nsi = SQL_ssi;
032700100201         clear  d3CP;
032800100201         clear  d3EW;
032900100201         select;
033000120110           when  SQL_tab = '3C';
033100100201           when  SQL_tab = '3CP';
033200100201             d3CP = SQL_uni;
033300100201           when  SQL_tab = '3EW';
033400100201             d3EW = SQL_uni;
033500100201         endsl;
033600100201
033700100201         // -?Rottura numero serie?
033800100201         if  wNrs <> SAVnrs;
033900120111           reset  $SerieCompl;
034000100201           if  SAVnrs > *zero;
034100100201             if  I54tel = 'R'    and
034200100201                 w_Dal > *zero   and   w_Dal < c_NscMAX   and
034300100201                 w_Al  = *zero;
034400100201               w_Al  = w_Dal + (I54tot - 1);
034500100201               if  w_Al <= c_NscMAX;
034600100201                 O54nrs = SAVnrs;
034700100201                 O54dal = w_Dal;
034800100201                 O54al  = w_Al;
034900100201                 $Fine  = *on;
035000100201                 leavesr;
035100100201               endif;
035200100201             endif;
035300100201             clear  w_Dal;
035400100201             clear  w_Al;
035500100201           endif;
035600100201           SAVnrs = wNrs;
035700100201         endif;
035800120111
035900120111         if  SQL_tab = '3C'   or
036000120111            (SQL_tab = '3CP'  and  wNsi = 1  and  §3CPal  = *hival)  or
036100120111            (SQL_tab = '3EW'  and  wNsi = 1  and  §3EWnsf = *hival);
036200120111           $SerieCompl = *on;
036300120111         endif;
036400100201
036500100201         // -OPERAZIONI PER TIPO ELABORAZIONE "RICERCA"?
036600100201         IF  I54tel = 'R';
036700100201
036800100201           select;
036900100201
037000120111             // -?Serie già assegnata come completa?
037100120111             //  ?(inutile elaborare questo record)?
037200120111             when  $SerieCompl;
037300120111               leavesr;
037400120113
037500120113             // -?Serie 99: NON gestibile?
037600120113             when  wNRS = *hival;
037700120113               $Fine  = *on;
037800120113               leavesr;
037900120111
038000100201             // -?Ricerca del segnacollo iniziale?
038100100201             //  ?trovato subito il range (iniziale) libero?
038200100201             when  w_Dal = *zero   and
038300100616                   wNSI  > 1       and   wNSI  > I54tot;
038400100201               O54dal = 1;
038500100201               O54al  = I54tot;
038600100201               O54nrs = wNrs;
038700100201               $Fine  = *on;
038800100201               leavesr;
038900100201
039000100201             // -?Ricerca del segnacollo iniziale:?
039100100201             //  ?assume come nuovo numero-dal il numero-al + 1?
039200100201             //  ?e prosegue nella lettura?
039300100616             when  w_Dal   = *zero   and
039400100201                 ((SQL_tab = '3CP'   and   §3CPal  < c_NscMAX)   OR
039500100201                  (SQL_tab = '3EW'   and   §3EWnsf < c_NscMAX));
039600100201               select;
039700100201                 when  SQL_tab = '3CP';
039800100201                   w_Dal = §3CPal + 1;
039900100201                 when  SQL_tab = '3EW';
040000100201                   w_Dal = §3EWnsf + 1;
040100100201               endsl;
040200100201
040300100201             // -?Ricerca del segnacollo finale?
040400100201             when  w_Al = *zero;
040500100201               w_Al = (wNSI - 1);
040600100616               if  (w_Al - w_Dal + 1) >= I54tot;
040700100201                 O54dal = w_Dal;
040800100201                 O54al  = w_Dal + (I54tot - 1);
040900100201                 O54nrs = wNrs;
041000100201                 $Fine  = *on;
041100100201                 leavesr;
041200100201               else;
041300100201                 // -?Range insufficiente:?
041400100201                 //  ?assume come nuovo numero-dal il numero-al + 1?
041500110221                 //  ?(SE RANGE NON GIÀ SOVRAPPOSTO)?
041600100201                 //  ?e prosegue nella lettura?
041700100201                 select;
041800100201                   when  SQL_tab = '3CP';
041900110221                     if  w_Dal <= §3CPal;
042000110221                       w_Dal = §3CPal + 1;
042100110221                     endif;
042200100201                   when  SQL_tab = '3EW';
042300110221                     if  w_Dal <= §3EWnsf;
042400110221                       w_Dal = §3EWnsf + 1;
042500110221                     endif;
042600100201                 endsl;
042700100201                 clear w_Al;
042800100201               endif;
042900100201           endsl;
043000100201
043100100201         // -OPERAZIONI PER TIPO ELABORAZIONE "CONTROLLO"?
043200100201         ELSE;
043300100201
043400100201           select;
043500100201
043600120113             // -?Range parziale valido?
043700100201             when  I54dal < wNSI   and   I54al < wNSI;
043800100201               $Fine = *on;
043900100201               leavesr;
044000100201
044100100201             // -?Range superiore a quello in input?
044200100201             //  ?(passaggio alla lettura successiva)?
044300100201             when  (SQL_tab = '3CP'    and
044400100201                    I54dal  > §3CPal   and   §3CPal  < c_NscMAX)   OR
044500100201                   (SQL_tab = '3EW'    and
044600100201                    I54dal  > §3EWnsf  and   §3EWnsf < c_NscMAX);
044700100201               leavesr;
044800100201
044900110208             // -?Range già in uso (da cliente con stesso padre)?
045000110208             when  (SQL_tab = '3CP'    and
045100110209                    %lookup( %int( %trimr( SQL_ke1 ) )
045200110208                             : sch_SKC ) > *zero)  OR
045300110208                   (SQL_tab = '3EW'    and
045400110209                    %lookup( %int( %trimr( %subst( SQL_ke1 : 2 ) ) )
045500110208                             : sch_SKC ) > *zero);
045600110208
045700100210             // -?Range già in uso (da altro cliente/padre)?
045800100201             other;
045900100201               O54err = '1';
046000100201               O54ksc = SQL_ke1;
046100100201               O54dal = wNSI;
046200100201               select;
046300120110                 when  SQL_tab = '3C ';
046400120110                   O54al  = *hival;
046500120110                   clear  O54prz;
046600100201                 when  SQL_tab = '3CP';
046700100201                   O54al  = §3CPal;
046800100201                 when  SQL_tab = '3EW';
046900100201                   O54al  = §3EWnsf;
047000100201               endsl;
047100100201               O54nrs = wNRS;
047200100201               $Fine  = *on;
047300100201               leavesr;
047400100201
047500100201           endsl;
047600100201
047700100201         ENDIF;
047800100201
047900100201       ENDSR;
048000100201
048100100201       //--------------------------------------------------------------
048200100201       //?Operazioni finali.                                           ?
048300100201       //--------------------------------------------------------------
048400100201       BEGSR  sr_RoutEnd;
048500100201
048600100201         // -?End SQL?
048700100201         exec SQL   close C1;
048800100201
048900100201         // -?Impostazione parametri in uscita (se NO *err)?
049000100201         IF  O54err = *blank;
049100100201
049200100201           if  I54tel = 'R'    and   O54dal = *zero   and
049300100201               w_Dal  > *zero  and   w_Al   = *zero   and
049400100201               w_Dal  < c_NscMAX;
049500100201             w_Al = ( w_Dal + (I54tot - 1) );
049600100201             if  w_Al <= c_NscMAX;
049700100201               O54al  = w_Al;
049800100201               O54nrs = wNrs;
049900100201               O54dal = w_Dal;
050000100201             endif;
050100100201           endif;
050200100201
050300100201           // -?SE richiesta ricerca per una particolare serie e il?
050400100201           //  ?*pgm NON ha trovato nessun range assegnabile (tutto?
050500100201           //  ?già impegnato): restituisce errore al chiamante?
050600100201           if  I54tel = 'R'   and   O54dal = *zero   and
050700100201               O54prz = 'P';
050800100201             O54err = '1';
050900100201           endif;
051000100201
051100100201         ENDIF;
051200100201
051300100201         // -?Uscita?
051400100201         KPJBU = TNTB54ds;
051500100201         *inLR = *on;
051600100201         return;
051700100201
051800100201       ENDSR;

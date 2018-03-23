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
006900120130     d dsTNTBE       e ds                  extname(TNTBE00F) inz
007000100201     d ds_TAB          ds                  inz
007100100201     d  SQL_tab                            like(TBEcod)      inz                  3=> 1-  3
007200100201     d  SQL_atb                            like(TBEatb)      inz                  1=> 4-  4
007300100201     d  SQL_ke1                            like(TBEke1)      inz                 15=> 5- 19
007400120130     d  SQL_ssi                       9a                     inz                  9=>20- 28
007500100201     d  SQL_uni                            like(TBEuni)      inz                256=>29-284
007600100201
007700100201       // -?Ridefinizione chiave tabella TBEKE2 per tab. "3CP"?
007800100201     d ds_Nrs_Nsi      ds             9    inz
007900100201     d   wNRS                         2s 0 inz
008000100201     d   wNSI                         7s 0 inz(*zero)
008100120131
008200120131       // -?*Temp x *Test (ogni elemento lungo 60 char)?
008300120220     d*// ds_Temp         ds          4140    inz
008400120220     d*//   sk_Temp                     60    inz  dim(69)
008500100201
008600100201       //--------------------------------------------------------------
008700100201       //?Definizione variabili globali.                               ?
008800100201       //--------------------------------------------------------------
008900110209
009000110209       // -?Parametri ricevuti?
009100110209     d P_ubLeg3C_SKC   s           3200
009200100201
009300100201       // -?Flags booleani?
009400100201     d $Fine           s               n   inz
009500120111     d $SerieCompl     s               n   inz
009600100201
009700100201       // -?Stringa SQL da eseguire?
009800100201     d wrkGetLista     s           4096    varying  inz
009900100201
010000100201       // -?Campi di comodo?
010100100201     d w_Dal           s              8s 0 inz
010200100201     d w_Al            s              8s 0 inz
010300100201     d SAVnrs          s                   like(wNrs)  inz
010400100201
010500100201       //--------------------------------------------------------------
010600100201       //?Definizione prototipi procedure.                             ?
010700100201       //--------------------------------------------------------------
010800100201
010900100201       // -?Reperimento dati utente?
011000100201      /copy gaitrasrc/srcProtoPR,TIBS34R
011100110208
011200110208       // -?Parametri per reperimento legami in tab. "3C"?
011300110208      /copy gaitrasrc/srcProtoPI,UBLEG3C
011400110208     ***  // -?Reperimento legami in tab. "3C"?
011500110208     *** /copy gaitrasrc/srcProtoPR,UBLEG3C
011600100201
011700100201       //--------------------------------------------------------------
011800100201       //?Definizione key-list.                                        ?
011900100201       //--------------------------------------------------------------
012000100201
012100100201
012200100201       //--------------------------------------------------------------
012300100201       //?M A I N - L I N E                                            ?
012400100201       //--------------------------------------------------------------
012500100201
012600100201     c     *Entry        plist
012700100201     c                   parm                    KPJBA
012800110209     c                   parm                    p_ubLeg3C_SKC
012900100201
013000100201      /free
013100100201
013200100201       // -?Operazioni iniziali?
013300100201       exsr  sr_RoutInz;
013400100201
013500100201       // -?Elaborazione SQL?
013600100201       doW  Not $Fine;
013700100201         exsr  sr_Elab;
013800100201       enddo;
013900051228
014000100201       // -?Operazioni finali?
014100100201       exsr  sr_RoutEnd;
014200100201
014300100201       //--------------------------------------------------------------
014400100201       //?Operazioni iniziali.                                         ?
014500100201       //--------------------------------------------------------------
014600100201       BEGSR  sr_RoutInz;
014700100201
014800100201         exec sql   set option dynusrprf = *owner, closqlcsr = *endmod;
014900100201         *inLR = *on;
015000100201
015100100201         TNTB54ds = KPJBU;
015200110209
015300110209         if  %parms() > 1;
015400110209           ubLeg3C_skC = P_ubLeg3C_skC;
015500110209         endif;
015600100201
015700100201         // -?Pulizia parametri di output?
015800100201         clear  O54nrs;
015900100201         clear  O54dal;
016000100201         clear  O54al;
016100100201         clear  O54err;
016200100201         clear  O54prz;
016300100201         clear  O54ksc;
016400100201
016500100201         // -?Reperimento dati job?
016600100201         exsr  sr_DatiJob;
016700120113
016800120113         // -?Controllo parameetri ricevuti?
016900120113         if  I54nrs = c_NrsERR;
017000120113           O54err = '9';
017100120113           $Fine = *on;
017200120113           leavesr;
017300120113         endif;
017400120110
017500120110         // -?Compilazione stringa SQL?
017600120110         wrkGetLista = 'with +
017700120110
017800120130         // -?-?selezione da tab. "3CP"?
017900120110                        Tab_3CP as +
018000120110                        (select TBEcod as SQL_cod, TBEatb, +
018100120110                                substr(TBEke1, 1, 7) as SQL_ksc, +
018200120110                                substr(TBEke2, 1, 9) as SQL_ssi, +
018300120110                                TBEuni +
018400120110                           from TNTBE00F +
018500120110                          where TBEcod = ''3CP'' +
018600120113                            and TBEatb = '' '' +
018700120110         // -?- - filiale segnacollo?
018800120110                            and substr(TBEke1, 1, 3) = ''' +
018900120110                                I54fil + '''';
019000120110         // -?- - serie segnacollo?
019100120110         if  I54nrs > *zero;
019200120110           wrkGetLista += ' and substr(TBEke2, 1, 2) = ''' +
019300120110                                I54nrs + '''';
019400120110         endif;
019500120110         wrkGetLista += '), +
019600120110
019700120130         // -?-?selezione da tab. "3EW"?
019800120110                        Tab_3EW as +
019900120110                        (select TBEcod as SQL_cod, TBEatb, +
020000120110                                substr(TBEke1, 2, 7) as SQL_ksc, +
020100120110                                substr(TBEuni, 7, 9) as SQL_ssi, +
020200120110                                TBEuni +
020300120110                           from TNTBE00F +
020400120110                          where TBEcod = ''3EW'' +
020500120113                            and TBEatb = '' '' +
020600120110         // -?- - filiale segnacollo?
020700120110                            and substr(TBEuni, 4, 3) = ''' +
020800120110                                I54fil + '''';
020900120110         // -?- - serie segnacollo?
021000120110         if  I54nrs > *zero;
021100120110           wrkGetLista += ' and substr(TBEuni, 7, 2) = ''' +
021200120110                                I54nrs + '''';
021300120110         endif;
021400120110         wrkGetLista += '), +
021500120130
021600120130         // -?-?selezione da tab. "3CL" (senza range segnacollo)?
021700120130                        Tab_3CL as +
021800120130                        (select TBEcod as SQL_cod, TBEatb, +
021900120210                                (substr(TBEke1, 1, 3) concat ''0000'') +
022000120131                                   as SQL_ksc, +
022100120130                                (substr(TBEke2, 1, 2) concat ''0000001'') +
022200120130                                   as SQL_ssi, ''9999999'' +
022300120130                           from TNTBE00F +
022400120130                          where TBEcod = ''3CL'' +
022500120130                            and TBEatb = '' '' +
022600120130         // -?- - filiale segnacollo?
022700120130                            and substr(TBEke1, 1, 3) = ''' +
022800120130                                I54fil + '''';
022900120130         // -?- - serie segnacollo?
023000120130         if  I54nrs > *zero;
023100120130           wrkGetLista += ' and substr(TBEke2, 1, 2) = ''' +
023200120130                                I54nrs + '''';
023300120130         endif;
023400120131         wrkGetLista += '), +
023500120110
023600120130         // -?-?selezione da tab. "3C" (senza range segnacollo)?
023700120110                         Tab_3C as +
023800120110                        (select TBLcod as SQL_cod, TBLflg, +
023900120110                                substr(TBLkey, 1, 7) as SQL_ksc, +
024000120110                                (substr(TBLuni, 1, 2) concat ''0000001'') +
024100120110                                   as SQL_ssi, ''9999999'' +
024200120131
024300120110                           from TABEL00F exception join Tab_3CP +
024400120110                             on TBLcod = ''3C'' +
024500120110                            and TBLkey = Tab_3CP.SQL_ksc +
024600120110                            and substr(TBLuni, 1, 2) = +
024700120110                                substr(Tab_3CP.SQL_ssi, 1, 2) +
024800120131
024900120110                                         exception join Tab_3EW +
025000120110                             on TBLcod = ''3C'' +
025100120110                            and TBLkey = Tab_3EW.SQL_ksc +
025200120110                            and substr(TBLuni, 1, 2) = +
025300120110                                substr(Tab_3EW.SQL_ssi, 1, 2) +
025400120131
025500120131                                         exception join Tab_3CL +
025600120131                             on TBLcod = ''3C'' +
025700120210                            and substr(TBLuni, 36, 3) = +
025800120131                                substr(Tab_3CL.SQL_ksc, 1, 3) +
025900120131                            and substr(TBLuni, 1, 2) = +
026000120131                                substr(Tab_3CL.SQL_ssi, 1, 2) +
026100120131
026200120113                          where TBLcod = ''3C'' +
026300120113                            and TBLflg = '' '' +
026400120110         // -?- - filiale segnacollo?
026500120113                            and substr(TBLuni, 36, 3) = ''' +
026600120110                                I54fil + '''';
026700120110         // -?- - serie segnacollo?
026800120110         if  I54nrs > *zero;
026900120110           wrkGetLista += ' and substr(TBLuni, 1, 2) = ''' +
027000120110                                I54nrs + '''';
027100120110         endif;
027200120110         wrkGetLista += ') +
027300120110
027400120110         // -?- estrazione dati
027500120110                         select * from Tab_3CP +
027600120110                          union +
027700120110                         select * from Tab_3EW +
027800120130                          union +
027900120130                         select * from Tab_3CL +
028000120110                          union +
028100120110                         select * from Tab_3C +
028200120110
028300120110         // -?- impostazioni finali?
028400120110                          order by SQL_ssi, SQL_cod +
028500100202                            for read only';
028600120131
028700120131         // -?Valorizzazione schiera *Temp x *Test?
028800120131         //  ?(per visualizzarla un debug)?
028900120220         //ds_Temp = wrkGetLista;
029000100201
029100100201         // -?Start SQL?
029200100201         exec sql   prepare S1   from :wrkGetLista;
029300100201         exec sql   declare C1   cursor for S1;
029400100201         exec sql   open C1;
029500100201
029600100201       ENDSR;
029700100201
029800100201       //--------------------------------------------------------------
029900100201       //?Reperimento Dati del job (Utente/Operativi).                 ?
030000100201       //--------------------------------------------------------------
030100100201       BEGSR  sr_DatiJob;
030200100201
030300100201         in(e) §AzUte;
030400100201         if NOT %error;
030500100201           in(e) §DatiUte;
030600100201         endif;
030700100201         if %error or RSut = *blank;
030800100201           tibs34r ( tibs34ds );
030900100201           in §AzUte;
031000100201           in §DatiUte;
031100100201         endif;
031200100201
031300100201       ENDSR;
031400100201
031500100201       //--------------------------------------------------------------
031600100201       //?Elaborazione singola estrazione SQL.                         ?
031700100201       //--------------------------------------------------------------
031800100201       BEGSR  sr_Elab;
031900100201
032000100201         exec sql   fetch next   from C1   into :ds_TAB;
032100100201
032200100201         select;
032300100201           when  SQLcode = 100;
032400100201             $Fine = *on;
032500100201             leavesr;
032600100201           when  SQLcode < *zero;
032700100201             dump(a);
032800100201             O54err = 'S';
032900100201             $Fine  = *on;
033000100201             leavesr;
033100100201           other;
033200100201             exsr  sr_ElabRec;
033300100201         endsl;
033400100201
033500100201       ENDSR;
033600100201
033700100201       //--------------------------------------------------------------
033800100201       //?Elaborazione singolo record estratto via SQL.                ?
033900100201       //--------------------------------------------------------------
034000100201       BEGSR  sr_ElabRec;
034100120111
034200120111         if  I54tel = 'C'   or   I54nrs > '00';
034300120111           O54prz = 'P';
034400120111         endif;
034500100201
034600120111         select;
034700120111
034800120111           // -?SE richiesto CONTROLLO:?
034900120111           //  ?si scartano i record annullati o del cliente ricevuto?
035000120111           when  I54tel = 'C'   and  ( SQL_atb = 'A'    or
035100120220                                       I54ksc  = %trimr(SQL_ke1)
035200120220                                and  SQL_tab <> '3CL' );
035300120111             leavesr;
035400120111
035500120111           // -?SE richiesto RICERCA:?
035600120111           //  ?inutile elaborare i record con range di una serie per?
035700120111           //  ?la quale è già stato rilevato un range totale?
035800120111           when  I54tel = 'R'   and  %subst( SQL_ssi : 1 : 2 ) =
035900120111                                     %editc( SAVnrs : 'X' )
036000120111                                and  $SerieCompl;
036100120111             leavesr;
036200120111
036300120111         endsl;
036400100201
036500100201         // -?Impostazione dati letti?
036600100201         clear  ds_Nrs_Nsi;
036700100201         ds_Nrs_Nsi = SQL_ssi;
036800100201         clear  d3CP;
036900100201         clear  d3EW;
037000100201         select;
037100120110           when  SQL_tab = '3C';
037200120130           when  SQL_tab = '3CL';
037300100201           when  SQL_tab = '3CP';
037400100201             d3CP = SQL_uni;
037500100201           when  SQL_tab = '3EW';
037600100201             d3EW = SQL_uni;
037700100201         endsl;
037800100201
037900100201         // -?Rottura numero serie?
038000100201         if  wNrs <> SAVnrs;
038100120111           reset  $SerieCompl;
038200100201           if  SAVnrs > *zero;
038300100201             if  I54tel = 'R'    and
038400100201                 w_Dal > *zero   and   w_Dal < c_NscMAX   and
038500100201                 w_Al  = *zero;
038600100201               w_Al  = w_Dal + (I54tot - 1);
038700100201               if  w_Al <= c_NscMAX;
038800100201                 O54nrs = SAVnrs;
038900100201                 O54dal = w_Dal;
039000100201                 O54al  = w_Al;
039100100201                 $Fine  = *on;
039200100201                 leavesr;
039300100201               endif;
039400100201             endif;
039500100201             clear  w_Dal;
039600100201             clear  w_Al;
039700100201           endif;
039800100201           SAVnrs = wNrs;
039900100201         endif;
040000120111
040100120111         if  SQL_tab = '3C'   or
040200120130             SQL_tab = '3CL'  or
040300120111            (SQL_tab = '3CP'  and  wNsi = 1  and  §3CPal  = *hival)  or
040400120111            (SQL_tab = '3EW'  and  wNsi = 1  and  §3EWnsf = *hival);
040500120111           $SerieCompl = *on;
040600120111         endif;
040700100201
040800100201         // -OPERAZIONI PER TIPO ELABORAZIONE "RICERCA"?
040900100201         IF  I54tel = 'R';
041000100201
041100100201           select;
041200100201
041300120111             // -?Serie già assegnata come completa?
041400120111             //  ?(inutile elaborare questo record)?
041500120111             when  $SerieCompl;
041600120111               leavesr;
041700120113
041800120113             // -?Serie 99: NON gestibile?
041900120113             when  wNRS = *hival;
042000120113               $Fine  = *on;
042100120113               leavesr;
042200120111
042300100201             // -?Ricerca del segnacollo iniziale?
042400100201             //  ?trovato subito il range (iniziale) libero?
042500100201             when  w_Dal = *zero   and
042600100616                   wNSI  > 1       and   wNSI  > I54tot;
042700100201               O54dal = 1;
042800100201               O54al  = I54tot;
042900100201               O54nrs = wNrs;
043000100201               $Fine  = *on;
043100100201               leavesr;
043200100201
043300100201             // -?Ricerca del segnacollo iniziale:?
043400100201             //  ?assume come nuovo numero-dal il numero-al + 1?
043500100201             //  ?e prosegue nella lettura?
043600100616             when  w_Dal   = *zero   and
043700100201                 ((SQL_tab = '3CP'   and   §3CPal  < c_NscMAX)   OR
043800100201                  (SQL_tab = '3EW'   and   §3EWnsf < c_NscMAX));
043900100201               select;
044000100201                 when  SQL_tab = '3CP';
044100100201                   w_Dal = §3CPal + 1;
044200100201                 when  SQL_tab = '3EW';
044300100201                   w_Dal = §3EWnsf + 1;
044400100201               endsl;
044500100201
044600100201             // -?Ricerca del segnacollo finale?
044700100201             when  w_Al = *zero;
044800100201               w_Al = (wNSI - 1);
044900100616               if  (w_Al - w_Dal + 1) >= I54tot;
045000100201                 O54dal = w_Dal;
045100100201                 O54al  = w_Dal + (I54tot - 1);
045200100201                 O54nrs = wNrs;
045300100201                 $Fine  = *on;
045400100201                 leavesr;
045500100201               else;
045600100201                 // -?Range insufficiente:?
045700100201                 //  ?assume come nuovo numero-dal il numero-al + 1?
045800110221                 //  ?(SE RANGE NON GIÀ SOVRAPPOSTO)?
045900100201                 //  ?e prosegue nella lettura?
046000100201                 select;
046100100201                   when  SQL_tab = '3CP';
046200110221                     if  w_Dal <= §3CPal;
046300110221                       w_Dal = §3CPal + 1;
046400110221                     endif;
046500100201                   when  SQL_tab = '3EW';
046600110221                     if  w_Dal <= §3EWnsf;
046700110221                       w_Dal = §3EWnsf + 1;
046800110221                     endif;
046900100201                 endsl;
047000100201                 clear w_Al;
047100100201               endif;
047200100201           endsl;
047300100201
047400100201         // -OPERAZIONI PER TIPO ELABORAZIONE "CONTROLLO"?
047500100201         ELSE;
047600100201
047700100201           select;
047800100201
047900120113             // -?Range parziale valido?
048000100201             when  I54dal < wNSI   and   I54al < wNSI;
048100100201               $Fine = *on;
048200100201               leavesr;
048300100201
048400100201             // -?Range superiore a quello in input?
048500100201             //  ?(passaggio alla lettura successiva)?
048600100201             when  (SQL_tab = '3CP'    and
048700100201                    I54dal  > §3CPal   and   §3CPal  < c_NscMAX)   OR
048800100201                   (SQL_tab = '3EW'    and
048900100201                    I54dal  > §3EWnsf  and   §3EWnsf < c_NscMAX);
049000100201               leavesr;
049100100201
049200110208             // -?Range già in uso (da cliente con stesso padre)?
049300110208             when  (SQL_tab = '3CP'    and
049400110209                    %lookup( %int( %trimr( SQL_ke1 ) )
049500110208                             : sch_SKC ) > *zero)  OR
049600110208                   (SQL_tab = '3EW'    and
049700110209                    %lookup( %int( %trimr( %subst( SQL_ke1 : 2 ) ) )
049800110208                             : sch_SKC ) > *zero);
049900120130               leavesr;
050000120130
050100120130             // -?Serie già in uso (ma inserita in tab. "3CL")?
050200120131             //  ?Memorizza i dati di output MA non esce dal *pgm?
050300120131             //  ?($Fine = *on) perchè ci potrebbero essere altre vere?
050400120131             //  ?sovrapposizioni (in altre tabelle).?
050500120130             when  SQL_tab = '3CL';
050600120130               O54err = 'A';
050700120130               O54ksc = *all'9';
050800120130               O54dal = wNSI;
050900120130               O54al  = *hival;
051000120130               clear  O54prz;
051100110208
051200100210             // -?Range già in uso (da altro cliente/padre)?
051300100201             other;
051400100201               O54err = '1';
051500100201               O54ksc = SQL_ke1;
051600100201               O54dal = wNSI;
051700100201               select;
051800120110                 when  SQL_tab = '3C ';
051900120110                   O54al  = *hival;
052000120110                   clear  O54prz;
052100100201                 when  SQL_tab = '3CP';
052200100201                   O54al  = §3CPal;
052300100201                 when  SQL_tab = '3EW';
052400100201                   O54al  = §3EWnsf;
052500100201               endsl;
052600120220               O54nrs = wNRS;
052700120220               if  %subst(I54ksc : 4) > *zero   or
052800120220                  (%subst(I54ksc : 4) = *zero   and
052900120220                   (O54dal > 1   or   O54al < *hival));
053000120220                 $Fine  = *on;
053100120220                 leavesr;
053200120220               endif;
053300100201
053400100201           endsl;
053500100201
053600100201         ENDIF;
053700100201
053800100201       ENDSR;
053900100201
054000100201       //--------------------------------------------------------------
054100100201       //?Operazioni finali.                                           ?
054200100201       //--------------------------------------------------------------
054300100201       BEGSR  sr_RoutEnd;
054400100201
054500100201         // -?End SQL?
054600100201         exec SQL   close C1;
054700100201
054800120131         Select;
054900120131
055000120131           // -?Re-Impostazione flag "range parziale" se reperito in?
055100120131           //  ?tab. "3CL" (potrebbe aver elaborato altri rec. da altre?
055200120131           //  ?tabelle)?
055300120131           When  O54err = 'A';
055400120131             clear  O54prz;
055500120131
055600120131           // -?Impostazione parametri in uscita (se NO *err)?
055700120131           When  O54err = *blank;
055800120131             if  I54tel = 'R'    and   O54dal = *zero   and
055900120131                 w_Dal  > *zero  and   w_Al   = *zero   and
056000120131                 w_Dal  < c_NscMAX;
056100120131               w_Al = ( w_Dal + (I54tot - 1) );
056200120131               if  w_Al <= c_NscMAX;
056300120131                 O54al  = w_Al;
056400120131                 O54nrs = wNrs;
056500120131                 O54dal = w_Dal;
056600120131               endif;
056700120131             endif;
056800120131             // -?SE richiesta ricerca per una particolare serie e il?
056900120131             //  ?*pgm NON ha trovato nessun range assegnabile (tutto?
057000120131             //  ?già impegnato): restituisce errore al chiamante?
057100120131             if  I54tel = 'R'   and   O54dal = *zero   and
057200120131                 O54prz = 'P';
057300120131               O54err = '1';
057400120131             endif;
057500100201
057600120131         EndSl;
057700100201
057800100201         // -?Uscita?
057900100201         KPJBU = TNTB54ds;
058000100201         *inLR = *on;
058100100201         return;
058200100201
058300100201       ENDSR;

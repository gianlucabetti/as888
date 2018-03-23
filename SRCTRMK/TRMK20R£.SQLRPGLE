000100040107      *---------------------------------------------------------------*
000200090320      * Gestione note clienti/potenziali/trattative commerciali       *
000300040107      *---------------------------------------------------------------*
000400040107
000500040108     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600090528     h dftactgrp(*no) actgrp(*caller)
000700040107
000800040107      *---------------------------------------------------------------*
000900040107
001000100224     fticpn00f  uf A e             disk    commit(flgcmt) usropn
001100100224
001200090408     fcnaco00f  if   e           k disk
001300090409     ftfaco00f  if   e           k disk    rename(cnaco000:tfaco000)
001400100526     f                                     usropn
001500090417     fazorg01l  if   e           k disk
001600090417     ftncpo01l  if   e           k disk
001700100224     fticpn03l  if   e           k disk    rename(ticpn000:ticpn03) prefix(D_)
001800100224     f                                     infds(cpnds)
001900040112      *
002000090526     ftrmk20d   cf   e             workstn sfile(mk20S01:S01nrr)
002100040112     f                                     infds(dspINFOds)
002200090527     f                                     usropn
002300090615     ftrmk20d4  cf   e             workstn sfile(mk20S41:S01nrr4)
002400090616     f                                     infds(dsp4DS)
002500090615     f                                     usropn
002600040107      *---------------------------------------------------------------*
002700040107
002800040107      *
002900040107      *   C O S T A N T I   - - - - - - - - - - - - - - - - - - - - - *
003000040107      *
003100040108     d K_tit1          c                   const('  IMMISSIONE  ')
003200040108     d K_tit2          c                   const(' MANUTENZIONE ')
003300040108     d K_tit3          c                   const('INTERROGAZIONE')
003400040109      * - nr. riga del 1° rec. del sfl
003500040109     d K_SflNrr1       c                   const(6)
003600090527     d K_SflNrr14      c                   const(7)
003700040120      * - tasto "Invio"
003800040120     d K_Enter         c                   const(X'F1')
003900100121      * - DspAtr "HI"
004000100121     d hi              C                   CONST(X'22')
004100040107      *
004200040107      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
004300040107      *
004400040107      *
004500040107      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
004600040107      *
004700040107     d KPJBA         e ds
004800040108      * - Parametri
004900090323     d trmk20ds      e ds
005000090323     d Save20ds      e ds                  extname(trmk20ds) inz
005100040213     d                                     prefix(X:1)
005200040108      *
005300040108      * - Parametri x Controllo profilo utenti
005400090414     d TIBS34DS      e ds                  inz
005500090402     d §azute        e ds                  extname(azute00f)
005600090402     d                                     dtaara
005700090402     d §datiute      e ds                  extname(ddatiute)
005800090402     d                                     dtaara
005900040123      *
006000090403     d ticpnds       e ds                  extname(TICPN00F) inz
006100040130      * - Salvataggio dati dei 1' 4 records immessi o modificati
006200090323     d SAVrec1       e ds                  extname(TICPN00F) inz
006300040130     d                                     prefix(SR1:3)
006400090323     d SAVrec2       e ds                  extname(TICPN00F) inz
006500040130     d                                     prefix(SR2:3)
006600090611     d*SAVrec3       e ds                  extname(TICPN00F) inz
006700090611     d*                                    prefix(SR3:3)
006800090611     d*SAVrec4       e ds                  extname(TICPN00F) inz
006900090611     d*                                    prefix(SR4:3)
007000100121
007100100121     d dtta          e ds
007200100121     d TIBS02ds      e ds                  inz
007300100121
007400090611     d                 ds
007500090611     d nota90                        90
007600090611     d nota                          30    dim(3) overlay(nota90)
007700040109      *
007800040109     d Status         sds
007900040109     d  SDSpgm           *proc
008000100224
008100100224     D cpnds           ds
008200100224     D  cpnnrr               397    400b 0
008300100224
008400040107      *
008500090616     d dspINFOds       ds           512
008600040120     d  di_TASTO             369    369
008700090416     d  di_NrRg              370    370
008800090416     d  di_NrCl              371    371
008900040120     d  di_SFLnrr            378    379B 0
009000090616      *
009100090616     d dsp4ds          ds                  likeds(dspinfods)
009200090409
009300090409     d Cursor          ds
009400090409     d  RIRI                   1      2B 0
009500090409     d  R$$                    2      2
009600090409     d  COCO                   3      4B 0
009700090409     D  C$$                    4      4
009800090416
009900090416     d Cur_R           s              2  0 inz
010000100614     d Cur_C           s              3  0 inz
010100040109      *
010200040107      *
010300040107      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
010400040107      *
010500090409     D WrkStringaSql   S           4500
010600090409     D                                     VARYING
010700040109      * - Campi di appoggio per il subfile
010800040123     d S01nrr          s                   like(V1Cnrr)   inz
010900090527     d S01nrr4         s                   like(V1Cnrr)   inz
011000090511     d wNrr1Free       s                   like(V1Cnrr)   inz
011100090511     d wNrr1new        s                   like(V1Cnrr)   inz(1)
011200040213     d $Nrr            s                   like(V1Cpos)   inz
011300090415     d $Nrr_agg        s                   like(V1Cpos)   inz
011400040213     d wNrr            s                   like(V1Cpos)   inz
011500040213     d wPag            s                   like(V1Cpos)   inz
011600040213     d wRig            s                   like(V1Cpos)   inz
011700090528      * - nr. di righe del sfl (SFLPAG)
011800090528     d K_SflPag        s              2s 0
011900040109      *
012000040109      * - Campi di salvataggio temporaneo dati
012100090409     d SAVpno          s                   like(cpnpno)   inz
012200090409     d SAVflm          s                   like(Imk20flm)   inz
012300090409     d SAVcpo          s                   like(cpncpo)   inz
012400090409     d SAVksc          s                   like(cpnksc)   inz
012500090409     d SAVdim          s                   like(cpndim)   inz
012600090409     d SAVhim          s                   like(cpnhim)   inz
012700090518     d SAVtat          s                   like(cpntat)   inz
012800040109      *
012900040109      * - Flags booleani
013000040108     d $Fine           s              1    inz(*off)
013100090421     d $Finef10        s              1    inz(*off)
013200090409     d $Finerec        s              1    inz(*off)
013300040108     d $InzS01         s              1    inz(*on)
013400040130     d $SavRec         s              1    inz(*on)
013500040213     d $PiuRec         s              1    inz(*off)
013600090615     d $no_aggio       s              1    inz(*off)
013700100929     d $FineSfl        s              1    inz(*off)
013800040213      *
013900040213      * - Indici di schiera / Contatori
014000040213     d xx              s              3  0 inz
014100090409     d $contrec        s              3  0 inz
014200090611     d ctn             s              2  0 inz
014300090615     d clt             s              2  0 inz
014400090612     d ps              s              2  0 inz
014500040109      *
014600040109      * - Campi di comodo
014700040108     d w0140           s             14  0
014800040123     d wDataAMG        s              8  0
014900040123     d wDataGMA        s              8  0
015000040120     d wOra            s              6  0
015100090414     d dataiso_cor     s               d   datfmt(*iso)
015200090414     d dataeur_cor     s               d   datfmt(*eur)
015300090416     d dataeur         s               d   datfmt(*eur)
015400090409     d datadmy         s               d   datfmt(*dmy)
015500090408     d kut             s                   like(acokut) inz(1)
015600090408     d w_cpo           s                   like(acolib)
015700090420     d S01nrr_blu      s                   like(s01nrr)   inz
015800090421     d w_mk20flm       s                   like(imk20flm)   inz
015900081023      *
016000081023      * -Campi per il controllo di caratteri non validi
016100081023     d TxtInOut        S           2048
016200081023     d ElencoChar      S            256
016300081023     d TipoElenco      S              1
016400081023     d CharSost        S              1
016500081023     d UpperCase       S              1
016600081023     d ChkNull         S              1    Inz('1')
016700081023     d CharNull        S              1
016800081023     d Esito           S              1
016900090526
017000090528     d dsp_mod         s             10I 0
017100090612     d w20nota         s                   like(emk20no1)
017200081023
017300090402      // ? PROTOTIPI ?
017400100121      /copy gaitrasrc/srcprotopr,tibs02r
017500090402      /copy gaitrasrc/srcprotopr,tibs34r
017600090416      /copy gaitrasrc/srcprotopr,xchkchar
017700090526      /copy gaitrasrc/srcprotopr,QsnQryModS
017800040107
017900040107      *===============================================================*
018000040107      *  RIEPILOGO INDICATORI                                         *
018100040107      *---------------------------------------------------------------*
018200040109      *  01    - Flag modalità DIVERSO da *blanks e da "V"            *
018300040224      *          (né "manutenzione" né "variazione")                  *
018400100121      *  03    - Cliente
018500100121      *  04    - Potenziale
018600040224      *  05    - Flag modalità UGUALE  a  "I" ("interrogazione")      *
018700040224      *  09    - Abilita RolUp nel subfile                            *
018800040109      *  20    - SFLDSP    in C01                                     *
018900040109      * N21    - SFLCLR    in C01                                     *
019000040109      *  21    - SFLDSPCTL in C01                                     *
019100040109      *  23    - SFLEND    in C01                                     *
019200040109      *  25    - Roll Up da subfile                                   *
019300040109      *  28    - Errore generico dspf                                 *
019400040428      *  34    - NON visualizzazione data/ora immissione e stato note *
019500040109      *  35    - Protezione campo note                                *
019600040224      *  36    - Evidenzia note non lette                             *
019700040224      *  41    - Posizionamento cursore su campo note                 *
019800040109      *  90    - Errore                                               *
019900040107      *===============================================================*
020000040107
020100040107     c     *Entry        plist
020200040107     c                   parm                    KPJBA
020300090323     c                   parm                    trmk20ds
020400040123      *
020500040123      * Pulizia campi di output
020600090331     c                   clear                   Omk20piu
020700090331     c                   clear                   Omk20fxx
020800090331     c                   clear                   Omk20err
020900090331     c                   clear                   Omk20msg
021000090331      /free
021100090403         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
021200090610
021300090612       if not %open(ticpn00f);
021400090612          if imk20cmt='1';
021500090612             flgcmt=*on;
021600090612          else;
021700090612             flgcmt=*off;
021800090612          endif;
021900090612          open ticpn00f;
022000090612       endif;
022100100526
022200100526       if  NOT  %open(TFACO00F)   and   %subst(KNSIF : 1 : 3) = 'FIL';
022300100526         open  TFACO00F;
022400100526       endif;
022500090421
022600090421       w_mk20flm=imk20flm;
022700090421
022800090409       //TIPO LANCIO, I65TLA "C" -           CHIUSO CON LR
022900090409       //TIPO LANCIO, I65TLA "L" - ELABORO E CHIUDO CON LR
023000090409       //TIPO LANCIO, I65TLA " " - ELABORO E CHIUDO IN RETRN
023100090331       select;
023200090331          when imk20tla='C';
023300090331             eval *inlr=*on;
023400090402          other;
023500090409       //Reperisco data corrente
023600090423             dataiso_cor=(%date());
023700090423             dataeur_cor=dataiso_cor;
023800090423             wora=%dec(%time());
023900090402             select;
024000100224       //Delete
024100100224                when imk20flm='D';
024200100224                   exsr sr_delete ;
024300090409       //Recupero
024400090402                when imk20flm='R';
024500090402                   exsr sr_recup;
024600090409       //Conferma dati immessi precedentemente nel subfile
024700090402                when imk20flm='C';
024800090402                   exsr sr_updTICPN;
024900090402                other;
025000090409       //Manutenzione dati a video
025100090402                   reset $fine;
025200090615                   if not %open(trmk20d) and not %open(trmk20d4);
025300090528                     dsp_mod = QueryDisplayModeSupport('4':*omit :*omit :*omit);
025400090528                     if dsp_mod=0;
025500090528                        k_sflpag=17;
025600090615                        open trmk20d;
025700090528                     else;
025800090616                        k_sflpag=20;
025900090615                        open trmk20d4;
026000090528                     endif;
026100090525                   endif;
026200090402                   dow $fine=*off;
026300090402                      exsr sr_gess01;
026400090402                   enddo;
026500090409       //Restituisco i dati elaborati al chiamante
026600090402                   if $piurec=*on;
026700090402                      omk20piu='+';
026800090402                   endif;
026900090402                   if omk20fxx=*blanks and imk20flm<>' ' and imk20flm<>'M';
027000090402                      emk20no1=SR1not;
027100090402                      emk20no2=SR2not;
027200090611       //             emk20no3=SR3not;
027300090611       //             emk20no4=SR4not;
027400090402                      emk20dim=SR1dim;
027500090402                      emk20him=SR1him;
027600090402                      emk20pru=SR1pru;
027700090402                      emk20pos=SR1pos;
027800090402                      emk20sns=SR1sns;
027900090402                   endif;
028000090402             endsl;
028100090331       endsl;
028200090331
028300090331       select;
028400090331          when imk20tla='L';
028500090331             eval *inlr=*on;
028600090331          when imk20tla=' ';
028700090331             eval *inrt=*on;
028800090331       endsl;
028900040108
029000090402       //-------------------------------------------------------------*
029100090402       //Routine Iniziale                                             *
029200090402       //-------------------------------------------------------------*
029300090414       begsr *inzsr;
029400090402
029500090402       //Reperisco dati job
029600090402       exsr datijob;
029700090402       vtcpgm=SDSpgm;
029800090402
029900090402       EndSr;
030000040108
030100090402       //-------------------------------------------------------------*
030200090402       //Reperimento Dati del job (Utente/Operativi)                  *
030300090402       //-------------------------------------------------------------*
030400090402       Begsr DatiJob;
030500090403
030600090402          in(e) §azute;
030700090402          if not %error;
030800090402           in(e) §datiute;
030900090402          endif;
031000090402          if %error or rsut = *blanks;
031100090402           tibs34r(tibs34ds);
031200090402           in §azute;
031300090402           in §datiute;
031400090402          endif;
031500090402       endsr;
031600100224       //-------------------------------------------------------------*
031700100224       //Deleto le note che mi ha chiesto  il chiamante               *
031800100224       //-------------------------------------------------------------*
031900100224       Begsr SR_delete ;
032000100224
032100100224         cpncpo = imk20cpo ;
032200100224         cpntat = imk20tat ;
032300100224         cpnatn = imk20atn ;
032400100224         cpnatnp= 0 ;
032500100224         setll (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
032600100224         reade (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
032700100224         dow not %eof(ticpn03l);
032800100224             chain cpnnrr ticpn00f ;
032900100224             if %found(ticpn00f) ;
033000100224                delete ticpn00f;
033100100224             endif ;
033200100224             reade (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
033300100224         enddo ;
033400100224
033500100224       endsr;
033600100224
033700090403       //-------------------------------------------------------------*
033800090403       //Recupero note da restituire al chiamante                     *
033900090403       //-------------------------------------------------------------*
034000090403       Begsr SR_recup;
034100090403       //Il recupero delle note lo gestisco solo recuperando per chiave
034200090403       //completa (utilizzo cioè tutti i campi ricevuti dal chiamante e
034300090403       //non faccio estrazioni con chiavi parziali)
034400090406       $finerec=*off;
034500090406       $contrec=1;
034600090403       exsr sr_prepSqlRec;
034700090403       exec sql prepare STRINGASQL from :WrkStringaSql;
034800090403       exec sql declare NOTEcsr cursor for StringaSql;
034900090403       exec sql open NOTEcsr;
035000090414       dow $finerec=*off;
035100090406       exec sql Fetch NOTEcsr into :ticpnds;
035200090406          if sqlcod=100 or sqlcod<0;
035300090406             $finerec = *on;
035400090415             if sqlcod<0;
035500090415                omk20err='E';
035600090415             endif;
035700090406             leave;
035800090406          endif;
035900090406          select;
036000090406          when $contrec=1;
036100090406             emk20no1=cpnnot;
036200090414             emk20dim=cpndim;
036300090414             emk20him=cpnhim;
036400090414             emk20pru=cpnpru;
036500090414             emk20pos=cpnpos;
036600090406          when $contrec=2;
036700090406             emk20no2=cpnnot;
036800090610       // when $contrec=3;
036900090610       //    emk20no3=cpnnot;
037000090610       // when $contrec=4;
037100090610       //    emk20no4=cpnnot;
037200090406          other;
037300090406             omk20piu='+';
037400090406             $finerec=*on;
037500090406          endsl;
037600090406          $contrec=$contrec+1;
037700090406       enddo;
037800090406
037900090403       endsr;
038000040108
038100090403       //-------------------------------------------------------------*
038200090403       //Preparazione stringa sql per recupero note                   *
038300090403       //-------------------------------------------------------------*
038400090403       Begsr SR_prepSqlRec;
038500090403
038600090403       WrkStringaSql='Select * from ticpn00f where';
038700090403
038800090403       if imk20cpo>0;
038900090922          WrkStringaSql=WrkStringaSql+' cpncpo=' + %editc(imk20cpo:'X');
039000090403       endif;
039100090403       if imk20ksc>0;
039200090922          WrkStringaSql=WrkStringaSql+' and cpnksc='+%editc(imk20ksc:'X');
039300090403       endif;
039400090518       if imk20atn>0;
039500100107          IF imk20cpo > 0;
039600090922          WrkStringaSql=WrkStringaSql+' and cpnatn='+%editc(imk20atn:'X');
039700090922          WrkStringaSql=WrkStringaSql+' and cpnatnp='+%editc(imk20atnp:'X');
039800100107          ELSE;
039900100107          WrkStringaSql=WrkStringaSql+' cpnatn=' +%editc(imk20atn:'X') +
040000100107          ' and cpnatnp='+%editc(imk20atnp:'X');
040100100107          ENDIF;
040200090403       endif;
040300090403       if imk20nrv>0;
040400090922          WrkStringaSql=WrkStringaSql+' and cpnnrv='+%editc(imk20nrv:'X');
040500090403       endif;
040600090403
040700110406       WrkStringaSql=WrkStringaSql+' Order by cpndim, cpnhim, cpnpno';
040800090403       WrkStringaSql=WrkStringaSql+' FOR READ ONLY';
040900090403
041000090403       endsr;
041100090407       //-------------------------------------------------------------*
041200090407       //Gestione dati a video                                        *
041300090407       //-------------------------------------------------------------*
041400090407       Begsr SR_Gess01;
041500090407
041600090407       // Controllo se devo inizializzare la videata
041700090407
041800090407       if imk20tla<>xmk20tla
041900090407       or imk20flm<>xmk20flm
042000090407       or imk20cpo<>xmk20cpo
042100090407       or imk20ksc<>xmk20ksc
042200090518       or imk20tat<>xmk20tat
042300090518       or imk20atn<>xmk20atn
042400090518       or imk20atnp<>xmk20atnp
042500090407       or imk20nrv<>xmk20nrv;
042600090407          reset $inzs01;
042700090407          save20ds=trmk20ds;
042800090407       endif;
042900090407
043000090407       // Inizializzo la videata
043100090407
043200090407       Select;
043300090407       when $inzs01=*on;
043400090407          exsr sr_inzs01;
043500090415          $inzs01=*off;
043600090407       when imk20flm=*blanks or imk20flm='M';
043700090407          exsr upds01;
043800090407       endsl;
043900090407
044000090407       if not *in90;
044100090528          if dsp_mod=1;
044200090528             write mk20t40;
044300090528             write mk20z41;
044400090528          else;
044500090528             write mk20t00;
044600090528             write mk20z01;
044700090528          endif;
044800090407       endif;
044900090407
045000090409       // Emetto messaggio in caso di assenza record
045100090528       if dsp_mod=0 and s01nrr=*zeros;
045200090407          write mk20d01;
045300090407       endif;
045400090528       if dsp_mod=1 and s01nrr4=*zeros;
045500090528          write mk20d41;
045600090528       endif;
045700090407
045800090604       // posiziono il cursore nel sfl
045900090604       if v1cpos>0;
046000090604          v1cnrr=v1cpos;
046100090604       else;
046200090604          if dsp_mod=0;
046300090604             v1cnrr=s01nrr;
046400090604          else;
046500090604             v1cnrr=s01nrr4;
046600090604          endif;
046700090604       endif;
046800090407
046900090407       // Emetto la videata
047000090528       if dsp_mod=0;
047100090528          exfmt mk20c01;
047200090528       else;
047300090528          exfmt mk20c41;
047400090528       endif;
047500090921
047600090407       *in28=*off;
047700090407       *in90=*off;
047800090407       clear v1cmsg;
047900090921
048000090407       select;
048100090407       // F3=Fine
048200090407          When *inkc;
048300090407             $fine=*on;
048400090407             $inzs01=*on;
048500090407             omk20fxx='03';
048600090407             leavesr;
048700090407       // F12=Ritorno
048800090414          When *inkl;
048900090407             $fine=*on;
049000090407             $inzs01=*on;
049100090407             omk20fxx='12';
049200090407             leavesr;
049300090416       // F1=Informazioni aggiuntive
049400090416          When *inka;
049500090416          exsr sr_f1;
049600090921       // F6=Conferma
049700090921          When *inkf;
049800090921          exsr sr_f6;
049900090421       // F10=Inserimento
050000090421          When *inkj;
050100090421             exsr sr_f10;
050200090421             if omk20fxx<>'12';
050300090421                $fine=*on;
050400090421                $inzs01=*on;
050500090421                leavesr;
050600090421             endif;
050700090407       // Rollup
050800090407          When *in25;
050900090407       //    Caricamento righe vuote
051000090407             if imk20flm<>'I';
051100090616                if dsp_mod=0;
051200090616                   v1cpos=di_sflnrr;
051300090616                else;
051400090616                   v1cpos=dsp4ds.di_sflnrr;
051500090616                endif;
051600090407                exsr sr_freerow;
051700090407             endif;
051800101007       //    Rollup recupero le note già scritte
051900101007             If Imk20flm = 'M' ;
052000101007                exsr sr_reps01;
052100101007             Endif ;
052200090407       endsl;
052300090407       // Enter=Prosegui (se manutenzione/immissione - Modalità " "/"M"
052400090616       if dsp_mod=1;
052500090616          di_tasto=dsp4ds.di_tasto;
052600090616       endif;
052700090407       if imk20flm= *blanks or imk20flm='M' and di_tasto=K_enter;
052800090407         exsr sr_reps01;
052900090921         if imk20flm <> ' ';
053000090407         $fine=*on;
053100090921         endif;
053200090407       endif;
053300090407       endsr;
053400090407       //-------------------------------------------------------------*
053500090407       //Inizializzazione subfile                                     *
053600090407       //-------------------------------------------------------------*
053700090407       Begsr SR_InzS01;
053800090921
053900090407       // Preparazione dati di testata
054000090407       select;
054100090921        when w_mk20flm='M' or w_mk20flm=' ';
054200090407          vtcmod=K_tit1;
054300090921        when w_mk20flm='I';
054400090407          vtcmod=K_tit3;
054500090407       endsl;
054600090921
054700100512       *in01=(W_mk20flm<>'V' and W_mk20flm<>' ');
054800090421       *in05=(w_mk20flm='I');
054900090423       *in08=(w_mk20flm='I' and imk20f10='S');
055000100121
055100100121       *in03 = *off;
055200100121       *in04 = *off;
055300090921
055400090407       if imk20cpo>0;
055500100121          *in04=*on;
055600090409          v1ccpo=imk20cpo;
055700090420          if imk20rsc<>*blanks;
055800090417             v1cdpo=imk20rsc;
055900090417          else;
056000090417             chain imk20cpo tncpo01l;
056100090417             if %found(tncpo01l);
056200090417                v1cdpo=cporag;
056300090417             endif;
056400090417          endif;
056500090407       else;
056600100121         if imk20ksc > 0;
056700100121          *in03=*on;
056800090407          v1cksc=imk20ksc;
056900090420          if imk20rsc<>*blanks;
057000090420             v1cdks=imk20rsc;
057100090420          else;
057200090420             chain (kut:dutkci:imk20ksc) cnaco00f;
057300090420             if %found(cnaco00f);
057400090420                v1cdks=acorag;
057500090420             endif;
057600090420          endif;
057700100121         endif;
057800090407       endif;
057900090921
058000090407       // Pulizia subfile
058100090407       *in20=*off;
058200090407       *in21=*off;
058300090528       if dsp_mod=0;
058400090528          write mk20c01;
058500090528       else;
058600090528          write mk20c41;
058700090528       endif;
058800090407       *in20=*on;
058900090407       *in21=*on;
059000090408
059100090407       clear riri;
059200090407       clear coco;
059300090407       clear V1cnrr;
059400090407       clear V1cpos;
059500090407       clear s01nrr;
059600090528       clear s01nrr4;
059700090420       clear s01nrr_blu;
059800090407       clear v1cmsg;
059900090407       *in28=*off;
060000090407       *in90=*off;
060100090407       *in41=*off;
060200090407       reset $SavRec;
060300090409       reset $PiuRec;
060400090407       reset SavRec1;
060500090407       reset SavRec2;
060600090611       //reset SavRec3;
060700090611       //reset SavRec4;
060800090407       reset wNrr1Free;
060900090511       reset wNrr1new;
061000090409       reset savpno;
061100090407       savflm=imk20flm;
061200090921
061300090407       // Caricamento dati nel sfl
061400090407       exsr Ries01;
061500090921
061600090407       endsr;
061700090921
061800090407       //-------------------------------------------------------------*
061900090407       //Caricamento sfl                                              *
062000090407       //-------------------------------------------------------------*
062100090407       Begsr RieS01;
062200090921
062300090408       // Recupero il codice del potenziale da TFACO/CNACO, se non passato
062400090408       clear w_cpo;
062500090408       clear savcpo;
062600090408       clear savksc;
062700090518       clear savtat;
062800090408       clear savdim;
062900090408       clear savhim;
063000090921
063100090408       if imk20cpo=0;
063200090408          select;
063300090408          when imk20ksc>0;
063400090408             chain (kut:dutkci:imk20ksc) cnaco00f;
063500090408          when imk20nrv>0;
063600090408             chain (kut:dutkci:imk20nrv) tfaco00f;
063700090408          endsl;
063800090416          if %found;
063900090416             w_cpo=acolib;
064000090416          endif;
064100090408       else;
064200090408          w_cpo=imk20cpo;
064300090408       endif;
064400090416
064500090921       // ---------- if w_mk20flm<>' ';
064600090408       // Con Sql estraggo tutti i record del potenziale ordinandoli
064700090408       // per codice potenziale, data e ora immissione
064800090421          $finerec=*off;
064900090421          exsr sr_prepSqls01;
065000090421          exec sql prepare STRINGASQL1 from :WrkStringaSql;
065100090421          exec sql declare NOTEcsr1 cursor for StringaSql1;
065200090421          exec sql open NOTEcsr1;
065300090421          dow $finerec=*off;
065400090421             exec sql Fetch NOTEcsr1 into :ticpnds;
065500090421             if sqlcod=100 or sqlcod<0;
065600090421                $finerec = *on;
065700090421                if sqlcod<0;
065800090421                   omk20err='E';
065900090421                endif;
066000090421                leave;
066100090421             endif;
066200090921          // Imposto potenziale/cliente/tipo contatto/data e ora immissione
066300090921          // solo a rottura
066400090518             if cpncpo<>savcpo or cpnksc<>savksc or cpntat<>savtat
066500090421             or cpndim<>savdim or cpnhim<>savhim;
066600090421                if cpncpo>0;
066700090421                   v1scpo=%editc(cpncpo:'X');
066800090421                endif;
066900090421                if cpnksc>0;
067000090421                   v1sksc=%editc(cpnksc:'X');
067100091027                else;
067200091027                clear v1sksc;
067300090421                endif;
067400090518                v1stat=cpntat;
067500090421                select;
067600090518                when cpntat='A';
067700090528       //          v1stat='App';
067800090518                   *in39=*on;
067900090518                when cpntat='T';
068000090528       //          v1stat='Tel';
068100090518                   *in38=*on;
068200090421                endsl;
068300090421                datadmy=%date(cpndim:*iso);
068400090421                v1sdim=%dec(datadmy);
068500090421                v1shim=%dec(%subst(%editc(cpnhim:'X'):1:4):4:0);
068600090422                v1ssns=cpnsns;
068700090422                clear h1sdim;
068800090422                clear h1shim;
068900090422                clear h1scpo;
069000090422                clear h1sksc;
069100090422                clear h1snrv;
069200090518                clear h1stat;
069300090518                clear h1satn;
069400090518                clear h1satnp;
069500090422                clear h1spos;
069600090422                clear h1spru;
069700090422                clear h1ssns;
069800090408
069900090421                savcpo=cpncpo;
070000090421                savksc=cpnksc;
070100090518                savtat=cpntat;
070200090421                savdim=cpndim;
070300090421                savhim=cpnhim;
070400090421             else;
070500090421                clear v1scpo;
070600090421                clear v1sksc;
070700090421                clear v1sdim;
070800090421                clear v1shim;
070900090518                clear v1stat;
071000090518                *in38='0';
071100090518                *in39='0';
071200090422                clear v1ssns;
071300090421             endif;
071400090421             h1sdim=cpndim;
071500090421             h1shim=%dec(%subst(%editc(cpnhim:'X'):1:4):4:0);
071600090416             h1scpo=cpncpo;
071700090416             h1sksc=cpnksc;
071800090416             h1snrv=cpnnrv;
071900090518             h1stat=cpntat;
072000090518             h1satn=cpnatn;
072100090518             h1satnp=cpnatnp;
072200090417             h1spos=cpnpos;
072300090417             h1spru=cpnpru;
072400090422             h1ssns=cpnsns;
072500090615       // Note
072600090528          if dsp_mod=0;
072700090409             v1snot=cpnnot;
072800090528          else;
072900090528             v1snt4=cpnnot;
073000090528          endif;
073100090409       // Impostazione indicatori
073200090409       // - protezione
073300090409             *in35=*on;
073400090421             h1in35=*in35;
073500090420       // Coloro note in blu se relative alla chiave passata dal chiammnte
073600090518             if imk20nrv>0 and cpnnrv=imk20nrv or (imk20atn>0 and
073700090518             imk20tat=cpntat and imk20atn=cpnatn and imk20atnp=cpnatnp);
073800090421                *in37=*on;
073900090421             else;
074000090421                *in37=*off;
074100090421             endif;
074200090615             h1in37=*in37;
074300090409       // - posizionamento cursore
074400090409       //   sul 1° campo "note" vuoto e non protetto
074500090420       //   oppure in interrogazione sulla nota evidenziata in base al tipo chiamata
074600090528          if dsp_mod=0;
074700090421             *in41=(*in35=*off and v1snot=*blanks);
074800090528          else;
074900090528             *in41=(*in35=*off and v1snt4=*blanks);
075000090528          endif;
075100090421             *in41=(*in37 and imk20flm='I');
075200090420
075300090421             s01nrr=s01nrr+1;
075400090528             s01nrr4=s01nrr4+1;
075500090421             if *in37;
075600090421                if w_mk20flm<>'I'  or v1cpos=0;
075700090421                   v1cpos=s01nrr;
075800090528                   v1cpos=s01nrr4;
075900090421                endif;
076000090421             endif;
076100090420       // Se chiamata per codice cliente memorizzo nrr se nota del cliente
076200090421       // Mi serve per impostare il colore sulla nota più recente
076300090420          if imk20ksc>0 and cpnksc=imk20ksc and v1sksc>*zeros;
076400090603             if w_mk20flm<>'I' or s01nrr_blu=0;
076500090528              if dsp_mod=0;
076600090421                s01nrr_blu=s01nrr;
076700090528              else;
076800090528                s01nrr_blu=s01nrr4;
076900090528              endif;
077000090421             endif;
077100090420          endif;
077200090409
077300090409       // Scrittura record sfl
077400090528          if dsp_mod=0;
077500090528             write mk20s01;
077600090612                clear v1scpo;
077700090612                clear v1sksc;
077800090612                clear v1sdim;
077900090612                clear v1shim;
078000090612                clear v1stat;
078100090612                *in38='0';
078200090612                *in39='0';
078300090612                clear v1ssns;
078400090611             v1snot=%subst(cpnnot: 31 :30);
078500090611             s01nrr=s01nrr+1;
078600090611             write mk20s01;
078700090611             v1snot=%subst(cpnnot: 61 :30);
078800090611             s01nrr=s01nrr+1;
078900090611             write mk20s01;
079000090528          else;
079100090528             write mk20s41;
079200090528          endif;
079300090409
079400090611       // - memorizzazione dei 1' 2 rec. da restituire al pgm chiamante
079500090409          if $savrec=*on;
079600090409             select;
079700090409w   4        when  SR1cpo  = *zeros;
079800090409                savrec1=ticpnds;
079900090409w   4        when  SR2cpo  = *zeros;
080000090409                savrec2=ticpnds;
080100090611                $savrec=*off;
080200090409             endsl;
080300090409          else;
080400090409             $piurec=*on;
080500090409          endif;
080600090421          enddo;
080700090921       // ---------------- endif;
080800090921
080900090409       // Memorizzo l'ultimo n° progressivo note già registrato
081000090409       //  (se non in variazione)
081100090921       // ---------------- if w_mk20flm<>*blanks;
081200090409          savpno=cpnpno;
081300090921       // ---------------- endif;
081400090409       // Memorizzo il nrr del primo record vuoto (inseribile)
081500090528       if dsp_mod=0;
081600090611          wNrr1Free=s01nrr+1;
081700090528       else;
081800090528          wNrr1Free=s01nrr4+1;
081900090528       endif;
082000090421       // aggiornamento colore blu sulle note più recenti del cliente
082100090420       if s01nrr_blu >0;
082200090604          if dsp_mod=0;
082300090604            chain s01nrr_blu mk20s01;
082400090604          else;
082500090604            chain s01nrr_blu mk20s41;
082600090604          endif;
082700090615          dow h1sksc=imk20ksc and %found;
082800090604             if v1cpos>0 and v1sksc>*zeros;
082900090604                leave;
083000090604             endif;
083100090420             *in37=*on;
083200090420             h1in37=*in37;
083300090604             if dsp_mod=0;
083400090604                if v1cpos=0;
083500090604                   v1cpos=s01nrr;
083600090604                endif;
083700090528                update mk20s01;
083800090604             else;
083900090604                if v1cpos=0;
084000090604                   v1cpos=s01nrr4;
084100090604                endif;
084200090528                update mk20s41;
084300090604             endif;
084400090420             s01nrr_blu+=1;
084500090604             if dsp_mod=0;
084600090604                chain s01nrr_blu mk20s01;
084700090604             else;
084800090604                chain s01nrr_blu mk20s41;
084900090604             endif;
085000090420          enddo;
085100091027        if dsp_mod=0;
085200091027          s01nrr = wNrr1Free -1 ;
085300091027        else;
085400091027          s01nrr4 = wNrr1Free -1;
085500091027        endif;
085600090420       endif;
085700090921
085800090409       // Caricamento righe vuote (se non interrogazione)
085900090421       if w_mk20flm<>'I';
086000090528        if dsp_mod=0;
086100090409          wnrr=s01nrr;
086200090528        else;
086300090528          wnrr=s01nrr4;
086400090528        endif;
086500090409          exsr sr_FreeRow;
086600111018          If wnrr1new < 3 ;
086700111018             exsr sr_FreeRow;
086800111018          Endif ;
086900090409       else;
087000090528         if dsp_mod=0;
087100090409          *in09=s01nrr>k_sflpag;
087200090528         else;
087300090528          *in09=s01nrr4>k_sflpag;
087400090528         endif;
087500090409          *in23=*on;
087600090409       endif;
087700090921
087800090528       if dsp_mod=0;
087900090528          *in20=s01nrr>*zeros;
088000090528       else;
088100090528          *in20=s01nrr4>*zeros;
088200090528       endif;
088300090921
088400090420       if v1cpos=0;
088500090420          v1cpos=1;
088600090420       endif;
088700100614
088800100614       //?Se interrogazione e riga posizionamento = 1 imposto ultimo
088900100614       //?record caricato come riga di posizionamento
089000100614       //?se non è prima pagina imposto costante 'precedente...'
089100100614       IF  V1Cpos = 1 and w_mk20flm = 'I';
089200100614         IF dsp_mod = 0;
089300100614          V1Cpos = S01nrr;
089400100614         ELSE;
089500100614          V1Cpos = S01nrr4;
089600100614         ENDIF;
089700100614         IF  V1Cpos > 1 and V1Cpos >= (k_sflpag + 1);
089800100614         ENDIF;
089900100614       ENDIF;
090000090409
090100090407       endsr;
090200090408       //-------------------------------------------------------------*
090300090408       //Preparazione stringa sql per caricamento note nel subfile    *
090400090408       //-------------------------------------------------------------*
090500090408       Begsr SR_prepSqls01;
090600090408
090700090420       select;
090800090420       when imk20ksc>0;
090900090420       // Entrata per cliente: seleziono solo records del cliente passato
091000090420       // o del suo potenziale ma non di altri clienti.
091100090420         WrkStringaSql='Select * from ticpn00f where cpnksc=';
091200090420         WrkStringaSql=WrkStringaSql + %editc(imk20ksc:'4');
091300090420         WrkStringaSql=WrkStringaSql + ' or (cpncpo=';
091400090420         WrkStringaSql=WrkStringaSql + %editc(w_cpo:'4');
091500090420         WrkStringaSql=WrkStringaSql + ' and cpnksc=0)';
091600100121       When imk20cpo > 0;
091700090420       // Entrata per potenziale:seleziono tutti i record del potenziale
091800090420       // passato
091900090420         WrkStringaSql='Select * from ticpn00f where cpncpo=';
092000090420         WrkStringaSql=WrkStringaSql + %editc(w_cpo:'4');
092100100121       when imk20atn > 0;
092200100121         WrkStringaSql='Select * from ticpn00f where cpnatn=';
092300100121         WrkStringaSql=WrkStringaSql + %editc(imk20atn:'X');
092400090420       endsl;
092500090421       // Ordinamento -> decrescente in interrogazione; crescente in manutenzione
092600100614       //?Sempre crescente per data come in manutenzione (ND - 14/06/2010)
092700100614       //if imk20flm='I';
092800100614       //   if imk20ksc=0;
092900100614       //     WrkStringaSql=WrkStringaSql+' Order by cpndim desc,cpnhim desc,';
093000100614       //     WrkStringaSql=WrkStringaSql+'cpnpru,cpnksc,cpnpno';
093100100614       //   else;
093200100614       //     WrkStringaSql=WrkStringaSql+' Order by cpndim desc,cpnhim desc,';
093300100614       //     WrkStringaSql=WrkStringaSql+'cpnpru,cpnpno';
093400100614       //   endif;
093500100614       //else;
093600090421          if imk20ksc=0;
093700090421            WrkStringaSql=WrkStringaSql+' Order by cpndim,cpnhim,cpnpru,cpnksc';
093800110407        //  WrkStringaSql=WrkStringaSql+' ,cpnpno';
093900110407            WrkStringaSql=WrkStringaSql+' ,cpnatn,cpnatnp,cpnpno';
094000090421          else;
094100110407       //   WrkStringaSql=WrkStringaSql+' Order by cpndim,cpnhim,cpnpru,cpnpno';
094200110407            WrkStringaSql=WrkStringaSql+' Order by cpndim,cpnhim,cpnpru';
094300110407            WrkStringaSql=WrkStringaSql+' ,cpnatn,cpnatnp,cpnpno';
094400090421          endif;
094500100614       //endif;
094600090408       WrkStringaSql=WrkStringaSql+' FOR READ ONLY';
094700090408
094800090408       endsr;
094900090921
095000090409       //-------------------------------------------------------------*
095100090409       //Riempimento sfl con righe vuote                              *
095200090409       //-------------------------------------------------------------*
095300090409       Begsr SR_FreeRow;
095400090921
095500090409       *in09=*on;
095600090528       if dsp_mod=0;
095700090528         s01nrr=wnrr;
095800090528       else;
095900090528         s01nrr4=wnrr;
096000090528       endif;
096100090409
096200111012         Wrig=%rem(wnrr:K_sflpag)+1;
096300090528        for wrig to k_sflpag;
096400090409          s01nrr=s01nrr+1;
096500090528          s01nrr4=s01nrr4+1;
096600090409          clear mk20s01;
096700090528          clear mk20s41;
096800090409          h1in35=*off;
096900090409          h1in36=*off;
097000090420          h1in37=*off;
097100090518          *in38='0';
097200090518          *in39='0';
097300090528          if (s01nrr=wNrr1Free and dsp_mod=0) or (s01nrr4=wnrr1Free and
097400090528          dsp_mod=1);
097500090415             if imk20cpo>0;
097600090415                v1scpo=%editc(imk20cpo:'X');
097700090415             endif;
097800090415             if imk20ksc>0;
097900090415                v1sksc=%editc(imk20ksc:'X');
098000090415             endif;
098100090422             v1ssns='S';
098200090518             v1stat=imk20tat;
098300090422             select;
098400090518             when v1stat='A';
098500090528       //       v1stat='App';
098600090518                *in39=*on;
098700090518             when v1stat='T';
098800090528       //       v1stat='Tel';
098900090518                *in38=*on;
099000090422             endsl;
099100090417             clear v1sdim;
099200090417             clear v1shim;
099300090409          endif;
099400090422          clear h1sdim;
099500090422          clear h1shim;
099600090422          h1scpo=w_cpo;
099700090422          h1sksc=imk20ksc;
099800090422          h1snrv=imk20nrv;
099900090518          h1stat=imk20tat;
100000090518          h1satn=imk20atn;
100100090518          h1satnp=imk20atnp;
100200090422          h1ssns='S';
100300090528            *in23= (wnrr=k_sflpag);
100400090611          if wnrr1new<3 and w_mk20flm<>' ';
100500090611             select;
100600090611             when wnrr1new=1;
100700090611              if dsp_mod=0;
100800090611                v1snot=emk20no1;
100900090611                w20nota=emk20no1;
101000090611              else;
101100090611                v1snt4=emk20no1;
101200090611              endif;
101300090611             when wnrr1new=2;
101400090611              if dsp_mod=0;
101500090611                v1snot=emk20no2;
101600090612                w20nota=emk20no2;
101700090611              else;
101800090611                v1snt4=emk20no2;
101900090611              endif;
102000090611             endsl;
102100090611          endif;
102200090528          if dsp_mod=0;
102300090528            *in41=(v1snot=*blanks);
102400090528            write mk20s01;
102500090611          if wnrr1new<3 and w_mk20flm<>' ';
102600090612            clear v1scpo;
102700090612            clear v1sksc;
102800090612            clear v1ssns;
102900090612            clear v1stat;
103000090611            v1snot=%subst(w20nota:31:30);
103100090611            s01nrr=s01nrr+1;
103200090611            write mk20s01;
103300090611            v1snot=%subst(w20nota:61:30);
103400090611            s01nrr=s01nrr+1;
103500090611            write mk20s01;
103600090611          endif;
103700090528          else;
103800090528            *in41=(v1snt4=*blanks);
103900090528            write mk20s41;
104000090528          endif;
104100090409          wnrr=wnrr+1;
104200090511          wnrr1new=wnrr1new+1;
104300090528        endfor;
104400111018
104500090528         v1cpos=s01nrr-k_sflpag+1;
104600090528       if dsp_mod=0;
104700090528         wnrr=s01nrr;
104800090528       else;
104900090528         wnrr=s01nrr4;
105000090528       endif;
105100090409
105200090409       endsr;
105300090409       //-------------------------------------------------------------*
105400090611       //Aggiornamento prime note del subfile con le note ricevute    *
105500090409       //-------------------------------------------------------------*
105600090409       Begsr Upds01;
105700090415       $nrr_agg=1;
105800090611       ps=1;
105900090415       for $nrr=wnrr1free to wnrr;
106000090528        if dsp_mod=0;
106100090409          chain $nrr mk20s01;
106200090528        else;
106300090528          chain $nrr mk20s41;
106400090528        endif;
106500090615          if %found and h1in35=*off;
106600090409             select;
106700090415             when $nrr_agg = 1;
106800090611                v1snot=%subst(emk20no1:ps:30);
106900090528                v1snt4=emk20no1;
107000090612              if dsp_mod=0;
107100090611                if ps=61;
107200090611                   $nrr_agg+=1;
107300090611                   ps=1;
107400090611                else;
107500090611                   ps=ps+30;
107600090611                endif;
107700090611              else;
107800090611                   $nrr_agg+=1;
107900090611              endif;
108000090415             when $nrr_agg = 2;
108100090611                v1snot=%subst(emk20no2:ps:30);
108200090528                v1snt4=emk20no2;
108300090612              if dsp_mod=0;
108400090611                if ps=61;
108500090611                   $nrr_agg+=1;
108600090611                   ps=1;
108700090611                else;
108800090611                   ps=ps+30;
108900090611                endif;
109000090611              else;
109100090611                   $nrr_agg+=1;
109200090611              endif;
109300090409             endsl;
109400090409       // Indicatori
109500090409       // - protezione
109600090409             *in35=h1in35;
109700090409       // - alta intensità
109800090409             *in36=h1in36;
109900090616       // - Colore
110000090420             *in37=h1in37;
110100090616             *in38=*off;
110200090616             *in39=*off;
110300090616             select;
110400090616             when v1stat='A';
110500090616                *in39=*on;
110600090616             when v1stat='T';
110700090616                *in38=*on;
110800100928             endsl;
110900090409       // - posizionamento cursore
111000090409       //   sul 1° campo vuoto e non protetto
111100090528             if dsp_mod=0;
111200090528                *in41=(*in35=*off) and (v1snot=*blanks);
111300090409       // - aggiornamento record nel subfile
111400090528                update mk20s01;
111500090528             else;
111600090528                *in41=(*in35=*off) and (v1snt4=*blanks);
111700090528       // - aggiornamento record nel subfile
111800090528                update mk20s41;
111900090528             endif;
112000090409          endif;
112100090409       endfor;
112200090409       endsr;
112300040108
112400090409       //-------------------------------------------------------------*
112500090612       //Reperimento prime righe del subfile con le note              *
112600090409       //-------------------------------------------------------------*
112700090409       Begsr Sr_reps01;
112800090921
112900090415       $nrr_agg=1;
113000090612       ctn=1;
113100090612       clear nota90;
113200090415       reset $piurec;
113300090921
113400090415       for $nrr=wnrr1free to wnrr;
113500090528          if dsp_mod=0;
113600090528             chain $nrr mk20s01;
113700090528          else;
113800090528             chain $nrr mk20s41;
113900090528          endif;
114000090615          if %found and h1in35=*off;
114100090415             select;
114200090415             when $nrr_agg = 1;
114300090528                if dsp_mod=0;
114400090612                 if ctn<4;
114500090612                    nota(ctn)=v1snot;
114600090612                    ctn+=1;
114700090612                 endif;
114800090612                 if ctn=4;
114900090612                    emk20no1=nota90;
115000090612                    $nrr_agg+=1;
115100090612                    ctn=1;
115200090612                 endif;
115300090528                else;
115400090528                   emk20no1=v1snt4;
115500090612                   $nrr_agg+=1;
115600090528                endif;
115700090416                emk20diM=h1sdim;
115800090417                emk20him=h1shim;
115900090415             when $nrr_agg = 2;
116000090528                if dsp_mod=0;
116100090612                 if ctn<4;
116200090612                    nota(ctn)=v1snot;
116300090612                    ctn+=1;
116400090612                 endif;
116500090612                 if ctn=4;
116600090612                    emk20no2=nota90;
116700090612                    $nrr_agg+=1;
116800090612                    ctn=1;
116900090612                 endif;
117000090528                else;
117100090528                   emk20no2=v1snt4;
117200090612                   $nrr_agg+=1;
117300090528                endif;
117400090612             when $nrr_agg > 2 and ((dsp_mod=0 and v1snot<>*blanks) or
117500090528                  (dsp_mod=1 and v1snt4<>*blanks));
117600090415                $PiuRec=*on;
117700090415             endsl;
117800090415       // Indicatori
117900090415       // - protezione
118000090415             *in35=h1in35;
118100090415       // - alta intensità
118200090415             *in36=h1in36;
118300090420       // - colore blu
118400090420             *in37=h1in37;
118500090415       // - posizionamento cursore
118600090415       //   sul 1° campo vuoto e non protetto
118700090528             if dsp_mod=0;
118800090528               *in41=(*in35=*off) and (v1snot=*blanks);
118900090415       // - aggiornamento record nel subfile
119000090528               update mk20s01;
119100090528             else;
119200090528               *in41=(*in35=*off) and (v1snt4=*blanks);
119300090528       // - aggiornamento record nel subfile
119400090528               update mk20s41;
119500090528             endif;
119600090415          endif;
119700090415          if $piurec=*on;
119800090415             leave;
119900090415          endif;
120000090415       endfor;
120100090409       endsr;
120200090409       //-------------------------------------------------------------*
120300090409       //                                                             *
120400090409       //-------------------------------------------------------------*
120500090409       Begsr Sr_UpdTICPN;
120600090415       clear ticpn000;
120700090416       cpnpno=0;
120800090611       nota90=*blanks;
120900090612       clear ctn;
121000090615       clear clt;
121100100929       $FineSfl = *off;
121200090424       // Se ho il subfile caricato scrivo in base a contenuto sfl
121300090424       if wnrr1free>0;
121400100928       // Se ho il subfile caricato prima aggiorno le prime 2 righe libere
121500100928          if imk20flm='C' and imk20fno = 'S' ;
121600100928             exsr Upds01 ;
121700100928          endif ;
121800090424          for $nrr=wnrr1free to wnrr;
121900090528             if dsp_mod=0;
122000090528                chain $nrr mk20s01;
122100090528             else;
122200090528                chain $nrr mk20s41;
122300090528             endif;
122400090615             if %found and ((dsp_mod=0 and v1snot<>*blanks) or
122500090615                            (dsp_mod=1 and v1snt4<>*blanks));
122600090615                $no_aggio=*off;
122700090615                clt+=1;
122800100929                // Richiamato con "C"-Conferma dati precedentemente immessi
122900100929                if imk20flm='C';
123000100929                 // Se richiesto, le prime due note le prendo dai parametri
123100100929                 // ricevuti in input
123200100929                   if imk20fno='S';
123300100929                 // Se devo utilizzare le prime due note ricevute in input
123400100929                 // memorizzo di non utilizzare le prime sei note del subfile
123500100929                 // in modalità 24x80
123600100929                      if dsp_mod=0 and clt<=6 and cpnpno >=2;
123700100929                         $no_aggio=*on;
123800100929                      endif;
123900100929                   endif;
124000100929                 // Telefonata e numero visita li prendo dai parametri ricevuti
124100100929                 // se nel sfl sono vuoti
124200100929                   if h1satn=*zeros;
124300100929                      h1stat=imk20tat;
124400100929                      h1satn=imk20atn;
124500100929                      h1satnp=imk20atnp;
124600100929                   endif;
124700100929                   if h1snrv=0;
124800100929                      h1snrv=imk20nrv;
124900100929                   endif;
125000100929                endif;
125100090615                if $no_aggio=*off;
125200090615                   exsr  rie_ticpn;
125300090615                endif;
125400090424             endif;
125500090424          endfor;
125600100929          $FineSfl = *on ;
125700090615       // Devo richiamare routine di scrittura note se in modalità 24x80
125800100929       // ed è rimasta sottostringa di nota lunga 90
125900090615          if dsp_mod=0 and nota90<>*blanks;
126000090615             exsr  rie_ticpn;
126100100929             $FineSfl = *off;
126200090615          endif;
126300090424       else;
126400090424       // Se non ho il sfl caricato scrivo in base ai parametri ricevuti
126500090424          h1scpo=imk20cpo;
126600090424          h1sksc=imk20ksc;
126700090518          h1stat=imk20tat;
126800090518          h1satn=imk20atn;
126900090518          h1satnp=imk20atnp;
127000090424          h1snrv=imk20nrv;
127100090424          h1ssns='S';
127200090424          clear v1snot;
127300090611          clear v1snt4;
127400090611          for xx=1 to 2;
127500090424             select;
127600090424             when xx=1;
127700090611                nota90=emk20no1;
127800090612                ctn=3;
127900090528                v1snt4=emk20no1;
128000090424             when xx=2;
128100090611                nota90=emk20no2;
128200090612                ctn=3;
128300090528                v1snt4=emk20no2;
128400090424             endsl;
128500090611             if (dsp_mod=0 and nota90<>*blanks) or
128600090528                (dsp_mod=1 and v1snt4<>*blanks);
128700090424                exsr  rie_ticpn;
128800090424             endif;
128900090424          endfor;
129000090424       endif;
129100090415       reset  $inzs01;
129200090415       reset save20ds;
129300090415       clear savflm;
129400090409       endsr;
129500090424       //-------------------------------------------------------------*
129600090424       //                                                             *
129700090424       //-------------------------------------------------------------*
129800090424       Begsr Rie_Ticpn;
129900090424
130000090424          cpncpo=h1scpo;
130100090424          CPNKSC=h1sksc;
130200090518          CPNTat=h1stat;
130300090518          CPNatn=h1satn;
130400090518          CPNatnP=h1satnp;
130500090424          CPNNRV=h1snrv;
130600090424       // Data ora immissione --> aggiorno con quella ricevuta dal chiamante se
130700090424       //                         presente, altrimenti con data/ora corrente
130800100503          // if emk20dim>0;
130900100503          // CPNDIM=emk20dim;
131000100503          // CPNHIM=emk20him;
131100100503          //  else;
131200090424             CPNDIM=%dec(dataiso_cor);
131300090424             CPNHIM=Wora;
131400100503          //  endif;
131500090424          CPNSNS=h1ssns;
131600090424          CPNPOS=dutpou;
131700090424          CPNPRU=knmus;
131800090611          if dsp_mod=0;
131900090611             if ctn<3;
132000090611                ctn+=1;
132100090611                nota(ctn)=v1snot;
132200090612             endif;
132300100929             if ctn=3 or $fineSfl = *on ;
132400090611                cpnnot=nota90;
132500090611                exsr sr_chk;
132600090612                CPNPNO+=1;
132700090611                write ticpn000;
132800090612                clear ctn;
132900090615                clear nota90;
133000090611             endif;
133100090610          else;
133200090615             cpnnot=v1snt4;
133300090615       //    campo note:controllo che non contenga caratteri particolari
133400090615             exsr sr_chk;
133500090612             CPNPNO+=1;
133600090610             write ticpn000;
133700090610          endif;
133800090424          endsr;
133900090416       //-------------------------------------------------------------*
134000090416       //                                                             *
134100090416       //-------------------------------------------------------------*
134200090416       Begsr Sr_Chk;
134300090416       clear TxtInOut;
134400090416       TxtInOut=cpnnot;
134500090416       xChkChar(TxtInOut:ElencoChar:TipoElenco:CharSost:Uppercase:chknull:
134600090416       CharNull:Esito);
134700090416       if esito='1';
134800090416          cpnnot=TxtInOut;
134900090416       endif;
135000090416       endsr;
135100090416       //-------------------------------------------------------------*
135200090416       // Gestione tasto funzione F1=Informazioni aggiuntive          *
135300090416       //-------------------------------------------------------------*
135400090416       Begsr Sr_f1;
135500090616       if dsp_mod=0;
135600090616          R$$=di_NrRg;
135700090616          C$$=di_NrCl;
135800090616       else;
135900090616          R$$=dsp4ds.di_NrRg;
136000090616          C$$=dsp4ds.di_NrCl;
136100090616       endif;
136200090416       cur_R=riri;
136300090416       cur_C=coco;
136400090422       clear mk20w01;
136500090528       clear mk20w41;
136600090416       exsr RepNrrSfl;
136700090416       if $nrr > 0 and $nrr<wnrr1free;
136800090528          if dsp_mod=0;
136900090528             chain $nrr mk20s01;
137000090528          else;
137100090528             chain $nrr mk20s41;
137200090528          endif;
137300090417          if h1spos>0;
137400090416             if  cur_r<=12;
137500090416                posrig=13;
137600090416             else;
137700090416                posrig=6;
137800090416             endif;
137900100121
138000100121          // Compongo la riga con descrizione tipo attività + data + ora
138100090518             if h1stat<>*blanks;
138200100121
138300100121                  clear  dTTA;
138400100121                  clear  TIBS02ds;
138500100121                  T02mod = 'C';
138600100121                  T02cod = 'TTA';
138700100121                  T02ke1 = h1stat;
138800100121                  T02sif = KNSIF;
138900100121                  TNTBE_RicercaControllo  (kpjba : tibs02ds);
139000100121                  if  T02err = *blanks;
139100100121                      dtta = t02uni ;
139200100121                  endif;
139300100121
139400100121                W1cdes = §ttades;
139500100121                W1cdes = %trim(W1cdes) + ' del' + ' ' + hi +
139600100121                         %subst(%editc(h1sdim:'X'):7:2) + '/' +
139700100121                         %subst(%editc(h1sdim:'X'):5:2) + '/' +
139800100121                         %subst(%editc(h1sdim:'X'):1:4) + '  ' +
139900100121                         %subst(%editc(h1shim:'X'):1:2) + ':' +
140000100121                         %subst(%editc(h1shim:'X'):3:2);
140100090416             endif;
140200090416             if h1snrv>0;
140300090416                w1cnrv=h1snrv;
140400090423                *in06=*on;
140500090423             else;
140600090423                *in06=*off;
140700090416             endif;
140800090417             w1cpos=h1spos;
140900090417             w1cpru=h1spru;
141000090417             clear w1dpos;
141100090417             chain w1cpos azorg01l;
141200090417             if %found (azorg01l);
141300090417             w1dpos=orgdes;
141400090417             endif;
141500090622       /// Uscita dalla window per F12-Ritorno
141600090622            dow *inkl=*off;
141700090528             if dsp_mod=0;
141800090528                exfmt mk20w01;
141900090528             else;
142000090528                exfmt mk20w41;
142100090528             endif;
142200090622            enddo;
142300090417          endif;
142400090416       endif;
142500090416       endsr;
142600090416       //-------------------------------------------------------------*
142700090416       // Reperimento  nrr del sfl su cui è posizionato il cursore    *
142800090416       //-------------------------------------------------------------*
142900090416       Begsr RepNrrSfl;
143000090616       if dsp_mod=1;
143100090616          di_sflnrr=dsp4ds.di_sflnrr;
143200090616       endif;
143300090416       $nrr=(di_sflnrr + riri - K_sflnrr1);
143400090416       if $nrr<*zeros or riri<k_sflnrr1;
143500090416          clear $nrr;
143600090416       endif;
143700090416       Endsr;
143800090421       //-------------------------------------------------------------*
143900090421       // Gestione tasto funzione F10=Inserimento                     *
144000090421       //-------------------------------------------------------------*
144100090421       Begsr Sr_f10;
144200090421
144300090421       w_mk20flm=' ';
144400090421       exsr sr_inzs01;
144500090421       if not *in90;
144600090528          if dsp_mod=0;
144700090528             write mk20t00;
144800090528             write mk20z01;
144900090528          else;
145000090528             write mk20t40;
145100090528             write mk20z41;
145200090528          endif;
145300090421       endif;
145400090421
145500090421       // posiziono il cursore nel sfl
145600090421       v1cnrr=v1cpos;
145700090421
145800090421       // Emetto la videata
145900090421       reset $finef10;
146000090421       dow $finef10=*off;
146100090528          if dsp_mod=0;
146200090528             exfmt mk20c01;
146300090528          else;
146400090528             exfmt mk20c41;
146500090528          endif;
146600090421          *in28=*off;
146700090421          *in90=*off;
146800090421          clear v1cmsg;
146900090421          select;
147000090421       // F3=Fine
147100090421             When *inkc;
147200090421                omk20fxx='03';
147300090421                leavesr;
147400090421       // F12=Ritorno --> per il momento non lo gestisco perchè per farlo bene dovrei
147500090421       //                 utilizzare un altro sfl. Vediamo prima cosa vogliono veramente
147600090421       //    When *inkl;
147700090421       //       omk20fxx='12';
147800090421       //       leavesr;
147900090421       // Rollup
148000090421             When *in25;
148100090421       //    Caricamento righe vuote
148200090616                if dsp_mod=0;
148300090616                   v1cpos=di_sflnrr;
148400090616                else;
148500090616                   v1cpos=dsp4ds.di_sflnrr;
148600090616                endif;
148700090421                exsr sr_freerow;
148800090421       // F6=Conferma
148900090421             when *inkf;
149000090421                exsr sr_updTICPN;
149100090421                omk20fxx='06';
149200090421                leavesr;
149300090421          endsl;
149400090421       enddo;
149500090421
149600090421       Endsr;
149700090921
149800090921       //-------------------------------------------------------------*
149900090921       // Gestione tasto funzione F6=Conferma                         *
150000090921       //-------------------------------------------------------------*
150100090921       Begsr Sr_f6;
150200090921
150300090921         exsr sr_updTICPN;
150400090921         omk20fxx='06';
150500090921         $fine=*on;
150600090921
150700090921       Endsr;

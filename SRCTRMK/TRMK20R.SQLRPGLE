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
001900120926     fTICPN04L  if   e           k disk    rename(TICPN000:TICPN04) prefix(S_)
002000121109
002100121109     fTIVIS05L  if   e           k disk
002200040112      *
002300090526     ftrmk20d   cf   e             workstn sfile(mk20S01:S01nrr)
002400040112     f                                     infds(dspINFOds)
002500090527     f                                     usropn
002600090615     ftrmk20d4  cf   e             workstn sfile(mk20S41:S01nrr4)
002700090616     f                                     infds(dsp4DS)
002800090615     f                                     usropn
002900040107      *---------------------------------------------------------------*
003000040107
003100040107      *
003200040107      *   C O S T A N T I   - - - - - - - - - - - - - - - - - - - - - *
003300040107      *
003400040108     d K_tit1          c                   const('  IMMISSIONE  ')
003500040108     d K_tit2          c                   const(' MANUTENZIONE ')
003600040108     d K_tit3          c                   const('INTERROGAZIONE')
003700040109      * - nr. riga del 1° rec. del sfl
003800040109     d K_SflNrr1       c                   const(6)
003900090527     d K_SflNrr14      c                   const(7)
004000040120      * - tasto "Invio"
004100040120     d K_Enter         c                   const(X'F1')
004200100121      * - DspAtr "HI"
004300100121     d hi              C                   CONST(X'22')
004400040107      *
004500040107      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
004600040107      *
004700040107      *
004800040107      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
004900040107      *
005000040107     d KPJBA         e ds
005100040108      * - Parametri
005200090323     d trmk20ds      e ds
005300090323     d Save20ds      e ds                  extname(trmk20ds) inz
005400040213     d                                     prefix(X:1)
005500040108      *
005600040108      * - Parametri x Controllo profilo utenti
005700090414     d TIBS34DS      e ds                  inz
005800090402     d §azute        e ds                  extname(azute00f)
005900090402     d                                     dtaara
006000090402     d §datiute      e ds                  extname(ddatiute)
006100090402     d                                     dtaara
006200040123      *
006300090403     d ticpnds       e ds                  extname(TICPN00F) inz
006400040130      * - Salvataggio dati dei 1' 4 records immessi o modificati
006500090323     d SAVrec1       e ds                  extname(TICPN00F) inz
006600040130     d                                     prefix(SR1:3)
006700090323     d SAVrec2       e ds                  extname(TICPN00F) inz
006800040130     d                                     prefix(SR2:3)
006900090611     d*SAVrec3       e ds                  extname(TICPN00F) inz
007000090611     d*                                    prefix(SR3:3)
007100090611     d*SAVrec4       e ds                  extname(TICPN00F) inz
007200090611     d*                                    prefix(SR4:3)
007300100121
007400100121     d dtta          e ds
007500100121     d TIBS02ds      e ds                  inz
007600100121
007700090611     d                 ds
007800090611     d nota90                        90
007900090611     d nota                          30    dim(3) overlay(nota90)
008000040109      *
008100040109     d Status         sds
008200040109     d  SDSpgm           *proc
008300100224
008400100224     D cpnds           ds
008500100224     D  cpnnrr               397    400b 0
008600100224
008700040107      *
008800090616     d dspINFOds       ds           512
008900040120     d  di_TASTO             369    369
009000090416     d  di_NrRg              370    370
009100090416     d  di_NrCl              371    371
009200040120     d  di_SFLnrr            378    379B 0
009300090616      *
009400090616     d dsp4ds          ds                  likeds(dspinfods)
009500090409
009600090409     d Cursor          ds
009700090409     d  RIRI                   1      2B 0
009800090409     d  R$$                    2      2
009900090409     d  COCO                   3      4B 0
010000090409     D  C$$                    4      4
010100090416
010200090416     d Cur_R           s              2  0 inz
010300100614     d Cur_C           s              3  0 inz
010400040109      *
010500040107      *
010600040107      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
010700040107      *
010800090409     D WrkStringaSql   S           4500
010900090409     D                                     VARYING
011000040109      * - Campi di appoggio per il subfile
011100040123     d S01nrr          s                   like(V1Cnrr)   inz
011200090527     d S01nrr4         s                   like(V1Cnrr)   inz
011300090511     d wNrr1Free       s                   like(V1Cnrr)   inz
011400090511     d wNrr1new        s                   like(V1Cnrr)   inz(1)
011500040213     d $Nrr            s                   like(V1Cpos)   inz
011600090415     d $Nrr_agg        s                   like(V1Cpos)   inz
011700040213     d wNrr            s                   like(V1Cpos)   inz
011800040213     d wPag            s                   like(V1Cpos)   inz
011900040213     d wRig            s                   like(V1Cpos)   inz
012000090528      * - nr. di righe del sfl (SFLPAG)
012100090528     d K_SflPag        s              2s 0
012200040109      *
012300040109      * - Campi di salvataggio temporaneo dati
012400090409     d SAVpno          s                   like(cpnpno)   inz
012500090409     d SAVflm          s                   like(Imk20flm)   inz
012600090409     d SAVcpo          s                   like(cpncpo)   inz
012700090409     d SAVksc          s                   like(cpnksc)   inz
012800090409     d SAVdim          s                   like(cpndim)   inz
012900090409     d SAVhim          s                   like(cpnhim)   inz
013000090518     d SAVtat          s                   like(cpntat)   inz
013100040109      *
013200040109      * - Flags booleani
013300040108     d $Fine           s              1    inz(*off)
013400090421     d $Finef10        s              1    inz(*off)
013500090409     d $Finerec        s              1    inz(*off)
013600040108     d $InzS01         s              1    inz(*on)
013700040130     d $SavRec         s              1    inz(*on)
013800040213     d $PiuRec         s              1    inz(*off)
013900090615     d $no_aggio       s              1    inz(*off)
014000100929     d $FineSfl        s              1    inz(*off)
014100040213      *
014200040213      * - Indici di schiera / Contatori
014300040213     d xx              s              3  0 inz
014400090409     d $contrec        s              3  0 inz
014500090611     d ctn             s              2  0 inz
014600090615     d clt             s              2  0 inz
014700090612     d ps              s              2  0 inz
014800040109      *
014900040109      * - Campi di comodo
015000040108     d w0140           s             14  0
015100040123     d wDataAMG        s              8  0
015200040123     d wDataGMA        s              8  0
015300040120     d wOra            s              6  0
015400090414     d dataiso_cor     s               d   datfmt(*iso)
015500090414     d dataeur_cor     s               d   datfmt(*eur)
015600090416     d dataeur         s               d   datfmt(*eur)
015700090409     d datadmy         s               d   datfmt(*dmy)
015800090408     d kut             s                   like(acokut) inz(1)
015900090408     d w_cpo           s                   like(acolib)
016000090420     d S01nrr_blu      s                   like(s01nrr)   inz
016100090421     d w_mk20flm       s                   like(imk20flm)   inz
016200081023      *
016300081023      * -Campi per il controllo di caratteri non validi
016400081023     d TxtInOut        S           2048
016500081023     d ElencoChar      S            256
016600081023     d TipoElenco      S              1
016700081023     d CharSost        S              1
016800081023     d UpperCase       S              1
016900081023     d ChkNull         S              1    Inz('1')
017000081023     d CharNull        S              1
017100081023     d Esito           S              1
017200090526
017300090528     d dsp_mod         s             10I 0
017400090612     d w20nota         s                   like(emk20no1)
017500081023
017600090402      // ? PROTOTIPI ?
017700100121      /copy gaitrasrc/srcprotopr,tibs02r
017800090402      /copy gaitrasrc/srcprotopr,tibs34r
017900090416      /copy gaitrasrc/srcprotopr,xchkchar
018000090526      /copy gaitrasrc/srcprotopr,QsnQryModS
018100040107
018200040107      *===============================================================*
018300040107      *  RIEPILOGO INDICATORI                                         *
018400040107      *---------------------------------------------------------------*
018500040109      *  01    - Flag modalità DIVERSO da *blanks e da "V"            *
018600040224      *          (né "manutenzione" né "variazione")                  *
018700100121      *  03    - Cliente
018800100121      *  04    - Potenziale
018900040224      *  05    - Flag modalità UGUALE  a  "I" ("interrogazione")      *
019000121109      *  06    - Visualizza dati trattativa (F1)                      *
019100121109      *  07    - Trattativa storicizzata (F1)                         *
019200040224      *  09    - Abilita RolUp nel subfile                            *
019300040109      *  20    - SFLDSP    in C01                                     *
019400040109      * N21    - SFLCLR    in C01                                     *
019500040109      *  21    - SFLDSPCTL in C01                                     *
019600040109      *  23    - SFLEND    in C01                                     *
019700040109      *  25    - Roll Up da subfile                                   *
019800040109      *  28    - Errore generico dspf                                 *
019900040428      *  34    - NON visualizzazione data/ora immissione e stato note *
020000040109      *  35    - Protezione campo note                                *
020100040224      *  36    - Evidenzia note non lette                             *
020200040224      *  41    - Posizionamento cursore su campo note                 *
020300040109      *  90    - Errore                                               *
020400040107      *===============================================================*
020500040107
020600040107     c     *Entry        plist
020700040107     c                   parm                    KPJBA
020800090323     c                   parm                    trmk20ds
020900040123      *
021000040123      * Pulizia campi di output
021100090331     c                   clear                   Omk20piu
021200090331     c                   clear                   Omk20fxx
021300090331     c                   clear                   Omk20err
021400090331     c                   clear                   Omk20msg
021500090331      /free
021600090403         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
021700090610
021800090612       if not %open(ticpn00f);
021900090612          if imk20cmt='1';
022000090612             flgcmt=*on;
022100090612          else;
022200090612             flgcmt=*off;
022300090612          endif;
022400090612          open ticpn00f;
022500090612       endif;
022600100526
022700100526       if  NOT  %open(TFACO00F)   and   %subst(KNSIF : 1 : 3) = 'FIL';
022800100526         open  TFACO00F;
022900100526       endif;
023000090421
023100090421       w_mk20flm=imk20flm;
023200090421
023300090409       //TIPO LANCIO, I65TLA "C" -           CHIUSO CON LR
023400090409       //TIPO LANCIO, I65TLA "L" - ELABORO E CHIUDO CON LR
023500090409       //TIPO LANCIO, I65TLA " " - ELABORO E CHIUDO IN RETRN
023600090331       select;
023700090331          when imk20tla='C';
023800090331             eval *inlr=*on;
023900090402          other;
024000090409       //Reperisco data corrente
024100090423             dataiso_cor=(%date());
024200090423             dataeur_cor=dataiso_cor;
024300090423             wora=%dec(%time());
024400090402             select;
024500100224       //Delete
024600100224                when imk20flm='D';
024700100224                   exsr sr_delete ;
024800090409       //Recupero
024900090402                when imk20flm='R';
025000090402                   exsr sr_recup;
025100090409       //Conferma dati immessi precedentemente nel subfile
025200090402                when imk20flm='C';
025300090402                   exsr sr_updTICPN;
025400090402                other;
025500090409       //Manutenzione dati a video
025600090402                   reset $fine;
025700090615                   if not %open(trmk20d) and not %open(trmk20d4);
025800090528                     dsp_mod = QueryDisplayModeSupport('4':*omit :*omit :*omit);
025900090528                     if dsp_mod=0;
026000090528                        k_sflpag=17;
026100090615                        open trmk20d;
026200090528                     else;
026300090616                        k_sflpag=20;
026400090615                        open trmk20d4;
026500090528                     endif;
026600090525                   endif;
026700090402                   dow $fine=*off;
026800090402                      exsr sr_gess01;
026900090402                   enddo;
027000090409       //Restituisco i dati elaborati al chiamante
027100090402                   if $piurec=*on;
027200090402                      omk20piu='+';
027300090402                   endif;
027400090402                   if omk20fxx=*blanks and imk20flm<>' ' and imk20flm<>'M';
027500090402                      emk20no1=SR1not;
027600090402                      emk20no2=SR2not;
027700090611       //             emk20no3=SR3not;
027800090611       //             emk20no4=SR4not;
027900090402                      emk20dim=SR1dim;
028000090402                      emk20him=SR1him;
028100090402                      emk20pru=SR1pru;
028200090402                      emk20pos=SR1pos;
028300090402                      emk20sns=SR1sns;
028400090402                   endif;
028500090402             endsl;
028600090331       endsl;
028700090331
028800090331       select;
028900090331          when imk20tla='L';
029000090331             eval *inlr=*on;
029100090331          when imk20tla=' ';
029200090331             eval *inrt=*on;
029300090331       endsl;
029400040108
029500090402       //-------------------------------------------------------------*
029600090402       //Routine Iniziale                                             *
029700090402       //-------------------------------------------------------------*
029800090414       begsr *inzsr;
029900090402
030000090402       //Reperisco dati job
030100090402       exsr datijob;
030200090402       vtcpgm=SDSpgm;
030300090402
030400090402       EndSr;
030500040108
030600090402       //-------------------------------------------------------------*
030700090402       //Reperimento Dati del job (Utente/Operativi)                  *
030800090402       //-------------------------------------------------------------*
030900090402       Begsr DatiJob;
031000090403
031100090402          in(e) §azute;
031200090402          if not %error;
031300090402           in(e) §datiute;
031400090402          endif;
031500090402          if %error or rsut = *blanks;
031600090402           tibs34r(tibs34ds);
031700090402           in §azute;
031800090402           in §datiute;
031900090402          endif;
032000090402       endsr;
032100120926
032200100224       //-------------------------------------------------------------*
032300100224       //Deleto le note che mi ha chiesto  il chiamante               *
032400100224       //-------------------------------------------------------------*
032500100224       Begsr SR_delete ;
032600100224
032700100224         cpncpo = imk20cpo ;
032800100224         cpntat = imk20tat ;
032900100224         cpnatn = imk20atn ;
033000100224         cpnatnp= 0 ;
033100100224         setll (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
033200100224         reade (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
033300100224         dow not %eof(ticpn03l);
033400100224             chain cpnnrr ticpn00f ;
033500100224             if %found(ticpn00f) ;
033600100224                delete ticpn00f;
033700100224             endif ;
033800100224             reade (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
033900100224         enddo ;
034000100224
034100100224       endsr;
034200100224
034300090403       //-------------------------------------------------------------*
034400090403       //Recupero note da restituire al chiamante                     *
034500090403       //-------------------------------------------------------------*
034600090403       Begsr SR_recup;
034700090403       //Il recupero delle note lo gestisco solo recuperando per chiave
034800090403       //completa (utilizzo cioè tutti i campi ricevuti dal chiamante e
034900090403       //non faccio estrazioni con chiavi parziali)
035000090406       $finerec=*off;
035100090406       $contrec=1;
035200090403       exsr sr_prepSqlRec;
035300090403       exec sql prepare STRINGASQL from :WrkStringaSql;
035400090403       exec sql declare NOTEcsr cursor for StringaSql;
035500090403       exec sql open NOTEcsr;
035600090414       dow $finerec=*off;
035700090406       exec sql Fetch NOTEcsr into :ticpnds;
035800090406          if sqlcod=100 or sqlcod<0;
035900090406             $finerec = *on;
036000090415             if sqlcod<0;
036100090415                omk20err='E';
036200090415             endif;
036300090406             leave;
036400090406          endif;
036500090406          select;
036600090406          when $contrec=1;
036700090406             emk20no1=cpnnot;
036800090414             emk20dim=cpndim;
036900090414             emk20him=cpnhim;
037000090414             emk20pru=cpnpru;
037100090414             emk20pos=cpnpos;
037200090406          when $contrec=2;
037300090406             emk20no2=cpnnot;
037400090610       // when $contrec=3;
037500090610       //    emk20no3=cpnnot;
037600090610       // when $contrec=4;
037700090610       //    emk20no4=cpnnot;
037800090406          other;
037900090406             omk20piu='+';
038000090406             $finerec=*on;
038100090406          endsl;
038200090406          $contrec=$contrec+1;
038300090406       enddo;
038400090406
038500090403       endsr;
038600040108
038700090403       //-------------------------------------------------------------*
038800090403       //Preparazione stringa sql per recupero note                   *
038900090403       //-------------------------------------------------------------*
039000090403       Begsr SR_prepSqlRec;
039100090403
039200090403       WrkStringaSql='Select * from ticpn00f where';
039300090403
039400090403       if imk20cpo>0;
039500090922          WrkStringaSql=WrkStringaSql+' cpncpo=' + %editc(imk20cpo:'X');
039600090403       endif;
039700090403       if imk20ksc>0;
039800090922          WrkStringaSql=WrkStringaSql+' and cpnksc='+%editc(imk20ksc:'X');
039900090403       endif;
040000090518       if imk20atn>0;
040100100107          IF imk20cpo > 0;
040200090922          WrkStringaSql=WrkStringaSql+' and cpnatn='+%editc(imk20atn:'X');
040300090922          WrkStringaSql=WrkStringaSql+' and cpnatnp='+%editc(imk20atnp:'X');
040400100107          ELSE;
040500100107          WrkStringaSql=WrkStringaSql+' cpnatn=' +%editc(imk20atn:'X') +
040600100107          ' and cpnatnp='+%editc(imk20atnp:'X');
040700100107          ENDIF;
040800090403       endif;
040900090403       if imk20nrv>0;
041000090922          WrkStringaSql=WrkStringaSql+' and cpnnrv='+%editc(imk20nrv:'X');
041100090403       endif;
041200090403
041300110406       WrkStringaSql=WrkStringaSql+' Order by cpndim, cpnhim, cpnpno';
041400090403       WrkStringaSql=WrkStringaSql+' FOR READ ONLY';
041500090403
041600090403       endsr;
041700090407       //-------------------------------------------------------------*
041800090407       //Gestione dati a video                                        *
041900090407       //-------------------------------------------------------------*
042000090407       Begsr SR_Gess01;
042100090407
042200090407       // Controllo se devo inizializzare la videata
042300090407
042400090407       if imk20tla<>xmk20tla
042500090407       or imk20flm<>xmk20flm
042600090407       or imk20cpo<>xmk20cpo
042700090407       or imk20ksc<>xmk20ksc
042800090518       or imk20tat<>xmk20tat
042900090518       or imk20atn<>xmk20atn
043000090518       or imk20atnp<>xmk20atnp
043100090407       or imk20nrv<>xmk20nrv;
043200090407          reset $inzs01;
043300090407          save20ds=trmk20ds;
043400090407       endif;
043500090407
043600090407       // Inizializzo la videata
043700090407
043800090407       Select;
043900090407       when $inzs01=*on;
044000090407          exsr sr_inzs01;
044100090415          $inzs01=*off;
044200090407       when imk20flm=*blanks or imk20flm='M';
044300090407          exsr upds01;
044400090407       endsl;
044500090407
044600090407       if not *in90;
044700090528          if dsp_mod=1;
044800090528             write mk20t40;
044900090528             write mk20z41;
045000090528          else;
045100090528             write mk20t00;
045200090528             write mk20z01;
045300090528          endif;
045400090407       endif;
045500090407
045600090409       // Emetto messaggio in caso di assenza record
045700090528       if dsp_mod=0 and s01nrr=*zeros;
045800090407          write mk20d01;
045900090407       endif;
046000090528       if dsp_mod=1 and s01nrr4=*zeros;
046100090528          write mk20d41;
046200090528       endif;
046300090407
046400090604       // posiziono il cursore nel sfl
046500090604       if v1cpos>0;
046600090604          v1cnrr=v1cpos;
046700090604       else;
046800090604          if dsp_mod=0;
046900090604             v1cnrr=s01nrr;
047000090604          else;
047100090604             v1cnrr=s01nrr4;
047200090604          endif;
047300090604       endif;
047400090407
047500090407       // Emetto la videata
047600090528       if dsp_mod=0;
047700090528          exfmt mk20c01;
047800090528       else;
047900090528          exfmt mk20c41;
048000090528       endif;
048100090921
048200090407       *in28=*off;
048300090407       *in90=*off;
048400090407       clear v1cmsg;
048500090921
048600090407       select;
048700090407       // F3=Fine
048800090407          When *inkc;
048900090407             $fine=*on;
049000090407             $inzs01=*on;
049100090407             omk20fxx='03';
049200090407             leavesr;
049300090407       // F12=Ritorno
049400090414          When *inkl;
049500090407             $fine=*on;
049600090407             $inzs01=*on;
049700090407             omk20fxx='12';
049800090407             leavesr;
049900090416       // F1=Informazioni aggiuntive
050000090416          When *inka;
050100090416          exsr sr_f1;
050200090921       // F6=Conferma
050300090921          When *inkf;
050400090921          exsr sr_f6;
050500090421       // F10=Inserimento
050600090421          When *inkj;
050700090421             exsr sr_f10;
050800090421             if omk20fxx<>'12';
050900090421                $fine=*on;
051000090421                $inzs01=*on;
051100090421                leavesr;
051200090421             endif;
051300090407       // Rollup
051400090407          When *in25;
051500090407       //    Caricamento righe vuote
051600120926             if imk20flm<>'I' and IMK20flm <> 'S';
051700090616                if dsp_mod=0;
051800090616                   v1cpos=di_sflnrr;
051900090616                else;
052000090616                   v1cpos=dsp4ds.di_sflnrr;
052100090616                endif;
052200090407                exsr sr_freerow;
052300090407             endif;
052400101007       //    Rollup recupero le note già scritte
052500101007             If Imk20flm = 'M' ;
052600101007                exsr sr_reps01;
052700101007             Endif ;
052800090407       endsl;
052900090407       // Enter=Prosegui (se manutenzione/immissione - Modalità " "/"M"
053000090616       if dsp_mod=1;
053100090616          di_tasto=dsp4ds.di_tasto;
053200090616       endif;
053300090407       if imk20flm= *blanks or imk20flm='M' and di_tasto=K_enter;
053400090407         exsr sr_reps01;
053500090921         if imk20flm <> ' ';
053600090407         $fine=*on;
053700090921         endif;
053800090407       endif;
053900090407       endsr;
054000090407       //-------------------------------------------------------------*
054100090407       //Inizializzazione subfile                                     *
054200090407       //-------------------------------------------------------------*
054300090407       Begsr SR_InzS01;
054400090921
054500090407       // Preparazione dati di testata
054600090407       select;
054700090921        when w_mk20flm='M' or w_mk20flm=' ';
054800090407          vtcmod=K_tit1;
054900090921        when w_mk20flm='I';
055000090407          vtcmod=K_tit3;
055100090407       endsl;
055200090921
055300100512       *in01=(W_mk20flm<>'V' and W_mk20flm<>' ');
055400120926       *in05=(w_mk20flm='I' or w_mk20flm = 'S');
055500090423       *in08=(w_mk20flm='I' and imk20f10='S');
055600100121
055700100121       *in03 = *off;
055800100121       *in04 = *off;
055900090921
056000090407       if imk20cpo>0;
056100100121          *in04=*on;
056200090409          v1ccpo=imk20cpo;
056300090420          if imk20rsc<>*blanks;
056400090417             v1cdpo=imk20rsc;
056500090417          else;
056600090417             chain imk20cpo tncpo01l;
056700090417             if %found(tncpo01l);
056800090417                v1cdpo=cporag;
056900090417             endif;
057000090417          endif;
057100090407       else;
057200100121         if imk20ksc > 0;
057300100121          *in03=*on;
057400090407          v1cksc=imk20ksc;
057500090420          if imk20rsc<>*blanks;
057600090420             v1cdks=imk20rsc;
057700090420          else;
057800090420             chain (kut:dutkci:imk20ksc) cnaco00f;
057900090420             if %found(cnaco00f);
058000090420                v1cdks=acorag;
058100090420             endif;
058200090420          endif;
058300100121         endif;
058400090407       endif;
058500090921
058600090407       // Pulizia subfile
058700090407       *in20=*off;
058800090407       *in21=*off;
058900090528       if dsp_mod=0;
059000090528          write mk20c01;
059100090528       else;
059200090528          write mk20c41;
059300090528       endif;
059400090407       *in20=*on;
059500090407       *in21=*on;
059600090408
059700090407       clear riri;
059800090407       clear coco;
059900090407       clear V1cnrr;
060000090407       clear V1cpos;
060100090407       clear s01nrr;
060200090528       clear s01nrr4;
060300090420       clear s01nrr_blu;
060400090407       clear v1cmsg;
060500090407       *in28=*off;
060600090407       *in90=*off;
060700090407       *in41=*off;
060800090407       reset $SavRec;
060900090409       reset $PiuRec;
061000090407       reset SavRec1;
061100090407       reset SavRec2;
061200090611       //reset SavRec3;
061300090611       //reset SavRec4;
061400090407       reset wNrr1Free;
061500090511       reset wNrr1new;
061600090409       reset savpno;
061700090407       savflm=imk20flm;
061800090921
061900090407       // Caricamento dati nel sfl
062000090407       exsr Ries01;
062100090921
062200090407       endsr;
062300090921
062400090407       //-------------------------------------------------------------*
062500090407       //Caricamento sfl                                              *
062600090407       //-------------------------------------------------------------*
062700090407       Begsr RieS01;
062800090921
062900090408       // Recupero il codice del potenziale da TFACO/CNACO, se non passato
063000090408       clear w_cpo;
063100090408       clear savcpo;
063200090408       clear savksc;
063300090518       clear savtat;
063400090408       clear savdim;
063500090408       clear savhim;
063600090921
063700090408       if imk20cpo=0;
063800090408          select;
063900090408          when imk20ksc>0;
064000090408             chain (kut:dutkci:imk20ksc) cnaco00f;
064100090408          when imk20nrv>0;
064200090408             chain (kut:dutkci:imk20nrv) tfaco00f;
064300090408          endsl;
064400090416          if %found;
064500090416             w_cpo=acolib;
064600090416          endif;
064700090408       else;
064800090408          w_cpo=imk20cpo;
064900090408       endif;
065000090416
065100090921       // ---------- if w_mk20flm<>' ';
065200090408       // Con Sql estraggo tutti i record del potenziale ordinandoli
065300090408       // per codice potenziale, data e ora immissione
065400090421          $finerec=*off;
065500090421          exsr sr_prepSqls01;
065600090421          exec sql prepare STRINGASQL1 from :WrkStringaSql;
065700090421          exec sql declare NOTEcsr1 cursor for StringaSql1;
065800090421          exec sql open NOTEcsr1;
065900090421          dow $finerec=*off;
066000090421             exec sql Fetch NOTEcsr1 into :ticpnds;
066100090421             if sqlcod=100 or sqlcod<0;
066200090421                $finerec = *on;
066300090421                if sqlcod<0;
066400090421                   omk20err='E';
066500090421                endif;
066600090421                leave;
066700090421             endif;
066800090921          // Imposto potenziale/cliente/tipo contatto/data e ora immissione
066900090921          // solo a rottura
067000090518             if cpncpo<>savcpo or cpnksc<>savksc or cpntat<>savtat
067100090421             or cpndim<>savdim or cpnhim<>savhim;
067200090421                if cpncpo>0;
067300090421                   v1scpo=%editc(cpncpo:'X');
067400090421                endif;
067500090421                if cpnksc>0;
067600090421                   v1sksc=%editc(cpnksc:'X');
067700091027                else;
067800091027                clear v1sksc;
067900090421                endif;
068000090518                v1stat=cpntat;
068100090421                select;
068200090518                when cpntat='A';
068300090528       //          v1stat='App';
068400090518                   *in39=*on;
068500090518                when cpntat='T';
068600090528       //          v1stat='Tel';
068700090518                   *in38=*on;
068800090421                endsl;
068900090421                datadmy=%date(cpndim:*iso);
069000090421                v1sdim=%dec(datadmy);
069100090421                v1shim=%dec(%subst(%editc(cpnhim:'X'):1:4):4:0);
069200090422                v1ssns=cpnsns;
069300090422                clear h1sdim;
069400090422                clear h1shim;
069500090422                clear h1scpo;
069600090422                clear h1sksc;
069700090422                clear h1snrv;
069800090518                clear h1stat;
069900090518                clear h1satn;
070000090518                clear h1satnp;
070100090422                clear h1spos;
070200090422                clear h1spru;
070300090422                clear h1ssns;
070400090408
070500090421                savcpo=cpncpo;
070600090421                savksc=cpnksc;
070700090518                savtat=cpntat;
070800090421                savdim=cpndim;
070900090421                savhim=cpnhim;
071000090421             else;
071100090421                clear v1scpo;
071200090421                clear v1sksc;
071300090421                clear v1sdim;
071400090421                clear v1shim;
071500090518                clear v1stat;
071600090518                *in38='0';
071700090518                *in39='0';
071800090422                clear v1ssns;
071900090421             endif;
072000090421             h1sdim=cpndim;
072100090421             h1shim=%dec(%subst(%editc(cpnhim:'X'):1:4):4:0);
072200090416             h1scpo=cpncpo;
072300090416             h1sksc=cpnksc;
072400090416             h1snrv=cpnnrv;
072500090518             h1stat=cpntat;
072600090518             h1satn=cpnatn;
072700090518             h1satnp=cpnatnp;
072800090417             h1spos=cpnpos;
072900090417             h1spru=cpnpru;
073000090422             h1ssns=cpnsns;
073100090615       // Note
073200090528          if dsp_mod=0;
073300090409             v1snot=cpnnot;
073400090528          else;
073500090528             v1snt4=cpnnot;
073600090528          endif;
073700090409       // Impostazione indicatori
073800090409       // - protezione
073900090409             *in35=*on;
074000090421             h1in35=*in35;
074100090420       // Coloro note in blu se relative alla chiave passata dal chiammnte
074200090518             if imk20nrv>0 and cpnnrv=imk20nrv or (imk20atn>0 and
074300090518             imk20tat=cpntat and imk20atn=cpnatn and imk20atnp=cpnatnp);
074400090421                *in37=*on;
074500090421             else;
074600090421                *in37=*off;
074700090421             endif;
074800090615             h1in37=*in37;
074900090409       // - posizionamento cursore
075000090409       //   sul 1° campo "note" vuoto e non protetto
075100090420       //   oppure in interrogazione sulla nota evidenziata in base al tipo chiamata
075200090528          if dsp_mod=0;
075300090421             *in41=(*in35=*off and v1snot=*blanks);
075400090528          else;
075500090528             *in41=(*in35=*off and v1snt4=*blanks);
075600090528          endif;
075700120926             *in41=(*in37 and imk20flm='I' and IMK20flm = 'S');
075800090420
075900090421             s01nrr=s01nrr+1;
076000090528             s01nrr4=s01nrr4+1;
076100090421             if *in37;
076200120926                if (w_mk20flm<>'I' and w_mk20flm <> 'S') or v1cpos=0;
076300090421                   v1cpos=s01nrr;
076400090528                   v1cpos=s01nrr4;
076500090421                endif;
076600090421             endif;
076700090420       // Se chiamata per codice cliente memorizzo nrr se nota del cliente
076800090421       // Mi serve per impostare il colore sulla nota più recente
076900090420          if imk20ksc>0 and cpnksc=imk20ksc and v1sksc>*zeros;
077000120926             if (w_mk20flm<>'I' and w_mk20flm <> 'S') or s01nrr_blu=0;
077100090528              if dsp_mod=0;
077200090421                s01nrr_blu=s01nrr;
077300090528              else;
077400090528                s01nrr_blu=s01nrr4;
077500090528              endif;
077600090421             endif;
077700090420          endif;
077800090409
077900090409       // Scrittura record sfl
078000090528          if dsp_mod=0;
078100090528             write mk20s01;
078200090612                clear v1scpo;
078300090612                clear v1sksc;
078400090612                clear v1sdim;
078500090612                clear v1shim;
078600090612                clear v1stat;
078700090612                *in38='0';
078800090612                *in39='0';
078900090612                clear v1ssns;
079000090611             v1snot=%subst(cpnnot: 31 :30);
079100090611             s01nrr=s01nrr+1;
079200090611             write mk20s01;
079300090611             v1snot=%subst(cpnnot: 61 :30);
079400090611             s01nrr=s01nrr+1;
079500090611             write mk20s01;
079600090528          else;
079700090528             write mk20s41;
079800090528          endif;
079900090409
080000090611       // - memorizzazione dei 1' 2 rec. da restituire al pgm chiamante
080100090409          if $savrec=*on;
080200090409             select;
080300090409w   4        when  SR1cpo  = *zeros;
080400090409                savrec1=ticpnds;
080500090409w   4        when  SR2cpo  = *zeros;
080600090409                savrec2=ticpnds;
080700090611                $savrec=*off;
080800090409             endsl;
080900090409          else;
081000090409             $piurec=*on;
081100090409          endif;
081200090421          enddo;
081300090921       // ---------------- endif;
081400090921
081500090409       // Memorizzo l'ultimo n° progressivo note già registrato
081600090409       //  (se non in variazione)
081700090921       // ---------------- if w_mk20flm<>*blanks;
081800090409          savpno=cpnpno;
081900090921       // ---------------- endif;
082000090409       // Memorizzo il nrr del primo record vuoto (inseribile)
082100090528       if dsp_mod=0;
082200090611          wNrr1Free=s01nrr+1;
082300090528       else;
082400090528          wNrr1Free=s01nrr4+1;
082500090528       endif;
082600090421       // aggiornamento colore blu sulle note più recenti del cliente
082700090420       if s01nrr_blu >0;
082800090604          if dsp_mod=0;
082900090604            chain s01nrr_blu mk20s01;
083000090604          else;
083100090604            chain s01nrr_blu mk20s41;
083200090604          endif;
083300090615          dow h1sksc=imk20ksc and %found;
083400090604             if v1cpos>0 and v1sksc>*zeros;
083500090604                leave;
083600090604             endif;
083700090420             *in37=*on;
083800090420             h1in37=*in37;
083900090604             if dsp_mod=0;
084000090604                if v1cpos=0;
084100090604                   v1cpos=s01nrr;
084200090604                endif;
084300090528                update mk20s01;
084400090604             else;
084500090604                if v1cpos=0;
084600090604                   v1cpos=s01nrr4;
084700090604                endif;
084800090528                update mk20s41;
084900090604             endif;
085000090420             s01nrr_blu+=1;
085100090604             if dsp_mod=0;
085200090604                chain s01nrr_blu mk20s01;
085300090604             else;
085400090604                chain s01nrr_blu mk20s41;
085500090604             endif;
085600090420          enddo;
085700091027        if dsp_mod=0;
085800091027          s01nrr = wNrr1Free -1 ;
085900091027        else;
086000091027          s01nrr4 = wNrr1Free -1;
086100091027        endif;
086200090420       endif;
086300090921
086400090409       // Caricamento righe vuote (se non interrogazione)
086500120926       if w_mk20flm<>'I' and w_mk20flm <> 'S';
086600090528        if dsp_mod=0;
086700090409          wnrr=s01nrr;
086800090528        else;
086900090528          wnrr=s01nrr4;
087000090528        endif;
087100090409          exsr sr_FreeRow;
087200111018          If wnrr1new < 3 ;
087300111018             exsr sr_FreeRow;
087400111018          Endif ;
087500090409       else;
087600090528         if dsp_mod=0;
087700090409          *in09=s01nrr>k_sflpag;
087800090528         else;
087900090528          *in09=s01nrr4>k_sflpag;
088000090528         endif;
088100090409          *in23=*on;
088200090409       endif;
088300090921
088400090528       if dsp_mod=0;
088500090528          *in20=s01nrr>*zeros;
088600090528       else;
088700090528          *in20=s01nrr4>*zeros;
088800090528       endif;
088900090921
089000090420       if v1cpos=0;
089100090420          v1cpos=1;
089200090420       endif;
089300100614
089400100614       //?Se interrogazione e riga posizionamento = 1 imposto ultimo
089500100614       //?record caricato come riga di posizionamento
089600100614       //?se non è prima pagina imposto costante 'precedente...'
089700120926       IF  V1Cpos = 1 and w_mk20flm = 'I' and w_mk20flm = 'S';
089800100614         IF dsp_mod = 0;
089900100614          V1Cpos = S01nrr;
090000100614         ELSE;
090100100614          V1Cpos = S01nrr4;
090200100614         ENDIF;
090300100614         IF  V1Cpos > 1 and V1Cpos >= (k_sflpag + 1);
090400100614         ENDIF;
090500100614       ENDIF;
090600090409
090700090407       endsr;
090800090408       //-------------------------------------------------------------*
090900090408       //Preparazione stringa sql per caricamento note nel subfile    *
091000090408       //-------------------------------------------------------------*
091100090408       Begsr SR_prepSqls01;
091200090408
091300090420       select;
091400090420       when imk20ksc>0;
091500090420       // Entrata per cliente: seleziono solo records del cliente passato
091600090420       // o del suo potenziale ma non di altri clienti.
091700090420         WrkStringaSql='Select * from ticpn00f where cpnksc=';
091800090420         WrkStringaSql=WrkStringaSql + %editc(imk20ksc:'4');
091900090420         WrkStringaSql=WrkStringaSql + ' or (cpncpo=';
092000090420         WrkStringaSql=WrkStringaSql + %editc(w_cpo:'4');
092100090420         WrkStringaSql=WrkStringaSql + ' and cpnksc=0)';
092200100121       When imk20cpo > 0;
092300090420       // Entrata per potenziale:seleziono tutti i record del potenziale
092400090420       // passato
092500090420         WrkStringaSql='Select * from ticpn00f where cpncpo=';
092600090420         WrkStringaSql=WrkStringaSql + %editc(w_cpo:'4');
092700120926         //?Se richiesta la visualizzazione dello storico ed ho la trattativa
092800120926         //?seleziono solo la trattativa
092900120926         IF imk20nrv > 0 and IMK20flm = 'S';
093000120926           WrkStringaSql += ' and CPNnrv = ' + %editc(IMK20nrv:'X');
093100120926         ENDIF;
093200100121       when imk20atn > 0;
093300100121         WrkStringaSql='Select * from ticpn00f where cpnatn=';
093400100121         WrkStringaSql=WrkStringaSql + %editc(imk20atn:'X');
093500090420       endsl;
093600090421       // Ordinamento -> decrescente in interrogazione; crescente in manutenzione
093700100614       //?Sempre crescente per data come in manutenzione (ND - 14/06/2010)
093800100614       //if imk20flm='I';
093900100614       //   if imk20ksc=0;
094000100614       //     WrkStringaSql=WrkStringaSql+' Order by cpndim desc,cpnhim desc,';
094100100614       //     WrkStringaSql=WrkStringaSql+'cpnpru,cpnksc,cpnpno';
094200100614       //   else;
094300100614       //     WrkStringaSql=WrkStringaSql+' Order by cpndim desc,cpnhim desc,';
094400100614       //     WrkStringaSql=WrkStringaSql+'cpnpru,cpnpno';
094500100614       //   endif;
094600100614       //else;
094700090421          if imk20ksc=0;
094800090421            WrkStringaSql=WrkStringaSql+' Order by cpndim,cpnhim,cpnpru,cpnksc';
094900110407        //  WrkStringaSql=WrkStringaSql+' ,cpnpno';
095000110407            WrkStringaSql=WrkStringaSql+' ,cpnatn,cpnatnp,cpnpno';
095100090421          else;
095200110407       //   WrkStringaSql=WrkStringaSql+' Order by cpndim,cpnhim,cpnpru,cpnpno';
095300110407            WrkStringaSql=WrkStringaSql+' Order by cpndim,cpnhim,cpnpru';
095400110407            WrkStringaSql=WrkStringaSql+' ,cpnatn,cpnatnp,cpnpno';
095500090421          endif;
095600100614       //endif;
095700090408       WrkStringaSql=WrkStringaSql+' FOR READ ONLY';
095800090408
095900090408       endsr;
096000090921
096100090409       //-------------------------------------------------------------*
096200090409       //Riempimento sfl con righe vuote                              *
096300090409       //-------------------------------------------------------------*
096400090409       Begsr SR_FreeRow;
096500090921
096600090409       *in09=*on;
096700090528       if dsp_mod=0;
096800090528         s01nrr=wnrr;
096900090528       else;
097000090528         s01nrr4=wnrr;
097100090528       endif;
097200090409
097300111012         Wrig=%rem(wnrr:K_sflpag)+1;
097400090528        for wrig to k_sflpag;
097500090409          s01nrr=s01nrr+1;
097600090528          s01nrr4=s01nrr4+1;
097700090409          clear mk20s01;
097800090528          clear mk20s41;
097900090409          h1in35=*off;
098000090409          h1in36=*off;
098100090420          h1in37=*off;
098200090518          *in38='0';
098300090518          *in39='0';
098400090528          if (s01nrr=wNrr1Free and dsp_mod=0) or (s01nrr4=wnrr1Free and
098500090528          dsp_mod=1);
098600090415             if imk20cpo>0;
098700090415                v1scpo=%editc(imk20cpo:'X');
098800090415             endif;
098900090415             if imk20ksc>0;
099000090415                v1sksc=%editc(imk20ksc:'X');
099100090415             endif;
099200090422             v1ssns='S';
099300090518             v1stat=imk20tat;
099400090422             select;
099500090518             when v1stat='A';
099600090528       //       v1stat='App';
099700090518                *in39=*on;
099800090518             when v1stat='T';
099900090528       //       v1stat='Tel';
100000090518                *in38=*on;
100100090422             endsl;
100200090417             clear v1sdim;
100300090417             clear v1shim;
100400090409          endif;
100500090422          clear h1sdim;
100600090422          clear h1shim;
100700090422          h1scpo=w_cpo;
100800090422          h1sksc=imk20ksc;
100900090422          h1snrv=imk20nrv;
101000090518          h1stat=imk20tat;
101100090518          h1satn=imk20atn;
101200090518          h1satnp=imk20atnp;
101300090422          h1ssns='S';
101400090528            *in23= (wnrr=k_sflpag);
101500090611          if wnrr1new<3 and w_mk20flm<>' ';
101600090611             select;
101700090611             when wnrr1new=1;
101800090611              if dsp_mod=0;
101900090611                v1snot=emk20no1;
102000090611                w20nota=emk20no1;
102100090611              else;
102200090611                v1snt4=emk20no1;
102300090611              endif;
102400090611             when wnrr1new=2;
102500090611              if dsp_mod=0;
102600090611                v1snot=emk20no2;
102700090612                w20nota=emk20no2;
102800090611              else;
102900090611                v1snt4=emk20no2;
103000090611              endif;
103100090611             endsl;
103200090611          endif;
103300090528          if dsp_mod=0;
103400090528            *in41=(v1snot=*blanks);
103500090528            write mk20s01;
103600090611          if wnrr1new<3 and w_mk20flm<>' ';
103700090612            clear v1scpo;
103800090612            clear v1sksc;
103900090612            clear v1ssns;
104000090612            clear v1stat;
104100090611            v1snot=%subst(w20nota:31:30);
104200090611            s01nrr=s01nrr+1;
104300090611            write mk20s01;
104400090611            v1snot=%subst(w20nota:61:30);
104500090611            s01nrr=s01nrr+1;
104600090611            write mk20s01;
104700090611          endif;
104800090528          else;
104900090528            *in41=(v1snt4=*blanks);
105000090528            write mk20s41;
105100090528          endif;
105200090409          wnrr=wnrr+1;
105300090511          wnrr1new=wnrr1new+1;
105400090528        endfor;
105500111018
105600090528         v1cpos=s01nrr-k_sflpag+1;
105700090528       if dsp_mod=0;
105800090528         wnrr=s01nrr;
105900090528       else;
106000090528         wnrr=s01nrr4;
106100090528       endif;
106200090409
106300090409       endsr;
106400090409       //-------------------------------------------------------------*
106500090611       //Aggiornamento prime note del subfile con le note ricevute    *
106600090409       //-------------------------------------------------------------*
106700090409       Begsr Upds01;
106800090415       $nrr_agg=1;
106900090611       ps=1;
107000090415       for $nrr=wnrr1free to wnrr;
107100090528        if dsp_mod=0;
107200090409          chain $nrr mk20s01;
107300090528        else;
107400090528          chain $nrr mk20s41;
107500090528        endif;
107600090615          if %found and h1in35=*off;
107700090409             select;
107800090415             when $nrr_agg = 1;
107900090611                v1snot=%subst(emk20no1:ps:30);
108000090528                v1snt4=emk20no1;
108100090612              if dsp_mod=0;
108200090611                if ps=61;
108300090611                   $nrr_agg+=1;
108400090611                   ps=1;
108500090611                else;
108600090611                   ps=ps+30;
108700090611                endif;
108800090611              else;
108900090611                   $nrr_agg+=1;
109000090611              endif;
109100090415             when $nrr_agg = 2;
109200090611                v1snot=%subst(emk20no2:ps:30);
109300090528                v1snt4=emk20no2;
109400090612              if dsp_mod=0;
109500090611                if ps=61;
109600090611                   $nrr_agg+=1;
109700090611                   ps=1;
109800090611                else;
109900090611                   ps=ps+30;
110000090611                endif;
110100090611              else;
110200090611                   $nrr_agg+=1;
110300090611              endif;
110400090409             endsl;
110500090409       // Indicatori
110600090409       // - protezione
110700090409             *in35=h1in35;
110800090409       // - alta intensità
110900090409             *in36=h1in36;
111000090616       // - Colore
111100090420             *in37=h1in37;
111200090616             *in38=*off;
111300090616             *in39=*off;
111400090616             select;
111500090616             when v1stat='A';
111600090616                *in39=*on;
111700090616             when v1stat='T';
111800090616                *in38=*on;
111900100928             endsl;
112000090409       // - posizionamento cursore
112100090409       //   sul 1° campo vuoto e non protetto
112200090528             if dsp_mod=0;
112300090528                *in41=(*in35=*off) and (v1snot=*blanks);
112400090409       // - aggiornamento record nel subfile
112500090528                update mk20s01;
112600090528             else;
112700090528                *in41=(*in35=*off) and (v1snt4=*blanks);
112800090528       // - aggiornamento record nel subfile
112900090528                update mk20s41;
113000090528             endif;
113100090409          endif;
113200090409       endfor;
113300090409       endsr;
113400040108
113500090409       //-------------------------------------------------------------*
113600090612       //Reperimento prime righe del subfile con le note              *
113700090409       //-------------------------------------------------------------*
113800090409       Begsr Sr_reps01;
113900090921
114000090415       $nrr_agg=1;
114100090612       ctn=1;
114200090612       clear nota90;
114300090415       reset $piurec;
114400090921
114500090415       for $nrr=wnrr1free to wnrr;
114600090528          if dsp_mod=0;
114700090528             chain $nrr mk20s01;
114800090528          else;
114900090528             chain $nrr mk20s41;
115000090528          endif;
115100090615          if %found and h1in35=*off;
115200090415             select;
115300090415             when $nrr_agg = 1;
115400090528                if dsp_mod=0;
115500090612                 if ctn<4;
115600090612                    nota(ctn)=v1snot;
115700090612                    ctn+=1;
115800090612                 endif;
115900090612                 if ctn=4;
116000090612                    emk20no1=nota90;
116100090612                    $nrr_agg+=1;
116200090612                    ctn=1;
116300090612                 endif;
116400090528                else;
116500090528                   emk20no1=v1snt4;
116600090612                   $nrr_agg+=1;
116700090528                endif;
116800090416                emk20diM=h1sdim;
116900090417                emk20him=h1shim;
117000090415             when $nrr_agg = 2;
117100090528                if dsp_mod=0;
117200090612                 if ctn<4;
117300090612                    nota(ctn)=v1snot;
117400090612                    ctn+=1;
117500090612                 endif;
117600090612                 if ctn=4;
117700090612                    emk20no2=nota90;
117800090612                    $nrr_agg+=1;
117900090612                    ctn=1;
118000090612                 endif;
118100090528                else;
118200090528                   emk20no2=v1snt4;
118300090612                   $nrr_agg+=1;
118400090528                endif;
118500090612             when $nrr_agg > 2 and ((dsp_mod=0 and v1snot<>*blanks) or
118600090528                  (dsp_mod=1 and v1snt4<>*blanks));
118700090415                $PiuRec=*on;
118800090415             endsl;
118900090415       // Indicatori
119000090415       // - protezione
119100090415             *in35=h1in35;
119200090415       // - alta intensità
119300090415             *in36=h1in36;
119400090420       // - colore blu
119500090420             *in37=h1in37;
119600090415       // - posizionamento cursore
119700090415       //   sul 1° campo vuoto e non protetto
119800090528             if dsp_mod=0;
119900090528               *in41=(*in35=*off) and (v1snot=*blanks);
120000090415       // - aggiornamento record nel subfile
120100090528               update mk20s01;
120200090528             else;
120300090528               *in41=(*in35=*off) and (v1snt4=*blanks);
120400090528       // - aggiornamento record nel subfile
120500090528               update mk20s41;
120600090528             endif;
120700090415          endif;
120800090415          if $piurec=*on;
120900090415             leave;
121000090415          endif;
121100090415       endfor;
121200090409       endsr;
121300090409       //-------------------------------------------------------------*
121400090409       //                                                             *
121500090409       //-------------------------------------------------------------*
121600090409       Begsr Sr_UpdTICPN;
121700090415       clear ticpn000;
121800090416       cpnpno=0;
121900090611       nota90=*blanks;
122000090612       clear ctn;
122100090615       clear clt;
122200100929       $FineSfl = *off;
122300090424       // Se ho il subfile caricato scrivo in base a contenuto sfl
122400090424       if wnrr1free>0;
122500100928       // Se ho il subfile caricato prima aggiorno le prime 2 righe libere
122600100928          if imk20flm='C' and imk20fno = 'S' ;
122700100928             exsr Upds01 ;
122800100928          endif ;
122900090424          for $nrr=wnrr1free to wnrr;
123000090528             if dsp_mod=0;
123100090528                chain $nrr mk20s01;
123200090528             else;
123300090528                chain $nrr mk20s41;
123400090528             endif;
123500090615             if %found and ((dsp_mod=0 and v1snot<>*blanks) or
123600090615                            (dsp_mod=1 and v1snt4<>*blanks));
123700090615                $no_aggio=*off;
123800090615                clt+=1;
123900100929                // Richiamato con "C"-Conferma dati precedentemente immessi
124000100929                if imk20flm='C';
124100100929                 // Se richiesto, le prime due note le prendo dai parametri
124200100929                 // ricevuti in input
124300100929                   if imk20fno='S';
124400100929                 // Se devo utilizzare le prime due note ricevute in input
124500100929                 // memorizzo di non utilizzare le prime sei note del subfile
124600100929                 // in modalità 24x80
124700100929                      if dsp_mod=0 and clt<=6 and cpnpno >=2;
124800100929                         $no_aggio=*on;
124900100929                      endif;
125000100929                   endif;
125100100929                 // Telefonata e numero visita li prendo dai parametri ricevuti
125200100929                 // se nel sfl sono vuoti
125300100929                   if h1satn=*zeros;
125400100929                      h1stat=imk20tat;
125500100929                      h1satn=imk20atn;
125600100929                      h1satnp=imk20atnp;
125700100929                   endif;
125800100929                   if h1snrv=0;
125900100929                      h1snrv=imk20nrv;
126000100929                   endif;
126100100929                endif;
126200090615                if $no_aggio=*off;
126300090615                   exsr  rie_ticpn;
126400090615                endif;
126500090424             endif;
126600090424          endfor;
126700100929          $FineSfl = *on ;
126800090615       // Devo richiamare routine di scrittura note se in modalità 24x80
126900100929       // ed è rimasta sottostringa di nota lunga 90
127000090615          if dsp_mod=0 and nota90<>*blanks;
127100090615             exsr  rie_ticpn;
127200100929             $FineSfl = *off;
127300090615          endif;
127400090424       else;
127500090424       // Se non ho il sfl caricato scrivo in base ai parametri ricevuti
127600090424          h1scpo=imk20cpo;
127700090424          h1sksc=imk20ksc;
127800090518          h1stat=imk20tat;
127900090518          h1satn=imk20atn;
128000090518          h1satnp=imk20atnp;
128100090424          h1snrv=imk20nrv;
128200090424          h1ssns='S';
128300090424          clear v1snot;
128400090611          clear v1snt4;
128500090611          for xx=1 to 2;
128600090424             select;
128700090424             when xx=1;
128800090611                nota90=emk20no1;
128900090612                ctn=3;
129000090528                v1snt4=emk20no1;
129100090424             when xx=2;
129200090611                nota90=emk20no2;
129300090612                ctn=3;
129400090528                v1snt4=emk20no2;
129500090424             endsl;
129600090611             if (dsp_mod=0 and nota90<>*blanks) or
129700090528                (dsp_mod=1 and v1snt4<>*blanks);
129800090424                exsr  rie_ticpn;
129900090424             endif;
130000090424          endfor;
130100090424       endif;
130200090415       reset  $inzs01;
130300090415       reset save20ds;
130400090415       clear savflm;
130500090409       endsr;
130600090424       //-------------------------------------------------------------*
130700090424       //                                                             *
130800090424       //-------------------------------------------------------------*
130900090424       Begsr Rie_Ticpn;
131000090424
131100090424          cpncpo=h1scpo;
131200090424          CPNKSC=h1sksc;
131300090518          CPNTat=h1stat;
131400090518          CPNatn=h1satn;
131500090518          CPNatnP=h1satnp;
131600090424          CPNNRV=h1snrv;
131700090424       // Data ora immissione --> aggiorno con quella ricevuta dal chiamante se
131800090424       //                         presente, altrimenti con data/ora corrente
131900100503          // if emk20dim>0;
132000100503          // CPNDIM=emk20dim;
132100100503          // CPNHIM=emk20him;
132200100503          //  else;
132300090424             CPNDIM=%dec(dataiso_cor);
132400090424             CPNHIM=Wora;
132500100503          //  endif;
132600090424          CPNSNS=h1ssns;
132700090424          CPNPOS=dutpou;
132800090424          CPNPRU=knmus;
132900090611          if dsp_mod=0;
133000090611             if ctn<3;
133100090611                ctn+=1;
133200090611                nota(ctn)=v1snot;
133300090612             endif;
133400100929             if ctn=3 or $fineSfl = *on ;
133500090611                cpnnot=nota90;
133600090611                exsr sr_chk;
133700090612                CPNPNO+=1;
133800090611                write ticpn000;
133900090612                clear ctn;
134000090615                clear nota90;
134100090611             endif;
134200090610          else;
134300090615             cpnnot=v1snt4;
134400090615       //    campo note:controllo che non contenga caratteri particolari
134500090615             exsr sr_chk;
134600090612             CPNPNO+=1;
134700090610             write ticpn000;
134800090610          endif;
134900090424          endsr;
135000090416       //-------------------------------------------------------------*
135100090416       //                                                             *
135200090416       //-------------------------------------------------------------*
135300090416       Begsr Sr_Chk;
135400090416       clear TxtInOut;
135500090416       TxtInOut=cpnnot;
135600090416       xChkChar(TxtInOut:ElencoChar:TipoElenco:CharSost:Uppercase:chknull:
135700090416       CharNull:Esito);
135800090416       if esito='1';
135900090416          cpnnot=TxtInOut;
136000090416       endif;
136100090416       endsr;
136200090416       //-------------------------------------------------------------*
136300090416       // Gestione tasto funzione F1=Informazioni aggiuntive          *
136400090416       //-------------------------------------------------------------*
136500090416       Begsr Sr_f1;
136600090616       if dsp_mod=0;
136700090616          R$$=di_NrRg;
136800090616          C$$=di_NrCl;
136900090616       else;
137000090616          R$$=dsp4ds.di_NrRg;
137100090616          C$$=dsp4ds.di_NrCl;
137200090616       endif;
137300090416       cur_R=riri;
137400090416       cur_C=coco;
137500090422       clear mk20w01;
137600090528       clear mk20w41;
137700090416       exsr RepNrrSfl;
137800090416       if $nrr > 0 and $nrr<wnrr1free;
137900090528          if dsp_mod=0;
138000090528             chain $nrr mk20s01;
138100090528          else;
138200090528             chain $nrr mk20s41;
138300090528          endif;
138400090417          if h1spos>0;
138500090416             if  cur_r<=12;
138600090416                posrig=13;
138700090416             else;
138800090416                posrig=6;
138900090416             endif;
139000100121
139100100121          // Compongo la riga con descrizione tipo attività + data + ora
139200090518             if h1stat<>*blanks;
139300100121
139400100121                  clear  dTTA;
139500100121                  clear  TIBS02ds;
139600100121                  T02mod = 'C';
139700100121                  T02cod = 'TTA';
139800100121                  T02ke1 = h1stat;
139900100121                  T02sif = KNSIF;
140000100121                  TNTBE_RicercaControllo  (kpjba : tibs02ds);
140100100121                  if  T02err = *blanks;
140200100121                      dtta = t02uni ;
140300100121                  endif;
140400100121
140500100121                W1cdes = §ttades;
140600100121                W1cdes = %trim(W1cdes) + ' del' + ' ' + hi +
140700100121                         %subst(%editc(h1sdim:'X'):7:2) + '/' +
140800100121                         %subst(%editc(h1sdim:'X'):5:2) + '/' +
140900100121                         %subst(%editc(h1sdim:'X'):1:4) + '  ' +
141000100121                         %subst(%editc(h1shim:'X'):1:2) + ':' +
141100100121                         %subst(%editc(h1shim:'X'):3:2);
141200090416             endif;
141300090416             if h1snrv>0;
141400090416                w1cnrv=h1snrv;
141500121109                *in07 = *off;
141600121109                chain (W1Cnrv) TIVIS05L;
141700121109                IF  not %found(TIVIS05L);
141800121109                  *in07 = *on;
141900121109                ENDIF;
142000090423                *in06=*on;
142100090423             else;
142200090423                *in06=*off;
142300090416             endif;
142400090417             w1cpos=h1spos;
142500090417             w1cpru=h1spru;
142600090417             clear w1dpos;
142700090417             chain w1cpos azorg01l;
142800090417             if %found (azorg01l);
142900090417             w1dpos=orgdes;
143000090417             endif;
143100090622       /// Uscita dalla window per F12-Ritorno
143200090622            dow *inkl=*off;
143300090528             if dsp_mod=0;
143400090528                exfmt mk20w01;
143500090528             else;
143600090528                exfmt mk20w41;
143700090528             endif;
143800090622            enddo;
143900090417          endif;
144000090416       endif;
144100090416       endsr;
144200090416       //-------------------------------------------------------------*
144300090416       // Reperimento  nrr del sfl su cui è posizionato il cursore    *
144400090416       //-------------------------------------------------------------*
144500090416       Begsr RepNrrSfl;
144600090616       if dsp_mod=1;
144700090616          di_sflnrr=dsp4ds.di_sflnrr;
144800090616       endif;
144900090416       $nrr=(di_sflnrr + riri - K_sflnrr1);
145000090416       if $nrr<*zeros or riri<k_sflnrr1;
145100090416          clear $nrr;
145200090416       endif;
145300090416       Endsr;
145400090421       //-------------------------------------------------------------*
145500090421       // Gestione tasto funzione F10=Inserimento                     *
145600090421       //-------------------------------------------------------------*
145700090421       Begsr Sr_f10;
145800090421
145900090421       w_mk20flm=' ';
146000090421       exsr sr_inzs01;
146100090421       if not *in90;
146200090528          if dsp_mod=0;
146300090528             write mk20t00;
146400090528             write mk20z01;
146500090528          else;
146600090528             write mk20t40;
146700090528             write mk20z41;
146800090528          endif;
146900090421       endif;
147000090421
147100090421       // posiziono il cursore nel sfl
147200090421       v1cnrr=v1cpos;
147300090421
147400090421       // Emetto la videata
147500090421       reset $finef10;
147600090421       dow $finef10=*off;
147700090528          if dsp_mod=0;
147800090528             exfmt mk20c01;
147900090528          else;
148000090528             exfmt mk20c41;
148100090528          endif;
148200090421          *in28=*off;
148300090421          *in90=*off;
148400090421          clear v1cmsg;
148500090421          select;
148600090421       // F3=Fine
148700090421             When *inkc;
148800090421                omk20fxx='03';
148900090421                leavesr;
149000090421       // F12=Ritorno --> per il momento non lo gestisco perchè per farlo bene dovrei
149100090421       //                 utilizzare un altro sfl. Vediamo prima cosa vogliono veramente
149200090421       //    When *inkl;
149300090421       //       omk20fxx='12';
149400090421       //       leavesr;
149500090421       // Rollup
149600090421             When *in25;
149700090421       //    Caricamento righe vuote
149800090616                if dsp_mod=0;
149900090616                   v1cpos=di_sflnrr;
150000090616                else;
150100090616                   v1cpos=dsp4ds.di_sflnrr;
150200090616                endif;
150300090421                exsr sr_freerow;
150400090421       // F6=Conferma
150500090421             when *inkf;
150600090421                exsr sr_updTICPN;
150700090421                omk20fxx='06';
150800090421                leavesr;
150900090421          endsl;
151000090421       enddo;
151100090421
151200090421       Endsr;
151300090921
151400090921       //-------------------------------------------------------------*
151500090921       // Gestione tasto funzione F6=Conferma                         *
151600090921       //-------------------------------------------------------------*
151700090921       Begsr Sr_f6;
151800090921
151900090921         exsr sr_updTICPN;
152000090921         omk20fxx='06';
152100090921         $fine=*on;
152200090921
152300090921       Endsr;

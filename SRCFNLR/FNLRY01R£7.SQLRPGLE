000100110617      *---------------------------------------------------------------*
000200140624      * Disposizioni di Consegna: validazione su as400                *
000300110617      *---------------------------------------------------------------*
000400110617
000500110617     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600140624     h dftactgrp(*no) actgrp(*caller)
000700141024     h bnddir('UBRTVNETA')
000800110617
000900110617      *---------------------------------------------------------------*
001000110617
001100140908     ftiidc04l  uf   e           k disk
001200140625     ffnarb01l  if   e           k disk
001300160201     ffnblp01l  if   e           k disk    prefix(arb:3)
001400140625     ffiar401l  if   e           k disk
001500141022     ffiarg01l  uf a e           k disk
001600140626     D FIDNA6R         PR
001700140626     D                                     extpgm('FIDNA6R')
001800140626     D fidna6ds                            likeds(fidna6ds)
001900140925     D fidna6bds                           likeds(fidna6bds)
002000141022     D fnlv55r         PR
002100141022     D                                     extpgm('FNLV55R')
002200141022     D fnlv55ds                            likeds(fnlv55ds)
002300140624     d tiidcds       e ds                  extname(tiidc00f)
002400140625     d fnlry00m0     e ds
002500140625     d fnlry00f0     e ds                  qualified inz(*extdft)
002600140625     d kpjba         e ds
002700140625     d fnlr48ds      e ds
002800140626     d dsarbd        e ds                  inz
002900140626     d dsarbk        e ds                  inz
003000140626     d darbt         e ds                  inz
003100140626     d dsarbg        e ds                  inz
003200140625     d trul90ds      e ds                  inz
003300140626     d dsarbn        e ds                  inz
003400140626     d fidna6ds      e ds                  inz
003500141002     d* campo notauni comprende ida06no1+ida06no2
003600141002     d  notauni               25    444
003700140925     d fidna6bds     e ds                  inz
003800141023     d fnlv55ds      e ds                  inz
003900140908     d tibs02ds      e ds
004000140908     d divp          e ds
004100110617      * - Campi di comodo
004200140626     d dataeur         s               d   datfmt(*eur)
004300140627     d dataiso         s               d   datfmt(*iso)
004400140624     d werrchk         s              1
004500140625     d wrsd2           s             35
004600140625     d wggc            s                   like(idcprcr)
004700140627     d wgg             s              2  0
004800140627     d wmm             s              2  0
004900140627     d waa             s              4  0
005000140627     d dw              s              1s 0
005100140806     d s_d48msg        s                   like(d48msg)
005200141002     d n               s              4  0
005300141002     d nrighe          s              2  0
005400160201     d wes_bolla       s              1
005500140625
005600140625     D prmRqsOpCode    s             10I 0
005700140625     D prmRpyOpCode    s             10I 0
005800140625     D prmRpyIdMsg     s             10I 0
005900140625     D prmRqsFormato   s             10A
006000140625     D prmRqsData      s          32767A
006100140625     D prmRqsDataSize  s             10I 0
006200140625     D prmRpyFormato   s             10a
006300140625     D prmRpyData      s          32767
006400140625     D prmRpyDataSize  s             10I 0
006500140625     D prmRpyFormMsg   s             10A
006600140625     D prmRpyMessage   s          32767
006700140625     D prmRpyMsgSize   s             10I 0
006800140625     D prmRpyFormForz  s             10A
006900140625     D prmRpyForzatu   s          32767
007000140625     D prmRpyForSize   s             10I 0
007100130406
007200140908       //--------------------------------------------------------------
007300140908       //?Definizione procedure.
007400140908       //--------------------------------------------------------------
007500140908     d tibs02r         pr                  extpgm('TIBS02R')
007600140908     d  kpjba                              likeds(kpjba)
007700140908     d  tibs02ds                           likeds(tibs02ds)
007800110617
007900110617      // ? PROTOTIPI ?
008000140625      /copy gaitrasrc/srcprotopr,Fnlry00r
008100140625      /copy gaitrasrc/srcprotopr,fnlr48r
008200140625      /copy gaitrasrc/srcconst,FNLRY00R
008300141024       // -?Nome del sistema?
008400141024     d currSysNeta     s              8a   inz
008500141024       // -?Reperimento NETA sistema AS/400 corrente?
008600141024      /copy gaitrasrc/srcProtoPR,UBRTVNETA
008700110617
008800130406
008900110627     C     *ENTRY        PLIST
009000110627     C                   PARM                    chiudi            1
009100130406
009200110617      /free
009300141024         // -?Verifica del sistema AS/400 corrente?
009400141024       if  UBRTVNETA_Rtv(currSysNeta) =  *zero;
009500141024
009600141024         // -?Imposto knsif e knmus ?
009700141024         if  %subst( currSysNetA : 1 : 6 ) = 'SETRAS';
009800141024           knsif  = 'FILTRA201 ';
009900141024         else;
010000141024           knsif  = 'FILTRAPRD ';
010100141024         endif;
010200141024         knmus='INTERNET';
010300141024
010400140908       // Reperimento dati tabella IVP - Variabili programma per INTERNET
010500140908       clear tibs02ds;
010600140908       clear divp;
010700140908       t02mod='C';
010800140908       t02cod='IVP';
010900140908       t02ke1='1  ';
011000140908       t02tla='L  ';
011100140908       tibs02r(kpjba:tibs02ds);
011200140908       if t02err=*blanks;
011300140908          divp=t02uni;
011400140908       endif;
011500130625
011600140624          // Leggo TIIDC  da elaborare
011700130604          exsr sr_Elabora;
011800141024
011900141024       endif;
012000130406
012100110627       if chiudi='S';
012200140623          eval *inlr=*on;
012300140626          D48TLA='C';
012400140626          kpjbu=fnlr48ds;
012500140626          fnlr48r(kpjba:dsarbd:dsarbk:darbt:dsarbg:trul90ds);
012600140623       else;
012700140623          eval *inrt=*on;
012800110627       endif;
012900130406
013000110617       //-------------------------------------------------------------*
013100140624       //Lettura TIIDC record da elaborare                             *
013200110617       //-------------------------------------------------------------*
013300110617       Begsr SR_Elabora;
013400140908        setll (*loval) tiidc04l;
013500140908        read           tiidc04l;
013600130605
013700140908  1     dow not %eof(tiidc04l) ;
013800130406
013900130502        // controllo se è stata richiesta la chiusura del sottosistema
014000110627           if %shtdn;
014100110627               chiudi='S';
014200110627               leavesr;
014300110627           endif;
014400130406
014500141022           werrchk='0';
014600141022       // Se dispo inserite da AS/400 (utente BRT) non eseguo chkistruco
014700141022           if idcmodacc<>'B';
014800141022               exsr sr_chkistruco;
014900141022           endif;
015000141008       // Chaino arb a prescindere da eventuale errore chkistruco perchè arblna mi
015100141008       // serve per creazione R.A. che viene fatta SEMPRE
015200141008             exsr sr_bolla;
015300140926
015400140624           if werrchk='0';
015500141022       // Altri controlli per monitorare situazioni non caricabili in automatico
015600140919             exsr sr_altrichk;
015700140919             if werrchk='0';
015800160201                 if IDCTIPODIS<>'4' and idcfla=' '  and §ivpindm=' ' ;
015900160201
016000160201                 if     %found(fnarb01l)  ;
016100140919                   exsr sr_lr48;
016200160201                 else  ;
016300160201                   idcerr='1';
016400160201                   IDCLOG='Non trovata bolla in FNARB00F'   ;
016500140919                 endif;
016600160201                 endif;
016700140919             endif             ;
016800140919           else;
016900140919               idcerr='1';
017000140919               IDCLOG=%subst(textmsg:1:200);
017100140919           endif;
017200140926
017300140926        // Sempre: Creazione R.A.
017400141219           if %scan ('salvataggio in corso':idclog)=0;
017500151116           // inserisco la r.a. soltanto se i campi sonon vuoti (per
017600151116           // cui non sto arrivando da gestine r.a.)
017700151119           if idcnut=0 ;
017800140926           exsr sr_InsRa;
017900140926           IDCANT= ODA6BANT;
018000140926           IDCNUT= ODA6BNUT;
018100140926           IDCPDT= ODA6BPDT;
018200141219           endif;
018300151201
018400151201           IDCELAFLG='E';
018500160201              if wes_bolla='N'  ;
018600160201                   idcerr='1';
018700160201                   IDCLOG='Non trovata bolla in FNARB00F e FNBLP00F' ;
018800160202                   IDCELAUTE=KNMUS;
018900160202                   IDCELADATA=%int( %subst( %char( %dec( %timestamp() ) )
019000160202                                 :1 :14 ) );
019100160201              endif;
019200160201           endif;
019300140624           update tiidc000;
019400141022
019500151119        // per le richieste di dirottamento/reso forzo il giro 'DIR'
019600151112           if (werrchk='0' and idctipodis='3' and idcnewlna>0) or
019700151112               (werrchk='0' and idctipodis='5' and idcfla='R')  ;
019800160201
019900160201             if wes_bolla<>'N'   ;
020000141022              exsr sr_fiarg;
020100160201             endif  ;
020200141022           endif;
020300140926
020400130406
020500140908           read         tiidc04l ;
020600110617        enddo;
020700130406
020800110617        endsr;
020900140624       //-------------------------------------------------------------*
021000140624       //Richiamo pgm di controllo eseguibilità dell'indicazione      *
021100140624       //-------------------------------------------------------------*
021200140624        Begsr sr_ChkIstruCo;
021300140624
021400130406
021500140624        prmrqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO    ;
021600140624        prmrqsformato='TIIDCDS  '  ;
021700140624        prmrpyformato='         '  ;
021800140624       prmRpyFormMsg='FNLRY00M0'  ;
021900140624
022000140624         prmrqsData = tiidcds  ;
022100140624         prmrqsDatasize = %SIZE(tiidcds);
022200140624         prmrpyDatasize = %SIZE(tiidcds);
022300140624         prmRpyMsgSize  = %SIZE(fnlry00m0);
022400140624         prmRpyForSize  = %SIZE(fnlry00f0);
022500140624         Fnlry00_Immetti(prmRqsOpCode : prmRpyOpCode : prmRpyIdMsg :
022600140624                         prmRqsFormato : prmRqsData : prmRqsDataSize:
022700140624                         prmRpyFormato : prmRpyData : prmRpyDataSize:
022800140624                         prmRpyFormMsg : prmRpyMessage : prmRpyMsgSize) ;
022900140624         //                prmRpyFormForz: prmRpyForzatu :prmRpyForSize) ;
023000130406
023100140624 5       if   prmrpyOpCode = FNLRY00_RPYOPCODE_ERROR  ;
023200140624            werrchk='1';
023300140626            fnlry00m0=prmrpymessage;
023400140624         endif;
023500140624
023600140624        endsr;
023700140919
023800140919       //-------------------------------------------------------------*
023900140919       //lettura bolla in arrivo                                      *
024000140919       //-------------------------------------------------------------*
024100140919        Begsr sr_bolla   ;
024200140919
024300140919             clear wrsd2;
024400160201             clear wes_bolla;
024500160202             clear arbnsp  ;
024600140919
024700140919             chain (idcaas:idclnp:idcnrs:idcnsp) fnarb01l;
024800160201             if not %found(fnarb01l)  ;
024900160201               chain (idcaas:idclnp:idcnrs:idcnsp) fnblp01l;
025000160201             if not %found(fnblp01l)  ;
025100160201              wes_bolla='N'  ;
025200160201             endif   ;
025300160201             endif   ;
025400160201
025500140919             chain (arbaas:arblnp:arbnrs:arbnsp:'D') fiar401l ;
025600140919             if %found(fiar401l);
025700140919                eval wrsd2=%subst(ar4not:1:35);
025800140919             endif;
025900140919        endsr;
026000140919       //-------------------------------------------------------------*
026100140919       //Altri controlli                                              *
026200140919       //-------------------------------------------------------------*
026300140919        Begsr sr_altrichk;
026400140919       // L'indicazione non è caricabile in automatico se:
026500140919       // 1) ricevute indicazioni di "Consegna Richiesta" e la bolla
026600140919       //    ha la consegna particolare a "Supermercato"
026700140919       // 2) ricevute indicazioni di "Indirizzo Alternativo" e la bolla è in distinta
026800140919        select;
026900141001        when (arbtc1='S' or arbtc2='S') and
027000141001             (IDCTIPODIS='1' or (IDCTIPODIS='3' and idcdcr>0));
027100140919          werrchk='1';
027200140919          idcerr='3';
027300141015          IDCLOG='Spedizione con Cons. Particolare ai Supermercati';
027400141015       //         caricare manualmente utilizzando la causale "CS"';
027500141015        when IDCTIPODIS='3'  and arbndc>0 and §IVPATIA=' ';
027600140919          idcerr='3';
027700140919          IDCLOG='Spedizione in distinta';
027800140919          werrchk='1';
027900140919        endsl;
028000140919        endsr;
028100140624       //-------------------------------------------------------------*
028200140624       //Richiamo pgm di caricamento variazioni bolle FNLR48R         *
028300140624       //-------------------------------------------------------------*
028400140624        Begsr sr_lr48      ;
028500140624
028600140624        clear fnlr48ds;
028700140807        clear s_d48msg;
028800140625        clear dsarbg;
028900140625        clear dsarbd;
029000140625        clear dsarbk;
029100140625        clear darbt;
029200140624        d48aas=arbaas;
029300140624        d48lnp=arblnp;
029400140624        d48nrs=arbnrs;
029500140624        d48nsp=arbnsp;
029600140625        d48fgs=idcfgs;
029700140625        d48cbo=arbcbo;
029800140625        d48tbo='A'   ;
029900140624        select;
030000140925       // APPUNTAMENTO
030100140624        when IDCTIPODIS='1';
030200140804       //  if arbtc1='S' or arbtc2='S';
030300140804       //     d48cvb='CS';
030400140804       //  else;
030500140804       //     d48cvb='CR';
030600140804       //  endif;
030700140926           d48ffr='E'   ;
030800140925           exsr impo_xcvbCI;
030900140925       // FERMO DEPOSITO O INDIRIZZO ALTERNATIVO
031000140624        when IDCTIPODIS='2' or IDCTIPODIS='3';
031100140625           d48cvb='I0';
031200140625           d48trc='D';
031300140625           if idctipodis='2';
031400140926             d48ffr='E'   ;
031500140911             if idctipocli='M';
031600140910              d48cvb='I7';
031700140911             endif;
031800140625              §BDRSD=arbrsd;
031900140625              §BDRD2=wrsd2 ;
032000140625              §BDIND=arbIND;
032100140625              §BDCAD=arbCAD;
032200140625              §BDLOD=arbLOD;
032300140625              §BDPRD=arbPRD;
032400140625              §BDNZD=arbNzd;
032500140625              §BDFIN=arbfin;
032600140625              §BDFFD=idcffd;
032700141017              if §bdffd='S';
032800141017                 §bdffd='Y';
032900141017              endif;
033000140625           else ;
033100141001              if idcdcr>0 ;
033200140926                 d48ffr='F'   ;
033300140926              else;
033400140926                 d48ffr='E'   ;
033500140926              endif;
033600140926              exsr sr_ImpoArbdI0 ;
033700140625           endif;
033800140625       // P.IVA: PASSO SEMPRE ARBCPI PERCHÈ FNLR48R usa il campo solo se sped assegnato
033900140625       // e in questo caso in arbcpi c'è proprio la p.iva del destinatario
034000140625           §BDCPI  = ARBCPI;
034100141002           §BDREF  = IDCREF;
034200140625           §BDTEL  = IDCTELD;
034300141010           §bdpkf  = arbpkf;
034400141010           §bdvlf  = arbvlf;
034500140625
034600140804       // Altre istruzioni  (non entro in questa routine in questo caso)
034700140804       // when IDCTIPODIS='4';
034800140624        endsl;
034900140625
035000140925        exsr sr_calllr48;
035100140925
035200140926       // NON RISCONTRATI ERRORI: OPERAZIONI PER Tipo dispo "INDIRIZZO ALTERNATIVO"
035300141001       // Con DCR>0
035400140926        if (d48err = *blanks or d48err='Z')
035500141001           and  IDCTIPODIS='3' and idcdcr>0 ;
035600140926               exsr sr_IndAlt;
035700140926        endif;
035800140925
035900140925       // NON RISCONTRATI ERRORI:
036000140925        if d48err = *blanks or d48err='Z';
036100140925       // Richiamo fnlr48r per la seconda volta per caricare referente/telefoni/email
036200140925           if d48err='Z';
036300140925              s_d48msg=d48msg;
036400140925           endif;
036500140926           d48ffr='E'   ;
036600140925           clear d48err;
036700140925           clear d48msg;
036800140925           d48cvb='NE';
036900140925           d48trc='N';
037000140925           clear dsarbn;
037100140925           §BNU_SN='S';
037200140925           §BNUNI70=IDCMAILAVV;
037300140925           §BNR_SN='S';
037400140925           §BNREF=IDCREF;
037500140925           §BNTEL=IDCTELD;
037600140925           §BNC_SN='S';
037700140925           §BNCEL=IDCTELAVV;
037800140929           kpjbu=fnlr48ds;
037900140929           fnlr48r(kpjba:dsarbd:dsarbk:darbt:dsarbg:trul90ds:dsarbn);
038000140929           fnlr48ds=kpjbu;
038100140929           if d48err <> *blanks and d48err<>'Z' and d48ffr='E';
038200140929              IDCERR='2' ;
038300140929              IDCLOG=d48msg;
038400140929           endif;
038500140925        endif;
038600140925
038700140925       // NON RISCONTRATI ERRORI: imposto utente e data di elaborazione su TIIDC
038800140925        if d48err = *blanks or d48err='Z';
038900140925       // Memorizzo eventuali messaggi di avviso
039000140925              if s_d48msg<>*blanks;
039100140925                 d48msg=s_d48msg;
039200140925              endif;
039300140925              IDCLOG=d48msg;
039400140925              IDCELAUTE=KNMUS;
039500140925              IDCELADATA=%int( %subst( %char( %dec( %timestamp() ) )
039600140925                                 :1 :14 ) );
039700140925        endif;
039800140925
039900140925
040000140624        endsr;
040100140925       //-------------------------------------------------------------*
040200140925       //valorzzazione campi parm lr48 per consegna richiesta         *
040300140925       //-------------------------------------------------------------*
040400140925        Begsr impo_xcvbCI;
040500140925           d48cvb='CI';
040600140925           d48trc='G';
040700140925           §BGTCR=IDCTCR;
040800140925           §BGHCR=idchcr;
040900140930           §BGDCR=idcdcr;
041000140925           §BGGC1=arbgc1;
041100140925           §BGGC2=arbgc2;
041200140925       //   Giorni di chiusura --> se richiesta preferenza M/P la traduco nel primo campo
041300140925       //                          libero dei giorni di chiusura
041400140925           clear wggc;
041500140925           if idcprcr<>' ';
041600140925           if idcprcr='M';
041700140925              wggc='P';
041800140925           else;
041900140925              wggc='M';
042000140925           endif;
042100140925           endif;
042200140925           if wggc<>*blanks;
042300140925       // Nel primo byte  del giorno di chiusura devo mettere il numero del giorno della
042400140925       // settimana che corrisponde alla data cons rich.
042500140925       // Nel secondo byte metto mattino o pomeriggio
042600140925               dataiso=%date(§bgdcr:*iso);
042700140925               EXEC SQL SET :dw = DAYOFWEEK_ISO(:dataiso);
042800140925              if §BGGC1=*blanks;
042900140925                 %subst(§BGGC1:2:1)=wggc;
043000140925                 %subst(§BGGC1:1:1)=%editc(dw:'X');
043100140925              else;
043200140925                 %subst(§BGGC2:2:1)=wggc;
043300140925                 %subst(§BGGC2:1:1)=%editc(dw:'X');
043400140925              endif;
043500140925              if §BGGC1=§BGGC2;
043600140925                 clear §BGGC2;
043700140925              endif;
043800140925           endif;
043900140925           §BGTC1=arbtc1;
044000140925           §BGTC2=arbtc2;
044100140925        endsr;
044200140926       //-------------------------------------------------------------*
044300140926       //Impostazione DSARBD per cvb "I0"                             *
044400140926       //-------------------------------------------------------------*
044500140926        Begsr sr_ImpoArbdI0;
044600140926              §BDRSD=%subst(idcrsd:1:35);
044700140926              §BDRD2=%subst(IDCRSD:36:35);
044800140926              §BDIND=IDCIND;
044900140926              §BDCAD=IDCCAD;
045000140926              §BDLOD=IDCLOD;
045100140926              §BDPRD=IDCPRD;
045200140926              §BDNZD=IDCNAZ;
045300140926       // Se CAP/LOCALITA/PROVINCIA sono invariati rispetto alla bolla passo
045400140926       // flag inoltro = alla bolla altrimenti lo passo vuoto in modo da farlo ricalcolare
045500140926       // a fnlr48r in base ai nuovi dati
045600140926              if arbcad=idccad and arblod=idclod and arbprd=idcprd;
045700140926                §BDFIN = arbfin;
045800140926              endif;
045900140926              §BDFFD=arbffd;
046000140926        endsr;
046100140925       //-------------------------------------------------------------*
046200140925       //Richiamo FNLR48R                                             *
046300140925       //-------------------------------------------------------------*
046400140925        Begsr sr_calllr48;
046500140925        kpjbu=fnlr48ds;
046600141024       // if knsif=*blanks;
046700141024       //    knsif='*LIBL';
046800141024       // endif;
046900141024       // if knmus=*blanks ;
047000141024       //    knmus='INTERNET  ';
047100141024       // endif;
047200140925        fnlr48r(kpjba:dsarbd:dsarbk:darbt:dsarbg:trul90ds);
047300140925        fnlr48ds=kpjbu;
047400140926        if d48err <> *blanks and d48err<>'Z' and d48ffr='E';
047500140925           IDCERR='2' ;
047600140925           IDCLOG=d48msg;
047700140926        endif;
047800140925        endsr;
047900140926       //-------------------------------------------------------------*
048000140926       //Richiami ulteriori lr48 per Indirizzo alternativo se ricevuta anche una DCR
048100140926       //-------------------------------------------------------------*
048200140926        Begsr sr_IndAlt;
048300140926
048400140926       // Richiamo lr48 anche per la variazione di Cons.Rich.
048500140926        if d48err='Z';
048600140926           s_d48msg=d48msg;
048700140926        endif;
048800140926        exsr impo_xcvbCI;
048900140926        d48ffr='F'   ;
049000140926        exsr sr_calllr48;
049100140926
049200140926       // NON RISCONTRATI ERRORI --> rieseguo call
049300140926       //                        a FNLR48R per l'aggiornamento
049400140926
049500140926        if d48err = *blanks or d48err='Z';
049600140926
049700140926       // Richiamo per "I0"
049800140926
049900140926           d48cvb='I0';
050000140926           d48trc='D';
050100140926           d48ffr='E'   ;
050200140926           exsr sr_ImpoArbdI0 ;
050300140926       // P.IVA: PASSO SEMPRE ARBCPI PERCHÈ FNLR48R usa il campo solo se sped assegnato
050400140926       // e in questo caso in arbcpi c'è proprio la p.iva del destinatario
050500140926           §BDCPI  = ARBCPI;
050600140926           §BDREF  = IDCREF;
050700140926           §BDTEL  = IDCTELD;
050800141010           §bdpkf  = arbpkf;
050900141010           §bdvlf  = arbvlf;
051000140926           exsr sr_calllr48;
051100140926
051200140926       // Richiamo per "CI"
051300140926
051400141222       //     devo richainare la bolla FNARB per recepire le variazioni fatte dalla chiamata
051500141222       //     precedente. In particolare devo recepire evenutali consegne particolari (es. passaggio
051600141222       //     da destinatario "normale" a destinatario che prevede appuntamento. in questo
051700141222       //     caso lr48 durante la prcedente chiamata oltre ad eseguire un I0 ha eseguito anche una
051800141222       //     "CP" per aggiungere la "A" di appuntamento che ora dovrò passare nel prossimo richiamo
051900141222
052000141222              exsr sr_bolla;
052100141222
052200140926           exsr impo_xcvbCI;
052300140926           d48ffr='E'   ;
052400140926           exsr sr_calllr48;
052500140926        endif;
052600140926        endsr;
052700140626       //-------------------------------------------------------------*
052800140626       //inserimento Richiesta Assistenza                             *
052900140626       //-------------------------------------------------------------*
053000140626        Begsr sr_InsRA;
053100140626
053200140626           clear fidna6ds;
053300140626
053400151116           iDA06MAD = ' 86';
053500140626
053600140626           select;
053700140627
053800140627       // APPUNTAMENTO
053900140627
054000140626           when IDCTIPODIS='1';
054100141002              notauni = 'Consegna Richiesta';
054200140627              dataeur=%date(IDCDCR:*iso);
054300141001       //     ida06no2= 'Cons.Rich.' ;
054400140627              select;
054500140627              when IDCTCR = *blanks ;
054600141002                 %subst(notauni:36)= 'Il:'  ;
054700140627              when IDCTCR = 'D'     ;
054800141002                 notauni= %trim(notauni) + 'Dopo il:'  ;
054900140627              when IDCTCR = 'P'     ;
055000141002                 notauni= %trim(notauni) + 'Prima del:'  ;
055100140627              endsl;
055200141002              notauni=%trim(notauni)+ ' '+ %editw(%dec(dataeur):'  /  /    ');
055300140627              if idchcr>0;
055400141002                 notauni=%trim(notauni) + ' Ore ' + %editw(idchcr:'  :  ');
055500140627              endif;
055600140627              if idcprcr<>*blanks;
055700140627              select;
055800140627              when idcprcr='M';
055900141002                 notauni=%trim(notauni) + ' Mattino';
056000140627              when idcprcr='P';
056100141002                 notauni=%trim(notauni) + ' Pomeriggio';
056200140627              endsl;
056300140627              endif;
056400140627
056500140627       // FERMO DEPOSITO
056600140627
056700140626           when IDCTIPODIS='2';
056800141002              notauni = 'Fermo Deposito';
056900151116       // R E S O
057000151116           when IDCTIPODIS='5';
057100151116              notauni  = 'R  E  S  O';
057200140627
057300140627       // CONSEGNA AD ALTRO INDIRIZZO
057400140627
057500140626           when IDCTIPODIS='3';
057600141002              notauni  = 'Indirizzo Alternativo';
057700141006             if idcnewlna<>0;
057800141006              notauni=%trim(notauni)+ '-Nuova Lna ' + %editc(idcnewlna:'X');
057900141006             endif;
058000141002              %subst(notauni:36)  = %trim(idcrsd) + ' ' + %trim(idcind) +
058100141002                                    ' ' + %trim(idccad) + ' ' + %trim(idclod) +
058200141002                                    ' ' + %trim(idcprd) + ' ' + %trim(idcnaz);
058300141001             if idcdcr>0 ;
058400141002              n= %checkr(' ':notauni);
058500141002              nrighe=%div(n:35) ;
058600141002              if %rem(n:35)>0;
058700141002                 nrighe+=1;
058800141002              endif;
058900141002              n=(nrighe*35)+1;
059000141002              %subst(notauni:n)= 'Cons.Rich.' ;
059100141001             endif;
059200141001             if idcdcr>0;
059300140930              dataeur=%date(IDCDCR:*iso);
059400140930              select;
059500140930              when IDCTCR = *blanks ;
059600141002                 notauni = %trim(notauni) + 'Il'  ;
059700140930              when IDCTCR = 'D'     ;
059800141002                 notauni= %trim(notauni) + 'Dopo il'  ;
059900140930              when IDCTCR = 'P'     ;
060000141002                 notauni= %trim(notauni) + 'Prima del'  ;
060100140930              endsl;
060200141002             notauni=%trim(notauni)+' '+%editw(%dec(dataeur:*dmy):'  /  /  ');
060300141001             endif;
060400140930              if idchcr>0;
060500141002                 notauni=%trim(notauni) + ' Ore ' + %editw(idchcr:'  :  ');
060600140930              endif;
060700140930              if idcprcr<>*blanks;
060800140930              select;
060900140930              when idcprcr='M';
061000141002                 notauni=%trim(notauni) + ' Mattino';
061100140930              when idcprcr='P';
061200141002                 notauni=%trim(notauni) + ' Pomeriggio';
061300140930              endsl;
061400140930              endif;
061500141001       // ALTRE INDICAZIONI
061600141001           when IDCTIPODIS='4';
061700141002              notauni = 'Altre Indicazioni';
061800141002              %subst(notauni:36)= %trim(idcnote);
061900140626           endsl;
062000140627       // referente e telefono + eventuale cell e indirizzo email
062100140930       //     ida06no2=%trim(ida06no2)+  ' Ref.Cons: ' + %trim(idcref) +
062200140930       //     ' ' + %trim(idcteld);
062300140627           if IDCTELAVV  <> *blanks;
062400141002            if %checkr(' ':notauni)<35;
062500141002              %subst(notauni:36)= 'Cell:' + %trim(idctelavv);
062600141002            else;
062700141002              notauni=%trim(notauni) + ' Cell:' +
062800140627              %trim(idctelavv);
062900141002            endif;
063000140627           endif;
063100141001           if IDCMAILAVV <> *blanks;
063200141002            if %checkr(' ':notauni)<35;
063300141002             %subst(notauni:36)= 'Email:' + %trim(idcmailavv);
063400141002            else;
063500141002             notauni=%trim(notauni) + ' Email:' + %trim(idcmailavv);
063600141002            endif;
063700141001           endif;
063800140627
063900140926       //  ida06no1=%trim(ida06no1) + ' - Richiesto da';
064000140926       //  if idctipocli ='M';
064100140926       //  ida06no1=%trim(ida06no1) + ' Mitt.';
064200140926       //  else;
064300140926       //  ida06no1=%trim(ida06no1) + ' Dest.';
064400140926       //  endif;
064500140926       //  if idcidcli<>*blanks ;
064600140926       //    ida06no1=%trim(ida06no1) + ' Id.cli. ' + idcidcli;
064700140926       //  endif;
064800140926       //    dataeur=%date(%dec(%subst(%editc(idcinsdata:'X'):1:8):8:0)
064900140926       //            :*iso);
065000140926       //    wgg=     %subdt(dataeur:*days);
065100140926       //    wmm=     %subdt(dataeur:*months);
065200140926       //    waa=     %subdt(dataeur:*years);
065300140926       //    ida06no1=%trim(ida06no1) + ' in data ' +
065400140926       //             %editc(wgg:'X') + '/' +
065500140926       //             %editc(wmm:'X') + '/' +
065600140926       //             %editc(waa:'X') + ' alle ' +
065700140926       //             %subst(%editc(idcinsdata:'X'):9:2) + ':'  +
065800140926       //             %subst(%editc(idcinsdata:'X'):11:2)        ;
065900141002           if idcip<>*blanks ;
066000141002            if %checkr(' ':notauni)<35;
066100141002             %subst(notauni:36)= 'IP:' + idcip;
066200141002            else;
066300141002             notauni=%trim(notauni) + ' IP:' + idcip;
066400141002            endif;
066500141002           endif;
066600140626
066700140626           iDA06TOR = 'S';
066800140926           iDA06OGG = %trim(%editc(idcLNP:'X')) +
066900140926                      %trim(%editc(idcNRS:'X')) +
067000140926                      %trim(%editc(idcNSP:'X')) +
067100140926                      %trim(%editc(idcAAS:'X'));
067200141023           if idcmodacc<>'B';
067300141022              iDA06UTE = 'INTERNET';
067400141022           else;
067500141022              iDA06UTE = idcelaute ;
067600141022           endif;
067700141008           if arblna>0;
067800141008              iDA06FIL = arblna;
067900141008           else;
068000141008              iDA06FIL = idcfgs;
068100141008           endif;
068200160317           if arbksc>0 and %subst(%editc(arbksc:'X'):4:4)<>'8888' and
068300160317                           %subst(%editc(arbksc:'X'):4:4)<>'9999'       ;
068400160317              ida06ksc=arbksc  ;
068500160317           endif  ;
068600160317
068700140926           ida06dar = %dec(%subst(%editc(idcinsdata:'X'):1:8):8:0);
068800140926           ida06orr = %dec(%subst(%editc(idcinsdata:'X'):9:6):6:0);
068900141022           if idcmodacc<>'B';
069000141022              if idctipocli='M';
069100141022                 ida06rsc = idcidcli;
069200141022              else;
069300141022                 ida06rsc = 'NON CODIFICATO';
069400141022              endif;
069500141022           endif;
069600141001           %subst(iDA06rsc:35:1) = idctipodis;
069700140926           iDA06pog = idcfgs;
069800141023           if idcmodacc<>'B';
069900141023              iDA06tco = 'I'   ;
070000141023           endif;
070100140926           iDA06tch = idctipocli;
070200140926
070300140926       // Non chiudo R.A. se la dispo non è stata caricata
070400140926           if idceladata=0;
070500140926               iDA06cdt = '999'     ;
070600150624               IDA06PRT = '6'       ;
070700150624               IDA06rpr = 'ROM'     ;
070800140926           endif;
070900140930           iDA06ref = idcref  ;
071000140926           iDA06tel = idcteld ;
071100140626
071200140925           callP(e) FIDNA6R (fidna6ds:fidna6bds);
071300140626        endsr;
071400140626
071500141022       //-------------------------------------------------------------*
071600141022       //Forzatura giro DIR per richieste di dirottamento             *
071700141022       //-------------------------------------------------------------*
071800141022        Begsr sr_fiarg;
071900141022           chain (idcaas:idclnp:idcnrs:idcnsp) fiarg01l;
072000141022           if not %found(fiarg01l);
072100141022              clear fiarg000;
072200141022              argaas  =  idcaas;
072300141022              arglnp  =  idclnp;
072400141022              argnrs  =  idcnrs;
072500141022              argnsp  =  idcnsp;
072600141022              arglna  =  arblna;
072700141022       // p.o. di consegna capofila della £6 del p.o. di arrivo
072800141022              clear  fnlv55ds;
072900141022              d55tpt = '6' ;
073000141022              d55lin=arblna;
073100141023              d55drf=%dec(%date());
073200141022              fnlv55r(fnlv55ds);
073300141022              argpoc  =  d55tfa;
073400141022           endif;
073500141022           argcgi='DIR';
073600141022           argtgi='M';
073700141022           if not %found(fiarg01l);
073800141022              write fiarg000;
073900141022           else;
074000141022              update fiarg000;
074100141022           endif;
074200141022        endsr;

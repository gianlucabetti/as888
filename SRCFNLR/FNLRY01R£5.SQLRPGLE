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
001300140625     ffiar401l  if   e           k disk
001400141022     ffiarg01l  uf a e           k disk
001500140626     D FIDNA6R         PR
001600140626     D                                     extpgm('FIDNA6R')
001700140626     D fidna6ds                            likeds(fidna6ds)
001800140925     D fidna6bds                           likeds(fidna6bds)
001900141022     D fnlv55r         PR
002000141022     D                                     extpgm('FNLV55R')
002100141022     D fnlv55ds                            likeds(fnlv55ds)
002200140624     d tiidcds       e ds                  extname(tiidc00f)
002300140625     d fnlry00m0     e ds
002400140625     d fnlry00f0     e ds                  qualified inz(*extdft)
002500140625     d kpjba         e ds
002600140625     d fnlr48ds      e ds
002700140626     d dsarbd        e ds                  inz
002800140626     d dsarbk        e ds                  inz
002900140626     d darbt         e ds                  inz
003000140626     d dsarbg        e ds                  inz
003100140625     d trul90ds      e ds                  inz
003200140626     d dsarbn        e ds                  inz
003300140626     d fidna6ds      e ds                  inz
003400141002     d* campo notauni comprende ida06no1+ida06no2
003500141002     d  notauni               25    444
003600140925     d fidna6bds     e ds                  inz
003700141023     d fnlv55ds      e ds                  inz
003800140908     d tibs02ds      e ds
003900140908     d divp          e ds
004000110617      * - Campi di comodo
004100140626     d dataeur         s               d   datfmt(*eur)
004200140627     d dataiso         s               d   datfmt(*iso)
004300140624     d werrchk         s              1
004400140625     d wrsd2           s             35
004500140625     d wggc            s                   like(idcprcr)
004600140627     d wgg             s              2  0
004700140627     d wmm             s              2  0
004800140627     d waa             s              4  0
004900140627     d dw              s              1s 0
005000140806     d s_d48msg        s                   like(d48msg)
005100141002     d n               s              4  0
005200141002     d nrighe          s              2  0
005300140625
005400140625     D prmRqsOpCode    s             10I 0
005500140625     D prmRpyOpCode    s             10I 0
005600140625     D prmRpyIdMsg     s             10I 0
005700140625     D prmRqsFormato   s             10A
005800140625     D prmRqsData      s          32767A
005900140625     D prmRqsDataSize  s             10I 0
006000140625     D prmRpyFormato   s             10a
006100140625     D prmRpyData      s          32767
006200140625     D prmRpyDataSize  s             10I 0
006300140625     D prmRpyFormMsg   s             10A
006400140625     D prmRpyMessage   s          32767
006500140625     D prmRpyMsgSize   s             10I 0
006600140625     D prmRpyFormForz  s             10A
006700140625     D prmRpyForzatu   s          32767
006800140625     D prmRpyForSize   s             10I 0
006900130406
007000140908       //--------------------------------------------------------------
007100140908       //?Definizione procedure.
007200140908       //--------------------------------------------------------------
007300140908     d tibs02r         pr                  extpgm('TIBS02R')
007400140908     d  kpjba                              likeds(kpjba)
007500140908     d  tibs02ds                           likeds(tibs02ds)
007600110617
007700110617      // ? PROTOTIPI ?
007800140625      /copy gaitrasrc/srcprotopr,Fnlry00r
007900140625      /copy gaitrasrc/srcprotopr,fnlr48r
008000140625      /copy gaitrasrc/srcconst,FNLRY00R
008100141024       // -?Nome del sistema?
008200141024     d currSysNeta     s              8a   inz
008300141024       // -?Reperimento NETA sistema AS/400 corrente?
008400141024      /copy gaitrasrc/srcProtoPR,UBRTVNETA
008500110617
008600130406
008700110627     C     *ENTRY        PLIST
008800110627     C                   PARM                    chiudi            1
008900130406
009000110617      /free
009100141024         // -?Verifica del sistema AS/400 corrente?
009200141024       if  UBRTVNETA_Rtv(currSysNeta) =  *zero;
009300141024
009400141024         // -?Imposto knsif e knmus ?
009500141024         if  %subst( currSysNetA : 1 : 6 ) = 'SETRAS';
009600141024           knsif  = 'FILTRA201 ';
009700141024         else;
009800141024           knsif  = 'FILTRAPRD ';
009900141024         endif;
010000141024         knmus='INTERNET';
010100141024
010200140908       // Reperimento dati tabella IVP - Variabili programma per INTERNET
010300140908       clear tibs02ds;
010400140908       clear divp;
010500140908       t02mod='C';
010600140908       t02cod='IVP';
010700140908       t02ke1='1  ';
010800140908       t02tla='L  ';
010900140908       tibs02r(kpjba:tibs02ds);
011000140908       if t02err=*blanks;
011100140908          divp=t02uni;
011200140908       endif;
011300130625
011400140624          // Leggo TIIDC  da elaborare
011500130604          exsr sr_Elabora;
011600141024
011700141024       endif;
011800130406
011900110627       if chiudi='S';
012000140623          eval *inlr=*on;
012100140626          D48TLA='C';
012200140626          kpjbu=fnlr48ds;
012300140626          fnlr48r(kpjba:dsarbd:dsarbk:darbt:dsarbg:trul90ds);
012400140623       else;
012500140623          eval *inrt=*on;
012600110627       endif;
012700130406
012800110617       //-------------------------------------------------------------*
012900140624       //Lettura TIIDC record da elaborare                             *
013000110617       //-------------------------------------------------------------*
013100110617       Begsr SR_Elabora;
013200140908        setll (*loval) tiidc04l;
013300140908        read           tiidc04l;
013400130605
013500140908  1     dow not %eof(tiidc04l) ;
013600130406
013700130502        // controllo se è stata richiesta la chiusura del sottosistema
013800110627           if %shtdn;
013900110627               chiudi='S';
014000110627               leavesr;
014100110627           endif;
014200130406
014300141022           werrchk='0';
014400141022       // Se dispo inserite da AS/400 (utente BRT) non eseguo chkistruco
014500141022           if idcmodacc<>'B';
014600141022               exsr sr_chkistruco;
014700141022           endif;
014800141008       // Chaino arb a prescindere da eventuale errore chkistruco perchè arblna mi
014900141008       // serve per creazione R.A. che viene fatta SEMPRE
015000141008             exsr sr_bolla;
015100140926
015200140624           if werrchk='0';
015300141022       // Altri controlli per monitorare situazioni non caricabili in automatico
015400140919             exsr sr_altrichk;
015500140919             if werrchk='0';
015600151112                 if IDCTIPODIS<>'4' and idcfla=' '  and §ivpindm=' ';
015700140919                   exsr sr_lr48;
015800140919                 endif;
015900140919             endif             ;
016000140919           else;
016100140919               idcerr='1';
016200140919               IDCLOG=%subst(textmsg:1:200);
016300140919           endif;
016400140926
016500140926        // Sempre: Creazione R.A.
016600141219           if %scan ('salvataggio in corso':idclog)=0;
016700151116           // inserisco la r.a. soltanto se i campi sonon vuoti (per
016800151116           // cui non sto arrivando da gestine r.a.)
016900151119           if idcnut=0 ;
017000140926           exsr sr_InsRa;
017100140926           IDCANT= ODA6BANT;
017200140926           IDCNUT= ODA6BNUT;
017300140926           IDCPDT= ODA6BPDT;
017400141219           endif;
017500151201
017600151201           IDCELAFLG='E';
017700151116           endif;
017800140624           update tiidc000;
017900141022
018000151119        // per le richieste di dirottamento/reso forzo il giro 'DIR'
018100151112           if (werrchk='0' and idctipodis='3' and idcnewlna>0) or
018200151112               (werrchk='0' and idctipodis='5' and idcfla='R')  ;
018300141022              exsr sr_fiarg;
018400141022           endif;
018500140926
018600130406
018700140908           read         tiidc04l ;
018800110617        enddo;
018900130406
019000110617        endsr;
019100140624       //-------------------------------------------------------------*
019200140624       //Richiamo pgm di controllo eseguibilità dell'indicazione      *
019300140624       //-------------------------------------------------------------*
019400140624        Begsr sr_ChkIstruCo;
019500140624
019600130406
019700140624        prmrqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO    ;
019800140624        prmrqsformato='TIIDCDS  '  ;
019900140624        prmrpyformato='         '  ;
020000140624       prmRpyFormMsg='FNLRY00M0'  ;
020100140624
020200140624         prmrqsData = tiidcds  ;
020300140624         prmrqsDatasize = %SIZE(tiidcds);
020400140624         prmrpyDatasize = %SIZE(tiidcds);
020500140624         prmRpyMsgSize  = %SIZE(fnlry00m0);
020600140624         prmRpyForSize  = %SIZE(fnlry00f0);
020700140624         Fnlry00_Immetti(prmRqsOpCode : prmRpyOpCode : prmRpyIdMsg :
020800140624                         prmRqsFormato : prmRqsData : prmRqsDataSize:
020900140624                         prmRpyFormato : prmRpyData : prmRpyDataSize:
021000140624                         prmRpyFormMsg : prmRpyMessage : prmRpyMsgSize) ;
021100140624         //                prmRpyFormForz: prmRpyForzatu :prmRpyForSize) ;
021200130406
021300140624 5       if   prmrpyOpCode = FNLRY00_RPYOPCODE_ERROR  ;
021400140624            werrchk='1';
021500140626            fnlry00m0=prmrpymessage;
021600140624         endif;
021700140624
021800140624        endsr;
021900140919
022000140919       //-------------------------------------------------------------*
022100140919       //lettura bolla in arrivo                                      *
022200140919       //-------------------------------------------------------------*
022300140919        Begsr sr_bolla   ;
022400140919
022500140919             clear wrsd2;
022600141008             clear fnarb000;
022700140919
022800140919             chain (idcaas:idclnp:idcnrs:idcnsp) fnarb01l;
022900140919             chain (arbaas:arblnp:arbnrs:arbnsp:'D') fiar401l ;
023000140919             if %found(fiar401l);
023100140919                eval wrsd2=%subst(ar4not:1:35);
023200140919             endif;
023300140919        endsr;
023400140919       //-------------------------------------------------------------*
023500140919       //Altri controlli                                              *
023600140919       //-------------------------------------------------------------*
023700140919        Begsr sr_altrichk;
023800140919       // L'indicazione non è caricabile in automatico se:
023900140919       // 1) ricevute indicazioni di "Consegna Richiesta" e la bolla
024000140919       //    ha la consegna particolare a "Supermercato"
024100140919       // 2) ricevute indicazioni di "Indirizzo Alternativo" e la bolla è in distinta
024200140919        select;
024300141001        when (arbtc1='S' or arbtc2='S') and
024400141001             (IDCTIPODIS='1' or (IDCTIPODIS='3' and idcdcr>0));
024500140919          werrchk='1';
024600140919          idcerr='3';
024700141015          IDCLOG='Spedizione con Cons. Particolare ai Supermercati';
024800141015       //         caricare manualmente utilizzando la causale "CS"';
024900141015        when IDCTIPODIS='3'  and arbndc>0 and §IVPATIA=' ';
025000140919          idcerr='3';
025100140919          IDCLOG='Spedizione in distinta';
025200140919          werrchk='1';
025300140919        endsl;
025400140919        endsr;
025500140624       //-------------------------------------------------------------*
025600140624       //Richiamo pgm di caricamento variazioni bolle FNLR48R         *
025700140624       //-------------------------------------------------------------*
025800140624        Begsr sr_lr48      ;
025900140624
026000140624        clear fnlr48ds;
026100140807        clear s_d48msg;
026200140625        clear dsarbg;
026300140625        clear dsarbd;
026400140625        clear dsarbk;
026500140625        clear darbt;
026600140624        d48aas=arbaas;
026700140624        d48lnp=arblnp;
026800140624        d48nrs=arbnrs;
026900140624        d48nsp=arbnsp;
027000140625        d48fgs=idcfgs;
027100140625        d48cbo=arbcbo;
027200140625        d48tbo='A'   ;
027300140624        select;
027400140925       // APPUNTAMENTO
027500140624        when IDCTIPODIS='1';
027600140804       //  if arbtc1='S' or arbtc2='S';
027700140804       //     d48cvb='CS';
027800140804       //  else;
027900140804       //     d48cvb='CR';
028000140804       //  endif;
028100140926           d48ffr='E'   ;
028200140925           exsr impo_xcvbCI;
028300140925       // FERMO DEPOSITO O INDIRIZZO ALTERNATIVO
028400140624        when IDCTIPODIS='2' or IDCTIPODIS='3';
028500140625           d48cvb='I0';
028600140625           d48trc='D';
028700140625           if idctipodis='2';
028800140926             d48ffr='E'   ;
028900140911             if idctipocli='M';
029000140910              d48cvb='I7';
029100140911             endif;
029200140625              §BDRSD=arbrsd;
029300140625              §BDRD2=wrsd2 ;
029400140625              §BDIND=arbIND;
029500140625              §BDCAD=arbCAD;
029600140625              §BDLOD=arbLOD;
029700140625              §BDPRD=arbPRD;
029800140625              §BDNZD=arbNzd;
029900140625              §BDFIN=arbfin;
030000140625              §BDFFD=idcffd;
030100141017              if §bdffd='S';
030200141017                 §bdffd='Y';
030300141017              endif;
030400140625           else ;
030500141001              if idcdcr>0 ;
030600140926                 d48ffr='F'   ;
030700140926              else;
030800140926                 d48ffr='E'   ;
030900140926              endif;
031000140926              exsr sr_ImpoArbdI0 ;
031100140625           endif;
031200140625       // P.IVA: PASSO SEMPRE ARBCPI PERCHÈ FNLR48R usa il campo solo se sped assegnato
031300140625       // e in questo caso in arbcpi c'è proprio la p.iva del destinatario
031400140625           §BDCPI  = ARBCPI;
031500141002           §BDREF  = IDCREF;
031600140625           §BDTEL  = IDCTELD;
031700141010           §bdpkf  = arbpkf;
031800141010           §bdvlf  = arbvlf;
031900140625
032000140804       // Altre istruzioni  (non entro in questa routine in questo caso)
032100140804       // when IDCTIPODIS='4';
032200140624        endsl;
032300140625
032400140925        exsr sr_calllr48;
032500140925
032600140926       // NON RISCONTRATI ERRORI: OPERAZIONI PER Tipo dispo "INDIRIZZO ALTERNATIVO"
032700141001       // Con DCR>0
032800140926        if (d48err = *blanks or d48err='Z')
032900141001           and  IDCTIPODIS='3' and idcdcr>0 ;
033000140926               exsr sr_IndAlt;
033100140926        endif;
033200140925
033300140925       // NON RISCONTRATI ERRORI:
033400140925        if d48err = *blanks or d48err='Z';
033500140925       // Richiamo fnlr48r per la seconda volta per caricare referente/telefoni/email
033600140925           if d48err='Z';
033700140925              s_d48msg=d48msg;
033800140925           endif;
033900140926           d48ffr='E'   ;
034000140925           clear d48err;
034100140925           clear d48msg;
034200140925           d48cvb='NE';
034300140925           d48trc='N';
034400140925           clear dsarbn;
034500140925           §BNU_SN='S';
034600140925           §BNUNI70=IDCMAILAVV;
034700140925           §BNR_SN='S';
034800140925           §BNREF=IDCREF;
034900140925           §BNTEL=IDCTELD;
035000140925           §BNC_SN='S';
035100140925           §BNCEL=IDCTELAVV;
035200140929           kpjbu=fnlr48ds;
035300140929           fnlr48r(kpjba:dsarbd:dsarbk:darbt:dsarbg:trul90ds:dsarbn);
035400140929           fnlr48ds=kpjbu;
035500140929           if d48err <> *blanks and d48err<>'Z' and d48ffr='E';
035600140929              IDCERR='2' ;
035700140929              IDCLOG=d48msg;
035800140929           endif;
035900140925        endif;
036000140925
036100140925       // NON RISCONTRATI ERRORI: imposto utente e data di elaborazione su TIIDC
036200140925        if d48err = *blanks or d48err='Z';
036300140925       // Memorizzo eventuali messaggi di avviso
036400140925              if s_d48msg<>*blanks;
036500140925                 d48msg=s_d48msg;
036600140925              endif;
036700140925              IDCLOG=d48msg;
036800140925              IDCELAUTE=KNMUS;
036900140925              IDCELADATA=%int( %subst( %char( %dec( %timestamp() ) )
037000140925                                 :1 :14 ) );
037100140925        endif;
037200140925
037300140925
037400140624        endsr;
037500140925       //-------------------------------------------------------------*
037600140925       //valorzzazione campi parm lr48 per consegna richiesta         *
037700140925       //-------------------------------------------------------------*
037800140925        Begsr impo_xcvbCI;
037900140925           d48cvb='CI';
038000140925           d48trc='G';
038100140925           §BGTCR=IDCTCR;
038200140925           §BGHCR=idchcr;
038300140930           §BGDCR=idcdcr;
038400140925           §BGGC1=arbgc1;
038500140925           §BGGC2=arbgc2;
038600140925       //   Giorni di chiusura --> se richiesta preferenza M/P la traduco nel primo campo
038700140925       //                          libero dei giorni di chiusura
038800140925           clear wggc;
038900140925           if idcprcr<>' ';
039000140925           if idcprcr='M';
039100140925              wggc='P';
039200140925           else;
039300140925              wggc='M';
039400140925           endif;
039500140925           endif;
039600140925           if wggc<>*blanks;
039700140925       // Nel primo byte  del giorno di chiusura devo mettere il numero del giorno della
039800140925       // settimana che corrisponde alla data cons rich.
039900140925       // Nel secondo byte metto mattino o pomeriggio
040000140925               dataiso=%date(§bgdcr:*iso);
040100140925               EXEC SQL SET :dw = DAYOFWEEK_ISO(:dataiso);
040200140925              if §BGGC1=*blanks;
040300140925                 %subst(§BGGC1:2:1)=wggc;
040400140925                 %subst(§BGGC1:1:1)=%editc(dw:'X');
040500140925              else;
040600140925                 %subst(§BGGC2:2:1)=wggc;
040700140925                 %subst(§BGGC2:1:1)=%editc(dw:'X');
040800140925              endif;
040900140925              if §BGGC1=§BGGC2;
041000140925                 clear §BGGC2;
041100140925              endif;
041200140925           endif;
041300140925           §BGTC1=arbtc1;
041400140925           §BGTC2=arbtc2;
041500140925        endsr;
041600140926       //-------------------------------------------------------------*
041700140926       //Impostazione DSARBD per cvb "I0"                             *
041800140926       //-------------------------------------------------------------*
041900140926        Begsr sr_ImpoArbdI0;
042000140926              §BDRSD=%subst(idcrsd:1:35);
042100140926              §BDRD2=%subst(IDCRSD:36:35);
042200140926              §BDIND=IDCIND;
042300140926              §BDCAD=IDCCAD;
042400140926              §BDLOD=IDCLOD;
042500140926              §BDPRD=IDCPRD;
042600140926              §BDNZD=IDCNAZ;
042700140926       // Se CAP/LOCALITA/PROVINCIA sono invariati rispetto alla bolla passo
042800140926       // flag inoltro = alla bolla altrimenti lo passo vuoto in modo da farlo ricalcolare
042900140926       // a fnlr48r in base ai nuovi dati
043000140926              if arbcad=idccad and arblod=idclod and arbprd=idcprd;
043100140926                §BDFIN = arbfin;
043200140926              endif;
043300140926              §BDFFD=arbffd;
043400140926        endsr;
043500140925       //-------------------------------------------------------------*
043600140925       //Richiamo FNLR48R                                             *
043700140925       //-------------------------------------------------------------*
043800140925        Begsr sr_calllr48;
043900140925        kpjbu=fnlr48ds;
044000141024       // if knsif=*blanks;
044100141024       //    knsif='*LIBL';
044200141024       // endif;
044300141024       // if knmus=*blanks ;
044400141024       //    knmus='INTERNET  ';
044500141024       // endif;
044600140925        fnlr48r(kpjba:dsarbd:dsarbk:darbt:dsarbg:trul90ds);
044700140925        fnlr48ds=kpjbu;
044800140926        if d48err <> *blanks and d48err<>'Z' and d48ffr='E';
044900140925           IDCERR='2' ;
045000140925           IDCLOG=d48msg;
045100140926        endif;
045200140925        endsr;
045300140926       //-------------------------------------------------------------*
045400140926       //Richiami ulteriori lr48 per Indirizzo alternativo se ricevuta anche una DCR
045500140926       //-------------------------------------------------------------*
045600140926        Begsr sr_IndAlt;
045700140926
045800140926       // Richiamo lr48 anche per la variazione di Cons.Rich.
045900140926        if d48err='Z';
046000140926           s_d48msg=d48msg;
046100140926        endif;
046200140926        exsr impo_xcvbCI;
046300140926        d48ffr='F'   ;
046400140926        exsr sr_calllr48;
046500140926
046600140926       // NON RISCONTRATI ERRORI --> rieseguo call
046700140926       //                        a FNLR48R per l'aggiornamento
046800140926
046900140926        if d48err = *blanks or d48err='Z';
047000140926
047100140926       // Richiamo per "I0"
047200140926
047300140926           d48cvb='I0';
047400140926           d48trc='D';
047500140926           d48ffr='E'   ;
047600140926           exsr sr_ImpoArbdI0 ;
047700140926       // P.IVA: PASSO SEMPRE ARBCPI PERCHÈ FNLR48R usa il campo solo se sped assegnato
047800140926       // e in questo caso in arbcpi c'è proprio la p.iva del destinatario
047900140926           §BDCPI  = ARBCPI;
048000140926           §BDREF  = IDCREF;
048100140926           §BDTEL  = IDCTELD;
048200141010           §bdpkf  = arbpkf;
048300141010           §bdvlf  = arbvlf;
048400140926           exsr sr_calllr48;
048500140926
048600140926       // Richiamo per "CI"
048700140926
048800141222       //     devo richainare la bolla FNARB per recepire le variazioni fatte dalla chiamata
048900141222       //     precedente. In particolare devo recepire evenutali consegne particolari (es. passaggio
049000141222       //     da destinatario "normale" a destinatario che prevede appuntamento. in questo
049100141222       //     caso lr48 durante la prcedente chiamata oltre ad eseguire un I0 ha eseguito anche una
049200141222       //     "CP" per aggiungere la "A" di appuntamento che ora dovrò passare nel prossimo richiamo
049300141222
049400141222              exsr sr_bolla;
049500141222
049600140926           exsr impo_xcvbCI;
049700140926           d48ffr='E'   ;
049800140926           exsr sr_calllr48;
049900140926        endif;
050000140926        endsr;
050100140626       //-------------------------------------------------------------*
050200140626       //inserimento Richiesta Assistenza                             *
050300140626       //-------------------------------------------------------------*
050400140626        Begsr sr_InsRA;
050500140626
050600140626           clear fidna6ds;
050700140626
050800151116           iDA06MAD = ' 86';
050900140626
051000140626           select;
051100140627
051200140627       // APPUNTAMENTO
051300140627
051400140626           when IDCTIPODIS='1';
051500141002              notauni = 'Consegna Richiesta';
051600140627              dataeur=%date(IDCDCR:*iso);
051700141001       //     ida06no2= 'Cons.Rich.' ;
051800140627              select;
051900140627              when IDCTCR = *blanks ;
052000141002                 %subst(notauni:36)= 'Il:'  ;
052100140627              when IDCTCR = 'D'     ;
052200141002                 notauni= %trim(notauni) + 'Dopo il:'  ;
052300140627              when IDCTCR = 'P'     ;
052400141002                 notauni= %trim(notauni) + 'Prima del:'  ;
052500140627              endsl;
052600141002              notauni=%trim(notauni)+ ' '+ %editw(%dec(dataeur):'  /  /    ');
052700140627              if idchcr>0;
052800141002                 notauni=%trim(notauni) + ' Ore ' + %editw(idchcr:'  :  ');
052900140627              endif;
053000140627              if idcprcr<>*blanks;
053100140627              select;
053200140627              when idcprcr='M';
053300141002                 notauni=%trim(notauni) + ' Mattino';
053400140627              when idcprcr='P';
053500141002                 notauni=%trim(notauni) + ' Pomeriggio';
053600140627              endsl;
053700140627              endif;
053800140627
053900140627       // FERMO DEPOSITO
054000140627
054100140626           when IDCTIPODIS='2';
054200141002              notauni = 'Fermo Deposito';
054300151116       // R E S O
054400151116           when IDCTIPODIS='5';
054500151116              notauni  = 'R  E  S  O';
054600140627
054700140627       // CONSEGNA AD ALTRO INDIRIZZO
054800140627
054900140626           when IDCTIPODIS='3';
055000141002              notauni  = 'Indirizzo Alternativo';
055100141006             if idcnewlna<>0;
055200141006              notauni=%trim(notauni)+ '-Nuova Lna ' + %editc(idcnewlna:'X');
055300141006             endif;
055400141002              %subst(notauni:36)  = %trim(idcrsd) + ' ' + %trim(idcind) +
055500141002                                    ' ' + %trim(idccad) + ' ' + %trim(idclod) +
055600141002                                    ' ' + %trim(idcprd) + ' ' + %trim(idcnaz);
055700141001             if idcdcr>0 ;
055800141002              n= %checkr(' ':notauni);
055900141002              nrighe=%div(n:35) ;
056000141002              if %rem(n:35)>0;
056100141002                 nrighe+=1;
056200141002              endif;
056300141002              n=(nrighe*35)+1;
056400141002              %subst(notauni:n)= 'Cons.Rich.' ;
056500141001             endif;
056600141001             if idcdcr>0;
056700140930              dataeur=%date(IDCDCR:*iso);
056800140930              select;
056900140930              when IDCTCR = *blanks ;
057000141002                 notauni = %trim(notauni) + 'Il'  ;
057100140930              when IDCTCR = 'D'     ;
057200141002                 notauni= %trim(notauni) + 'Dopo il'  ;
057300140930              when IDCTCR = 'P'     ;
057400141002                 notauni= %trim(notauni) + 'Prima del'  ;
057500140930              endsl;
057600141002             notauni=%trim(notauni)+' '+%editw(%dec(dataeur:*dmy):'  /  /  ');
057700141001             endif;
057800140930              if idchcr>0;
057900141002                 notauni=%trim(notauni) + ' Ore ' + %editw(idchcr:'  :  ');
058000140930              endif;
058100140930              if idcprcr<>*blanks;
058200140930              select;
058300140930              when idcprcr='M';
058400141002                 notauni=%trim(notauni) + ' Mattino';
058500140930              when idcprcr='P';
058600141002                 notauni=%trim(notauni) + ' Pomeriggio';
058700140930              endsl;
058800140930              endif;
058900141001       // ALTRE INDICAZIONI
059000141001           when IDCTIPODIS='4';
059100141002              notauni = 'Altre Indicazioni';
059200141002              %subst(notauni:36)= %trim(idcnote);
059300140626           endsl;
059400140627       // referente e telefono + eventuale cell e indirizzo email
059500140930       //     ida06no2=%trim(ida06no2)+  ' Ref.Cons: ' + %trim(idcref) +
059600140930       //     ' ' + %trim(idcteld);
059700140627           if IDCTELAVV  <> *blanks;
059800141002            if %checkr(' ':notauni)<35;
059900141002              %subst(notauni:36)= 'Cell:' + %trim(idctelavv);
060000141002            else;
060100141002              notauni=%trim(notauni) + ' Cell:' +
060200140627              %trim(idctelavv);
060300141002            endif;
060400140627           endif;
060500141001           if IDCMAILAVV <> *blanks;
060600141002            if %checkr(' ':notauni)<35;
060700141002             %subst(notauni:36)= 'Email:' + %trim(idcmailavv);
060800141002            else;
060900141002             notauni=%trim(notauni) + ' Email:' + %trim(idcmailavv);
061000141002            endif;
061100141001           endif;
061200140627
061300140926       //  ida06no1=%trim(ida06no1) + ' - Richiesto da';
061400140926       //  if idctipocli ='M';
061500140926       //  ida06no1=%trim(ida06no1) + ' Mitt.';
061600140926       //  else;
061700140926       //  ida06no1=%trim(ida06no1) + ' Dest.';
061800140926       //  endif;
061900140926       //  if idcidcli<>*blanks ;
062000140926       //    ida06no1=%trim(ida06no1) + ' Id.cli. ' + idcidcli;
062100140926       //  endif;
062200140926       //    dataeur=%date(%dec(%subst(%editc(idcinsdata:'X'):1:8):8:0)
062300140926       //            :*iso);
062400140926       //    wgg=     %subdt(dataeur:*days);
062500140926       //    wmm=     %subdt(dataeur:*months);
062600140926       //    waa=     %subdt(dataeur:*years);
062700140926       //    ida06no1=%trim(ida06no1) + ' in data ' +
062800140926       //             %editc(wgg:'X') + '/' +
062900140926       //             %editc(wmm:'X') + '/' +
063000140926       //             %editc(waa:'X') + ' alle ' +
063100140926       //             %subst(%editc(idcinsdata:'X'):9:2) + ':'  +
063200140926       //             %subst(%editc(idcinsdata:'X'):11:2)        ;
063300141002           if idcip<>*blanks ;
063400141002            if %checkr(' ':notauni)<35;
063500141002             %subst(notauni:36)= 'IP:' + idcip;
063600141002            else;
063700141002             notauni=%trim(notauni) + ' IP:' + idcip;
063800141002            endif;
063900141002           endif;
064000140626
064100140626           iDA06TOR = 'S';
064200140926           iDA06OGG = %trim(%editc(idcLNP:'X')) +
064300140926                      %trim(%editc(idcNRS:'X')) +
064400140926                      %trim(%editc(idcNSP:'X')) +
064500140926                      %trim(%editc(idcAAS:'X'));
064600141023           if idcmodacc<>'B';
064700141022              iDA06UTE = 'INTERNET';
064800141022           else;
064900141022              iDA06UTE = idcelaute ;
065000141022           endif;
065100141008           if arblna>0;
065200141008              iDA06FIL = arblna;
065300141008           else;
065400141008              iDA06FIL = idcfgs;
065500141008           endif;
065600140926           ida06dar = %dec(%subst(%editc(idcinsdata:'X'):1:8):8:0);
065700140926           ida06orr = %dec(%subst(%editc(idcinsdata:'X'):9:6):6:0);
065800141022           if idcmodacc<>'B';
065900141022              if idctipocli='M';
066000141022                 ida06rsc = idcidcli;
066100141022              else;
066200141022                 ida06rsc = 'NON CODIFICATO';
066300141022              endif;
066400141022           endif;
066500141001           %subst(iDA06rsc:35:1) = idctipodis;
066600140926           iDA06pog = idcfgs;
066700141023           if idcmodacc<>'B';
066800141023              iDA06tco = 'I'   ;
066900141023           endif;
067000140926           iDA06tch = idctipocli;
067100140926
067200140926       // Non chiudo R.A. se la dispo non è stata caricata
067300140926           if idceladata=0;
067400140926               iDA06cdt = '999'     ;
067500150624               IDA06PRT = '6'       ;
067600150624               IDA06rpr = 'ROM'     ;
067700140926           endif;
067800140930           iDA06ref = idcref  ;
067900140926           iDA06tel = idcteld ;
068000140626
068100140925           callP(e) FIDNA6R (fidna6ds:fidna6bds);
068200140626        endsr;
068300140626
068400141022       //-------------------------------------------------------------*
068500141022       //Forzatura giro DIR per richieste di dirottamento             *
068600141022       //-------------------------------------------------------------*
068700141022        Begsr sr_fiarg;
068800141022           chain (idcaas:idclnp:idcnrs:idcnsp) fiarg01l;
068900141022           if not %found(fiarg01l);
069000141022              clear fiarg000;
069100141022              argaas  =  idcaas;
069200141022              arglnp  =  idclnp;
069300141022              argnrs  =  idcnrs;
069400141022              argnsp  =  idcnsp;
069500141022              arglna  =  arblna;
069600141022       // p.o. di consegna capofila della £6 del p.o. di arrivo
069700141022              clear  fnlv55ds;
069800141022              d55tpt = '6' ;
069900141022              d55lin=arblna;
070000141023              d55drf=%dec(%date());
070100141022              fnlv55r(fnlv55ds);
070200141022              argpoc  =  d55tfa;
070300141022           endif;
070400141022           argcgi='DIR';
070500141022           argtgi='M';
070600141022           if not %found(fiarg01l);
070700141022              write fiarg000;
070800141022           else;
070900141022              update fiarg000;
071000141022           endif;
071100141022        endsr;

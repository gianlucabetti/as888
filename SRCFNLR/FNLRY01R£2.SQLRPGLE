000100110617      *---------------------------------------------------------------*
000200140624      * Disposizioni di Consegna: validazione su as400                *
000300110617      *---------------------------------------------------------------*
000400110617
000500110617     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600140624     h dftactgrp(*no) actgrp(*caller)
000601141024     h bnddir('UBRTVNETA')
000700110617
000800110617      *---------------------------------------------------------------*
000900110617
001000140908     ftiidc04l  uf   e           k disk
001001140625     ffnarb01l  if   e           k disk
001002140625     ffiar401l  if   e           k disk
001003141022     ffiarg01l  uf a e           k disk
001004140626     D FIDNA6R         PR
001005140626     D                                     extpgm('FIDNA6R')
001006140626     D fidna6ds                            likeds(fidna6ds)
001008140925     D fidna6bds                           likeds(fidna6bds)
001009141022     D fnlv55r         PR
001010141022     D                                     extpgm('FNLV55R')
001011141022     D fnlv55ds                            likeds(fnlv55ds)
003900140624     d tiidcds       e ds                  extname(tiidc00f)
003901140625     d fnlry00m0     e ds
003902140625     d fnlry00f0     e ds                  qualified inz(*extdft)
004701140625     d kpjba         e ds
004702140625     d fnlr48ds      e ds
004703140626     d dsarbd        e ds                  inz
004704140626     d dsarbk        e ds                  inz
004705140626     d darbt         e ds                  inz
004706140626     d dsarbg        e ds                  inz
004708140625     d trul90ds      e ds                  inz
004709140626     d dsarbn        e ds                  inz
004710140626     d fidna6ds      e ds                  inz
004712141002     d* campo notauni comprende ida06no1+ida06no2
004713141002     d  notauni               25    444
004715140925     d fidna6bds     e ds                  inz
004716141023     d fnlv55ds      e ds                  inz
004717140908     d tibs02ds      e ds
004718140908     d divp          e ds
006100110617      * - Campi di comodo
006101140626     d dataeur         s               d   datfmt(*eur)
006102140627     d dataiso         s               d   datfmt(*iso)
006200140624     d werrchk         s              1
006501140625     d wrsd2           s             35
006502140625     d wggc            s                   like(idcprcr)
006503140627     d wgg             s              2  0
006504140627     d wmm             s              2  0
006505140627     d waa             s              4  0
006506140627     d dw              s              1s 0
006507140806     d s_d48msg        s                   like(d48msg)
006508141002     d n               s              4  0
006509141002     d nrighe          s              2  0
006510140625
006511140625     D prmRqsOpCode    s             10I 0
006512140625     D prmRpyOpCode    s             10I 0
006513140625     D prmRpyIdMsg     s             10I 0
006514140625     D prmRqsFormato   s             10A
006515140625     D prmRqsData      s          32767A
006516140625     D prmRqsDataSize  s             10I 0
006517140625     D prmRpyFormato   s             10a
006518140625     D prmRpyData      s          32767
006519140625     D prmRpyDataSize  s             10I 0
006520140625     D prmRpyFormMsg   s             10A
006521140625     D prmRpyMessage   s          32767
006522140625     D prmRpyMsgSize   s             10I 0
006523140625     D prmRpyFormForz  s             10A
006524140625     D prmRpyForzatu   s          32767
006525140625     D prmRpyForSize   s             10I 0
008000130406
008001140908       //--------------------------------------------------------------
008002140908       //?Definizione procedure.
008003140908       //--------------------------------------------------------------
008004140908     d tibs02r         pr                  extpgm('TIBS02R')
008005140908     d  kpjba                              likeds(kpjba)
008006140908     d  tibs02ds                           likeds(tibs02ds)
016400110617
016500110617      // ? PROTOTIPI ?
016501140625      /copy gaitrasrc/srcprotopr,Fnlry00r
016502140625      /copy gaitrasrc/srcprotopr,fnlr48r
016503140625      /copy gaitrasrc/srcconst,FNLRY00R
016504141024       // -?Nome del sistema?
016505141024     d currSysNeta     s              8a   inz
016506141024       // -?Reperimento NETA sistema AS/400 corrente?
016507141024      /copy gaitrasrc/srcProtoPR,UBRTVNETA
017100110617
017200130406
017300110627     C     *ENTRY        PLIST
017400110627     C                   PARM                    chiudi            1
017500130406
017600110617      /free
017601141024         // -?Verifica del sistema AS/400 corrente?
017602141024       if  UBRTVNETA_Rtv(currSysNeta) =  *zero;
017606141024
017607141024         // -?Imposto knsif e knmus ?
017608141024         if  %subst( currSysNetA : 1 : 6 ) = 'SETRAS';
017609141024           knsif  = 'FILTRA201 ';
017610141024         else;
017611141024           knsif  = 'FILTRAPRD ';
017612141024         endif;
017613141024         knmus='INTERNET';
017616141024
017617140908       // Reperimento dati tabella IVP - Variabili programma per INTERNET
017618140908       clear tibs02ds;
017619140908       clear divp;
017620140908       t02mod='C';
017621140908       t02cod='IVP';
017622140908       t02ke1='1  ';
017623140908       t02tla='L  ';
017624140908       tibs02r(kpjba:tibs02ds);
017625140908       if t02err=*blanks;
017626140908          divp=t02uni;
017627140908       endif;
017900130625
018400140624          // Leggo TIIDC  da elaborare
018500130604          exsr sr_Elabora;
018501141024
018502141024       endif;
018700130406
018800110627       if chiudi='S';
018900140623          eval *inlr=*on;
018901140626          D48TLA='C';
018902140626          kpjbu=fnlr48ds;
018903140626          fnlr48r(kpjba:dsarbd:dsarbk:darbt:dsarbg:trul90ds);
019400140623       else;
019500140623          eval *inrt=*on;
019600110627       endif;
020100130406
029600110617       //-------------------------------------------------------------*
029700140624       //Lettura TIIDC record da elaborare                             *
029800110617       //-------------------------------------------------------------*
029900110617       Begsr SR_Elabora;
030000140908        setll (*loval) tiidc04l;
030100140908        read           tiidc04l;
030200130605
030600140908  1     dow not %eof(tiidc04l) ;
030700130406
030800130502        // controllo se è stata richiesta la chiusura del sottosistema
030900110627           if %shtdn;
031000110627               chiudi='S';
031100110627               leavesr;
031200110627           endif;
032000130406
032001141022           werrchk='0';
032002141022       // Se dispo inserite da AS/400 (utente BRT) non eseguo chkistruco
032003141022           if idcmodacc<>'B';
032004141022               exsr sr_chkistruco;
032005141022           endif;
032006141008       // Chaino arb a prescindere da eventuale errore chkistruco perchè arblna mi
032007141008       // serve per creazione R.A. che viene fatta SEMPRE
032008141008             exsr sr_bolla;
032009140926
032010140624           if werrchk='0';
032011141022       // Altri controlli per monitorare situazioni non caricabili in automatico
032012140919             exsr sr_altrichk;
032013140919             if werrchk='0';
032014140926                 if IDCTIPODIS<>'4' and idcnewlna=0 and §ivpindm=' ';
032015140919                   exsr sr_lr48;
032016140919                 endif;
032017140919             endif             ;
032018140919           else;
032019140919               idcerr='1';
032020140919               IDCLOG=%subst(textmsg:1:200);
032021140919           endif;
032022140926
032023140926        // Sempre: Creazione R.A.
032024141219           if %scan ('salvataggio in corso':idclog)=0;
032025140926           exsr sr_InsRa;
032026140926           IDCANT= ODA6BANT;
032027140926           IDCNUT= ODA6BNUT;
032028140926           IDCPDT= ODA6BPDT;
032029140806           IDCELAFLG='E';
032030141219           endif;
032031140624           update tiidc000;
032032141022
032033141022        // per le richieste di dirottamento forzo il giro 'DIR'
032034141022           if werrchk='0' and idctipodis='3' and idcnewlna>0;
032035141022              exsr sr_fiarg;
032036141022           endif;
032037140926
032800130406
032900140908           read         tiidc04l ;
033000110617        enddo;
033100130406
033200110617        endsr;
033201140624       //-------------------------------------------------------------*
033202140624       //Richiamo pgm di controllo eseguibilità dell'indicazione      *
033203140624       //-------------------------------------------------------------*
033204140624        Begsr sr_ChkIstruCo;
033205140624
033300130406
033301140624        prmrqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO    ;
033302140624        prmrqsformato='TIIDCDS  '  ;
033303140624        prmrpyformato='         '  ;
033304140624       prmRpyFormMsg='FNLRY00M0'  ;
033345140624
033346140624         prmrqsData = tiidcds  ;
033347140624         prmrqsDatasize = %SIZE(tiidcds);
033348140624         prmrpyDatasize = %SIZE(tiidcds);
033349140624         prmRpyMsgSize  = %SIZE(fnlry00m0);
033350140624         prmRpyForSize  = %SIZE(fnlry00f0);
033351140624         Fnlry00_Immetti(prmRqsOpCode : prmRpyOpCode : prmRpyIdMsg :
033352140624                         prmRqsFormato : prmRqsData : prmRqsDataSize:
033353140624                         prmRpyFormato : prmRpyData : prmRpyDataSize:
033354140624                         prmRpyFormMsg : prmRpyMessage : prmRpyMsgSize) ;
033355140624         //                prmRpyFormForz: prmRpyForzatu :prmRpyForSize) ;
033400130406
033401140624 5       if   prmrpyOpCode = FNLRY00_RPYOPCODE_ERROR  ;
033402140624            werrchk='1';
033403140626            fnlry00m0=prmrpymessage;
033404140624         endif;
033405140624
033406140624        endsr;
033408140919
033409140919       //-------------------------------------------------------------*
033410140919       //lettura bolla in arrivo                                      *
033411140919       //-------------------------------------------------------------*
033412140919        Begsr sr_bolla   ;
033413140919
033414140919             clear wrsd2;
033415141008             clear fnarb000;
033416140919
033417140919             chain (idcaas:idclnp:idcnrs:idcnsp) fnarb01l;
033418140919             chain (arbaas:arblnp:arbnrs:arbnsp:'D') fiar401l ;
033419140919             if %found(fiar401l);
033420140919                eval wrsd2=%subst(ar4not:1:35);
033421140919             endif;
033422140919        endsr;
033423140919       //-------------------------------------------------------------*
033424140919       //Altri controlli                                              *
033425140919       //-------------------------------------------------------------*
033426140919        Begsr sr_altrichk;
033427140919       // L'indicazione non è caricabile in automatico se:
033428140919       // 1) ricevute indicazioni di "Consegna Richiesta" e la bolla
033430140919       //    ha la consegna particolare a "Supermercato"
033431140919       // 2) ricevute indicazioni di "Indirizzo Alternativo" e la bolla è in distinta
033432140919        select;
033435141001        when (arbtc1='S' or arbtc2='S') and
033436141001             (IDCTIPODIS='1' or (IDCTIPODIS='3' and idcdcr>0));
033437140919          werrchk='1';
033438140919          idcerr='3';
033439141015          IDCLOG='Spedizione con Cons. Particolare ai Supermercati';
033440141015       //         caricare manualmente utilizzando la causale "CS"';
033441141015        when IDCTIPODIS='3'  and arbndc>0 and §IVPATIA=' ';
033442140919          idcerr='3';
033443140919          IDCLOG='Spedizione in distinta';
033444140919          werrchk='1';
033445140919        endsl;
033446140919        endsr;
033447140624       //-------------------------------------------------------------*
033448140624       //Richiamo pgm di caricamento variazioni bolle FNLR48R         *
033449140624       //-------------------------------------------------------------*
033450140624        Begsr sr_lr48      ;
033451140624
033452140624        clear fnlr48ds;
033453140807        clear s_d48msg;
033454140625        clear dsarbg;
033455140625        clear dsarbd;
033456140625        clear dsarbk;
033457140625        clear darbt;
033458140624        d48aas=arbaas;
033459140624        d48lnp=arblnp;
033460140624        d48nrs=arbnrs;
033461140624        d48nsp=arbnsp;
033462140625        d48fgs=idcfgs;
033463140625        d48cbo=arbcbo;
033464140625        d48tbo='A'   ;
033465140624        select;
033466140925       // APPUNTAMENTO
033467140624        when IDCTIPODIS='1';
033468140804       //  if arbtc1='S' or arbtc2='S';
033469140804       //     d48cvb='CS';
033470140804       //  else;
033471140804       //     d48cvb='CR';
033472140804       //  endif;
033473140926           d48ffr='E'   ;
033474140925           exsr impo_xcvbCI;
033508140925       // FERMO DEPOSITO O INDIRIZZO ALTERNATIVO
033509140624        when IDCTIPODIS='2' or IDCTIPODIS='3';
033510140625           d48cvb='I0';
033511140625           d48trc='D';
033512140625           if idctipodis='2';
033513140926             d48ffr='E'   ;
033514140911             if idctipocli='M';
033515140910              d48cvb='I7';
033516140911             endif;
033517140625              §BDRSD=arbrsd;
033518140625              §BDRD2=wrsd2 ;
033519140625              §BDIND=arbIND;
033520140625              §BDCAD=arbCAD;
033521140625              §BDLOD=arbLOD;
033522140625              §BDPRD=arbPRD;
033523140625              §BDNZD=arbNzd;
033524140625              §BDFIN=arbfin;
033525140625              §BDFFD=idcffd;
033526141017              if §bdffd='S';
033527141017                 §bdffd='Y';
033528141017              endif;
033529140625           else ;
033530141001              if idcdcr>0 ;
033531140926                 d48ffr='F'   ;
033532140926              else;
033533140926                 d48ffr='E'   ;
033534140926              endif;
033535140926              exsr sr_ImpoArbdI0 ;
033547140625           endif;
033548140625       // P.IVA: PASSO SEMPRE ARBCPI PERCHÈ FNLR48R usa il campo solo se sped assegnato
033549140625       // e in questo caso in arbcpi c'è proprio la p.iva del destinatario
033550140625           §BDCPI  = ARBCPI;
033551141002           §BDREF  = IDCREF;
033552140625           §BDTEL  = IDCTELD;
033553141010           §bdpkf  = arbpkf;
033554141010           §bdvlf  = arbvlf;
033555140625
033556140804       // Altre istruzioni  (non entro in questa routine in questo caso)
033557140804       // when IDCTIPODIS='4';
033558140624        endsl;
033559140625
033560140925        exsr sr_calllr48;
033561140925
033562140926       // NON RISCONTRATI ERRORI: OPERAZIONI PER Tipo dispo "INDIRIZZO ALTERNATIVO"
033563141001       // Con DCR>0
033564140926        if (d48err = *blanks or d48err='Z')
033567141001           and  IDCTIPODIS='3' and idcdcr>0 ;
033568140926               exsr sr_IndAlt;
033569140926        endif;
033602140925
033603140925       // NON RISCONTRATI ERRORI:
033604140925        if d48err = *blanks or d48err='Z';
033605140925       // Richiamo fnlr48r per la seconda volta per caricare referente/telefoni/email
033606140925           if d48err='Z';
033607140925              s_d48msg=d48msg;
033608140925           endif;
033609140926           d48ffr='E'   ;
033610140925           clear d48err;
033611140925           clear d48msg;
033612140925           d48cvb='NE';
033613140925           d48trc='N';
033615140925           clear dsarbn;
033616140925           §BNU_SN='S';
033617140925           §BNUNI70=IDCMAILAVV;
033618140925           §BNR_SN='S';
033619140925           §BNREF=IDCREF;
033620140925           §BNTEL=IDCTELD;
033621140925           §BNC_SN='S';
033622140925           §BNCEL=IDCTELAVV;
033623140929           kpjbu=fnlr48ds;
033624140929           fnlr48r(kpjba:dsarbd:dsarbk:darbt:dsarbg:trul90ds:dsarbn);
033625140929           fnlr48ds=kpjbu;
033626140929           if d48err <> *blanks and d48err<>'Z' and d48ffr='E';
033627140929              IDCERR='2' ;
033628140929              IDCLOG=d48msg;
033629140929           endif;
033630140925        endif;
033631140925
033632140925       // NON RISCONTRATI ERRORI: imposto utente e data di elaborazione su TIIDC
033633140925        if d48err = *blanks or d48err='Z';
033634140925       // Memorizzo eventuali messaggi di avviso
033635140925              if s_d48msg<>*blanks;
033636140925                 d48msg=s_d48msg;
033637140925              endif;
033638140925              IDCLOG=d48msg;
033639140925              IDCELAUTE=KNMUS;
033640140925              IDCELADATA=%int( %subst( %char( %dec( %timestamp() ) )
033641140925                                 :1 :14 ) );
033642140925        endif;
033643140925
033644140925
033645140624        endsr;
033646140925       //-------------------------------------------------------------*
033647140925       //valorzzazione campi parm lr48 per consegna richiesta         *
033648140925       //-------------------------------------------------------------*
033649140925        Begsr impo_xcvbCI;
033650140925           d48cvb='CI';
033651140925           d48trc='G';
033652140925           §BGTCR=IDCTCR;
033653140925           §BGHCR=idchcr;
033654140930           §BGDCR=idcdcr;
033658140925           §BGGC1=arbgc1;
033659140925           §BGGC2=arbgc2;
033660140925       //   Giorni di chiusura --> se richiesta preferenza M/P la traduco nel primo campo
033661140925       //                          libero dei giorni di chiusura
033662140925           clear wggc;
033663140925           if idcprcr<>' ';
033664140925           if idcprcr='M';
033665140925              wggc='P';
033666140925           else;
033667140925              wggc='M';
033668140925           endif;
033669140925           endif;
033670140925           if wggc<>*blanks;
033671140925       // Nel primo byte  del giorno di chiusura devo mettere il numero del giorno della
033672140925       // settimana che corrisponde alla data cons rich.
033673140925       // Nel secondo byte metto mattino o pomeriggio
033674140925               dataiso=%date(§bgdcr:*iso);
033675140925               EXEC SQL SET :dw = DAYOFWEEK_ISO(:dataiso);
033676140925              if §BGGC1=*blanks;
033677140925                 %subst(§BGGC1:2:1)=wggc;
033678140925                 %subst(§BGGC1:1:1)=%editc(dw:'X');
033679140925              else;
033680140925                 %subst(§BGGC2:2:1)=wggc;
033681140925                 %subst(§BGGC2:1:1)=%editc(dw:'X');
033682140925              endif;
033683140925              if §BGGC1=§BGGC2;
033684140925                 clear §BGGC2;
033685140925              endif;
033686140925           endif;
033687140925           §BGTC1=arbtc1;
033688140925           §BGTC2=arbtc2;
033689140925        endsr;
033690140926       //-------------------------------------------------------------*
033691140926       //Impostazione DSARBD per cvb "I0"                             *
033692140926       //-------------------------------------------------------------*
033693140926        Begsr sr_ImpoArbdI0;
033694140926              §BDRSD=%subst(idcrsd:1:35);
033695140926              §BDRD2=%subst(IDCRSD:36:35);
033696140926              §BDIND=IDCIND;
033697140926              §BDCAD=IDCCAD;
033698140926              §BDLOD=IDCLOD;
033699140926              §BDPRD=IDCPRD;
033700140926              §BDNZD=IDCNAZ;
033701140926       // Se CAP/LOCALITA/PROVINCIA sono invariati rispetto alla bolla passo
033702140926       // flag inoltro = alla bolla altrimenti lo passo vuoto in modo da farlo ricalcolare
033703140926       // a fnlr48r in base ai nuovi dati
033704140926              if arbcad=idccad and arblod=idclod and arbprd=idcprd;
033705140926                §BDFIN = arbfin;
033706140926              endif;
033707140926              §BDFFD=arbffd;
033708140926        endsr;
033709140925       //-------------------------------------------------------------*
033710140925       //Richiamo FNLR48R                                             *
033711140925       //-------------------------------------------------------------*
033712140925        Begsr sr_calllr48;
033714140925        kpjbu=fnlr48ds;
033715141024       // if knsif=*blanks;
033716141024       //    knsif='*LIBL';
033717141024       // endif;
033718141024       // if knmus=*blanks ;
033719141024       //    knmus='INTERNET  ';
033720141024       // endif;
033721140925        fnlr48r(kpjba:dsarbd:dsarbk:darbt:dsarbg:trul90ds);
033722140925        fnlr48ds=kpjbu;
033723140926        if d48err <> *blanks and d48err<>'Z' and d48ffr='E';
033724140925           IDCERR='2' ;
033725140925           IDCLOG=d48msg;
033726140926        endif;
033727140925        endsr;
033728140926       //-------------------------------------------------------------*
033729140926       //Richiami ulteriori lr48 per Indirizzo alternativo se ricevuta anche una DCR
033730140926       //-------------------------------------------------------------*
033731140926        Begsr sr_IndAlt;
033732140926
033733140926       // Richiamo lr48 anche per la variazione di Cons.Rich.
033734140926        if d48err='Z';
033735140926           s_d48msg=d48msg;
033736140926        endif;
033737140926        exsr impo_xcvbCI;
033738140926        d48ffr='F'   ;
033739140926        exsr sr_calllr48;
033740140926
033741140926       // NON RISCONTRATI ERRORI --> rieseguo call
033742140926       //                        a FNLR48R per l'aggiornamento
033743140926
033744140926        if d48err = *blanks or d48err='Z';
033745140926
033746140926       // Richiamo per "I0"
033747140926
033748140926           d48cvb='I0';
033749140926           d48trc='D';
033750140926           d48ffr='E'   ;
033751140926           exsr sr_ImpoArbdI0 ;
033752140926       // P.IVA: PASSO SEMPRE ARBCPI PERCHÈ FNLR48R usa il campo solo se sped assegnato
033753140926       // e in questo caso in arbcpi c'è proprio la p.iva del destinatario
033754140926           §BDCPI  = ARBCPI;
033755140926           §BDREF  = IDCREF;
033756140926           §BDTEL  = IDCTELD;
033757141010           §bdpkf  = arbpkf;
033758141010           §bdvlf  = arbvlf;
033759140926           exsr sr_calllr48;
033760140926
033761140926       // Richiamo per "CI"
033762140926
033763141222       //     devo richainare la bolla FNARB per recepire le variazioni fatte dalla chiamata
033764141222       //     precedente. In particolare devo recepire evenutali consegne particolari (es. passaggio
033765141222       //     da destinatario "normale" a destinatario che prevede appuntamento. in questo
033766141222       //     caso lr48 durante la prcedente chiamata oltre ad eseguire un I0 ha eseguito anche una
033767141222       //     "CP" per aggiungere la "A" di appuntamento che ora dovrò passare nel prossimo richiamo
033768141222
033769141222              exsr sr_bolla;
033770141222
033771140926           exsr impo_xcvbCI;
033772140926           d48ffr='E'   ;
033773140926           exsr sr_calllr48;
033774140926        endif;
033775140926        endsr;
033776140626       //-------------------------------------------------------------*
033777140626       //inserimento Richiesta Assistenza                             *
033800140626       //-------------------------------------------------------------*
033900140626        Begsr sr_InsRA;
034000140626
034100140626           clear fidna6ds;
034200140626
034300140627           iDA06MAD = ' 86';
034400140626
034700140626
034701140626           select;
034702140627
034703140627       // APPUNTAMENTO
034704140627
034705140626           when IDCTIPODIS='1';
034706141002              notauni = 'Consegna Richiesta';
034710140627              dataeur=%date(IDCDCR:*iso);
034711141001       //     ida06no2= 'Cons.Rich.' ;
034712140627              select;
034713140627              when IDCTCR = *blanks ;
034714141002                 %subst(notauni:36)= 'Il:'  ;
034715140627              when IDCTCR = 'D'     ;
034716141002                 notauni= %trim(notauni) + 'Dopo il:'  ;
034717140627              when IDCTCR = 'P'     ;
034718141002                 notauni= %trim(notauni) + 'Prima del:'  ;
034719140627              endsl;
034720141002              notauni=%trim(notauni)+ ' '+ %editw(%dec(dataeur):'  /  /    ');
034721140627              if idchcr>0;
034722141002                 notauni=%trim(notauni) + ' Ore ' + %editw(idchcr:'  :  ');
034723140627              endif;
034724140627              if idcprcr<>*blanks;
034725140627              select;
034726140627              when idcprcr='M';
034727141002                 notauni=%trim(notauni) + ' Mattino';
034728140627              when idcprcr='P';
034729141002                 notauni=%trim(notauni) + ' Pomeriggio';
034730140627              endsl;
034731140627              endif;
034732140627
034733140627       // FERMO DEPOSITO
034734140627
034735140626           when IDCTIPODIS='2';
034736141002              notauni = 'Fermo Deposito';
034737140627
034738140627       // CONSEGNA AD ALTRO INDIRIZZO
034739140627
034740140626           when IDCTIPODIS='3';
034741141002              notauni  = 'Indirizzo Alternativo';
034742141006             if idcnewlna<>0;
034743141006              notauni=%trim(notauni)+ '-Nuova Lna ' + %editc(idcnewlna:'X');
034744141006             endif;
034745141002              %subst(notauni:36)  = %trim(idcrsd) + ' ' + %trim(idcind) +
034746141002                                    ' ' + %trim(idccad) + ' ' + %trim(idclod) +
034747141002                                    ' ' + %trim(idcprd) + ' ' + %trim(idcnaz);
034748141001             if idcdcr>0 ;
034749141002              n= %checkr(' ':notauni);
034750141002              nrighe=%div(n:35) ;
034751141002              if %rem(n:35)>0;
034752141002                 nrighe+=1;
034753141002              endif;
034754141002              n=(nrighe*35)+1;
034755141002              %subst(notauni:n)= 'Cons.Rich.' ;
034756141001             endif;
034757141001             if idcdcr>0;
034758140930              dataeur=%date(IDCDCR:*iso);
034759140930              select;
034760140930              when IDCTCR = *blanks ;
034761141002                 notauni = %trim(notauni) + 'Il'  ;
034762140930              when IDCTCR = 'D'     ;
034763141002                 notauni= %trim(notauni) + 'Dopo il'  ;
034764140930              when IDCTCR = 'P'     ;
034765141002                 notauni= %trim(notauni) + 'Prima del'  ;
034766140930              endsl;
034767141002             notauni=%trim(notauni)+' '+%editw(%dec(dataeur:*dmy):'  /  /  ');
034768141001             endif;
034769140930              if idchcr>0;
034770141002                 notauni=%trim(notauni) + ' Ore ' + %editw(idchcr:'  :  ');
034771140930              endif;
034772140930              if idcprcr<>*blanks;
034773140930              select;
034774140930              when idcprcr='M';
034775141002                 notauni=%trim(notauni) + ' Mattino';
034776140930              when idcprcr='P';
034777141002                 notauni=%trim(notauni) + ' Pomeriggio';
034778140930              endsl;
034779140930              endif;
034780141001       // ALTRE INDICAZIONI
034781141001           when IDCTIPODIS='4';
034782141002              notauni = 'Altre Indicazioni';
034783141002              %subst(notauni:36)= %trim(idcnote);
034784140626           endsl;
034785140627       // referente e telefono + eventuale cell e indirizzo email
034786140930       //     ida06no2=%trim(ida06no2)+  ' Ref.Cons: ' + %trim(idcref) +
034787140930       //     ' ' + %trim(idcteld);
034788140627           if IDCTELAVV  <> *blanks;
034789141002            if %checkr(' ':notauni)<35;
034790141002              %subst(notauni:36)= 'Cell:' + %trim(idctelavv);
034791141002            else;
034792141002              notauni=%trim(notauni) + ' Cell:' +
034793140627              %trim(idctelavv);
034794141002            endif;
034795140627           endif;
034796141001           if IDCMAILAVV <> *blanks;
034797141002            if %checkr(' ':notauni)<35;
034798141002             %subst(notauni:36)= 'Email:' + %trim(idcmailavv);
034799141002            else;
034800141002             notauni=%trim(notauni) + ' Email:' + %trim(idcmailavv);
034801141002            endif;
034802141001           endif;
034803140627
034804140926       //  ida06no1=%trim(ida06no1) + ' - Richiesto da';
034805140926       //  if idctipocli ='M';
034806140926       //  ida06no1=%trim(ida06no1) + ' Mitt.';
034807140926       //  else;
034808140926       //  ida06no1=%trim(ida06no1) + ' Dest.';
034809140926       //  endif;
034810140926       //  if idcidcli<>*blanks ;
034811140926       //    ida06no1=%trim(ida06no1) + ' Id.cli. ' + idcidcli;
034812140926       //  endif;
034813140926       //    dataeur=%date(%dec(%subst(%editc(idcinsdata:'X'):1:8):8:0)
034814140926       //            :*iso);
034815140926       //    wgg=     %subdt(dataeur:*days);
034816140926       //    wmm=     %subdt(dataeur:*months);
034817140926       //    waa=     %subdt(dataeur:*years);
034818140926       //    ida06no1=%trim(ida06no1) + ' in data ' +
034819140926       //             %editc(wgg:'X') + '/' +
034820140926       //             %editc(wmm:'X') + '/' +
034821140926       //             %editc(waa:'X') + ' alle ' +
034822140926       //             %subst(%editc(idcinsdata:'X'):9:2) + ':'  +
034823140926       //             %subst(%editc(idcinsdata:'X'):11:2)        ;
034824141002           if idcip<>*blanks ;
034825141002            if %checkr(' ':notauni)<35;
034826141002             %subst(notauni:36)= 'IP:' + idcip;
034827141002            else;
034828141002             notauni=%trim(notauni) + ' IP:' + idcip;
034829141002            endif;
034830141002           endif;
034831140626
035000140626           iDA06TOR = 'S';
035100140926           iDA06OGG = %trim(%editc(idcLNP:'X')) +
035200140926                      %trim(%editc(idcNRS:'X')) +
035300140926                      %trim(%editc(idcNSP:'X')) +
035400140926                      %trim(%editc(idcAAS:'X'));
035401141023           if idcmodacc<>'B';
035402141022              iDA06UTE = 'INTERNET';
035403141022           else;
035500141022              iDA06UTE = idcelaute ;
035501141022           endif;
035502141008           if arblna>0;
035600141008              iDA06FIL = arblna;
035601141008           else;
035602141008              iDA06FIL = idcfgs;
035603141008           endif;
035604140926           ida06dar = %dec(%subst(%editc(idcinsdata:'X'):1:8):8:0);
035605140926           ida06orr = %dec(%subst(%editc(idcinsdata:'X'):9:6):6:0);
035606141022           if idcmodacc<>'B';
035607141022              if idctipocli='M';
035608141022                 ida06rsc = idcidcli;
035609141022              else;
035610141022                 ida06rsc = 'NON CODIFICATO';
035611141022              endif;
035612141022           endif;
035613141001           %subst(iDA06rsc:35:1) = idctipodis;
035614140926           iDA06pog = idcfgs;
035615141023           if idcmodacc<>'B';
035616141023              iDA06tco = 'I'   ;
035618141023           endif;
035619140926           iDA06tch = idctipocli;
035620140926
035621140926       // Non chiudo R.A. se la dispo non è stata caricata
035622140926           if idceladata=0;
035623140926               iDA06cdt = '999'     ;
035624140926           endif;
035625140930           iDA06ref = idcref  ;
035626140926           iDA06tel = idcteld ;
035700140626
035800140925           callP(e) FIDNA6R (fidna6ds:fidna6bds);
036000140626        endsr;
036100140626
036200141022       //-------------------------------------------------------------*
036300141022       //Forzatura giro DIR per richieste di dirottamento             *
036400141022       //-------------------------------------------------------------*
036500141022        Begsr sr_fiarg;
036501141022           chain (idcaas:idclnp:idcnrs:idcnsp) fiarg01l;
036502141022           if not %found(fiarg01l);
036503141022              clear fiarg000;
036504141022              argaas  =  idcaas;
036505141022              arglnp  =  idclnp;
036506141022              argnrs  =  idcnrs;
036507141022              argnsp  =  idcnsp;
036508141022              arglna  =  arblna;
036509141022       // p.o. di consegna capofila della £6 del p.o. di arrivo
036510141022              clear  fnlv55ds;
036511141022              d55tpt = '6' ;
036512141022              d55lin=arblna;
036513141023              d55drf=%dec(%date());
036514141022              fnlv55r(fnlv55ds);
036516141022              argpoc  =  d55tfa;
036517141022           endif;
036518141022           argcgi='DIR';
036519141022           argtgi='M';
036520141022           if not %found(fiarg01l);
036521141022              write fiarg000;
036522141022           else;
036523141022              update fiarg000;
036524141022           endif;
036600141022        endsr;

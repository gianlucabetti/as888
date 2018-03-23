000100150109       //==============================================================
000200150112       //?TRKC08R * Aggiornamento BLOCCO Cliente in Campagna.          ?
000300150109       //==============================================================
000400150109
000500150109       //--------------------------------------------------------------
000600150109       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700150109       //--------------------------------------------------------------
000800150109
000900150109     /*PRM  dbgview(*source)
001000150109     /*END
001100150109
001200150109       //--------------------------------------------------------------
001300150109       //?Specifiche di controllo.                                     ?
001400150109       //--------------------------------------------------------------
001500150109
001600150109     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700150109     h dftactgrp(*no)
001800150115     h bnddir('TRUL')
001900150109
002000150109       //--------------------------------------------------------------
002100150109       //?Dichiarazione file.                                          ?
002200150109       //--------------------------------------------------------------
002300150109
002400170310       // -?Anagrafica Campagne Commerciali?
002500150507     fTICMP01L  if   e           k disk
002600150507
002700150115       // -?Anagrafica Clienti in Campagna?
002800150507     fTICMC01L  if   e           k disk
002900150115
003000150115       // -?File di stampa errori per eventuale invio e-mail?
003100150115     fPrtEMAIL  o    f  132        printer  usropn  oflind(*inOF)
003200150109
003300150109       //--------------------------------------------------------------
003400150109       //?Definizione costanti.                                        ?
003500150109       //--------------------------------------------------------------
003600150109
003700150112       // -?Causale Chiusura/Esito per cliente Bloccato?
003800150112       //  ?(e tutti i figli pure)?
003900150115     d c_CausaleChiusura...
004000150115     d                 c                   const(' 8')
004100150115
004200150115       // -?Fase di Chiusura Campagna comm.le?
004300150115     d c_FaseChiusura...
004400150115     d                 c                   const(' 90')
004500150115
004600150115       // -?Comandi di override al PrtF?
004700150115     d c_CmdOvrPrtF    c                   const('OVRPRTF +
004800150115     d                                           file(PRTEMAIL) +
004900150115     d                                           pagesize(66 132) +
005000150115     d                                           lpi(6) cpi(10) +
005100150115     d                                           ovrscope(*actgrpdfn) +
005200150115     d                                           ')
005300150115     d c_CmdDltOvr     c                   const('DLTOVR +
005400150115     d                                            file(PRTEMAIL) +
005500150115     d                                            lvl(*actgrpdfn)')
005600150116
005700150116       // -?Costante per controllo "caratteri solo numerici"?
005800150116     d c_Digits        c                   const('0123456789')
005900150109
006000150109       //--------------------------------------------------------------
006100150109       //?Definizione schiere.                                         ?
006200150109       //--------------------------------------------------------------
006300150109
006400150109
006500150109       //--------------------------------------------------------------
006600150109       //?Definizione aree dati.                                       ?
006700150109       //--------------------------------------------------------------
006800150109
006900150109       // -?Dati utente?
007000150109     d §AzUte        e ds                  extname(AZUTE00F)
007100150109     d                                     dtaara
007200150109     d §DatiUte      e ds                  extname(dDatiUte)
007300150109     d                                     dtaara
007400150109
007500150109       //--------------------------------------------------------------
007600150109       //?Definizione strutture dati.                                  ?
007700150109       //--------------------------------------------------------------
007800150109
007900150109       // -?Parametri ricevuti?
008000150109     d KPJBA         e ds
008100150115
008200150115       // -?Fasi Campagne Comm.li?
008300150115     d dFCM          e ds                  inz
008400150115
008500150115       // -?Tab. "MRA" = Bart-Mailing x TRKC08R?
008600150115     d dMRAdan       e ds                  inz
008700150115     d   §MRAddes    e                     inz('*ERR (S)Blocco campagna')
008800150115     d   §MRAdreg    e                     inz('COR')
008900150115     d   §MRAdmitt   e                     inz('ced')
009000150115     d   §MRAddest   e                     inz('antonella.gallucci+
009100150115     d                                          @brt.it;+
009200150115     d                                          monica.beghelli+
009300150115     d                                          @brt.it;+
009400150115     d                                          stefano.merighi+
009500150115     d                                          @brt.it')
009600150115     d   §MRAdidpro  e                     inz('1')
009700150115     d   §MRAdoutqi  e                     inz('EMAILIN')
009800150115
009900150115       // -?Parametri x Ridefinizione dati utente estesi x mailing *msg?
010000150115     d TRTCM1ds      e ds                  inz
010100150115       //  ?·§CM1mitt = Indirizzo e-mail del mittente?
010200150115     d   §CM1mitt    e                     inz('ced')
010300150115       //  ?·§CM1dst  = Indirizzo e-mail del destinatario?
010400150115     d   §CM1dst     e                     inz('ced@brt.it')
010500150115       //  ?·§CM1tips = Tipo lettera via e-mail:?
010600150115       //   ?"LET" = testo allegato in corpo con logo?
010700150115       //           ?(richiede righe libere iniziali per il logo)?
010800150115       //   ?"COR" = testo integrato senza logo?
010900150115       //           ?(non consente né UNDERLINE né HIGHLIGHT)?
011000150115     d   §CM1tips    e                     inz('COR')
011100150115       //  ?·§CM1po   = Filiale?
011200150115     d   §CM1po      e                     inz('046')
011300150115       //  ?·§CM1var  = Oggetto e-mail?
011400150115     d   §CM1var     e                     inz('*OBJM*+
011500150115     d                                     ERRORI in BLOCCO/SBLOCCO +
011600150115     d                                     clienti in campagna comm.le')
011700150115       //  ?·§CM1sts  = Stato?
011800150115     d   §CM1sts     e                     inz(*off)
011900150115       //  ?·§CM1sts  = Id processo?
012000150115     d   §CM1idp     e                     inz('1')
012100150115
012200150115       // -?Status ds?
012300150115     d Status         sds
012400150115     d   SDSpgm          *proc
012500150115
012600150115       // -?Data/Ora attuali?
012700150115     d wTime_ds        ds                  inz
012800150115     d   wDate                        8s 0 inz
012900150115     d   wTime                        6s 0 inz
013000150109
013100150109       //--------------------------------------------------------------
013200150109       //?Definizione variabili globali.                               ?
013300150109       //--------------------------------------------------------------
013400150109
013500150112       // -?Flags booleani?
013600150112     d $Blocco         s               n   inz
013700150112
013800150112       // -?Indici di Schiera?
013900150112     d xx              s              3  0 inz
014000150115
014100150507       // -?Data odierna?
014200150115     d wDate_EUR       s               d   datfmt(*EUR)    inz(*loval)
014300150507     d wDate_AMG       s              8  0 inz
014400150115
014500150115       // -?Campi in stampa?
014600150115     d wErrMsg         s                   like(oKC71msg)  inz
014700150116
014800150507       // -?Eventuale parzializzazione per Campagna?
014900150507     d wCMP            s              7  0 inz
015000150109
015100150109       //--------------------------------------------------------------
015200150109       //?Definizione prototipi procedure.                             ?
015300150109       //--------------------------------------------------------------
015400150109
015500150109       // -?Ricerca unificante padre?
015600150109     d TIBS10ds      e ds                  inz
015700150116     d   sk_D10skc            21   5520  0 inz  dim(500)
015800150109      /copy gaitrasrc/srcProtoPR,TIBS10R
015900150109
016000150109       // -?Reperimento dati utente?
016100150109     d TIBS34ds      e ds                  inz
016200150109      /copy gaitrasrc/srcProtoPR,TIBS34R
016300150112
016400150112       // -?Controllo/Decodifica cliente?
016500150112      /copy gaitrasrc/srcProtoPI,TIBS69R
016600150112      /copy gaitrasrc/srcProtoPR,TIBS69R
016700150115
016800150115       // -?Scrittura fase (di Blocco) per cliente in Campagna?
016900150115     d TRKC71ds      e ds                  inz
017000150115      /copy gaitrasrc/srcProtoPR,TRKC71R
017100150115
017200150115       // -?Riattivazione Cliente in Campagna commerciale?
017300150115     d TRKC74ds      e ds                  inz
017400150115      /copy gaitrasrc/srcProtoPR,TRKC74R
017500150115
017600150115       // -?Reperimento dati tabelle?
017700150115      /copy gaitrasrc/srcProtoPI,TRULTAB
017800150115      /copy gaitrasrc/srcProtoPR,TRULTAB
017900150115
018000150115       // -?Parametri API QCAPCMD (Process Commands)?
018100150115     d Qcmd            s           2048    inz  varying
018200150115      /copy qSysInc/qRpgleSrc,QCAPCMD
018300150115       // -?API QCAPCMD (Process Commands)?
018400150115      /copy gaitrasrc/srcProtoPR,QCAPCMD
018500150115
018600150115       // -?Parametri gestione errori API.?
018700150115      /copy qsysinc/qrpglesrc,QUSEC
018800150109
018900150109       //--------------------------------------------------------------
019000150109       //?Definizione key-list.                                        ?
019100150109       //--------------------------------------------------------------
019200150109
019300150507       // -?File TICMC01L: Anagrafica Clienti in Campagna?
019400150507     d keyTICMC01    e ds                  extname(TICMC01L : *key)
019500150109     d                                     prefix(k_)   inz
019600150109
019700150109       //--------------------------------------------------------------
019800150109       //?M A I N - L I N E                                            ?
019900150109       //--------------------------------------------------------------
020000150109
020100150109     c     *Entry        plist
020200150109     c                   parm                    KPJBA
020300150109
020400150109      /free
020500150109
020600150109       // -?Operazioni iniziali?
020700150109       exsr sr_RoutInz;
020800150109
020900150507       // -?Ciclo di lettura del file TICMP01L/TICMC01L?
021000150507       if  wCMP = *zero;
021100150507         setll  ( *loval )  TICMP000;
021200150116       else;
021300150507         // -?Ricevuta campagna con cui parzializzare?
021400150507         setll  ( wCMP )  TICMP000;
021500150116       endif;
021600150507       read  TICMP000;
021700150507
021800150507       DoW  Not  %eof(TICMP01L);
021900150109
022000150507         // -?Elaborazione clienti SE campagna NON chiusa?
022100150507         IF  CMPdfc >= wDate_AMG;
022200150507
022300150507           clear  keyTICMC01;
022400150507           k_CMCncm = CMPncm;
022500150507           setll  %kds( keyTICMC01 )  TICMC000;
022600150507           readE  %kds( keyTICMC01 : 1 )  TICMC000;
022700150507
022800150507           DoW  Not  %eof(TICMC01L);
022900150507
023000150507             // -?Verificare SE l'unificante in Campagna e tutti i suoi?
023100150507             //  ?figli sono Bloccati?
023200150527             exsr  sr_CtrlSKC_Blocc;
023300150507
023400150507             // -?Blocco o Sblocco dell'Unificante nella Campagna?
023500150507             if  ( $Blocco      and  CMCcch = *blank )  or
023600150507                 ( Not $Blocco  and  CMCcch = c_CausaleChiusura);
023700150507               exsr  sr_ChiusuraRiapertura;
023800150507             endif;
023900150507
024000150507             readE  %kds( keyTICMC01 : 1 )  TICMC000;
024100150507
024200150507           EndDo;
024300150507
024400150507         ENDIF;
024500150507
024600150507         read  TICMP000;
024700150507         if  wCMP <> *zero  and  CMPncm <> wCMP;
024800150507           leave;
024900150507         endif;
025000150507
025100150507       EndDo;
025200150109
025300150109       // -?Operazioni finali?
025400150109       exsr sr_RoutEnd;
025500150109
025600150109       //--------------------------------------------------------------
025700150109       //?Operazioni iniziali.                                         ?
025800150109       //--------------------------------------------------------------
025900150109       BEGSR  sr_RoutInz;
026000150109
026100150109         *inLR = *on;
026200150116
026300150116         // -?Verifica SE ricevute parzializzazioni?
026400150507         clear  wCMP;
026500150507         if  %subst( kpjbu : 1 : 7 ) <> *blank  and
026600150507             %check( c_Digits : %subst( kpjbu : 1 : 7 ) ) = *zero;
026700150507           wCMP = %int( %subst( kpjbu : 1 : 7 ) );
026800150116         endif;
026900150109
027000150109         // -?Reperimento dati job?
027100150109         exsr  sr_DatiJob;
027200150507
027300150507         // -?Reperimento data odierna?
027400150507         wTime_ds  = %editc( %dec( %timestamp() ) : 'X' );
027500150507         wDate_AMG = wDate;
027600150109
027700150109       ENDSR;
027800150109
027900150109       //--------------------------------------------------------------
028000150109       //?Reperimento Dati del job (Utente/Operativi).                 ?
028100150109       //--------------------------------------------------------------
028200150109       BEGSR  sr_DatiJob;
028300150109
028400150109         in(e) §AzUte;
028500150109         if NOT %error;
028600150109           in(e) §DatiUte;
028700150109         endif;
028800150109         if %error or RSut = *blank;
028900150109           tibs34r ( tibs34ds );
029000150109           in §AzUte;
029100150109           in §DatiUte;
029200150109         endif;
029300150109
029400150109       ENDSR;
029500150116
029600150116       //--------------------------------------------------------------
029700150116       //?Verifica SE l'Unificante in Campagna e tutti i suoi figli    ?
029800150116       //?sono Bloccati.                                               ?
029900150116       //--------------------------------------------------------------
030000150116       BEGSR  sr_CtrlSKC_Blocc;
030100150116
030200150116         // -?Reperimento FIGLI del cliente in elaborazione?
030300150116         clear  TIBS10ds;
030400150116         D10tle = 'ST';
030500150116         D10paf = 'F';
030600150116         D10cod = CMCksu;
030700150116         //tibs10r ( TIBS10ds );
030800150116         GetKSCfamily ( TIBS10ds : 'R' );
030900170309         // -?Non reperiti figli del cliente in elaborazione:?
031000170309         //  ?=> Reperimento PADRE del cliente in elaborazione?
031100170309         if  D10err <> *blank  or  D10skc = *zero;
031200170309           clear  TIBS10ds;
031300170309           D10tle = 'ST';
031400170309           D10paf = 'P';
031500170309           D10cod = CMCksu;
031600170309           GetKSCfamily ( TIBS10ds : 'R' );
031700170309         endif;
031800170309         // -?Non reperito padre del cliente in elaborazione:?
031900170309         //  ?=> Cliente PADRE di se stesso?
032000150116         if  D10err <> *blank  or  D10skc = *zero;
032100150116           sk_D10skc(01) = CMCksu;
032200150116         endif;
032300150116
032400150116         // -?Ciclo di controllo SE ogni cliente della famiglia è Bloccato?
032500150116         //  ?(NON verifica SE la campagna è ancora aperta?
032600150116         //  ?            e SE con trattative ancora inseribili)?
032700150116         $Blocco = *on;
032800150116         For  xx = 1  To  %elem(sk_D10skc);
032900150116
033000150116           if  sk_D10skc(xx) = *zero;
033100150116             leave;
033200150116           endif;
033300150116
033400150116           // -?Decodifica cliente (per reperire il flag "Blocco")?
033500150116           clear  TIBS69ds;
033600150116           i69sif = knsif;
033700150116           i69kcc = DUTkci;
033800150116           i69kac = sk_D10skc(xx);
033900150116           tibs69r( TIBS69ds :
034000150116                    ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
034100150116
034200150116           // -?Trovato almeno un cliente NON Bloccato => uscita?
034300150116           if  O69err = *blank  and  ACOabl = *blank;
034400150116             $Blocco = *off;
034500150116             leave;
034600150116           endif;
034700150116
034800150116         EndFor;
034900150116
035000150116       ENDSR;
035100150109
035200150109       //--------------------------------------------------------------
035300150116       //?SE l'Unificante in Campagna e tutti i suoi figli sono        ?
035400150116       //?   Bloccati: chiude tutte le campagne aperte sul cliente.    ?
035500150116       //?Altrimenti: riapre le campagne chiuse sul cliente.           ?
035600150109       //--------------------------------------------------------------
035700150116       BEGSR  sr_ChiusuraRiapertura;
035800150109
035900150115         Select;
036000150115
036100150115           // -?SE Unificante e tutti Figli BLOCCATI  &  con Campagna Aperta?
036200150527           //  ?=> Chiusura Campagna con causale "  8"?
036300150115           When  $Blocco      and  CMCcch = *blank;
036400150115             exsr  sr_ChiusuraCampagneUnif;
036500150115
036600150115           // -?SE Unificante o almeno un Figlio NON BLOCCATI  &?
036700150527           //  ?Campagna Chiusa con "  8" => Riapertura Campagna?
036800150115           When  Not $Blocco  and  CMCcch = c_CausaleChiusura;
036900150115             exsr  sr_RiaperturaCampagneUnif;
037000150115
037100150115         EndSl;
037200150109
037300150109       ENDSR;
037400150112
037500150112       //--------------------------------------------------------------
037600150115       //?Chiusura Campagna per l'Unificante.                          ?
037700150112       //--------------------------------------------------------------
037800150112       BEGSR  sr_ChiusuraCampagneUnif;
037900150115
038000150115         clear  TRKC71ds;
038100150115         iKC71ncm = CMCncm;
038200150115         iKC71ksu = CMCksu;
038300150115         iKC71ksc = CMCksc;
038400150115         iKC71cpo = CMCcpo;
038500150122         iKC71acm = c_FaseChiusura;      //?" 90"?
038600150122         iKC71cch = c_CausaleChiusura;   //?" 8"?
038700150122         iKC71aut = 'A';                 //?(Fase Automatica)?
038800150115
038900150115         TRKC71R ( kpjba : TRKC71ds );
039000150115
039100150115         if  oKC71err <> *blank;
039200150115           wErrMsg = oKC71msg;
039300150115           exsr  sr_ListaErrori;
039400150115         endif;
039500150112
039600150112       ENDSR;
039700150115
039800150115       //--------------------------------------------------------------
039900150115       //?Riapertura Campagna per l'Unificante.                        ?
040000150115       //--------------------------------------------------------------
040100150115       BEGSR  sr_RiaperturaCampagneUnif;
040200150115
040300150115         clear  TRKC74ds;
040400150115         iKC74ncm = CMCncm;
040500150115         iKC74ksu = CMCksu;
040600150115         iKC74ksc = CMCksc;
040700150115         iKC74cpo = CMCcpo;
040800150116         kpjbu = TRKC74ds;
040900150115
041000150116         TRKC74R ( kpjba );
041100150115
041200150116         TRKC74ds = kpjbu;
041300150115         if  oKC74err = *on;
041400150115           wErrMsg = oKC74msg;
041500150115           exsr  sr_ListaErrori;
041600150115         endif;
041700150115
041800150115       ENDSR;
041900150109
042000150115       //--------------------------------------------------------------
042100150115       //?Seganalazione errori rilevati.                               ?
042200150115       //--------------------------------------------------------------
042300150115       BEGSR  sr_ListaErrori;
042400150115
042500150115         // -?Reperimento data e ora attuali?
042600150115         wTime_ds  = %editc( %dec( %timestamp() ) : 'X' );
042700150115         wDate_Eur = %date( wDate : *Iso );
042800150115         wDate     = %dec( wDate_Eur );
042900150115
043000150115         // -?Override al file di stampa ed apertura dello stesso?
043100150115         if  NOT  %open(PrtEMAIL);
043200150115           §CM1mitt = %trim(§MRAdmitt);
043300150115           §CM1dst  = %trim(§MRAddest);
043400150115           §CM1tips = §MRAdreg;
043500150115           §CM1var  = '*OBJM*' + §MRAddes;
043600150115           §CM1idp  = §MRAdidpro;
043700150115           Qcmd = c_CmdOvrPrtF
043800150115                + ' outq(' + %trim(§MRAdoutqi) + ')'
043900150115                + ' usrdfndta(''' + TRTCM1ds + ''')';
044000150115           exsr  sr_ExecCmd;
044100150115           open  PrtEMAIL;
044200150115           except  PrtTxt;
044300150115         endif;
044400150115
044500150115         // -?Stampa del messaggio di errore?
044600150115         except  PrtErr;
044700150115
044800150115       ENDSR;
044900150115
045000150109       //--------------------------------------------------------------
045100150115       //?Operazioni finali.                                           ?
045200150109       //--------------------------------------------------------------
045300150109       BEGSR  sr_RoutEnd;
045400150109
045500150112         // -?Chiusura applicazioni precedentemente aperte?
045600150115         clear  TIBS10ds;
045700150115         GetKSCfamily ( TIBS10ds : 'C' );
045800150115
045900150112         reset  TIBS69ds;
046000150115         //I69tla = 'C';         ?(già così)?
046100150112         tibs69r( TIBS69ds :
046200150112                  ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
046300150115
046400150115         cloTabella ( c_Tntbe );
046500150115
046600150115         // -?Chiusura del file di stampa e cancellazione override?
046700150115         if  %open(PrtEMAIL);
046800150115           close  PrtEMAIL;
046900150115           Qcmd = c_CmdDltOvr;
047000150115           exsr  sr_ExecCmd;
047100150115         endif;
047200150112
047300150109         // -?Uscita?
047400150109         return;
047500150109
047600150109       ENDSR;
047700150115
047800150115       //--------------------------------------------------------------
047900150115       //?Esecuzione del comando (già impostato).                      ?
048000150115       //--------------------------------------------------------------
048100150115       BEGSR  sr_ExecCmd;
048200150115
048300150115         clear Qcap0100;
048400150115         Qcabcsdh = *off;
048500150115         Qcapa    = *off;
048600150115         Qcacmdss = *off;
048700150115         Qcaerved = *allX'00';
048800150115
048900150115         clear Qusec;
049000150115         Qusbprv  = %size(Qusec);
049100150115
049200150115         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
049300150115                           %size(Qcap0100) : 'CPOP0100' : *omit :
049400150115                           0 : 0 : Qusec);
049500150115
049600150115         //if  Qusei <> *blank;
049700150115         //  ...;
049800150115         //endif;
049900150115
050000150115       ENDSR;
050100150109
050200150109      /end-free
050300150115
050400150115       //--------------------------------------------------------------
050500150115       //?Spool di stampa (per e-mail).                                ?
050600150115       //--------------------------------------------------------------
050700150115
050800150115     oPrtEMAIL  e            PRTtxt            2
050900150115     o                       RSUt
051000150115     o                                        +   3 'LISTA ERRORI RILEVATI IN -
051100150115     o                                              FASE DI BLOCCO/SBLOCCO -
051200150115     o                                              CLIENTI IN CAMPAGNA'
051300150115     o                       SDSpgm           +   3
051400150115     o                       *date         y  +   3
051500150115     o          e            PRTtxt      1
051600150115     o                       KNSIF
051700150115     o                       KNMUS            +   1
051800150115     o                                        +   2 '                         -
051900150115     o                                                                     -
052000150115     o                                                                 '
052100150115     o                       wTime         y  +  16
052200150115      *
052300150115     o          e            PRTtxt      3
052400150115     o                                              'Cliente'
052500150115     o                                        +   2 'Blo'
052600150115     o                                        +   2 'Campagna'
052700150115     o                                        +   2 'Chiu'
052800150115     o                                        +   2 'Messaggio di Errore'
052900150115     o          e            PRTtxt      1
053000150115     o                                              '======='
053100150115     o                                        +   2 '==='
053200150115     o                                        +   2 '========'
053300150115     o                                        +   2 '===='
053400150115     o                                        +   2 '==================='
053500150115      *
053600150115     o          e            PRTerr      1
053700150115     o                       CMCksu
053800150115     o                       ACOabl           +   3
053900150115     o                       CMCncm        j  +   3
054000150115     o                       CMCcch           +   3
054100150115     o                       wErrMsg          +   3
054200150115      *
054300150115     o          e            PRTend      3
054400150115     o                                        +  20 '***  Fine Lista  ***'

000100131217       //==============================================================
000200131217       //?FIOR54R - Preparazione telefonate AUT x O.R.M.               ?
000300131217       //==============================================================
000400131217
000500131217       //--------------------------------------------------------------
000600131217       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700131217       //--------------------------------------------------------------
000800131217
000900131217     /*PRM  dbgview(*source)
001000131217     /*END
001100131217
001200131217       //--------------------------------------------------------------
001300131217       //?Specifiche di controllo.                                     ?
001400131217       //--------------------------------------------------------------
001500131217
001600131217     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700131217     h dftactgrp(*no) actgrp(*caller)
001800131217     h bnddir('UBBNDDIR':'TRUL')
001900131217
002000131217       //--------------------------------------------------------------
002100131217       //?Dichiarazione file.                                          ?
002200131217       //--------------------------------------------------------------
002300131218
002400131218       // -?Distinte uscite AUT (Consegna/Ritiro)?
002500131218     fFIDST01L  if   e           k disk
002600131217
002700131217       // -?Stampa segnalazioni di errore?
002800131217     fQSYSPRT   o    f  132        printer usropn
002900131217     f                                     oflind(*inOF)
003000131217
003100131217       //--------------------------------------------------------------
003200131217       //?Definizione costanti.                                        ?
003300131217       //--------------------------------------------------------------
003400131217
003500131217       // -?Costanti per stringa SQL?
003600131217     d c_ForFetch      c                   const('FOR FETCH ONLY')
003700131217
003800131217       // -?Comandi da eseguire?
003900131217     d c_OvrPrtF       c                   const('OVRPRTF +
004000131217     d                                            file(QSYSPRT) +
004100131217     d                                            usrdta(''FIOR54+
004200131217     d                                                     *ERR'')')
004300131217     d c_DltOvr        c                   const('DLTOVR +
004400131217     d                                            file(QSYSPRT)')
004500131217
004600131217       //--------------------------------------------------------------
004700131217       //?Definizione schiere.                                         ?
004800131217       //--------------------------------------------------------------
004900131217
005000131217       // -?Messaggi di errore?
005100131218     d sk_Msg          s             78    dim( 3)  ctdata  perrcd( 1)
005200131217
005300131217       //--------------------------------------------------------------
005400131217       //?Definizione aree dati.                                       ?
005500131217       //--------------------------------------------------------------
005600131217
005700131217       // -?Dati utente?
005800131217     d §AzUte        e ds                  extname(AZUTE00F)
005900131217     d                                     dtaara
006000131217     d §DatiUte      e ds                  extname(dDatiUte)
006100131217     d                                     dtaara
006200131217
006300131217       //--------------------------------------------------------------
006400131217       //?Definizione strutture dati.                                  ?
006500131217       //--------------------------------------------------------------
006600131217
006700131217       // -?Status ds?
006800131217     d Status         sds
006900131217     d   SDSpgm          *proc
007000131217
007100131217       // -?Parametri ricevuti?
007200131217     d KPJBA         e ds
007300131217     d FIOR56ds      e ds                  inz   qualified
007400131217
007500131217       // -?Dati estratti via SQL?
007600131217     d FNORMds       e ds                  extname(FNORM00F)
007700131217     d                                     inz   qualified
007800131217     d FNORGds       e ds                  extname(FNORG00F)
007900131217     d                                     inz   qualified
008000131218
008100131218       // -?DS lettura campo DSTFLR di FIDST00F?
008200131218     d dDSTflr       e ds                  inz   qualified
008300131217
008400131217       // -?Filiali abilitate?
008500131217     d dVPOdecofi816   ds                  inz
008600131217     d   sk_AbiFilTelAUT...
008700131217     d                                3    inz   dim(85)
008800131217
008900131218       // -?Stringa SQL da eseguire?
009000131218       //  ?(anche "scomposta" in schiera per la stampa)?
009100131218     d*// ds_Temp         ds                  inz
009200131218     d*//   $Temp                 1   5000    inz   dim(50)
009300131217
009400131217       //--------------------------------------------------------------
009500131217       //?Definizione variabili globali.                               ?
009600131217       //--------------------------------------------------------------
009700131217
009800131217       // -?Parametri ricevuti?
009900131217     d prm_FIOR56ds    s                   like(FIOR56ds)
010000131217
010100131217       // -?Flags booleani?
010200131217     d $Fine           s               n   inz
010300131218
010400131218       // -?Indici di schiera?
010500131218     d xx              s              5  0 inz
010600131217
010700131217       // -?Stringa SQL da eseguire?
010800131218     d wSQL            s           5000    inz   varying
010900131217
011000131217       // -?Campi di comodo?
011100131218     d wDate_Eur       s               d   inz  datfmt(*eur)
011200131217     d wDate           s              8  0 inz
011300131218     d wTime           s              6  0 inz
011400131218     d w100a           s            100    inz
011500131217
011600131217       //--------------------------------------------------------------
011700131217       //?Definizione prototipi procedure.                             ?
011800131217       //--------------------------------------------------------------
011900131217
012000131217       // -?Reperimento dati utente?
012100131217     d TIBS34ds      e ds                  inz
012200131217      /copy gaitrasrc/srcProtoPR,TIBS34R
012300131217
012400131217       // -?Reperimento dati tabelle?
012500131217      /copy gaitrasrc/srcProtoPI,TRULTAB
012600131217      /copy gaitrasrc/srcProtoPR,TRULTAB
012700131217
012800131218       // -?Controllo fase distinta?
012900131217     d FIOR90ds      e ds                  inz   qualified
013000131218     d   OR90tla     e                     inz('C')
013100131217      /copy gaitrasrc/srcProtoPR,FIOR90R
013200131218
013300131218       // -?Scrittura file Telefonate AUT da Note AUT?
013400131218     d FNLRX3ds      e ds                  inz   qualified
013500131218     d   iX3tpDoc    e                     inz('R')
013600131218     d   iX3cmd      e                     inz('I')
013700131218     d   oX3err      e                     inz(*off)
013800131218      /copy gaitrasrc/srcProtoPR,FNLRX3R
013900131217
014000131217       // -?API QCAPCMD (Process Commands)?
014100131218     d Qcmd            s           2048    inz   varying
014200131217      /copy qSysInc/qRpgleSrc,QCAPCMD
014300131217      /copy gaitrasrc/srcProtoPR,QCAPCMD
014400131217
014500131217       // -?Parametri gestione errori API.?
014600131217      /copy qsysinc/qrpglesrc,QUSEC
014700131217
014800131217       //--------------------------------------------------------------
014900131217       //?Definizione key-list.                                        ?
015000131217       //--------------------------------------------------------------
015100131217
015200131218     d keyFIDST01    e ds                  extname( FIDST01L : *key )
015300131218     d                                     prefix(k_)   inz
015400131217
015500131217       //--------------------------------------------------------------
015600131217       //?M A I N - L I N E                                            ?
015700131217       //--------------------------------------------------------------
015800131217
015900131217     c     *Entry        plist
016000131217     c                   parm                    KPJBA
016100131217     c                   parm                    prm_FIOR56ds
016200131217
016300131217      /free
016400131217
016500131217       // -?Operazioni iniziali?
016600131217       exsr sr_RoutInz;
016700131217
016800131217       // -?Elaborazione *file FNORM/FNORG?
016900131217       exsr  sr_SQL_Prep;
017000131217       exsr  sr_SQL_Read;
017100131217       DoW  Not $Fine;
017200131217         exsr  sr_Elaboraz;
017300131217         exsr  sr_SQL_Read;
017400131217       EndDo;
017500131217
017600131217       // -?Operazioni finali?
017700131217       exsr sr_RoutEnd;
017800131217
017900131217       //--------------------------------------------------------------
018000131217       //?Operazioni iniziali.                                         ?
018100131217       //--------------------------------------------------------------
018200131217       BEGSR  sr_RoutInz;
018300131218
018400131218         // -?Impostazione opzioni SQL?
018500131218         exec sql  set option  DynUsrPrf = *Owner,
018600131218                               CloSqlCsr = *EndMod;
018700131217
018800131217         *inLR = *on;
018900131217
019000131217         // -?Verifica dei parametri ricevuti?
019100131217         if  %parms() = 1;
019200131217           FIOR56ds = KPJBU;
019300131217         else;
019400131217           FIOR56ds = prm_FIOR56ds;
019500131217         endif;
019600131217
019700131217         clear  FIOR56ds.or56err;
019800131217         clear  FIOR56ds.or56msg;
019900131217
020000131217         if  FIOR56ds.OR56tla <> 'T';
020100131217           FIOR56ds.OR56err = *on;
020200131217           FIOR56ds.OR56msg = sk_Msg(01);
020300131217           exsr  sr_RoutEnd;
020400131217         endif;
020500131217
020600131217         // -?Reperimento dati job?
020700131217         exsr  sr_DatiJob;
020800131217
020900131217         // -?Reperimento tab. "VPO"/"DECOFI816TEL"?
021000131217         if  getTabella ( c_Tntbe : 'VPO' : 'DECOFI816TEL' :
021100131217                          *blank : *blank : *blank :
021200131217                          *omit : *omit :
021300131217                          *omit : *omit : *omit : *omit :
021400131217                          *omit : *omit : *omit : *omit :
021500131217                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
021600131217                      = *zero      AND
021700131217             ds_TNTBE.TBEatb = *blank;
021800131218           dVPOdecofi816 = ds_TNTBE.TBEuni;
021900131217         endif;
022000131217
022100131217         // -?Verifica se la filiale della distinta è in tab. "VPO"?
022200131217         if  %lookup( %editc( FIOR56ds.OR56fgs : 'X' ) :
022300131217                      sk_AbiFilTelAUT ) = *zero;
022400131217           FIOR56ds.OR56err = *on;
022500131217           FIOR56ds.OR56msg = %replace(
022600131217                                %editc( FIOR56ds.OR56fgs : 'X' ) :
022700131217                                sk_Msg(02) :
022800140107                                %scan( '&PO' : sk_Msg(02) ) );
022900131217           exsr  sr_RoutEnd;
023000131217         endif;
023100131217
023200131218         // -?Reperimento data odierna?
023300131218         //wDate = %int( %subst( %char( %dec( %timestamp() ) )
023400131218         //                      : 1 : 8 ) );
023500131218         wDate_Eur = %date();
023600131218         wDate = %dec( wDate_Eur );
023700131218
023800131218         // -?Reperimento orario?
023900131218         wTime = %dec( %time() );
024000131218
024100131218         // -?Impostazione campi chiave?
024200131218         clear  keyFIDST01;
024300131218         k_DSTfgs = FIOR56ds.OR56fgs;
024400131218         k_DSTnpg = 4;
024500131217
024600131217       ENDSR;
024700131217
024800131217       //--------------------------------------------------------------
024900131217       //?Reperimento Dati del job (Utente/Operativi).                 ?
025000131217       //--------------------------------------------------------------
025100131217       BEGSR  sr_DatiJob;
025200131217
025300131217         in(e) §AzUte;
025400131217         if NOT %error;
025500131217           in(e) §DatiUte;
025600131217         endif;
025700131217         if %error or RSut = *blank;
025800131217           tibs34r ( tibs34ds );
025900131217           in §AzUte;
026000131217           in §DatiUte;
026100131217         endif;
026200131217
026300131217       ENDSR;
026400131217
026500131217       //--------------------------------------------------------------
026600131217       //?Preperazione / Dichiarazione / Apertura cursore SQL          ?
026700131217       //--------------------------------------------------------------
026800131217       BEGSR  sr_SQL_Prep;
026900131217
027000131217         // -?Preparazione stringa SQL?
027100131218         wSQL = 'select FNORM00F.*, FNORG00F.* +
027200131218                   from FNORM00F inner join FNORG00F +
027300131218                     on FNORM00F.ORMpoe = FNORG00F.ORGpoe and +
027400131218                        FNORM00F.ORMnsr = FNORG00F.ORGnsr and +
027500131218                        FNORM00F.ORMnor = FNORG00F.ORGnor and +
027600131218                        FNORM00F.ORMnrv = FNORG00F.ORGnrv +
027700131218                  where FNORG00F.ORGpor = ' +
027800131218                          %editc( FIOR56ds.OR56fgs : 'X' ) +
027900131218                  ' and FNORG00F.ORGddc = ' +
028000131218                          %triml( %editc( FIOR56ds.OR56ddc : '3' ) ) +
028100131219                  ' and FNORM00F.ORMfao = 400 +
028200131218                    and FNORG00F.ORGndc between ' +
028300131218                          %editc( FIOR56ds.OR56ndcD : '3' ) + ' and ' +
028400131218                          %editc( FIOR56ds.OR56ndcA : '3' ) +
028500131218                ' order by FNORG00F.ORGfgs, FNORG00F.ORGndc';
028600131217
028700131217         // -?Dichiarazione cursore?
028800131217         exec sql   prepare S1   from :wSQL;
028900131217         exec sql   declare C1   cursor for S1;
029000131219
029100131219         if  SQLcode < *zero;
029200131219           $Fine = *on;
029300131219           FIOR56ds.OR56err = *on;
029400131219           FIOR56ds.OR56msg = sk_Msg(03);
029500131219           exsr  sr_PrintErrSQL;
029600131219         endif;
029700131217
029800131217         // -?Apertura del cursore?
029900131217         exec sql   open C1;
030000131217
030100131217         if  SQLcode < *zero;
030200131218           $Fine = *on;
030300131218           FIOR56ds.OR56err = *on;
030400131218           FIOR56ds.OR56msg = sk_Msg(03);
030500131217           exsr  sr_PrintErrSQL;
030600131217         endif;
030700131217
030800131217       ENDSR;
030900131217
031000131217       //--------------------------------------------------------------
031100131217       //?Lettura SQL                                                  ?
031200131217       //--------------------------------------------------------------
031300131217       BEGSR  sr_SQL_Read;
031400131217
031500131217         clear  FNORMds;
031600131217         clear  FNORGds;
031700131217
031800131217         exec sql   fetch next   from C1   into :FNORMds, :FNORGds;
031900131217
032000131217         select;
032100131217           when  SQLcode = 100;
032200131217             $Fine = *on;
032300131217           when  SQLcode < *zero;
032400131217             $Fine = *on;
032500131217             exsr  sr_PrintErrSQL;
032600131217         endsl;
032700131217
032800131217       ENDSR;
032900131217
033000131217       //--------------------------------------------------------------
033100131218       //?Elaborazione singolo O.R.M.                                  ?
033200131217       //--------------------------------------------------------------
033300131217       BEGSR  sr_Elaboraz;
033400131218
033500131218         // -?Verifica distinta?
033600131218         //k_DSTfgs = FIOR56ds.OR56fgs;    ?(già così)?
033700131218         //k_DSTnpg = 4;                   ?(già così)?
033800131218         k_DSTnfv = FNORGds.ORGndc;
033900131218         chain  %kds( keyFIDST01 )  FIDST000;
034000140114         if  Not %found(FIDST01L);
034100131218           leavesr;
034200131218         endif;
034300140114
034400140114         // -?Flag "Dati a PDA" & "Test su PDA"?
034500140114         dDSTflr = DSTflr;
034600140114         select;
034700140114           // ·?Distinta a PDA per ORM & in test x ORM => OK?
034800140114           when  (DSTpda = 'E'  or  DSTpda = 'O')  and
034900140114                 (dDSTflr.§DSTtstPda = 'O'  or  dDSTflr.§DSTtstPda = 'E');
035000140114           // ·?Distinta NO a PDA per ORM => OK?
035100140114           when  (DSTpda <> 'E'  and  DSTpda <> 'O');
035200140114           // ·?Altrimanti: da scartare?
035300140114           other;
035400140114             leavesr;
035500140114         endsl;
035600131218
035700131218         // -?Parzializzazioni:?
035800131218         Select;
035900131218           // ·?Tipo Uscita?
036000131218           When  FIOR56ds.OR56tUsc <> *blank  and
036100131218                 FIOR56ds.OR56tUsc <> DSTfpp;
036200131218             leavesr;
036300131218           // ·?Tipo Distinta Parcel?
036400131218           When  FIOR56ds.OR56tDis  = 'P'  and
036500131218                 DSTtpm            <> *blank;
036600131218             leavesr;
036700131218           // ·?Tipo Distinta Messaggerie?
036800131218           When  FIOR56ds.OR56tDis  = 'M'  and
036900131218                 FIOR56ds.OR56tDis <> DSTtpm;
037000131218             leavesr;
037100131218         EndSl;
037200131218
037300131218         // -?Controllo che NON ci sia la fase:?
037400131217         //  ?· "RCH" per le distinte su PDA?
037500131217         //  ?· "RQE" per le altre distinte?
037600131217         reset  FIOR90ds;
037700131218         //FIOR90ds.OR90tla = 'C';         ?(già così)?
037800131218         if  DSTpda = 'O'  or  DSTpda = 'E';
037900131218           FIOR90ds.OR90fas = 'RCH';
038000131218         else;
038100131218           FIOR90ds.OR90fas = 'RQE';
038200131218         endif;
038300131218         FIOR90ds.OR90fgs = FNORGds.ORGfgs;
038400131218         FIOR90ds.OR90ndc = FNORGds.ORGndc;
038500131218
038600131218         FIOR90R ( kpjba : FIOR90ds );
038700131218
038800131218         // -?Fase reperita => rec. scartato?
038900131218         if  FIOR90ds.OR90ndcOk <> *blank;
039000131217           leavesr;
039100131217         endif;
039200131218
039300131218
039400131218         // -?Registrazione rec. in "Telefonate AUT"?
039500131218         reset  FNLRX3ds;
039600131218         //FNLRX3ds.iX3tpDoc = 'R';        ?(già così)?
039700131218         FNLRX3ds.iX3idDoc = %editc( FNORGds.ORGpoe : 'X' ) +
039800131218                             %editc( FNORGds.ORGnsr : 'X' ) +
039900131218                             %editc( FNORGds.ORGnor : 'X' ) +
040000131218                             %editc( FNORGds.ORGnrv : 'X' );
040100131218         FNLRX3ds.iX3fgs   = FNORGds.ORGfgs;
040200131218         FNLRX3ds.iX3nfv   = FNORGds.ORGndc;
040300131218         FNLRX3ds.iX3dfv   = FNORGds.ORGddc;
040400131218         FNLRX3ds.iX3pdr   = FNORGds.ORGpdc;
040500131218         //FNLRX3ds.iX3var   = *blank;     ?(già così)?
040600131218         //FNLRX3ds.iX3cmd   = 'I';        ?(già così)?
040700131218
040800131218         FNLRX3R ( KPJBA : FNLRX3ds );
040900131218
041000131218         //if  FNLRX3ds.oX3err <> *off;
041100131218         //  FIOR56ds.OR56err = *on;
041200131218         //  FIOR56ds.OR56msg = FNLRX3ds.oX3msg;
041300131218         //  leavesr;
041400131218         //endif;
041500131217
041600131217       ENDSR;
041700131217
041800131217       //--------------------------------------------------------------
041900131217       //?Stampa segnalazione dell'errore rilevato via SQL             ?
042000131217       //--------------------------------------------------------------
042100131217       BEGSR  sr_PrintErrSQL;
042200131217
042300131217         // -?Stampa del Dump?
042400131217         Dump(A);
042500131217
042600131217         // -?Stampa del Job-Log?
042700131217         Qcmd = 'DSPJOBLOG job(*) output(*print)';
042800131218         exsr  sr_ExecCmd;
042900131217
043000131217         // -?Apertura del printer-file?
043100131217         if  Not %open(QSYSPRT);
043200131218           Qcmd = c_OvrPrtF;
043300131218           exsr  sr_ExecCmd;
043400131217           open  QSYSPRT;
043500131219           *inOF = *on;
043600131217         endif;
043700131217
043800131217         // -?Stampa dell'errore in QSYSPRT?
043900131217         if  *inOF;
044000131217           except  PRTtxt;
044100131217           *inOF = *off;
044200131217         endif;
044300131217         except  PRTerr1;
044400131217         *inOF = *off;
044500131218         For  xx = 1  By  %size(w100a)  To  %len(wSQL);
044600131219           if  ( %len(wSQL) - xx ) >= %size(w100a);
044700131219             w100a = %subst( wSQL : xx : %size(w100a) );
044800131219           else;
044900131219             w100a = %subst( wSQL : xx );
045000131219           endif;
045100131218           if  w100a = *blank;
045200131217             leave;
045300131217           endif;
045400131217           if  *inOF;
045500131217             except  PRTtxt;
045600131217             *inOF = *off;
045700131217           endif;
045800131217           except  PRTerr2;
045900131217         EndFor;
046000131217
046100131217         // -?Chiusura del programma?
046200131217         exsr  sr_RoutEnd;
046300131217
046400131217       ENDSR;
046500131218
046600131218       //--------------------------------------------------------------
046700131218       //?Esecuzione del comando (già impostato).                      ?
046800131218       //--------------------------------------------------------------
046900131218       BEGSR  sr_ExecCmd;
047000131218
047100131218         clear Qcap0100;
047200131218         Qcabcsdh = *off;
047300131218         Qcapa    = *off;
047400131218         Qcacmdss = *off;
047500131218         Qcaerved = *allX'00';
047600131218
047700131218         clear Qusec;
047800131218         Qusbprv  = %size(Qusec);
047900131218
048000131218         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
048100131218                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
048200131218                           0 : 0 : Qusec);
048300131218
048400131218         // -?Invio *msg per segnalazione errore?
048500131218         //if  Qusei <> *blank;
048600131218         //  ...;
048700131218         //endif;
048800131218
048900131218       ENDSR;
049000131217
049100131217       //--------------------------------------------------------------
049200131217       //?Operazioni finali.                                           ?
049300131217       //--------------------------------------------------------------
049400131217       BEGSR  sr_RoutEnd;
049500131217
049600131217         // -?Chiusura del cursore SQL?
049700131217         exec sql   close C1;
049800131217
049900131217         // -?Chiusura del printer-file?
050000131217         if  %open(QSYSPRT);
050100131217           close  QSYSPRT;
050200131218           Qcmd = c_DltOvr;
050300131218           exsr  sr_ExecCmd;
050400131217         endif;
050500131217
050600131217         // -?Chiusura funzioni precedentemente aperte?
050700131217         cloTabella( c_Tntbe );
050800131217
050900131217         // -?Restituzione eventuali parametri?
051000131217         if  %parms() = 1;
051100131217           KPJBU = FIOR56ds;
051200131217         else;
051300131217           prm_FIOR56ds = FIOR56ds;
051400131217         endif;
051500131217
051600131217         // -?Uscita dal *pgm?
051700131217         return;
051800131217
051900131217       ENDSR;
052000131217
052100131217      /end-free
052200131217
052300131217       //--------------------------------------------------------------
052400131217       //?S P O O L - F I L E S                                        ?
052500131217       //--------------------------------------------------------------
052600131217
052700131217     oQSYSPRT   e            PRTtxt            1
052800131217     o                       RSUT
052900131218     o                                        +   6 'ERRORI IN PREPARA-
053000131218     o                                              ZIONE DISTINTE AUT'
053100131217     o                       SDSpgm           +   6
053200131217     o                       wDate         y  +   3
053300131217     o          e            PRTtxt      1
053400131217     o                       KNSIF
053500131217     o                       KNMUS            +   1
053600131218     o                                        +   5 '------------------
053700131218     o                                              ------------------'
053800131217     o                                        +   6 'Pag.'
053900131217     o                       Page          z  +   0
054000131217     o                       wTime            +   5 '  :  :  '
054100131217     o          e            PRTerr1     2
054200131217     o                                              'RILEVATO ERRORE: +
054300131217     o                                               SQLCODE'
054400131217     o                       SQLcode       k  +   1
054500131217     o                                        +   1 'SQLSTATE'
054600131217     o                       SQLstate         +   1
054700131217     o                                        +   1 'DURANTE L''ESECUZ-
054800131217     o                                              IONE DEL SEGUENTE -
054900131217     o                                              COMANDO SQL:'
055000131217     o          e            PRTerr1     0  1
055100131217     o                                              'RILEVATO ERRORE: +
055200131217     o                                               SQLCODE'
055300131217     o                       SQLcode       k  +   1
055400131217     o                                        +   1 'SQLSTATE'
055500131217     o                       SQLstate         +   1
055600131217     o          e            PRTerr2     1
055700131218     o                       w100a            +   3
055800131217
055900131217       //--------------------------------------------------------------
056000131217       //?S C H I E R E   A   T E M P O   D I   C O M P I L A Z I O N E?
056100131217       //--------------------------------------------------------------
056200131217
056300131217** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
056400131218Tipo lancio NON previsto                                                        1
056500131217La filiale &PO non risulta abilitata                                            2
056600131217Rilevato errore SQL                                                             3

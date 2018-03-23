000100131217       //==============================================================
000200131217       //?FIOR54R - Preparazione telefonate AUT x O.R.M.               ?
000300131217       //==============================================================
000400131217
000500131217       //--------------------------------------------------------------
000600131217       //?Specifiche di controllo.                                     ?
000700131217       //--------------------------------------------------------------
000800131217
000900131217     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001000131217     h dftactgrp(*no) actgrp(*caller)
001100131217     h bnddir('UBBNDDIR':'TRUL')
001200131217
001300131217       //--------------------------------------------------------------
001400131217       //?Dichiarazione file.                                          ?
001500131217       //--------------------------------------------------------------
001600131218
001700131218       // -?Distinte uscite AUT (Consegna/Ritiro)?
001800131218     fFIDST01L  if   e           k disk
001900131217
002000131217       //--------------------------------------------------------------
002100131217       //?Definizione costanti.                                        ?
002200131217       //--------------------------------------------------------------
002300131217
002400131217       // -?Costanti per stringa SQL?
002500131217     d c_ForFetch      c                   const('FOR FETCH ONLY')
002600131217
002700131217       //--------------------------------------------------------------
002800131217       //?Definizione schiere.                                         ?
002900131217       //--------------------------------------------------------------
003000131217
003100131217       //--------------------------------------------------------------
003200131217       //?Definizione aree dati.                                       ?
003300131217       //--------------------------------------------------------------
003400131217
003500131217       // -?Dati utente?
003600131217     d §AzUte        e ds                  extname(AZUTE00F)
003700131217     d                                     dtaara
003800131217     d §DatiUte      e ds                  extname(dDatiUte)
003900131217     d                                     dtaara
004000131217
004100131217       //--------------------------------------------------------------
004200131217       //?Definizione strutture dati.                                  ?
004300131217       //--------------------------------------------------------------
004400131217
004500131217       // -?Status ds?
004600131217     d Status         sds
004700131217     d   SDSpgm          *proc
004800131217
004900131217       // -?Parametri ricevuti?
005000131217     d KPJBA         e ds
005100131217     d FIOR56ds      e ds                  inz   qualified
005200131217
005300131217       // -?Dati estratti via SQL?
005400131217     d FNORMds       e ds                  extname(FNORM00F)
005500131217     d                                     inz   qualified
005600131217     d FNORGds       e ds                  extname(FNORG00F)
005700131217     d                                     inz   qualified
005800131218
005900131218       // -?DS lettura campo DSTFLR di FIDST00F?
006000131218     d dDSTflr       e ds                  inz   qualified
006100131217
006200131217       // -?Filiali abilitate?
006300131217     d dVPOdecofi816   ds                  inz
006400131217     d   sk_AbiFilTelAUT...
006500131217     d                                3    inz   dim(85)
006600131217
006700131217       //--------------------------------------------------------------
006800131217       //?Definizione variabili globali.                               ?
006900131217       //--------------------------------------------------------------
007000131217
007100131217       // -?Parametri ricevuti?
007200131217     d prm_FIOR56ds    s                   like(FIOR56ds)
007300131217
007400131217       // -?Flags booleani?
007500131217     d $Fine           s               n   inz
007600131218
007700131218       // -?Indici di schiera?
007800131218     d xx              s              5  0 inz
007900131217
008000131217       // -?Stringa SQL da eseguire?
008100131218     d wSQL            s           5000    inz   varying
008200131217
008300131217       // -?Campi di comodo?
008400131218     d wDate_Eur       s               d   inz  datfmt(*eur)
008500131217     d wDate           s              8  0 inz
008600131218     d wTime           s              6  0 inz
008700131218     d w100a           s            100    inz
008800140121
008900140121     d wpoe            s                   like(FNORGds.ORGpoe)
009000140121     d wnsr            s                   like(FNORGds.ORGnsr)
009100140121     d wnor            s                   like(FNORGds.ORGnor)
009200140121     d wnrv            s                   like(FNORGds.ORGnrv)
009300131217
009400131217       //--------------------------------------------------------------
009500131217       //?Definizione prototipi procedure.                             ?
009600131217       //--------------------------------------------------------------
009700131217
009800131217       // -?Reperimento dati utente?
009900131217     d TIBS34ds      e ds                  inz
010000131217      /copy gaitrasrc/srcProtoPR,TIBS34R
010100131217
010200131217       // -?Reperimento dati tabelle?
010300131217      /copy gaitrasrc/srcProtoPI,TRULTAB
010400131217      /copy gaitrasrc/srcProtoPR,TRULTAB
010500131217
010600131218       // -?Controllo fase distinta?
010700131217     d FIOR90ds      e ds                  inz   qualified
010800131218     d   OR90tla     e                     inz('C')
010900131217      /copy gaitrasrc/srcProtoPR,FIOR90R
011000131218
011100131218       // -?Scrittura file Telefonate AUT da Note AUT?
011200131218     d FNLRX3ds      e ds                  inz   qualified
011300131218     d   iX3tpDoc    e                     inz('R')
011400131218     d   oX3err      e                     inz(*off)
011500131218      /copy gaitrasrc/srcProtoPR,FNLRX3R
011600131217
011700131217       //--------------------------------------------------------------
011800131217       //?Definizione key-list.                                        ?
011900131217       //--------------------------------------------------------------
012000131217
012100131218     d keyFIDST01    e ds                  extname( FIDST01L : *key )
012200131218     d                                     prefix(k_)   inz
012300131217
012400131217       //--------------------------------------------------------------
012500131217       //?M A I N - L I N E                                            ?
012600131217       //--------------------------------------------------------------
012700131217
012800131217     c     *Entry        plist
012900131217     c                   parm                    KPJBA
013000131217     c                   parm                    prm_FIOR56ds
013100131217
013200131217      /free
013300131217
013400131217       // -?Operazioni iniziali?
013500131217       exsr sr_RoutInz;
013600131217
013700131217       // -?Elaborazione *file FNORM/FNORG?
013800140121       // -?se immissione?
013900140121       IF  FIOR56ds.OR56cmd = 'I';
014000131217       exsr  sr_SQL_Prep;
014100131217       exsr  sr_SQL_Read;
014200131217       DoW  Not $Fine;
014300140121         k_DSTnfv = FNORGds.ORGndc;
014400140121         wpoe = FNORGds.ORGpoe;
014500140121         wnsr = FNORGds.ORGnsr;
014600140121         wnor = FNORGds.ORGnor;
014700140121         wnrv = FNORGds.ORGnrv;
014800131217         exsr  sr_Elaboraz;
014900131217         exsr  sr_SQL_Read;
015000131217       EndDo;
015100140121       ENDIF;
015200140121
015300140121       // -?Elaborazione singolo ORM?
015400140121       // -?se NON è immissione?
015500140121       IF  FIOR56ds.OR56cmd <> 'I';
015600140121         k_DSTnfv = FIOR56ds.OR56ndcD;
015700140121         wpoe = FIOR56ds.OR56poe;
015800140121         wnsr = FIOR56ds.OR56nsr;
015900140121         wnor = FIOR56ds.OR56nor;
016000140121         wnrv = FIOR56ds.OR56nrv;
016100140121         exsr  sr_Elaboraz;
016200140121       ENDIF;
016300131217
016400131217       // -?Operazioni finali?
016500131217       exsr sr_RoutEnd;
016600131217
016700131217       //--------------------------------------------------------------
016800131217       //?Operazioni iniziali.                                         ?
016900131217       //--------------------------------------------------------------
017000131217       BEGSR  sr_RoutInz;
017100131218
017200131218         // -?Impostazione opzioni SQL?
017300131218         exec sql  set option  DynUsrPrf = *Owner,
017400131218                               CloSqlCsr = *EndMod;
017500131217
017600131217         *inLR = *on;
017700131217
017800131217         // -?Verifica dei parametri ricevuti?
017900131217         if  %parms() = 1;
018000131217           FIOR56ds = KPJBU;
018100131217         else;
018200131217           FIOR56ds = prm_FIOR56ds;
018300131217         endif;
018400131217
018500131217         clear  FIOR56ds.or56err;
018600131217         clear  FIOR56ds.or56msg;
018700131217
018800140120         if  FIOR56ds.OR56cmd = *blanks;
018900131217           exsr  sr_RoutEnd;
019000131217         endif;
019100131217
019200131217         // -?Reperimento dati job?
019300131217         exsr  sr_DatiJob;
019400131217
019500131217         // -?Reperimento tab. "VPO"/"DECOFI816TEL"?
019600131217         if  getTabella ( c_Tntbe : 'VPO' : 'DECOFI816TEL' :
019700131217                          *blank : *blank : *blank :
019800131217                          *omit : *omit :
019900131217                          *omit : *omit : *omit : *omit :
020000131217                          *omit : *omit : *omit : *omit :
020100131217                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
020200131217                      = *zero      AND
020300131217             ds_TNTBE.TBEatb = *blank;
020400131218           dVPOdecofi816 = ds_TNTBE.TBEuni;
020500131217         endif;
020600131217
020700131217         // -?Verifica se la filiale della distinta è in tab. "VPO"?
020800131217         if  %lookup( %editc( FIOR56ds.OR56fgs : 'X' ) :
020900131217                      sk_AbiFilTelAUT ) = *zero;
021000131217           exsr  sr_RoutEnd;
021100131217         endif;
021200131217
021300131218         // -?Reperimento data odierna?
021400131218         wDate_Eur = %date();
021500131218         wDate = %dec( wDate_Eur );
021600131218
021700131218         // -?Reperimento orario?
021800131218         wTime = %dec( %time() );
021900131218
022000131218         // -?Impostazione campi chiave?
022100131218         clear  keyFIDST01;
022200131218         k_DSTfgs = FIOR56ds.OR56fgs;
022300131218         k_DSTnpg = 4;
022400131217
022500131217       ENDSR;
022600131217
022700131217       //--------------------------------------------------------------
022800131217       //?Reperimento Dati del job (Utente/Operativi).                 ?
022900131217       //--------------------------------------------------------------
023000131217       BEGSR  sr_DatiJob;
023100131217
023200131217         in(e) §AzUte;
023300131217         if NOT %error;
023400131217           in(e) §DatiUte;
023500131217         endif;
023600131217         if %error or RSut = *blank;
023700131217           tibs34r ( tibs34ds );
023800131217           in §AzUte;
023900131217           in §DatiUte;
024000131217         endif;
024100131217
024200131217       ENDSR;
024300131217
024400131217       //--------------------------------------------------------------
024500131217       //?Preperazione / Dichiarazione / Apertura cursore SQL          ?
024600131217       //--------------------------------------------------------------
024700131217       BEGSR  sr_SQL_Prep;
024800131217
024900131217         // -?Preparazione stringa SQL?
025000131218         wSQL = 'select FNORM00F.*, FNORG00F.* +
025100131218                   from FNORM00F inner join FNORG00F +
025200131218                     on FNORM00F.ORMpoe = FNORG00F.ORGpoe and +
025300131218                        FNORM00F.ORMnsr = FNORG00F.ORGnsr and +
025400131218                        FNORM00F.ORMnor = FNORG00F.ORGnor and +
025500131218                        FNORM00F.ORMnrv = FNORG00F.ORGnrv +
025600131218                  where FNORG00F.ORGpor = ' +
025700131218                          %editc( FIOR56ds.OR56fgs : 'X' ) +
025800131218                  ' and FNORG00F.ORGddc = ' +
025900131218                          %triml( %editc( FIOR56ds.OR56ddc : '3' ) ) +
026000131219                  ' and FNORM00F.ORMfao = 400 +
026100131218                    and FNORG00F.ORGndc between ' +
026200131218                          %editc( FIOR56ds.OR56ndcD : '3' ) + ' and ' +
026300131218                          %editc( FIOR56ds.OR56ndcA : '3' ) +
026400131218                ' order by FNORG00F.ORGfgs, FNORG00F.ORGndc';
026500131217
026600131217         // -?Dichiarazione cursore?
026700131217         exec sql   prepare S1   from :wSQL;
026800131217         exec sql   declare C1   cursor for S1;
026900131219
027000131219         if  SQLcode < *zero;
027100131219           $Fine = *on;
027200131219         endif;
027300131217
027400131217         // -?Apertura del cursore?
027500131217         exec sql   open C1;
027600131217
027700131217         if  SQLcode < *zero;
027800131218           $Fine = *on;
027900131217         endif;
028000131217
028100131217       ENDSR;
028200131217
028300131217       //--------------------------------------------------------------
028400131217       //?Lettura SQL                                                  ?
028500131217       //--------------------------------------------------------------
028600131217       BEGSR  sr_SQL_Read;
028700131217
028800131217         clear  FNORMds;
028900131217         clear  FNORGds;
029000131217
029100131217         exec sql   fetch next   from C1   into :FNORMds, :FNORGds;
029200131217
029300131217         select;
029400131217           when  SQLcode = 100;
029500131217             $Fine = *on;
029600131217           when  SQLcode < *zero;
029700131217             $Fine = *on;
029800131217         endsl;
029900131217
030000131217       ENDSR;
030100131217
030200131217       //--------------------------------------------------------------
030300131218       //?Elaborazione singolo O.R.M.                                  ?
030400131217       //--------------------------------------------------------------
030500131217       BEGSR  sr_Elaboraz;
030600131218
030700131218         // -?Verifica distinta?
030800131218         chain  %kds( keyFIDST01 )  FIDST000;
030900140114         if  Not %found(FIDST01L);
031000131218           leavesr;
031100131218         endif;
031200140114
031300140114         // -?Flag "Dati a PDA" & "Test su PDA"?
031400140114         dDSTflr = DSTflr;
031500140114         select;
031600140114           // ·?Distinta a PDA per ORM & in test x ORM => OK?
031700140114           when  (DSTpda = 'E'  or  DSTpda = 'O')  and
031800140114                 (dDSTflr.§DSTtstPda = 'O'  or  dDSTflr.§DSTtstPda = 'E');
031900140114           // ·?Distinta NO a PDA per ORM => OK?
032000140114           when  (DSTpda <> 'E'  and  DSTpda <> 'O');
032100140114           // ·?Altrimanti: da scartare?
032200140114           other;
032300140114             leavesr;
032400140114         endsl;
032500131218
032600131218         // -?Parzializzazioni:?
032700131218         Select;
032800131218           // ·?Tipo Uscita?
032900131218           When  FIOR56ds.OR56tUsc <> *blank  and
033000131218                 FIOR56ds.OR56tUsc <> DSTfpp;
033100131218             leavesr;
033200131218           // ·?Tipo Distinta Parcel?
033300131218           When  FIOR56ds.OR56tDis  = 'P'  and
033400131218                 DSTtpm            <> *blank;
033500131218             leavesr;
033600131218           // ·?Tipo Distinta Messaggerie?
033700131218           When  FIOR56ds.OR56tDis  = 'M'  and
033800131218                 FIOR56ds.OR56tDis <> DSTtpm;
033900131218             leavesr;
034000131218         EndSl;
034100131218
034200131218         // -?Controllo che NON ci sia la fase:?
034300131217         //  ?· "RCH" per le distinte su PDA?
034400131217         //  ?· "RQE" per le altre distinte?
034500131217         reset  FIOR90ds;
034600131218         if  DSTpda = 'O'  or  DSTpda = 'E';
034700131218           FIOR90ds.OR90fas = 'RCH';
034800131218         else;
034900131218           FIOR90ds.OR90fas = 'RQE';
035000131218         endif;
035100140121         FIOR90ds.OR90fgs = DSTfgs;
035200140121         FIOR90ds.OR90ndc = DSTnfv;
035300131218
035400131218         FIOR90R ( kpjba : FIOR90ds );
035500131218
035600131218         // -?Fase reperita => rec. scartato?
035700131218         if  FIOR90ds.OR90ndcOk <> *blank;
035800131217           leavesr;
035900131217         endif;
036000131218
036100131218
036200131218         // -?Registrazione rec. in "Telefonate AUT"?
036300131218         reset  FNLRX3ds;
036400140121         FNLRX3ds.iX3idDoc = %editc(wpoe : 'X' ) +
036500140121                             %editc(wnsr : 'X' ) +
036600140121                             %editc(wnor : 'X' ) +
036700140121                             %editc(wnrv : 'X' );
036800140121         FNLRX3ds.iX3fgs   = DSTfgs;
036900140121         FNLRX3ds.iX3nfv   = DSTnfv;
037000140121         FNLRX3ds.iX3dfv   = DSTdfv;
037100140121         FNLRX3ds.iX3pdr   = DSTpdr;
037200140120         FNLRX3ds.iX3var   = FIOR56ds.OR56var;
037300140120         FNLRX3ds.iX3cmd   = FIOR56ds.OR56cmd;
037400131218
037500131218         FNLRX3R ( KPJBA : FNLRX3ds );
037600131217
037700131217       ENDSR;
037800131217
037900131217       //--------------------------------------------------------------
038000131217       //?Operazioni finali.                                           ?
038100131217       //--------------------------------------------------------------
038200131217       BEGSR  sr_RoutEnd;
038300131217
038400131217         // -?Chiusura del cursore SQL?
038500131217         exec sql   close C1;
038600131217
038700131217         // -?Chiusura funzioni precedentemente aperte?
038800131217         cloTabella( c_Tntbe );
038900131217
039000131217         // -?Restituzione eventuali parametri?
039100131217         if  %parms() = 1;
039200131217           KPJBU = FIOR56ds;
039300131217         else;
039400131217           prm_FIOR56ds = FIOR56ds;
039500131217         endif;
039600131217
039700131217         // -?Uscita dal *pgm?
039800131217         return;
039900131217
040000131217       ENDSR;
040100131217
040200131217      /end-free

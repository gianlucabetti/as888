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
001900140213       // -?Anagrafica Autista
002000140213     fFIAPD01L  if   e           k disk
002100131217
002200131217       //--------------------------------------------------------------
002300131217       //?Definizione costanti.                                        ?
002400131217       //--------------------------------------------------------------
002500131217
002600131217       // -?Costanti per stringa SQL?
002700131217     d c_ForFetch      c                   const('FOR FETCH ONLY')
002800131217
002900131217       //--------------------------------------------------------------
003000131217       //?Definizione schiere.                                         ?
003100131217       //--------------------------------------------------------------
003200131217
003300131217       //--------------------------------------------------------------
003400131217       //?Definizione aree dati.                                       ?
003500131217       //--------------------------------------------------------------
003600131217
003700131217       // -?Dati utente?
003800131217     d §AzUte        e ds                  extname(AZUTE00F)
003900131217     d                                     dtaara
004000131217     d §DatiUte      e ds                  extname(dDatiUte)
004100131217     d                                     dtaara
004200131217
004300131217       //--------------------------------------------------------------
004400131217       //?Definizione strutture dati.                                  ?
004500131217       //--------------------------------------------------------------
004600131217
004700131217       // -?Status ds?
004800131217     d Status         sds
004900131217     d   SDSpgm          *proc
005000131217
005100131217       // -?Parametri ricevuti?
005200131217     d KPJBA         e ds
005300131217     d FIOR56ds      e ds                  inz   qualified
005400131217
005500131217       // -?Dati estratti via SQL?
005600131217     d FNORMds       e ds                  extname(FNORM00F)
005700131217     d                                     inz   qualified
005800131217     d FNORGds       e ds                  extname(FNORG00F)
005900131217     d                                     inz   qualified
006000131218
006100131218       // -?DS lettura campo DSTFLR di FIDST00F?
006200131218     d dDSTflr       e ds                  inz   qualified
006300131217
006400131217       //--------------------------------------------------------------
006500131217       //?Definizione variabili globali.                               ?
006600131217       //--------------------------------------------------------------
006700131217
006800131217       // -?Parametri ricevuti?
006900131217     d prm_FIOR56ds    s                   like(FIOR56ds)
007000131217
007100131217       // -?Flags booleani?
007200131217     d $Fine           s               n   inz
007300131218
007400131218       // -?Indici di schiera?
007500131218     d xx              s              5  0 inz
007600131217
007700131217       // -?Stringa SQL da eseguire?
007800131218     d wSQL            s           5000    inz   varying
007900131217
008000131217       // -?Campi di comodo?
008100131218     d wDate_Eur       s               d   inz  datfmt(*eur)
008200131217     d wDate           s              8  0 inz
008300131218     d wTime           s              6  0 inz
008400131218     d w100a           s            100    inz
008500140121
008600140121     d wpoe            s                   like(FNORGds.ORGpoe)
008700140121     d wnsr            s                   like(FNORGds.ORGnsr)
008800140121     d wnor            s                   like(FNORGds.ORGnor)
008900140121     d wnrv            s                   like(FNORGds.ORGnrv)
009000131217
009100131217       //--------------------------------------------------------------
009200131217       //?Definizione prototipi procedure.                             ?
009300131217       //--------------------------------------------------------------
009400131217
009500131217       // -?Reperimento dati utente?
009600131217     d TIBS34ds      e ds                  inz
009700131217      /copy gaitrasrc/srcProtoPR,TIBS34R
009800131217
009900131217       // -?Reperimento dati tabelle?
010000131217      /copy gaitrasrc/srcProtoPI,TRULTAB
010100131217      /copy gaitrasrc/srcProtoPR,TRULTAB
010200131217
010300131218       // -?Controllo fase distinta?
010400131217     d FIOR90ds      e ds                  inz   qualified
010500131218     d   OR90tla     e                     inz('C')
010600131217      /copy gaitrasrc/srcProtoPR,FIOR90R
010700131218
010800131218       // -?Scrittura file Telefonate AUT da Note AUT?
010900131218     d FNLRX3ds      e ds                  inz   qualified
011000131218     d   iX3tpDoc    e                     inz('R')
011100131218     d   oX3err      e                     inz(*off)
011200131218      /copy gaitrasrc/srcProtoPR,FNLRX3R
011300131217
011400131217       //--------------------------------------------------------------
011500131217       //?Definizione key-list.                                        ?
011600131217       //--------------------------------------------------------------
011700131217
011800131218     d keyFIDST01    e ds                  extname( FIDST01L : *key )
011900131218     d                                     prefix(k_)   inz
012000131217
012100140213     d keyFIAPD01    e ds                  extname( FIAPD01L : *key )
012200140213     d                                     prefix(k_)   inz
012300140213
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
019100140212
019200140212         if  FIOR56ds.OR56ndcA = *zero;
019300140212           FIOR56ds.OR56ndcA = FIOR56ds.OR56ndcD;
019400140212         endif;
019500131217
019600131217         // -?Reperimento dati job?
019700131217         exsr  sr_DatiJob;
019800131217
019900131218         // -?Reperimento data odierna?
020000131218         wDate_Eur = %date();
020100131218         wDate = %dec( wDate_Eur );
020200131218
020300131218         // -?Reperimento orario?
020400131218         wTime = %dec( %time() );
020500131218
020600131218         // -?Impostazione campi chiave?
020700131218         clear  keyFIDST01;
020800131218         k_DSTfgs = FIOR56ds.OR56fgs;
020900131218         k_DSTnpg = 4;
021000131217
021100131217       ENDSR;
021200131217
021300131217       //--------------------------------------------------------------
021400131217       //?Reperimento Dati del job (Utente/Operativi).                 ?
021500131217       //--------------------------------------------------------------
021600131217       BEGSR  sr_DatiJob;
021700131217
021800131217         in(e) §AzUte;
021900131217         if NOT %error;
022000131217           in(e) §DatiUte;
022100131217         endif;
022200131217         if %error or RSut = *blank;
022300131217           tibs34r ( tibs34ds );
022400131217           in §AzUte;
022500131217           in §DatiUte;
022600131217         endif;
022700131217
022800131217       ENDSR;
022900131217
023000131217       //--------------------------------------------------------------
023100131217       //?Preperazione / Dichiarazione / Apertura cursore SQL          ?
023200131217       //--------------------------------------------------------------
023300131217       BEGSR  sr_SQL_Prep;
023400131217
023500131217         // -?Preparazione stringa SQL?
023600131218         wSQL = 'select FNORM00F.*, FNORG00F.* +
023700131218                   from FNORM00F inner join FNORG00F +
023800131218                     on FNORM00F.ORMpoe = FNORG00F.ORGpoe and +
023900131218                        FNORM00F.ORMnsr = FNORG00F.ORGnsr and +
024000131218                        FNORM00F.ORMnor = FNORG00F.ORGnor and +
024100131218                        FNORM00F.ORMnrv = FNORG00F.ORGnrv +
024200131218                  where FNORG00F.ORGpor = ' +
024300131218                          %editc( FIOR56ds.OR56fgs : 'X' ) +
024400131218                  ' and FNORG00F.ORGddc = ' +
024500131218                          %triml( %editc( FIOR56ds.OR56ddc : '3' ) ) +
024600131219                  ' and FNORM00F.ORMfao = 400 +
024700131218                    and FNORG00F.ORGndc between ' +
024800131218                          %editc( FIOR56ds.OR56ndcD : '3' ) + ' and ' +
024900131218                          %editc( FIOR56ds.OR56ndcA : '3' ) +
025000131218                ' order by FNORG00F.ORGfgs, FNORG00F.ORGndc';
025100131217
025200131217         // -?Dichiarazione cursore?
025300131217         exec sql   prepare S1   from :wSQL;
025400131217         exec sql   declare C1   cursor for S1;
025500131219
025600131219         if  SQLcode < *zero;
025700131219           $Fine = *on;
025800131219         endif;
025900131217
026000131217         // -?Apertura del cursore?
026100131217         exec sql   open C1;
026200131217
026300131217         if  SQLcode < *zero;
026400131218           $Fine = *on;
026500131217         endif;
026600131217
026700131217       ENDSR;
026800131217
026900131217       //--------------------------------------------------------------
027000131217       //?Lettura SQL                                                  ?
027100131217       //--------------------------------------------------------------
027200131217       BEGSR  sr_SQL_Read;
027300131217
027400131217         clear  FNORMds;
027500131217         clear  FNORGds;
027600131217
027700131217         exec sql   fetch next   from C1   into :FNORMds, :FNORGds;
027800131217
027900131217         select;
028000131217           when  SQLcode = 100;
028100131217             $Fine = *on;
028200131217           when  SQLcode < *zero;
028300131217             $Fine = *on;
028400131217         endsl;
028500131217
028600131217       ENDSR;
028700131217
028800131217       //--------------------------------------------------------------
028900131218       //?Elaborazione singolo O.R.M.                                  ?
029000131217       //--------------------------------------------------------------
029100131217       BEGSR  sr_Elaboraz;
029200131218
029300131218         // -?Verifica distinta?
029400131218         chain  %kds( keyFIDST01 )  FIDST000;
029500140114         if  Not %found(FIDST01L);
029600131218           leavesr;
029700131218         endif;
029800140114
029900140213         // -?Impostazione campi chiave?
030000140213         clear  keyFIAPD01;
030100140213         k_APDPDR = DSTpdr;
030200140213         k_APDtip = 'A';
030300140213
030400140213         // -?Verifica se Autista escluso da fatturazione es "c/servizi" non crea FICAU
030500140213         chain  %kds( keyFIAPD01 )  FIAPD000;
030600140213         if  Not %found(FIAPD01L);
030700140213           leavesr;
030800140213         endif;
030900140213         if APDpdd = 'S'or APDatb <> ' ';
031000140213           leavesr;
031100140213         endif;
031200140213
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

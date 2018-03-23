000100160824       //==============================================================
000200160825       //?Statistica Spedizioni per Num. Telefono Destinatario.        ?
000300160824       //==============================================================
000400160824
000500160824       //--------------------------------------------------------------
000600160824       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700160824       //--------------------------------------------------------------
000800160824
000900160824     /*PRM dbgview(*source)
001000160824     /*END
001100160824
001200160824       //--------------------------------------------------------------
001300160824       //?Specifiche di controllo.                                     ?
001400160824       //--------------------------------------------------------------
001500160824
001600160824     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700160824     h dftactgrp(*no)
001800160825     h bnddir('TRUL')
001900160824     h alwnull(*inputonly)
002000160824
002100160824       //--------------------------------------------------------------
002200160824       //?Dichiarazione file.                                          ?
002300160824       //--------------------------------------------------------------
002400160829
002500160829       // -?BOLLE SEDE: da Fatturare + fatturate "ultimi mesi"?
002600160829       //  ?                         + fatturate "storico"?
002700160829     fTITAS30C  if   e           k disk
002800160829
002900160902       // -?BOLLE SEDE: Anagrafiche (con "Storico")?
003000160902     fTITAA30C  if   e           k disk    usropn
003100160902
003200160829       // -?BOLLE SEDE: Riferimenti bolla (con "Storico")?
003300160829     fTITA430C  if   e           k disk
003400160825
003500160825       // -?Archivio Contrassegni - Bolle -?
003600160825     fTNCSB03L  if   e           k disk    usropn
003700160824
003800160824       // -?Work-File di Output?
003900160825     fWFTELD0F  o    e             disk    usropn
004000160824
004100160824       // -?Stampa segnalazioni di errore?
004200160824     fQSYSPRT   o    f  132        printer usropn
004300160824     f                                     oflind(*inOF)
004400160824
004500160824       //--------------------------------------------------------------
004600160824       //?Definizione costanti.                                        ?
004700160824       //--------------------------------------------------------------
004800160824
004900160824
005000160824       //--------------------------------------------------------------
005100160824       //?Definizione schiere.                                         ?
005200160824       //--------------------------------------------------------------
005300160824
005400160824
005500160824       //--------------------------------------------------------------
005600160824       //?Definizione aree dati.                                       ?
005700160824       //--------------------------------------------------------------
005800160824
005900160824       // -?Dati utente?
006000160824     d §AzUte        e ds                  extname(AZUTE00F)
006100160824     d                                     dtaara
006200160824     d §DatiUte      e ds                  extname(dDatiUte)
006300160824     d                                     dtaara
006400160824
006500160824       //--------------------------------------------------------------
006600160824       //?Definizione strutture dati.                                  ?
006700160824       //--------------------------------------------------------------
006800160824
006900160824       // -?Status ds?
007000160824     d Status         sds
007100160824     d   SDSpgmName      *proc
007200160824
007300160824       // -?Parametri ricevuti?
007400160824     d KPJBA         e ds
007500160825     d TNSB58ds      e ds                  inz   qualified
007600160825
007700160825       // -?TITA4 tipo record "A"?
007800160825     d dTA4a         e ds                  inz   qualified
007900160825
008000160825       // -?Tab. "3A" = Codici Bolla?
008100160825     d ds3A          e ds                  inz   qualified
008200160825       // -?Tab. "QT" = Campi per tassazione STD?
008300160825     d dsQT1         e ds                  inz   qualified
008400160824
008500160829       // -?FIAR5 tipo record "GEN"?
008600160829     d dAR5gen       e ds                  based(nullPtr)  qualified
008700160829
008800160825       // -?Dati estratti via SQL?
008900160825     d FIAR500F      e ds                  based(nullPtr)  qualified
009000160824
009100160825     d wSQL_ds         ds                  inz   qualified
009200160825     d   AR5telD                           inz   like(dAR5gen.§AR5telD)
009300160825     d   AR5aas                            inz   like(FIAR500F.AR5aas)
009400160825     d   AR5lnp                            inz   like(FIAR500F.AR5lnp)
009500160825     d   AR5nrs                            inz   like(FIAR500F.AR5nrs)
009600160825     d   AR5nsp                            inz   like(FIAR500F.AR5nsp)
009700160825
009800160825       // -?Data / Ora del *job?
009900160825     d ds_Time14       ds            14    inz   qualified
010000160825     d   wDate                        8  0 inz
010100160825     d   wTime                        6  0 inz
010200160824
010300160825       // -?Schiera per "scomposizione" stringa SQL in stampa?
010400160824     d ds_Temp         ds         10000    inz
010500160824     d   sk_Temp               1  10000    inz   dim(100)
010600160824
010700160824       //--------------------------------------------------------------
010800160824       //?Definizione variabili globali.                               ?
010900160824       //--------------------------------------------------------------
011000160824
011100160824       // -?Flags booleani?
011200160824     d $EoF            s               n   inz
011300160824
011400160824       // -?Indici di schiera / Contatori?
011500160824     d xx              s              5  0 inz
011600160824
011700160824       // -?Stringhe SQL da eseguire?
011800160824     d wSQL            s          32740    inz   varying
011900160824
012000160824       // -?Campi di comodo?
012100160825     d wDate           s              8  0 inz
012200160825     d wTime           s              6  0 inz
012300160824     d wDate_Eur       s               d   inz  datfmt(*eur)
012400160829     d wFPF            s                   inz  like(TASfpf)
012500160825     d wTara           s                   inz  like(dsQT1.§QTtpc)
012600160824
012700160824       //--------------------------------------------------------------
012800160824       //?Definizione prototipi procedure.                             ?
012900160824       //--------------------------------------------------------------
013000160824
013100160824       // -?Reperimento dati utente?
013200160824     d TIBS34ds      e ds                  inz
013300160824      /copy gaitrasrc/srcProtoPR,TIBS34R
013400160825
013500160825       // -?Reperimento dati tabelle?
013600160825      /copy gaitrasrc/srcProtoPI,TRULTAB
013700160825      /copy gaitrasrc/srcProtoPR,TRULTAB
013800160826
013900160826       // -?Controllo/Decodifica cliente?
014000160826      /copy gaitrasrc/srcProtoPI,TIBS69R
014100160826      /copy gaitrasrc/srcProtoPR,TIBS69R
014200160824
014300160824       // -?Parametri API QCAPCMD (Process Commands)?
014400160824     d Qcmd            s           2048    inz  varying
014500160824      /copy qSysInc/qRpgleSrc,QCAPCMD
014600160824       // -?API QCAPCMD (Process Commands)?
014700160824      /copy gaitrasrc/srcProtoPR,QCAPCMD
014800160824
014900160824       // -?Parametri gestione errori API.?
015000160824      /copy qsysinc/qrpglesrc,QUSEC
015100160824
015200160824       //--------------------------------------------------------------
015300160824       //?Definizione key-list.                                        ?
015400160824       //--------------------------------------------------------------
015500160829
015600160829       // -?File TITAS30C?
015700160829     d keyTITAS30    e ds                  extname( TITAS30C : *key )
015800160829     d                                     prefix( k_ )  inz
015900160829
016000160829       // -?File TITA430C?
016100160829     d keyTITA430    e ds                  extname( TITA430C : *key )
016200160829     d                                     prefix( k_ )  inz
016300160826
016400160826       // -?File TITAA30C?
016500160826     d keyTITAA30    e ds                  extname( TITAA30C : *key )
016600160826     d                                     prefix( k_ )  inz
016700160824
016800160825       // -?File TNCSB03L?
016900160825     d keyTNCSB03    e ds                  extname( TNCSB03L : *key )
017000160825     d                                     prefix( k_ )  inz
017100160824
017200160824       //--------------------------------------------------------------
017300160824       //?Riepilogo indicatori utilizzati.                             ?
017400160824       //--------------------------------------------------------------
017500160824       // *inOF     - OverFlow in QSYSPRT
017600160824       //--------------------------------------------------------------
017700160824
017800160824       //--------------------------------------------------------------
017900160824       //?M A I N - L I N E                                            ?
018000160824       //--------------------------------------------------------------
018100160824
018200160824     c     *Entry        plist
018300160824     c                   parm                    KPJBA
018400160824
018500160824      /free
018600160824
018700160824       // -?Operazioni iniziali?
018800160824       exsr  sr_RoutInz;
018900160824
019000160825       // -?Preparazione stringa SQL per l'estrazione dei dati?
019100160824       exsr  sr_PrepSQL;
019200160824
019300160824       // -?Ciclo di elaborazione?
019400160824       reset  $EoF;
019500160824       exsr  sr_OpenCursor;
019600160824
019700160824       DoW  $EoF = *off;
019800160824         exsr  sr_ReadCursor;
019900160824       EndDo;
020000160824
020100160824       exsr  sr_CloseCursor;
020200160824
020300160824       // -?Operazioni finali?
020400160824       exsr sr_RoutEnd;
020500160824
020600160824       //--------------------------------------------------------------
020700160824       //?Operazioni iniziali.                                         ?
020800160824       //--------------------------------------------------------------
020900160824       BEGSR  sr_RoutInz;
021000160824
021100160824         // -?Impostazione opzioni per SQL?
021200160824         exec sql   set  option  DynUsrPrf = *Owner,
021300160824                                 CloSqlCsr = *EndMod;
021400160824
021500160824         // -?Impostazione chiusura?
021600160824         *inLR = *on;
021700160824
021800160824         // -?Reperimento dati job?
021900160824         exsr  sr_DatiJob;
022000160824
022100160824         // -?Reperimento parametri?
022200160825         TNSB58ds = kpjbu;
022300160825
022400160825         // -?Richiesta pulizia del *file WFTELD0F?
022500160825         if  TNSB58ds.SB58cwf = 'S';
022600160826           //*·wSQL = 'DELETE from WFTELD0F +
022700160826           //*·         where WTDpUte = ''' + KNMUS + '''';
022800160826           //*·exec sql  execute  immediate :wSQL;
022900160826           Qcmd = 'CLRPFM file(*libl/WFTELD0F)';
023000160826           exsr  sr_ExecCmd;
023100160826           if  Qusei <> *blank;
023200160826             exsr  sr_RoutEnd;
023300160826           endif;
023400160825         endif;
023500160825
023600160825         open  WFTELD0F;
023700160824
023800160824       ENDSR;
023900160824
024000160824       //--------------------------------------------------------------
024100160824       //?Reperimento Dati del job (Utente/Operativi).                 ?
024200160824       //--------------------------------------------------------------
024300160824       BEGSR  sr_DatiJob;
024400160824
024500160824         in(e) §AzUte;
024600160824         if NOT %error;
024700160824           in(e) §DatiUte;
024800160824         endif;
024900160824         if %error or RSut = *blank;
025000160824           tibs34r ( tibs34ds );
025100160824           in §AzUte;
025200160824           in §DatiUte;
025300160824         endif;
025400160824
025500160824       ENDSR;
025600160824
025700160824       //--------------------------------------------------------------
025800160824       //?Preparazione stringa SQL per estrazione dati.                ?
025900160824       //--------------------------------------------------------------
026000160824       BEGSR  sr_PrepSQL;
026100160824
026200160829         wSQL = 'select substr( AR5uni, 36, 16 ), +
026300160829                        AR5aas, AR5lnp, AR5nrs, AR5nsp +
026400160829
026500160829                   from FIAR5P0F +
026600160829
026700160829                  where AR5aas between ' +
026800160829                        %subst( %editc( TNSB58ds.SB58dsi : 'X' ) : 1 : 4 ) +
026900160829                        ' and ' +
027000160829                        %subst( %editc( TNSB58ds.SB58dsf : 'X' ) : 1 : 4 ) +
027100160829                  ' and AR5trd = ''GEN'' +
027200160901                    and substr( AR5uni, 36, 16 ) like ''%' +
027300160901                        %trim( TNSB58ds.SB58tel ) + '%'' +
027400160829
027500160829                  UNION +
027600160829
027700160829                 select substr( AR5uni, 36, 16 ), +
027800160829                        AR5aas, AR5lnp, AR5nrs, AR5nsp +
027900160829
028000160829                   from FIAR500F +
028100160829
028200160829                  where AR5aas between ' +
028300160829                        %subst( %editc( TNSB58ds.SB58dsi : 'X' ) : 1 : 4 ) +
028400160829                        ' and ' +
028500160829                        %subst( %editc( TNSB58ds.SB58dsf : 'X' ) : 1 : 4 ) +
028600160829                  ' and AR5trd = ''GEN'' +
028700160901                    and substr( AR5uni, 36, 16 ) like ''%' +
028800160901                        %trim( TNSB58ds.SB58tel ) + '%'' +
028900160829
029000160829                  order by AR5aas, AR5lnp, AR5nrs, AR5nsp +
029100160824
029200160829                    for fetch only';
029300160824
029400160824       ENDSR;
029500160824
029600160824       //--------------------------------------------------------------
029700160824       //?Apertura cursore.                                            ?
029800160824       //--------------------------------------------------------------
029900160824       BEGSR  sr_OpenCursor;
030000160824
030100160824         // -?Dichiarazione cursore?
030200160824         exec sql   prepare S1   from :wSQL;
030300160824         exec sql   declare C1   cursor for S1;
030400160824
030500160824         if  SQLcode < *zero;
030600160824           $EoF = *on;
030700160824           exsr  sr_PrintERR;
030800160824         endif;
030900160824
031000160824         // -?Apertura del cursore?
031100160824         exec sql   open C1;
031200160824
031300160824         if  SQLcode < *zero;
031400160824           $EoF = *on;
031500160824           exsr  sr_PrintERR;
031600160824         endif;
031700160824
031800160824       ENDSR;
031900160824
032000160824       //--------------------------------------------------------------
032100160824       //?Chiusura cursore.                                            ?
032200160824       //--------------------------------------------------------------
032300160824       BEGSR  sr_CloseCursor;
032400160824
032500160824         // -?Chiusura del cursore?
032600160824         exec sql   close C1;
032700160824
032800160824       ENDSR;
032900160824
033000160824       //--------------------------------------------------------------
033100160824       //?Lettura cursore.                                             ?
033200160824       //--------------------------------------------------------------
033300160824       BEGSR  sr_ReadCursor;
033400160824
033500160825         clear  wSQL_ds;
033600160824
033700160825         exec sql   fetch next   from C1   into :wSQL_ds;
033800160824
033900160824         select;
034000160824           when  SQLcode = 100;
034100160824             $EoF = *on;
034200160824           when  SQLcode < *zero;
034300160824             $EoF = *on;
034400160824             exsr  sr_PrintERR;
034500160824           other;
034600160825             exsr  sr_Wrt_WFTELD;
034700160824         endsl;
034800160824
034900160824       ENDSR;
035000160824
035100160824       //--------------------------------------------------------------
035200160829       //?Registrazione dati della spedizione estratta.                ?
035300160824       //--------------------------------------------------------------
035400160825       BEGSR  sr_Wrt_WFTELD;
035500160829
035600160829         // -?Reperimento dati della spedizione?
035700160829         clear  keyTITAS30;
035800160829         k_TASaas = wSQL_ds.AR5aas;
035900160829         k_TASlnp = wSQL_ds.AR5lnp;
036000160829         k_TASnrs = wSQL_ds.AR5nrs;
036100160829         k_TASnsp = wSQL_ds.AR5nsp;
036200160901         chain  %kds( keyTITAS30 : 4 )  TITAS30C;
036300160829
036400160829         // -?Filtro per data spedizione?
036500160829         if  NOT %found( TITAS30C )                              or
036600160829             ( ( TASaas * 10000 ) + TASmgs ) < TNSB58ds.SB58dsi  or
036700160829             ( ( TASaas * 10000 ) + TASmgs ) > TNSB58ds.SB58dsf;
036800160829           leavesr;
036900160829         endif;
037000160824
037100160829         // -?Registrazione record in WFTELD0F?
037200160825         clear  WFTELD00;
037300160825
037400160825         WTDpUte = KNMUS;
037500160825         WTDpDtL = %dec( %date() );
037600160825         WTDpTel = TNSB58ds.SB58tel;
037700160825         WTDpDtI = TNSB58ds.SB58dsi;
037800160825         WTDpDtF = TNSB58ds.SB58dsf;
037900160825
038000160825         WTDlnp  = wSQL_ds.AR5lnp;
038100160825         WTDnrs  = wSQL_ds.AR5nrs;
038200160825         WTDnsp  = wSQL_ds.AR5nsp;
038300160829         WTDdsp  = ( wSQL_ds.AR5aas * 10000 ) + TASmgs;
038400160825
038500160829         WTDrsd  = TASrsd;
038600160829         WTDind  = TASind;
038700160829         WTDcad  = TAScad;
038800160829         WTDlod  = TASlod;
038900160829         WTDprd  = TASprd;
039000160829         //*·WTDnzd  = TASnzd;
039100160901
039200160829         WTDrmn  = TASrmn;
039300160901
039400160829         WTDdcm  = TASdcm;
039500160901
039600160829         WTDncl  = TASncl;
039700160901         WTDpkf  = TASpkb;
039800160901         WTDvlf  = TASvlb;
039900160826
040000160826         // -?Decodifica Mittente?
040100160826         // ·?SE non codificato: aggancia TITAA?
040200160829         IF  %subst( %editc( TASccm : 'X' ) : 4 : 4 ) = *zero  or
040300160829             %subst( %editc( TASccm : 'X' ) : 4 : 4 ) = *all'8';
040400160826
040500160826           if  Not %open( TITAA30C );
040600160826             open  TITAA30C;
040700160826           endif;
040800160826           clear  keyTITAA30;
040900160826           k_TAAaas = wSQL_ds.AR5aas;
041000160826           k_TAAlnp = wSQL_ds.AR5lnp;
041100160826           k_TAAnrs = wSQL_ds.AR5nrs;
041200160826           k_TAAnsp = wSQL_ds.AR5nsp;
041300160826           k_TAAtrc = 'M';
041400160826           chain  %kds( keyTITAA30 )  TITAA30C;
041500160826           If  %found( TITAA30C );
041600160826             WTDrsm  = TAArsc;
041700160826             WTDinm  = TAAind;
041800160826             WTDcam  = TAAcap;
041900160826             WTDlom  = TAAloc;
042000160826             WTDprm  = TAAprv;
042100160826             WTDnzm  = TAAnaz;
042200160826           EndIf;
042300160826
042400160826         // ·?SE codificato: aggancia Anagrafica?
042500160826         ELSE;
042600160826
042700160826           clear  TIBS69ds;
042800160826           I69sif = knsif;
042900160826           I69kcc = DUTkci;
043000160829           I69kac = TASccm;
043100160826           I69kin = I69kac;
043200160826           I69kcs = I69kac;
043300160826           tibs69r( TIBS69ds :
043400160826                    ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
043500160826           If  O69err = *blank;
043600160826             WTDrsm  = ACOrag;
043700160826             WTDinm  = INDvia;
043800160826             WTDcam  = INDcae;
043900160826             WTDlom  = INDcit;
044000160826             WTDprm  = INDprv;
044100160826             WTDnzm  = INDsta;
044200160826           EndIf;
044300160826
044400160826         ENDIF;
044500160829
044600160829         // -?Reperimento dati dal *file TITA4 / trk "A"?
044700160829         //  ?(Rif.Mitt.Alfan. e Nat.Merce)?
044800160829         clear  dTA4a;
044900160829         clear  keyTITA430;
045000160829         k_TA4aas = wSQL_ds.AR5aas;
045100160829         k_TA4lnp = wSQL_ds.AR5lnp;
045200160829         k_TA4nrs = wSQL_ds.AR5nrs;
045300160829         k_TA4nsp = wSQL_ds.AR5nsp;
045400160829         k_TA4trc = 'A';
045500160829         chain  %kds( keyTITA430 )  TITA430C;
045600160829         If  %found( TITA430C );
045700160829           dTA4a   = TA4not;
045800160829           WTDrma  = dTA4a.§TA4Arma;
045900160829           WTDnat  = dTA4a.§TA4Anas;
046000160829         EndIf;
046100160825
046200160825         // -?Reperimento dati dal *file TNCSB?
046300160829         If  TASfca = 'S';
046400160825           if  Not %open( TNCSB03L );
046500160825             open  TNCSB03L;
046600160825           endif;
046700160825           clear  keyTNCSB03;
046800160825           k_CSBaas = wSQL_ds.AR5aas;
046900160825           k_CSBlnp = wSQL_ds.AR5lnp;
047000160825           k_CSBnrs = wSQL_ds.AR5nrs;
047100160825           k_CSBnsp = wSQL_ds.AR5nsp;
047200160825           chain  %kds( keyTNCSB03 )  TNCSB000;
047300160901           if  %found( TNCSB03L )  and
047400160902               CSBsta <> 9;
047500160825             WTDcas = CSBcas;
047600160825             //*·WTDvca = CSBvca;
047700160825           endif;
047800160825         EndIf;
047900160825
048000160825         // -?Scrittura rec. nel *file WFTELD0F?
048100160825         write  WFTELD00;
048200160824
048300160824       ENDSR;
048400160824
048500160824       //--------------------------------------------------------------
048600160824       //?Stampa segnalazione dell'errore rilevato via SQL             ?
048700160824       //--------------------------------------------------------------
048800160824       BEGSR  sr_PrintERR;
048900160824
049000160824         // -?Stampa del Dump?
049100160824         Dump(A);
049200160824
049300160824         // -?Stampa del Job-Log?
049400160824         Qcmd = 'DSPJOBLOG job(*) output(*print)';
049500160824         exsr  sr_ExecCmd;
049600160824
049700160824         // -?Apertura del printer-file?
049800160825         if  Not %open( QSYSPRT );
049900160824           open  QSYSPRT;
050000160825           ds_Time14 = %subst( %char( %dec( %timestamp() ) )
050100160825                               : 1 : 14 );
050200160825           wDate_Eur = %date( ds_Time14.wDate : *iso );
050300160825           wDate     = %dec( wDate_Eur );
050400160825           wTime     = ds_Time14.wTime;
050500160824           except  PRTtxt;
050600160824         endif;
050700160824
050800160824
050900160824         // -?Stampa dell'errore SQL (e chiusura del *pgm)?
051000160824         clear  ds_Temp;
051100160824         ds_Temp = wSQL;
051200160824         if  *inOF;
051300160824           except  PRTtxt;
051400160824           *inOF = *off;
051500160824         endif;
051600160824         except  PRTerr1;
051700160824         *inOF = *off;
051800160824         For  xx = 1  To  %elem(sk_Temp);
051900160824           if  sk_Temp(xx) = *blank;
052000160824             leave;
052100160824           endif;
052200160824           if  *inOF;
052300160824             except  PRTtxt;
052400160824             *inOF = *off;
052500160824           endif;
052600160824           except  PRTerr2;
052700160824         EndFor;
052800160824
052900160824         // -?Chiusura del programma (DOPO)?
053000160824         exsr  sr_RoutEnd;
053100160824
053200160824       ENDSR;
053300160824
053400160824       //--------------------------------------------------------------
053500160824       //?Esecuzione del comando (già impostato).                      ?
053600160824       //--------------------------------------------------------------
053700160824       BEGSR  sr_ExecCmd;
053800160824
053900160824         clear Qcap0100;
054000160824         Qcabcsdh = *off;
054100160824         Qcapa    = *off;
054200160824         Qcacmdss = *off;
054300160824         Qcaerved = *allX'00';
054400160824
054500160824         clear Qusec;
054600160824         Qusbprv  = %size(Qusec);
054700160824
054800160824         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
054900160824                           %size(Qcap0100) : 'CPOP0100' : *omit :
055000160824                           0 : 0 : Qusec);
055100160824
055200160824         //if  Qusei <> *blank;
055300160824         //  ...;
055400160824         //endif;
055500160824
055600160824       ENDSR;
055700160825
055800160825       //--------------------------------------------------------------
055900160825       //?Operazioni finali.                                           ?
056000160825       //--------------------------------------------------------------
056100160825       BEGSR  sr_RoutEnd;
056200160825
056300160825         // -?Chiusura funzioni precedentemente aperte?
056400160825         cloTabella ( c_Tabel );
056500160826
056600160826         reset  TIBS69ds;
056700160826         tibs69r( TIBS69ds :
056800160826                  ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
056900160825
057000160825         // -?Chiusura printer-file Errori?
057100160825         if  %open( QSYSPRT );
057200160825           close  QSYSPRT;
057300160825         endif;
057400160825
057500160825         // -?Uscita dal *pgm?
057600160825         return;
057700160825
057800160825       ENDSR;
057900160824
058000160824      /end-free
058100160824
058200160824       //--------------------------------------------------------------
058300160824       //?S P O O L - F I L E S                                        ?
058400160824       //--------------------------------------------------------------
058500160824
058600160824     oQSYSPRT   e            PRTtxt            1
058700160824     o                       RSUT
058800160824     o                                        +   5 'ERRORI RILEVATI I-
058900160825     o                                              N FASE DI PREPARAZ-
059000160825     o                                              IONE WORKFILE WFTE-
059100160825     o                                              LD0F'
059200160824     o                       SDSpgmName       +   5
059300160824     o                       wDate         y  +   3
059400160824     o          e            PRTtxt      0
059500160824     o                                        +  25 'ERRORI RILEVATI I-
059600160825     o                                              N FASE DI PREPARAZ-
059700160825     o                                              IONE WORKFILE WFTE-
059800160825     o                                              LD0F'
059900160824     o          e            PRTtxt      1
060000160824     o                       KNSIF
060100160824     o                       KNMUS            +   1
060200160825     o                                        +   4 '                 -
060300160825     o                                                                -
060400160825     o                                                                -
060500160825     o                                                  '
060600160824     o                                        +   5 'Pag.'
060700160824     o                       Page          z  +   0
060800160824     o                       wTime            +   5 '0 :  :  '
060900160824      *
061000160824     o          e            PRTerr1     2
061100160824     o                                              'RILEVATO ERRORE: +
061200160824     o                                               SQLCODE'
061300160824     o                       SQLcode       k  +   1
061400160824     o                                        +   1 'SQLSTATE'
061500160824     o                       SQLstate         +   1
061600160824     o                                        +   1 'DURANTE L''ESECUZ-
061700160824     o                                              IONE DEL SEGUENTE -
061800160824     o                                              COMANDO SQL:'
061900160824     o          e            PRTerr1     0  1
062000160824     o                                              'RILEVATO ERRORE: +
062100160824     o                                               SQLCODE'
062200160824     o                       SQLcode       k  +   1
062300160824     o                                        +   1 'SQLSTATE'
062400160824     o                       SQLstate         +   1
062500160824     o          e            PRTerr2     1
062600160824     o                       sk_Temp(xx)      +   3

000100100407       //==============================================================
000200100407       //?TIBS29R - Cancellazione Profili di massa (batch)            ?
000300100407       //==============================================================
000400100407
000500100407       //--------------------------------------------------------------
000600100407       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700100407       //--------------------------------------------------------------
000800100407
000900100407     /*PRM dbgview(*source)
001000100407     /*END
001100100407
001200100407       //--------------------------------------------------------------
001300100407       //?Specifiche di controllo.                                     ?
001400100407       //--------------------------------------------------------------
001500100407
001600100407     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700100408     h dftactgrp(*no)
001800100407
001900100407       //--------------------------------------------------------------
002000100407       //?Dichiarazione file.                                          ?
002100100407       //--------------------------------------------------------------
002200100407
002300100407       // -?Utenti aziendali?
002400100407     fAZUTE01L  Uf   e           k disk
002500161010       // -?NOTE Utenti aziendali?
002600161010     fAZUTN01L  Uf   e           k disk
002700100408       // -?Profili Modulo Base Proj?
002800100407     fKPPRF01L  Uf   e           k disk
002900100408
003000100408       // -?Stampa?
003100100408     fQPRINT    o    f  132        printer  oflind(*inOF)  usropn
003200100407
003300100407       //--------------------------------------------------------------
003400100407       //?Definizione costanti.                                        ?
003500100407       //--------------------------------------------------------------
003600100407
003700100407
003800100407       //--------------------------------------------------------------
003900100407       //?Definizione schiere.                                         ?
004000100407       //--------------------------------------------------------------
004100100407
004200100407
004300100407       //--------------------------------------------------------------
004400100407       //?Definizione aree dati.                                       ?
004500100407       //--------------------------------------------------------------
004600100407
004700100407       // -?Dati utente?
004800100407     d §AzUte        e ds                  extname(AZUTE00F)
004900100407     d                                     dtaara
005000100407     d §DatiUte      e ds                  extname(dDatiUte)
005100100407     d                                     dtaara
005200100407
005300100407       //--------------------------------------------------------------
005400100407       //?Definizione strutture dati.                                  ?
005500100407       //--------------------------------------------------------------
005600100407
005700100407       // -?Status ds?
005800100407     d Status         sds
005900100407     d  SDSpgm           *proc
006000100408
006100100408       // -?Parametri per Reperimento dati utente?
006200100408     d TIBS34ds      e ds                  inz
006300100408
006400100408       // -?(Altri) Flags utenti aziendali?
006500100408     d dUte02        e ds                  inz
006600100407
006700100407       // -?Parametri ricevuti?
006800100407     d KPJBA         e ds
006900100407     d TIBS29ds        ds                  inz
007000100407     d   D29dsc                       8  0 inz
007100100408     d   D29lst                       1    inz
007200100408     d   D29dlt                       1    inz
007300100408
007400100408       // -?Data e Ora correnti?
007500100408     d ds_Time         ds            20    inz
007600100408     d   wDate                        8s 0 inz
007700100408     d   wTime                        6s 0 inz
007800100408     d   wMmSs                        6s 0 inz
007900100407
008000100407       //--------------------------------------------------------------
008100100407       //?Definizione variabili globali.                               ?
008200100407       //--------------------------------------------------------------
008300100407
008400100407       // -?Flags booleani?
008500100408     d $Fine           s               n   inz
008600100408     d $Err            s               n   inz
008700100408
008800100408       // -?Campi di stampa?
008900100408     d o_Txt1          s             15    inz
009000100408     d o_Txt2          s             30    inz
009100100408     d o_Dsc           s                   inz  like(D29dsc)
009200100416     d o_UTEdsc        s                   inz  like(UTEdsc)
009300100408
009400100408       // -?Campi di comodo?
009500100408     d wDate_Eur       s               d   inz  datfmt(*eur)
009600100407
009700100407       //--------------------------------------------------------------
009800100408       //?Definizione procedure.                                       ?
009900100407       //--------------------------------------------------------------
010000100407
010100100407       // -?Reperimento dati utente?
010200100407      /copy gaitrasrc/srcProtoPR,TIBS34R
010300100408
010400100408       // -?Cancellazione profilo utente?
010500100408     d tibs29c         pr                  extpgm('TIBS29C')
010600100408     d   D29usrprf                         like(UTEute)
010700100408     d   D29esito                     1a
010800100408
010900100408       // -?API QCAPCMD (Process Commands)?
011000100408     d Qcmd            s           2048    inz  varying
011100100408      /copy qSysInc/qRpgleSrc,QCAPCMD
011200100408      /copy gaitrasrc/srcProtoPR,QCAPCMD
011300100408
011400100408       // -?Parametri gestione errori API.?
011500100408      /copy qsysinc/qrpglesrc,QUSEC
011600100407
011700100407       //--------------------------------------------------------------
011800100407       //?Definizione key-list.                                        ?
011900100407       //--------------------------------------------------------------
012000100407
012100100407
012200100407       //--------------------------------------------------------------
012300100407       //?M A I N - L I N E                                            ?
012400100407       //--------------------------------------------------------------
012500100407
012600100407     c     *Entry        plist
012700100407     c                   parm                    KPJBA
012800100407
012900100407      /free
013000100407
013100100407       // -?Operazioni iniziali?
013200100407       exsr  sr_RoutInz;
013300100407
013400100408       // -?Elaborazione (cancellazione profili di massa)?
013500100408       exsr  sr_ElabUsrPrf;
013600100407
013700100407       // -?Operazioni finali?
013800100407       exsr sr_RoutEnd;
013900100407
014000100407       //--------------------------------------------------------------
014100100407       //?Operazioni iniziali.                                         ?
014200100407       //--------------------------------------------------------------
014300100407       BEGSR  sr_RoutInz;
014400100407
014500100407         *inLR = *on;
014600100408
014700100408         // -?Impostazione parametri?
014800100408         if  KPJBU <> *blank;
014900100408           // -?Ricevuti (dal filtro di lancio)?
015000100408           TIBS29ds = KPJBU;
015100100408         else;
015200100408           // -?Non ricevuti (lavoro schedulato)?
015300100408           clear  TIBS29ds;
015400100408           D29dsc = %dec( %date() );
015500100408           D29dlt = 'S';
015600100408         endif;
015700100407
015800100407         // -?Reperimento dati job?
015900100407         exsr  sr_DatiJob;
016000100517
016100100517         // -?Apertura file di stampa?
016200100517         If  D29lst = 'S'   and   Not %open(Qprint);
016300100517           open  Qprint;
016400100517           select;
016500100517             when  D29lst = 'S'   and   D29dlt = *blank;
016600100517               o_Txt1 = 'CANCELLABILI   ';
016700100517             when  D29lst = 'S'   and   D29dlt = 'S';
016800100517               o_Txt1 = 'CANCELLATI     ';
016900100517             when  D29lst = ' '   and   D29dlt = 'S';
017000100517               o_Txt1 = 'NON CANCELLATI ';
017100100517           endsl;
017200100517           ds_Time   = %char( %dec( %timestamp() ) );
017300100517           wDate_Eur = %date( wDate : *iso );
017400100517           wDate     = %dec( wDate_Eur );
017500100517           wDate_Eur = %date( D29dsc : *iso );
017600100517           o_Dsc     = %dec( wDate_Eur );
017700100517           except  PrtTxt;
017800100517           *inOF = *off;
017900100517         EndIf;
018000100407
018100100407       ENDSR;
018200100407
018300100407       //--------------------------------------------------------------
018400100407       //?Reperimento Dati del job (Utente/Operativi).                 ?
018500100407       //--------------------------------------------------------------
018600100407       BEGSR  sr_DatiJob;
018700100407
018800100407         in(e) §AzUte;
018900100407         if NOT %error;
019000100407           in(e) §DatiUte;
019100100407         endif;
019200100407         if %error or RSut = *blank;
019300100407           tibs34r ( tibs34ds );
019400100407           in §AzUte;
019500100407           in §DatiUte;
019600100407         endif;
019700100407
019800100407       ENDSR;
019900100407
020000100407       //--------------------------------------------------------------
020100100408       //?Cancellazione Profili di massa.                              ?
020200100407       //--------------------------------------------------------------
020300100408       BEGSR  sr_ElabUsrPrf;
020400100407
020500100408         // -?Ciclo di lettura file AZUTE00F?
020600100408         setll  (*loval)  AZUTE000;
020700100408         read  AZUTE000;
020800100408
020900100408         DoW  Not %eof(AZUTE01L);
021000100408
021100100408           dUTE02 = UTEfa1;
021200100408           If  UTEdsc < D29dsc   and   §UT2mopr = *blank;
021300100408
021400100408             // -?Verifica sul file KPPRF00F delle condizioni idonee?
021500100408             //  ?alla cancellazione del profilo?
021600100408             chain  (UTEute)  KPPRF000;
021700100408
021800100408             if  %found(KPPRF01L)   and
021900100408                 %subst( PRFnsi : 1 : 6 ) = 'FILTRA';
022000100408
022100100408               // -?Cancellazione singolo profilo utente?
022200100408               exsr  sr_DltUsrPrf;
022300100408
022400100408             else;
022500100408
022600100408               unlock  KPPRF01L;
022700100408               unlock  AZUTE01L;
022800100408
022900100408             endif;
023000100408
023100100408           EndIf;
023200100408
023300100408           read  AZUTE000;
023400100408
023500100408         EndDo;
023600100408
023700100408       ENDSR;
023800100408
023900100408       //--------------------------------------------------------------
024000100408       //?Cancellazione/Stampa del singolo profilo utente.             ?
024100100408       //--------------------------------------------------------------
024200100408       BEGSR  sr_DltUsrPrf;
024300100408
024400100408         // -?Verifica esistenza profilo su AS/400?
024500100408         clear  o_Txt2;
024600100408         Qcmd = 'ChkObj obj(' + %trim(UTEute) + ') +
024700100408                        objtype(*usrprf)';
024800100408         exsr  sr_ExecCmd;
024900100408         if  Qusei <> *blank;
025000100408           o_Txt2 = 'Profilo inesistente su AS/400.';
025100100408         endif;
025200100408
025300100408         // -?Stampa profilo (se richiesta)?
025400100408         If  D29lst = 'S';
025500100408           exsr  sr_PrtUsrPrf;
025600100408         EndIf;
025700100408
025800100408         // -?Cancellazione profilo (se richiesta)?
025900100408         If  D29dlt = 'S';
026000100408
026100161010           // -?Cancellazione NOTE del profilo utente dal file AZUTN00F?
026200161010           setll  (UTEute)  AZUTN000;
026300161010           reade  (UTEute)  AZUTN000;
026400161010           DoW  not %eof(AZUTN01L);
026500161010             delete  AZUTN000;
026600161010             reade  (UTEute)  AZUTN000;
026700161010           EndDo;
026800161010
026900100408           // -?Cancellazione profilo utente dal file AZUTE00F?
027000100408           delete  AZUTE000;
027100100408
027200100408           // -?Cancellazione profilo utente dal file KPPRF00F?
027300100408           delete  KPPRF000;
027400100408
027500100408           if  o_Txt2 <> *blank;
027600100408             leavesr;
027700100408           endif;
027800100408
027900100408           // -?Cancellazione profilo utente di sistema?
028000100408           tibs29c ( UTEute : $Err );
028100100408
028200100408           If  $Err = *on;
028300100408             // -?Stampa segnalazione del profilo NON cancellato?
028400100408             if  D29lst = ' '   and   D29dlt = 'S';
028500100408               exsr  sr_PrtUsrPrf;
028600100408             else;
028700100416               if  o_Txt2 <> *blank;
028800100416                 except  PrtSkip;
028900100416               endif;
029000100408               except  PrtErr;
029100100408             endif;
029200100408           EndIf;
029300100408
029400100408         Else;
029500100408
029600100408           unlock  KPPRF01L;
029700100408           unlock  AZUTE01L;
029800100408
029900100408         EndIf;
030000100408
030100100408       ENDSR;
030200100408
030300100408       //--------------------------------------------------------------
030400100408       //?Stampa del singolo profilo utente.                           ?
030500100408       //--------------------------------------------------------------
030600100408       BEGSR  sr_PrtUsrPrf;
030700100408
030800100408         // -?Apertura file di stampa?
030900100408         if  Not %open(Qprint);
031000100408           open  Qprint;
031100100408           *inOF = *on;
031200100408           select;
031300100408             when  D29lst = 'S'   and   D29dlt = *blank;
031400100408               o_Txt1 = 'CANCELLABILI   ';
031500100408             when  D29lst = 'S'   and   D29dlt = 'S';
031600100408               o_Txt1 = 'CANCELLATI     ';
031700100408             when  D29lst = ' '   and   D29dlt = 'S';
031800100408               o_Txt1 = 'NON CANCELLATI ';
031900100408           endsl;
032000100408         endif;
032100100408
032200100408         // -?Salto pagina?
032300100408         if  *inOF;
032400100408           ds_Time   = %char( %dec( %timestamp() ) );
032500100408           wDate_Eur = %date( wDate : *iso );
032600100408           wDate     = %dec( wDate_Eur );
032700100408           wDate_Eur = %date( D29dsc : *iso );
032800100408           o_Dsc     = %dec( wDate_Eur );
032900100408           except  PrtTxt;
033000100408           *inOF = *off;
033100100408         endif;
033200100408
033300100408         // -?Stampa utente?
033400100416         if  UTEdsc > *zero;
033500100416           wDate_Eur = %date( UTEdsc : *iso );
033600100416           o_UTEdsc  = %dec( wDate_Eur );
033700100416         else;
033800100416           clear  o_UTEdsc;
033900100416         endif;
034000100408         except  PrtDet;
034100100408
034200100408       ENDSR;
034300100408
034400100408       //--------------------------------------------------------------
034500100408       //?Esecuzione del comando (già impostato).                      ?
034600100408       //--------------------------------------------------------------
034700100408       BEGSR  sr_ExecCmd;
034800100408
034900100408         clear Qcap0100;
035000100408         Qcabcsdh = *off;
035100100408         Qcapa    = *off;
035200100408         Qcacmdss = *off;
035300100408         Qcaerved = *allX'00';
035400100408
035500100408         clear Qusec;
035600100408         Qusbprv  = %size(Qusec);
035700100408
035800100408         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
035900100408                           %size(Qcap0100) : 'CPOP0100' : *omit :
036000100408                           0 : 0 : Qusec);
036100100408
036200100408         //if  Qusei <> *blank;
036300100408         //  ...;
036400100408         //endif;
036500100408
036600100408       ENDSR;
036700100408
036800100408       //--------------------------------------------------------------
036900100408       //?Operazioni finali.                                           ?
037000100408       //--------------------------------------------------------------
037100100408       BEGSR  sr_RoutEnd;
037200100408
037300100408         if  %open(Qprint);
037400100408           except  PrtEnd;
037500100408           close  Qprint;
037600100408         endif;
037700100408
037800100408         return;
037900100408
038000100408       ENDSR;
038100100407
038200100407      /end-free
038300100408
038400100408      //---------------------------------------------------------------
038500100408      //?Spool di stampa (per e-mail).                                 ?
038600100408      //---------------------------------------------------------------
038700100408
038800100408     oQPRINT    e            PrtTxt            2
038900100408     o                       RSut
039000100408     o                                        +   5 'LISTA PROFILI UTENTE'
039100100408     o                       o_Txt1           +   1
039200100408     o                       SDSpgm           +   3
039300100408     o                       kNmUs            +   5
039400100408     o                       wDate         y  +   5
039500100408     o                       wTime            +   3 '  :  :  '
039600100408     o                       kNSif            +   5
039700100408     o          e            PrtTxt      0
039800100408     o                                        +  25 'LISTA PROFILI UTENTE'
039900100408     o                       o_Txt1           +   1
040000100408     o          e            PrtTxt      1
040100100408     o                                        +  25 'CON DATA SCADENZA LIMITE ='
040200100408     o                       o_Dsc         y  +   1
040300100408     o          e            PrtTxt      0
040400100408     o                                        +  25 'CON DATA SCADENZA LIMITE ='
040500100408     o                       o_Dsc         y  +   1
040600100408     o          e            PrtTxt      2
040700100416     o                                              'Profilo utente            -
040800100416     o                                                                         -
040900100416     o                                                       '
041000100416     o                                        +   2 'Scadenza  '
041100100416     o*//                                     +   2 'Note                      -
041200100416     o*//                                               '
041300100408     o          e            PrtTxt      1  1
041400100416     o                                              '---------------------------
041500100408     o                                              ----------------------------
041600100416     o                                              ---------'
041700100416     o                                        +   2 '----------'
041800100416     o*//                                     +   2 '---------------------------
041900100416     o*//                                           ----'
042000100408     o          e            PrtDet      1
042100100408     o                       UTEute
042200100408     o                       PRFtxt           +   2
042300100416     o                       o_UTEdsc      y  +   2
042400100408     o                       o_Txt2           +   2
042500100416     o          e            PrtSkip     1
042600100408     o          e            PrtErr      0
042700100416     o                                        +  76 'Profilo utente NON cancell-
042800100416     o                                              ato.'
042900100416     o          e            PrtErr      0
043000100416     o                                        +  76 'Profilo utente NON cancell-
043100100416     o                                              ato.'
043200100408     o          e            PrtEnd      2
043300100408     o                                        +  25 '***  Fine Lista  ***'

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
002400100805     fAZUTE01L  Uf   e           k disk    commit
002500100408       // -?Profili Modulo Base Proj?
002600100805     fKPPRF01L  Uf   e           k disk    commit
002700100408
002800100408       // -?Stampa?
002900100408     fQPRINT    o    f  132        printer  oflind(*inOF)  usropn
003000100407
003100100407       //--------------------------------------------------------------
003200100407       //?Definizione costanti.                                        ?
003300100407       //--------------------------------------------------------------
003400100407
003500100407
003600100407       //--------------------------------------------------------------
003700100407       //?Definizione schiere.                                         ?
003800100407       //--------------------------------------------------------------
003900100407
004000100407
004100100407       //--------------------------------------------------------------
004200100407       //?Definizione aree dati.                                       ?
004300100407       //--------------------------------------------------------------
004400100407
004500100407       // -?Dati utente?
004600100407     d §AzUte        e ds                  extname(AZUTE00F)
004700100407     d                                     dtaara
004800100407     d §DatiUte      e ds                  extname(dDatiUte)
004900100407     d                                     dtaara
005000100407
005100100407       //--------------------------------------------------------------
005200100407       //?Definizione strutture dati.                                  ?
005300100407       //--------------------------------------------------------------
005400100407
005500100407       // -?Status ds?
005600100407     d Status         sds
005700100407     d  SDSpgm           *proc
005800100408
005900100408       // -?Parametri per Reperimento dati utente?
006000100408     d TIBS34ds      e ds                  inz
006100100408
006200100408       // -?(Altri) Flags utenti aziendali?
006300100408     d dUte02        e ds                  inz
006400100407
006500100407       // -?Parametri ricevuti?
006600100407     d KPJBA         e ds
006700100407     d TIBS29ds        ds                  inz
006800100407     d   D29dsc                       8  0 inz
006900100408     d   D29lst                       1    inz
007000100408     d   D29dlt                       1    inz
007100100408
007200100408       // -?Data e Ora correnti?
007300100408     d ds_Time         ds            20    inz
007400100408     d   wDate                        8s 0 inz
007500100408     d   wTime                        6s 0 inz
007600100408     d   wMmSs                        6s 0 inz
007700100407
007800100407       //--------------------------------------------------------------
007900100407       //?Definizione variabili globali.                               ?
008000100407       //--------------------------------------------------------------
008100100407
008200100407       // -?Flags booleani?
008300100408     d $Fine           s               n   inz
008400100408     d $Err            s               n   inz
008500100408
008600100408       // -?Campi di stampa?
008700100408     d o_Txt1          s             15    inz
008800100408     d o_Txt2          s             30    inz
008900100408     d o_Dsc           s                   inz  like(D29dsc)
009000100416     d o_UTEdsc        s                   inz  like(UTEdsc)
009100100408
009200100408       // -?Campi di comodo?
009300100408     d wDate_Eur       s               d   inz  datfmt(*eur)
009400100407
009500100407       //--------------------------------------------------------------
009600100408       //?Definizione procedure.                                       ?
009700100407       //--------------------------------------------------------------
009800100407
009900100407       // -?Reperimento dati utente?
010000100407      /copy gaitrasrc/srcProtoPR,TIBS34R
010100100408
010200100408       // -?Cancellazione profilo utente?
010300100408     d tibs29c         pr                  extpgm('TIBS29C')
010400100408     d   D29usrprf                         like(UTEute)
010500100408     d   D29esito                     1a
010600100805       // -?Cancellazione autorizzazioni proj e tabelle
010700100805     d tibs29ar        pr                  extpgm('TIBS29AR')
010800100805     d   D29usrprf                         like(UTEute)
010900100408
011000100408       // -?API QCAPCMD (Process Commands)?
011100100408     d Qcmd            s           2048    inz  varying
011200100408      /copy qSysInc/qRpgleSrc,QCAPCMD
011300100408      /copy gaitrasrc/srcProtoPR,QCAPCMD
011400100408
011500100408       // -?Parametri gestione errori API.?
011600100408      /copy qsysinc/qrpglesrc,QUSEC
011700100407
011800100407       //--------------------------------------------------------------
011900100407       //?Definizione key-list.                                        ?
012000100407       //--------------------------------------------------------------
012100100407
012200100407
012300100407       //--------------------------------------------------------------
012400100407       //?M A I N - L I N E                                            ?
012500100407       //--------------------------------------------------------------
012600100407
012700100407     c     *Entry        plist
012800100407     c                   parm                    KPJBA
012900100407
013000100407      /free
013100100407
013200100407       // -?Operazioni iniziali?
013300100407       exsr  sr_RoutInz;
013400100407
013500100408       // -?Elaborazione (cancellazione profili di massa)?
013600100408       exsr  sr_ElabUsrPrf;
013700100407
013800100407       // -?Operazioni finali?
013900100407       exsr sr_RoutEnd;
014000100407
014100100407       //--------------------------------------------------------------
014200100407       //?Operazioni iniziali.                                         ?
014300100407       //--------------------------------------------------------------
014400100407       BEGSR  sr_RoutInz;
014500100407
014600100407         *inLR = *on;
014700100408
014800100408         // -?Impostazione parametri?
014900100408         if  KPJBU <> *blank;
015000100408           // -?Ricevuti (dal filtro di lancio)?
015100100408           TIBS29ds = KPJBU;
015200100408         else;
015300100408           // -?Non ricevuti (lavoro schedulato)?
015400100408           clear  TIBS29ds;
015500100408           D29dsc = %dec( %date() );
015600100408           D29dlt = 'S';
015700100408         endif;
015800100407
015900100407         // -?Reperimento dati job?
016000100407         exsr  sr_DatiJob;
016100100517
016200100517         // -?Apertura file di stampa?
016300100517         If  D29lst = 'S'   and   Not %open(Qprint);
016400100517           open  Qprint;
016500100517           select;
016600100517             when  D29lst = 'S'   and   D29dlt = *blank;
016700100517               o_Txt1 = 'CANCELLABILI   ';
016800100517             when  D29lst = 'S'   and   D29dlt = 'S';
016900100517               o_Txt1 = 'CANCELLATI     ';
017000100517             when  D29lst = ' '   and   D29dlt = 'S';
017100100517               o_Txt1 = 'NON CANCELLATI ';
017200100517           endsl;
017300100517           ds_Time   = %char( %dec( %timestamp() ) );
017400100517           wDate_Eur = %date( wDate : *iso );
017500100517           wDate     = %dec( wDate_Eur );
017600100517           wDate_Eur = %date( D29dsc : *iso );
017700100517           o_Dsc     = %dec( wDate_Eur );
017800100517           except  PrtTxt;
017900100517           *inOF = *off;
018000100517         EndIf;
018100100407
018200100407       ENDSR;
018300100407
018400100407       //--------------------------------------------------------------
018500100407       //?Reperimento Dati del job (Utente/Operativi).                 ?
018600100407       //--------------------------------------------------------------
018700100407       BEGSR  sr_DatiJob;
018800100407
018900100407         in(e) §AzUte;
019000100407         if NOT %error;
019100100407           in(e) §DatiUte;
019200100407         endif;
019300100407         if %error or RSut = *blank;
019400100407           tibs34r ( tibs34ds );
019500100407           in §AzUte;
019600100407           in §DatiUte;
019700100407         endif;
019800100407
019900100407       ENDSR;
020000100407
020100100407       //--------------------------------------------------------------
020200100408       //?Cancellazione Profili di massa.                              ?
020300100407       //--------------------------------------------------------------
020400100408       BEGSR  sr_ElabUsrPrf;
020500100407
020600100408         // -?Ciclo di lettura file AZUTE00F?
020700100408         setll  (*loval)  AZUTE000;
020800100408         read  AZUTE000;
020900100408
021000100408         DoW  Not %eof(AZUTE01L);
021100100408
021200100408           dUTE02 = UTEfa1;
021300100408           If  UTEdsc < D29dsc   and   §UT2mopr = *blank;
021400100408
021500100408             // -?Verifica sul file KPPRF00F delle condizioni idonee?
021600100408             //  ?alla cancellazione del profilo?
021700100408             chain  (UTEute)  KPPRF000;
021800100408
021900100408             if  %found(KPPRF01L)   and
022000100408                 %subst( PRFnsi : 1 : 6 ) = 'FILTRA';
022100100408
022200100408               // -?Cancellazione singolo profilo utente?
022300100408               exsr  sr_DltUsrPrf;
022400100408
022500100408             else;
022600100408
022700100408               unlock  KPPRF01L;
022800100408               unlock  AZUTE01L;
022900100408
023000100408             endif;
023100100408
023200100408           EndIf;
023300100408
023400100408           read  AZUTE000;
023500100408
023600100408         EndDo;
023700100408
023800100408       ENDSR;
023900100408
024000100408       //--------------------------------------------------------------
024100100408       //?Cancellazione/Stampa del singolo profilo utente.             ?
024200100408       //--------------------------------------------------------------
024300100408       BEGSR  sr_DltUsrPrf;
024400100408
024500100408         // -?Verifica esistenza profilo su AS/400?
024600100408         clear  o_Txt2;
024700100408         Qcmd = 'ChkObj obj(' + %trim(UTEute) + ') +
024800100408                        objtype(*usrprf)';
024900100408         exsr  sr_ExecCmd;
025000100408         if  Qusei <> *blank;
025100100408           o_Txt2 = 'Profilo inesistente su AS/400.';
025200100408         endif;
025300100408
025400100408         // -?Stampa profilo (se richiesta)?
025500100408         If  D29lst = 'S';
025600100408           exsr  sr_PrtUsrPrf;
025700100408         EndIf;
025800100408
025900100408         // -?Cancellazione profilo (se richiesta)?
026000100408         If  D29dlt = 'S';
026100100408
026200100408           // -?Cancellazione profilo utente dal file AZUTE00F?
026300100408           delete  AZUTE000;
026400100408
026500100408           // -?Cancellazione profilo utente dal file KPPRF00F?
026600100408           delete  KPPRF000;
026700100805
026800100805           // -?Cancellazione archivi correlati proj e tabelle
026900100805           tibs29ar ( UTEute );
027000100805
027100100805           commit;
027200100408
027300100408           if  o_Txt2 <> *blank;
027400100408             leavesr;
027500100408           endif;
027600100408
027700100408           // -?Cancellazione profilo utente di sistema?
027800100408           tibs29c ( UTEute : $Err );
027900100408
028000100408           If  $Err = *on;
028100100408             // -?Stampa segnalazione del profilo NON cancellato?
028200100408             if  D29lst = ' '   and   D29dlt = 'S';
028300100408               exsr  sr_PrtUsrPrf;
028400100408             else;
028500100416               if  o_Txt2 <> *blank;
028600100416                 except  PrtSkip;
028700100416               endif;
028800100408               except  PrtErr;
028900100408             endif;
029000100408           EndIf;
029100100408
029200100408         Else;
029300100408
029400100408           unlock  KPPRF01L;
029500100408           unlock  AZUTE01L;
029600100408
029700100408         EndIf;
029800100408
029900100408       ENDSR;
030000100408
030100100408       //--------------------------------------------------------------
030200100408       //?Stampa del singolo profilo utente.                           ?
030300100408       //--------------------------------------------------------------
030400100408       BEGSR  sr_PrtUsrPrf;
030500100408
030600100408         // -?Apertura file di stampa?
030700100408         if  Not %open(Qprint);
030800100408           open  Qprint;
030900100408           *inOF = *on;
031000100408           select;
031100100408             when  D29lst = 'S'   and   D29dlt = *blank;
031200100408               o_Txt1 = 'CANCELLABILI   ';
031300100408             when  D29lst = 'S'   and   D29dlt = 'S';
031400100408               o_Txt1 = 'CANCELLATI     ';
031500100408             when  D29lst = ' '   and   D29dlt = 'S';
031600100408               o_Txt1 = 'NON CANCELLATI ';
031700100408           endsl;
031800100408         endif;
031900100408
032000100408         // -?Salto pagina?
032100100408         if  *inOF;
032200100408           ds_Time   = %char( %dec( %timestamp() ) );
032300100408           wDate_Eur = %date( wDate : *iso );
032400100408           wDate     = %dec( wDate_Eur );
032500100408           wDate_Eur = %date( D29dsc : *iso );
032600100408           o_Dsc     = %dec( wDate_Eur );
032700100408           except  PrtTxt;
032800100408           *inOF = *off;
032900100408         endif;
033000100408
033100100408         // -?Stampa utente?
033200100416         if  UTEdsc > *zero;
033300100416           wDate_Eur = %date( UTEdsc : *iso );
033400100416           o_UTEdsc  = %dec( wDate_Eur );
033500100416         else;
033600100416           clear  o_UTEdsc;
033700100416         endif;
033800100408         except  PrtDet;
033900100408
034000100408       ENDSR;
034100100408
034200100408       //--------------------------------------------------------------
034300100408       //?Esecuzione del comando (già impostato).                      ?
034400100408       //--------------------------------------------------------------
034500100408       BEGSR  sr_ExecCmd;
034600100408
034700100408         clear Qcap0100;
034800100408         Qcabcsdh = *off;
034900100408         Qcapa    = *off;
035000100408         Qcacmdss = *off;
035100100408         Qcaerved = *allX'00';
035200100408
035300100408         clear Qusec;
035400100408         Qusbprv  = %size(Qusec);
035500100408
035600100408         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
035700100408                           %size(Qcap0100) : 'CPOP0100' : *omit :
035800100408                           0 : 0 : Qusec);
035900100408
036000100408         //if  Qusei <> *blank;
036100100408         //  ...;
036200100408         //endif;
036300100408
036400100408       ENDSR;
036500100408
036600100408       //--------------------------------------------------------------
036700100408       //?Operazioni finali.                                           ?
036800100408       //--------------------------------------------------------------
036900100408       BEGSR  sr_RoutEnd;
037000100408
037100100408         if  %open(Qprint);
037200100408           except  PrtEnd;
037300100408           close  Qprint;
037400100408         endif;
037500100408
037600100408         return;
037700100408
037800100408       ENDSR;
037900100407
038000100407      /end-free
038100100408
038200100408      //---------------------------------------------------------------
038300100408      //?Spool di stampa (per e-mail).                                 ?
038400100408      //---------------------------------------------------------------
038500100408
038600100408     oQPRINT    e            PrtTxt            2
038700100408     o                       RSut
038800100408     o                                        +   5 'LISTA PROFILI UTENTE'
038900100408     o                       o_Txt1           +   1
039000100408     o                       SDSpgm           +   3
039100100408     o                       kNmUs            +   5
039200100408     o                       wDate         y  +   5
039300100408     o                       wTime            +   3 '  :  :  '
039400100408     o                       kNSif            +   5
039500100408     o          e            PrtTxt      0
039600100408     o                                        +  25 'LISTA PROFILI UTENTE'
039700100408     o                       o_Txt1           +   1
039800100408     o          e            PrtTxt      1
039900100408     o                                        +  25 'CON DATA SCADENZA LIMITE ='
040000100408     o                       o_Dsc         y  +   1
040100100408     o          e            PrtTxt      0
040200100408     o                                        +  25 'CON DATA SCADENZA LIMITE ='
040300100408     o                       o_Dsc         y  +   1
040400100408     o          e            PrtTxt      2
040500100416     o                                              'Profilo utente            -
040600100416     o                                                                         -
040700100416     o                                                       '
040800100416     o                                        +   2 'Scadenza  '
040900100416     o*//                                     +   2 'Note                      -
041000100416     o*//                                               '
041100100408     o          e            PrtTxt      1  1
041200100416     o                                              '---------------------------
041300100408     o                                              ----------------------------
041400100416     o                                              ---------'
041500100416     o                                        +   2 '----------'
041600100416     o*//                                     +   2 '---------------------------
041700100416     o*//                                           ----'
041800100408     o          e            PrtDet      1
041900100408     o                       UTEute
042000100408     o                       PRFtxt           +   2
042100100416     o                       o_UTEdsc      y  +   2
042200100408     o                       o_Txt2           +   2
042300100416     o          e            PrtSkip     1
042400100408     o          e            PrtErr      0
042500100416     o                                        +  76 'Profilo utente NON cancell-
042600100416     o                                              ato.'
042700100416     o          e            PrtErr      0
042800100416     o                                        +  76 'Profilo utente NON cancell-
042900100416     o                                              ato.'
043000100408     o          e            PrtEnd      2
043100100408     o                                        +  25 '***  Fine Lista  ***'

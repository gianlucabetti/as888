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
002500100408       // -?Profili Modulo Base Proj?
002600100407     fKPPRF01L  Uf   e           k disk
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
010600100408
010700100408       // -?API QCAPCMD (Process Commands)?
010800100408     d Qcmd            s           2048    inz  varying
010900100408      /copy qSysInc/qRpgleSrc,QCAPCMD
011000100408      /copy gaitrasrc/srcProtoPR,QCAPCMD
011100100408
011200100408       // -?Parametri gestione errori API.?
011300100408      /copy qsysinc/qrpglesrc,QUSEC
011400100407
011500100407       //--------------------------------------------------------------
011600100407       //?Definizione key-list.                                        ?
011700100407       //--------------------------------------------------------------
011800100407
011900100407
012000100407       //--------------------------------------------------------------
012100100407       //?M A I N - L I N E                                            ?
012200100407       //--------------------------------------------------------------
012300100407
012400100407     c     *Entry        plist
012500100407     c                   parm                    KPJBA
012600100407
012700100407      /free
012800100407
012900100407       // -?Operazioni iniziali?
013000100407       exsr  sr_RoutInz;
013100100407
013200100408       // -?Elaborazione (cancellazione profili di massa)?
013300100408       exsr  sr_ElabUsrPrf;
013400100407
013500100407       // -?Operazioni finali?
013600100407       exsr sr_RoutEnd;
013700100407
013800100407       //--------------------------------------------------------------
013900100407       //?Operazioni iniziali.                                         ?
014000100407       //--------------------------------------------------------------
014100100407       BEGSR  sr_RoutInz;
014200100407
014300100407         *inLR = *on;
014400100408
014500100408         // -?Impostazione parametri?
014600100408         if  KPJBU <> *blank;
014700100408           // -?Ricevuti (dal filtro di lancio)?
014800100408           TIBS29ds = KPJBU;
014900100408         else;
015000100408           // -?Non ricevuti (lavoro schedulato)?
015100100408           clear  TIBS29ds;
015200100408           D29dsc = %dec( %date() );
015300100408           D29dlt = 'S';
015400100408         endif;
015500100407
015600100407         // -?Reperimento dati job?
015700100407         exsr  sr_DatiJob;
015800100517
015900100517         // -?Apertura file di stampa?
016000100517         If  D29lst = 'S'   and   Not %open(Qprint);
016100100517           open  Qprint;
016200100517           select;
016300100517             when  D29lst = 'S'   and   D29dlt = *blank;
016400100517               o_Txt1 = 'CANCELLABILI   ';
016500100517             when  D29lst = 'S'   and   D29dlt = 'S';
016600100517               o_Txt1 = 'CANCELLATI     ';
016700100517             when  D29lst = ' '   and   D29dlt = 'S';
016800100517               o_Txt1 = 'NON CANCELLATI ';
016900100517           endsl;
017000100517           ds_Time   = %char( %dec( %timestamp() ) );
017100100517           wDate_Eur = %date( wDate : *iso );
017200100517           wDate     = %dec( wDate_Eur );
017300100517           wDate_Eur = %date( D29dsc : *iso );
017400100517           o_Dsc     = %dec( wDate_Eur );
017500100517           except  PrtTxt;
017600100517           *inOF = *off;
017700100517         EndIf;
017800100407
017900100407       ENDSR;
018000100407
018100100407       //--------------------------------------------------------------
018200100407       //?Reperimento Dati del job (Utente/Operativi).                 ?
018300100407       //--------------------------------------------------------------
018400100407       BEGSR  sr_DatiJob;
018500100407
018600100407         in(e) §AzUte;
018700100407         if NOT %error;
018800100407           in(e) §DatiUte;
018900100407         endif;
019000100407         if %error or RSut = *blank;
019100100407           tibs34r ( tibs34ds );
019200100407           in §AzUte;
019300100407           in §DatiUte;
019400100407         endif;
019500100407
019600100407       ENDSR;
019700100407
019800100407       //--------------------------------------------------------------
019900100408       //?Cancellazione Profili di massa.                              ?
020000100407       //--------------------------------------------------------------
020100100408       BEGSR  sr_ElabUsrPrf;
020200100407
020300100408         // -?Ciclo di lettura file AZUTE00F?
020400100408         setll  (*loval)  AZUTE000;
020500100408         read  AZUTE000;
020600100408
020700100408         DoW  Not %eof(AZUTE01L);
020800100408
020900100408           dUTE02 = UTEfa1;
021000100408           If  UTEdsc < D29dsc   and   §UT2mopr = *blank;
021100100408
021200100408             // -?Verifica sul file KPPRF00F delle condizioni idonee?
021300100408             //  ?alla cancellazione del profilo?
021400100408             chain  (UTEute)  KPPRF000;
021500100408
021600100408             if  %found(KPPRF01L)   and
021700100408                 %subst( PRFnsi : 1 : 6 ) = 'FILTRA';
021800100408
021900100408               // -?Cancellazione singolo profilo utente?
022000100408               exsr  sr_DltUsrPrf;
022100100408
022200100408             else;
022300100408
022400100408               unlock  KPPRF01L;
022500100408               unlock  AZUTE01L;
022600100408
022700100408             endif;
022800100408
022900100408           EndIf;
023000100408
023100100408           read  AZUTE000;
023200100408
023300100408         EndDo;
023400100408
023500100408       ENDSR;
023600100408
023700100408       //--------------------------------------------------------------
023800100408       //?Cancellazione/Stampa del singolo profilo utente.             ?
023900100408       //--------------------------------------------------------------
024000100408       BEGSR  sr_DltUsrPrf;
024100100408
024200100408         // -?Verifica esistenza profilo su AS/400?
024300100408         clear  o_Txt2;
024400100408         Qcmd = 'ChkObj obj(' + %trim(UTEute) + ') +
024500100408                        objtype(*usrprf)';
024600100408         exsr  sr_ExecCmd;
024700100408         if  Qusei <> *blank;
024800100408           o_Txt2 = 'Profilo inesistente su AS/400.';
024900100408         endif;
025000100408
025100100408         // -?Stampa profilo (se richiesta)?
025200100408         If  D29lst = 'S';
025300100408           exsr  sr_PrtUsrPrf;
025400100408         EndIf;
025500100408
025600100408         // -?Cancellazione profilo (se richiesta)?
025700100408         If  D29dlt = 'S';
025800100408
025900100408           // -?Cancellazione profilo utente dal file AZUTE00F?
026000100408           delete  AZUTE000;
026100100408
026200100408           // -?Cancellazione profilo utente dal file KPPRF00F?
026300100408           delete  KPPRF000;
026400100408
026500100408           if  o_Txt2 <> *blank;
026600100408             leavesr;
026700100408           endif;
026800100408
026900100408           // -?Cancellazione profilo utente di sistema?
027000100408           tibs29c ( UTEute : $Err );
027100100408
027200100408           If  $Err = *on;
027300100408             // -?Stampa segnalazione del profilo NON cancellato?
027400100408             if  D29lst = ' '   and   D29dlt = 'S';
027500100408               exsr  sr_PrtUsrPrf;
027600100408             else;
027700100416               if  o_Txt2 <> *blank;
027800100416                 except  PrtSkip;
027900100416               endif;
028000100408               except  PrtErr;
028100100408             endif;
028200100408           EndIf;
028300100408
028400100408         Else;
028500100408
028600100408           unlock  KPPRF01L;
028700100408           unlock  AZUTE01L;
028800100408
028900100408         EndIf;
029000100408
029100100408       ENDSR;
029200100408
029300100408       //--------------------------------------------------------------
029400100408       //?Stampa del singolo profilo utente.                           ?
029500100408       //--------------------------------------------------------------
029600100408       BEGSR  sr_PrtUsrPrf;
029700100408
029800100408         // -?Apertura file di stampa?
029900100408         if  Not %open(Qprint);
030000100408           open  Qprint;
030100100408           *inOF = *on;
030200100408           select;
030300100408             when  D29lst = 'S'   and   D29dlt = *blank;
030400100408               o_Txt1 = 'CANCELLABILI   ';
030500100408             when  D29lst = 'S'   and   D29dlt = 'S';
030600100408               o_Txt1 = 'CANCELLATI     ';
030700100408             when  D29lst = ' '   and   D29dlt = 'S';
030800100408               o_Txt1 = 'NON CANCELLATI ';
030900100408           endsl;
031000100408         endif;
031100100408
031200100408         // -?Salto pagina?
031300100408         if  *inOF;
031400100408           ds_Time   = %char( %dec( %timestamp() ) );
031500100408           wDate_Eur = %date( wDate : *iso );
031600100408           wDate     = %dec( wDate_Eur );
031700100408           wDate_Eur = %date( D29dsc : *iso );
031800100408           o_Dsc     = %dec( wDate_Eur );
031900100408           except  PrtTxt;
032000100408           *inOF = *off;
032100100408         endif;
032200100408
032300100408         // -?Stampa utente?
032400100416         if  UTEdsc > *zero;
032500100416           wDate_Eur = %date( UTEdsc : *iso );
032600100416           o_UTEdsc  = %dec( wDate_Eur );
032700100416         else;
032800100416           clear  o_UTEdsc;
032900100416         endif;
033000100408         except  PrtDet;
033100100408
033200100408       ENDSR;
033300100408
033400100408       //--------------------------------------------------------------
033500100408       //?Esecuzione del comando (già impostato).                      ?
033600100408       //--------------------------------------------------------------
033700100408       BEGSR  sr_ExecCmd;
033800100408
033900100408         clear Qcap0100;
034000100408         Qcabcsdh = *off;
034100100408         Qcapa    = *off;
034200100408         Qcacmdss = *off;
034300100408         Qcaerved = *allX'00';
034400100408
034500100408         clear Qusec;
034600100408         Qusbprv  = %size(Qusec);
034700100408
034800100408         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
034900100408                           %size(Qcap0100) : 'CPOP0100' : *omit :
035000100408                           0 : 0 : Qusec);
035100100408
035200100408         //if  Qusei <> *blank;
035300100408         //  ...;
035400100408         //endif;
035500100408
035600100408       ENDSR;
035700100408
035800100408       //--------------------------------------------------------------
035900100408       //?Operazioni finali.                                           ?
036000100408       //--------------------------------------------------------------
036100100408       BEGSR  sr_RoutEnd;
036200100408
036300100408         if  %open(Qprint);
036400100408           except  PrtEnd;
036500100408           close  Qprint;
036600100408         endif;
036700100408
036800100408         return;
036900100408
037000100408       ENDSR;
037100100407
037200100407      /end-free
037300100408
037400100408      //---------------------------------------------------------------
037500100408      //?Spool di stampa (per e-mail).                                 ?
037600100408      //---------------------------------------------------------------
037700100408
037800100408     oQPRINT    e            PrtTxt            2
037900100408     o                       RSut
038000100408     o                                        +   5 'LISTA PROFILI UTENTE'
038100100408     o                       o_Txt1           +   1
038200100408     o                       SDSpgm           +   3
038300100408     o                       kNmUs            +   5
038400100408     o                       wDate         y  +   5
038500100408     o                       wTime            +   3 '  :  :  '
038600100408     o                       kNSif            +   5
038700100408     o          e            PrtTxt      0
038800100408     o                                        +  25 'LISTA PROFILI UTENTE'
038900100408     o                       o_Txt1           +   1
039000100408     o          e            PrtTxt      1
039100100408     o                                        +  25 'CON DATA SCADENZA LIMITE ='
039200100408     o                       o_Dsc         y  +   1
039300100408     o          e            PrtTxt      0
039400100408     o                                        +  25 'CON DATA SCADENZA LIMITE ='
039500100408     o                       o_Dsc         y  +   1
039600100408     o          e            PrtTxt      2
039700100416     o                                              'Profilo utente            -
039800100416     o                                                                         -
039900100416     o                                                       '
040000100416     o                                        +   2 'Scadenza  '
040100100416     o*//                                     +   2 'Note                      -
040200100416     o*//                                               '
040300100408     o          e            PrtTxt      1  1
040400100416     o                                              '---------------------------
040500100408     o                                              ----------------------------
040600100416     o                                              ---------'
040700100416     o                                        +   2 '----------'
040800100416     o*//                                     +   2 '---------------------------
040900100416     o*//                                           ----'
041000100408     o          e            PrtDet      1
041100100408     o                       UTEute
041200100408     o                       PRFtxt           +   2
041300100416     o                       o_UTEdsc      y  +   2
041400100408     o                       o_Txt2           +   2
041500100416     o          e            PrtSkip     1
041600100408     o          e            PrtErr      0
041700100416     o                                        +  76 'Profilo utente NON cancell-
041800100416     o                                              ato.'
041900100416     o          e            PrtErr      0
042000100416     o                                        +  76 'Profilo utente NON cancell-
042100100416     o                                              ato.'
042200100408     o          e            PrtEnd      2
042300100408     o                                        +  25 '***  Fine Lista  ***'

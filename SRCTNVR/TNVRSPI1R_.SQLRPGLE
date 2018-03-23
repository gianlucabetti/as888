000100170711       //==============================================================
000200171212       //? TNVRSPI1R // Aggiornamento *file ANSPI00F con dati scaricati ?
000300171212       //?              dal sito www.finanze.it (in formato .XLS) e     ?
000400171212       //?              portati nel work-file WSPLIT0F.                 ?
000500170711       //==============================================================
000600170711
000700170711       //--------------------------------------------------------------
000800171212       //? Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000900170711       //--------------------------------------------------------------
001000170711
001100170711     /*PRM  dbgview(*source)
001200170711     /*END
001300170711
001400170711       //--------------------------------------------------------------
001500171212       //? Specifiche di controllo.                                     ?
001600170711       //--------------------------------------------------------------
001700170711
001800170711     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001900170711     h dftactgrp(*no)
002000170711     h alwnull(*inputonly)
002100170711
002200170711       //--------------------------------------------------------------
002300171212       //? Dichiarazione file.                                          ?
002400170711       //--------------------------------------------------------------
002500170711
002600171212       // - ?Soggetti Split Payment?
002700170711     fANSPI01L  Uf A e           k disk
002800170711
002900171212       // - ?Lista Errori rilevati?
003000170711     fQSYSPRT   o    f  132        printer usropn
003100170711     f                                     oflind(*inOF)
003200170711
003300170711       //--------------------------------------------------------------
003400171212       //? Definizione costanti.                                        ?
003500170711       //--------------------------------------------------------------
003600170711
003700171212       // - ?Data scadenza massima (numerica)?
003800170711     d c_DataMax       c                   const(20391231)
003900170830
004000171212       // - ?Comandi di override al PrtF QSYSPRT?
004100170830     d c_CmdOvrPrtF    c                   const('OVRPRTF +
004200170830     d                                           file(QSYSPRT) +
004300170830     d                                           usrdta(''PI_scadute'') +
004400170830     d                                           ovrscope(*actgrpdfn) +
004500170830     d                                           ')
004600170830     d c_CmdDltOvr     c                   const('DLTOVR +
004700170830     d                                            file(QSYSPRT) +
004800170830     d                                            lvl(*actgrpdfn)')
004900170711
005000170711       //--------------------------------------------------------------
005100170711       //?Definizione schiere.                                         ?
005200170711       //--------------------------------------------------------------
005300170711
005400170711
005500170711       //--------------------------------------------------------------
005600171212       //? Definizione aree dati.                                       ?
005700170711       //--------------------------------------------------------------
005800170711
005900170711
006000170711       //--------------------------------------------------------------
006100171212       //? Definizione strutture dati.                                  ?
006200170711       //--------------------------------------------------------------
006300170711
006400171212       // - ?Status ds?
006500170711     d Status         sds
006600170711     d   SDSpgm          *proc
006700170711     d*//SDSprm          *parms
006800170711     d*//SDSdta              191    198
006900170711     d*//SDSjob              244    253
007000170711     d   SDSusr              254    263
007100170830
007200171212       // - ?Parametri ricevuti?
007300170830     d KPJBA         e ds
007400170830     d TNVRSPI1ds    e ds                  qualified
007500170711
007600171212       // - ?Data/Ora attuali?
007700170711     d wTime_ds        ds                  inz
007800170711     d   wDate                        8s 0 inz
007900170711     d   wTime                        6s 0 inz
008000170711     d   wTimeHM               9     12s 0 inz
008100170711
008200170711       //--------------------------------------------------------------
008300171212       //? Definizione variabili globali.                               ?
008400170711       //--------------------------------------------------------------
008500170711
008600171212       // - ?Altri parametri (forse) ricevuti?
008700170711     d prm_FileName    s             21
008800170711
008900171212       // - ?Nome esteso Libreria/File del file con i nuovi dati?
009000171218     d wLibFile        s             21a   inz('WSPLIT0F             ')
009100170711
009200171212       // - ?Flags booleani?
009300170711     d $Fine           s               n   inz
009400170711
009500171212       // - ?Indici di schiera?
009600170711     d xx              s              3  0 inz
009700170711
009800171212       // - ?Stringa SQL da eseguire?
009900170711     d wSql            s          32740    inz    varying
010000170711
010100171212       // - ?Dati estratti via SQL?
010200170711     d wPIVA           s                   inz    like( SPIiva )
010300170711
010400171212       // - ?Campi di stampa?
010500170711     d prt_IVA         s                   inz    like( SPIiva )
010600170711     d prt_DSC         s                   inz    like( SPIdsc )
010700170711     d prt_DMO         s                   inz    like( SPIdmo )
010800170711     d prt_PMO         s                   inz    like( SPIpmo )
010900170711
011000171212       // - ?Campi di comodo?
011100171212       // · ?Data *ISO?
011200170711     d wDate_Iso       s               d   datfmt(*ISO)    inz(*loval)
011300171212       // · ?Data *EUR?
011400170711     d wDate_Eur       s               d   datfmt(*EUR)    inz(*loval)
011500171212       // · ?Data in stampa?
011600170711     d wDate_Gma       s              8  0 inz
011700170711
011800170711       //--------------------------------------------------------------
011900171212       //? Definizione prototipi procedure usate.                       ?
012000170711       //--------------------------------------------------------------
012100170711
012200171212       // - ?API QCAPCMD (Process Commands)?
012300170711     d Qcmd            s           2048    inz   varying
012400170711      /copy qSysInc/qRpgleSrc,QCAPCMD
012500170711      /copy gaitrasrc/srcProtoPR,QCAPCMD
012600171212       // - ?Parametri gestione errori API.?
012700170711      /copy qsysinc/qRpgleSrc,QUSEC
012800170711
012900170711       //--------------------------------------------------------------
013000171212       //? Definizione key-list.                                        ?
013100170711       //--------------------------------------------------------------
013200170711
013300171212       // - ?File ANSPI01L?
013400170711     d keyANSPI01    e ds                  extname( ANSPI01L : *key )
013500170711     d                                     prefix( k_ )  inz
013600170711
013700170711       //--------------------------------------------------------------
013800171212       //? Riepilogo indicatori utilizzati.                             ?
013900170711       //--------------------------------------------------------------
014000170711       //--------------------------------------------------------------
014100170711
014200170711       //--------------------------------------------------------------
014300171212       //? M A I N - L I N E                                            ?
014400170711       //--------------------------------------------------------------
014500170711
014600170711     c     *Entry        plist
014700170830     c                   parm                    KPJBA
014800170711     c                   parm                    prm_FileName
014900170711
015000171212       // - ?Operazioni iniziali?
015100170711       exsr  sr_RoutInz;
015200170711
015300171212       // - ?Operazioni iniziali?
015400171212       If  tnvrSPI1ds.VRSP1aoi = 'A';
015500171212         exsr  sr_Aggiornamento;
015600171212       Else;
015700171212         exsr  sr_Inserimento;
015800171212       EndIf;
015900170711
016000171212       // - ?Operazioni finali?
016100170711       exsr  sr_RoutEnd;
016200170711
016300170711       //--------------------------------------------------------------
016400171212       //? Operazioni iniziali.                                         ?
016500170711       //--------------------------------------------------------------
016600170711       BEGSR  sr_RoutInz;
016700170711
016800170711         exec sql   set option   DynUsrPrf = *Owner,
016900170711                                 CloSqlCsr = *EndMod;
017000170711
017100171212         // - ?Impostazione chiusura?
017200170711         *inLR = *on;
017300170711
017400171212         // - ?Reperimento data e ora attuali?
017500170711         wTime_ds  = %editc( %dec( %timestamp() ) : 'X' );
017600170711         wDate_Iso = %date( wDate : *Iso );
017700170711         wDate_Eur = wDate_Iso;
017800170711         wDate_Gma = %dec( wDate_Eur );
017900170830
018000171212         // - ?Reperimento parametri?
018100170830         TNVRSPI1ds = kpjbu;
018200170711
018300171212         // - ?Impostaz. nome file EVENTUALMENTE ricevuto come 2° parametro?
018400170711         reset  wLibFile;
018500170830         if  %parms()               > 1       and
018600170711             %trim( prm_FileName ) <> *blank;
018700170711           wLibFile = %trim( prm_FileName );
018800170711         endif;
018900170711
019000170711       ENDSR;
019100171212
019200171212       //--------------------------------------------------------------
019300171212       //? Richiesto Aggiornamento.                                     ?
019400171212       //--------------------------------------------------------------
019500171212       BEGSR  sr_Aggiornamento;
019600171212
019700171212         // - ?1) SCRITTURA (in aggiunta) ........ dati in ANSPI00F?
019800171212         // - ?2) SEGNALAZIONE (in stampa) .... scaduti in ANSPI00F?
019900171212         // - ?3) ANNULLAMENTO (con data scadenza) dati in ANSPI00F?
020000171212         For  xx = 1  To 3;
020100171212
020200171212           // - ?Preparazione stringa SQL?
020300171212           exsr  sr_PrepSQL;
020400171212
020500171212           // - ?Apertura del cursore?
020600171212           exsr  sr_OpenCursor;
020700171212
020800171212           // - ?Lettura del file?
020900171212           DoW  Not $Fine;
021000171212             exsr  sr_ReadCursor;
021100171212           EndDo;
021200171212
021300171212           // - ?Chiusura del cursore?
021400171212           exsr  sr_CloseCursor;
021500171212
021600171212         EndFor;
021700171212
021800171212       ENDSR;
021900171212
022000171212       //--------------------------------------------------------------
022100171212       //? Richiesto Inserimento di massa.                              ?
022200171212       //--------------------------------------------------------------
022300171212       BEGSR  sr_Inserimento;
022400171212
022500171212         // - ?4) ANNULLAMENTO (con data scadenza) dati in ANSPI00F?
022600171218         //      ?(soggetti NON più presenti nel nuovo work-file WSPLIT0F)?
022700171218         // - ?5) ANNULLAMENTO (con data scadenza) . dati in ANSPI00F?
022800171218         //      ?(tutti i soggetti, con l'anno di competenza < TNVRSPI1DS.VRSP1dssa)?
022900171218         // - ?6) SCRITTURA (in aggiunta) ........ dati in ANSPI00F?
023000171218         //      ?(tutti i soggetti presenti nel nuovo work-file WSPLIT0F)?
023100171218         For  xx = 4  To 6;
023200171212
023300171212           // - ?Preparazione stringa SQL?
023400171212           exsr  sr_PrepSQL;
023500171212
023600171212           // - ?Apertura del cursore?
023700171212           exsr  sr_OpenCursor;
023800171212
023900171212           // - ?Lettura del file?
024000171212           DoW  Not $Fine;
024100171212             exsr  sr_ReadCursor;
024200171212           EndDo;
024300171212
024400171212           // - ?Chiusura del cursore?
024500171212           exsr  sr_CloseCursor;
024600171212
024700171212         EndFor;
024800171212
024900171212       ENDSR;
025000170711
025100170711       //--------------------------------------------------------------
025200171212       //? Preparazione stringa SQL per estrazione dati da WSPLIT0F.    ?
025300170711       //--------------------------------------------------------------
025400170711       BEGSR  sr_PrepSQL;
025500170711
025600170711         clear  wSQL;
025700170711         clear  $Fine;
025800170711
025900170711         Select;
026000170711
026100171218           // - ?1) AGGIORNAMENTO:?
026200171218           //      ?Per SCRITTURA (in aggiunta) .......... dati in ANSPI00F?
026300170711           When  xx = 1;
026400170711
026500170712             wSQL =  'select distinct w.SPIiva +
026600170711
026700170711                        from ' + %trim( wLibFile ) + ' w +
026800170711                             exception join ANSPI00F a +
026900170711
027000170711                          on w.SPIiva = a.SPIiva +
027100171212                         and a.SPIaco = ' +
027200171212                             %trim( %editc( tnvrSPI1ds.VRSP1aco : 'X' ) ) +
027300170711
027400171212                     ' order by w.SPIiva +
027500170711
027600170711                         for fetch only';
027700170711
027800171218           // - ?2) AGGIORNAMENTO:?
027900171218           //      ?Per STAMPA delle P.I. che risultano scadute in ANSPI00F?
028000171218           //      ?(ma ancora presenti nel nuovo work-file WSPLIT0F)?
028100170711           When  xx = 2;
028200170711
028300170712             wSQL =  'select distinct a.SPIiva +
028400170711
028500170711                        from ANSPI00F a inner join ' +
028600170711                             %trim( wLibFile ) + ' w +
028700170711
028800170711                          on a.SPIiva = w.SPIiva +
028900171212                         and a.SPIaco = ' +
029000171212                             %trim( %editc( tnvrSPI1ds.VRSP1aco : 'X' ) ) +
029100170711
029200171212                     ' where a.SPIdsc < ' +
029300171212                             %trim( %editc( c_DataMax : 'X' ) ) +
029400170711
029500170711                     ' order by a.SPIiva +
029600170711
029700170711                         for fetch only';
029800170711
029900171218           // - ?3) AGGIORNAMENTO:?
030000171218           //      ?Per ANNULLAMENTO (con data scadenza) . dati in ANSPI00F?
030100171218           //      ?(soggetti NON più presenti nel nuovo work-file WSPLIT0F)?
030200170711           When  xx = 3;
030300170711
030400170712             wSQL =  'select distinct a.SPIiva +
030500170711
030600170711                        from ANSPI00F a exception join ' +
030700170711                             %trim( wLibFile ) + ' w +
030800170711
030900170711                          on a.SPIiva = w.SPIiva +
031000171212                         and a.SPIaco = ' +
031100171212                             %trim( %editc( tnvrSPI1ds.VRSP1aco : 'X' ) ) +
031200170711
031300171212                     ' where a.SPIdsc = ' +
031400171212                             %trim( %editc( c_DataMax : 'X' ) ) +
031500170711
031600170711                     ' order by a.SPIiva +
031700170711
031800170711                         for fetch only';
031900171218
032000171218           // - ?4) INSERIMENTO:?
032100171218           //      ?Per ANNULLAMENTO (con data scadenza) . dati in ANSPI00F?
032200171218           //      ?(soggetti NON più presenti nel nuovo work-file WSPLIT0F)?
032300171218           When  xx = 4;
032400171218
032500171218             wSQL =  'select distinct a.SPIiva +
032600171218
032700171218                        from ANSPI00F a exception join ' +
032800171218                             %trim( wLibFile ) + ' w +
032900171218
033000171218                          on a.SPIiva = w.SPIiva +
033100171218                         and a.SPIaco < ' +
033200171218                             %trim( %editc( tnvrSPI1ds.VRSP1aco : 'X' ) ) +
033300171218
033400171218                     ' where a.SPIdsc >= ' +
033500171218                             %trim( %editc( TNVRSPI1ds.VRSP1dde : 'X' ) ) +
033600171218
033700171218                     ' order by a.SPIiva +
033800171218
033900171218                         for fetch only';
034000170711
034100171218           // - ?5) INSERIMENTO:?
034200171218           //      ?Per ANNULLAMENTO (con data scadenza) . dati in ANSPI00F?
034300171218           //      ?(tutti i soggetti, con l'anno di competenza < TNVRSPI1DS.VRSP1dssa)?
034400171218           When  xx = 5;
034500171212
034600171218             wSQL =  'select distinct a.SPIiva +
034700171218
034800171218                        from ANSPI00F a inner join ' +
034900171218                             %trim( wLibFile ) + ' w +
035000171218
035100171218                          on a.SPIiva = w.SPIiva +
035200171218                         and a.SPIaco < ' +
035300171218                             %trim( %editc( tnvrSPI1ds.VRSP1aco : 'X' ) ) +
035400171218
035500171218                     ' where a.SPIdsc >= ' +
035600171218                             %trim( %editc( TNVRSPI1ds.VRSP1dde : 'X' ) ) +
035700171218
035800171218                     ' order by a.SPIiva +
035900171218
036000171218                         for fetch only';
036100171212
036200171218           // - ?6) INSERIMENTO:?
036300171218           //      ?Per SCRITTURA (in aggiunta) .......... dati in ANSPI00F?
036400171218           //      ?(tutti i soggetti presenti nel nuovo work-file WSPLIT0F)?
036500171218           When  xx = 6;
036600171212
036700171212             wSQL =  'select distinct w.SPIiva +
036800171212
036900171212                        from ' + %trim( wLibFile ) + ' w +
037000171218             //*-                exception join ANSPI00F a +
037100171218             //*-
037200171218             //*-             on w.SPIiva = a.SPIiva +
037300171218             //*-            and a.SPIaco = ' +
037400171218             //*-                %trim( %editc( tnvrSPI1ds.VRSP1aco : 'X' ) ) +
037500171218             //*-          ' and a.SPIdsc = ' +
037600171218             //*-                %trim( %editc( c_DataMax : 'X' ) ) + ' +
037700171212
037800171218                       order by w.SPIiva +
037900171212
038000171212                         for fetch only';
038100171212
038200171212           // - ?? ? ? ? ??
038300170711           Other;
038400170711
038500170711             $Fine = *on;
038600170711
038700170711         EndSl;
038800170711
038900170711       ENDSR;
039000170711
039100170711       //--------------------------------------------------------------
039200171212       //? Apertura cursore.                                            ?
039300170711       //--------------------------------------------------------------
039400170711       BEGSR  sr_OpenCursor;
039500170711
039600171212         // - ?Dichiarazione cursore?
039700170711         exec sql   prepare S1   from :wSQL;
039800170711         exec sql   declare C1   cursor for S1;
039900170711
040000170711         if  SQLcode < *zero;
040100170711           $Fine = *on;
040200170711           exsr  sr_PrintERR;
040300170711         endif;
040400170711
040500171212         // - ?Apertura del cursore?
040600170711         exec sql   open C1;
040700170711
040800170711         if  SQLcode < *zero;
040900170711           $Fine = *on;
041000170711           exsr  sr_PrintERR;
041100170711         endif;
041200170711
041300170711       ENDSR;
041400170711
041500170711       //--------------------------------------------------------------
041600171212       //? Lettura cursore.                                             ?
041700170711       //--------------------------------------------------------------
041800170711       BEGSR  sr_ReadCursor;
041900170711
042000170711         $Fine = *off;
042100170711         clear  wPIVA;
042200170711
042300170711         exec sql   fetch next   from C1   into :wPIVA;
042400170711
042500170711         select;
042600170711           when  SQLcode = 100;
042700170711             $Fine = *on;
042800170711           when  SQLcode < *zero;
042900170711             $Fine = *on;
043000170711             exsr  sr_PrintERR;
043100170711           other;
043200170711             select;
043300170711               when  xx = 1;
043400170711                 exsr  sr_WriteANSPI;
043500170711               when  xx = 2;
043600170711                 exsr  sr_PrintANSPI;
043700170711               when  xx = 3;
043800170711                 exsr  sr_AnnullANSPI;
043900171218               when  xx = 4;
044000171218                 exsr  sr_AnnullANSPI;
044100171218               when  xx = 5;
044200171218                 exsr  sr_AnnullANSPI;
044300171218               when  xx = 6;
044400171212                 exsr  sr_WriteANSPI;
044500170711             endsl;
044600170711         endsl;
044700170711
044800170711       ENDSR;
044900170711
045000170711       //--------------------------------------------------------------
045100171212       //? Chiusura cursore.                                            ?
045200170711       //--------------------------------------------------------------
045300170711       BEGSR  sr_CloseCursor;
045400170711
045500171212         // - ?Chiusura del cursore?
045600170711         exec sql   close C1;
045700170711
045800170711       ENDSR;
045900170711
046000170711       //--------------------------------------------------------------
046100171212       //? Scrittura record in ANSPI00F.                                ?
046200170711       //--------------------------------------------------------------
046300170711       BEGSR  sr_WriteANSPI;
046400170711
046500170711         clear  ANSPI000;
046600170711
046700170711         SPIiva  = wPIVA;
046800170830         SPIdde  = TNVRSPI1ds.VRSP1dde;
046900171218         SPIdsc  = c_DataMax;
047000170711
047100170711         SPIdim  = wDate;
047200170711         SPIhim  = wTimeHM;
047300170711         SPIpim  = 'BATCH     ';
047400171218         SPIaco  = TNVRSPI1ds.VRSP1aco;
047500170711
047600170711         //______________
047700170711         write  ANSPI000;
047800170711         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
047900170711
048000170711       ENDSR;
048100170711
048200170711       //--------------------------------------------------------------
048300171212       //? Annullamento record in ANSPI00F.                             ?
048400170711       //--------------------------------------------------------------
048500170711       BEGSR  sr_AnnullANSPI;
048600170711
048700170711         chain  ( wPIVA )  ANSPI000;
048800170711
048900170711         if  NOT %found( ANSPI01L );
049000170711           leavesr;
049100170711         endif;
049200170711
049300170830         //*·SPIdsc  = wDate;
049400171218         if  xx < 5;
049500171218           SPIdsc  = TNVRSPI1ds.VRSP1dsc;
049600171218         else;
049700171218           SPIdsc  = TNVRSPI1ds.VRSP1dssa;
049800171218         endif;
049900170711
050000170711         SPIdmo  = wDate;
050100170711         SPIhmo  = wTimeHM;
050200170711         SPIpmo  = 'BATCH     ';
050300170711
050400170711         //_______________
050500170711         update  ANSPI000;
050600170711         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
050700170711
050800170711       ENDSR;
050900170711
051000170711       //--------------------------------------------------------------
051100171212       //? Stampa P.I. che risulta scaduta in ANSPI00F.                 ?
051200170711       //--------------------------------------------------------------
051300170711       BEGSR  sr_PrintANSPI;
051400170711
051500170711         chain(N)  ( wPIVA )  ANSPI000;
051600170711
051700170711         if  NOT %found( ANSPI01L );
051800170711           leavesr;
051900170711         endif;
052000170711
052100170711         if  Not  %open(QSYSPRT);
052200170830           Qcmd = c_CmdOvrPrtF;
052300170830           exsr  sr_ExecCmd;
052400170711           open  QSYSPRT;
052500170711           *inOF = *on;
052600170711         endif;
052700170711
052800170711         prt_IVA = SPIiva;
052900170711
053000170711         wDate_Iso = %date( SPIdsc : *Iso );
053100170711         wDate_Eur = wDate_Iso;
053200170711         prt_DSC   = %dec( wDate_Eur );
053300170711
053400170711         clear  wDate_Iso;
053500170711         clear  wDate_Eur;
053600170711         clear  prt_DMO;
053700170711         if  SPIdmo > *zero;
053800170711           wDate_Iso = %date( SPIdmo : *Iso );
053900170711           wDate_Eur = wDate_Iso;
054000170711           prt_DMO   = %dec( wDate_Eur );
054100170711         endif;
054200170711
054300170711         prt_PMO   = SPIpmo;
054400170711
054500170711         if  *inOF;
054600170711           except  PRTtxt;
054700170711           *inOF = *off;
054800170711         endif;
054900170711
055000170711         except  PRTdet;
055100170711
055200170711       ENDSR;
055300170711
055400170711       //--------------------------------------------------------------
055500171212       //? Stampa segnalazione dell'errore rilevato via SQL             ?
055600170711       //--------------------------------------------------------------
055700170711       BEGSR  sr_PrintERR;
055800170711
055900171212         // - ?Stampa del Dump?
056000170711         Dump(A);
056100170711
056200171212         // - ?Stampa del Job-Log?
056300170711         Qcmd = 'DSPJOBLOG job(*) output(*print)';
056400170711         exsr  sr_ExecCmd;
056500170711
056600171212         // - ?Uscita dal *pgm?
056700170711         exsr  sr_RoutEnd;
056800170711
056900170711       ENDSR;
057000170711
057100170711       //--------------------------------------------------------------
057200171212       //? Operazioni finali.                                           ?
057300170711       //--------------------------------------------------------------
057400170711       BEGSR  sr_RoutEnd;
057500170711
057600171212         // - ?SE stampato almeno un Errore: stampa "fine"?
057700170711         if  %open(QSYSPRT);
057800170711           except  PRTend;
057900170711           close   QSYSPRT;
058000170830           Qcmd = c_CmdDltOvr;
058100170830           exsr  sr_ExecCmd;
058200170711         endif;
058300170711
058400171212         // - ?Uscita dal *pgm?
058500170711         return;
058600170711
058700170711       ENDSR;
058800170830
058900170830       //--------------------------------------------------------------
059000171212       //? Esecuzione del comando (già impostato).                      ?
059100170830       //--------------------------------------------------------------
059200170830       BEGSR  sr_ExecCmd;
059300170830
059400170830         clear Qcap0100;
059500170830         Qcabcsdh = *off;
059600170830         Qcapa    = *off;
059700170830         Qcacmdss = *off;
059800170830         Qcaerved = *allX'00';
059900170830
060000170830         clear Qusec;
060100170830         Qusbprv  = %size(Qusec);
060200170830
060300170830         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
060400170830                           %size(Qcap0100) : 'CPOP0100' : *omit :
060500170830                           0 : 0 : Qusec);
060600170830
060700170830         //*·if  Qusei <> *blank;
060800170830         //*·  ...;
060900170830         //*·endif;
061000170830
061100170830       ENDSR;
061200170711
061300170711       //--------------------------------------------------------------
061400171212       //? Output di stampa.                                            ?
061500170711       //--------------------------------------------------------------
061600170711
061700170711     oQSYSPRT   e            PRTtxt            1
061800170711     o                                              'LISTA P.I. SCARIC-
061900170711     o                                              ATE DAL SITO, MA C-
062000170711     o                                              ON DATA SCADENZA I-
062100170711     o                                              N ANSPI00F'
062200170711     o                       SDSpgm           +   5
062300170711     o                       SDSusr           +   3
062400170711     o                       wDate_Gma     y  +   3
062500170711     o                       wTime            +   3 '  :  :  '
062600170711     o                                        +   3 'Pag.'
062700170711     o                       Page          z  +   0
062800170711     o          e            PRTtxt      0
062900170711     o                                              'LISTA P.I. SCARIC-
063000170711     o                                              ATE DAL SITO, MA C-
063100170711     o                                              ON DATA SCADENZA I-
063200170711     o                                              N ANSPI00F'
063300170711     o          e            PRTtxt      1
063400170711     o                                              '------------------
063500170711     o                                              -------------------
063600170711     o                                              -------------------
063700170711     o                                              ----------'
063800170711     o          e            PRTtxt      0
063900170711     o                                              '------------------
064000170711     o                                              -------------------
064100170711     o                                              -------------------
064200170711     o                                              ----------'
064300170711     o          e            PRTtxt      2
064400170711     o                                              'P.I.            '
064500170711     o                                        +   2 'Scadenza  '
064600170711     o                                        +   2 'Modifica  '
064700170711     o                                        +   2 'Utente    '
064800170711     o          e            PRTtxt      1  1
064900170711     o                                              '================'
065000170711     o                                        +   2 '=========='
065100170711     o                                        +   2 '=========='
065200170711     o                                        +   2 '=========='
065300170711      *
065400170711     o          e            PRTdet      1
065500170711     o                       prt_IVA
065600170711     o                       prt_DSC          +   2 '  /  /  0 '
065700170711     o                       prt_DMO          +   2 '  /  /  0 '
065800170711     o                       prt_PMO          +   2
065900170711      *
066000170711     o          e            PRTend      2
066100170711     o                                              '***  Fine Lista  ***'
066200170711     o          e            PRTend      0
066300170711     o                                              '***  Fine Lista  ***'

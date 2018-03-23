000100170711       //==============================================================
000200170711       //?TNVRSPI1R // Aggiornamento *file ANSPI00F con dati scaricati ?
000300170711       //?             dal sito www.finanze.it (in formato .XLS) e     ?
000400170711       //?             portati nel work-file WSPLIT0F.                 ?
000500170711       //==============================================================
000600170711
000700170711       //--------------------------------------------------------------
000800170711       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000900170711       //--------------------------------------------------------------
001000170711
001100170711     /*PRM  dbgview(*source)
001200170711     /*END
001300170711
001400170711       //--------------------------------------------------------------
001500170711       //?Specifiche di controllo.                                     ?
001600170711       //--------------------------------------------------------------
001700170711
001800170711     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001900170711     h dftactgrp(*no)
002000170711     h alwnull(*inputonly)
002100170711
002200170711       //--------------------------------------------------------------
002300170711       //?Dichiarazione file.                                          ?
002400170711       //--------------------------------------------------------------
002500170711
002600170711       // -?Soggetti Split Payment?
002700170711     fANSPI01L  Uf A e           k disk
002800170711
002900170711       // -?Lista Errori rilevati?
003000170711     fQSYSPRT   o    f  132        printer usropn
003100170711     f                                     oflind(*inOF)
003200170711
003300170711       //--------------------------------------------------------------
003400170711       //?Definizione costanti.                                        ?
003500170711       //--------------------------------------------------------------
003600170711
003700170711       // -?Data scadenza massima (numerica)?
003800170711     d c_DataMax       c                   const(20391231)
003900170830
004000170830       // -?Comandi di override al PrtF QSYSPRT?
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
005600170711       //?Definizione aree dati.                                       ?
005700170711       //--------------------------------------------------------------
005800170711
005900170711
006000170711       //--------------------------------------------------------------
006100170711       //?Definizione strutture dati.                                  ?
006200170711       //--------------------------------------------------------------
006300170711
006400170711       // -?Status ds?
006500170711     d Status         sds
006600170711     d   SDSpgm          *proc
006700170711     d*//SDSprm          *parms
006800170711     d*//SDSdta              191    198
006900170711     d*//SDSjob              244    253
007000170711     d   SDSusr              254    263
007100170830
007200170830       // -?Parametri ricevuti?
007300170830     d KPJBA         e ds
007400170830     d TNVRSPI1ds    e ds                  qualified
007500170711
007600170711       // -?Data/Ora attuali?
007700170711     d wTime_ds        ds                  inz
007800170711     d   wDate                        8s 0 inz
007900170711     d   wTime                        6s 0 inz
008000170711     d   wTimeHM               9     12s 0 inz
008100170711
008200170711       //--------------------------------------------------------------
008300170711       //?Definizione variabili globali.                               ?
008400170711       //--------------------------------------------------------------
008500170711
008600170830       // -?Altri parametri (forse) ricevuti?
008700170711     d prm_FileName    s             21
008800170711
008900170711       // -?Nome esteso Libreria/File del file con i nuovi dati?
009000170711     d wLibFile        s             21a   inz('WSPLIT0F            ')
009100170711
009200170711       // -?Flags booleani?
009300170711     d $Fine           s               n   inz
009400170711
009500170711       // -?Indici di schiera?
009600170711     d xx              s              3  0 inz
009700170711
009800170711       // -?Stringa SQL da eseguire?
009900170711     d wSql            s          32740    inz    varying
010000170711
010100170711       // -?Dati estratti via SQL?
010200170711     d wPIVA           s                   inz    like( SPIiva )
010300170711
010400170711       // -?Campi di stampa?
010500170711     d prt_IVA         s                   inz    like( SPIiva )
010600170711     d prt_DSC         s                   inz    like( SPIdsc )
010700170711     d prt_DMO         s                   inz    like( SPIdmo )
010800170711     d prt_PMO         s                   inz    like( SPIpmo )
010900170711
011000170711       // -?Campi di comodo?
011100170711       // ·?Data *ISO?
011200170711     d wDate_Iso       s               d   datfmt(*ISO)    inz(*loval)
011300170711       // ·?Data *EUR?
011400170711     d wDate_Eur       s               d   datfmt(*EUR)    inz(*loval)
011500170711       // ·?Data in stampa?
011600170711     d wDate_Gma       s              8  0 inz
011700170711
011800170711       //--------------------------------------------------------------
011900170711       //?Definizione prototipi procedure usate.                       ?
012000170711       //--------------------------------------------------------------
012100170711
012200170711       // -?API QCAPCMD (Process Commands)?
012300170711     d Qcmd            s           2048    inz   varying
012400170711      /copy qSysInc/qRpgleSrc,QCAPCMD
012500170711      /copy gaitrasrc/srcProtoPR,QCAPCMD
012600170711       // -?Parametri gestione errori API.?
012700170711      /copy qsysinc/qRpgleSrc,QUSEC
012800170711
012900170711       //--------------------------------------------------------------
013000170711       //?Definizione key-list.                                        ?
013100170711       //--------------------------------------------------------------
013200170711
013300170711       // -?File ANSPI01L?
013400170711     d keyANSPI01    e ds                  extname( ANSPI01L : *key )
013500170711     d                                     prefix( k_ )  inz
013600170711
013700170711       //--------------------------------------------------------------
013800170711       //?Riepilogo indicatori utilizzati.                             ?
013900170711       //--------------------------------------------------------------
014000170711       //--------------------------------------------------------------
014100170711
014200170711       //--------------------------------------------------------------
014300170711       //?M A I N - L I N E                                            ?
014400170711       //--------------------------------------------------------------
014500170711
014600170711     c     *Entry        plist
014700170830     c                   parm                    KPJBA
014800170711     c                   parm                    prm_FileName
014900170711
015000170711       // -?Operazioni iniziali?
015100170711       exsr  sr_RoutInz;
015200170711
015300170711       // -?1) SCRITTURA (in aggiunta) ........ dati in ANSPI00F?
015400170711       // -?2) ANNULLAMENTO (con data scadenza) dati in ANSPI00F?
015500170711       // -?3) SEGNALAZIONE (in stampa) .... scaduti in ANSPI00F?
015600170711       For  xx = 1  To 3;
015700170711
015800170711         // -?Preparazione stringa SQL?
015900170711         exsr  sr_PrepSQL;
016000170711
016100170711         // -?Apertura del cursore?
016200170711         exsr  sr_OpenCursor;
016300170711
016400170711         // -?Lettura del file?
016500170711         DoW  Not $Fine;
016600170711           exsr  sr_ReadCursor;
016700170711         EndDo;
016800170711
016900170711         // -?Chiusura del cursore?
017000170711         exsr  sr_CloseCursor;
017100170711
017200170711       EndFor;
017300170711
017400170711       // -?Operazioni finali?
017500170711       exsr  sr_RoutEnd;
017600170711
017700170711       //--------------------------------------------------------------
017800170711       //?Operazioni iniziali.                                         ?
017900170711       //--------------------------------------------------------------
018000170711       BEGSR  sr_RoutInz;
018100170711
018200170711         exec sql   set option   DynUsrPrf = *Owner,
018300170711                                 CloSqlCsr = *EndMod;
018400170711
018500170711         // -?Impostazione chiusura?
018600170711         *inLR = *on;
018700170711
018800170711         // -?Reperimento data e ora attuali?
018900170711         wTime_ds  = %editc( %dec( %timestamp() ) : 'X' );
019000170711         wDate_Iso = %date( wDate : *Iso );
019100170711         wDate_Eur = wDate_Iso;
019200170711         wDate_Gma = %dec( wDate_Eur );
019300170830
019400170830         // -?Reperimento parametri?
019500170830         TNVRSPI1ds = kpjbu;
019600170711
019700170830         // -?Impostaz. nome file EVENTUALMENTE ricevuto come 2° parametro?
019800170711         reset  wLibFile;
019900170830         if  %parms()               > 1       and
020000170711             %trim( prm_FileName ) <> *blank;
020100170711           wLibFile = %trim( prm_FileName );
020200170711         endif;
020300170711
020400170711       ENDSR;
020500170711
020600170711       //--------------------------------------------------------------
020700170711       //?Preparazione stringa SQL per estrazione dati da WSPLIT0F.    ?
020800170711       //--------------------------------------------------------------
020900170711       BEGSR  sr_PrepSQL;
021000170711
021100170711         clear  wSQL;
021200170711         clear  $Fine;
021300170711
021400170711         Select;
021500170711
021600170711           // -?1) Per SCRITTURA (in aggiunta) .......... dati in ANSPI00F?
021700170711           When  xx = 1;
021800170711
021900170712             wSQL =  'select distinct w.SPIiva +
022000170711
022100170711                        from ' + %trim( wLibFile ) + ' w +
022200170711                             exception join ANSPI00F a +
022300170711
022400170711                          on w.SPIiva = a.SPIiva +
022500170711
022600170711                       order by w.SPIiva +
022700170711
022800170711                         for fetch only';
022900170711
023000170711           // -?2) Per STAMPA delle P.I. che risultano scadute in ANSPI00F?
023100170711           When  xx = 2;
023200170711
023300170712             wSQL =  'select distinct a.SPIiva +
023400170711
023500170711                        from ANSPI00F a inner join ' +
023600170711                             %trim( wLibFile ) + ' w +
023700170711
023800170711                          on a.SPIiva = w.SPIiva +
023900170711
024000170711                       where a.SPIdsc < ' + %trim( %editc( c_DataMax : 'X' ) ) +
024100170711
024200170711                     ' order by a.SPIiva +
024300170711
024400170711                         for fetch only';
024500170711
024600170711           // -?3) Per ANNULLAMENTO (con data scadenza) . dati in ANSPI00F?
024700170711           When  xx = 3;
024800170711
024900170712             wSQL =  'select distinct a.SPIiva +
025000170711
025100170711                        from ANSPI00F a exception join ' +
025200170711                             %trim( wLibFile ) + ' w +
025300170711
025400170711                          on a.SPIiva = w.SPIiva +
025500170711
025600170711                       where a.SPIdsc = ' + %trim( %editc( c_DataMax : 'X' ) ) +
025700170711
025800170711                     ' order by a.SPIiva +
025900170711
026000170711                         for fetch only';
026100170711
026200170711           // -?? ? ? ? ??
026300170711           Other;
026400170711
026500170711             $Fine = *on;
026600170711
026700170711         EndSl;
026800170711
026900170711       ENDSR;
027000170711
027100170711       //--------------------------------------------------------------
027200170711       //?Apertura cursore.                                            ?
027300170711       //--------------------------------------------------------------
027400170711       BEGSR  sr_OpenCursor;
027500170711
027600170711         // -?Dichiarazione cursore?
027700170711         exec sql   prepare S1   from :wSQL;
027800170711         exec sql   declare C1   cursor for S1;
027900170711
028000170711         if  SQLcode < *zero;
028100170711           $Fine = *on;
028200170711           exsr  sr_PrintERR;
028300170711         endif;
028400170711
028500170711         // -?Apertura del cursore?
028600170711         exec sql   open C1;
028700170711
028800170711         if  SQLcode < *zero;
028900170711           $Fine = *on;
029000170711           exsr  sr_PrintERR;
029100170711         endif;
029200170711
029300170711       ENDSR;
029400170711
029500170711       //--------------------------------------------------------------
029600170711       //?Lettura cursore.                                             ?
029700170711       //--------------------------------------------------------------
029800170711       BEGSR  sr_ReadCursor;
029900170711
030000170711         $Fine = *off;
030100170711         clear  wPIVA;
030200170711
030300170711         exec sql   fetch next   from C1   into :wPIVA;
030400170711
030500170711         select;
030600170711           when  SQLcode = 100;
030700170711             $Fine = *on;
030800170711           when  SQLcode < *zero;
030900170711             $Fine = *on;
031000170711             exsr  sr_PrintERR;
031100170711           other;
031200170711             select;
031300170711               when  xx = 1;
031400170711                 exsr  sr_WriteANSPI;
031500170711               when  xx = 2;
031600170711                 exsr  sr_PrintANSPI;
031700170711               when  xx = 3;
031800170711                 exsr  sr_AnnullANSPI;
031900170711             endsl;
032000170711         endsl;
032100170711
032200170711       ENDSR;
032300170711
032400170711       //--------------------------------------------------------------
032500170711       //?Chiusura cursore.                                            ?
032600170711       //--------------------------------------------------------------
032700170711       BEGSR  sr_CloseCursor;
032800170711
032900170711         // -?Chiusura del cursore?
033000170711         exec sql   close C1;
033100170711
033200170711       ENDSR;
033300170711
033400170711       //--------------------------------------------------------------
033500170711       //?Scrittura record in ANSPI00F.                                ?
033600170711       //--------------------------------------------------------------
033700170711       BEGSR  sr_WriteANSPI;
033800170711
033900170711         clear  ANSPI000;
034000170711
034100170711         SPIiva  = wPIVA;
034200170830         //*·SPIdde  = 20170701;
034300170830         SPIdde  = TNVRSPI1ds.VRSP1dde;
034400170711         SPIdsc  = c_DataMax;
034500170711
034600170711         SPIdim  = wDate;
034700170711         SPIhim  = wTimeHM;
034800170711         SPIpim  = 'BATCH     ';
034900170711
035000170711         //______________
035100170711         write  ANSPI000;
035200170711         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
035300170711
035400170711       ENDSR;
035500170711
035600170711       //--------------------------------------------------------------
035700170711       //?Annullamento record in ANSPI00F.                             ?
035800170711       //--------------------------------------------------------------
035900170711       BEGSR  sr_AnnullANSPI;
036000170711
036100170711         chain  ( wPIVA )  ANSPI000;
036200170711
036300170711         if  NOT %found( ANSPI01L );
036400170711           leavesr;
036500170711         endif;
036600170711
036700170830         //*·SPIdsc  = wDate;
036800170830         SPIdsc  = TNVRSPI1ds.VRSP1dsc;
036900170711
037000170711         SPIdmo  = wDate;
037100170711         SPIhmo  = wTimeHM;
037200170711         SPIpmo  = 'BATCH     ';
037300170711
037400170711         //_______________
037500170711         update  ANSPI000;
037600170711         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
037700170711
037800170711       ENDSR;
037900170711
038000170711       //--------------------------------------------------------------
038100170711       //?Stampa P.I. che risulta scaduta in ANSPI00F.                 ?
038200170711       //--------------------------------------------------------------
038300170711       BEGSR  sr_PrintANSPI;
038400170711
038500170711         chain(N)  ( wPIVA )  ANSPI000;
038600170711
038700170711         if  NOT %found( ANSPI01L );
038800170711           leavesr;
038900170711         endif;
039000170711
039100170711         if  Not  %open(QSYSPRT);
039200170830           Qcmd = c_CmdOvrPrtF;
039300170830           exsr  sr_ExecCmd;
039400170711           open  QSYSPRT;
039500170711           *inOF = *on;
039600170711         endif;
039700170711
039800170711         prt_IVA = SPIiva;
039900170711
040000170711         wDate_Iso = %date( SPIdsc : *Iso );
040100170711         wDate_Eur = wDate_Iso;
040200170711         prt_DSC   = %dec( wDate_Eur );
040300170711
040400170711         clear  wDate_Iso;
040500170711         clear  wDate_Eur;
040600170711         clear  prt_DMO;
040700170711         if  SPIdmo > *zero;
040800170711           wDate_Iso = %date( SPIdmo : *Iso );
040900170711           wDate_Eur = wDate_Iso;
041000170711           prt_DMO   = %dec( wDate_Eur );
041100170711         endif;
041200170711
041300170711         prt_PMO   = SPIpmo;
041400170711
041500170711         if  *inOF;
041600170711           except  PRTtxt;
041700170711           *inOF = *off;
041800170711         endif;
041900170711
042000170711         except  PRTdet;
042100170711
042200170711       ENDSR;
042300170711
042400170711       //--------------------------------------------------------------
042500170711       //?Stampa segnalazione dell'errore rilevato via SQL             ?
042600170711       //--------------------------------------------------------------
042700170711       BEGSR  sr_PrintERR;
042800170711
042900170711         // -?Stampa del Dump?
043000170711         Dump(A);
043100170711
043200170711         // -?Stampa del Job-Log?
043300170711         Qcmd = 'DSPJOBLOG job(*) output(*print)';
043400170711         exsr  sr_ExecCmd;
043500170711
043600170711         // -?Uscita dal *pgm?
043700170711         exsr  sr_RoutEnd;
043800170711
043900170711       ENDSR;
044000170711
044100170711       //--------------------------------------------------------------
044200170711       //?Operazioni finali.                                           ?
044300170711       //--------------------------------------------------------------
044400170711       BEGSR  sr_RoutEnd;
044500170711
044600170711         // -?SE stampato almeno un Errore: stampa "fine"?
044700170711         if  %open(QSYSPRT);
044800170711           except  PRTend;
044900170711           close   QSYSPRT;
045000170830           Qcmd = c_CmdDltOvr;
045100170830           exsr  sr_ExecCmd;
045200170711         endif;
045300170711
045400170711         // -?Uscita dal *pgm?
045500170711         return;
045600170711
045700170711       ENDSR;
045800170830
045900170830       //--------------------------------------------------------------
046000170830       //?Esecuzione del comando (già impostato).                      ?
046100170830       //--------------------------------------------------------------
046200170830       BEGSR  sr_ExecCmd;
046300170830
046400170830         clear Qcap0100;
046500170830         Qcabcsdh = *off;
046600170830         Qcapa    = *off;
046700170830         Qcacmdss = *off;
046800170830         Qcaerved = *allX'00';
046900170830
047000170830         clear Qusec;
047100170830         Qusbprv  = %size(Qusec);
047200170830
047300170830         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
047400170830                           %size(Qcap0100) : 'CPOP0100' : *omit :
047500170830                           0 : 0 : Qusec);
047600170830
047700170830         //*·if  Qusei <> *blank;
047800170830         //*·  ...;
047900170830         //*·endif;
048000170830
048100170830       ENDSR;
048200170711
048300170711       //--------------------------------------------------------------
048400170711       //?Output di stampa.                                            ?
048500170711       //--------------------------------------------------------------
048600170711
048700170711     oQSYSPRT   e            PRTtxt            1
048800170711     o                                              'LISTA P.I. SCARIC-
048900170711     o                                              ATE DAL SITO, MA C-
049000170711     o                                              ON DATA SCADENZA I-
049100170711     o                                              N ANSPI00F'
049200170711     o                       SDSpgm           +   5
049300170711     o                       SDSusr           +   3
049400170711     o                       wDate_Gma     y  +   3
049500170711     o                       wTime            +   3 '  :  :  '
049600170711     o                                        +   3 'Pag.'
049700170711     o                       Page          z  +   0
049800170711     o          e            PRTtxt      0
049900170711     o                                              'LISTA P.I. SCARIC-
050000170711     o                                              ATE DAL SITO, MA C-
050100170711     o                                              ON DATA SCADENZA I-
050200170711     o                                              N ANSPI00F'
050300170711     o          e            PRTtxt      1
050400170711     o                                              '------------------
050500170711     o                                              -------------------
050600170711     o                                              -------------------
050700170711     o                                              ----------'
050800170711     o          e            PRTtxt      0
050900170711     o                                              '------------------
051000170711     o                                              -------------------
051100170711     o                                              -------------------
051200170711     o                                              ----------'
051300170711     o          e            PRTtxt      2
051400170711     o                                              'P.I.            '
051500170711     o                                        +   2 'Scadenza  '
051600170711     o                                        +   2 'Modifica  '
051700170711     o                                        +   2 'Utente    '
051800170711     o          e            PRTtxt      1  1
051900170711     o                                              '================'
052000170711     o                                        +   2 '=========='
052100170711     o                                        +   2 '=========='
052200170711     o                                        +   2 '=========='
052300170711      *
052400170711     o          e            PRTdet      1
052500170711     o                       prt_IVA
052600170711     o                       prt_DSC          +   2 '  /  /  0 '
052700170711     o                       prt_DMO          +   2 '  /  /  0 '
052800170711     o                       prt_PMO          +   2
052900170711      *
053000170711     o          e            PRTend      2
053100170711     o                                              '***  Fine Lista  ***'
053200170711     o          e            PRTend      0
053300170711     o                                              '***  Fine Lista  ***'

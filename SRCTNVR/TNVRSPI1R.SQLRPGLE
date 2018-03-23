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
002500171220
002600171220       // -?WrkF Temp x Split Payment?
002700171220     fWSPLIT0F  Uf   e           k disk    usropn
002800171220     f                                     extfile( wLibFile )
002900171220     f                                     rename( WSPLIT0F : WSPLIT00 )
003000170711
003100170711       // -?Soggetti Split Payment?
003200170711     fANSPI01L  Uf A e           k disk
003300170711
003400170711       // -?Lista Errori rilevati?
003500170711     fQSYSPRT   o    f  132        printer usropn
003600170711     f                                     oflind(*inOF)
003700170711
003800170711       //--------------------------------------------------------------
003900170711       //?Definizione costanti.                                        ?
004000170711       //--------------------------------------------------------------
004100170711
004200170711       // -?Data scadenza massima (numerica)?
004300170711     d c_DataMax       c                   const(20391231)
004400170830
004500170830       // -?Comandi di override al PrtF QSYSPRT?
004600170830     d c_CmdOvrPrtF    c                   const('OVRPRTF +
004700170830     d                                           file(QSYSPRT) +
004800170830     d                                           usrdta(''PI_scadute'') +
004900170830     d                                           ovrscope(*actgrpdfn) +
005000170830     d                                           ')
005100170830     d c_CmdDltOvr     c                   const('DLTOVR +
005200170830     d                                            file(QSYSPRT) +
005300170830     d                                            lvl(*actgrpdfn)')
005400170711
005500170711       //--------------------------------------------------------------
005600170711       //?Definizione schiere.                                         ?
005700170711       //--------------------------------------------------------------
005800170711
005900170711
006000170711       //--------------------------------------------------------------
006100170711       //?Definizione aree dati.                                       ?
006200170711       //--------------------------------------------------------------
006300170711
006400170711
006500170711       //--------------------------------------------------------------
006600170711       //?Definizione strutture dati.                                  ?
006700170711       //--------------------------------------------------------------
006800170711
006900170711       // -?Status ds?
007000170711     d Status         sds
007100170711     d   SDSpgm          *proc
007200170711     d*//SDSprm          *parms
007300170711     d*//SDSdta              191    198
007400170711     d*//SDSjob              244    253
007500170711     d   SDSusr              254    263
007600170830
007700170830       // -?Parametri ricevuti?
007800170830     d KPJBA         e ds
007900170830     d TNVRSPI1ds    e ds                  qualified
008000170711
008100170711       // -?Data/Ora attuali?
008200170711     d wTime_ds        ds                  inz
008300170711     d   wDate                        8s 0 inz
008400170711     d   wTime                        6s 0 inz
008500170711     d   wTimeHM               9     12s 0 inz
008600170711
008700170711       //--------------------------------------------------------------
008800170711       //?Definizione variabili globali.                               ?
008900170711       //--------------------------------------------------------------
009000170711
009100170830       // -?Altri parametri (forse) ricevuti?
009200170711     d prm_FileName    s             21
009300170711
009400170711       // -?Nome esteso Libreria/File del file con i nuovi dati?
009500170711     d wLibFile        s             21a   inz('WSPLIT0F            ')
009600170711
009700170711       // -?Flags booleani?
009800170711     d $Fine           s               n   inz
009900170711
010000170711       // -?Indici di schiera?
010100170711     d xx              s              3  0 inz
010200170711
010300170711       // -?Stringa SQL da eseguire?
010400170711     d wSql            s          32740    inz    varying
010500170711
010600170711       // -?Dati estratti via SQL?
010700170711     d wPIVA           s                   inz    like( SPIiva )
010800170711
010900170711       // -?Campi di stampa?
011000170711     d prt_IVA         s                   inz    like( SPIiva )
011100170711     d prt_DSC         s                   inz    like( SPIdsc )
011200170711     d prt_DMO         s                   inz    like( SPIdmo )
011300170711     d prt_PMO         s                   inz    like( SPIpmo )
011400170711
011500170711       // -?Campi di comodo?
011600170711       // ·?Data *ISO?
011700170711     d wDate_Iso       s               d   datfmt(*ISO)    inz(*loval)
011800170711       // ·?Data *EUR?
011900170711     d wDate_Eur       s               d   datfmt(*EUR)    inz(*loval)
012000170711       // ·?Data in stampa?
012100170711     d wDate_Gma       s              8  0 inz
012200171220       // ·?Partita Iva numerica?
012300171220     d wPartitaIvaN    s             11  0 inz
012400170711
012500170711       //--------------------------------------------------------------
012600170711       //?Definizione prototipi procedure usate.                       ?
012700170711       //--------------------------------------------------------------
012800170711
012900170711       // -?API QCAPCMD (Process Commands)?
013000170711     d Qcmd            s           2048    inz   varying
013100170711      /copy qSysInc/qRpgleSrc,QCAPCMD
013200170711      /copy gaitrasrc/srcProtoPR,QCAPCMD
013300170711       // -?Parametri gestione errori API.?
013400170711      /copy qsysinc/qRpgleSrc,QUSEC
013500170711
013600170711       //--------------------------------------------------------------
013700170711       //?Definizione key-list.                                        ?
013800170711       //--------------------------------------------------------------
013900170711
014000170711       // -?File ANSPI01L?
014100170711     d keyANSPI01    e ds                  extname( ANSPI01L : *key )
014200170711     d                                     prefix( k_ )  inz
014300170711
014400170711       //--------------------------------------------------------------
014500170711       //?Riepilogo indicatori utilizzati.                             ?
014600170711       //--------------------------------------------------------------
014700170711       //--------------------------------------------------------------
014800170711
014900170711       //--------------------------------------------------------------
015000170711       //?M A I N - L I N E                                            ?
015100170711       //--------------------------------------------------------------
015200170711
015300170711     c     *Entry        plist
015400170830     c                   parm                    KPJBA
015500170711     c                   parm                    prm_FileName
015600170711
015700170711       // -?Operazioni iniziali?
015800170711       exsr  sr_RoutInz;
015900170711
016000170711       // -?1) SCRITTURA (in aggiunta) ........ dati in ANSPI00F?
016100170711       // -?2) ANNULLAMENTO (con data scadenza) dati in ANSPI00F?
016200170711       // -?3) SEGNALAZIONE (in stampa) .... scaduti in ANSPI00F?
016300170711       For  xx = 1  To 3;
016400170711
016500170711         // -?Preparazione stringa SQL?
016600170711         exsr  sr_PrepSQL;
016700170711
016800170711         // -?Apertura del cursore?
016900170711         exsr  sr_OpenCursor;
017000170711
017100170711         // -?Lettura del file?
017200170711         DoW  Not $Fine;
017300170711           exsr  sr_ReadCursor;
017400170711         EndDo;
017500170711
017600170711         // -?Chiusura del cursore?
017700170711         exsr  sr_CloseCursor;
017800170711
017900170711       EndFor;
018000170711
018100170711       // -?Operazioni finali?
018200170711       exsr  sr_RoutEnd;
018300170711
018400170711       //--------------------------------------------------------------
018500170711       //?Operazioni iniziali.                                         ?
018600170711       //--------------------------------------------------------------
018700170711       BEGSR  sr_RoutInz;
018800170711
018900170711         exec sql   set option   DynUsrPrf = *Owner,
019000170711                                 CloSqlCsr = *EndMod;
019100170711
019200170711         // -?Impostazione chiusura?
019300170711         *inLR = *on;
019400170711
019500170711         // -?Reperimento data e ora attuali?
019600170711         wTime_ds  = %editc( %dec( %timestamp() ) : 'X' );
019700170711         wDate_Iso = %date( wDate : *Iso );
019800170711         wDate_Eur = wDate_Iso;
019900170711         wDate_Gma = %dec( wDate_Eur );
020000170830
020100170830         // -?Reperimento parametri?
020200170830         TNVRSPI1ds = kpjbu;
020300170711
020400170830         // -?Impostaz. nome file EVENTUALMENTE ricevuto come 2° parametro?
020500170711         reset  wLibFile;
020600170830         if  %parms()               > 1       and
020700170711             %trim( prm_FileName ) <> *blank;
020800170711           wLibFile = %trim( prm_FileName );
020900170711         endif;
021000171220
021100171220         // -?Sistemazione Partite Iva con *zeri iniziali mancanti?
021200171220         open(E)  WSPLIT0F;
021300171220         if  %error();
021400171220           exsr  sr_PrintERR;
021500171220           exsr  sr_RoutEnd;
021600171220         endif;
021700171220
021800171220         read  WSPLIT00;
021900171220         DoW  NOT %eof(WSPLIT0F);
022000171220           if  %len( %trim( SPIiva ) ) < 11;
022100171220             wPartitaIvaN = %int( %trim( SPIiva ) );
022200171220             SPIiva       = %editc( wPartitaIvaN : 'X' );
022300171220             update  WSPLIT00;
022400171220           endif;
022500171220           read  WSPLIT00;
022600171220         EndDo;
022700171220
022800171220         close  WSPLIT0F;
022900170711
023000170711       ENDSR;
023100170711
023200170711       //--------------------------------------------------------------
023300170711       //?Preparazione stringa SQL per estrazione dati da WSPLIT0F.    ?
023400170711       //--------------------------------------------------------------
023500170711       BEGSR  sr_PrepSQL;
023600170711
023700170711         clear  wSQL;
023800170711         clear  $Fine;
023900170711
024000170711         Select;
024100170711
024200170711           // -?1) Per SCRITTURA (in aggiunta) .......... dati in ANSPI00F?
024300170711           When  xx = 1;
024400170711
024500170712             wSQL =  'select distinct w.SPIiva +
024600170711
024700170711                        from ' + %trim( wLibFile ) + ' w +
024800170711                             exception join ANSPI00F a +
024900170711
025000170711                          on w.SPIiva = a.SPIiva +
025100170711
025200170711                       order by w.SPIiva +
025300170711
025400170711                         for fetch only';
025500170711
025600170711           // -?2) Per STAMPA delle P.I. che risultano scadute in ANSPI00F?
025700170711           When  xx = 2;
025800170711
025900170712             wSQL =  'select distinct a.SPIiva +
026000170711
026100170711                        from ANSPI00F a inner join ' +
026200170711                             %trim( wLibFile ) + ' w +
026300170711
026400170711                          on a.SPIiva = w.SPIiva +
026500170711
026600170711                       where a.SPIdsc < ' + %trim( %editc( c_DataMax : 'X' ) ) +
026700170711
026800170711                     ' order by a.SPIiva +
026900170711
027000170711                         for fetch only';
027100170711
027200170711           // -?3) Per ANNULLAMENTO (con data scadenza) . dati in ANSPI00F?
027300170711           When  xx = 3;
027400170711
027500170712             wSQL =  'select distinct a.SPIiva +
027600170711
027700170711                        from ANSPI00F a exception join ' +
027800170711                             %trim( wLibFile ) + ' w +
027900170711
028000170711                          on a.SPIiva = w.SPIiva +
028100170711
028200170711                       where a.SPIdsc = ' + %trim( %editc( c_DataMax : 'X' ) ) +
028300170711
028400170711                     ' order by a.SPIiva +
028500170711
028600170711                         for fetch only';
028700170711
028800170711           // -?? ? ? ? ??
028900170711           Other;
029000170711
029100170711             $Fine = *on;
029200170711
029300170711         EndSl;
029400170711
029500170711       ENDSR;
029600170711
029700170711       //--------------------------------------------------------------
029800170711       //?Apertura cursore.                                            ?
029900170711       //--------------------------------------------------------------
030000170711       BEGSR  sr_OpenCursor;
030100170711
030200170711         // -?Dichiarazione cursore?
030300170711         exec sql   prepare S1   from :wSQL;
030400170711         exec sql   declare C1   cursor for S1;
030500170711
030600170711         if  SQLcode < *zero;
030700170711           $Fine = *on;
030800170711           exsr  sr_PrintERR;
030900170711         endif;
031000170711
031100170711         // -?Apertura del cursore?
031200170711         exec sql   open C1;
031300170711
031400170711         if  SQLcode < *zero;
031500170711           $Fine = *on;
031600170711           exsr  sr_PrintERR;
031700170711         endif;
031800170711
031900170711       ENDSR;
032000170711
032100170711       //--------------------------------------------------------------
032200170711       //?Lettura cursore.                                             ?
032300170711       //--------------------------------------------------------------
032400170711       BEGSR  sr_ReadCursor;
032500170711
032600170711         $Fine = *off;
032700170711         clear  wPIVA;
032800170711
032900170711         exec sql   fetch next   from C1   into :wPIVA;
033000170711
033100170711         select;
033200170711           when  SQLcode = 100;
033300170711             $Fine = *on;
033400170711           when  SQLcode < *zero;
033500170711             $Fine = *on;
033600170711             exsr  sr_PrintERR;
033700170711           other;
033800170711             select;
033900170711               when  xx = 1;
034000170711                 exsr  sr_WriteANSPI;
034100170711               when  xx = 2;
034200170711                 exsr  sr_PrintANSPI;
034300170711               when  xx = 3;
034400170711                 exsr  sr_AnnullANSPI;
034500170711             endsl;
034600170711         endsl;
034700170711
034800170711       ENDSR;
034900170711
035000170711       //--------------------------------------------------------------
035100170711       //?Chiusura cursore.                                            ?
035200170711       //--------------------------------------------------------------
035300170711       BEGSR  sr_CloseCursor;
035400170711
035500170711         // -?Chiusura del cursore?
035600170711         exec sql   close C1;
035700170711
035800170711       ENDSR;
035900170711
036000170711       //--------------------------------------------------------------
036100170711       //?Scrittura record in ANSPI00F.                                ?
036200170711       //--------------------------------------------------------------
036300170711       BEGSR  sr_WriteANSPI;
036400170711
036500170711         clear  ANSPI000;
036600170711
036700170711         SPIiva  = wPIVA;
036800170830         //*·SPIdde  = 20170701;
036900170830         SPIdde  = TNVRSPI1ds.VRSP1dde;
037000170711         SPIdsc  = c_DataMax;
037100170711
037200170711         SPIdim  = wDate;
037300170711         SPIhim  = wTimeHM;
037400170711         SPIpim  = 'BATCH     ';
037500170711
037600170711         //______________
037700170711         write  ANSPI000;
037800170711         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
037900170711
038000170711       ENDSR;
038100170711
038200170711       //--------------------------------------------------------------
038300170711       //?Annullamento record in ANSPI00F.                             ?
038400170711       //--------------------------------------------------------------
038500170711       BEGSR  sr_AnnullANSPI;
038600170711
038700170711         chain  ( wPIVA )  ANSPI000;
038800170711
038900170711         if  NOT %found( ANSPI01L );
039000170711           leavesr;
039100170711         endif;
039200170711
039300170830         //*·SPIdsc  = wDate;
039400170830         SPIdsc  = TNVRSPI1ds.VRSP1dsc;
039500170711
039600170711         SPIdmo  = wDate;
039700170711         SPIhmo  = wTimeHM;
039800170711         SPIpmo  = 'BATCH     ';
039900170711
040000170711         //_______________
040100170711         update  ANSPI000;
040200170711         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
040300170711
040400170711       ENDSR;
040500170711
040600170711       //--------------------------------------------------------------
040700170711       //?Stampa P.I. che risulta scaduta in ANSPI00F.                 ?
040800170711       //--------------------------------------------------------------
040900170711       BEGSR  sr_PrintANSPI;
041000170711
041100170711         chain(N)  ( wPIVA )  ANSPI000;
041200170711
041300170711         if  NOT %found( ANSPI01L );
041400170711           leavesr;
041500170711         endif;
041600170711
041700170711         if  Not  %open(QSYSPRT);
041800170830           Qcmd = c_CmdOvrPrtF;
041900170830           exsr  sr_ExecCmd;
042000170711           open  QSYSPRT;
042100170711           *inOF = *on;
042200170711         endif;
042300170711
042400170711         prt_IVA = SPIiva;
042500170711
042600170711         wDate_Iso = %date( SPIdsc : *Iso );
042700170711         wDate_Eur = wDate_Iso;
042800170711         prt_DSC   = %dec( wDate_Eur );
042900170711
043000170711         clear  wDate_Iso;
043100170711         clear  wDate_Eur;
043200170711         clear  prt_DMO;
043300170711         if  SPIdmo > *zero;
043400170711           wDate_Iso = %date( SPIdmo : *Iso );
043500170711           wDate_Eur = wDate_Iso;
043600170711           prt_DMO   = %dec( wDate_Eur );
043700170711         endif;
043800170711
043900170711         prt_PMO   = SPIpmo;
044000170711
044100170711         if  *inOF;
044200170711           except  PRTtxt;
044300170711           *inOF = *off;
044400170711         endif;
044500170711
044600170711         except  PRTdet;
044700170711
044800170711       ENDSR;
044900170711
045000170711       //--------------------------------------------------------------
045100170711       //?Stampa segnalazione dell'errore rilevato via SQL             ?
045200170711       //--------------------------------------------------------------
045300170711       BEGSR  sr_PrintERR;
045400170711
045500170711         // -?Stampa del Dump?
045600170711         Dump(A);
045700170711
045800170711         // -?Stampa del Job-Log?
045900170711         Qcmd = 'DSPJOBLOG job(*) output(*print)';
046000170711         exsr  sr_ExecCmd;
046100170711
046200170711         // -?Uscita dal *pgm?
046300170711         exsr  sr_RoutEnd;
046400170711
046500170711       ENDSR;
046600170711
046700170711       //--------------------------------------------------------------
046800170711       //?Operazioni finali.                                           ?
046900170711       //--------------------------------------------------------------
047000170711       BEGSR  sr_RoutEnd;
047100170711
047200170711         // -?SE stampato almeno un Errore: stampa "fine"?
047300170711         if  %open(QSYSPRT);
047400170711           except  PRTend;
047500170711           close   QSYSPRT;
047600170830           Qcmd = c_CmdDltOvr;
047700170830           exsr  sr_ExecCmd;
047800170711         endif;
047900170711
048000170711         // -?Uscita dal *pgm?
048100170711         return;
048200170711
048300170711       ENDSR;
048400170830
048500170830       //--------------------------------------------------------------
048600170830       //?Esecuzione del comando (già impostato).                      ?
048700170830       //--------------------------------------------------------------
048800170830       BEGSR  sr_ExecCmd;
048900170830
049000170830         clear Qcap0100;
049100170830         Qcabcsdh = *off;
049200170830         Qcapa    = *off;
049300170830         Qcacmdss = *off;
049400170830         Qcaerved = *allX'00';
049500170830
049600170830         clear Qusec;
049700170830         Qusbprv  = %size(Qusec);
049800170830
049900170830         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
050000170830                           %size(Qcap0100) : 'CPOP0100' : *omit :
050100170830                           0 : 0 : Qusec);
050200170830
050300170830         //*·if  Qusei <> *blank;
050400170830         //*·  ...;
050500170830         //*·endif;
050600170830
050700170830       ENDSR;
050800170711
050900170711       //--------------------------------------------------------------
051000170711       //?Output di stampa.                                            ?
051100170711       //--------------------------------------------------------------
051200170711
051300170711     oQSYSPRT   e            PRTtxt            1
051400170711     o                                              'LISTA P.I. SCARIC-
051500170711     o                                              ATE DAL SITO, MA C-
051600170711     o                                              ON DATA SCADENZA I-
051700170711     o                                              N ANSPI00F'
051800170711     o                       SDSpgm           +   5
051900170711     o                       SDSusr           +   3
052000170711     o                       wDate_Gma     y  +   3
052100170711     o                       wTime            +   3 '  :  :  '
052200170711     o                                        +   3 'Pag.'
052300170711     o                       Page          z  +   0
052400170711     o          e            PRTtxt      0
052500170711     o                                              'LISTA P.I. SCARIC-
052600170711     o                                              ATE DAL SITO, MA C-
052700170711     o                                              ON DATA SCADENZA I-
052800170711     o                                              N ANSPI00F'
052900170711     o          e            PRTtxt      1
053000170711     o                                              '------------------
053100170711     o                                              -------------------
053200170711     o                                              -------------------
053300170711     o                                              ----------'
053400170711     o          e            PRTtxt      0
053500170711     o                                              '------------------
053600170711     o                                              -------------------
053700170711     o                                              -------------------
053800170711     o                                              ----------'
053900170711     o          e            PRTtxt      2
054000170711     o                                              'P.I.            '
054100170711     o                                        +   2 'Scadenza  '
054200170711     o                                        +   2 'Modifica  '
054300170711     o                                        +   2 'Utente    '
054400170711     o          e            PRTtxt      1  1
054500170711     o                                              '================'
054600170711     o                                        +   2 '=========='
054700170711     o                                        +   2 '=========='
054800170711     o                                        +   2 '=========='
054900170711      *
055000170711     o          e            PRTdet      1
055100170711     o                       prt_IVA
055200170711     o                       prt_DSC          +   2 '  /  /  0 '
055300170711     o                       prt_DMO          +   2 '  /  /  0 '
055400170711     o                       prt_PMO          +   2
055500170711      *
055600170711     o          e            PRTend      2
055700170711     o                                              '***  Fine Lista  ***'
055800170711     o          e            PRTend      0
055900170711     o                                              '***  Fine Lista  ***'

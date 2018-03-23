000100110110       //==============================================================
000200110110       //?TNVRSM009R - Lista loghi (segmenti di pagina) eliminabili.   ?
000300110110       //==============================================================
000400110110
000500110110       //--------------------------------------------------------------
000600110110       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700110110       //--------------------------------------------------------------
000800110110
000900110110     /*PRM  dbgview(*source)
001000110110     /*CMD  ovrdbf file(FNBLP01L) +
001100110110     /*CMD         tofile(FILTRAPRD/FNBLP01L) +
001200110110     /*CMD         ovrscope(*calllvl)
001300110110     /*END  dltovr file(FNBLP01L) +
001400110110     /*END         lvl(*)
001500110110     /*END
001600110110
001700110110       //--------------------------------------------------------------
001800110110       //?Specifiche di controllo.                                     ?
001900110110       //--------------------------------------------------------------
002000110110
002100110110     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002200110110     h dftactgrp(*no) bnddir('UBBNDDIR')
002300110110     h alwnull(*inputonly)
002400110110
002500110110       //--------------------------------------------------------------
002600110110       //?Dichiarazione file.                                          ?
002700110110       //--------------------------------------------------------------
002800110110
002900110110
003000110110       // -?Tabella "AR8"?
003100110110     fTNTBE01L  if   e           k disk
003200110110
003300110110       // -?Bolle partenza?
003400110110     fFNBLP01L  if   e           k disk    extfile(wLibFile)  usropn
003500110110
003600110110       // -?Printer File?
003700110110     fQSYSPRT   o    f  198        printer usropn oflind(*inOF)
003800110110
003900110110       //--------------------------------------------------------------
004000110110       //?Definizione costanti.                                        ?
004100110110       //--------------------------------------------------------------
004200110110
004300110110     d c_OvrPrtF       c                   const('OVRPRTF +
004400110110     d                                            file(QSYSPRT) +
004500110110     d                                            pagesize(66 198 *rowcol) +
004600110110     d                                            lpi(6) cpi(15) +
004700110110     d                                            usrdta(''LoghiElim.'')')
004800110110     d c_DltOvr        c                   const('DLTOVR +
004900110110     d                                            file(QSYSPRT)')
005000110110
005100110110       //--------------------------------------------------------------
005200110110       //?Definizione schiere.                                         ?
005300110110       //--------------------------------------------------------------
005400110110
005500110110       // -?Loghi clienti in packing-list Logistica (vecchio modo)?
005600110110     d Sch_LOGx1       s              4    dim(5)  inz
005700110110     d Sch_LOGx2       s              4    dim(5)  inz
005800110110
005900110110       //--------------------------------------------------------------
006000110110       //?Definizione aree dati.                                       ?
006100110110       //--------------------------------------------------------------
006200110110
006300110110
006400110110       //--------------------------------------------------------------
006500110110       //?Definizione strutture dati.                                  ?
006600110110       //--------------------------------------------------------------
006700110110
006800110110       // -?Status?
006900110110     d Status         sds
007000110110     d  SDSpgm           *proc
007100110110     d  SDSusr               254    263
007200110110
007300110110       // -?Dati estratti via SQL dal file del *cmd DSPOBJD?
007400110110     d wSQLrecC1       ds                  inz
007500110110     d   SQL_LbNm                    10    inz
007600110110     d   SQL_ObNm                    10    inz
007700110110     d   SQL_ObTp                     8    inz
007800110110     d   SQL_ObSz                    10  0 inz
007900110110     d   SQL_ObTx                    50    inz
008000110110     d   SQL_CCen                     6    inz
008100110110     d   SQL_CDat                     6    inz
008200110110     d   SQL_LCen                     6    inz
008300110110     d   SQL_LDat                     6    inz
008400110110     d   SQL_CrtU                    10    inz
008500110110     d   SQL_UUpd                     1    inz
008600110110     d   SQL_UCen                     1    inz
008700110110     d   SQL_UDat                     6    inz
008800110110     d   SQL_UCnt                     5  0 inz
008900110110
009000110110       // -?Dati estratti via SQL dal file FIAR800F?
009100110110     d wSQLrecC2       ds                  inz
009200110110     d   AR8aas                       4  0 inz
009300110110     d   AR8lnp                       3  0 inz
009400110110     d   AR8nrs                       2  0 inz
009500110110     d   AR8nsp                       7  0 inz
009600110110
009700110110       // -?Tab. AR8 = Tipi Modulo DDT x file FIAR8?
009800110110     d dAR8          e ds                  inz
009900110110
010000110110       //--------------------------------------------------------------
010100110110       //?Definizione variabili globali.                               ?
010200110110       //--------------------------------------------------------------
010300110110
010400110110       // -?Indici di schiera?
010500110110     d xx              s              3  0 inz
010600110110
010700110110       // -?Stringhe SQL da eseguire?
010800110110     d wSQL_C1         s           1024    Varying        inz
010900110110     d wSQL_C2         s           1024    Varying        inz
011000110110
011100110110       // -?Nome libreria dei file di filiale?
011200110110     d wLibr           s             10a   inz
011300110110     d wLibFile        s             21a   inz
011400110110
011500110110       // -?*Date in diversi formati?
011600110110     d*// wDate_USA       s               d   datfmt(*usa)  ?(anno di 4!)?
011700110110     d*// wDate_MDY       s               d   datfmt(*mdy)
011800110110     d wDate_EUR       s               d   datfmt(*eur)
011900130219     d wDate_YMD       s               d   datfmt(*ymd)
012000110110
012100110110       // -?Orario?
012200110110     d w_Time          s             10    inz
012300110110
012400110110       // -?Campi di stampa?
012500110110     d wDt_CDat        s              8  0 inz
012600110110     d wDt_LDat        s              8  0 inz
012700110110     d wDt_UDat        s              8  0 inz
012800110110     d wDt_SDat        s              8  0 inz
012900110110     d w_TabAR8        s              2    inz
013000110110     d w_Note          s             30    inz
013100110110     d w_TotLog        s              5  0 inz
013200110110
013300110110       // -?Campi di comodo?
013400110110     d SaveAAS         s                   like(AR8aas)  inz
013500110110     d SaveNRS         s                   like(AR8nrs)  inz
013600110110
013700110110       //--------------------------------------------------------------
013800110110       //?Definizione procedure usate.                                 ?
013900110110       //--------------------------------------------------------------
014000110110
014100110110       // -?Reperimento NETA sistema AS/400 corrente?
014200110110     d currSysNeta     s              8a   inz
014300110110      /copy gaitrasrc/srcProtoPr,UBRTVNETA
014400110110
014500110110       // -?Parametri API QCAPCMD (Process Commands)?
014600110110     d Qcmd            s           1024    inz  varying
014700110110      /copy qSysInc/qRpgleSrc,QCAPCMD
014800110110      /copy gaitrasrc/srcProtoPR,QCAPCMD
014900110110
015000110110       // -?Parametri gestione errori API.?
015100110110      /copy qSysInc/qRpgleSrc,QUSEC
015200110110
015300110110       //--------------------------------------------------------------
015400110110       //?Definizione key-list.                                        ?
015500110110       //--------------------------------------------------------------
015600110110
015700110110       // -?File TNTBE01L?
015800110110     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
015900110110     d                                     prefix(k_)   inz
016000110110
016100110110       // -?File FNBLP01L?
016200110110     d k04fnblp01    e ds                  extname(FNBLP01L : *key)
016300110110     d                                     prefix(k_)   inz
016400110110
016500110110       //--------------------------------------------------------------
016600110110       //?Riepilogo indicatori.                                        ?
016700110110       //--------------------------------------------------------------
016800110110
016900110110
017000110110       //--------------------------------------------------------------
017100110110       //?M A I N - L I N E                                            ?
017200110110       //--------------------------------------------------------------
017300110110
017400110110      /free
017500110110
017600110110       // -?Operazioni iniziali?
017700110110       exsr  sr_RoutInz;
017800110110
017900110110       // -?Estrazione loghi non utilizzati?
018000110110       DoU  sqlCode <> *zero;
018100110110         exsr  sr_ReadCursor;
018200110110       EndDo;
018300110110
018400110110       // -?Operazioni finali?
018500110110       exsr  sr_RoutEnd;
018600110110
018700110110       //--------------------------------------------------------------
018800110110       //?Operazioni iniziali.                                         ?
018900110110       //--------------------------------------------------------------
019000110110       BEGSR sr_RoutInz;
019100110110
019200110110         *inLR = *on;
019300110110
019400110110         // -?Impostazione opzioni per SQL?
019500110110         exec SQL   set option   DynUsrPrf = *owner,
019600110110                                 CloSqlCsr = *endmod;
019700130219
019800130219         // -?Impostazione data limite dell'ultimo utilizzo?
019900130219         wDate_YMD = %date() - %months(12);
020000110110
020100110110         // -?Verifica del sistema AS/400 corrente?
020200110110         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
020300130219           return;
020400110110         endif;
020500110110
020600110110         // -?Impostazione libreria archivi di filiale?
020700110110         if  %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
020800110110           wLibr = 'FILTRA201 ';
020900110110         else;
021000110110           wLibr = 'FILTRAPRD ';
021100110110         endif;
021200110110
021300110110         // -?Apertura archivi di filiale?
021400110110         wLibFile = %trimr(wLibr) + '/FNBLP01L';
021500110110         open  FNBLP01L;
021600110110
021700110110         // -?Intabellam. vecchi loghi clienti associati alla Logistica?
021800110110         k_TBEcod = 'AR8';
021900110110         setll  %kds( k05tntbe01 )  TNTBE000;
022000110110         reade  %kds( k05tntbe01 : 1 )  TNTBE000;
022100110110         DoW  Not %eof(TNTBE01L);
022200110110           if  TBEatb = *blank;
022300110110             dAR8 = TBEuni;
022400110110             if  §AR8logoF <> *blank;
022500110110               xx += 1;
022600110110               sch_LOGx1(xx) = §AR8logoF;
022700110110               sch_LOGx2(xx) = TBEke1;
022800110110             endif;
022900110110           endif;
023000110110           reade  %kds( k05tntbe01 : 1 )  TNTBE000;
023100110110         EndDo;
023200110110
023300110110         // -?Creazione work-file in libr. QTEMP?
023400110110         exsr  sr_CrtWFps;
023500110110
023600110110         // -?Creazione stringa SQL da eseguire?
023700110110         wSQL_C1 = 'select odLbNm, odObNm, odObTp, odObSz, odObTx, +
023800110110                           odCCen, odCDat, odLCen, odLDat, +
023900110110                           odCrtU, odUUpd, odUCen, odUDat, odUCnt +
024000110110                      from QTEMP/WFPAGSEG +
024100110110                     where odUDat = '' '' +
024200130219         //             or odUCnt <= 3 +
024300130219                        or odUDat < ''' +
024400130219                                    %editc( %dec(wDate_YMD) : 'X' ) + ''' +
024500110113                     order by odObNm, odObTx';
024600110110
024700110110         // -?Apertura cursore?
024800110110         exsr  sr_OpenCursor;
024900110110
025000110110       ENDSR;
025100110110
025200110110       //--------------------------------------------------------------
025300110110       //?Creazione work-file con elenco loghi (*PAGSEG).              ?
025400110110       //--------------------------------------------------------------
025500110110       BEGSR sr_CrtWFps;
025600110110
025700110110         // -?Cancellazione (preventiva) work-file già esistente?
025800110110         Qcmd = 'DLTF file(QTEMP/WFPAGSEG)';
025900110110         exsr  sr_ExecCmd;
026000110110
026100110110         // -?Creazione work-file con elenco loghi?
026200110110         Qcmd = 'DSPOBJD obj(GAITRAOBJ/LOGO*) +
026300110110                         objtype(*PAGSEG) +
026400110110                         output(*outfile) +
026500110110                         outfile(QTEMP/WFPAGSEG) +
026600110110                         outmbr(*first *replace)';
026700110110         exsr  sr_ExecCmd;
026800110110         if  Qusei <> *blank;
026900110110           exsr  sr_PrintErr;
027000110110         endif;
027100110110
027200110110       ENDSR;
027300110110
027400110110       //--------------------------------------------------------------
027500110110       //?Esecuzione del comando (già impostato).                      ?
027600110110       //--------------------------------------------------------------
027700110110       BEGSR  sr_ExecCmd;
027800110110
027900110110         clear Qcap0100;
028000110110         Qcabcsdh = *off;
028100110110         Qcapa    = *off;
028200110110         Qcacmdss = *off;
028300110110         Qcaerved = *allX'00';
028400110110
028500110110         clear Qusec;
028600110110         Qusbprv  = %size(Qusec);
028700110110
028800110110         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
028900110110                           %size(Qcap0100) : 'CPOP0100' : *omit :
029000110110                           0 : 0 : Qusec);
029100110110
029200110110         //if  Qusei <> *blank;
029300110110         //  ...;
029400110110         //endif;
029500110110
029600110110       ENDSR;
029700110110
029800110110       //--------------------------------------------------------------
029900110110       //?Apertura cursore C1.                                         ?
030000110110       //--------------------------------------------------------------
030100110110       BEGSR  sr_OpenCursor;
030200110110
030300110110         // -?Dichiarazione del cursore?
030400110110         exec sql  prepare S1  from :wSQL_C1;
030500110110         exec sql  declare C1  cursor for S1;
030600110110
030700110110         // -?Apertura del cursore?
030800110110         exec sql   open C1;
030900110110
031000110110         if  SQLcode < *zero;
031100110110           exsr  sr_PrintErr;
031200110110         endif;
031300110110
031400110110       ENDSR;
031500110110
031600110110       //--------------------------------------------------------------
031700110110       //?Lettura cursore C1.                                          ?
031800110110       //--------------------------------------------------------------
031900110110       BEGSR  sr_ReadCursor;
032000110110
032100110110         clear  wSQLrecC1;
032200110110
032300110110         exec SQL  fetch next  from C1  into :wSQLrecC1;
032400110110
032500110110         select;
032600110110           // -?Fine lettura?
032700110110           when  sqlCode = 100;
032800110110             exsr  sr_RoutEnd;
032900110110           // -?Errore?
033000110110           when  sqlCode < *zero;
033100110110             exsr  sr_PrintErr;
033200110110           // -?Elaborazione?
033300110110           other;
033400110110             exsr  sr_ElabRec;
033500110110         endsl;
033600110110
033700110110       ENDSR;
033800110110
033900110110       //--------------------------------------------------------------
034000110110       //?Chiusura cursore C1.                                         ?
034100110110       //--------------------------------------------------------------
034200110110       BEGSR  sr_CloseCursor;
034300110110
034400110110         // -?Chiusura del cursore?
034500110110         exec sql   close C1;
034600110110
034700110110       ENDSR;
034800110110
034900110110       //--------------------------------------------------------------
035000110110       //?Elaborazione singolo record estratto.                        ?
035100110110       //--------------------------------------------------------------
035200110110       BEGSR  sr_ElabRec;
035300110110
035400110110         // -?Pulizia iniziale dei campi in stampa?
035500110110         clear  wDt_CDat;
035600110110         clear  wDt_LDat;
035700110110         clear  wDt_UDat;
035800110110         clear  wDt_SDat;
035900110110         clear  w_TabAR8;
036000110110         clear  w_Note;
036100110110
036200110110         // -?Verifica ultimo utilizzo:?
036300110110         //  ?da segnalare se NON richiesto negli ultimi?12?mesi?
036400110110         if  SQL_UDat > *zero    and
036500110110             %diff( %date() :
036600110110                    %date( %int( SQL_UDat ) : *mdy ) :
036700110110                    *months ) <= 12;
036800110110           leavesr;
036900110110         endif;
037000110110
037100110110         // -?Reperimento tabella "AR8"?
037200110110         clear  dAR8;
037300110110         k_TBEke1 = %subst( SQL_ObNm : 5 );
037400110110         chain  %kds( k05tntbe01 )  TNTBE000;
037500110110         if  %found(TNTBE01L)  and  TBEatb = *blank;
037600110110           dAR8 = TBEuni;
037700110110           SQL_ObTx = §AR8des;
037800110110         else;
037900110110           xx = %lookup( %subst( SQL_ObNm : 5 : 4 ) : sch_LOGx1 );
038000110110           if  xx = *zero;
038100110110             w_TabAR8 = 'NO';
038200110110           else;
038300110110             k_TBEke1 = sch_LOGx2(xx);
038400110110             chain  %kds( k05tntbe01 )  TNTBE000;
038500110110             if  %found(TNTBE01L)  and  TBEatb = *blank;
038600110110               dAR8 = TBEuni;
038700110110               w_Note = 'Vedi ' + sch_LOGx2(xx) + ': ' + §AR8des;
038800110110             else;
038900110110               w_TabAR8 = 'NO';
039000110110             endif;
039100110110           endif;
039200110110         endif;
039300110110
039400110110         // -?Ricerca ultima packing-list in FIAR800F con il logo?
039500110110         exsr  sr_ChkFIAR8;
039600110110
039700110110         // -?Stampa segnalazione logo?
039800110110         exsr  sr_Stampa;
039900110110
040000110110       ENDSR;
040100110110
040200110110       //--------------------------------------------------------------
040300110110       //?Verifica esistenza packing-list con il logo in esame.        ?
040400110110       //--------------------------------------------------------------
040500110110       BEGSR  sr_ChkFIAR8;
040600110110
040700110110         reset  SaveAAS;
040800110110         reset  SaveNRS;
040900110110
041000110110         // -?Creazione stringa SQL da eseguire?
041100110110         wSQL_C2 = 'select AR8aas, AR8lnp, AR8nrs, max(AR8nsp) +
041200110110                      from ' + %trimr(wLibr) + '/FIAR801L +
041300110110                     where AR8tmd = ''' + %trimr(k_TBEke1) + ''' +
041400110110                     group by AR8aas, AR8lnp, AR8nrs +
041500110110                     order by AR8aas desc, AR8lnp, AR8nrs +
041600110110                       for fetch only';
041700110110
041800110110         // -?Dichiarazione del cursore?
041900110110         exec sql  prepare S2  from :wSQL_C2;
042000110110         exec sql  declare C2  cursor for S2;
042100110110
042200110110         // -?Apertura del cursore?
042300110110         exec sql  open C2;
042400110110
042500110110         if  SQLcode < *zero;
042600110110           exsr  sr_PrintErr;
042700110110         endif;
042800110110
042900110110         // -?Estrazione dati?
043000110110         DoU  sqlCode = 100;
043100110110
043200110110           clear  wSQLrecC2;
043300110110           exec SQL  fetch next  from C2  into :wSQLrecC2;
043400110110
043500110110           select;
043600110110             // -?Fine lettura?
043700110110             when  sqlCode = 100;
043800110110               leave;
043900110110             // -?Errore?
044000110110             when  sqlCode < *zero;
044100110110               exsr  sr_PrintErr;
044200110110             // -?Elaborazione: memorizzazione data ultima spedizione?
044300110110             //  ?(in formato aaaa/mm/gg)?
044400110110             other;
044500110110               if  AR8aas > SaveAAS  or
044600110110                  (AR8aas = SaveAAS  and  AR8nrs <> SaveNRS);
044700110110                 if  AR8aas > SaveAAS;
044800110110                   SaveAAS = AR8aas;
044900110110                 endif;
045000110110                 if  AR8nrs <> SaveNRS;
045100110110                   SaveAAS = AR8aas;
045200110110                 endif;
045300110110                 chain  %kds(k04fnblp01)  FNBLP000;
045400110110                 if  %found(FNBLP01L)  and
045500110110                     %dec( %date( (BLPaas * 10000) + BLPmgs : *iso ) )
045600110110                          > wDt_SDat;
045700110110                   wDt_SDat = %dec( %date( (BLPaas * 10000) + BLPmgs :
045800110110                                           *iso ) );
045900110110                 endif;
046000110110               endif;
046100110110           endsl;
046200110110
046300110110         EndDo;
046400110110
046500110110         // -?Chiusura del cursore?
046600110110         exec sql  close C2;
046700110110
046800110110       ENDSR;
046900110110
047000110110       //--------------------------------------------------------------
047100110110       //?Stampa segnalazione segmento di pagina.                      ?
047200110110       //--------------------------------------------------------------
047300110110       BEGSR  sr_Stampa;
047400110110
047500110110         // -?Conversione data creazione?
047600110110         if  SQL_CDat > *zero;
047700110110           wDate_Eur = %date( %int( SQL_CDat ) : *mdy );
047800110110           wDt_CDat  = %dec( wDate_Eur );
047900110110         endif;
048000110110
048100110110         // -?Conversione data modifica?
048200110110         if  SQL_LDat > *zero;
048300110110           wDate_Eur = %date( %int( SQL_LDat ) : *mdy );
048400110110           wDt_LDat  = %dec( wDate_Eur );
048500110110         endif;
048600110110
048700110110         // -?Conversione data ultimo utilizzo?
048800110110         if  SQL_UDat > *zero;
048900110110           wDate_Eur = %date( %int( SQL_UDat ) : *mdy );
049000110110           wDt_UDat  = %dec( wDate_Eur );
049100110110         endif;
049200110110
049300110110         // -?Conversione data ultima LdV con packing-list?
049400110110         if  wDt_SDat > *zero;
049500110110           wDate_Eur = %date( wDt_SDat : *iso );
049600110110           wDt_SDat  = %dec( wDate_Eur );
049700110110         endif;
049800110110
049900110110         // -?Test OverFlow e stampa testata?
050000110110         exsr  sr_TestOF;
050100110110
050200110110         // -?Stampa riga di dettaglio?
050300110110         except  PRTdet;
050400110110
050500110110         // -?Incremento n° totale loghi?
050600110110         w_TotLog += 1;
050700110110
050800110110       ENDSR;
050900110110
051000110110       //--------------------------------------------------------------
051100110110       //?Test OverFlow in stampa.                                     ?
051200110110       //--------------------------------------------------------------
051300110110       BEGSR  sr_TestOF;
051400110110
051500110110         // -?Apertura file di stampa?
051600110110         if  not %open (QSYSPRT);
051700110110           Qcmd = c_OvrPrtF;
051800110110           exsr  sr_ExecCmd;
051900110110           open  QSYSPRT;
052000110110           *inOF = *on;
052100110110         endif;
052200110110
052300110110         // -?Test OverFlow e stampa testata?
052400110110         if  *inOF;
052500110110           w_Time = %char( %time() );
052600110110           except PRTtxt;
052700110110           *inOF  = *off;
052800110110         endif;
052900110110
053000110110       ENDSR;
053100110110
053200110110       //--------------------------------------------------------------
053300110110       //?Stampa segnalazione dell'errore rilevato.                    ?
053400110110       //--------------------------------------------------------------
053500110110       BEGSR  sr_PrintErr;
053600110110
053700110110         // -?Stampa del Dump?
053800110110         Dump(A);
053900110110
054000110110         // -?Stampa del Job-Log?
054100110110         Qcmd = 'DSPJOBLOG job(*) output(*print)';
054200110110         exsr  sr_ExecCmd;
054300110110
054400110110         // -?Chiusura del programma?
054500110110         exsr  sr_RoutEnd;
054600110110
054700110110       ENDSR;
054800110110
054900110110       //--------------------------------------------------------------
055000110110       //?Operazioni finali.                                           ?
055100110110       //--------------------------------------------------------------
055200110110       BEGSR  sr_RoutEnd;
055300110110
055400110110         // -?Chiusura cursore?
055500110110         Qcmd = c_DltOvr;
055600110110         exsr  sr_ExecCmd;
055700110110         exsr  sr_CloseCursor;
055800110110
055900110110         // -?Stampa "fine-lista"?
056000110110         exsr  sr_TestOF;
056100110110         except  PRTend;
056200110110
056300110110         // -?Chiusura printer-file?
056400110110         close  QSYSPRT;
056500110110
056600110110         // -?Uscita?
056700110110         return;
056800110110
056900110110       ENDSR;
057000110110
057100110110      /end-free
057200110110
057300110110       //--------------------------------------------------------------
057400110110       //?P R I N T E R - F I L E S                                    ?
057500110110       //--------------------------------------------------------------
057600110110
057700110110     oQSYSPRT   e            PRTtxt            2
057800110110     o                                              'ELENCO SEGMENTI D-
057900110110     o                                              I PAGINA ELIMINABI-
058000110110     o                                              LI'
058100110110     o                       SDSpgm           +   5
058200110110     o                       SDSusr           +   5
058300110110     o                       CurrSysNetA      +   5
058400110110     o                       *date         y  +   5
058500110110     o                       w_Time           +   5
058600110113     o                                        +   5 'Pag.'
058700110113     o                       Page          z  +   0
058800110110     o          e            PRTtxt      0  0
058900110110     o                                              'ELENCO SEGMENTI D-
059000110110     o                                              I PAGINA ELIMINABI-
059100110110     o                                              LI'
059200110110     o          e            PRTtxt      2
059300110110     o                                              'Logo    '
059400110110     o                                        +   2 'Descrizione'
059500110110     o                                        +  42 'Dimensione'
059600110110     o                                        +   2 'Utente    '
059700110110     o                                        +   2 'Dta Creaz.'
059800110110     o                                        +   2 'Dta Modif.'
059900110113     o                                        +   2 'DtaUltUtil'
060000110113     o                                        +   2 'DtaUltSped'
060100110110     o                                        +   2 'Tab.'
060200110110     o                                        +   2 'Note'
060300110110     o          e            PRTtxt      0  0
060400110110     o                                              'Logo    '
060500110110     o                                        +   2 'Descrizione'
060600110110     o                                        +  42 'Dimensione'
060700110110     o                                        +   2 'Utente    '
060800110110     o                                        +   2 'Dta Creaz.'
060900110110     o                                        +   2 'Dta Modif.'
061000110113     o                                        +   2 'DtaUltUtil'
061100110113     o                                        +   2 'DtaUltSped'
061200110110     o                                        +   2 'Tab.'
061300110110     o                                        +   2 'Note'
061400110110     o          e            PRTtxt      1
061500110110     o                                              '========'
061600110113     o                                        +   2 '===========+
061700110113     o                                               ===============+
061800110113     o                                               ===============+
061900110113     o                                               ========='
062000110113     o                                        +   2 '==========='
062100110110     o                                        +   2 '=========='
062200110110     o                                        +   2 '=========='
062300110110     o                                        +   2 '=========='
062400110110     o                                        +   2 '=========='
062500110110     o                                        +   2 '=========='
062600110110     o                                        +   2 '===='
062700110113     o                                        +   2 '====+
062800110113     o                                               ===============+
062900110113     o                                               ==========='
063000110110     o          e            PRTtxt      0  1
063100110110     o                                              '========'
063200110113     o                                        +   2 '===========+
063300110113     o                                               ===============+
063400110113     o                                               ===============+
063500110113     o                                               ========='
063600110113     o                                        +   2 '==========='
063700110110     o                                        +   2 '=========='
063800110110     o                                        +   2 '=========='
063900110110     o                                        +   2 '=========='
064000110110     o                                        +   2 '=========='
064100110110     o                                        +   2 '=========='
064200110110     o                                        +   2 '===='
064300110113     o                                        +   2 '====+
064400110113     o                                               ===============+
064500110113     o                                               ==========='
064600110110      *
064700110110     o          e            PRTdet      1
064800110110     o                       SQL_ObNm
064900110110     o                       SQL_ObTx         +   0
065000110110     o                       SQL_ObSz      1  +   0
065100110110     o                       SQL_CrtU         +   2
065200110110     o                       wDt_CDat         +   2 '  /  /  0 '
065300110110     o                       wDt_LDat         +   2 '  /  /  0 '
065400110110     o                       wDt_UDat         +   2 '  /  /  0 '
065500110110     o                       wDt_SDat         +   2 '  /  /  0 '
065600110110     o                       w_TabAR8         +   3
065700110110     o                       w_Note           +   3
065800110110      *
065900110110     o          e            PRTend      2
066000110110     o                                        +  10 '***  Fine Lista  +
066100110110     o                                               ***'
066200110110     o                                        +   3 'Tot. loghi = '
066300110110     o                       w_TotLog      k  +   1
066400110110     o          e            PRTend      0  0
066500110110     o                                        +  10 '***  Fine Lista  +
066600110110     o                                               ***'

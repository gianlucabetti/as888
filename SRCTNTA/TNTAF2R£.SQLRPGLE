000100120529       //==============================================================
000200120529       //?Estrazione Clienti messi in Sofferenza dalla Sede            ?
000300120529       //==============================================================
000400120529
000500120529       //--------------------------------------------------------------
000600120529       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700120529       //--------------------------------------------------------------
000800120529
000900120529     /*PRM dbgview(*source)
001000120529     /*PRM commit(*none)
001100120529     /*END
001200120529
001300120529       //--------------------------------------------------------------
001400120529       //?Specifiche di controllo.                                     ?
001500120529       //--------------------------------------------------------------
001600120529
001700120529     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001800120529     h dftactgrp(*no)
001900120529     h alwnull(*inputonly)
002000120529
002100120529       //--------------------------------------------------------------
002200120529       //?Dichiarazione file.                                          ?
002300120529       //--------------------------------------------------------------
002400120529
002500120529       // -?Work-File con dati estratti?
002600120529     fWFCSS00F  o    e             disk    usropn
002700120529
002800120529       //--------------------------------------------------------------
002900120529       //?Definizione costanti.                                        ?
003000120529       //--------------------------------------------------------------
003100120529
003200120529
003300120529       //--------------------------------------------------------------
003400120529       //?Definizione schiere.                                         ?
003500120529       //--------------------------------------------------------------
003600120529
003700120529
003800120529       //--------------------------------------------------------------
003900120529       //?Definizione aree dati.                                       ?
004000120529       //--------------------------------------------------------------
004100120529
004200120529       // -?Dati utente?
004300120529     d §AzUte        e ds                  extname(AZUTE00F)
004400120529     d                                     dtaara
004500120529     d §DatiUte      e ds                  extname(dDatiUte)
004600120529     d                                     dtaara
004700120529
004800120529       //--------------------------------------------------------------
004900120529       //?Definizione strutture dati.                                  ?
005000120529       //--------------------------------------------------------------
005100120529
005200120529       // -?Status ds?
005300120529     d Status         sds           333
005400120529     d   SDSpgm          *proc
005500120529
005600120529       // -?Parametri ricevuti?
005700120529     d KPJBA         e ds
005800120529     d TNTAF2ds      e ds                  inz
005900120529
006000120529       // -?Campi modificanti di CNACVD0F - rec. BLOCCO?
006100120529     d*// dACV_B        e ds                  inz
006200120529
006300120529       // -?Causali "Stato del Credito" da elaborare?
006400120529     d ds_CON          ds                  inz
006500120529     d   sk_Con                            inz   like(iF2con1)
006600120529     d                                           dim(10)
006700120529
006800120529       // -?Dati estratti via SQL?
006900120529     d CNACO00F      e ds                  based(nullPtr)
007000120529     d CNCLP00F      e ds                  based(nullPtr)
007100120529     d CNIND00F      e ds                  based(nullPtr)
007200120529     d CNACVD0F      e ds                  based(nullPtr)
007300120529     d ds_SQL          ds                  inz
007400120529     d  sql_KSC                            inz   like(ACOksc)
007500120529     d  sql_RSC                            inz   like(ACOrag)
007600120604     d*//  sql_ITC                            inz   like(ACOitc)
007700120529     d  sql_KSC_clp                        inz   like(CLPksc)
007800120529     d  sql_CON                            inz   like(CLPcon)
007900120604     d  sql_CLV                            inz   like(CLPclv)
008000120529     d  sql_KSC_acv                        inz   like(ACVksc)
008100120529     d  sql_DTS                            inz   like(ACVdav)
008200120529     d  sql_YDA                            inz   like(ACVyda)
008300120529     d  sql_PRIMA                          inz   like(ACVprima)
008400120529     d  sql_DOPO                           inz   like(ACVdopo)
008500120529     d  sql_KSC_ind                        inz   like(INDksc)
008600120529     d  sql_IND                            inz   like(INDvia)
008700120529     d  sql_LOC                            inz   like(INDcit)
008800120529     d  sql_CAP                            inz   like(INDcae)
008900120529     d  sql_PRV                            inz   like(INDprv)
009000120529     d  sql_NAZ                            inz   like(INDsta)
009100120529
009200120529       //--------------------------------------------------------------
009300120529       //?Definizione variabili globali.                               ?
009400120529       //--------------------------------------------------------------
009500120529
009600120529       // -?Flags booleani?
009700120529     d $Fine           s               n   inz
009800120529
009900120529       // -?Indici di schiera?
010000120529     d xx              s              5  0 inz
010100120529
010200120529       // -?Elenco causali "Stato del credito" da elaborare via SQL?
010300120529     d wCon            s             58    inz  varying
010400120529
010500120529       // -?Stringa SQL da eseguire?
010600120529     d wSql            s           4096    inz  varying
010700120529     d ds_Temp         ds          4200    inz
010800120529     d   $Temp                       60    inz  dim(70)
010900120529
011000120529       // -?Campi data?
011100120529     d wDate           s              8  0 inz
011200120529
011300120529       //--------------------------------------------------------------
011400120529       //?Definizione prototipi procedure.                             ?
011500120529       //--------------------------------------------------------------
011600120529
011700120529       // -?Reperimento dati utente?
011800120529     d TIBS34ds      e ds                  inz
011900120529      /copy gaitrasrc/srcProtoPR,TIBS34R
012000120529
012100120529       // -?Parametri API QCAPCMD (Process Commands)?
012200120529     d Qcmd            s            512    inz  varying
012300120529      /copy qSysInc/qRpgleSrc,QCAPCMD
012400120529       // -?API QCAPCMD (Process Commands)?
012500120529      /copy gaitrasrc/srcProtoPR,QCAPCMD
012600120529
012700120529       // -?Parametri gestione errori API.?
012800120529      /copy qsysinc/qrpglesrc,QUSEC
012900120529
013000120529       //--------------------------------------------------------------
013100120529       //?Definizione key-list.                                        ?
013200120529       //--------------------------------------------------------------
013300120529
013400120529
013500120529       //--------------------------------------------------------------
013600120529       //?M A I N - L I N E                                            ?
013700120529       //--------------------------------------------------------------
013800120529
013900120529     c     *Entry        plist
014000120529     c                   parm                    KPJBA
014100120529
014200120529      /free
014300120529
014400120529       // -?Operazioni iniziali?
014500120529       exsr sr_RoutInz;
014600120529
014700120529       // -?Ciclo di lettura tab. "3C"?
014800120529       DoU  $Fine;
014900120529         exsr  sr_ExecSql;
015000120529       EndDo;
015100120529
015200120529       // -?Operazioni finali?
015300120529       exsr sr_RoutEnd;
015400120529
015500120529       //--------------------------------------------------------------
015600120529       //?Operazioni iniziali.                                         ?
015700120529       //--------------------------------------------------------------
015800120529       BEGSR  sr_RoutInz;
015900120529
016000120529         *inLR = *on;
016100120529
016200120529         // -?Impostazione opzioni per SQL?
016300120529         exec sql  set  option  DynUsrPrf = *Owner,
016400120529                                CloSqlCsr = *EndMod;
016500120529
016600120529         // -?Impostazione parametri ricevuti?
016700120529         TNTAF2ds = kpjbu;
016800120529
016900120529         // -?Reperimento data odierna?
017000120529         wDate = %int( %subst( %char( %dec( %timestamp() ) )
017100120529                               : 1 : 8 ) );
017200120529
017300120529         // -?Reperimento dati job?
017400120529         exsr  sr_DatiJob;
017500120529
017600120529         // -?Pulizia ed apertura WrkF?
017700120529         Qcmd = 'CLRPFM file(*libl/WFCSS00F)';
017800120529         exsr  sr_ExecCmd;
017900120529         open  WFCSS00F;
018000120529
018100120529         // -?Intabellam. causali "Stato del Credito"?
018200120529         ds_CON = %subst( TNTAF2ds : 11 );
018300120529         // -?Preparazione elenco causali "Stato del Credito" da?
018400120529         //  ?elaborare via SQL?
018500120529         For  xx = 1  To  %elem(sk_Con);
018600120529           if  sk_Con(xx) <> *blank;
018700120529             if  wCon <> *blank;
018800120529               wCon += ', ';
018900120529             endif;
019000120530             wCon += '''' + sk_Con(xx) + '''';
019100120529           endif;
019200120529         EndFor;
019300120529
019400120529         // -?Compilazione stringa SQL?
019500120529         wSql = 'with +
019600120529
019700120604                  CNACO as (select ACOksc, ACOrag +
019800120529                              from CNACO00F +
019900120529                             where ACOkut = 1 +
020000120529                               and ACOkcc = ' +
020100120529                                   %trim( %editc( DUTkci : '3' ) ) +
020200120529                             ' and ACOflg = '' '' +
020300120529                             order by ACOksc), +
020400120529
020500120604                  CNCLP as (select CLPksc, CLPcon, CLPclv +
020600120529                              from CNCLP00F +
020700120529                             where CLPkut = 1 +
020800120529                               and CLPkcc = ' +
020900120529                                   %trim( %editc( DUTkci : '3' ) ) +
021000120529                             ' and CLPflg = '' '' +
021100120530                               and substr(CLPcon, 2, 2) in (' +
021200120529                                   wCon + ') +
021300120529                             order by CLPksc), +
021400120529
021500120529                  CNIND as (select INDksc, INDvia, INDcit, INDcae, +
021600120529                                   INDprv, INDsta +
021700120529                              from CNIND00F +
021800120529                             where INDkut = 1 +
021900120529                               and INDkcc = ' +
022000120529                                   %trim( %editc( DUTkci : '3' ) ) +
022100120529                             ' and INDflg = '' '' +
022200120529                             order by INDksc), +
022300120529
022400120529                 CNACVD as (select ACVksc, ACVdav, ACVyda, +
022500120529                                   ACVprima, ACVdopo +
022600120529                              from CNACVD0F +
022700120529                             where ACVkcc = ' +
022800120529                                   %trim( %editc( DUTkci : '3' ) ) +
022900120529                             ' and ACVdav between ' +
023000120529                                   %trim( %editc( iF2dti : '3' ) ) +
023100120529                                   ' and ' +
023200120529                                   %trim( %editc( iF2dtf : '3' ) ) +
023300120529                             ' and ACVatb = '' '' +
023400120529                               and ACVyda = ''B'' +
023500120529                               and substr(ACVprima, 1, 2) <> +
023600120529                                   substr(ACVdopo, 1, 2) +
023700120529                               and substr(ACVdopo, 1, 2) in (' +
023800120529                                   wCon + ') +
023900120529                             order by ACVksc, ACVdav desc)';
024000120529
024100120529         wSql += ' select * +
024200120529
024300120529                     from CNACO inner join CNCLP +
024400120529                       on CNACO.ACOksc = CNCLP.CLPksc +
024500120529
024600120529                                inner join CNACVD +
024700120529                       on CNACO.ACOksc = CNACVD.ACVksc +
024800120529                      and substr(CNCLP.CLPcon, 2, 2) = +
024900120529                          substr(CNACVD.ACVdopo, 1, 2) +
025000120529
025100120529                                inner join CNIND +
025200120529                       on CNACO.ACOksc = CNIND.INDksc +
025300120529
025400120529                    order by CNACO.ACOksc +
025500120529
025600120529                      for fetch only';
025700120529
025800120529         ds_Temp = wSql;
025900120529
026000120529         // -?Start SQL?
026100120529         exec sql   prepare S1   from :wSql;
026200120529         exec sql   declare C1   cursor for S1;
026300120529         exec sql   open C1;
026400120529
026500120529       ENDSR;
026600120529
026700120529       //--------------------------------------------------------------
026800120529       //?Reperimento Dati del job (Utente/Operativi).                 ?
026900120529       //--------------------------------------------------------------
027000120529       BEGSR  sr_DatiJob;
027100120529
027200120529         in(e) §AzUte;
027300120529         if NOT %error;
027400120529           in(e) §DatiUte;
027500120529         endif;
027600120529         if %error or RSut = *blank;
027700120529           tibs34r ( tibs34ds );
027800120529           in §AzUte;
027900120529           in §DatiUte;
028000120529         endif;
028100120529
028200120529       ENDSR;
028300120529
028400120529       //--------------------------------------------------------------
028500120529       //?Esecuzione singola lettura via SQL dalla tab. "3C".          ?
028600120529       //--------------------------------------------------------------
028700120529       BEGSR  sr_ExecSql;
028800120529
028900120529         clear  ds_SQL;
029000120529
029100120529         exec sql   fetch next   from C1   into :ds_SQL;
029200120529
029300120529         select;
029400120529           when  SQLcode = 100;
029500120529             $Fine = *on;
029600120529           when  SQLcode < *zero;
029700120529             exsr  sr_PrintErr;
029800120529             $Fine  = *on;
029900120529           other;
030000120529             exsr  sr_ElabCSS;
030100120529         endsl;
030200120529
030300120529       ENDSR;
030400120529
030500120529       //--------------------------------------------------------------
030600120529       //?Elaborazione singolo cliente.                                ?
030700120529       //--------------------------------------------------------------
030800120529       BEGSR  sr_ElabCSS;
030900120529
031000120529         // -?Verifica se già registrato un rec. (con data più recente)?
031100120529         //  ?per il cliente in esame?
031200120529         //  ?(potrebbe già essere stato elaborato un record più recen-?
031300120529         //  ?te per lo stesso cliente: la lettura di CNACVD0F prevede?
031400120529         //  ?la data variazione in ordine DESC - per cliente)?
031500120529         if  sql_KSC = WCSksc;
031600120529           leavesr;
031700120529         endif;
031800120529
031900120529         // -?Impostazione dati del nuovo cliente?
032000120529         clear  WFCSS000;
032100120529
032200120529         WCSpru = knmus;
032300120529         WCSdte = wDate;
032400120529         WCSdti = iF2dti;
032500120529         WCSdtf = iF2dtf;
032600120529         WCSdts = sql_DTS;
032700120604         //WCSitc = %int( %subst( %editc( sql_ITC : 'X' ) : 7 ) );
032800120604         WCSclv = sql_CLV;
032900120529         WCSksc = sql_KSC;
033000120529         WCSrsc = sql_RSC;
033100120529         WCSind = sql_IND;
033200120529         WCSloc = sql_LOC;
033300120529         WCScap = sql_CAP;
033400120529         WCSprv = sql_PRV;
033500120529         WCSnaz = sql_NAZ;
033600120529         WCScon = sql_CON;
033700120529
033800120529         // -?Scrittura record nel work-file?
033900120529         //______________
034000120529         write  WFCSS000;
034100120529         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
034200120529
034300120529       ENDSR;
034400120529
034500120529       //--------------------------------------------------------------
034600120529       //?Esecuzione del comando (già impostato).                      ?
034700120529       //--------------------------------------------------------------
034800120529       BEGSR  sr_ExecCmd;
034900120529
035000120529         clear Qcap0100;
035100120529         Qcabcsdh = *off;
035200120529         Qcapa    = *off;
035300120529         Qcacmdss = *off;
035400120529         Qcaerved = *allX'00';
035500120529
035600120529         clear Qusec;
035700120529         Qusbprv  = %size(Qusec);
035800120529
035900120529         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
036000120529                           %size(Qcap0100) : 'CPOP0100' : *omit :
036100120529                           0 : 0 : Qusec);
036200120529
036300120529         if  Qusei <> *blank;
036400120529           exsr  sr_PrintErr;
036500120529           exsr  sr_RoutEnd;
036600120529         endif;
036700120529
036800120529       ENDSR;
036900120529
037000120529       //--------------------------------------------------------------
037100120529       //?Stampa segnalazioni dell'errore rilevato.                    ?
037200120529       //--------------------------------------------------------------
037300120529       BEGSR  sr_PrintErr;
037400120529
037500120529         // -?Stampa Dump?
037600120529         Dump(A);
037700120529
037800120529         // -?Stampa Job-Log?
037900120529         Qcmd = 'DSPJOBLOG job(*) output(*print)';
038000120529         clear Qcap0100;
038100120529         Qcabcsdh = *off;
038200120529         Qcapa    = *off;
038300120529         Qcacmdss = *off;
038400120529         Qcaerved = *allX'00';
038500120529         clear Qusec;
038600120529         Qusbprv  = %size(Qusec);
038700120529         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
038800120529                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
038900120529                           0 : 0 : Qusec);
039000120529
039100120529       ENDSR;
039200120529
039300120529       //--------------------------------------------------------------
039400120529       //?Operazioni finali.                                           ?
039500120529       //--------------------------------------------------------------
039600120529       BEGSR  sr_RoutEnd;
039700120529
039800120529         // -?End SQL?
039900120529         exec SQL   close C1;
040000120529
040100120529         // -?Uscita dal *pgm?
040200120529         return;
040300120529
040400120529       ENDSR;
040500120529
040600120529      /end-free

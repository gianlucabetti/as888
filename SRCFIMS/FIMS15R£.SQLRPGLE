000100170522       //==============================================================
000200170524       //?FIMS15R - Statistica Clienti con Merce Incompatibile: crea   ?
000300170524       //?          *file FIMIS00F: Merce Incompatibile - Spedizioni   ?
000400170524       //?          - batch -                                          ?
000500170522       //==============================================================
000600170522
000700170522       //--------------------------------------------------------------
000800170522       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000900170522       //--------------------------------------------------------------
001000170522
001100170609     /*PRM  dbgview(*source)
001200170609     /*CMD  ovrdbf file(FIPSL00F) tofile(FILTRAPRD/FIPSL00F) +
001300170609     /*CMD         ovrscope(*calllvl)
001400170609     /*CMD  ovrdbf file(FLTR700F) tofile(FILTRAPRD/FLTR700F) +
001500170609     /*CMD         ovrscope(*calllvl)
001600170609     /*CMD  ovrdbf file(FIART27L) tofile(FILTRAPRD/FIART27L) +
001700170609     /*CMD         ovrscope(*calllvl)
001800170609     /****  ovrdbf file(FNARB01L) tofile(FILTRAPRD/FNARB01L) +
001900170609     /****         ovrscope(*calllvl)
002000170609     /*CMD  ovrdbf file(TITAS30C) tofile(GAITRAGRPS/TITAS30C) +
002100170609     /*CMD         ovrscope(*calllvl)
002200170609     /*END  dltovr file(*all) lvl(*)
002300170522     /*END
002400170522
002500170522       //--------------------------------------------------------------
002600170522       //?Specifiche di controllo.                                     ?
002700170522       //--------------------------------------------------------------
002800170522
002900170522     h decedit('0,') datedit(*dmy/) option(*nodebugio)
003000170522     h alwnull(*inputonly)
003100170522     h dftactgrp(*no)
003200170524     h bnddir('TRUL')
003300170522
003400170522       //--------------------------------------------------------------
003500170522       //?Dichiarazione file.                                          ?
003600170522       //--------------------------------------------------------------
003700170609
003800170609       // -?Tabelle?
003900170609     fTABEL00F  if   e           k disk
004000170522
004100170524       // -?BOLLE: Dettaglio Segnacolli?
004200170609     fFIART27L  if   e           k disk    usropn
004300170609     f                                     extfile(wLibFileFIART)
004400170522
004500170609      *//· // -?BOLLE ARRIVI: Testata?
004600170609     f*//·FNARB01L  if   e           k disk    usropn
004700170609     f*//·                                     extfile(wLibFileFNARB)
004800170609
004900170609       // -?BOLLE SEDE: da Fatturare + Fatt. Ultimi mesi + Fatt. Storico?
005000170609     fTITAS30C  if   e           k disk    usropn
005100170609     f                                     extfile(wLibFileTITAS)
005200170522
005300170522       // -?Merce Incompatibile: Spedizioni?
005400170529     fFIMIS01L  Uf A e           k disk
005500170522
005600170522       //--------------------------------------------------------------
005700170522       //?Definizione costanti.                                        ?
005800170522       //--------------------------------------------------------------
005900170522
006000170609       // -?Costante per controllo "caratteri solo numerici"?
006100170609     d c_Digits        c                   const('0123456789')
006200170522
006300170522       //--------------------------------------------------------------
006400170522       //?Definizione schiere.                                         ?
006500170522       //--------------------------------------------------------------
006600170522
006700170609       // -?Numero massimo di elementi delle schiere?
006800170609     d c_MaxCBO        c                   const(50)
006900170609       // -?Schiera Cod. Bolla?
007000170609     d sk_CBO          s                   like( TAScbo )
007100170609     d                                     dim( c_MaxCBO )  inz
007200170609       // -?Schiera Tipi Bolla per Cod. Bolla?
007300170609     d sk_TBL          s                   like( ds3A.§3Atb1 )
007400170609     d                                     dim( c_MaxCBO )  inz
007500170609     d sk_TB2          s                   like( ds3A.§3Atb2 )
007600170609     d                                     dim( c_MaxCBO )  inz
007700170522
007800170522       //--------------------------------------------------------------
007900170522       //?Definizione aree dati.                                       ?
008000170522       //--------------------------------------------------------------
008100170522
008200170522       // -?Dati utente?
008300170522     d §AzUte        e ds                  extname(AZUTE00F)
008400170522     d                                     dtaara
008500170522     d §DatiUte      e ds                  extname(dDatiUte)
008600170522     d                                     dtaara
008700170522
008800170522       //--------------------------------------------------------------
008900170522       //?Definizione strutture dati.                                  ?
009000170522       //--------------------------------------------------------------
009100170522
009200170522       // -?Status ds?
009300170522     d Status         sds
009400170522     d   SDSpgmName      *proc
009500170522     d*//SDSdta              191    198
009600170526     d*//SDSjob              244    253
009700170526     d*//SDSusr              254    263
009800170522
009900170522       // -?Parametri ricevuti?
010000170523     d KPJBA         e ds
010100170522     d FIMS15ds        ds                  inz   qualified
010200170523     d   iMS15fil                     3s 0 inz
010300170524     d   iMS15dti                     8s 0 inz
010400170524     d   iMS15dtf                     8s 0 inz
010500170523     d*//iMS15npg                     1s 0 inz
010600170522
010700170522       // -?Dati estratti via SQL?
010800170524     d FIMIP_ds      e ds                  extname(FIMIP00F)
010900170524     d                                     inz
011000170523
011100170523     d FIPSL00F      e ds                  based(nullPtr)
011200170523     d                                     qualified
011300170523     d wSQL_FIPSL_ds   ds                  inz   qualified
011400170523     d   pslFIL                            inz   like(FIPSL00F.PSLfil)
011500170523     d   pslNPG                            inz   like(FIPSL00F.PSLnpg)
011600170523     d   pslUSR                            inz   like(FIPSL00F.PSLusr)
011700170523     d   pslBARco                          inz   like(FIPSL00F.PSLbarCo)
011800170523
011900170523     d FLTR700F      e ds                  based(nullPtr)
012000170523     d                                     qualified
012100170523     d wSQL_FLTR7_ds   ds                  inz   qualified
012200170523     d   tr7FIL                            inz   like(FLTR700F.TR7fil)
012300170523     d   tr7NPG                            inz   like(FLTR700F.TR7ctl)
012400170523     d   tr7PEP                            inz   like(FLTR700F.TR7pep)
012500170523     d   tr7BAR                            inz   like(FLTR700F.TR7bar)
012600170523
012700170523       // -?BarCode Segnacollo?
012800170524     d wBarCode        ds            18    inz   qualified
012900170524     d   wBar_LNP                     3s 0 inz
013000170524     d   wBar_LNA                     3s 0 inz
013100170524     d   wBar_NRS                     2s 0 inz
013200170524     d   wBar_NSC                     7s 0 inz
013300170524     d   wBar_ZNC                     2s 0 inz
013400170524     d   wBar_CHK                     1s 0 inz
013500170524
013600170524       // -?Tabelle?
013700170524     d ds3A          e ds                  inz   qualified
013800170522
013900170522       //--------------------------------------------------------------
014000170522       //?Definizione variabili globali.                               ?
014100170522       //--------------------------------------------------------------
014200170522
014300170522       // -?Flags booleani?
014400170522     d $EoF            s               n   inz
014500170524     d $EoF2           s               n   inz
014600170524     d $EoF3           s               n   inz
014700170609
014800170609       // -?Indici di schiera?
014900170609     d xx              s              3  0 inz
015000170522
015100170522       // -?Stringhe SQL da eseguire?
015200170522     d wSQL            s          32740    inz   varying
015300170523     d wSQL2           s          32740    inz   varying
015400170523     d wSQL3           s          32740    inz   varying
015500170523
015600170523       // -?Data/Ora limite (dati alfanumerici)?
015700170523     d wDataOraIni     s             14    inz
015800170523     d wDataOraFin     s             14    inz
015900170522
016000170523       // -?Data in formato *EUR?
016100170523     d wDate_Eur       s               d   inz   datfmt(*EUR)
016200170523
016300170523       // -?Campi di Comodo?
016400170523     d wDateMax        s              8  0 inz
016500170523     d wDate           s              8  0 inz
016600170523     d wTime           s              6  0 inz
016700170609
016800170609       // -?Nomi Libreria/File?
016900170609     d wLibFileFIPSL   s             21    inz('FILTRA201/FIPSL00F   ')
017000170609     d wLibFileFLTR7   s             21    inz('FILTRA201/FLTR700F   ')
017100170609     d wLibFileFIART   s             21    inz('FILTRA201/FIART27L   ')
017200170609     d*//· wLibFileFNARB   s             21    inz('FILTRA201/FNARB01L   ')
017300170609     d wLibFileTITAS   s             21    inz('GAITRAGRU/TITAS30C   ')
017400170522
017500170522       //--------------------------------------------------------------
017600170522       //?Definizione prototipi procedure.                             ?
017700170522       //--------------------------------------------------------------
017800170522
017900170522       // -?Reperimento dati utente?
018000170522     d TIBS34ds      e ds                  inz
018100170522      /copy gaitrasrc/srcProtoPR,TIBS34R
018200170522
018300170522       // -?Reperimento dati tabelle?
018400170522      /copy gaitrasrc/srcProtoPI,TRULTAB
018500170522      /copy gaitrasrc/srcProtoPR,TRULTAB
018600170522
018700170522       // -?Parametri API QCAPCMD (Process Commands)?
018800170522     d Qcmd            s           2048    inz   varying
018900170522      /copy qSysInc/qRpgleSrc,QCAPCMD
019000170522       // -?API QCAPCMD (Process Commands)?
019100170522      /copy gaitrasrc/srcProtoPR,QCAPCMD
019200170522
019300170522       // -?Parametri gestione errori API.?
019400170522      /copy qsysinc/qrpglesrc,QUSEC
019500170522
019600170522       //--------------------------------------------------------------
019700170522       //?Definizione key-list.                                        ?
019800170522       //--------------------------------------------------------------
019900170609
020000170609       // -?File TABEL00F?
020100170609     d keyTABEL00    e ds                  extname( TABEL00F : *key )
020200170609     d                                     prefix( k_ )  inz
020300170522
020400170524       // -?File FIART27L?
020500170524     d keyFIART27    e ds                  extname( FIART27L : *key )
020600170524     d                                     prefix( k_ )  inz
020700170524
020800170609      *//· // -?File FNARB01L?
020900170609     d*//· keyFNARB01    e ds                  extname( FNARB01L : *key )
021000170609     d*//·                                     prefix( k_ )  inz
021100170609
021200170609       // -?File TITAS30C?
021300170609     d keyTITAS30    e ds                  extname( TITAS30C : *key )
021400170609     d                                     prefix( k_ )  inz
021500170524
021600170524       // -?File FIMIS01L?
021700170524     d keyFIMIS01    e ds                  extname( FIMIS01L : *key )
021800170524     d                                     prefix( k_ )  inz
021900170522
022000170522       //--------------------------------------------------------------
022100170522       //?Riepilogo indicatori utilizzati.                             ?
022200170522       //--------------------------------------------------------------
022300170522
022400170522
022500170522       //--------------------------------------------------------------
022600170522       //?M A I N - L I N E                                            ?
022700170522       //--------------------------------------------------------------
022800170522
022900170522     c     *Entry        plist
023000170522     c                   parm                    KPJBA
023100170522
023200170522      /free
023300170522
023400170522       // -?Operazioni iniziali?
023500170522       exsr  sr_RoutInz;
023600170522
023700170522       // -?Preparazione stringa SQL per estrazione dati?
023800170522       exsr  sr_PrepSQL;
023900170522
024000170522       // -?Apertura Cursore?
024100170522       exsr  sr_OpenCursor;
024200170522
024300170523       // -?Ciclo di elaborazione?
024400170522       DoW  $EoF = *off;
024500170522         exsr  sr_ReadCursor;
024600170522       EndDo;
024700170522
024800170522       // -?Chiusura Cursore?
024900170522       exsr  sr_CloseCursor;
025000170522
025100170522       // -?Operazioni finali?
025200170522       exsr sr_RoutEnd;
025300170522
025400170522       //--------------------------------------------------------------
025500170522       //?Operazioni iniziali.                                         ?
025600170522       //--------------------------------------------------------------
025700170522       BEGSR  sr_RoutInz;
025800170522
025900170522         // -?Impostazione opzioni per SQL?
026000170522         exec sql   set  option  DynUsrPrf = *Owner,
026100170522                                 CloSqlCsr = *EndMod;
026200170522
026300170522         // -?Impostazione chiusura?
026400170522         *inLR = *on;
026500170522
026600170522         // -?Reperimento dati job?
026700170522         exsr  sr_DatiJob;
026800170522
026900170522         // -?Reperimento parametri?
027000170523         clear  FIMS15ds;
027100170523         If  KPJBU <> *blank;
027200170523           FIMS15ds = KPJBU;
027300170523           if  %check( c_Digits : %subst( KPJBU : 1 :
027400170523                                  %size( FIMS15ds.iMS15fil ) ) )      > *zero;
027500170523             clear  FIMS15ds.iMS15fil;
027600170523           endif;
027700170523           if  %check( c_Digits : %subst( KPJBU :
027800170523                                  %size( FIMS15ds.iMS15fil ) + 1 :
027900170523                                  %size( FIMS15ds.iMS15dti ) ) ) > *zero;
028000170523             clear  FIMS15ds.iMS15dti;
028100170523           endif;
028200170523           if  %check( c_Digits : %subst( KPJBU :
028300170523                                  %size( FIMS15ds.iMS15fil ) +
028400170523                                  %size( FIMS15ds.iMS15dti ) + 1 :
028500170523                                  %size( FIMS15ds.iMS15dtf ) ) ) > *zero;
028600170523             clear  FIMS15ds.iMS15dtf;
028700170523           endif;
028800170523           if  FIMS15ds.iMS15dti > *zero  and
028900170523               FIMS15ds.iMS15dtf = *zero;
029000170523             FIMS15ds.iMS15dtf = FIMS15ds.iMS15dti;
029100170523           endif;
029200170523         EndIf;
029300170523
029400170523         // -?Reperimento data odierna in formato gg/mm/aaaa?
029500170523         wDate_Eur = %date();
029600170523         wDate = %dec( wDate_Eur );
029700170523
029800170523         // -?Impostazione data limite?
029900170523         wDateMax = %dec( %date() - %days(1) );
030000170523
030100170523         // -?Reperimento orario?
030200170523         wTime = %dec( %time() );
030300170609
030400170609         // -?Apertura archivi?
030500170609         If  %subst( KNSIF : 7 : 1 ) = 'P';
030600170609           wLibFileFIPSL = 'FILTRAPRD/FIPSL00F   ';
030700170609           wLibFileFLTR7 = 'FILTRAPRD/FLTR700F   ';
030800170609           wLibFileFIART = 'FILTRAPRD/FIART27L   ';
030900170609           //*·wLibFileFNARB = 'FILTRAPRD/FNARB01L   ';
031000170609           wLibFileTITAS = 'GAITRAGRPS/TITAS30C  ';
031100170609         EndIf;
031200170609         open  FIART27L;
031300170609         //*·open  FNARB01L;
031400170609         open  TITAS30C;
031500170609
031600170609         // -?Intabellamento Codici e Tipi Bolla?
031700170609         clear  sk_CBO;
031800170609         clear  sk_TBL;
031900170609         clear  sk_TB2;
032000170609         clear  keyTABEL00;
032100170609         k_TBLkut = 1;
032200170609         k_TBLcod = '3A';
032300170609         setll  %kds( keyTABEL00 )  TABEL;
032400170609         reade  %kds( keyTABEL00 : 2 )  TABEL;
032500170609         DoW  Not %eof( TABEL00F );
032600170609           ds3A = TBLuni;
032700170609           xx  += 1;
032800170609           sk_CBO(xx) = TBLkey;
032900170609           sk_TBL(xx) = ds3A.§3Atb1;
033000170609           sk_TB2(xx) = ds3A.§3Atb2;
033100170609           reade  %kds( keyTABEL00 : 2 )  TABEL;
033200170609         EndDo;
033300170522
033400170522       ENDSR;
033500170522
033600170522       //--------------------------------------------------------------
033700170522       //?Reperimento Dati del job (Utente/Operativi).                 ?
033800170522       //--------------------------------------------------------------
033900170522       BEGSR  sr_DatiJob;
034000170522
034100170522         in(e)  §AzUte;
034200170522         if  NOT  %error;
034300170522           in(e)  §DatiUte;
034400170522         endif;
034500170522         if  %error  or  RSut = *blank;
034600170522           tibs34r ( tibs34ds );
034700170522           in  §AzUte;
034800170522           in  §DatiUte;
034900170522         endif;
035000170522
035100170522       ENDSR;
035200170522
035300170522       //--------------------------------------------------------------
035400170523       //?Preparazione stringa SQL - per estrazione dati da FIMIP.     ?
035500170522       //--------------------------------------------------------------
035600170522       BEGSR  sr_PrepSQL;
035700170522
035800170524         wSQL  = 'select MIPfil, MIPnpg, MIPtip, MIPpep, +
035900170524                         MIPdti, MIPdtf, MIPhmi, MIPhmf +
036000170523                    from FIMIP00F';
036100170523
036200170523         if  FIMS15ds.iMS15fil > *zero;
036300170524           wSQL +=
036400170524                 ' where MIPfil = ' + %editc( FIMS15ds.iMS15fil : 'X' );
036500170524         endif;
036600170523
036700170524         wSQL += ' order by MIPfil, MIPnpg, MIPtip, MIPpep, +
036800170524                         MIPdti, MIPdtf, MIPhmi, MIPhmf +
036900170522                     for fetch only';
037000170522
037100170522       ENDSR;
037200170522
037300170522       //--------------------------------------------------------------
037400170522       //?Apertura cursore.                                            ?
037500170522       //--------------------------------------------------------------
037600170522       BEGSR  sr_OpenCursor;
037700170522
037800170524         $EoF  = *off;
037900170522
038000170522         // -?Dichiarazione cursore?
038100170522         exec sql   prepare S1   from :wSQL;
038200170522         exec sql   declare C1   cursor for S1;
038300170522
038400170522         if  SQLcode < *zero;
038500170524           $EoF  = *on;
038600170522           exsr  sr_PrintERR;
038700170522           leavesr;
038800170522         endif;
038900170522
039000170522         // -?Apertura del cursore?
039100170522         exec sql   open C1;
039200170522
039300170522         if  SQLcode < *zero;
039400170524           $EoF  = *on;
039500170522           exsr  sr_PrintERR;
039600170522           leavesr;
039700170522         endif;
039800170522
039900170522       ENDSR;
040000170522
040100170522       //--------------------------------------------------------------
040200170522       //?Chiusura cursore.                                            ?
040300170522       //--------------------------------------------------------------
040400170522       BEGSR  sr_CloseCursor;
040500170522
040600170522         // -?Chiusura del cursore?
040700170522         exec sql   close C1;
040800170522
040900170522       ENDSR;
041000170522
041100170522       //--------------------------------------------------------------
041200170523       //?Lettura cursore.                                             ?
041300170522       //--------------------------------------------------------------
041400170522       BEGSR  sr_ReadCursor;
041500170522
041600170524         clear  FIMIP_ds;
041700170522
041800170524         exec sql   fetch next   from C1   into :FIMIP_ds;
041900170522
042000170522         Select;
042100170522
042200170522           // -?E.o.F.?
042300170522           When  SQLcode = 100;
042400170524             $EoF = *on;
042500170522
042600170522           // -?Errore?
042700170522           When  SQLcode < *zero;
042800170522             $EoF = *on;
042900170522             exsr  sr_PrintERR;
043000170522
043100170522           // -?Elaborazione?
043200170522           Other;
043300170523             exsr  sr_Elab_FIMIP;
043400170522
043500170522         EndSl;
043600170522
043700170522       ENDSR;
043800170523
043900170523       //--------------------------------------------------------------
044000170523       //?Elaborazione singolo record del *file FIMIP00F.              ?
044100170523       //--------------------------------------------------------------
044200170523       BEGSR  sr_Elab_FIMIP;
044300170524
044400170524         // -?Impostazione date e orari limite per elaborazione?
044500170525         // ·?Data inizio elaborazione?
044600170524         select;
044700170524           when  FIMS15ds.iMS15dti >  *zero  and
044800170524                 FIMS15ds.iMS15dti >= MIPdti;
044900170524             wDataOraIni = %editc( FIMS15ds.iMS15dti : 'X' );
045000170524           when  FIMS15ds.iMS15dti <= *zero  and
045100170524                 wDateMax >= MIPdti;
045200170524             wDataOraIni = %editc( wDateMax : 'X' );
045300170524           other;
045400170524             leavesr;
045500170524         endsl;
045600170525         // ·?Orario inizio elaborazione?
045700170524         if  MIPhmi <> *zero;
045800170524           wDataOraIni = %trimR( wDataOraIni ) + %editc( MIPhmi : 'X' )
045900170524                                               + '00';
046000170524         else;
046100170524           wDataOraIni = %trimR( wDataOraIni ) + '000000';
046200170524         endif;
046300170525         // ·?Data fine elaborazione?
046400170524         select;
046500170524           when  FIMS15ds.iMS15dtf >  *zero  and
046600170524                 FIMS15ds.iMS15dtf <= MIPdtf;
046700170524             wDataOraFin = %editc( FIMS15ds.iMS15dtf : 'X' );
046800170524           when  FIMS15ds.iMS15dtf <= *zero  and
046900170524                 wDateMax <= MIPdtf;
047000170524             wDataOraFin = %editc( wDateMax : 'X' );
047100170524           other;
047200170524             leavesr;
047300170524         endsl;
047400170525         // ·?Orario fine elaborazione?
047500170524         if  MIPhmf <> *zero;
047600170524           wDataOraFin = %trimR( wDataOraFin ) + %editc( MIPhmf : 'X' )
047700170524                                               + '99';
047800170524         else;
047900170524           wDataOraFin = %trimR( wDataOraFin ) + '999999';
048000170524         endif;
048100170523
048200170525
048300170525         Select;
048400170525
048500170525           // -?Estrazione letture da Pistola da FIPSL?
048600170525           When  MIPtip = 'P';
048700170523             exsr  sr_Pistola;
048800170525
048900170525           // -?Estrazione letture da Banco da FLTR7?
049000170525           When  MIPtip = 'F';
049100170523             exsr  sr_Banco;
049200170525
049300170525         EndSl;
049400170523
049500170523       ENDSR;
049600170522
049700170522       //--------------------------------------------------------------
049800170523       //?Elaborazioni letture da Pistola.                             ?
049900170522       //--------------------------------------------------------------
050000170523       BEGSR  sr_Pistola;
050100170523
050200170524         $EoF2 = *off;
050300170523
050400170523         // -?Preparazione stringa SQL per estrazione dati?
050500170523         exsr  sr_PrepSQL_FIPSL;
050600170523
050700170523         // -?Apertura Cursore?
050800170523         exsr  sr_OpenCursor_FIPSL;
050900170523
051000170523         // -?Ciclo di elaborazione?
051100170524         DoW  $EoF2 = *off;
051200170523           exsr  sr_ReadCursor_FIPSL;
051300170523         EndDo;
051400170523
051500170523         // -?Chiusura Cursore?
051600170523         exsr  sr_CloseCursor_FIPSL;
051700170522
051800170522       ENDSR;
051900170523
052000170523       //--------------------------------------------------------------
052100170523       //?Elaborazioni letture da Banco.                               ?
052200170523       //--------------------------------------------------------------
052300170523       BEGSR  sr_Banco;
052400170523
052500170524         $EoF3 = *off;
052600170523
052700170523         // -?Preparazione stringa SQL per estrazione dati?
052800170523         exsr  sr_PrepSQL_FLTR7;
052900170523
053000170523         // -?Apertura Cursore?
053100170523         exsr  sr_OpenCursor_FLTR7;
053200170523
053300170523         // -?Ciclo di elaborazione?
053400170524         DoW  $EoF3 = *off;
053500170523           exsr  sr_ReadCursor_FLTR7;
053600170523         EndDo;
053700170523
053800170523         // -?Chiusura Cursore?
053900170523         exsr  sr_CloseCursor_FLTR7;
054000170523
054100170523       ENDSR;
054200170523
054300170523       //--------------------------------------------------------------
054400170523       //?Preparazione stringa SQL - per estrazione dati da FIPSL.     ?
054500170523       //--------------------------------------------------------------
054600170523       BEGSR  sr_PrepSQL_FIPSL;
054700170524
054800170605         wSQL2 = 'select PSLfil, PSLnpg, PSLusr, PSLbarCo +
054900170609                    from ' + %trimR( wLibFileFIPSL ) + ' +
055000170523                   where PSLfil = ''' + %editc( MIPfil : 'X' ) + ''' +
055100170524                     and PSLnpg = ''' + %editc( MIPnpg : 'X' ) + ''' +
055200170523                     and PSLerr = ''  '' +
055300170524                     and PSLusr like ''%' + %editc( MIPfil : 'X' ) +
055400170605                                            %trimR( MIPpep ) + '%'' +
055500170523                     and PSLdtOraR between ''' + wDataOraIni + ''' +
055600170523                                       and ''' + wDataOraFin + ''' +
055700170523                order by PSLfil, PSLnpg, PSLusr, PSLbarCo +
055800170523                     for fetch only';
055900170523
056000170523       ENDSR;
056100170523
056200170523       //--------------------------------------------------------------
056300170523       //?Apertura cursore - per estrazione dati da FIPSL.             ?
056400170523       //--------------------------------------------------------------
056500170523       BEGSR  sr_OpenCursor_FIPSL;
056600170523
056700170524         $EoF2 = *off;
056800170523
056900170523         // -?Dichiarazione cursore?
057000170523         exec sql   prepare S2   from :wSQL2;
057100170523         exec sql   declare C2   cursor for S2;
057200170523
057300170523         if  SQLcode < *zero;
057400170524           $EoF2 = *on;
057500170523           exsr  sr_PrintERR;
057600170523           leavesr;
057700170523         endif;
057800170523
057900170523         // -?Apertura del cursore?
058000170523         exec sql   open C2;
058100170523
058200170523         if  SQLcode < *zero;
058300170524           $EoF2 = *on;
058400170523           exsr  sr_PrintERR;
058500170523           leavesr;
058600170523         endif;
058700170523
058800170523       ENDSR;
058900170523
059000170523       //--------------------------------------------------------------
059100170523       //?Chiusura cursore - per estrazione dati da FIPSL.             ?
059200170523       //--------------------------------------------------------------
059300170523       BEGSR  sr_CloseCursor_FIPSL;
059400170523
059500170523         // -?Chiusura del cursore?
059600170523         exec sql   close C2;
059700170525
059800170525         if  SQLcode < *zero;
059900170525           $EoF2 = *on;
060000170525           exsr  sr_PrintERR;
060100170525           leavesr;
060200170525         endif;
060300170523
060400170523       ENDSR;
060500170523
060600170523       //--------------------------------------------------------------
060700170523       //?Lettura cursore - per estrazione dati da FIPSL.              ?
060800170523       //--------------------------------------------------------------
060900170523       BEGSR  sr_ReadCursor_FIPSL;
061000170523
061100170523         clear  wSQL_FIPSL_ds;
061200170523
061300170523         exec sql   fetch next   from C2   into :wSQL_FIPSL_ds;
061400170523
061500170523         Select;
061600170523
061700170523           // -?E.o.F.?
061800170523           When  SQLcode = 100;
061900170524             $EoF2 = *on;
062000170523
062100170523           // -?Errore?
062200170523           When  SQLcode < *zero;
062300170524             $EoF2 = *on;
062400170523             exsr  sr_PrintERR;
062500170523
062600170523           // -?Elaborazione?
062700170523           Other;
062800170524             clear  wBarCode;
062900170524             wBarCode = wSQL_FIPSL_ds.PSLbarCo;
063000170524             exsr  sr_Write_FIMIS;
063100170523
063200170523         EndSl;
063300170523
063400170523       ENDSR;
063500170523
063600170523       //--------------------------------------------------------------
063700170523       //?Preparazione stringa SQL - per estrazione dati da FLTR7.     ?
063800170523       //--------------------------------------------------------------
063900170523       BEGSR  sr_PrepSQL_FLTR7;
064000170523
064100170605         wSQL3 = 'select TR7fil, TR7ctl, TR7pep, TR7bar +
064200170609                    from ' + %trimR( wLibFileFLTR7 ) + ' +
064300170523                   where TR7fil = ''' + %editc( MIPfil : 'X' ) + ''' +
064400170524                     and TR7ctl = ''' + %editc( MIPnpg : 'X' ) + ''' +
064500170523                     and TR7crc = ''  '' +
064600170524                     and TR7pep like ''%' + %trimR( MIPpep ) + ''' +
064700170605                     and TR7bar > ''00000000000000'' +
064800170523                     and TR7tim between ''' + wDataOraIni + ''' +
064900170523                                    and ''' + wDataOraFin + ''' +
065000170523                   order by TR7fil, TR7ctl, TR7pep, TR7bar +
065100170523                     for fetch only';
065200170523
065300170523       ENDSR;
065400170523
065500170523       //--------------------------------------------------------------
065600170523       //?Apertura cursore - per estrazione dati da FLTR7.             ?
065700170523       //--------------------------------------------------------------
065800170523       BEGSR  sr_OpenCursor_FLTR7;
065900170523
066000170524         $EoF3 = *off;
066100170523
066200170523         // -?Dichiarazione cursore?
066300170523         exec sql   prepare S3   from :wSQL3;
066400170523         exec sql   declare C3   cursor for S3;
066500170523
066600170523         if  SQLcode < *zero;
066700170524           $EoF3 = *on;
066800170523           exsr  sr_PrintERR;
066900170523           leavesr;
067000170523         endif;
067100170523
067200170523         // -?Apertura del cursore?
067300170523         exec sql   open C3;
067400170523
067500170523         if  SQLcode < *zero;
067600170524           $EoF3 = *on;
067700170523           exsr  sr_PrintERR;
067800170523           leavesr;
067900170523         endif;
068000170523
068100170523       ENDSR;
068200170523
068300170523       //--------------------------------------------------------------
068400170523       //?Chiusura cursore - per estrazione dati da FLTR7.             ?
068500170523       //--------------------------------------------------------------
068600170524       BEGSR  sr_CloseCursor_FLTR7;
068700170523
068800170523         // -?Chiusura del cursore?
068900170523         exec sql   close C3;
069000170525
069100170525         if  SQLcode < *zero;
069200170525           $EoF3 = *on;
069300170525           exsr  sr_PrintERR;
069400170525           leavesr;
069500170525         endif;
069600170523
069700170523       ENDSR;
069800170523
069900170523       //--------------------------------------------------------------
070000170523       //?Lettura cursore - per estrazione dati da FLTR7.              ?
070100170523       //--------------------------------------------------------------
070200170523       BEGSR  sr_ReadCursor_FLTR7;
070300170523
070400170523         clear  wSQL_FLTR7_ds;
070500170523
070600170523         exec sql   fetch next   from C3   into :wSQL_FLTR7_ds;
070700170523
070800170523         Select;
070900170523
071000170523           // -?E.o.F.?
071100170523           When  SQLcode = 100;
071200170524             $EoF3 = *on;
071300170523
071400170523           // -?Errore?
071500170523           When  SQLcode < *zero;
071600170524             $EoF3 = *on;
071700170523             exsr  sr_PrintERR;
071800170523
071900170523           // -?Elaborazione?
072000170523           Other;
072100170524             clear  wBarCode;
072200170524             wBarCode = wSQL_FLTR7_ds.TR7bar;
072300170524             exsr  sr_Write_FIMIS;
072400170523
072500170523         EndSl;
072600170523
072700170523       ENDSR;
072800170522
072900170522       //--------------------------------------------------------------
073000170522       //?Controllo/Scrittura record in FIMIS00F.                      ?
073100170522       //--------------------------------------------------------------
073200170522       BEGSR  sr_Write_FIMIS;
073300170524
073400170524         // -?Reperimento n° spedizione dal segnacollo in esame?
073500170524         clear  keyFIART27;
073600170524         k_ARTfls = wBarCode.wBar_LNP;
073700170524         k_ARTlna = wBarCode.wBar_LNA;
073800170524         k_ARTnrs = wBarCode.wBar_NRS;
073900170524         k_ARTnsc = wBarCode.wBar_NSC;
074000170524         chain  %kds( keyFIART27 )  FIART000;
074100170524
074200170524         if  not %found( FIART27L );
074300170524           leavesr;
074400170524         endif;
074500170522
074600170522         // -?Controllo se esiste già un rec. per la spedizione?
074700170524         clear  keyFIMIS01;
074800170524         k_MISaas = ARTaas;
074900170524         k_MISlnp = ARTlnp;
075000170524         k_MISnrs = ARTnrs;
075100170524         k_MISnsp = ARTnsp;
075200170524         setll  %kds( keyFIMIS01 )  FIMIS000;
075300170524
075400170529         //*·if  %equal( FIMIS01L );
075500170529         //*·  leavesr;
075600170529         //*·endif;
075700170524
075800170609         //*·// -?Aggancio FNARB00F?
075900170609         //*·clear  keyFNARB01;
076000170609         //*·k_ARBaas = ARTaas;
076100170609         //*·k_ARBlnp = ARTlnp;
076200170609         //*·k_ARBnrs = ARTnrs;
076300170609         //*·k_ARBnsp = ARTnsp;
076400170609         //*·chain  %kds( keyFNARB01 )  FNARB000;
076500170609         //*·
076600170609         //*·if  not %found( FNARB01L );
076700170609         //*·  leavesr;
076800170609         //*·endif;
076900170529
077000170529         // -?Aggiornamento n° Spunte Incompatibili?
077100170529         //  ?(suddiviso tra Partenze e Arrivi)?
077200170529         IF  %equal( FIMIS01L );
077300170529
077400170529           chain  %kds( keyFIMIS01 )  FIMIS000;
077500170529           If  %found( FIMIS01L );
077600170529
077700170529             if  MIPnpg = 5;
077800170529               MISnsiP += 1;
077900170529             else;
078000170529               MISnsiA += 1;
078100170529             endif;
078200170529             //_______________
078300170529             update  FIMIS000;
078400170529             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
078500170529             leavesr;
078600170529
078700170529           EndIf;
078800170529
078900170529         ENDIF;
079000170609
079100170609         // -?Aggancio TITAS30C?
079200170609         clear  keyTITAS30;
079300170609         k_TASaas = ARTaas;
079400170609         k_TASlnp = ARTlnp;
079500170609         k_TASnrs = ARTnrs;
079600170609         k_TASnsp = ARTnsp;
079700170609         setll  %kds( keyTITAS30 )  TITAS30C;
079800170609         reade  %kds( keyTITAS30 : 4 )  TITAS30C;
079900170609
080000170609         if  %eof( TITAS30C );
080100170609           leavesr;
080200170609         endif;
080300170609
080400170609         xx = %lookup( TAScbo : sk_CBO );
080500170609
080600170609         If  xx > *zero  and  TAStbl <> sk_TBL(xx)
080700170609                         and  TAStbl =  sk_TB2(xx);
080800170609
080900170609           k_TAStbl = sk_TBL(xx);
081000170609           chain  %kds( keyTITAS30 )  TITAS30C;
081100170609           if  NOT %found( TITAS30C );
081200170609             leavesr;
081300170609           endif;
081400170609
081500170609         EndIf;
081600170524
081700170524         // -?Scrittura record in FIMIS00F?
081800170524         clear  FIMIS000;
081900170609         MISaas = TASaas;
082000170609         MISmgs = TASmgs;
082100170609         MISlnp = TASlnp;
082200170609         MISnrs = TASnrs;
082300170609         MISnsp = TASnsp;
082400170609         MIStbl = TAStbl;
082500170609         MIStfp = TAStfp;
082600170524         select;
082700170609           when  TASccm <> *zero;
082800170609             MISccm = TASccm;
082900170609           when  %subst( TAStbl : 1 : 1 ) = 'F';
083000170609             MISccm = TASksc;
083100170526           other;
083200170609             MISccm = ( TASlnp * 10000 ) + 8888;
083300170524         endsl;
083400170609         //*·MISctr = TASctr;
083500170609         MISpkf = TASpkf;
083600170609         MISvlf = TASvlf;
083700170609         MISncl = TASncl;
083800170529         if  MIPnpg = 5;
083900170529           MISnsiP = 1;
084000170529         else;
084100170529           MISnsiA = 1;
084200170529         endif;
084300170524
084400170524         //______________
084500170524         write  FIMIS000;
084600170524         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
084700170522
084800170522       ENDSR;
084900170522
085000170522       //--------------------------------------------------------------
085100170522       //?Stampa segnalazione dell'errore rilevato via SQL.            ?
085200170522       //--------------------------------------------------------------
085300170522       BEGSR  sr_PrintERR;
085400170522
085500170522         // -?Stampa del Dump?
085600170522         Dump(A);
085700170522
085800170522         // -?Stampa del Job-Log?
085900170522         Qcmd = 'DSPJOBLOG job(*) output(*print)';
086000170522         exsr  sr_ExecCmd;
086100170522
086200170522       ENDSR;
086300170522
086400170522       //--------------------------------------------------------------
086500170522       //?Operazioni finali.                                           ?
086600170522       //--------------------------------------------------------------
086700170522       BEGSR  sr_RoutEnd;
086800170522
086900170522         // -?Uscita dal *pgm?
087000170522         return;
087100170522
087200170522       ENDSR;
087300170522
087400170522       //--------------------------------------------------------------
087500170522       //?Esecuzione del comando (già impostato).                      ?
087600170522       //--------------------------------------------------------------
087700170522       BEGSR  sr_ExecCmd;
087800170522
087900170522         clear Qcap0100;
088000170522         Qcabcsdh = *off;
088100170522         Qcapa    = *off;
088200170522         Qcacmdss = *off;
088300170522         Qcaerved = *allX'00';
088400170522
088500170522         clear Qusec;
088600170522         Qusbprv  = %size(Qusec);
088700170522
088800170522         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
088900170522                           %size(Qcap0100) : 'CPOP0100' : *omit :
089000170522                           0 : 0 : Qusec);
089100170522
089200170522         //if  Qusei <> *blank;
089300170522         //  ...;
089400170522         //endif;
089500170522
089600170522       ENDSR;
089700170522
089800170522      /end-free

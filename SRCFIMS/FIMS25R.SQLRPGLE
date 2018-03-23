000100170124       //==============================================================
000200170124       //?FIMS25R * Letture Picking per minuto di ogni Turno (FIMSP00F)?
000300170124       //==============================================================
000400170124
000500170124       //--------------------------------------------------------------
000600170124       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700170124       //--------------------------------------------------------------
000800170124
000900170124     /*PRM  dbgview(*source)
001000170124     /*END
001100170124
001200170124       //--------------------------------------------------------------
001300170124       //?Specifiche di controllo.                                     ?
001400170124       //--------------------------------------------------------------
001500170124
001600170124     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700170124     h dftactgrp(*no)
001800170124     h alwnull(*inputonly)
001900170124
002000170124       //--------------------------------------------------------------
002100170124       //?Dichiarazione file.                                          ?
002200170124       //--------------------------------------------------------------
002300170125
002400170125       // -?BOLLE : Dettaglio Segnacolli?
002500170125     fFIART27L  if   e           k disk
002600170125
002700170125       // -?BOLLE ARRIVI : Testata?
002800170125     fFNARB01L  if   e           k disk
002900170125
003000170125       // -?Distinte Uscita AUT Consegne/Ritiri?
003100170125     fFIDST01L  if   e           k disk
003200170125
003300170125       // -?Organigramma Filiale/Aziendale?
003400170125     fAZORG01L  if   e           k disk
003500170125
003600170125       // -?Tabelle?
003700170125     fTNTBE01L  if   e           k disk
003800170124
003900170208       // -?Letture modalità Picking - Saldi per minuto?
004000170125     fFIMSP01L  Uf A e           k disk
004100170124
004200170124       //--------------------------------------------------------------
004300170124       //?Definizione costanti.                                        ?
004400170124       //--------------------------------------------------------------
004500170124
004600170124       // -?Costante per controllo "caratteri solo numerici"?
004700170124     d c_Digits        c                   const('0123456789')
004800170124
004900170124       // -?Nome Area Dati con orario ultimo aggiornamento?
005000170124     d c_FIMS25dtAr    c                   const('GAITRAOBJ/FIMS25DTAR')
005100170125
005200170125       // -?Parametri per *cmd "CRTDTAATA"?
005300170125     d IgnoreDBCSdata...
005400170125     d                 c                   const('0')
005500170125     d NeverPromptTheCommand...
005600170125     d                 c                   const('0')
005700170125     d CommandRunning...
005800170125     d                 c                   const('0')
005900170124
006000170124       //--------------------------------------------------------------
006100170124       //?Definizione schiere.                                         ?
006200170124       //--------------------------------------------------------------
006300170124
006400170125       // -?Tab. "MSL" = LayOut postazioni?
006500170125     d sk_MSLpep       s                   dim(80)
006600170125     d                                     inz    like(dMSL.§MSLpep)
006700170125     d sk_MSLent       s                   dim(80)
006800170125     d                                     inz    like(dMSL.§MSLent)
006900170125     d sk_MSLapl       s                   dim(80)
007000170125     d                                     inz    like(dMSL.§MSLapl)
007100170125     d sk_MSLap5       s                   dim(80)
007200170125     d                                     inz    like(dMSL.§MSLap5)
007300170125     d sk_MSLpke       s                   dim(80)
007400170125     d                                     inz    like(dMSL.§MSLpke)
007500170125     d sk_MSLpwf       s                   dim(80)
007600170125     d                                     inz    like(dMSL.§MSLpwf)
007700170126
007800170126       // -?Filiali abilitate al Turno Picking consegna?
007900170126     d sk_FilTurno     s                   dim(250)
008000170126     d                                     inz    like(ORGfil)
008100170124
008200170124       //--------------------------------------------------------------
008300170124       //?Definizione aree dati.                                       ?
008400170124       //--------------------------------------------------------------
008500170124
008600170124       // -?Data/Ora ultima elaborazione?
008700170124     d FIMS25dtAr      ds
008800170124     d                                     dtaara( *var : c_FIMS25dtAr )
008900170125     d                                     inz    qualified
009000170124     d   §UltElab_Data...
009100170124     d                                8s 0 inz
009200170124     d   §UltElab_Ora...
009300170124     d                                4s 0 inz
009400170124
009500170124       //--------------------------------------------------------------
009600170124       //?Definizione strutture dati.                                  ?
009700170124       //--------------------------------------------------------------
009800170125
009900170125       // -?Status ds?
010000170125     d Status         sds
010100170125     d   SDSpgm          *proc
010200170125     d*//SDSprm          *parms
010300170125     d*//SDSdta              191    198
010400170125     d   SDSjob              244    253
010500170125     d   SDSusr              254    263
010600170124
010700170124       // -?Parametri Ricevuti?
010800170124     d KPJBA         e ds
010900170124     d FIMS25ds        ds                  inz    qualified
011000170124     d   prm_Fil                      3s 0 inz
011100170124     d   prm_Time_Ini                14s 0 inz
011200170124     d   prm_Time_End                14s 0 inz
011300170124     d   prm_Turno                    1a   inz
011400170124     d   prm_Impianto                 1a   inz
011500170125
011600170125       // -?Tab. "MFP" = PosiTrova - Attivo in filiale?
011700170125     d dMFP          e ds                  inz    qualified
011800170125       // -?Tab. "MSL" = VdL - Layout postazioni?
011900170125     d dMSL          e ds                  inz    qualified
012000170125
012100170125       // -?DS lettura campo FIDST00F.DSTFLR?
012200170125     d dDSTflr       e ds                  inz    qualified
012300170125
012400170125       // -?150ª DESCRIZIONE dell'Organigramma?
012500170125     d Og150         e ds                  inz    qualified
012600170124
012700170124       // -?Campi Formato Record FLTR700F?
012800170125     d FLTR7_ds      e ds                  inz    extname(FLTR700F)
012900170124
013000170124       // -?Data/Ora attuali?
013100170125     d wTime_ds        ds                  inz    qualified
013200170124     d   wDate                        8s 0 inz
013300170124     d   wTime                        6s 0 inz
013400170124     d   wTimeHM               9     12s 0 inz
013500170124     d   wTimeS               13     14s 0 inz
013600170125
013700170125       // -?Segnacollo?
013800170125     d wBarCode_BRT    ds                  inz    qualified
013900170125     d   wBar_LNP              1      3  0 inz
014000170125     d   wBar_LNA              4      6  0 inz
014100170125     d   wBar_NRS              7      8  0 inz
014200170125     d   wBar_NSC              9     15  0 inz
014300170125     d   wBar_ZNC             16     17  0 inz
014400170124
014500170124       //--------------------------------------------------------------
014600170124       //?Definizione variabili globali.                               ?
014700170124       //--------------------------------------------------------------
014800170124
014900170124       // -?Flags booleani?
015000170124     d $Err            s               n   inz
015100170124     d $RecOK          s               n   inz
015200170124     d $Fine           s               n   inz
015300170125
015400170125       // -?Indici di schiera?
015500170125     d xx              s              3  0 inz
015600170125
015700170125       // -?Stringa SQL da eseguire?
015800170127     d wSql            s          32740    inz    varying
015900170124
016000170124       // -?Campi per controllo "rotture"?
016100170124     d wTurno          s                   inz   like(MSPtur)
016200170124     d wImpianto       s                   inz   like(MSPimp)
016300170125
016400170125       // -?Campi di comodo?
016500170127     d wTimeStamp      s               z   inz
016600170125     d wMSPncl         s                   inz   like(MSPncl)
016700170124
016800170124       //--------------------------------------------------------------
016900170124       //?Definizione prototipi procedure usate.                       ?
017000170124       //--------------------------------------------------------------
017100170124
017200170124       // -?API QCAPCMD (Process Commands)?
017300170124     d Qcmd            s           2048    inz   varying
017400170124      /copy qSysInc/qRpgleSrc,QCAPCMD
017500170124      /copy gaitrasrc/srcProtoPR,QCAPCMD
017600170124       // -?Parametri gestione errori API.?
017700170124      /copy qsysinc/qRpgleSrc,QUSEC
017800170124
017900170124       //--------------------------------------------------------------
018000170124       //?Definizione key-list.                                        ?
018100170124       //--------------------------------------------------------------
018200170125
018300170125       // -?File FIMSP01L?
018400170125     d keyFIMSP01    e ds                  extname( FIMSP01L : *key )
018500170125     d                                     prefix( k_ )
018600170125     d                                     inz
018700170124
018800170125       // -?File FIART27L?
018900170125     d keyFIART27    e ds                  extname( FIART27L : *key )
019000170125     d                                     prefix( k_ )
019100170125     d                                     inz
019200170125
019300170125       // -?File FNARB01L?
019400170125     d keyFNARB01    e ds                  extname( FNARB01L : *key )
019500170125     d                                     prefix( k_ )
019600170125     d                                     inz
019700170125
019800170125       // -?File FIDST01L?
019900170125     d keyFIDST01    e ds                  extname( FIDST01L : *key )
020000170125     d                                     prefix( k_ )
020100170125     d                                     inz
020200170125
020300170125       // -?File TNTBE01L?
020400170125     d keyTNTBE01    e ds                  extname( TNTBE01L : *key )
020500170125     d                                     prefix( k_ )
020600170125     d                                     inz
020700170124
020800170124       //--------------------------------------------------------------
020900170124       //?Riepilogo indicatori utilizzati.                             ?
021000170124       //--------------------------------------------------------------
021100170124
021200170124
021300170124       //--------------------------------------------------------------
021400170124       //?M A I N - L I N E                                            ?
021500170124       //--------------------------------------------------------------
021600170124
021700170124     c     *Entry        plist
021800170124     c                   parm                    KPJBA
021900170124
022000170124      /free
022100170124
022200170124       // -?Operazioni iniziali?
022300170124       exsr  sr_RoutInz;
022400170124
022500170124       // -?Preparazione stringa SQL?
022600170124       exsr  sr_PrepSQL;
022700170124
022800170124       // -?Apertura del cursore?
022900170124       exsr  sr_OpenCursor;
023000170124
023100170124       // -?Lettura del file?
023200170124       DoW  Not $Fine;
023300170124         exsr  sr_ReadCursor;
023400170124       EndDo;
023500170124
023600170124       // -?Chiusura del cursore?
023700170124       exsr  sr_CloseCursor;
023800170124
023900170124       // -?Operazioni finali?
024000170124       exsr  sr_RoutEnd;
024100170124
024200170124       //--------------------------------------------------------------
024300170124       //?Operazioni iniziali.                                         ?
024400170124       //--------------------------------------------------------------
024500170124       BEGSR  sr_RoutInz;
024600170124
024700170124         // -?Impostazione opzioni per SQL?
024800170124         exec sql   set option   DynUsrPrf = *Owner,
024900170124                                 CloSqlCsr = *EndMod;
025000170124
025100170124         // -?Impostazione chiusura?
025200170124         *inLR = *on;
025300170124
025400170124         // -?Ricezione Parametri?
025500170124         FIMS25ds = KPJBU;
025600170124         if  %check( c_Digits : %subst( KPJBU : 1 :
025700170124                                %size( FIMS25ds.prm_Fil ) ) )      > *zero;
025800170124           clear  FIMS25ds.prm_Fil;
025900170124         endif;
026000170124         if  %check( c_Digits : %subst( KPJBU :
026100170124                                %size( FIMS25ds.prm_Fil ) + 1 :
026200170124                                %size( FIMS25ds.prm_Time_Ini ) ) ) > *zero;
026300170124           clear  FIMS25ds.prm_Time_Ini;
026400170124         endif;
026500170124         if  %check( c_Digits : %subst( KPJBU :
026600170124                                %size( FIMS25ds.prm_Fil) +
026700170124                                %size( FIMS25ds.prm_Time_Ini ) + 1 :
026800170124                                %size( FIMS25ds.prm_Time_End ) ) ) > *zero;
026900170124           clear  FIMS25ds.prm_Time_End;
027000170124         endif;
027100170126
027200170126         // -?Memorizzazione filiali abilitate al Turno Picking Consegna?
027300170126         //  ?(dalla 150ª descrizione in Organigramma)?
027400170126         clear  xx;
027500170126         setll  ( *loval )  AZORG;
027600170126         read  AZORG;
027700170126         DoW  Not %eof(AZORG01L);
027800170126           if  ORGfva = *blank;
027900170126             Og150 = ORGdf0;
028000170126             if  Og150.§ogTurno = 'S';
028100170126               xx += 1;
028200170126               sk_FilTurno(xx)  = ORGfil;
028300170126             endif;
028400170126           endif;
028500170126           read  AZORG;
028600170126         EndDo;
028700170124
028800170124         // -?Reperimento area dati con l'orario dell'ultima elaborazione?
028900170124         exsr  sr_OraUltEla;
029000170124
029100170125         // -?Impostazione data e ora limite?
029200170127
029300170127         // -?Reperimento data e ora attuali?
029400170127         //*·wTime_ds = %subst( %char( %dec( %timestamp() ) )
029500170127         //*·                   : 1 : 14 );
029600170127         wTimeStamp = %timestamp() - %minutes(60) + %minutes(1);
029700170127         wTime_ds   = %subst( %char( %dec( wTimeStamp ) ) : 1 : 12 ) + '00';
029800170127
029900170127         if  FIMS25ds.prm_Time_Ini > *zero  and
030000170127             FIMS25ds.prm_Time_End = *zero;
030100170127           FIMS25ds.prm_Time_End = wTime_ds.wTime;
030200170127         endif;
030300170124
030400170124       ENDSR;
030500170124
030600170124       //--------------------------------------------------------------
030700170124       //?Reperimento Area Dati con l'orario dell'ultima elaborazione. ?
030800170124       //--------------------------------------------------------------
030900170124       BEGSR  sr_OraUltEla;
031000170124
031100170124         in(E)  *lock  FIMS25dtAr;
031200170124
031300170124         If  %error;
031400170124
031500170124           clear  Qcap0100;
031600170124           Qcabcsdh = IgnoreDBCSdata;
031700170124           Qcapa    = NeverPromptTheCommand;
031800170124           Qcacmdss = CommandRunning;
031900170124           Qcaerved = X'000000000000000000';
032000170125           Qcmd     = 'CRTDTAARA dtaara(' + c_FIMS25dtAr + ') +
032100170125                                 type(*char) +
032200170125                                 len(' + %char(%size(FIMS25dtAr)) + ') +
032300170125                                 value(''000000000000'') +
032400170125                                 text(''Limite aggiornamento FIMSP00F'')';
032500170125           exsr  sr_ExecCmd;
032600170124           if  Qusei <> ' ';
032700170124             $Err = *on;
032800170124             leavesr;
032900170124           endif;
033000170124
033100170124           in  *lock  FIMS25dtAr;
033200170124
033300170124           clear  FIMS25dtAr;
033400170124
033500170124         EndIf;
033600170124
033700170124       ENDSR;
033800170124
033900170124       //--------------------------------------------------------------
034000170124       //?Preparazione stringa SQL per estrazione dati da FLTR700F.    ?
034100170124       //--------------------------------------------------------------
034200170124       BEGSR  sr_PrepSQL;
034300170124
034400170124         wSQL =  'select * +
034500170124                    from FLTR700F +
034600170124                   where TR7ctl = ''2'' +
034700170124                     and TR7npl = ''00'' +
034800170208                     and TR7020 <> ''36'' +
034900170124                     and TR7crc = '' '' +
035000170124                     and TR7bar > ''00000000000000000''';
035100170124
035200170124         if  FIMS25ds.prm_Fil > *zero;
035300170124
035400170124           wSQL += ' and TR7fil = ''' + %editc( FIMS25ds.prm_Fil : 'X' )
035500170124                                + '''';
035600170124
035700170124         endif;
035800170124
035900170124         if  FIMS25ds.prm_Time_Ini > *zero  or
036000170124             FIMS25ds.prm_Time_End > *zero;
036100170124
036200170124           wSQL += ' and TR7tim between ''' +
036300170124                                %char( FIMS25ds.prm_Time_Ini ) +
036400170124                                ''' and ''' +
036500170124                                %char( FIMS25ds.prm_Time_End ) +
036600170124                                '''';
036700170124
036800170124         else;
036900170124
037000170131           wSQL += ' and TR7tim >= ''' +
037100170125                                %editc( FIMS25dtAr.§UltElab_Data : 'X' ) +
037200170125                                %editc( FIMS25dtAr.§UltElab_Ora  : 'X' ) +
037300170131                                '00'' +
037400170201                     and TR7tim < ''' +
037500170201                                %editc( wTime_ds.wDate : 'X' ) +
037600170201                                %editc( wTime_ds.wTime : 'X' ) + '''';
037700170124
037800170124         endif;
037900170124
038000170127         wSQL += ' order by TR7fil, substr(TR7tim, 1, 12), TR7020, +
038100170127                            TR7pep, TR7bar +
038200170127                     for fetch only';
038300170124
038400170124       ENDSR;
038500170124
038600170124       //--------------------------------------------------------------
038700170124       //?Apertura cursore.                                            ?
038800170124       //--------------------------------------------------------------
038900170124       BEGSR  sr_OpenCursor;
039000170124
039100170124         // -?Dichiarazione cursore?
039200170124         exec sql   prepare S1   from :wSQL;
039300170124         exec sql   declare C1   cursor for S1;
039400170124
039500170124         if  SQLcode < *zero;
039600170124           $Fine = *on;
039700170124           exsr  sr_PrintERR;
039800170124         endif;
039900170124
040000170124         // -?Apertura del cursore?
040100170124         exec sql   open C1;
040200170124
040300170124         if  SQLcode < *zero;
040400170124           $Fine = *on;
040500170124           exsr  sr_PrintERR;
040600170124         endif;
040700170124
040800170124       ENDSR;
040900170124
041000170124       //--------------------------------------------------------------
041100170124       //?Lettura cursore.                                             ?
041200170124       //--------------------------------------------------------------
041300170124       BEGSR  sr_ReadCursor;
041400170124
041500170124         $Fine = *off;
041600170124         clear  FLTR7_ds;
041700170124
041800170124         exec sql   fetch next   from C1   into :FLTR7_ds;
041900170124
042000170124         select;
042100170124           when  SQLcode = 100;
042200170124             exsr  sr_Write_FIMSP;
042300170124             $Fine = *on;
042400170124           when  SQLcode < *zero;
042500170124             $Fine = *on;
042600170124             exsr  sr_PrintERR;
042700170124           other;
042800170124             exsr  sr_Elab_FLTR7;
042900170124         endsl;
043000170124
043100170124       ENDSR;
043200170124
043300170124       //--------------------------------------------------------------
043400170124       //?Chiusura cursore.                                            ?
043500170124       //--------------------------------------------------------------
043600170124       BEGSR  sr_CloseCursor;
043700170124
043800170124         // -?Chiusura del cursore?
043900170124         exec sql   close C1;
044000170124
044100170124       ENDSR;
044200170124
044300170124       //--------------------------------------------------------------
044400170124       //?Caricamento dati di FLTR700F nel record di FIMSP00F.         ?
044500170124       //--------------------------------------------------------------
044600170124       BEGSR  sr_Elab_FLTR7;
044700170125
044800170125         // -?Reperimento tab. "MSL" e "MFP" a rottura di filiale?
044900170125         if  TR7fil <> k_TBEke1;
045000170125           exsr  sr_MemoTNTBE;
045100170125         endif;
045200170125
045300170125         // -?Controllo dati nel record di FLTR700F?
045400170125         exsr  sr_CtrlFLTR7;
045500170125         if  Not $RecOK;
045600170125           leavesr;
045700170125         endif;
045800170124
045900170124         // -?Scrittura record a rottura di?
046000170124         //  ?Filiale/Data/Orario/Turno/Impianto/Postazione?
046100170124         If  TR7fil                   <> %editc( MSPfil : 'X' )  or
046200170124             %subst( TR7tim : 1 : 8 ) <> %editc( MSPdtl : 'X' )  or
046300170124             %subst( TR7tim : 9 : 4 ) <> %editc( MSPhml : 'X' )  or
046400170124             wTurno                   <> MSPtur                  or
046500170124             wImpianto                <> MSPimp                  or
046600170124             TR7pep                   <> MSPpos;
046700170124
046800170124           exsr  sr_Write_FIMSP;
046900170124
047000170124           // -?Impostazione dei nuovi campi chiave?
047100170124           MSPfil    = %int( TR7fil );
047200170124           MSPdtl    = %int( %subst( TR7tim : 1 : 8 ) );
047300170125           MSPhml    = %int( %subst( TR7tim : 9 : 4 ) );
047400170124           MSPtur    = wTurno;
047500170124           MSPimp    = wImpianto;
047600170124           MSPpos    = TR7pep;
047700170124
047800170124         EndIf;
047900170124
048000170124         // -?Incremento n° colli letti?
048100170125         wMSPncl   += 1;
048200170124
048300170124       ENDSR;
048400170125
048500170125       //--------------------------------------------------------------
048600170125       //?Intabellamento dati della tab. "MFP" e "MSL" per la filiale. ?
048700170125       //--------------------------------------------------------------
048800170125       BEGSR  sr_MemoTNTBE;
048900170125
049000170125         // -?Tab. "MFP" = Tipo applicazione principale della filiale?
049100170125         clear  dMFP;
049200170125         clear  keyTNTBE01;
049300170125         k_TBEcod = 'MFP';
049400170125         k_TBEke1 = TR7fil;
049500170125         chain  %kds( keyTNTBE01 )  TNTBE000;
049600170125         if  %found( TNTBE01L )  and  TBEatb = *blank;
049700170125           dMFP = TBEuni;
049800170125         endif;
049900170125
050000170125         // -?Tab. "MSL" = Layout postazioni macchinone smistacolli?
050100170125         clear  sk_MSLpep;
050200170125         clear  sk_MSLent;
050300170125         clear  sk_MSLapl;
050400170125         clear  sk_MSLap5;
050500170125         clear  sk_MSLpke;
050600170125         clear  sk_MSLpwf;
050700170125         clear  xx;
050800170125         clear  keyTNTBE01;
050900170125         k_TBEcod = 'MSL';
051000170125         k_TBEke1 = TR7fil;
051100170125         setll  %kds( keyTNTBE01 )  TNTBE000;
051200170125         reade  %kds( keyTNTBE01 : 2 )  TNTBE000;
051300170125         DoW  NOT %eof(TNTBE01L);
051400170125           if  TBEatb = *blank;
051500170125             xx  += 1;
051600170125             dMSL = TBEuni;
051700170125             sk_MSLpep(xx) = dMSL.§MSLpep;      //?- filiale + entrata?
051800170125             sk_MSLent(xx) = dMSL.§MSLent;      //?- n° Entrance Point?
051900170125             sk_MSLapl(xx) = dMSL.§MSLapl;      //?- TipoApplic. Cat.2?
052000170125             sk_MSLap5(xx) = dMSL.§MSLap5;      //?- TipoApplic. Cat.5?
052100170125             sk_MSLpke(xx) = dMSL.§MSLpke;      //?- S=Entrata Picking?
052200170125             sk_MSLpwf(xx) = dMSL.§MSLpwf;      //?- W=Pistola Wi-Fi?
052300170125           endif;
052400170125           reade  %kds( keyTNTBE01 : 2 )  TNTBE000;
052500170125         EndDo;
052600170125
052700170125       ENDSR;
052800170124
052900170124       //--------------------------------------------------------------
053000170124       //?Controllo dati nel record di FLTR700F.                       ?
053100170124       //--------------------------------------------------------------
053200170124       BEGSR  sr_CtrlFLTR7;
053300170124
053400170125         clear  $RecOK;
053500170124         clear  wTurno;
053600170124         clear  wImpianto;
053700170124
053800170125         // -?Controllo numericità del campo TR7BAR?
053900170125         if  %check( c_Digits : TR7bar ) > *zero;
054000170125           leavesr;
054100170125         endif;
054200170125
054300170125
054400170125         // -?Reperimento Spedizione e Distinta per determinare il?
054500170125         //  ?Turno Picking?
054600170125         wBarCode_BRT = TR7bar;
054700170125         clear  keyFIART27;
054800170125         k_ARTfls = wBarCode_BRT.wBar_LNP;
054900170125         k_ARTlna = wBarCode_BRT.wBar_LNA;
055000170125         k_ARTnrs = wBarCode_BRT.wBar_NRS;
055100170125         k_ARTnsc = wBarCode_BRT.wBar_NSC;
055200170125         chain  %kds( keyFIART27 )  FIART000;
055300170125         if  Not %found(FIART27L);
055400170125           leavesr;
055500170125         endif;
055600170125
055700170125         clear  dDSTflr;
055800170125         clear  keyFNARB01;
055900170125         k_ARBaas = ARTaas;
056000170125         k_ARBlnp = ARTlnp;
056100170125         k_ARBnrs = ARTnrs;
056200170125         k_ARBnsp = ARTnsp;
056300170125         chain  %kds( keyFNARB01 )  FNARB000;
056400170125         if  Not %found(FNARB01L);
056500170125           leavesr;
056600170125         endif;
056700170126
056800170126         Select;
056900170126
057000170126           // -?Spedizione in distinta "fittizia"?
057100170126           When  ARBndc = *all'9'  or
057200170126                 ARBndc = *all'8';
057300170126             leavesr;
057400170125
057500170126           // -?Spedizione NON assegnata a nessuna distinta?
057600170126           When  ARBndc = *zero;
057700170126
057800170126             // -?Filiale Spunta <> LNA?
057900170126             if  TR7fil <> %editc( ARBlna : 'X' );
058000170126               leavesr;
058100170126             endif;
058200170126
058300170126             // -?Filiale Spunta =  LNA?
058400170126             wTurno = 'N';
058500170126
058600170126           // -?Spedizione in distinta?
058700170126           Other;
058800170126
058900170126             // -?Filiale Spunta <> Filiale Distinta?
059000170126             if  TR7fil <> %subst( %editc( ARBifp : 'X' ) :
059100170126                           %len( ARBifp ) - 2 : 3 );
059200170126               leavesr;
059300170126             endif;
059400170126
059500170126             // -?Filiale Spunta =  Filiale Distinta?
059600170126             // -?Reperimento Distinta consegna?
059700170126             //  ?per risalire al Turno Picking Consegna (2 o *blank)?
059800170126             clear  keyFIDST01;
059900170126             k_DSTnpg = 4;
060000170126             k_DSTnfv = ARBndc;
060100170126             k_DSTfgs = ARBifp;
060200170126             chain  %kds( keyFIDST01 )  FIDST000;
060300170126             if  %found(FIDST01L);
060400170126               dDSTflr = DSTflr;
060500170126             endif;
060600170126             // -?Turno "2"?
060700170126             If  dDSTflr.§DSTturno <> *blank;
060800170126               wTurno = dDSTflr.§DSTturno;
060900170126             // -?Turno *blank: "1" o unico...?
061000170126             Else;
061100170126               // -?Turno "1" x fil. abilitata al Turno Picking Consegna?
061200170126               if  %lookup( k_DSTfgs : sk_FilTurno ) > *zero;
061300170126                 wTurno = '1';
061400170126               // -?Turno "U" x fil. NON abilitata al Turno Picking Consegna?
061500170126               else;
061600170126                 wTurno = 'U';
061700170126               endif;
061800170126             EndIf;
061900170125
062000170126         EndSl;
062100170125
062200170125
062300170125         // -?Tipo Impianto?
062400170125         xx = %lookup( TR7pep : sk_MSLpep );
062500170125         select;
062600170125           when  xx > *zero  and  sk_MSLpwf(xx) = *blank;
062700170125             wImpianto = sk_MSLapl(xx);
062800170125           when  xx > *zero  and  sk_MSLpwf(xx) = 'W';
062900170125             wImpianto = sk_MSLpwf(xx);
063000170125           when  xx = *zero;
063100170125             wImpianto = dMFP.§MFPapl;
063200170125         endsl;
063300170125
063400170125
063500170125         // -?Lettura da elaborare?
063600170125         $RecOK = *on;
063700170124
063800170124       ENDSR;
063900170124
064000170124       //--------------------------------------------------------------
064100170124       //?Scrittura del record nel *file FIMSP00F.                     ?
064200170124       //--------------------------------------------------------------
064300170124       BEGSR  sr_Write_FIMSP;
064400170124
064500170124         // -?Nessun dato elaborato dopo l'ultima scrittura?
064600170125         if  wMSPncl = *zero;
064700170124           leavesr;
064800170124         endif;
064900170124
065000170125         // -?Scrittura/Aggiornamento dati nel record del *file FIMSP00F?
065100170125         clear  keyFIMSP01;
065200170125         k_MSPfil = MSPfil;
065300170125         k_MSPdtl = MSPdtl;
065400170125         k_MSPhml = MSPhml;
065500170125         k_MSPtur = MSPtur;
065600170125         k_MSPimp = MSPimp;
065700170125         k_MSPpos = MSPpos;
065800170125         chain  %kds( keyFIMSP01 )  FIMSP000;
065900170125
066000170125         if  NOT %found( FIMSP01L );
066100170125           clear  FIMSP000;
066200170125           MSPfil = k_MSPfil;
066300170125           MSPdtl = k_MSPdtl;
066400170125           MSPhml = k_MSPhml;
066500170125           MSPtur = k_MSPtur;
066600170125           MSPimp = k_MSPimp;
066700170125           MSPpos = k_MSPpos;
066800170125         endif;
066900170125
067000170125         MSPncl  += wMSPncl;
067100170125
067200170125         if  NOT %found( FIMSP01L );
067300170125           //______________
067400170125           write  FIMSP000;
067500170125           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
067600170125         else;
067700170125           //_______________
067800170125           update  FIMSP000;
067900170125           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
068000170125         endif;
068100170124
068200170124         clear  FIMSP000;
068300170125         clear  wMSPncl;
068400170124
068500170124       ENDSR;
068600170124
068700170124       //--------------------------------------------------------------
068800170124       //?Stampa segnalazione dell'errore rilevato via SQL             ?
068900170124       //--------------------------------------------------------------
069000170124       BEGSR  sr_PrintERR;
069100170124
069200170124         // -?Stampa del Dump?
069300170124         Dump(A);
069400170124
069500170124         // -?Stampa del Job-Log?
069600170124         Qcmd = 'DSPJOBLOG job(*) output(*print)';
069700170124         exsr  sr_ExecCmd;
069800170127
069900170127         // -?Aggiornamento Area Dati con l'orario dell'ultimo rec.?
070000170127         //  ?(se già elaborato almeno un record)?
070100170127         if  TR7tim  <> *blank;
070200170127           wTimeStamp = %date( %int( %subst( TR7tim : 1 : 8 ) ) : *iso )
070300170127                      + %time( %int( %subst( TR7tim : 9 : 6 ) ) : *hms )
070400170127                      + %minutes(1);
070500170127           wTime_ds   = %subst( %char( %dec( wTimeStamp ) ) : 1 : 12 )
070600170127                      + '00';
070700170127           FIMS25dtAr.§UltElab_Data = wTime_ds.wDate;
070800170127           FIMS25dtAr.§UltElab_Ora  = wTime_ds.wTime;
070900170127           OUT  FIMS25dtAr;
071000170127         endif;
071100170124
071200170124         // -?Chiusura del programma?
071300170124         //*·exsr  sr_RoutEnd;
071400170124         return;
071500170124
071600170124       ENDSR;
071700170124
071800170124       //--------------------------------------------------------------
071900170124       //?Esecuzione del comando (già impostato).                      ?
072000170124       //--------------------------------------------------------------
072100170124       BEGSR  sr_ExecCmd;
072200170124
072300170124         clear Qcap0100;
072400170124         Qcabcsdh = *off;
072500170124         Qcapa    = *off;
072600170124         Qcacmdss = *off;
072700170124         Qcaerved = *allX'00';
072800170124
072900170124         clear Qusec;
073000170124         Qusbprv  = %size(Qusec);
073100170124
073200170124         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
073300170124                           %size(Qcap0100) : 'CPOP0100' : *omit :
073400170124                           0 : 0 : Qusec);
073500170124
073600170127         //*·if  Qusei <> *blank;
073700170127         //*·  ...;
073800170127         //*·endif;
073900170124
074000170124       ENDSR;
074100170124
074200170124       //--------------------------------------------------------------
074300170124       //?Operazioni finali.                                           ?
074400170124       //--------------------------------------------------------------
074500170124       BEGSR  sr_RoutEnd;
074600170124
074700170124         // -?Aggiornamento Area Dati?
074800170127         //  ?(wTime_ds è già impostata con data e ora limite)?
074900170131         //  ?SE  non  ricevute parzializzazioni in KPJBU?
075000170131         if  FIMS25ds.prm_Fil      = *zero  and
075100170131             FIMS25ds.prm_Time_Ini = *zero  and
075200170131             FIMS25ds.prm_Time_End = *zero;
075300170131           //*·IN  *lock  FIMS25dtAr;
075400170131           FIMS25dtAr.§UltElab_Data = wTime_ds.wDate;
075500170131           FIMS25dtAr.§UltElab_Ora  = wTime_ds.wTimeHM;
075600170131           OUT  FIMS25dtAr;
075700170131         endif;
075800170124
075900170124         // -?Uscita dal *pgm?
076000170124         return;
076100170124
076200170124       ENDSR;
076300170124
076400170124      /end-free

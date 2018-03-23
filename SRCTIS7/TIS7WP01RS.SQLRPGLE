000100160923       //==============================================================
000200160919       // WS PERS. Back-end AmazonShipTrackAPI v01
000300160923       //==============================================================
000400091228
000500091228
000600160202     **
000700160202     ** ISTRUZIONI PER LA COMPILAZIONE
000800160202     **
000900160923     ** 1. Creare il modulo TIS7WP01R (Opz 15 PDM)
001000160923     ** 2. Creare/Aggiornare il programma di servizio TIS7WP01R (CRTSRVPGM / UPDSRVPGM)
001100160922     **    con ACTGRP(*caller) e BNDDIR=UBBNDDIR
001200160922     **    CRTSRVPGM SRVPGM(GAITRAOBJ/TIS7WP01R) EXPORT(*ALL) BNDDIR(UBBNDDIR)
001300160922     **              TEXT('WS PERS. Back-end AmazonShipTrackAPI v01')
001400160923     ** 3. Creare/Aggiornare il programma TIS7WP01R (CRTPGM / UPDPGM)
001500160922     **    con ACTGRP(*caller) e BNDDIR=UBBNDDIR
001600160922     **    CRTPGM PGM(GAITRAOBJ/TIS7WP01R) BNDDIR(UBBNDDIR) ACTGRP(*CALLER)
001700160202     ** 4. Cancellare il modulo.
001800160202     **
001900091228
002000091228       //--------------------------------------------------------------
002100121106       // Specifiche di controllo.
002200091228       //--------------------------------------------------------------
002300091223
002400160921     H decedit('0,') datedit(*dmy/) option(*nodebugio)
002500160921     H alwnull(*inputonly)
002600091223
002700091223       //--------------------------------------------------------------
002800121106       // Dichiarazione file.
002900091223       //--------------------------------------------------------------
003000160921     Fazorg01l  if   e           k disk
003100160923     Ftita430c  if   e           k disk
003200160531
003300160921
003400091223       //--------------------------------------------------------------
003500121106       // Definizione costanti.
003600091223       //--------------------------------------------------------------
003700100302
003800091223
003900091223       //--------------------------------------------------------------
004000121106       // Definizione schiere.
004100091223       //--------------------------------------------------------------
004200091223
004300091223
004400091223       //--------------------------------------------------------------
004500121106       // Definizione aree dati.
004600091223       //--------------------------------------------------------------
004700160802
004800160919
004900091223       //--------------------------------------------------------------
005000160919       // Definizione variabili e strutture dati.
005100091223       //--------------------------------------------------------------
005200161011      /COPY STRABASRC/SRCWSER,WPER0010D
005300161010      /COPY GAITRASRC/SRCPROTOPI,UBISVPWDR
005400161010      /COPY GAITRASRC/SRCPROTOPI,TIS7WP01R
005500160920      /COPY GAITRASRC/SRCPROTOPR,UBRTVSPE
005600160920      /COPY GAITRASRC/SRCPROTOPI,UBRTVSPE
005700160920      /COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
005800160920      /COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
005900160921      /COPY GAITRASRC/SRCPROTOPR,UBVMSB2CR
006000160921      /COPY GAITRASRC/SRCPROTOPI,UBVMSB2CR
006100121106
006200160919     D TISTBDS       e ds                  inz extname(tistb00f)
006300160923     D TISTBDDS      e ds                  inz extname(tistbd0f)
006400160924     D wSQL            s           2048a   inz varying
006500160919     D wCount          s              1s 0 inz
006600160921     D wSTBAAS         s                   inz like(stbAAS)
006700160921     D wSTBLNP         s                   inz like(stbLNP)
006800160921     D wSTBNRS         s                   inz like(stbNRS)
006900160921     D wSTBNSP         s                   inz like(stbNSP)
007000160923     D wSTBDDAD        s                   inz like(stbDDAD)
007100160923     D wSTBDORD        s                   inz like(stbDORD)
007200160926     D wCli_PROC       s                   inz like(iVMSB2CProc)
007300160921     D wFiliale        s                   inz like(orgFIL)
007400160921     D wLNP            s                   inz like(orgFIL)
007500160921     D wLNA            s                   inz like(orgFIL)
007600160926     D ErrCode         s             32A   inz varying
007700160926     D ErrCodeDesc     s            256A   inz varying
007800160930     D skCOS           s                   inz like(stbCOS) dim(100)
007900160930     D ixCOS           s              3S 0 inz
008000160923
008100160802
008200091223       //--------------------------------------------------------------
008300160603       // Definizione costanti.
008400091223       //--------------------------------------------------------------
008500160921      /COPY STRABASRC/SRCCONST,WSER000000
008600160921      /COPY STRABASRC/SRCCONST,WPER0010W
008700160926     D Cli_PROC        c                   const('TITVED01R')
008800160923     D wApi            c                   const('''')
008900130307
009000160531
009100160603       //--------------------------------------------------------------
009200160603       // Definizione prototipi procedure.
009300160603       //--------------------------------------------------------------
009400160603
009500160603
009600091223       //--------------------------------------------------------------
009700121106       // Definizione key-list.
009800091223       //--------------------------------------------------------------
009900091223
010000091223
010100091223       //--------------------------------------------------------------
010200121107       // Definizione parametri procedura.
010300091223       //--------------------------------------------------------------
010400091223
010500091223     c     *Entry        plist
010600161011     c                   parm                    Request
010700161011     c                   parm                    Response
010800091223
010900160924      /free
011000160924
011100091223       //--------------------------------------------------------------
011200121106       // M A I N - L I N E
011300091223       //--------------------------------------------------------------
011400140806
011500160805       // Monitorizzo tutto (per necessità massima solidità stack chiamate)
011600160805       Monitor;
011700161011
011800161011       // Ridefinisco il passaggio parametri "flat" nelle relative strutture dati
011900161011       TIS7WP01DI = Request;
012000160929
012100160929       // Gestioen Versioni ?
012200160929       exsr exeVersioni;
012300160804
012400160805       // Fine monitoring
012500160805       On-Error;
012600160926
012700160926          ErrCode     = WS_AMAZON_TRACKING_SERVICE_UNAVAILABLE;
012800160926          ErrCodeDesc = WS_AMAZON_TRACKING_SERVICE_UNAVAILABLE_DESC;
012900161010          TIS7WP01DO.Esito = WSER_ESITO_ERRORE_GENERICO;
013000160926          exsr exeUscita;
013100160926
013200160919       Endmon;
013300160919
013400160919       // Uscita
013500160919       exsr exeUscita;
013600160929
013700160929
013800160929       //--------------------------------------------------------------
013900160929       // Gestione versioni
014000160929       //--------------------------------------------------------------
014100160929       BEGSR  exeVersioni;
014200160929
014300160929         select;
014400161010           when TIS7WP01DI.APIVersion = 101;
014500160929
014600160929              // Operazioni?iniziali
014700160929              exsr sr_RoutInz101;
014800160929
014900160929              // Se OK recupero tutti i dati (Spedizione ed Eventi)
015000160929              exsr RtvDati101;
015100160929
015200160929           other;
015300160929
015400160929              ErrCode     = WS_AMAZON_INVALID_API_VERSION;
015500160929              ErrCodeDesc = WS_AMAZON_INVALID_API_VERSION_DESC;
015600161010              TIS7WP01DO.Esito = WSER_ESITO_ERRORE_VERSIONE_ERRATA;
015700160929              exsr exeUscita;
015800160929
015900160929         endsl;
016000160929
016100160929       ENDSR;
016200160802
016300160802
016400160601       //--------------------------------------------------------------
016500160601       //
016600160601       //--------------------------------------------------------------
016700160601       BEGSR  *inzsr;
016800160601
016900160601         // Inizializzazioni parametri esecuzione comandi sql
017000160802         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
017100160601
017200160601       ENDSR;
017300091223
017400160802
017500091223       //--------------------------------------------------------------
017600121107       // Operazioni iniziali.
017700091223       //--------------------------------------------------------------
017800160929       BEGSR  sr_RoutInz101;
017900160601
018000160926         // Inizializzo variabili di errore Tracking
018100160926         clear ErrCode;
018200160926         clear ErrCodeDesc;
018300160930
018400160930         // Inizializzo variabili di work
018500160930         clear skCOS;
018600160930         clear ixCOS;
018700161011
018800161011         // Inizializzo i parametri di output
018900161011         clear TIS7WP01DO;
019000161011         clear Response;
019100160926
019200160921         // Inizializzo l'esito a errore generico
019300161010         TIS7WP01DO.Esito = WSER_ESITO_ERRORE_GENERICO;
019400160919
019500160608         // Controllo i parametri di input
019600160919         exsr sr_CtrlPIn;
019700160202
019800160202       ENDSR;
019900160802
020000160804
020100160202       //--------------------------------------------------------------
020200160202       // Controllo i parametri di input
020300160202       //--------------------------------------------------------------
020400160202       BEGSR  sr_CtrlPIn;
020500140724
020600160919         // Deve essere necessariamente passato un valore di Tracking
020700161010         if TIS7WP01DI.TrkNumber = *blanks;
020800160921
020900160926            ErrCode     = WS_AMAZON_TRACKING_SERVICE_UNAVAILABLE;
021000160926            ErrCodeDesc = WS_AMAZON_TRACKING_SERVICE_UNAVAILABLE_DESC;
021100161010            TIS7WP01DO.Esito =
021200161010                       WSER_ESITO_ERRORE_PARAMETRO_OBBLIGATORIO_NON_PASSATO;
021300160926            exsr exeUscita;
021400160921
021500160802         endif;
021600091223
021700091223       ENDSR;
021800121106
021900160802
022000091223       //--------------------------------------------------------------
022100160217       // Esecuzione
022200091223       //--------------------------------------------------------------
022300160929       BEGSR  RtvDati101;
022400160531
022500161011         // Compongo dinamicamente l'istruzione SQL
022600160923         wSQL = ' SELECT' +
022700160926                ' STBKSU, STBAAS, STBLNP, STBNRS, STBNSP, STBTIS, ' +
022800160926                ' STBPRS, STBCOS, STBFOP, STBSTS, STBDAS, STBORS, ' +
022900160924                ' case when STBDDAD is null then 0 ' +
023000160924                ' else STBDDAD end as STBDDAD,' +
023100160924                ' case when STBDORD is null then 0 ' +
023200160924                ' else STBDORD end as STBDORD' +
023300160923                ' FROM tistb00f left join tistbd0f on' +
023400160923                ' STBKSU=STBDKSU and STBAAS=STBDAAS and STBLNP=STBDLNP and' +
023500160923                ' STBNRS=STBDNRS and STBNSP=STBDNSP and STBDAS=STBDDAS and' +
023600160923                ' STBORS=STBDORS and STBCOS=STBDCOS' +
023700160924                ' WHERE STBKSU in (' +
023800161010                ' SELECT DISTINCT ' + %trim(TIS7WP01DI.CliUnif) +
023900161010                ' FROM TIKUN00F' +
024000160919                ' UNION ALL' +
024100160919                ' SELECT KUNCOF FROM tikun00f' +
024200160923                ' WHERE KUNVAT='' '' ' +
024300161010                ' and KUNTLE=' + wApi + TIS7WP01DI.TipoLegame + wApi +
024400160923                ' and KUNAZP='' ''' +
024500161010                ' and KUNCOP=' + %trim(TIS7WP01DI.CliUnif) + ' and' +
024600160922                ' (select DC_YYMD_DEC from DATECNV0F' +
024700160922                '  where DC_DATE = curdate()) between KUNDDE and KUNDSC)' +
024800161010                ' and STBRMN=' + %trim(TIS7WP01DI.TrkNumber) +
024900160919                ' ORDER BY STBDAS desc, STBORS desc' +
025000160919                ' for read only';
025100160802
025200160531         // Dichiarazione cursore
025300160802         exec sql   prepare S0   from :wSQL;
025400160531         exec sql   declare C0   cursor   for S0;
025500160802
025600160531         // Apro il cursore
025700160531         exec sql open C0;
025800160802
025900160531         // Leggo i dati
026000160531         exec sql
026100160926             fetch next from C0 into :STBKSU, :STBAAS, :STBLNP, :STBNRS,
026200160923                   :STBNSP, :STBTIS, :STBPRS, :STBCOS,  :STBFOP,
026300160924                   :STBSTS, :STBDAS, :STBORS, :STBDDAD, :STBDORD;
026400160923
026500160919         select;
026600160919           // Se errore SQL probabile UTENTE errato
026700160919           when sqlCode < 0;
026800160926              ErrCode     = WS_AMAZON_TRACKING_SERVICE_UNAVAILABLE;
026900160926              ErrCodeDesc = WS_AMAZON_TRACKING_SERVICE_UNAVAILABLE_DESC;
027000161010              TIS7WP01DO.Esito = WSER_ESITO_ERRORE_SQL_REPERIMENTO_DATI;
027100160919              exsr exeUscita;
027200160919
027300160919           // Se Tracking inesistente
027400160919           when sqlCode = 100;
027500160926              ErrCode     = WS_AMAZON_NO_TRACKING_INFO_FOUND;
027600160926              ErrCodeDesc = WS_AMAZON_NO_TRACKING_INFO_FOUND_DESC;
027700161010              TIS7WP01DO.Esito = WSER_ESITO_ERRORE_PARAMETRO_NON_VALIDO;
027800160919              exsr exeUscita;
027900160919
028000160919           // Diversamente Tracking trovato e stati reperiti
028100160919           other;
028200160919              // Se chiave Sped OK procedo
028300160921              if stbAAS <> *zeros and
028400160923                 stbLNP <> *zeros and
028500160921                 stbNSP <> *zeros;
028600160919
028700160919                 // Salvo la chiave spedizione corrente
028800160921                 wSTBAAS = stbAAS;
028900160921                 wSTBLNP = stbLNP;
029000160921                 wSTBNRS = stbNRS;
029100160921                 wSTBNSP = stbNSP;
029200160919
029300160926                 // Reperisco ID mappatura da utilizzare
029400160926                 exsr rtvMapp;
029500160926
029600160919                 // Recupero i dati della spedizione
029700160921                 exsr RtvDatiSped;
029800160921
029900160921                 // A questo punto valorizzo il buffer di output
030000160921                 exsr ValOut;
030100160919
030200160921              else;
030300160926                 ErrCode     = WS_AMAZON_NO_TRACKING_INFO_FOUND;
030400160926                 ErrCodeDesc = WS_AMAZON_NO_TRACKING_INFO_FOUND_DESC;
030500161010                 TIS7WP01DO.Esito = WSER_ESITO_ERRORE_PARAMETRO_NON_VALIDO;
030600160921                 exsr exeUscita;
030700160921              endif;
030800160919
030900160921         endsl;
031000161011
031100161011         // Ritorno struttura dati di output alla Response di chiamata
031200161011         Response = TIS7WP01DO;
031300160217
031400160217       ENDSR;
031500160920
031600160920
031700160920       //--------------------------------------------------------------
031800160920       // Reperimento dati spedizione
031900160920       //--------------------------------------------------------------
032000160920       BEGSR RtvDatiSped;
032100160920
032200160920         // Reperisco i dati spedizione (in qualunque fase)
032300160922         UBRTVSPE_S ( 'APS'
032400160922                     : stbAAS
032500160922                     : stbLNP
032600160922                     : stbNRS
032700160922                     : stbNSP
032800160922                     : oRTVSPE_TASDS
032900160922                     : oRTVSPE_ARBDS
033000160922                     : oRTVSPE_BLPDS
033100160922                     : oRTVSPE_Esito
033200160922                    );
033300160920
033400160921         clear wLNP;
033500160921         clear wLNA;
033600160921
033700160920         // A seconda della "sorgente" dati valorizzo
033800160920         select;
033900160920
034000160920           // Partenza
034100160920           when %subst(oRTVSPE_Esito:1:2) = ' P';
034200161010             TIS7WP01DO.TrkInfoDta.TrackingNumber = %trim(TIS7WP01DI.TrkNumber);
034300161010             TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.Street1 =
034400160923                            %trim(blpIND);
034500161010             TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.Street2 =
034600160923                            '';
034700161010             TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.City =
034800160923                            %trim(blpLOD);
034900161010             TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.StateProvince =
035000160923                            %trim(blpPRD);
035100161010             TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.PostalCode =
035200160923                            %trim(blpCAD);
035300160921             if blpNZD = *blanks;
035400161010                TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.
035500161010                                      CountryCode = 'IT';
035600160921             else;
035700161010                TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.
035800161010                                      CountryCode = blpNZD;
035900160921             endif;
036000160922             if blpDCR > *zeros;
036100161010                TIS7WP01DO.TrkInfoDta.PackageDeliveryDate.
036200161010                                      ScheduledDeliveryDate =
036300160921                    UBFMTDATE_Convert( %editc(blpDCR:'X')
036400160921                              : 'YYYYMMDD' : 'YYYY-MM-DD');
036500160922             endif;
036600160922
036700160921             wLNP = blpLNP;
036800160921             wLNA = blpLNA;
036900160920
037000160921
037100160920           // Arrivo
037200160920           when %subst(oRTVSPE_Esito:1:1) = 'A';
037300161010             TIS7WP01DO.TrkInfoDta.TrackingNumber = %trim(TIS7WP01DI.TrkNumber);
037400161010             TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.Street1 =
037500160923                            %trim(arbIND);
037600161010             TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.Street2 =
037700160923                            '';
037800161010             TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.City =
037900160923                            %trim(arbLOD);
038000161010             TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.StateProvince =
038100160923                            %trim(arbPRD);
038200161010             TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.PostalCode =
038300160923                            %trim(arbCAD);
038400160921             if arbNZD = *blanks;
038500161010                TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.
038600161010                                      CountryCode = 'IT';
038700160921             else;
038800161010                TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.
038900161010                                      CountryCode = arbNZD;
039000160921             endif;
039100160922             if arbDCR > *zeros;
039200161010                TIS7WP01DO.TrkInfoDta.PackageDeliveryDate.
039300161010                                      ScheduledDeliveryDate =
039400160922                    UBFMTDATE_Convert( %editc(arbDCR:'X')
039500160922                              : 'YYYYMMDD' : 'YYYY-MM-DD');
039600160922             endif;
039700160922
039800160921             wLNP = arbLNP;
039900160921             wLNA = arbLNA;
040000160921
040100160920
040200160920           // Sede
040300160920           when %subst(oRTVSPE_Esito:3:1) = 'S';
040400161010             TIS7WP01DO.TrkInfoDta.TrackingNumber = %trim(TIS7WP01DI.TrkNumber);
040500161010             TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.Street1 =
040600160923                            %trim(tasIND);
040700161010             TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.Street2 =
040800160923                            '';
040900161010             TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.City =
041000160923                            %trim(tasLOD);
041100161010             TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.StateProvince =
041200160923                            %trim(tasPRD);
041300161010             TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.PostalCode =
041400160923                            %trim(tasCAD);
041500160921             if tasNZD = *blanks;
041600161010                TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.
041700161010                                      CountryCode = 'IT';
041800160921             else;
041900161010                TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.
042000161010                                      CountryCode = tasNZD;
042100160921             endif;
042200160922             if tasDCR > *zeros;
042300161010                TIS7WP01DO.TrkInfoDta.PackageDeliveryDate.
042400161010                                      ScheduledDeliveryDate =
042500160922                    UBFMTDATE_Convert( %editc(tasDCR:'X')
042600160922                              : 'YYYYMMDD' : 'YYYY-MM-DD');
042700160922             endif;
042800160922
042900160921             wLNP = tasLNP;
043000160921             wLNA = tasLNA;
043100160920
043200160920         endsl;
043300160920
043400160920       ENDSR;
043500160921
043600160921
043700160921       //--------------------------------------------------------------
043800160921       // Scorro gli stati e valorizzo il buffer di output
043900160921       //--------------------------------------------------------------
044000160921       BEGSR  ValOut;
044100160921
044200160921         // Scorro tutto il recordset reperito
044300160921         dow sqlCode >= *zeros AND sqlCode <> 100 AND
044400160921             wSTBAAS  = stbAAS AND
044500160921             wSTBLNP  = stbLNP AND
044600160921             wSTBNRS  = stbNRS AND
044700160921             wSTBNSP  = stbNSP;
044800160921
044900160921             // Considero solamente i record con le caratteristiche necessarie
045000160921             if (
045100160921                 (stbTIS='1' AND stbPRS<>'7') OR
045200160921                 (stbTIS='1' AND stbPRS ='7'
045300160921                             AND stbCOS ='CONSOK') OR
045400160921                 (stbTIS='1' AND stbPRS ='8'
045500160921                             AND stbCOS ='SPUNTNPG5') OR
045600160921                 (stbTIS='9' AND stbPRS ='1')
045700160921                 ) AND
045800160921                 (%subst(stbFOP:1:1)= 'O' OR
045900160921                 (%subst(stbFOP:1:1)= 'F' AND stbSTS= '9')
046000160921                );
046100160923
046200160923
046300160923              // Considero solamente gli eventi con data e ora valorizzati
046400160924              if (stbDAS  > *zeros AND stbORS  > *zeros) OR
046500160924                 (stbDDAD > *zeros AND stbDORD > *zeros);
046600160930
046700160930
046800160930              // Considero solamente gli eventi solamente 1 volta
046900160930              if  %lookup(stbCOS:skCOS) = 0 OR ixCOS >= %elem(skCOS);
047000160930
047100160930                 // Salvo l'evento corrente nella lista degli eventi già ritornati
047200160930                 ixCOS = ixCOS + 1;
047300160930                 skCOS(ixCOS) = stbCOS;
047400160926
047500160926                 // Effettuo mappatura tra causale BRT e stato CLIENTE
047600160923                 UBVMSB2CR_S (  *blanks
047700160923                              : *zeros
047800160923                              : *blanks
047900160926                              : wCli_PROC
048000160926                              : *blanks
048100160923                              : stbCOS
048200160923                              : oVMSB2CEsito
048300160923                              : oVMSB2CCauTip
048400160923                              : oVMSB2CCauTT
048500160923                              : oVMSB2CStsCli
048600160923                              : oVMSB2CStsOpz
048700160923                              : oVMSB2CFasOp
048800160923                              : oVMSB2CDDE
048900160923                              : oVMSB2CDesc
049000160923                             );
049100160921
049200160921               // Se mappatura riuscita e stato CLIENTE da ritornare
049300160921               if oVMSB2CEsito   = '1' AND
049400160923                  oVMSB2CStsCli <> 'NONINVIARE' AND
049500160923                  oVMSB2CStsCli <> *blanks;
049600160923
049700160923
049800160923                  // Se evento che prevede "dettagli" considero data/ora del dettaglio
049900160923                  if oVMSB2CStsOpz = '*DETT' AND
050000160924                           STBDDAD > *zeros  AND
050100160924                           STBDORD > *zeros;
050200160923                     wSTBDDAD = STBDDAD;
050300160923                     wSTBDORD = STBDORD;
050400160923                  else;
050500160923                     wSTBDDAD = STBDAS;
050600160923                     wSTBDORD = STBORS*100;
050700160923                  endif;
050800160923
050900160922
051000160922                  // Incremento il contatore di eventi per Tracking corrente
051100161010                  TIS7WP01DO.EventCount = TIS7WP01DO.EventCount + 1;
051200160922
051300160922                  // Inizializzo la schiersa degli Eventi del Tracking corrente
051400161010                  clear TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
051500161010                              TrackingEventDetail(TIS7WP01DO.EventCount);
051600160922
051700160922                  // Valorizzo i dati di ritorno in output
051800161010                  TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
051900161010                     TrackingEventDetail(TIS7WP01DO.EventCount).EventStatus =
052000160921                              %trim(%subst(oVMSB2CStsCli:1:10));
052100160921
052200161010                  TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
052300161010                     TrackingEventDetail(TIS7WP01DO.EventCount).EventReason =
052400160921                              %trim(%subst(oVMSB2CStsCli:11:5));
052500160921
052600161010                  TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
052700161010                     TrackingEventDetail(TIS7WP01DO.EventCount).EventDateTime =
052800160921                         UBFMTDATE_Convert(
052900160923                            %trim(%char(wSTBDDAD*1000000+wSTBDORD)) :
053000160921                           'YYYYMMDDHHMISS' : 'YYYY-MM-DDTHH:MI:SS UTC');
053100160921
053200160921                  // Calcolo la location del evento
053300160921                  exsr exeEventLocation;
053400160921
053500161010 xxx            // TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
053600161010                //     TrackingEventDetail(TIS7WP01DO.EventCount).
053700161010                //     EstimatedDeliveryDate ;
053800160921
053900160921
054000161010                  TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
054100161010                      TrackingEventDetail(TIS7WP01DO.EventCount).
054200160923                         AdditionalLocationInfo = '';
054300160923
054400160923                  // Se evento di consegna reperisco la FIRMA del consegnatario merce
054500160923                  if  %subst(stbCOS:1:6) = 'CONSOK';
054600160923                      chain (stbAAS:stbLNP:stbNRS:stbNSP:'I') tita430c;
054700160923                      if  %found(tita430c);
054800161010                          TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
054900161010                              TrackingEventDetail(TIS7WP01DO.EventCount).
055000160923                                 SignedForByName = %trim(ta4NOT);
055100160923                      endif;
055200160923                  endif;
055300160921
055400161010                  TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
055500161010                      TrackingEventDetail(TIS7WP01DO.EventCount).
055600160923                         DeliveryAppointmentWindow.Day = '';
055700160921
055800161010                  TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
055900161010                      TrackingEventDetail(TIS7WP01DO.EventCount).
056000160923                         DeliveryAppointmentWindow.StartTime = '';
056100160921
056200161010                  TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
056300161010                     TrackingEventDetail(TIS7WP01DO.EventCount).
056400160923                         DeliveryAppointmentWindow.EndTime = '';
056500160921
056600161010                  TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
056700161010                     TrackingEventDetail(TIS7WP01DO.EventCount).
056800160923                        DeliveryAppointmentWindow.TimeZone = '';
056900160921
057000160930               endif;
057100160923               endif;
057200160923
057300160923              endif;
057400160921
057500160921             endif;
057600160921
057700160921             // Leggo il record successivo
057800160921             exec sql
057900160926                  fetch next from C0 into :STBKSU, :STBAAS, :STBLNP, :STBNRS,
058000160923                        :STBNSP, :STBTIS, :STBPRS, :STBCOS,  :STBFOP,
058100160924                        :STBSTS, :STBDAS, :STBORS, :STBDDAD, :STBDORD;
058200160921
058300160921         enddo;
058400160924
058500160921         // Valorizzo esito a OK
058600161010         TIS7WP01DO.Esito = WSER_ESITO_OK;
058700160921
058800160921       ENDSR;
058900160926
059000160926
059100160926       //--------------------------------------------------------------
059200160926       // Reperisce ID mappatura da utilizzare
059300160926       //--------------------------------------------------------------
059400160926       BEGSR  rtvMapp;
059500160926
059600160926         // Innanzitutto imposto il default
059700160926         wCli_PROC = Cli_PROC;
059800160926
059900160926         wSQL = ' SELECT VIRPGI FROM tivir00f' +
060000160926                ' WHERE VIRKSC = ' + wApi + '0' + %editc(stbKSU:'X') + wApi +
060100160926                ' and VIRTIP=''VC'' and VIRPGT=''TITV67R'' and VIRPGI<>'' ''' +
060200160926                ' and ' +
060300160926                ' (select DC_YYMD_DEC from DATECNV0F' +
060400160926                '  where DC_DATE = curdate()) between VIRDTI and VIRDTF ' +
060500160926                ' for read only';
060600160926
060700160926         // Dichiarazione cursore
060800160926         exec sql   prepare S1   from :wSQL;
060900160926         exec sql   declare C1   cursor   for S1;
061000160926
061100160926         // Apro il cursore
061200160926         exec sql open C1;
061300160926
061400160926         // Leggo i dati
061500160926         exec sql
061600160926             fetch next from C1 into :wCli_PROC;
061700160926
061800160926         select;
061900160926           // Se errore (gestisco) ed utilizzo mappatura default
062000160926           when sqlCode < 0;
062100160926                wCli_PROC = Cli_PROC;
062200160926
062300160926           // Se non trovato utilizzo mappatura default
062400160926           when sqlCode = 100;
062500160926                wCli_PROC = Cli_PROC;
062600160926         endsl;
062700160926
062800160926       ENDSR;
062900160921
063000160921
063100160921       //--------------------------------------------------------------
063200160921       // Calcolo Location del Evento
063300160921       //--------------------------------------------------------------
063400160921       BEGSR exeEventLocation;
063500160921
063600160921         // Inizializzazioni
063700160921         clear wFiliale;
063800161010         clear TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
063900161010                 TrackingEventDetail(TIS7WP01DO.EventCount).EventLocation;
064000160921
064100160921         // A seconda delle fase operativa considero una specifica location della merce
064200160921         if oVMSB2CFasOp = '*DEST';
064300160921            // Location merce coincide con indirizo destinatario
064400161010            TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
064500161010              TrackingEventDetail(TIS7WP01DO.EventCount).EventLocation =
064600161010                  TIS7WP01DO.TrkInfoDta.PackageDestinationLocation;
064700160921         else;
064800160921
064900160921           select;
065000160921
065100160921             // Location merce in arrivo
065200160921             when oVMSB2CFasOp = '*ANOM'   or
065300160921                  oVMSB2CFasOp = '*ARR'    or
065400160921                  oVMSB2CFasOp = '*GEN'    or
065500160921                  oVMSB2CFasOp = '*GIAC';
065600160921               wFiliale = wLNA;
065700160921
065800160921             // Location merce in partenza o transito (dirottamento ad esempio)
065900160921             when oVMSB2CFasOp = '*PART'   or
066000160921                  oVMSB2CFasOp = '*TRANS';
066100160921               wFiliale = wLNP;
066200160921
066300160921             // Altro lo considero in arrivo
066400160921             other;
066500160921               wFiliale = wLNA;
066600160921
066700160921           endsl;
066800160921
066900160921           // Reperisco quindi i dati di indirizzo
067000160921           chain (wFiliale) azorg01l;
067100160921           if %found(azorg01l);
067200160921
067300161010              TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
067400161010                 TrackingEventDetail(TIS7WP01DO.EventCount).EventLocation.
067500161010                         Street1 = %trim(orgIND);
067600160921
067700161010              TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
067800161010                 TrackingEventDetail(TIS7WP01DO.EventCount).EventLocation.
067900161010                         Street2 = '';
068000160921
068100161010              TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
068200161010                 TrackingEventDetail(TIS7WP01DO.EventCount).EventLocation.City =
068300160923                          %trim(orgLOC);
068400160921
068500161010              TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
068600161010                 TrackingEventDetail(TIS7WP01DO.EventCount).EventLocation.
068700160923                        StateProvince = %trim(orgPRO);
068800160921
068900161010              TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
069000161010                 TrackingEventDetail(TIS7WP01DO.EventCount).EventLocation.
069100161010                         PostalCode = %trim(%editc(orgCPF:'X'));
069200160921
069300161010              TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
069400161010                 TrackingEventDetail(TIS7WP01DO.EventCount).EventLocation.
069500160922                        CountryCode = 'IT';
069600160921
069700160921           endif;
069800160921
069900160921         endif;
070000160921
070100160921       ENDSR;
070200160926
070300160926
070400160926       //--------------------------------------------------------------
070500160926       // Valorizza struttura di errore Amazon
070600160926       //--------------------------------------------------------------
070700160926       BEGSR valErrore;
070800160926
070900161010         TIS7WP01DO.ErrInfoDta.TrackingNumber = TIS7WP01DI.TrkNumber;
071000161010         TIS7WP01DO.ErrInfoDta.TrackingErrorDetail.
071100160926                        ErrorDetailCode = %trim(ErrCode);
071200161010         TIS7WP01DO.ErrInfoDta.TrackingErrorDetail.
071300160926                        ErrorDetailCodeDesc = %trim(ErrCodeDesc);
071400160926
071500160926       ENDSR;
071600160926
071700160919
071800160919
071900160919       //--------------------------------------------------------------
072000160919       // Uscita
072100160919       //--------------------------------------------------------------
072200160919       BEGSR exeUscita;
072300160919
072400160926         // Chiusura cursori
072500160921         exec sql close C0;
072600160926         exec sql close C1;
072700160926
072800160926         // Valorizza struttura di errore Amazon
072900160926         exsr valErrore;
073000160921
073100160919         return;
073200160919
073300160919       ENDSR;
073400091223
073500160802
073600091223      /end-free

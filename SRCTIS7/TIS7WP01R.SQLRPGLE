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
014400170302           when TIS7WP01DI.APIVersion = 4;
014500160929
014600160929              // Operazioni?iniziali
014700170302              exsr sr_RoutInz4;
014800160929
014900160929              // Se OK recupero tutti i dati (Spedizione ed Eventi)
015000170302              exsr RtvDati4;
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
017800170302       BEGSR  sr_RoutInz4;
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
022300170302       BEGSR  RtvDati4;
022400170130
022500170130         // Compongo dinamicamente l'istruzione SQL
022600170130         wSQL =
022700170130            ' WITH ' +
022800170130
022900170130            ' SEL_KUN_KSU as ( ' +
023000170130            ' SELECT ' + %trim(TIS7WP01DI.CliUnif) +
023100170130            ' AS KUNCOF FROM SYSIBM/SYSDUMMY1 ' +
023200170130            ' UNION ALL ' +
023300170130            ' SELECT KUNCOF FROM TIKUN00F WHERE ' +
023400170130            ' KUNVAT='' '' ' +
023500170130            ' and KUNTLE=' + wApi + TIS7WP01DI.TipoLegame + wApi +
023600170130            ' and KUNAZP='' ''' +
023700170130            ' and KUNCOP=' + %trim(TIS7WP01DI.CliUnif) +
023800170130            ' AND (SELECT DC_YYMD_DEC from DATECNV0F ' +
023900170130            ' WHERE DC_DATE = curdate()) ' +
024000170130            ' BETWEEN KUNDDE and KUNDSC), ' +
024100170130
024200170130            ' SEL_STB_RMN as (SELECT ' +
024300170130            ' STBKSU, STBAAS, STBLNP, STBNRS, STBNSP, STBTIS, STBPRS, ' +
024400170130            ' STBCOS, STBFOP, STBSTS, STBDAS, STBORS ' +
024500170130            ' FROM TISTB00F ' +
024600170130            ' WHERE STBRMN= ' + %trim(TIS7WP01DI.TrkNumber) +
024700170130            ' AND STBKSU in (SELECT KUNCOF FROM SEL_KUN_KSU)) ' +
024800170130
024900170130            ' SELECT STBKSU, STBAAS, STBLNP, STBNRS, STBNSP, STBTIS, STBPRS, ' +
025000170130            ' STBCOS, STBFOP, STBSTS, STBDAS, STBORS, ' +
025100170130            ' case when STBDDAD is null then 0 ' +
025200170130            '      else STBDDAD end as STBDDAD, ' +
025300170130            ' case when STBDORD is null then 0 ' +
025400170130            '      else STBDORD end as STBDORD ' +
025500170130            ' FROM SEL_STB_RMN left join TISTBD0F ' +
025600170130            ' on STBKSU=STBDKSU and STBAAS=STBDAAS and ' +
025700170130            ' STBLNP=STBDLNP and  STBNRS=STBDNRS and STBNSP=STBDNSP and ' +
025800170130            ' STBDAS=STBDDAS and  STBORS=STBDORS and STBCOS=STBDCOS ' +
025900170130            ' ORDER BY STBDAS desc, STBORS desc ' +
026000170130            ' for read only';
026100170130
026200160802
026300160531         // Dichiarazione cursore
026400160802         exec sql   prepare S0   from :wSQL;
026500160531         exec sql   declare C0   cursor   for S0;
026600160802
026700160531         // Apro il cursore
026800160531         exec sql open C0;
026900160802
027000160531         // Leggo i dati
027100160531         exec sql
027200160926             fetch next from C0 into :STBKSU, :STBAAS, :STBLNP, :STBNRS,
027300160923                   :STBNSP, :STBTIS, :STBPRS, :STBCOS,  :STBFOP,
027400160924                   :STBSTS, :STBDAS, :STBORS, :STBDDAD, :STBDORD;
027500160923
027600160919         select;
027700160919           // Se errore SQL probabile UTENTE errato
027800160919           when sqlCode < 0;
027900160926              ErrCode     = WS_AMAZON_TRACKING_SERVICE_UNAVAILABLE;
028000160926              ErrCodeDesc = WS_AMAZON_TRACKING_SERVICE_UNAVAILABLE_DESC;
028100161010              TIS7WP01DO.Esito = WSER_ESITO_ERRORE_SQL_REPERIMENTO_DATI;
028200160919              exsr exeUscita;
028300160919
028400160919           // Se Tracking inesistente
028500160919           when sqlCode = 100;
028600160926              ErrCode     = WS_AMAZON_NO_TRACKING_INFO_FOUND;
028700160926              ErrCodeDesc = WS_AMAZON_NO_TRACKING_INFO_FOUND_DESC;
028800161010              TIS7WP01DO.Esito = WSER_ESITO_ERRORE_PARAMETRO_NON_VALIDO;
028900160919              exsr exeUscita;
029000160919
029100160919           // Diversamente Tracking trovato e stati reperiti
029200160919           other;
029300160919              // Se chiave Sped OK procedo
029400160921              if stbAAS <> *zeros and
029500160923                 stbLNP <> *zeros and
029600160921                 stbNSP <> *zeros;
029700160919
029800160919                 // Salvo la chiave spedizione corrente
029900160921                 wSTBAAS = stbAAS;
030000160921                 wSTBLNP = stbLNP;
030100160921                 wSTBNRS = stbNRS;
030200160921                 wSTBNSP = stbNSP;
030300160919
030400160926                 // Reperisco ID mappatura da utilizzare
030500160926                 exsr rtvMapp;
030600160926
030700160919                 // Recupero i dati della spedizione
030800160921                 exsr RtvDatiSped;
030900160921
031000160921                 // A questo punto valorizzo il buffer di output
031100160921                 exsr ValOut;
031200160919
031300160921              else;
031400160926                 ErrCode     = WS_AMAZON_NO_TRACKING_INFO_FOUND;
031500160926                 ErrCodeDesc = WS_AMAZON_NO_TRACKING_INFO_FOUND_DESC;
031600161010                 TIS7WP01DO.Esito = WSER_ESITO_ERRORE_PARAMETRO_NON_VALIDO;
031700160921                 exsr exeUscita;
031800160921              endif;
031900160919
032000160921         endsl;
032100160217
032200160217       ENDSR;
032300160920
032400160920
032500160920       //--------------------------------------------------------------
032600160920       // Reperimento dati spedizione
032700160920       //--------------------------------------------------------------
032800160920       BEGSR RtvDatiSped;
032900160920
033000160920         // Reperisco i dati spedizione (in qualunque fase)
033100160922         UBRTVSPE_S ( 'APS'
033200160922                     : stbAAS
033300160922                     : stbLNP
033400160922                     : stbNRS
033500160922                     : stbNSP
033600160922                     : oRTVSPE_TASDS
033700160922                     : oRTVSPE_ARBDS
033800160922                     : oRTVSPE_BLPDS
033900160922                     : oRTVSPE_Esito
034000160922                    );
034100160920
034200160921         clear wLNP;
034300160921         clear wLNA;
034400160921
034500160920         // A seconda della "sorgente" dati valorizzo
034600160920         select;
034700160920
034800160920           // Partenza
034900160920           when %subst(oRTVSPE_Esito:1:2) = ' P';
035000161010             TIS7WP01DO.TrkInfoDta.TrackingNumber = %trim(TIS7WP01DI.TrkNumber);
035100161010             TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.Street1 =
035200160923                            %trim(blpIND);
035300161010             TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.Street2 =
035400160923                            '';
035500161010             TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.City =
035600160923                            %trim(blpLOD);
035700161010             TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.StateProvince =
035800160923                            %trim(blpPRD);
035900161010             TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.PostalCode =
036000160923                            %trim(blpCAD);
036100160921             if blpNZD = *blanks;
036200161010                TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.
036300161010                                      CountryCode = 'IT';
036400160921             else;
036500161010                TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.
036600161010                                      CountryCode = blpNZD;
036700160921             endif;
036800160922             if blpDCR > *zeros;
036900161010                TIS7WP01DO.TrkInfoDta.PackageDeliveryDate.
037000161010                                      ScheduledDeliveryDate =
037100160921                    UBFMTDATE_Convert( %editc(blpDCR:'X')
037200160921                              : 'YYYYMMDD' : 'YYYY-MM-DD');
037300160922             endif;
037400160922
037500160921             wLNP = blpLNP;
037600160921             wLNA = blpLNA;
037700160920
037800160921
037900160920           // Arrivo
038000160920           when %subst(oRTVSPE_Esito:1:1) = 'A';
038100161010             TIS7WP01DO.TrkInfoDta.TrackingNumber = %trim(TIS7WP01DI.TrkNumber);
038200161010             TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.Street1 =
038300160923                            %trim(arbIND);
038400161010             TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.Street2 =
038500160923                            '';
038600161010             TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.City =
038700160923                            %trim(arbLOD);
038800161010             TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.StateProvince =
038900160923                            %trim(arbPRD);
039000161010             TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.PostalCode =
039100160923                            %trim(arbCAD);
039200160921             if arbNZD = *blanks;
039300161010                TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.
039400161010                                      CountryCode = 'IT';
039500160921             else;
039600161010                TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.
039700161010                                      CountryCode = arbNZD;
039800160921             endif;
039900160922             if arbDCR > *zeros;
040000161010                TIS7WP01DO.TrkInfoDta.PackageDeliveryDate.
040100161010                                      ScheduledDeliveryDate =
040200160922                    UBFMTDATE_Convert( %editc(arbDCR:'X')
040300160922                              : 'YYYYMMDD' : 'YYYY-MM-DD');
040400160922             endif;
040500160922
040600160921             wLNP = arbLNP;
040700160921             wLNA = arbLNA;
040800160921
040900160920
041000160920           // Sede
041100160920           when %subst(oRTVSPE_Esito:3:1) = 'S';
041200161010             TIS7WP01DO.TrkInfoDta.TrackingNumber = %trim(TIS7WP01DI.TrkNumber);
041300161010             TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.Street1 =
041400160923                            %trim(tasIND);
041500161010             TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.Street2 =
041600160923                            '';
041700161010             TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.City =
041800160923                            %trim(tasLOD);
041900161010             TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.StateProvince =
042000160923                            %trim(tasPRD);
042100161010             TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.PostalCode =
042200160923                            %trim(tasCAD);
042300160921             if tasNZD = *blanks;
042400161010                TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.
042500161010                                      CountryCode = 'IT';
042600160921             else;
042700161010                TIS7WP01DO.TrkInfoDta.PackageDestinationLocation.
042800161010                                      CountryCode = tasNZD;
042900160921             endif;
043000160922             if tasDCR > *zeros;
043100161010                TIS7WP01DO.TrkInfoDta.PackageDeliveryDate.
043200161010                                      ScheduledDeliveryDate =
043300160922                    UBFMTDATE_Convert( %editc(tasDCR:'X')
043400160922                              : 'YYYYMMDD' : 'YYYY-MM-DD');
043500160922             endif;
043600160922
043700160921             wLNP = tasLNP;
043800160921             wLNA = tasLNA;
043900160920
044000160920         endsl;
044100160920
044200160920       ENDSR;
044300160921
044400160921
044500160921       //--------------------------------------------------------------
044600160921       // Scorro gli stati e valorizzo il buffer di output
044700160921       //--------------------------------------------------------------
044800160921       BEGSR  ValOut;
044900160921
045000160921         // Scorro tutto il recordset reperito
045100160921         dow sqlCode >= *zeros AND sqlCode <> 100 AND
045200160921             wSTBAAS  = stbAAS AND
045300160921             wSTBLNP  = stbLNP AND
045400160921             wSTBNRS  = stbNRS AND
045500160921             wSTBNSP  = stbNSP;
045600160921
045700160921             // Considero solamente i record con le caratteristiche necessarie
045800160921             if (
045900160921                 (stbTIS='1' AND stbPRS<>'7') OR
046000160921                 (stbTIS='1' AND stbPRS ='7'
046100160921                             AND stbCOS ='CONSOK') OR
046200160921                 (stbTIS='1' AND stbPRS ='8'
046300160921                             AND stbCOS ='SPUNTNPG5') OR
046400160921                 (stbTIS='9' AND stbPRS ='1')
046500160921                 ) AND
046600160921                 (%subst(stbFOP:1:1)= 'O' OR
046700160921                 (%subst(stbFOP:1:1)= 'F' AND stbSTS= '9')
046800160921                );
046900160923
047000160923
047100160923              // Considero solamente gli eventi con data e ora valorizzati
047200160924              if (stbDAS  > *zeros AND stbORS  > *zeros) OR
047300160924                 (stbDDAD > *zeros AND stbDORD > *zeros);
047400160930
047500160930
047600160930              // Considero solamente gli eventi solamente 1 volta
047700160930              if  %lookup(stbCOS:skCOS) = 0 OR ixCOS >= %elem(skCOS);
047800160930
047900160930                 // Salvo l'evento corrente nella lista degli eventi già ritornati
048000160930                 ixCOS = ixCOS + 1;
048100160930                 skCOS(ixCOS) = stbCOS;
048200160926
048300160926                 // Effettuo mappatura tra causale BRT e stato CLIENTE
048400160923                 UBVMSB2CR_S (  *blanks
048500160923                              : *zeros
048600160923                              : *blanks
048700160926                              : wCli_PROC
048800160926                              : *blanks
048900160923                              : stbCOS
049000160923                              : oVMSB2CEsito
049100160923                              : oVMSB2CCauTip
049200160923                              : oVMSB2CCauTT
049300160923                              : oVMSB2CStsCli
049400160923                              : oVMSB2CStsOpz
049500160923                              : oVMSB2CFasOp
049600160923                              : oVMSB2CDDE
049700160923                              : oVMSB2CDesc
049800160923                             );
049900160921
050000160921               // Se mappatura riuscita e stato CLIENTE da ritornare
050100160921               if oVMSB2CEsito   = '1' AND
050200160923                  oVMSB2CStsCli <> 'NONINVIARE' AND
050300160923                  oVMSB2CStsCli <> *blanks;
050400160923
050500160923
050600160923                  // Se evento che prevede "dettagli" considero data/ora del dettaglio
050700160923                  if oVMSB2CStsOpz = '*DETT' AND
050800160924                           STBDDAD > *zeros  AND
050900160924                           STBDORD > *zeros;
051000160923                     wSTBDDAD = STBDDAD;
051100160923                     wSTBDORD = STBDORD;
051200160923                  else;
051300160923                     wSTBDDAD = STBDAS;
051400160923                     wSTBDORD = STBORS*100;
051500160923                  endif;
051600160923
051700160922
051800160922                  // Incremento il contatore di eventi per Tracking corrente
051900161010                  TIS7WP01DO.EventCount = TIS7WP01DO.EventCount + 1;
052000160922
052100160922                  // Inizializzo la schiersa degli Eventi del Tracking corrente
052200161010                  clear TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
052300161010                              TrackingEventDetail(TIS7WP01DO.EventCount);
052400160922
052500160922                  // Valorizzo i dati di ritorno in output
052600161010                  TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
052700161010                     TrackingEventDetail(TIS7WP01DO.EventCount).EventStatus =
052800160921                              %trim(%subst(oVMSB2CStsCli:1:10));
052900160921
053000161010                  TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
053100161010                     TrackingEventDetail(TIS7WP01DO.EventCount).EventReason =
053200160921                              %trim(%subst(oVMSB2CStsCli:11:5));
053300160921
053400161010                  TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
053500161010                     TrackingEventDetail(TIS7WP01DO.EventCount).EventDateTime =
053600160921                         UBFMTDATE_Convert(
053700160923                            %trim(%char(wSTBDDAD*1000000+wSTBDORD)) :
053800160921                           'YYYYMMDDHHMISS' : 'YYYY-MM-DDTHH:MI:SS UTC');
053900160921
054000160921                  // Calcolo la location del evento
054100160921                  exsr exeEventLocation;
054200160921
054300161010 xxx            // TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
054400161010                //     TrackingEventDetail(TIS7WP01DO.EventCount).
054500161010                //     EstimatedDeliveryDate ;
054600160921
054700160921
054800161010                  TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
054900161010                      TrackingEventDetail(TIS7WP01DO.EventCount).
055000160923                         AdditionalLocationInfo = '';
055100160923
055200160923                  // Se evento di consegna reperisco la FIRMA del consegnatario merce
055300160923                  if  %subst(stbCOS:1:6) = 'CONSOK';
055400180202                      chain (stbAAS:stbLNP:stbNRS:stbNSP:'1') tita430c;
055500160923                      if  %found(tita430c);
055600161010                          TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
055700161010                              TrackingEventDetail(TIS7WP01DO.EventCount).
055800160923                                 SignedForByName = %trim(ta4NOT);
055900160923                      endif;
056000160923                  endif;
056100160921
056200161010                  TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
056300161010                      TrackingEventDetail(TIS7WP01DO.EventCount).
056400160923                         DeliveryAppointmentWindow.Day = '';
056500160921
056600161010                  TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
056700161010                      TrackingEventDetail(TIS7WP01DO.EventCount).
056800160923                         DeliveryAppointmentWindow.StartTime = '';
056900160921
057000161010                  TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
057100161010                     TrackingEventDetail(TIS7WP01DO.EventCount).
057200160923                         DeliveryAppointmentWindow.EndTime = '';
057300160921
057400161010                  TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
057500161010                     TrackingEventDetail(TIS7WP01DO.EventCount).
057600160923                        DeliveryAppointmentWindow.TimeZone = '';
057700160921
057800160930               endif;
057900160923               endif;
058000160923
058100160923              endif;
058200160921
058300160921             endif;
058400160921
058500160921             // Leggo il record successivo
058600160921             exec sql
058700160926                  fetch next from C0 into :STBKSU, :STBAAS, :STBLNP, :STBNRS,
058800160923                        :STBNSP, :STBTIS, :STBPRS, :STBCOS,  :STBFOP,
058900160924                        :STBSTS, :STBDAS, :STBORS, :STBDDAD, :STBDORD;
059000160921
059100160921         enddo;
059200170308
059300170308         // Valorizzo il numero di eventi per il tracking corrrente
059400170308         TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
059500170308                    TrackingEventDetail_LENGTH = TIS7WP01DO.EventCount;
059600160924
059700160921         // Valorizzo esito a OK
059800161010         TIS7WP01DO.Esito = WSER_ESITO_OK;
059900160921
060000160921       ENDSR;
060100160926
060200160926
060300160926       //--------------------------------------------------------------
060400160926       // Reperisce ID mappatura da utilizzare
060500160926       //--------------------------------------------------------------
060600160926       BEGSR  rtvMapp;
060700160926
060800160926         // Innanzitutto imposto il default
060900160926         wCli_PROC = Cli_PROC;
061000160926
061100160926         wSQL = ' SELECT VIRPGI FROM tivir00f' +
061200160926                ' WHERE VIRKSC = ' + wApi + '0' + %editc(stbKSU:'X') + wApi +
061300160926                ' and VIRTIP=''VC'' and VIRPGT=''TITV67R'' and VIRPGI<>'' ''' +
061400160926                ' and ' +
061500160926                ' (select DC_YYMD_DEC from DATECNV0F' +
061600160926                '  where DC_DATE = curdate()) between VIRDTI and VIRDTF ' +
061700160926                ' for read only';
061800160926
061900160926         // Dichiarazione cursore
062000160926         exec sql   prepare S1   from :wSQL;
062100160926         exec sql   declare C1   cursor   for S1;
062200160926
062300160926         // Apro il cursore
062400160926         exec sql open C1;
062500160926
062600160926         // Leggo i dati
062700160926         exec sql
062800160926             fetch next from C1 into :wCli_PROC;
062900160926
063000160926         select;
063100160926           // Se errore (gestisco) ed utilizzo mappatura default
063200160926           when sqlCode < 0;
063300160926                wCli_PROC = Cli_PROC;
063400160926
063500160926           // Se non trovato utilizzo mappatura default
063600160926           when sqlCode = 100;
063700160926                wCli_PROC = Cli_PROC;
063800160926         endsl;
063900160926
064000160926       ENDSR;
064100160921
064200160921
064300160921       //--------------------------------------------------------------
064400160921       // Calcolo Location del Evento
064500160921       //--------------------------------------------------------------
064600160921       BEGSR exeEventLocation;
064700160921
064800160921         // Inizializzazioni
064900160921         clear wFiliale;
065000161010         clear TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
065100161010                 TrackingEventDetail(TIS7WP01DO.EventCount).EventLocation;
065200160921
065300160921         // A seconda delle fase operativa considero una specifica location della merce
065400160921         if oVMSB2CFasOp = '*DEST';
065500160921            // Location merce coincide con indirizo destinatario
065600161010            TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
065700161010              TrackingEventDetail(TIS7WP01DO.EventCount).EventLocation =
065800161010                  TIS7WP01DO.TrkInfoDta.PackageDestinationLocation;
065900160921         else;
066000160921
066100160921           select;
066200160921
066300160921             // Location merce in arrivo
066400160921             when oVMSB2CFasOp = '*ANOM'   or
066500160921                  oVMSB2CFasOp = '*ARR'    or
066600160921                  oVMSB2CFasOp = '*GEN'    or
066700160921                  oVMSB2CFasOp = '*GIAC';
066800160921               wFiliale = wLNA;
066900160921
067000160921             // Location merce in partenza o transito (dirottamento ad esempio)
067100160921             when oVMSB2CFasOp = '*PART'   or
067200160921                  oVMSB2CFasOp = '*TRANS';
067300160921               wFiliale = wLNP;
067400160921
067500160921             // Altro lo considero in arrivo
067600160921             other;
067700160921               wFiliale = wLNA;
067800160921
067900160921           endsl;
068000160921
068100160921           // Reperisco quindi i dati di indirizzo
068200160921           chain (wFiliale) azorg01l;
068300160921           if %found(azorg01l);
068400160921
068500161010              TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
068600161010                 TrackingEventDetail(TIS7WP01DO.EventCount).EventLocation.
068700161010                         Street1 = %trim(orgIND);
068800160921
068900161010              TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
069000161010                 TrackingEventDetail(TIS7WP01DO.EventCount).EventLocation.
069100161010                         Street2 = '';
069200160921
069300161010              TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
069400161010                 TrackingEventDetail(TIS7WP01DO.EventCount).EventLocation.City =
069500160923                          %trim(orgLOC);
069600160921
069700161010              TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
069800161010                 TrackingEventDetail(TIS7WP01DO.EventCount).EventLocation.
069900160923                        StateProvince = %trim(orgPRO);
070000160921
070100161010              TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
070200161010                 TrackingEventDetail(TIS7WP01DO.EventCount).EventLocation.
070300161010                         PostalCode = %trim(%editc(orgCPF:'X'));
070400160921
070500161010              TIS7WP01DO.TrkInfoDta.TrackingEventHistory.
070600161010                 TrackingEventDetail(TIS7WP01DO.EventCount).EventLocation.
070700160922                        CountryCode = 'IT';
070800160921
070900160921           endif;
071000160921
071100160921         endif;
071200160921
071300160921       ENDSR;
071400160926
071500160926
071600160926       //--------------------------------------------------------------
071700160926       // Valorizza struttura di errore Amazon
071800160926       //--------------------------------------------------------------
071900160926       BEGSR valErrore;
072000160926
072100161010         TIS7WP01DO.ErrInfoDta.TrackingNumber = TIS7WP01DI.TrkNumber;
072200161010         TIS7WP01DO.ErrInfoDta.TrackingErrorDetail.
072300160926                        ErrorDetailCode = %trim(ErrCode);
072400161010         TIS7WP01DO.ErrInfoDta.TrackingErrorDetail.
072500160926                        ErrorDetailCodeDesc = %trim(ErrCodeDesc);
072600160926
072700160926       ENDSR;
072800160926
072900160919
073000160919
073100160919       //--------------------------------------------------------------
073200160919       // Uscita
073300160919       //--------------------------------------------------------------
073400160919       BEGSR exeUscita;
073500160919
073600160926         // Chiusura cursori
073700160921         exec sql close C0;
073800160926         exec sql close C1;
073900160926
074000160926         // Valorizza struttura di errore Amazon
074100160926         exsr valErrore;
074200170313
074300170313         // Ritorno struttura dati di output alla Response di chiamata
074400170313         Response = TIS7WP01DO;
074500160921
074600160919         return;
074700160919
074800160919       ENDSR;
074900091223
075000160802
075100091223      /end-free

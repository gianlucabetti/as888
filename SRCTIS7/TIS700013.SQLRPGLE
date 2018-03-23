000100150601      //************************************************************************
000101150601      //
000102150601      // Questo programma esegue il calcolo dei tempi di consegna per il
000103150601      // servizio web in modalità 2.0 (progetto 845).
000104150601      //
000105150601      //************************************************************************
000106150601
000107150601     H DFTACTGRP(*NO)
000108150601
000109150601      //************************************************************************
000110150601      //
000111150601      // Costanti.
000112150601      //
000113150601      //************************************************************************
000114150601
000115150601      /COPY GAITRASRC/SRCCONST,TISI795R
000116150629     D HTML_LINE_BREAK...
000117150629     D                 C                   '<br>'
000118150601     D RQSOPC_GETTEMPI2...
000119150601     D                 C                   'GETTEMPI2'
000120150603     D RPYOPC_DONE...
000121150603     D                 C                   'DONE'
000122150629     D RPYOPC_ERROR...
000123150629     D                 C                   'ERROR'
000124150601     D RPYOPC_OTHERERROR...
000125150601     D                 C                   'OTHERERROR'
000126150603
000127150603      //************************************************************************
000128150603      //
000129150603      // Strutture.
000130150603      //
000131150603      //************************************************************************
000183150601
000184150603     D this            DS                  INZ QUALIFIED
000185150603     D  esito                        10I 0
000186150603
000187150601      //************************************************************************
000188150601      //
000189150601      // Campi.
000190150601      //
000191150601      //************************************************************************
000192150601
000193150603     D rqsOpc          S             10                                         * REQUEST OPCODE
000194150601     D rqsDta          S           9999                                         * REQUEST DATA
000195150601     D rqsLen          S              4P 0                                      * REQUEST DATA LENGT
000196150601     D rqsCty          S             10                                         * REQUEST CLIENT TYP
000197150601     D rqsCid          S             10                                         * REQUEST CLIENT
000198150601     D rpyOpc          S             10                                         * REPLY OPCODE
000199150601     D rpyDta          S           9999                                         * REPLY DATA
000200150601     D rpyLen          S              4P 0                                      * REPLY DATA LENGTH
000207150601
000208150601      //************************************************************************
000209150601      //
000210150601      // Prototipi.
000211150601      //
000212150601      //************************************************************************
000213150601
000214150601      /COPY GAITRASRC/SRCPROTOPR,TISI795R
000215150601
000216150601      //************************************************************************
000217150601      //
000218150601      // Parametri.
000219150601      //
000220150601      //************************************************************************
000221150601
000222150601     C     *ENTRY        PLIST
000223150601     C                   PARM                    rqsOpc
000224150601     C                   PARM                    rqsDta
000225150601     C                   PARM                    rqsLen
000226150601     C                   PARM                    rqsCty
000227150601     C                   PARM                    rqsCid
000228150601     C                   PARM                    rpyOpc
000229150601     C                   PARM                    rpyDta
000230150601     C                   PARM                    rpyLen
000231150601
000232150601      //************************************************************************
000233150601      //
000234150601      // Main.
000235150601      //
000236150601      //************************************************************************
000237150601
000238150601      /FREE
000239150601
000240150601       CLEAR rpyOpc;
000241150601       CLEAR rpyDta;
000242150601       CLEAR rpyLen;
000243150603       RESET this;
000244150601
000245150601       SELECT;
000246150601         WHEN rqsOpc = RQSOPC_GETTEMPI2;
000247150603           this.esito = GetTempi2();
000252150601         OTHER;
000253150601           rpyOpc = RPYOPC_OTHERERROR;
000254150601           rpyDta = 'TIS700013 ha ricevuto un codice operativo sconosciuto ('
000255150601                  + rqsOpc + ').';
000256150601           rpyLen = %LEN(%TRIMR(rpyDta));
000257150601           DUMP(A);
000258150601           RETURN;
000259150601       ENDSL;
000263150603
000264150601       RETURN;
000265150601
000266150601      /END-FREE
000267150601
000268150601     P*--------------------------------------------------
000269150601     P* Procedure name: GetTempi2
000270150601     P* Purpose:        Calcola i tempi di consegna in modalità 2.0
000271150601     P* Returns:        Esito.
000272150601     P*--------------------------------------------------
000273150601     P GetTempi2       B
000274150601     D GetTempi2       PI            10I 0
000275150601
000276150601     D retField        S             10I 0 STATIC
000277150601     D getTempi2i...
000278150601     D                 DS                  QUALIFIED INZ STATIC
000279150601     D  formato...
000280150601     D                               10A
000281150601     D  versione...
000282150601     D                                1A
000283150601     D  lingua...
000284150601     D                                2A
000285150601     D  ksu...
000286150601     D                                8A
000287150601     D  sun...
000288150601     D                                9S 0
000289150601     D  clientIpAddress...
000290150601     D                               15A
000291150601     D  siglaProvinciaPartenza...
000292150601     D                                2A
000293150601     D  localitaPartenza...
000294150601     D                               35A
000295150601     D  capPartenza...
000296150601     D                                9A
000297150601     D  siglaProvinciaArrivo...
000298150601     D                                2A
000299150601     D  localitaArrivo...
000300150601     D                               35A
000301150601     D  capArrivo...
000302150601     D                                9A
000303150601     D  dataPronta...
000304150601     D                               10A
000305150601     D  contattoTelefonico...
000306150601     D                                1A
000307150601     D  peso...
000308150601     D                                7S 1
000309150603     D  volume1...
000310150603     D                                2A
000311150603     D  volume2...
000312150603     D                                3A
000313150601     D getTempi2o...
000314150601     D                 DS                  QUALIFIED INZ STATIC
000315150601     D  formato...
000316150601     D                               10A   INZ('GETTEMPI2O')
000317150601     D  versione...
000318150601     D                                1A   INZ('A')
000319150603     D  filialePartenzaId...
000320150603     D                                3S 0
000321150603     D  filialePartenzaDescrizione...
000322150603     D                               20A
000323150603     D  filialePartenzaIndirizzo...
000324150603     D                               20A
000325150603     D  filialePartenzaCap...
000326150603     D                                5A
000327150603     D  filialePartenzaLocalita...
000328150603     D                               20A
000329150603     D  filialePartenzaProvincia...
000330150603     D                                2A
000331150603     D  filialePartenzaTelefono...
000332150603     D                               13A
000333150603     D  filialePartenzaTelefax...
000334150603     D                               13A
000335150603     D  filialePartenzaEmail...
000336150603     D                              255A
000337150603     D  filialeArrivoId...
000338150603     D                                3S 0
000339150603     D  filialeArrivoDescrizione...
000340150603     D                               20A
000341150603     D  filialeArrivoIndirizzo...
000342150603     D                               20A
000343150603     D  filialeArrivoCap...
000344150603     D                                5A
000345150603     D  filialeArrivoLocalita...
000346150603     D                               20A
000347150603     D  filialeArrivoProvincia...
000348150603     D                                2A
000349150603     D  filialeArrivoTelefono...
000350150603     D                               13A
000351150603     D  filialeArrivoTelefax...
000352150603     D                               13A
000353150603     D  filialeArrivoEmail...
000354150603     D                              255A
000355150603     D  servizioCount...
000356150603     D                                2S 0
000357150603     D  servizioDisponibile...
000358150603     D                               18A
000359150603     D  servizioId...
000360150603     D                               18A
000361150603     D  servizioDescrizione...
000362150603     D                              450A
000363150603     D  servizioTempi...
000364150603     D                             1800A
000365150603     D  ritiroNote1...
000366150603     D                              255A
000367150603     D  ritiroNote2...
000368150603     D                              255A
000369150603     D  consegnaNote1...
000370150603     D                              255A
000371150603     D  consegnaNote2...
000372150603     D                              255A
000373150603     D  giornoRitiro...
000374150603     D                               20A
000375150629     D  filialePartenzaUrl...
000376150629     D                              256A
000377150629     D  filialeArrivoUrl...
000378150629     D                              256A
000379150629     D getTempi2m...
000380150629     D                 DS                  QUALIFIED INZ STATIC
000381150629     D  formato...
000382150629     D                               10A   INZ('GETTEMPI2M')
000383150629     D  versione...
000384150629     D                                1A   INZ('A')
000385150629     D  erroriPartenza...
000386150629     D                             3000A
000387150629     D  erroriArrivo...
000388150629     D                             3000A
000389150629     D  erroriCalcolo...
000390150629     D                             3000A
000391150601     D tisi795f0     E DS                  QUALIFIED TEMPLATE
000392150601     D tisi795i0     E DS                  INZ(*EXTDFT) QUALIFIED STATIC
000393150601     D tisi795m0     E DS                  INZ(*EXTDFT) QUALIFIED STATIC
000394150601     D tisi795o0     E DS                  INZ(*EXTDFT) QUALIFIED STATIC
000395150603     D local           DS                  QUALIFIED STATIC INZ
000396150629     D  m                            10I 0
000397150603     D  rpyOpCode                    10I 0
000398150603     D  rpyIdMsg                     10I 0
000399150629     D  idMsg                         7A
000400150629     D  idCampo                      10A
000401150629     D  errWarn                       1A
000402150629     D  textMsg                     255A   VARYING
000403150629     D  erroriPartenza...
000404150629     D                             3000A   VARYING
000405150629     D  erroriArrivo               3000A   VARYING
000406150629     D  erroriCalcolo              3000A   VARYING
000407150601
000408150601      /FREE
000409150601
000410150601       CLEAR retField;
000411150603       RESET local;
000412150603       RESET getTempi2o;
000413150629       RESET getTempi2m;
000414150601       //RESET tisi795f0;
000415150601       RESET tisi795i0;
000416150601       RESET tisi795m0;
000417150601       RESET tisi795o0;
000418150601
000419150603       getTempi2i = %SUBST(rqsDta : 1 : rqsLen);
000420150601       tisi795i0.lingua = getTempi2i.lingua;
000421150601       tisi795i0.ksu = getTempi2i.ksu;
000422150603       tisi795i0.sun = %EDITC(getTempi2i.sun : 'X');
000423150603       tisi795i0.cltIpAddr = getTempi2i.clientIpAddress;
000424150601       tisi795i0.prv_p = getTempi2i.siglaProvinciaPartenza;
000425150601       tisi795i0.loc_p = getTempi2i.localitaPartenza;
000426150601       tisi795i0.cap_p = getTempi2i.capPartenza;
000427150601       tisi795i0.prv_a = getTempi2i.siglaProvinciaArrivo;
000428150601       tisi795i0.loc_a = getTempi2i.localitaArrivo;
000429150601       tisi795i0.cap_a = getTempi2i.capArrivo;
000430150601       tisi795i0.orm_comm = getTempi2i.contattoTelefonico;
000431150601       tisi795i0.pesoKg = getTempi2i.peso;
000432150604
000433150603       getTempi2i.volume2 = %XLATE(' ' : '0' : getTempi2i.volume2);
000434150604       MONITOR;
000435150604         tisi795i0.vol_mc = %DEC(%TRIM(getTempi2i.volume1) + ','
000436150604                          + %TRIM(getTempi2i.volume2) : 5 : 3);
000437150604       ON-ERROR;
000438150604       ENDMON;
000439150604
000440150604       MONITOR;
000441150604         tisi795i0.data_rit = %CHAR(%DATE(getTempi2i.dataPronta : *EUR) : *EUR);
000442150604       ON-ERROR;
000443150604         tisi795i0.data_rit = %CHAR(%DATE() : *EUR);
000444150604       ENDMON;
000445150601
000446150603       MONITOR;
000447150603         Tisi795_GetTempi2( TISI795_RQSOPCODE_GET_TEMPI2
000448150603                          : local.rpyOpCode
000449150603                          : local.rpyIdMsg
000450150603                          : tisi795i0.formato
000451150603                          : tisi795i0
000452150603                          : %SIZE(tisi795i0)
000453150603                          : tisi795o0.formato
000454150603                          : tisi795o0
000455150603                          : %SIZE(tisi795o0)
000456150603                          : tisi795m0.formato
000457150603                          : tisi795m0
000458150603                          : %SIZE(tisi795m0)
000459150603                          );
000460150603       ON-ERROR;
000461150603         retField = %STATUS() * -1;
000462150603         DUMP(A);
000463150603         RETURN retField;
000464150603       ENDMON;
000465150601
000466150603       IF local.rpyOpCode < *ZERO;
000467150629         FOR local.m = 1 TO tisi795m0.nrMsg;
000471150629           local.idMsg = %SUBST(tisi795m0.idMsg : (local.m * 7) - 6 : 7);
000472150629           local.idCampo = %SUBST(tisi795m0.idCampo : (local.m * 10) - 9 : 10);
000473150629           local.errWarn = %SUBST(tisi795m0.errWarn : local.m : 1);
000474150629           local.textMsg = %TRIMR(%SUBST( tisi795m0.textMsg
000475150629                                        : (local.m * 255) - 254 : 255));
000476150629           SELECT;
000477150629             WHEN local.idCampo = 'PRV_P'
000478150629               OR local.idCampo = 'LOC_P'
000481150629               OR local.idCampo = 'CAP_P';
000482150629               IF %LEN(local.erroriPartenza) > *ZERO;
000483150629                 local.erroriPartenza += HTML_LINE_BREAK;
000484150629               ENDIF;
000485150629               local.erroriPartenza += local.textMsg;
000486150629             WHEN local.idCampo = 'PRV_A'
000487150629               OR local.idCampo = 'LOC_A'
000488150629               OR local.idCampo = 'CAP_A';
000489150629               IF %LEN(local.erroriArrivo) > *ZERO;
000490150629                 local.erroriArrivo += HTML_LINE_BREAK;
000491150629               ENDIF;
000492150629               local.erroriArrivo += local.textMsg;
000493150629             OTHER;
000494150629               IF %LEN(local.erroriCalcolo) > *ZERO;
000495150629                 local.erroriCalcolo += HTML_LINE_BREAK;
000496150629               ENDIF;
000497150629               local.erroriCalcolo += local.textMsg;
000498150629           ENDSL;
000499150629         ENDFOR;
000500150629         getTempi2m.erroriPartenza = local.erroriPartenza;
000501150629         getTempi2m.erroriArrivo = local.erroriArrivo;
000502150629         getTempi2m.erroriCalcolo = local.erroriCalcolo;
000503150629         rpyDta = getTempi2m;
000504150629         rpyLen = %SIZE(getTempi2m);
000505150629         rpyOpc = RPYOPC_ERROR;
000506150603         retField = local.rpyOpCode;
000507150603         RETURN retField;
000508150603       ENDIF;
000509150601
000510150603       getTempi2o.filialePartenzaId = tisi795o0.fil_fp;
000511150603       getTempi2o.filialePartenzaDescrizione = tisi795o0.des_fp;
000512150603       getTempi2o.filialePartenzaIndirizzo = tisi795o0.ind_fp;
000513150603       getTempi2o.filialePartenzaCap = tisi795o0.cap_fp;
000514150603       getTempi2o.filialePartenzaLocalita = tisi795o0.loc_fp;
000515150603       getTempi2o.filialePartenzaProvincia = tisi795o0.prv_fp;
000516150603       getTempi2o.filialePartenzaTelefono = tisi795o0.tel_fp;
000517150603       getTempi2o.filialePartenzaTelefax = tisi795o0.fax_fp;
000518150603       getTempi2o.filialePartenzaEmail = tisi795o0.mail_fp;
000519150603       getTempi2o.filialeArrivoId = tisi795o0.fil_fa;
000520150603       getTempi2o.filialeArrivoDescrizione = tisi795o0.des_fa;
000521150603       getTempi2o.filialeArrivoIndirizzo = tisi795o0.ind_fa;
000522150603       getTempi2o.filialeArrivoCap = tisi795o0.cap_fa;
000523150603       getTempi2o.filialeArrivoLocalita = tisi795o0.loc_fa;
000524150603       getTempi2o.filialeArrivoProvincia = tisi795o0.prv_fa;
000525150603       getTempi2o.filialeArrivoTelefono = tisi795o0.tel_fa;
000526150603       getTempi2o.filialeArrivoTelefax = tisi795o0.fax_fa;
000527150603       getTempi2o.filialeArrivoEmail = tisi795o0.mail_fa;
000528150603       getTempi2o.ritiroNote1 = tisi795o0.not1_rit;
000529150603       getTempi2o.ritiroNote2 = tisi795o0.not2_rit;
000530150603       getTempi2o.consegnaNote1 = tisi795o0.not1_con;
000531150603       getTempi2o.consegnaNote2 = tisi795o0.not2_con;
000532150603       getTempi2o.servizioCount = tisi795o0.tsp_tot;
000533150603       getTempi2o.servizioDisponibile = tisi795o0.tsp_dispo;
000534150603       getTempi2o.servizioId = tisi795o0.tsp_cod;
000535150603       getTempi2o.servizioDescrizione = tisi795o0.tsp_descr;
000536150603       getTempi2o.servizioTempi = tisi795o0.tsp_not;
000537150603       getTempi2o.giornoRitiro = tisi795o0.day_rit;
000538150629       getTempi2o.filialePartenzaUrl = tisi795o0.url_fp;
000539150629       getTempi2o.filialeArrivoUrl = tisi795o0.url_fa;
000540150603
000541150603       rpyDta = getTempi2o;
000542150603       rpyLen = %SIZE(getTempi2o);
000543150603       rpyOpc = RPYOPC_DONE;
000544150603
000545150601       RETURN retField;
000546150601
000547150601      /END-FREE
000548150601     P GetTempi2       E
000549150601

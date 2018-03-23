000100091126     H DFTACTGRP(*NO) BNDDIR('TIBS') ACTGRP(*CALLER)
000200091027
000300091027     ***************************************************************************
000400091027     **
000500091027     ** Procedure.
000600091027     **
000700091027     ***************************************************************************
000800091027     D/COPY GAITRASRC/SRCPROTOPR,TIBSORG00
000900091030     D/COPY GAITRASRC/SRCPROTOPR,TIBS0800R
001000091027     D/COPY GAITRASRC/SRCPROTOPR,TISI95R1
001100091027     D/COPY GAITRASRC/SRCPROTOPI,TISI95R1
001200091102     D/COPY GAITRASRC/SRCPROTOPR,FNLV55R
001300091026
001400091026     ***************************************************************************
001500091026     **
001600091026     ** Strutture dati.
001700091026     **
001800091026     ***************************************************************************
001900091026     D getTempiI...
002000091026     D                 DS                  QUALIFIED
002100091027     D  lingua...
002200091027     D                                2A
002300091027     D  ksu...
002400091027     D                                8A
002500091027     D  sun...
002600091027     D                                9A
002700091027     D  clientIpAddress...
002800091027     D                               15A
002900091026     D  siglaProvinciaPartenza...
003000091026     D                                2A
003100091026     D  localitaPartenza...
003200091026     D                               35A
003300091026     D  capPartenza...
003400091026     D                                9A
003500091026     D  siglaProvinciaArrivo...
003600091026     D                                2A
003700091026     D  localitaArrivo...
003800091026     D                               35A
003900091026     D  capArrivo...
004000091026     D                                9A
004100091026     D getTempiO...
004200091026     D                 DS                  QUALIFIED
004300091026     D                                     INZ
004400091026     D  filialePartenzaId...
004500091026     D                                3S 0
004600091026     D  filialePartenzaDescrizione...
004700091026     D                               20A
004800091029     D  filialePartenzaIndirizzo...
004900091029     D                               20A
005000091029     D  filialePartenzaCap...
005100091029     D                                5S 0
005200091029     D  filialePartenzaLocalita...
005300091029     D                               20A
005400091029     D  filialePartenzaProvincia...
005500091029     D                                2A
005600091029     D  filialePartenzaTelefono...
005700091029     D                               13A
005800091029     D  filialePartenzaFAX...
005900091029     D                               13A
006000091029     D  filialePartenzaEmail...
006100091029     D                              255A
006200091026     D  filialeArrivoId...
006300091026     D                                3S 0
006400091026     D  filialeArrivoDescrizione...
006500091026     D                               20A
006600091029     D  filialeArrivoIndirizzo...
006700091029     D                               20A
006800091029     D  filialeArrivoCap...
006900091029     D                                5S 0
007000091029     D  filialeArrivoLocalita...
007100091029     D                               20A
007200091029     D  filialeArrivoProvincia...
007300091029     D                                2A
007400091029     D  filialeArrivoTelefono...
007500091029     D                               13A
007600091029     D  filialeArrivoFAX...
007700091029     D                               13A
007800091029     D  filialeArrivoEmail...
007900091029     D                              255A
008000091030     D  numeroTipiServizio...
008100091030     D                                2S 0
008200091030     D  listaTipiServizioDisponibili...
008300091030     D                                1N   DIM(18)
008400091030     D  listaTipiServizioCodice...
008500091030     D                                1A   DIM(18)
008600091030     D  listaTipiServizioDescrizione...
008700091030     D                               25A   DIM(18)
008800091030     D  listaTipiServizioTempi...
008900091030     D                              100A   DIM(18)
009000091026     D tisi95Ds_partenza...
009100091026     D                 DS                  LIKEDS(tisi95Ds)
009200091026     D                                     INZ
009300091026     D tisi95Ds_arrivo...
009400091026     D                 DS                  LIKEDS(tisi95Ds)
009500091026     D                                     INZ
009600091030     D*
009700091026
009800091026     ***************************************************************************
009900091026     **
010000091026     ** Campi.
010100091026     **
010200091026     ***************************************************************************
010300091126     D errori          DS                  QUALIFIED INZ
010400091126     D  err_partenza               3333A
010500091126     D  err_arrivo                 3333A
010600091126     D  err_calcolo                3333A
010700091026     D RQSOPC          S             10A                                        * REQUEST OPCODE
010800091026     D RQSDTA          S           9999A                                        * REQUEST DATA
010900091026     D RQSLEN          S              4P 0                                      * REQUEST DATA LENGT
011000091026     D RPYOPC          S             10A                                        * REPLY OPCODE
011100091026     D RPYDTA          S           9999A                                        * REPLY DATA
011200091026     D RPYLEN          S              4P 0                                      * REPLY DATA LENGTH
011300091030     D tisi95ds      E DS                  BASED(nullptr)
011400091030     D Kpjba         E DS                  BASED(nullptr)
011500091030     D tabel00f      E DS                  QUALIFIED INZ
011600091030     D azorg00f      E DS                  QUALIFIED INZ
011700091030     D ds5E          E DS                  QUALIFIED INZ
011800091102     D fnlv55ds      E DS                  QUALIFIED INZ
011900091027     D LnaPartenza     S                   LIKE(o95lna)
012000091027     D LnaArrivo       S                   LIKE(o95lna)
012100091102     D Tfp             S                   LIKE(fnlv55ds.d55tfp)
012200091029     D CapPartenza     S                   LIKE(azorg00f.orgcpf)
012300091029     D CapArrivo       S                   LIKE(azorg00f.orgcpf)
012400091029     D MailPartenza    S            255A   VARYING
012500091029     D MailArrivo      S            255A   VARYING
012600091030     D LinguaBartTab   S              1P 0
012700091030     D TempiResaCli    S              3P 0
012800091030     D GgResaCli       S              1S 0
012900091030     D OreResaCli      S              2S 0
013000091030     D GgResaCliAlfa   S              1A
013100091030     D OreResaCliAlfa  S              5A
013200091030     D skTabelKey      S                   LIKE(tabel00f.tblkey)
013300091030     D                                     DIM(18)
013400091030     D skTabelUni      S                   LIKE(tabel00f.tbluni)
013500091030     D                                     DIM(18)
013600091030     D jTabel          S              2S 0
013700091030     D CurrTipoServ    S                   LIKE(tabel00f.tblkey)
013800091026
013900091026     ***************************************************************************
014000091026     **
014100091026     ** Parametri.
014200091026     **
014300091026     ***************************************************************************
014400091026     C     *ENTRY        PLIST
014500091026     C                   PARM                    RQSOPC
014600091026     C                   PARM                    RQSDTA
014700091026     C                   PARM                    RQSLEN
014800091026     C                   PARM                    RPYOPC
014900091026     C                   PARM                    RPYDTA
015000091026     C                   PARM                    RPYLEN
015100091026
015200091026     ***************************************************************************
015300091026     **
015400091026     ** Main.
015500091026     **
015600091026     ***************************************************************************
015700091026      /FREE
015800091026
015900091026       RESET rpyOpc;
016000091026       RESET rpyDta;
016100091026       RESET rpyLen;
016200091026       RESET getTempiO;
016300091026       RESET errori;
016400091026
016500091026       getTempiI = rqsDta;
016600091026
016700091030
016800091030
016900091026       // Partenza.
017000091026
017100091026       RESET tisi95Ds_partenza;
017200091026       tisi95Ds_partenza.i95Cap = getTempiI.capPartenza;
017300091026       tisi95Ds_partenza.i95Loc = getTempiI.localitaPartenza;
017400091026       tisi95Ds_partenza.i95Prv = getTempiI.siglaProvinciaPartenza;
017500091027
017600091027       CLEAR tisi95r1ds;
017700091027       tisi95r1ds.tisi95ds  = tisi95Ds_partenza;
017800091027       tisi95r1ds.si95ilang = getTempiI.lingua;
017900091026
018000091027       ChkCapLocalitaLang(tisi95r1ds);
018100091027       tisi95Ds_partenza = tisi95r1ds.tisi95ds;
018200091027       tisi95Ds_partenza.o95msg = tisi95r1ds.si95odmsg;
018300091127       if tisi95Ds_partenza.o95Lia >= '2' AND
018400091127          tisi95Ds_partenza.o95Lid >= '3';
018500091127       else;
018600091125          tisi95Ds_partenza.o95Err = '*';
018700091125       endif;
018800091030
018900091102
019000091027
019100091026       // Arrivo.
019200091026
019300091026       RESET tisi95Ds_arrivo;
019400091026       tisi95Ds_arrivo.i95Cap = getTempiI.capArrivo;
019500091026       tisi95Ds_arrivo.i95Loc = getTempiI.localitaArrivo;
019600091026       tisi95Ds_arrivo.i95Prv = getTempiI.siglaProvinciaArrivo;
019700091027
019800091027       CLEAR tisi95r1ds;
019900091027       tisi95r1ds.tisi95ds  = tisi95Ds_arrivo;
020000091027       tisi95r1ds.si95ilang = getTempiI.lingua;
020100091026
020200091027       ChkCapLocalitaLang(tisi95r1ds);
020300091027       tisi95Ds_arrivo = tisi95r1ds.tisi95ds;
020400091027       tisi95Ds_arrivo.o95msg = tisi95r1ds.si95odmsg;
020500091127       if tisi95Ds_arrivo.o95Lia >= '2' AND
020600091127          tisi95Ds_arrivo.o95Lid >= '3';
020700091127       else;
020800091127          tisi95Ds_arrivo.o95Err = '*';
020900091127       endif;
021000091026
021100091102
021200091027
021300091026       // Errori.
021400091026
021500091026       IF tisi95Ds_partenza.o95Err <> *BLANK
021600091125       OR tisi95Ds_arrivo.o95Err   <> *BLANK;
021700091026         IF tisi95Ds_partenza.o95Err <> *BLANK;
021800091126           errori.err_partenza = %TRIMR(tisi95Ds_partenza.o95Msg);
021900091026         ENDIF;
022000091126         IF tisi95Ds_arrivo.o95Err <> *BLANK;
022100091126           errori.err_arrivo = %TRIMR(tisi95Ds_arrivo.o95Msg);
022200091126         ENDIF;
022300091029         rpyOpc = 'ERROR';
022400091126         rpyDta = errori;
022500091026         rpyLen = %LEN(errori);
022600091026         RETURN;
022700091026       ENDIF;
022800091027
022900091027
023000091030       if rpyOpc <> 'ERROR';
023100091030
023200091030
023300091030          // Stabilisco la filiale di partenza
023400091030
023500091030          if tisi95Ds_partenza.o95liv = 'C';
023600091030             LnaPartenza = tisi95Ds_partenza.o95cla;
023700091030          else;
023800091030             LnaPartenza = tisi95Ds_partenza.o95lla;
023900091030          endif;
024000091102
024100091102
024200091102          // Reperisco il terminal della filiale partenza
024300091102
024400091102          CLEAR fnlv55ds;
024500091102          fnlv55ds.d55tpt = 'P';
024600091102          fnlv55ds.d55lin = LnaPartenza;
024700091102          fnlv55ds.d55drf = %dec(%date():*ISO);
024800091102          fnlv55r(fnlv55ds);
024900091102          if fnlv55ds.d55err <> *blanks;
025000091102             Tfp = LnaPartenza;
025100091102          else;
025200091102             Tfp = fnlv55ds.d55tfp;
025300091102          endif;
025400091030
025500091102
025600091030
025700091030          // Stabilisco la filiale di arrivo
025800091030
025900091030          if tisi95Ds_arrivo.o95liv = 'C';
026000091030             LnaArrivo = tisi95Ds_arrivo.o95cla;
026100091030          else;
026200091030             LnaArrivo = tisi95Ds_arrivo.o95lla;
026300091030          endif;
026400091030
026500091030
026600091030          // Reperisco attributi filiale di partenza
026700091030
026800091030          getTempiO.filialePartenzaId = LnaPartenza;
026900091030
027000091030          Organigramma_getDescrizioneFiliale( LnaPartenza                      :
027100091030                                          getTempiO.filialePartenzaDescrizione);
027200091029
027300091030          Organigramma_getIndirizzoFiliale( LnaPartenza                        :
027400091030                                          getTempiO.filialePartenzaIndirizzo   :
027500091030                                          CapPartenza                          :
027600091030                                          getTempiO.filialePartenzaLocalita    :
027700091030                                          getTempiO.filialePartenzaProvincia  );
027800091030          getTempiO.filialePartenzaCap =  CapPartenza;
027900091030
028000091030          Organigramma_getRecapitiFiliale( LnaPartenza                         :
028100091030                                          getTempiO.filialePartenzaTelefono    :
028200091030                                          getTempiO.filialePartenzaFAX         :
028300091030                                          *omit                                :
028400091030                                          MailPartenza                        );
028500091030          getTempiO.filialePartenzaEmail = MailPartenza;
028600091030
028700091030
028800091030
028900091030          // Reperisco attributi filiale di arrivo
029000091030
029100091030          getTempiO.filialeArrivoId = LnaArrivo;
029200091030
029300091030          Organigramma_getDescrizioneFiliale( LnaArrivo                        :
029400091030                                          getTempiO.filialeArrivoDescrizione);
029500091029
029600091030          Organigramma_getIndirizzoFiliale( LnaArrivo                          :
029700091030                                          getTempiO.filialeArrivoIndirizzo     :
029800091030                                          CapArrivo                            :
029900091030                                          getTempiO.filialeArrivoLocalita      :
030000091030                                          getTempiO.filialeArrivoProvincia    );
030100091030          getTempiO.filialeArrivoCap =  CapArrivo;
030200091030
030300091030          Organigramma_getRecapitiFiliale( LnaArrivo                           :
030400091030                                          getTempiO.filialeArrivoTelefono      :
030500091030                                          getTempiO.filialeArrivoFAX           :
030600091030                                          *omit                                :
030700091030                                          MailArrivo                          );
030800091030          getTempiO.filialeArrivoEmail = MailArrivo;
030900091030
031000091030
031100091030
031200091030          // Conversione codice lingua da ISO2 a "formato Bartolini"
031300091030
031400091030          clear LinguaBartTab;
031500091030          cvtLinguaISO2ToTabel( getTempiI.lingua                     :
031600091030                                *omit                                :
031700091030                                *omit                                :
031800091030                                LinguaBartTab                       );
031900091030          if LinguaBartTab = *zeros;
032000091126             errori.err_calcolo = 'Codice ISO nazione non valido o non gestito';
032100091030             rpyOpc = 'ERROR';
032200091126             rpyDta = errori;
032300091030             rpyLen = %LEN(errori);
032400091030          else;
032500091030
032600091030             exsr RTVTIPISERVIZI;
032700091030
032800091030             if skTabelKey(1) = *blanks;
032900091030                select;
033000091030                  when getTempiI.lingua = 'it';
033100091126                       errori.err_calcolo = 'Errore interno: tipo ' +
033200091126                                            'servizio spedizione';
033300091030                  when getTempiI.lingua = 'en';
033400091126                       errori.err_calcolo = 'Internal error: shipment ' +
033500091126                                            'service type';
033600091030                  when getTempiI.lingua = 'fr';
033700091126                       errori.err_calcolo = 'Erreur interne: type de ' +
033800091126                                            'services de la expédition';
033900091030                  when getTempiI.lingua = 'de';
034000091126                       errori.err_calcolo = 'Interner Fehler: Typ ' +
034100091126                                            'des Sendungdiensts';
034200091030                  other;
034300091126                       errori.err_calcolo = 'Errore interno: tipo ' +
034400091126                                            'servizio spedizione';
034500091030                endsl;
034600091030                rpyOpc = 'ERROR';
034700091126                rpyDta = errori;
034800091030                rpyLen = %LEN(errori);
034900091030
035000091030             else;
035100091030
035200091030                // Ciclo sulla schiera dei tipi servizio
035300091030                jTabel = *zeros;
035400091030                dow jTabel <= %elem(skTabelKey);
035500091030                    jTabel = jTabel + 1;
035600091030                    if skTabelKey(jTabel) = *blanks;
035700091030                       leave;
035800091030                    else;
035900091030                       CurrTipoServ = skTabelKey(jTabel);
036000091030                       ds5E         = skTabelUni(jTabel);
036100091030                       getTempiO.numeroTipiServizio =
036200091030                                 getTempiO.numeroTipiServizio + 1;
036300091030
036400091030                       getTempiO.listaTipiServizioCodice
036500091030                                (getTempiO.numeroTipiServizio) =
036600091030                                CurrTipoServ;
036700091030
036800091030                       getTempiO.listaTipiServizioDescrizione
036900091030                                (getTempiO.numeroTipiServizio) =
037000091030                                ds5E.§5EDES;
037100091030
037200091030                       exsr RTVTEMPICONSEGNA;
037300091030
037400091030                       if rpyOpc = 'ERROR';
037500091030                          leave;
037600091030                       endif;
037700091030
037800091030                    endif;
037900091030                enddo;
038000091030             endif;
038100091030          endif;
038200091030       endif;
038300091029
038400091026
038500091026       rpyOpc = 'DONE';
038600091026       rpyDta = getTempiO;
038700091026       rpyLen = %SIZE(getTempiO);
038800091026
038900091026       RETURN;
039000091030
039100091030       //***********************************************************************
039200091030       //
039300091030       // Reperimento tempi consegna
039400091030       //
039500091030       //***********************************************************************
039600091030
039700091030       BEGSR RTVTEMPICONSEGNA;
039800091030
039900091030          TempiResaCli   = *zeros;
040000091030          GgResaCli      = *zeros;
040100091030          OreResaCli     = *zeros;
040200091030          GgResaCliAlfa  = *blanks;
040300091030          OreResaCliAlfa = *blanks;
040400091030
040500091030          exec sql select dlytrc into :TempiResaCli from wadly00f
040600091102               where dlylnp = :Tfp                         and
040700091030                     dlytsp = :CurrTipoServ                and
040800091030                     dlynar = ' '                          and
040900091126                     dlycap = :tisi95Ds_arrivo.o95Cap      and
041000091126                     dlyloc = :tisi95Ds_arrivo.o95Loc ;
041100091030
041200091030          if sqlcod < *zeros;
041300091030             select;
041400091030               when getTempiI.lingua = 'it';
041500091126                    errori.err_calcolo = 'Servizio temporaneamente ' +
041600091126                                         'non disponibile';
041700091030               when getTempiI.lingua = 'en';
041800091126                    errori.err_calcolo = 'Service temporarily unavailable';
041900091030               when getTempiI.lingua = 'fr';
042000091126                    errori.err_calcolo = 'Service temporairement indisponible';
042100091030               when getTempiI.lingua = 'de';
042200091126                    errori.err_calcolo = 'Service vorübergehend ' +
042300091126                                         'nicht verfügbar';
042400091030               other;
042500091126                    errori.err_calcolo = 'Servizio temporaneamente ' +
042600091126                                         'non disponibile';
042700091030             endsl;
042800091030             rpyOpc = 'ERROR';
042900091126             rpyDta = errori;
043000091030             rpyLen = %LEN(errori);
043100091030
043200091030          else;
043300091030
043400091030             GgResaCli  = %div(TempiResaCli:24);  // "giornalizzo" i tempi resa cliente
043500091030             OreResaCli = %rem(TempiResaCli:24);  // Ottengo le eventuali frazioni di giorno
043600091030             if OreResaCli <> *zeros;
043700091030                GgResaCli  = GgResaCli + 1;       // Se frazione di giorno => + 1 giorno
043800091030             endif;
043900091030
044000091030             if CurrTipoServ = 'H';
044100091030                select;
044200091030                  when TempiResaCli = 10;
044300091103                       GgResaCliAlfa  = '1';
044400091030                       OreResaCliAlfa = '10:30';
044500091030                  when TempiResaCli = 34;
044600091030                       GgResaCliAlfa  = '2';
044700091030                       OreResaCliAlfa = '10:30';
044800091030                  other;
044900091103                       GgResaCliAlfa  = 'N';
045000091030                       OreResaCliAlfa = *blanks;
045100091030                endsl;
045200091030
045300091030             else;
045400091030
045500091030                GgResaCliAlfa  = %char(GgResaCli);
045600091030                if OreResaCli = 12;
045700091030                   OreResaCliAlfa = '12:00';
045800091030                else;
045900091030                   OreResaCliAlfa = *blanks;
046000091030                endif;
046100091030
046200091030             endif;
046300091030
046400091030
046500091030
046600091030             // Compondo la descrizione delle tempistiche
046700091030             getTempiO.listaTipiServizioDisponibili
046800091030                      (getTempiO.numeroTipiServizio) = '1';
046900091030
047000091030             select;
047100091030
047200091030               // Italiano
047300091030               when getTempiI.lingua = 'it';
047400091030                    select;
047500091103                      when GgResaCliAlfa = 'N';
047600091030                         getTempiO.listaTipiServizioTempi
047700091030                                  (getTempiO.numeroTipiServizio) =
047800091030                            'Servizio non disponibile';
047900091030
048000091030                         getTempiO.listaTipiServizioDisponibili
048100091030                                  (getTempiO.numeroTipiServizio) = '0';
048200091030
048300091030
048400091030                      when GgResaCliAlfa = '1' and OreResaCliAlfa = *blanks;
048500091030                         getTempiO.listaTipiServizioTempi
048600091030                                  (getTempiO.numeroTipiServizio) =
048700091030                                  'Il giorno lavorativo successivo';
048800091030
048900091030
049000091030                      when GgResaCliAlfa = '1' and OreResaCliAlfa <> *blanks;
049100091030                         getTempiO.listaTipiServizioTempi
049200091030                                  (getTempiO.numeroTipiServizio) =
049300091030                            'Entro le ore ' + OreResaCliAlfa +
049400091030                            ' del giorno lavorativo successivo';
049500091030
049600091030
049700091030                      when GgResaCliAlfa > '1' and OreResaCliAlfa = *blanks;
049800091030                         getTempiO.listaTipiServizioTempi
049900091030                                  (getTempiO.numeroTipiServizio) =
050000091030                                  GgResaCliAlfa + ' giorni lavorativi';
050100091030
050200091030
050300091030                      when GgResaCliAlfa > '1' and OreResaCliAlfa <> *blanks;
050400091030                         getTempiO.listaTipiServizioTempi
050500091030                                  (getTempiO.numeroTipiServizio) =
050600091030                            'Entro le ore ' + OreResaCliAlfa +
050700091030                            ' del ' + GgResaCliAlfa          +
050800091030                            '° giorno lavorativo successivo';
050900091030
051000091030                    endsl;
051100091030
051200091030
051300091030               // Inglese
051400091030               when getTempiI.lingua = 'en';
051500091030                    select;
051600091103                      when GgResaCliAlfa = 'N';
051700091030                         getTempiO.listaTipiServizioTempi
051800091030                                  (getTempiO.numeroTipiServizio) =
051900091104                            'Unavailable service';
052000091030
052100091030                         getTempiO.listaTipiServizioDisponibili
052200091030                                  (getTempiO.numeroTipiServizio) = '0';
052300091030
052400091030
052500091030                      when GgResaCliAlfa = '1' and OreResaCliAlfa = *blanks;
052600091030                         getTempiO.listaTipiServizioTempi
052700091030                                  (getTempiO.numeroTipiServizio) =
052800091104                                  'The following working day';
052900091030
053000091030
053100091030                      when GgResaCliAlfa = '1' and OreResaCliAlfa <> *blanks;
053200091030                         getTempiO.listaTipiServizioTempi
053300091030                                  (getTempiO.numeroTipiServizio) =
053400091104                            'Within ' + OreResaCliAlfa +
053500091104                            ' of the following working day';
053600091030
053700091030
053800091030                      when GgResaCliAlfa > '1' and OreResaCliAlfa = *blanks;
053900091030                         getTempiO.listaTipiServizioTempi
054000091030                                  (getTempiO.numeroTipiServizio) =
054100091104                                  GgResaCliAlfa + ' working days';
054200091030
054300091030
054400091030                      when GgResaCliAlfa > '1' and OreResaCliAlfa <> *blanks;
054500091030                         getTempiO.listaTipiServizioTempi
054600091030                                  (getTempiO.numeroTipiServizio) =
054700091104                            'Within '  + OreResaCliAlfa +
054800091104                            ' of the ' + GgResaCliAlfa  +
054900091104                            '° following working day';
055000091030
055100091030                    endsl;
055200091030
055300091030
055400091030               // Francese
055500091030               when getTempiI.lingua = 'fr';
055600091030                    select;
055700091103                      when GgResaCliAlfa = 'N';
055800091030                         getTempiO.listaTipiServizioTempi
055900091030                                  (getTempiO.numeroTipiServizio) =
056000091104                            'Service indisponible';
056100091030
056200091030                         getTempiO.listaTipiServizioDisponibili
056300091030                                  (getTempiO.numeroTipiServizio) = '0';
056400091030
056500091030
056600091030                      when GgResaCliAlfa = '1' and OreResaCliAlfa = *blanks;
056700091030                         getTempiO.listaTipiServizioTempi
056800091030                                  (getTempiO.numeroTipiServizio) =
056900091104                                  'Le jour ouvrable suivant';
057000091030
057100091030
057200091030                      when GgResaCliAlfa = '1' and OreResaCliAlfa <> *blanks;
057300091030                         getTempiO.listaTipiServizioTempi
057400091030                                  (getTempiO.numeroTipiServizio) =
057500091104                            'Dans ' + OreResaCliAlfa +
057600091104                            ' heures du jour ouvrable suivant';
057700091030
057800091030
057900091030                      when GgResaCliAlfa > '1' and OreResaCliAlfa = *blanks;
058000091030                         getTempiO.listaTipiServizioTempi
058100091030                                  (getTempiO.numeroTipiServizio) =
058200091104                                  GgResaCliAlfa + ' jours ouvrables';
058300091030
058400091030
058500091030                      when GgResaCliAlfa > '1' and OreResaCliAlfa <> *blanks;
058600091030                         getTempiO.listaTipiServizioTempi
058700091030                                  (getTempiO.numeroTipiServizio) =
058800091104                            'Dans '       + OreResaCliAlfa +
058900091104                            ' heures du ' + GgResaCliAlfa  +
059000091104                            '° jour ouvrable suivant';
059100091030
059200091030                    endsl;
059300091030
059400091030
059500091030               // Tedesco
059600091030               when getTempiI.lingua = 'de';
059700091030                    select;
059800091103                      when GgResaCliAlfa = 'N';
059900091030                         getTempiO.listaTipiServizioTempi
060000091030                                  (getTempiO.numeroTipiServizio) =
060100091104                            'Nicht vorrätiger Dienst';
060200091030
060300091030                         getTempiO.listaTipiServizioDisponibili
060400091030                                  (getTempiO.numeroTipiServizio) = '0';
060500091030
060600091030
060700091030                      when GgResaCliAlfa = '1' and OreResaCliAlfa = *blanks;
060800091030                         getTempiO.listaTipiServizioTempi
060900091030                                  (getTempiO.numeroTipiServizio) =
061000091104                                  'Der nächste Arbeits-Tag';
061100091030
061200091030
061300091030                      when GgResaCliAlfa = '1' and OreResaCliAlfa <> *blanks;
061400091030                         getTempiO.listaTipiServizioTempi
061500091030                                  (getTempiO.numeroTipiServizio) =
061600091104                            'Innerhalb ' + OreResaCliAlfa +
061700091104                            ' Uhr des nächstes Arbeits-Tag';
061800091030
061900091030
062000091030                      when GgResaCliAlfa > '1' and OreResaCliAlfa = *blanks;
062100091030                         getTempiO.listaTipiServizioTempi
062200091030                                  (getTempiO.numeroTipiServizio) =
062300091104                                  GgResaCliAlfa + ' Arbeits-Tage';
062400091030
062500091030
062600091030                      when GgResaCliAlfa > '1' and OreResaCliAlfa <> *blanks;
062700091030                         getTempiO.listaTipiServizioTempi
062800091030                                  (getTempiO.numeroTipiServizio) =
062900091104                            'Innerhalb '         + OreResaCliAlfa +
063000091104                            ' Uhr des nächstes ' + GgResaCliAlfa  +
063100091104                            '° Arbeits-Tag';
063200091030
063300091030                    endsl;
063400091030
063500091030               other;
063600091030
063700091030
063800091030               // Italiano
063900091030                    select;
064000091103                      when GgResaCliAlfa = 'N';
064100091030                         getTempiO.listaTipiServizioTempi
064200091030                                  (getTempiO.numeroTipiServizio) =
064300091030                            'Servizio non disponibile';
064400091030
064500091030                         getTempiO.listaTipiServizioDisponibili
064600091030                                  (getTempiO.numeroTipiServizio) = '0';
064700091030
064800091030
064900091030                      when GgResaCliAlfa = '1' and OreResaCliAlfa = *blanks;
065000091030                         getTempiO.listaTipiServizioTempi
065100091030                                  (getTempiO.numeroTipiServizio) =
065200091030                                  'Il giorno lavorativo successivo';
065300091030
065400091030
065500091030                      when GgResaCliAlfa = '1' and OreResaCliAlfa <> *blanks;
065600091030                         getTempiO.listaTipiServizioTempi
065700091030                                  (getTempiO.numeroTipiServizio) =
065800091030                            'Entro le ore ' + OreResaCliAlfa +
065900091030                            ' del giorno lavorativo successivo';
066000091030
066100091030
066200091030                      when GgResaCliAlfa > '1' and OreResaCliAlfa = *blanks;
066300091030                         getTempiO.listaTipiServizioTempi
066400091030                                  (getTempiO.numeroTipiServizio) =
066500091030                                  GgResaCliAlfa + ' giorni lavorativi';
066600091030
066700091030
066800091030                      when GgResaCliAlfa > '1' and OreResaCliAlfa <> *blanks;
066900091030                         getTempiO.listaTipiServizioTempi
067000091030                                  (getTempiO.numeroTipiServizio) =
067100091030                            'Entro le ore ' + OreResaCliAlfa +
067200091030                            ' del ' + GgResaCliAlfa          +
067300091030                            '° giorno lavorativo successivo';
067400091030
067500091030                    endsl;
067600091030               endsl;
067700091030
067800091030           endif;
067900091030
068000091030       ENDSR;
068100091030
068200091030
068300091030
068400091030       //***********************************************************************
068500091030       //
068600091030       // Caricamento tabelle tipi servizio (in lungua)
068700091030       //
068800091030       //***********************************************************************
068900091030
069000091030       BEGSR RTVTIPISERVIZI;
069100091030
069200091030         // Caricamento tipi servizi internet
069300091030         exec sql declare tabel no scroll cursor for
069400091030              select * from tabel00f
069500091030              where tblkut = :LinguaBartTab    and
069600091030                    tblcod = '5E'              and
069700091030                    substr(tbluni, 83, 1) = 'S'
069800091030              order by substr(tbluni, 81, 2);
069900091030
070000091030         exec sql open tabel;
070100091030
070200091030         exec sql fetch next from tabel into :tabel00f;
070300091030
070400091030         dow sqlcod >= *zeros;
070500091030             if sqlcod = 100;
070600091030                leave;
070700091030             else;
070800091030                jTabel             = jTabel + 1;
070900091030                skTabelKey(jTabel) = tabel00f.tblkey;
071000091030                skTabelUni(jTabel) = tabel00f.tbluni;
071100091030             endif;
071200091030             exec sql fetch next from tabel into :tabel00f;
071300091030         enddo;
071400100107
071500100107         exec sql close tabel;
071600091030
071700091030       ENDSR;
071800091026
071900091030
072000091030
072100091026       //***********************************************************************
072200091026       //
072300091026       // Inizializzazioni.
072400091026       //
072500091026       //***********************************************************************
072600091026
072700091026       BEGSR *INZSR;
072800091026
072900091026         CLEAR tisi95Ds_partenza;
073000091026         CLEAR tisi95Ds_arrivo;
073100091027         tisi95Ds_partenza.i95Tcn = '7';
073200091027         tisi95Ds_arrivo.i95Tcn = '7';
073300091026         Organigramma_init();
073400091026
073500091026       ENDSR;
073600091026
073700091026      /END-FREE

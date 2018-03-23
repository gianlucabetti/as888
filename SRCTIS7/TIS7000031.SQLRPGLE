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
001300170720     D/COPY GAITRASRC/SRCPROTOPR,XGIOLAV
001400170720     D/COPY GAITRASRC/SRCPROTOPI,XGIOLAV
001500170720     D/COPY GAITRASRC/SRCPROTOPR,TIS7000031
001600170720     D/COPY GAITRASRC/SRCPROTOPI,TIS7000031
001700091026
001800091026     ***************************************************************************
001900091026     **
002000091026     ** Strutture dati.
002100091026     **
002200091026     ***************************************************************************
002300091026     D tisi95Ds_partenza...
002400091026     D                 DS                  LIKEDS(tisi95Ds)
002500091026     D                                     INZ
002600091026     D tisi95Ds_arrivo...
002700091026     D                 DS                  LIKEDS(tisi95Ds)
002800091026     D                                     INZ
002900091030     D*
003000091026
003100091026     ***************************************************************************
003200091026     **
003300091026     ** Campi.
003400091026     **
003500091026     ***************************************************************************
003600091030     D tisi95ds      E DS                  BASED(nullptr)
003700091030     D Kpjba         E DS                  BASED(nullptr)
003800091030     D tabel00f      E DS                  QUALIFIED INZ
003900091030     D azorg00f      E DS                  QUALIFIED INZ
004000091030     D ds5E          E DS                  QUALIFIED INZ
004100091102     D fnlv55ds      E DS                  QUALIFIED INZ
004200091027     D LnaPartenza     S                   LIKE(o95lna)
004300091027     D LnaArrivo       S                   LIKE(o95lna)
004400091102     D Tfp             S                   LIKE(fnlv55ds.d55tfp)
004500091029     D CapPartenza     S                   LIKE(azorg00f.orgcpf)
004600091029     D CapArrivo       S                   LIKE(azorg00f.orgcpf)
004700091029     D MailPartenza    S            255A   VARYING
004800091029     D MailArrivo      S            255A   VARYING
004900091030     D LinguaBartTab   S              1P 0
005000091030     D TempiResaCli    S              3P 0
005100091030     D GgResaCli       S              1S 0
005200091030     D OreResaCli      S              2S 0
005300091030     D GgResaCliAlfa   S              1A
005400091030     D OreResaCliAlfa  S              5A
005500091030     D skTabelKey      S                   LIKE(tabel00f.tblkey)
005600091030     D                                     DIM(18)
005700091030     D skTabelUni      S                   LIKE(tabel00f.tbluni)
005800091030     D                                     DIM(18)
005900091030     D jTabel          S              2S 0
006000091030     D CurrTipoServ    S                   LIKE(tabel00f.tblkey)
006100091026
006200091026     ***************************************************************************
006300091026     **
006400091026     ** Parametri.
006500091026     **
006600091026     ***************************************************************************
006700091026     C     *ENTRY        PLIST
006800170720     C                   PARM                    DataRif
006900170720     C                   PARM                    getTempiI
007000170720     C                   PARM                    getTempiO
007100170720     C                   PARM                    getTempiO2
007200170720     C                   PARM                    errori
007300170720     C                   PARM                    esito
007400091026
007500091026     ***************************************************************************
007600091026     **
007700091026     ** Main.
007800091026     **
007900091026     ***************************************************************************
008000091026      /FREE
008100091026
008200170720       CLEAR getTempiO;
008300170720       CLEAR getTempiO2;
008400170720       CLEAR errori;
008500170720       CLEAR esito;
008600091030
008700170720       // Monitorizzo intero flusso *pgm
008800170720       Monitor;
008900091030
009000091026       // Partenza.
009100091026
009200170720       CLEAR tisi95Ds_partenza;
009300170720       tisi95Ds_partenza.i95Tcn = '7';
009400170720       tisi95Ds_partenza.i95Dat = DataRif;
009500091026       tisi95Ds_partenza.i95Cap = getTempiI.capPartenza;
009600091026       tisi95Ds_partenza.i95Loc = getTempiI.localitaPartenza;
009700091026       tisi95Ds_partenza.i95Prv = getTempiI.siglaProvinciaPartenza;
009800091027
009900091027       CLEAR tisi95r1ds;
010000091027       tisi95r1ds.tisi95ds  = tisi95Ds_partenza;
010100091027       tisi95r1ds.si95ilang = getTempiI.lingua;
010200091026
010300091027       ChkCapLocalitaLang(tisi95r1ds);
010400091027       tisi95Ds_partenza = tisi95r1ds.tisi95ds;
010500091027       tisi95Ds_partenza.o95msg = tisi95r1ds.si95odmsg;
010600091127       if tisi95Ds_partenza.o95Lia >= '2' AND
010700091127          tisi95Ds_partenza.o95Lid >= '3';
010800091127       else;
010900091125          tisi95Ds_partenza.o95Err = '*';
011000091125       endif;
011100091030
011200091102
011300091027
011400091026       // Arrivo.
011500091026
011600170720       CLEAR tisi95Ds_arrivo;
011700170720       tisi95Ds_arrivo.i95Tcn = '7';
011800170720       tisi95Ds_arrivo.i95Dat = DataRif;
011900091026       tisi95Ds_arrivo.i95Cap = getTempiI.capArrivo;
012000091026       tisi95Ds_arrivo.i95Loc = getTempiI.localitaArrivo;
012100091026       tisi95Ds_arrivo.i95Prv = getTempiI.siglaProvinciaArrivo;
012200091027
012300091027       CLEAR tisi95r1ds;
012400091027       tisi95r1ds.tisi95ds  = tisi95Ds_arrivo;
012500091027       tisi95r1ds.si95ilang = getTempiI.lingua;
012600091026
012700091027       ChkCapLocalitaLang(tisi95r1ds);
012800091027       tisi95Ds_arrivo = tisi95r1ds.tisi95ds;
012900091027       tisi95Ds_arrivo.o95msg = tisi95r1ds.si95odmsg;
013000091127       if tisi95Ds_arrivo.o95Lia >= '2' AND
013100091127          tisi95Ds_arrivo.o95Lid >= '3';
013200091127       else;
013300091127          tisi95Ds_arrivo.o95Err = '*';
013400091127       endif;
013500091026
013600091102
013700091027
013800170720       // Errori
013900091026
014000091026       IF tisi95Ds_partenza.o95Err <> *BLANK
014100091125       OR tisi95Ds_arrivo.o95Err   <> *BLANK;
014200091026         IF tisi95Ds_partenza.o95Err <> *BLANK;
014300091126           errori.err_partenza = %TRIMR(tisi95Ds_partenza.o95Msg);
014400091026         ENDIF;
014500091126         IF tisi95Ds_arrivo.o95Err <> *BLANK;
014600091126           errori.err_arrivo = %TRIMR(tisi95Ds_arrivo.o95Msg);
014700091126         ENDIF;
014800170720         esito = 2;
014900091026         RETURN;
015000091026       ENDIF;
015100091027
015200091027
015300170720       if esito <> 2;
015400091030
015500091030
015600091030          // Stabilisco la filiale di partenza
015700091030
015800091030          if tisi95Ds_partenza.o95liv = 'C';
015900091030             LnaPartenza = tisi95Ds_partenza.o95cla;
016000091030          else;
016100091030             LnaPartenza = tisi95Ds_partenza.o95lla;
016200091030          endif;
016300091102
016400091102
016500091102          // Reperisco il terminal della filiale partenza
016600091102
016700091102          CLEAR fnlv55ds;
016800091102          fnlv55ds.d55tpt = 'P';
016900091102          fnlv55ds.d55lin = LnaPartenza;
017000091102          fnlv55ds.d55drf = %dec(%date():*ISO);
017100091102          fnlv55r(fnlv55ds);
017200091102          if fnlv55ds.d55err <> *blanks;
017300091102             Tfp = LnaPartenza;
017400091102          else;
017500091102             Tfp = fnlv55ds.d55tfp;
017600091102          endif;
017700091030
017800091102
017900091030
018000091030          // Stabilisco la filiale di arrivo
018100091030
018200091030          if tisi95Ds_arrivo.o95liv = 'C';
018300091030             LnaArrivo = tisi95Ds_arrivo.o95cla;
018400091030          else;
018500091030             LnaArrivo = tisi95Ds_arrivo.o95lla;
018600091030          endif;
018700091030
018800091030
018900091030          // Reperisco attributi filiale di partenza
019000091030
019100091030          getTempiO.filialePartenzaId = LnaPartenza;
019200091030
019300091030          Organigramma_getDescrizioneFiliale( LnaPartenza                      :
019400091030                                          getTempiO.filialePartenzaDescrizione);
019500091029
019600091030          Organigramma_getIndirizzoFiliale( LnaPartenza                        :
019700091030                                          getTempiO.filialePartenzaIndirizzo   :
019800091030                                          CapPartenza                          :
019900091030                                          getTempiO.filialePartenzaLocalita    :
020000091030                                          getTempiO.filialePartenzaProvincia  );
020100091030          getTempiO.filialePartenzaCap =  CapPartenza;
020200091030
020300091030          Organigramma_getRecapitiFiliale( LnaPartenza                         :
020400091030                                          getTempiO.filialePartenzaTelefono    :
020500091030                                          getTempiO.filialePartenzaFAX         :
020600091030                                          *omit                                :
020700091030                                          MailPartenza                        );
020800091030          getTempiO.filialePartenzaEmail = MailPartenza;
020900091030
021000091030
021100091030
021200091030          // Reperisco attributi filiale di arrivo
021300091030
021400091030          getTempiO.filialeArrivoId = LnaArrivo;
021500091030
021600091030          Organigramma_getDescrizioneFiliale( LnaArrivo                        :
021700091030                                          getTempiO.filialeArrivoDescrizione);
021800091029
021900091030          Organigramma_getIndirizzoFiliale( LnaArrivo                          :
022000091030                                          getTempiO.filialeArrivoIndirizzo     :
022100091030                                          CapArrivo                            :
022200091030                                          getTempiO.filialeArrivoLocalita      :
022300091030                                          getTempiO.filialeArrivoProvincia    );
022400091030          getTempiO.filialeArrivoCap =  CapArrivo;
022500091030
022600091030          Organigramma_getRecapitiFiliale( LnaArrivo                           :
022700091030                                          getTempiO.filialeArrivoTelefono      :
022800091030                                          getTempiO.filialeArrivoFAX           :
022900091030                                          *omit                                :
023000091030                                          MailArrivo                          );
023100091030          getTempiO.filialeArrivoEmail = MailArrivo;
023200091030
023300091030
023400091030
023500091030          // Conversione codice lingua da ISO2 a "formato Bartolini"
023600091030
023700091030          clear LinguaBartTab;
023800091030          cvtLinguaISO2ToTabel( getTempiI.lingua                     :
023900091030                                *omit                                :
024000091030                                *omit                                :
024100091030                                LinguaBartTab                       );
024200091030          if LinguaBartTab = *zeros;
024300091126             errori.err_calcolo = 'Codice ISO nazione non valido o non gestito';
024400170720             esito = 2;
024500091030          else;
024600091030
024700091030             exsr RTVTIPISERVIZI;
024800091030
024900091030             if skTabelKey(1) = *blanks;
025000091030                select;
025100091030                  when getTempiI.lingua = 'it';
025200091126                       errori.err_calcolo = 'Errore interno: tipo ' +
025300091126                                            'servizio spedizione';
025400091030                  when getTempiI.lingua = 'en';
025500091126                       errori.err_calcolo = 'Internal error: shipment ' +
025600091126                                            'service type';
025700091030                  when getTempiI.lingua = 'fr';
025800091126                       errori.err_calcolo = 'Erreur interne: type de ' +
025900091126                                            'services de la expédition';
026000091030                  when getTempiI.lingua = 'de';
026100091126                       errori.err_calcolo = 'Interner Fehler: Typ ' +
026200091126                                            'des Sendungdiensts';
026300091030                  other;
026400091126                       errori.err_calcolo = 'Errore interno: tipo ' +
026500091126                                            'servizio spedizione';
026600091030                endsl;
026700170720                esito = 2;
026800091030
026900091030             else;
027000091030
027100091030                // Ciclo sulla schiera dei tipi servizio
027200091030                jTabel = *zeros;
027300091030                dow jTabel <= %elem(skTabelKey);
027400091030                    jTabel = jTabel + 1;
027500091030                    if skTabelKey(jTabel) = *blanks;
027600091030                       leave;
027700091030                    else;
027800091030                       CurrTipoServ = skTabelKey(jTabel);
027900091030                       ds5E         = skTabelUni(jTabel);
028000091030                       getTempiO.numeroTipiServizio =
028100091030                                 getTempiO.numeroTipiServizio + 1;
028200091030
028300091030                       getTempiO.listaTipiServizioCodice
028400091030                                (getTempiO.numeroTipiServizio) =
028500091030                                CurrTipoServ;
028600091030
028700091030                       getTempiO.listaTipiServizioDescrizione
028800091030                                (getTempiO.numeroTipiServizio) =
028900091030                                ds5E.§5EDES;
029000091030
029100091030                       exsr RTVTEMPICONSEGNA;
029200091030
029300170720                       if esito = 2;
029400091030                          leave;
029500091030                       endif;
029600091030
029700091030                    endif;
029800091030                enddo;
029900091030             endif;
030000091030          endif;
030100091030       endif;
030200091029
030300170720       // Gestisco errore *pgm
030400170720       On-Error;
030500170720          esito = 2;
030600170720          dump(A);
030700170720          return;
030800170720
030900170720       // Fine monitoring
031000170720       Endmon;
031100170720
031200170720       esito = 1;
031300170720
031400091026
031500091026       RETURN;
031600091030
031700091030       //***********************************************************************
031800091030       //
031900091030       // Reperimento tempi consegna
032000091030       //
032100091030       //***********************************************************************
032200091030
032300091030       BEGSR RTVTEMPICONSEGNA;
032400091030
032500091030          TempiResaCli   = *zeros;
032600091030          GgResaCli      = *zeros;
032700091030          OreResaCli     = *zeros;
032800091030          GgResaCliAlfa  = *blanks;
032900091030          OreResaCliAlfa = *blanks;
033000091030
033100091030          exec sql select dlytrc into :TempiResaCli from wadly00f
033200091102               where dlylnp = :Tfp                         and
033300091030                     dlytsp = :CurrTipoServ                and
033400091030                     dlynar = ' '                          and
033500091126                     dlycap = :tisi95Ds_arrivo.o95Cap      and
033600091126                     dlyloc = :tisi95Ds_arrivo.o95Loc ;
033700091030
033800091030          if sqlcod < *zeros;
033900091030             select;
034000091030               when getTempiI.lingua = 'it';
034100091126                    errori.err_calcolo = 'Servizio temporaneamente ' +
034200091126                                         'non disponibile';
034300091030               when getTempiI.lingua = 'en';
034400091126                    errori.err_calcolo = 'Service temporarily unavailable';
034500091030               when getTempiI.lingua = 'fr';
034600091126                    errori.err_calcolo = 'Service temporairement indisponible';
034700091030               when getTempiI.lingua = 'de';
034800091126                    errori.err_calcolo = 'Service vorübergehend ' +
034900091126                                         'nicht verfügbar';
035000091030               other;
035100091126                    errori.err_calcolo = 'Servizio temporaneamente ' +
035200091126                                         'non disponibile';
035300091030             endsl;
035400170720             esito = 2;
035500091030
035600091030          else;
035700091030
035800091030             GgResaCli  = %div(TempiResaCli:24);  // "giornalizzo" i tempi resa cliente
035900091030             OreResaCli = %rem(TempiResaCli:24);  // Ottengo le eventuali frazioni di giorno
036000091030             if OreResaCli <> *zeros;
036100091030                GgResaCli  = GgResaCli + 1;       // Se frazione di giorno => + 1 giorno
036200091030             endif;
036300091030
036400091030             if CurrTipoServ = 'H';
036500091030                select;
036600091030                  when TempiResaCli = 10;
036700170720                       GgResaCli      = 1;
036800091103                       GgResaCliAlfa  = '1';
036900091030                       OreResaCliAlfa = '10:30';
037000091030                  when TempiResaCli = 34;
037100170720                       GgResaCli      = 2;
037200091030                       GgResaCliAlfa  = '2';
037300091030                       OreResaCliAlfa = '10:30';
037400091030                  other;
037500170720                       GgResaCli      = *zeros;
037600091103                       GgResaCliAlfa  = 'N';
037700091030                       OreResaCliAlfa = *blanks;
037800091030                endsl;
037900091030
038000091030             else;
038100091030
038200091030                GgResaCliAlfa  = %char(GgResaCli);
038300091030                if OreResaCli = 12;
038400091030                   OreResaCliAlfa = '12:00';
038500091030                else;
038600091030                   OreResaCliAlfa = *blanks;
038700091030                endif;
038800091030
038900091030             endif;
039000091030
039100091030
039200091030
039300091030             // Compondo la descrizione delle tempistiche
039400091030             getTempiO.listaTipiServizioDisponibili
039500091030                      (getTempiO.numeroTipiServizio) = '1';
039600091030
039700170720             getTempiO2.listaTipiServizioGg
039800170720                      (getTempiO.numeroTipiServizio) = GgResaCliAlfa;
039900170720
040000170720             // Calcolo la data "finale"
040100170720             if GgResaCli > *zeros;
040200170720
040300170720               clear XGIOLAVDS;
040400170720               XGIOLAVDS.IXGLDATA = DataRif;
040500170720               XGIOLAVDS.IXGLFIL  = LnaArrivo;
040600170720               XGIOLAVDS.IXGLPA   = 'A';
040700170720               XGIOLAVDS.IXGLADD  = 'S';
040800170720               XGIOLAVDS.IXGLGG   = GgResaCli;
040900170720               XGIOLAVDS.IXGLLAV  = 'S';
041000170720               XGIOLAV(XGIOLAVDS);
041100170720
041200170720               if XGIOLAVDS.OXGLERR = *blanks;
041300170720
041400170720                  getTempiO2.listaTipiServizioData
041500170720                         (getTempiO.numeroTipiServizio) = XGIOLAVDS.OXGLDATA;
041600170720
041700170720                  getTempiO2.listaTipiServizioOra
041800170720                         (getTempiO.numeroTipiServizio) = OreResaCliAlfa;
041900170720
042000170720               endif;
042100170720
042200170720             endif;
042300170720
042400170720
042500170720             // Descrizioni
042600170720             select;
042700170720
042800170720               // Italiano
042900091030               when getTempiI.lingua = 'it';
043000091030                    select;
043100091103                      when GgResaCliAlfa = 'N';
043200091030                         getTempiO.listaTipiServizioTempi
043300091030                                  (getTempiO.numeroTipiServizio) =
043400091030                            'Servizio non disponibile';
043500091030
043600091030                         getTempiO.listaTipiServizioDisponibili
043700091030                                  (getTempiO.numeroTipiServizio) = '0';
043800091030
043900091030
044000091030                      when GgResaCliAlfa = '1' and OreResaCliAlfa = *blanks;
044100091030                         getTempiO.listaTipiServizioTempi
044200091030                                  (getTempiO.numeroTipiServizio) =
044300091030                                  'Il giorno lavorativo successivo';
044400091030
044500091030
044600091030                      when GgResaCliAlfa = '1' and OreResaCliAlfa <> *blanks;
044700091030                         getTempiO.listaTipiServizioTempi
044800091030                                  (getTempiO.numeroTipiServizio) =
044900091030                            'Entro le ore ' + OreResaCliAlfa +
045000091030                            ' del giorno lavorativo successivo';
045100091030
045200091030
045300091030                      when GgResaCliAlfa > '1' and OreResaCliAlfa = *blanks;
045400091030                         getTempiO.listaTipiServizioTempi
045500091030                                  (getTempiO.numeroTipiServizio) =
045600091030                                  GgResaCliAlfa + ' giorni lavorativi';
045700091030
045800091030
045900091030                      when GgResaCliAlfa > '1' and OreResaCliAlfa <> *blanks;
046000091030                         getTempiO.listaTipiServizioTempi
046100091030                                  (getTempiO.numeroTipiServizio) =
046200091030                            'Entro le ore ' + OreResaCliAlfa +
046300091030                            ' del ' + GgResaCliAlfa          +
046400091030                            '° giorno lavorativo successivo';
046500091030
046600091030                    endsl;
046700091030
046800091030
046900091030               // Inglese
047000091030               when getTempiI.lingua = 'en';
047100091030                    select;
047200091103                      when GgResaCliAlfa = 'N';
047300091030                         getTempiO.listaTipiServizioTempi
047400091030                                  (getTempiO.numeroTipiServizio) =
047500091104                            'Unavailable service';
047600091030
047700091030                         getTempiO.listaTipiServizioDisponibili
047800091030                                  (getTempiO.numeroTipiServizio) = '0';
047900091030
048000091030
048100091030                      when GgResaCliAlfa = '1' and OreResaCliAlfa = *blanks;
048200091030                         getTempiO.listaTipiServizioTempi
048300091030                                  (getTempiO.numeroTipiServizio) =
048400091104                                  'The following working day';
048500091030
048600091030
048700091030                      when GgResaCliAlfa = '1' and OreResaCliAlfa <> *blanks;
048800091030                         getTempiO.listaTipiServizioTempi
048900091030                                  (getTempiO.numeroTipiServizio) =
049000091104                            'Within ' + OreResaCliAlfa +
049100091104                            ' of the following working day';
049200091030
049300091030
049400091030                      when GgResaCliAlfa > '1' and OreResaCliAlfa = *blanks;
049500091030                         getTempiO.listaTipiServizioTempi
049600091030                                  (getTempiO.numeroTipiServizio) =
049700091104                                  GgResaCliAlfa + ' working days';
049800091030
049900091030
050000091030                      when GgResaCliAlfa > '1' and OreResaCliAlfa <> *blanks;
050100091030                         getTempiO.listaTipiServizioTempi
050200091030                                  (getTempiO.numeroTipiServizio) =
050300091104                            'Within '  + OreResaCliAlfa +
050400091104                            ' of the ' + GgResaCliAlfa  +
050500091104                            '° following working day';
050600091030
050700091030                    endsl;
050800091030
050900091030
051000091030               // Francese
051100091030               when getTempiI.lingua = 'fr';
051200091030                    select;
051300091103                      when GgResaCliAlfa = 'N';
051400091030                         getTempiO.listaTipiServizioTempi
051500091030                                  (getTempiO.numeroTipiServizio) =
051600091104                            'Service indisponible';
051700091030
051800091030                         getTempiO.listaTipiServizioDisponibili
051900091030                                  (getTempiO.numeroTipiServizio) = '0';
052000091030
052100091030
052200091030                      when GgResaCliAlfa = '1' and OreResaCliAlfa = *blanks;
052300091030                         getTempiO.listaTipiServizioTempi
052400091030                                  (getTempiO.numeroTipiServizio) =
052500091104                                  'Le jour ouvrable suivant';
052600091030
052700091030
052800091030                      when GgResaCliAlfa = '1' and OreResaCliAlfa <> *blanks;
052900091030                         getTempiO.listaTipiServizioTempi
053000091030                                  (getTempiO.numeroTipiServizio) =
053100091104                            'Dans ' + OreResaCliAlfa +
053200091104                            ' heures du jour ouvrable suivant';
053300091030
053400091030
053500091030                      when GgResaCliAlfa > '1' and OreResaCliAlfa = *blanks;
053600091030                         getTempiO.listaTipiServizioTempi
053700091030                                  (getTempiO.numeroTipiServizio) =
053800091104                                  GgResaCliAlfa + ' jours ouvrables';
053900091030
054000091030
054100091030                      when GgResaCliAlfa > '1' and OreResaCliAlfa <> *blanks;
054200091030                         getTempiO.listaTipiServizioTempi
054300091030                                  (getTempiO.numeroTipiServizio) =
054400091104                            'Dans '       + OreResaCliAlfa +
054500091104                            ' heures du ' + GgResaCliAlfa  +
054600091104                            '° jour ouvrable suivant';
054700091030
054800091030                    endsl;
054900091030
055000091030
055100091030               // Tedesco
055200091030               when getTempiI.lingua = 'de';
055300091030                    select;
055400091103                      when GgResaCliAlfa = 'N';
055500091030                         getTempiO.listaTipiServizioTempi
055600091030                                  (getTempiO.numeroTipiServizio) =
055700091104                            'Nicht vorrätiger Dienst';
055800091030
055900091030                         getTempiO.listaTipiServizioDisponibili
056000091030                                  (getTempiO.numeroTipiServizio) = '0';
056100091030
056200091030
056300091030                      when GgResaCliAlfa = '1' and OreResaCliAlfa = *blanks;
056400091030                         getTempiO.listaTipiServizioTempi
056500091030                                  (getTempiO.numeroTipiServizio) =
056600091104                                  'Der nächste Arbeits-Tag';
056700091030
056800091030
056900091030                      when GgResaCliAlfa = '1' and OreResaCliAlfa <> *blanks;
057000091030                         getTempiO.listaTipiServizioTempi
057100091030                                  (getTempiO.numeroTipiServizio) =
057200091104                            'Innerhalb ' + OreResaCliAlfa +
057300091104                            ' Uhr des nächstes Arbeits-Tag';
057400091030
057500091030
057600091030                      when GgResaCliAlfa > '1' and OreResaCliAlfa = *blanks;
057700091030                         getTempiO.listaTipiServizioTempi
057800091030                                  (getTempiO.numeroTipiServizio) =
057900091104                                  GgResaCliAlfa + ' Arbeits-Tage';
058000091030
058100091030
058200091030                      when GgResaCliAlfa > '1' and OreResaCliAlfa <> *blanks;
058300091030                         getTempiO.listaTipiServizioTempi
058400091030                                  (getTempiO.numeroTipiServizio) =
058500091104                            'Innerhalb '         + OreResaCliAlfa +
058600091104                            ' Uhr des nächstes ' + GgResaCliAlfa  +
058700091104                            '° Arbeits-Tag';
058800091030
058900091030                    endsl;
059000091030
059100091030               other;
059200091030
059300091030
059400091030               // Italiano
059500091030                    select;
059600091103                      when GgResaCliAlfa = 'N';
059700091030                         getTempiO.listaTipiServizioTempi
059800091030                                  (getTempiO.numeroTipiServizio) =
059900091030                            'Servizio non disponibile';
060000091030
060100091030                         getTempiO.listaTipiServizioDisponibili
060200091030                                  (getTempiO.numeroTipiServizio) = '0';
060300091030
060400091030
060500091030                      when GgResaCliAlfa = '1' and OreResaCliAlfa = *blanks;
060600091030                         getTempiO.listaTipiServizioTempi
060700091030                                  (getTempiO.numeroTipiServizio) =
060800091030                                  'Il giorno lavorativo successivo';
060900091030
061000091030
061100091030                      when GgResaCliAlfa = '1' and OreResaCliAlfa <> *blanks;
061200091030                         getTempiO.listaTipiServizioTempi
061300091030                                  (getTempiO.numeroTipiServizio) =
061400091030                            'Entro le ore ' + OreResaCliAlfa +
061500091030                            ' del giorno lavorativo successivo';
061600091030
061700091030
061800091030                      when GgResaCliAlfa > '1' and OreResaCliAlfa = *blanks;
061900091030                         getTempiO.listaTipiServizioTempi
062000091030                                  (getTempiO.numeroTipiServizio) =
062100091030                                  GgResaCliAlfa + ' giorni lavorativi';
062200091030
062300091030
062400091030                      when GgResaCliAlfa > '1' and OreResaCliAlfa <> *blanks;
062500091030                         getTempiO.listaTipiServizioTempi
062600091030                                  (getTempiO.numeroTipiServizio) =
062700091030                            'Entro le ore ' + OreResaCliAlfa +
062800091030                            ' del ' + GgResaCliAlfa          +
062900091030                            '° giorno lavorativo successivo';
063000091030
063100091030                    endsl;
063200091030               endsl;
063300091030
063400091030           endif;
063500091030
063600091030       ENDSR;
063700091030
063800091030
063900091030
064000091030       //***********************************************************************
064100091030       //
064200091030       // Caricamento tabelle tipi servizio (in lungua)
064300091030       //
064400091030       //***********************************************************************
064500091030
064600091030       BEGSR RTVTIPISERVIZI;
064700091030
064800091030         // Caricamento tipi servizi internet
064900091030         exec sql declare tabel no scroll cursor for
065000091030              select * from tabel00f
065100091030              where tblkut = :LinguaBartTab    and
065200091030                    tblcod = '5E'              and
065300091030                    substr(tbluni, 83, 1) = 'S'
065400091030              order by substr(tbluni, 81, 2);
065500091030
065600091030         exec sql open tabel;
065700091030
065800091030         exec sql fetch next from tabel into :tabel00f;
065900091030
066000091030         dow sqlcod >= *zeros;
066100091030             if sqlcod = 100;
066200091030                leave;
066300091030             else;
066400091030                jTabel             = jTabel + 1;
066500091030                skTabelKey(jTabel) = tabel00f.tblkey;
066600091030                skTabelUni(jTabel) = tabel00f.tbluni;
066700091030             endif;
066800091030             exec sql fetch next from tabel into :tabel00f;
066900091030         enddo;
067000100107
067100100107         exec sql close tabel;
067200091030
067300091030       ENDSR;
067400091026
067500091030
067600091030
067700091026       //***********************************************************************
067800091026       //
067900091026       // Inizializzazioni.
068000091026       //
068100091026       //***********************************************************************
068200091026
068300091026       BEGSR *INZSR;
068400091026
068500091026         Organigramma_init();
068600091026
068700091026       ENDSR;
068800091026
068900091026      /END-FREE

000100140414       //==============================================================
000200140414       //
000300170530       //?FIOR000R - ORM IMMISSIONE: Video
000400140613       //
000500140613       // nel paradigma MVC è il pgm che gestisce la View
000600140414       //
000700140414       //==============================================================
000800140414     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000900141020     h dftactgrp(*no) actgrp(*caller) bnddir('UBBNDDIR':'TRUL')
001000140414
001100140414       //--------------------------------------------------------------
001200140414       //?Dichiarazione file.                                          ?
001300140414       //--------------------------------------------------------------
001400140414       // -?File video
001500140414     fFIOR000D  cf   e             workstn
001600140618     f                                     indds(IndDspF)
001700140618     f                                     infds(InfDspF)
001800140414
001900140414       //--------------------------------------------------------------
002000140414       //?Definizione costanti.                                        ?
002100140414       //--------------------------------------------------------------
002200140613       // -?Costanti dei programmi
002300140613      /copy gaitrasrc/srcconst,FIOR000R
002400141013      /copy gaitrasrc/srcconst,FIORA00R
002500140414
002600140414       //---------------------------------------------------------------
002700140414       //?Definizione aree dati.
002800140414       //---------------------------------------------------------------
002900140414
003000140414       //--------------------------------------------------------------
003100140414       //?Definizione strutture dati.                                  ?
003200140414       //--------------------------------------------------------------
003300140414       // -?Dati INPUT
003400141008     d FIOR000I      e ds                  QUALIFIED INZ(*EXTDFT)
003500140414
003600140414       // -?Dati OUTPUT
003700141008     d FIOR000O      e ds                  QUALIFIED INZ(*EXTDFT)
003800140617
003900140617       // -?Messaggi
004000140617     d FIORA0M0      e ds                  QUALIFIED INZ(*EXTDFT)
004100140617     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
004200140617     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
004300140617     d  skErrWarn                     1a   dim(99) overlay(ErrWarn)
004400140617     d  skTextMsg                   255a   dim(99) overlay(TextMsg)
004500140617
004600140617       // -?Forzature
004700140617     d FIORA0F0      e ds                  QUALIFIED INZ(*EXTDFT)
004800140617     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
004900140617     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
005000140617     d  skGiaForza                    1a   dim(99) overlay(GiaForzato)
005100140617
005200140618       // -?Attributi campo e Tasti Funzione
005300140618     d FIOR000A      e ds                  QUALIFIED INZ(*EXTDFT)
005400170330     d  skTastiFun                    1a   dim(26) overlay(TastiFun)
005500140617     d  skIdAttr                      5a   dim(99) overlay(IdAttr)
005600140617     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
005700140617
005800140617      // -?Kpjba
005900140617     d KPJBA         e ds                  INZ
006000140617
006100140617      // -?Ricerca GIRO Ritiro
006200140617     d FIDG09DS      e ds                  QUALIFIED INZ
006300160523
006400160523       // -?Interrogazione Cappario
006500160523     d FNLV14DS      e ds                  QUALIFIED INZ
006600140415
006700140513      // -?Reperimento Tabelle TNTBE
006800140617     d TIBS02DS      e ds                  QUALIFIED INZ
006900160621
007000160621       // -?Calcola orari di servizio
007100160621     d TRULORSDS     e ds                  QUALIFIED INZ
007200160621     d TRULOR2ds     e ds                  QUALIFIED INZ
007300160621
007400160621       // -?Data
007500160621     d                 ds
007600160621     d  wData                  1      8s 0 inz
007700160621     d  wAnno                  1      4s 0 inz
007800160621     d  wMese                  5      6s 0 inz
007900160621     d  wGiorno                7      8s 0 inz
008000140414
008100140414       // -?Status
008200140414     d Psds           sds
008300140414     d  SDSpgm           *proc
008400140414
008500140414       // -?InfDS
008600140414     d InfDspF         ds
008700140414     d  dsp_aid              369    369a
008800140414
008900140414       // -?Indicatori su DspF
009000140414     d IndDspF         ds
009100140414       // -?Indicatori di abilitazione tasti
009200170330     d  AbilitaF                     26a   overlay(IndDspF : 01)
009300140513       // -?Indicatori di emissione errore in videata
009400170330     d  ErrMessage                    1n   overlay(IndDspF : 27)
009500170330     d  ErrMessage02                  1n   overlay(IndDspF : 28)
009600170330     d  ErrMessage03                  1n   overlay(IndDspF : 29)
009700170330     d  ErrMessage04                  1n   overlay(IndDspF : 30)
009800140620       // -?Indicatori di posizionamento cursore
009900140620     d  PosCurTOR                     1n   overlay(IndDspF : 50)
010000141014     d  PosCurCGI                     1n   overlay(IndDspF : 50)
010100160620     d  PosCurSMS                     1n   overlay(IndDspF : 50)
010200160620     d  PosCurSMSC                    1n   overlay(IndDspF : 50)
010300140620     d  PosCurCRA                     1n   overlay(IndDspF : 51)
010400160613     d  PosCurBLC                     1n   overlay(IndDspF : 51)
010500160620     d  PosCurMAIL                    1n   overlay(IndDspF : 51)
010600160620     d  PosCurMAILC                   1n   overlay(IndDspF : 51)
010700140620     d  PosCurRSR                     1n   overlay(IndDspF : 52)
010800160613     d  PosCurMTC                     1n   overlay(IndDspF : 52)
010900140620     d  PosCurINR                     1n   overlay(IndDspF : 53)
011000160613     d  PosCurATT                     1n   overlay(IndDspF : 53)
011100140620     d  PosCurLOR                     1n   overlay(IndDspF : 54)
011200141008     d  PosCurNCL                     1n   overlay(IndDspF : 55)
011300141008     d  PosCurPKG                     1n   overlay(IndDspF : 56)
011400141008     d  PosCurBNC                     1n   overlay(IndDspF : 57)
011500141008     d  PosCurVLM                     1n   overlay(IndDspF : 58)
011600141009     d  PosCurCAR                     1n   overlay(IndDspF : 59)
011700141009     d  PosCurORR                     1n   overlay(IndDspF : 60)
011800141009     d  PosCurNO1                     1n   overlay(IndDspF : 61)
011900141010     d  PosCurRER                     1n   overlay(IndDspF : 62)
012000141013     d  PosCurPOR                     1n   overlay(IndDspF : 63)
012100141014     d  PosCurTER                     1n   overlay(IndDspF : 64)
012200141016     d  PosCurCRC                     1n   overlay(IndDspF : 65)
012300141016     d  PosCurCOR                     1n   overlay(IndDspF : 66)
012400141016     d  PosCurPAG                     1n   overlay(IndDspF : 67)
012500141020     d  PosCurPOC                     1n   overlay(IndDspF : 68)
012600141021     d  PosCurKSC                     1n   overlay(IndDspF : 69)
012700141021     d  PosCurCTR                     1n   overlay(IndDspF : 70)
012800160404     d  PosCurTCO                     1n   overlay(IndDspF : 71)
012900160530     d  PosCurDALLEAM                 1n   overlay(IndDspF : 72)
013000160530     d  PosCurALLEAM                  1n   overlay(IndDspF : 73)
013100160530     d  PosCurDALLEPM                 1n   overlay(IndDspF : 74)
013200160530     d  PosCurALLEPM                  1n   overlay(IndDspF : 75)
013300170403     d  PosCurDPM                     1n   overlay(IndDspF : 76)
013400160608     d  PosCurRSC                     1n   overlay(IndDspF : 77)
013500160608     d  PosCurNAR                     1n   overlay(IndDspF : 78)
013600160608     d  PosCurPRR                     1n   overlay(IndDspF : 79)
013700160608     d  PosCurRSO                     1n   overlay(IndDspF : 80)
013800160608     d  PosCurINC                     1n   overlay(IndDspF : 81)
013900160608     d  PosCurINO                     1n   overlay(IndDspF : 82)
014000160608     d  PosCurLOC                     1n   overlay(IndDspF : 83)
014100160608     d  PosCurPRC                     1n   overlay(IndDspF : 84)
014200160608     d  PosCurCAC                     1n   overlay(IndDspF : 85)
014300160608     d  PosCurNAC                     1n   overlay(IndDspF : 86)
014400160608     d  PosCurLOO                     1n   overlay(IndDspF : 87)
014500160608     d  PosCurPRO                     1n   overlay(IndDspF : 88)
014600160608     d  PosCurCAO                     1n   overlay(IndDspF : 89)
014700160608     d  PosCurNAO                     1n   overlay(IndDspF : 90)
014800170522     d  PosCurCOMM                    1n   overlay(IndDspF : 91)
014900140414       // -?Indicatori di errore generico
015000140414     d  ErrGenerico                   1n   overlay(IndDspF : 99)
015100140414
015200140414     d WindDspF        ds                  inz
015300140513     d  WindDspFa              1     49    inz(*zero)
015400140513     d  WindDspFb             50     99    inz(*zero)
015500140414
015600140414       //--------------------------------------------------------------
015700140414       //?Definizione schiere.
015800140414       //--------------------------------------------------------------
015900140414
016000140414       //--------------------------------------------------------------
016100140414       //?Definizione campi.
016200140414       //--------------------------------------------------------------
016300140414       // -?Flags boolenai
016400140414     d wFine           s               n   inz(*off)
016500140414
016600140617       // -?Contatori
016700140619     d NrAttributo     s              2s 0
016800140618     d NrErrore        s              2s 0
016900141013     d NrForza         s              2s 0
017000140618     d NrTasto         s              2s 0
017100140617
017200140617       // -?Campi di comodo
017300160523     d dataoggi        s              8s 0
017400160607     d savloc          s                   inz like(fior000I.loc)
017500160607     d savloo          s                   inz like(fior000I.loo)
017600160615     d UL36volume      s              8  3 inz
017700160523     d wCap            s                   inz like(fior000I.car)
017800160523     d wIndirizzo      s                   inz like(fior000I.inr)
017900160523     d wLocalita       s                   inz like(fior000I.lor)
018000160610     d wMsg            s                   inz like(V01msg)
018100160523     d wNazione        s                   inz like(fior000I.nar)
018200160523     d wProvincia      s                   inz like(fior000I.prr)
018300160523     d wRagSoc         s                   inz like(fior000I.rsr)
018400140414     d wVideo          s              2    inz('D1')
018500140513
018600140513       // -?Parametri per ricerca Filiale
018700140513     d pinFIL          s              3
018800140513     d pinFAG          s              1
018900140513     d pinDES          s             25
019000140513     d pinDIT          s              3
019100140414
019200140414       //--------------------------------------------------------------
019300140414       //?Definizione procedure.
019400140414       //--------------------------------------------------------------
019500160523       // -?Pgm interrogazione giri
019600140617     d fidg09r         pr                  extpgm('FIDG09R')
019700140617     d  kpjba                              likeds(kpjba)
019800160523
019900160523       // -?Pgm interrogazione cappario
020000160523     d FNLV14R         pr                  extpgm('FNLV14R')
020100160523     d  kpjba                              likeds(kpjba)
020200160523     d  fnlv14ds                           likeds(FNLV14DS)
020300160615
020400160615       // -?Calcolo volume
020500160615     d TRUL36R         pr                  extpgm('TRUL36R')
020600160615     d  UL36volume                    8  3
020700160621
020800160621       // -?Calcolo orari servizio
020900160621     d trulorsr        pr                  extpgm('TRULORSR')
021000160621     d  kpjba                       502a
021100160621     d  trulorsds                          likeds(TRULORSDS)
021200160621     d  trulor2ds                          likeds(TRULOR2DS)
021300160621     d                                     options (*nopass)
021400140414
021500140414       //--------------------------------------------------------------
021600140414       //?Definizione prototipi.
021700140414       //--------------------------------------------------------------
021800140618      /copy gaitrasrc/srcprotopr,FIOR000R
021900140513      /copy gaitrasrc/srcprotopr,TIBS02R
022000140617      /copy gaitrasrc/srcprotopr,TNSD24R
022100140414
022200140414       //--------------------------------------------------------------
022300140414       //?Definizione parametri programma.
022400140414       //--------------------------------------------------------------
022500140617     d Fior000_Video...
022600140414     d                 PI
022700140414     d prmRqsOpCode...
022800140414     d                               10I 0 CONST
022900140414     d prmRpyOpCode...
023000140414     d                               10I 0
023100140414     d prmRpyIdMsg...
023200140414     d                               10I 0
023300140414     d prmRqsFormato...
023400140414     d                               10A   CONST
023500140414     d prmRqsData...
023600140414     d                            32767A   OPTIONS(*VARSIZE)
023700140617     d prmRqsSize...
023800140414     d                               10I 0 CONST
023900140414     d prmRpyFormato...
024000140414     d                               10A   CONST
024100140414     d prmRpyData...
024200140414     d                            32767A   OPTIONS(*VARSIZE)
024300140617     d prmRpySize...
024400140414     d                               10I 0 CONST
024500140617     d prmRpyFormMsg...
024600140617     d                               10A   CONST OPTIONS(*NOPASS)
024700140617     d prmRpyMessage...
024800140617     d                            32767A   OPTIONS(*VARSIZE : *NOPASS)
024900140617     d prmRpySizeMsg...
025000140617     d                               10I 0 CONST OPTIONS(*NOPASS)
025100140617     d prmRpyFormForz...
025200140617     d                               10A   CONST OPTIONS(*NOPASS)
025300140617     d prmRpyForzatu...
025400140617     d                            32767A   OPTIONS(*VARSIZE : *NOPASS)
025500140617     d prmRpySizeForz...
025600140617     d                               10I 0 CONST OPTIONS(*NOPASS)
025700140617     d prmRpyFormTastiFun...
025800140617     d                               10A   CONST OPTIONS(*NOPASS)
025900140617     d prmRpyTastiFun...
026000140617     d                            32767A   OPTIONS(*VARSIZE : *NOPASS)
026100140617     d prmRpySizeTastiFun...
026200140617     d                               10I 0 CONST OPTIONS(*NOPASS)
026300140414     d prmKPJBA...
026400140414     d                              502A   CONST OPTIONS(*NOPASS)
026500140414
026600140414       //--------------------------------------------------------------
026700140414       //?MAIN.
026800140414       //--------------------------------------------------------------
026900140414
027000140414      /free
027100140414
027200140414       //?Operazioni iniziali
027300140414       exsr RoutInz;
027400140414
027500140414       //?Controllo formale dei dati passati
027600140414       exsr ControllaParm;
027700140617
027800140617       //?Imposto i dati dalla DS a Video
027900140617       exsr MoveDSVideo;
028000140617
028100140617       //?Imposto Tasti funzione
028200140617       exsr Tastifunzione;
028300140618
028400140618       //?Imposto Attributi del video
028500140619       exsr Attributi;
028600140618
028700140618       //?Imposto errori
028800140618       IF  fiora0m0.NrMsg > 0;
028900140618         exsr EmetteErrore;
029000140618       ENDIF;
029100140618
029200140618       //?Emetto videata
029300140618       exsr EmetteVideo;
029400160523
029500160523       //?Resetto gli errori e gli indicatori
029600160523       reset fiora0M0;
029700160523       ErrMessage  = *off;
029800160523       ErrGenerico = *off;
029900160523       WindDspF    = IndDspF;
030000160523       reset WindDspFb;
030100160523       IndDspF     = WindDspF;
030200160607
030300160607       IF  savloc = *blanks;
030400160608         savloc = V01loc;
030500160607       ENDIF;
030600160607       IF  savloo = *blanks;
030700160608         savloo = V01loo;
030800160607       ENDIF;
030900160608       IF  V01loc = *blanks;
031000160608         clear savloc;
031100160608       ENDIF;
031200160608       IF  V01loo = *blanks;
031300160608         clear savloo;
031400160608       ENDIF;
031500140618
031600140618       //?Controllo se è stata richiesta una ricerca con '?'
031700140618       exsr RicercaDati;
031800160523
031900160523       //?Interrogazione cappario
032000160523       exsr IntCappario;
032100160615
032200160615       //?Se tasto F11 devo farlo qua
032300160615       //?perchè il pgm richiamato emette una WIN e se lo faccio
032400160615       //?nel pgm principale perdo la videata dell'orm
032500160615       IF  dsp_aid = FIOR000_TASTO_F11;
032600160621         exsr TastoF11;
032700160615       ENDIF;
032800160621
032900160621       //?Se tasto F14 devo farlo qua
033000160621       //?perchè il pgm richiamato emette una WIN e se lo faccio
033100160621       //?nel pgm principale perdo la videata dell'orm
033200160621       IF  dsp_aid = FIOR000_TASTO_F14;
033300160621         exsr TastoF14;
033400160621       ENDIF;
033500140617
033600140617       //?Tutti i controlli + gestione dei tasti funzione sono demandati
033700140617       //?al pgm controller
033800140617       //?Sposto i dati dal video alla DS
033900140617       exsr MoveVideoDS;
034000140414
034100140414       //?Operazioni finali
034200140414       exsr RoutEnd;
034300140414
034400140414       //--------------------------------------------------------------
034500140414       //?Operazioni iniziali.                                         ?
034600140414       //--------------------------------------------------------------
034700140414       BEGSR  RoutInz;
034800140414
034900140414         prmRpyOpCode = FIOR000_RPYOPCODE_DONE;
035000140414         prmRpyIdMsg  = FIOR000_ESITO_OK;
035100140414
035200140414       //?Pulisco ds di output
035300140618         reset fior000o;
035400160523
035500160523       //?Data del giorno
035600160523         dataoggi = %dec(%date());
035700140414
035800140414         *inLR = *on;
035900140414
036000140414       ENDSR;
036100140414
036200140414       //--------------------------------------------------------------
036300140414       //?Controllo formale dei dati passati.
036400140414       //--------------------------------------------------------------
036500140414       BEGSR  ControllaParm;
036600140414
036700140414       //?OpCode
036800160321         IF  prmRqsOpCode <> FIOR000_RQSOPCODE_IMMISSIONE_TEL and
036900160321             prmRqsOpCode <> FIOR000_RQSOPCODE_IMMISSIONE_MAIL and
037000141010             prmRqsOpCode <> FIOR000_RQSOPCODE_CONFERMA_INTERNET and
037100141010             prmRqsOpCode <> FIOR000_RQSOPCODE_MANUTENZIONE;
037200140414           prmRpyOpCode = FIOR000_RPYOPCODE_ERROR;
037300140414           prmRpyIdMsg  = FIOR000_ESITO_RQSOPCODE_SCONOSCIUTO;
037400140414           exsr RoutEnd;
037500140414         ENDIF;
037600140414
037700140414       //?Formato e size RQS
037800140617         IF  prmRqsSize > 0;
037900140617         ELSE;
038000140617           prmRpyOpCode = FIOR000_RPYOPCODE_ERROR;
038100140617           prmRpyIdMsg = FIOR000_ESITO_SIZE_DATA_ERRATO;
038200140617           exsr RoutEnd;
038300140617         ENDIF;
038400140618         IF  prmRqsFormato = fior000i.formato;
038500140618           fior000i = %SUBST(prmRqsData:1:prmRqsSize);
038600140414         ELSE;
038700140414           prmRpyOpCode = FIOR000_RPYOPCODE_ERROR;
038800140414           prmRpyIdMsg = FIOR000_ESITO_NOME_FORMATO_SCONOSCIUTO;
038900140414           exsr RoutEnd;
039000140414         ENDIF;
039100140414
039200140414       //?Formato e size RPY
039300140617         IF  prmRpySize > 0;
039400140617         ELSE;
039500140617           prmRpyOpCode = FIOR000_RPYOPCODE_ERROR;
039600140617           prmRpyIdMsg = FIOR000_ESITO_SIZE_DATA_ERRATO;
039700140617           exsr RoutEnd;
039800140617         ENDIF;
039900140618         IF  prmRpyFormato = fior000o.formato;
040000140414         ELSE;
040100140414           prmRpyOpCode = FIOR000_RPYOPCODE_ERROR;
040200140414           prmRpyIdMsg = FIOR000_ESITO_NOME_FORMATO_SCONOSCIUTO;
040300140414           exsr RoutEnd;
040400140414         ENDIF;
040500140617
040600140617       //?Formato e size MSG
040700140617         IF  prmRpySizeMsg > 0;
040800140617         ELSE;
040900140617           prmRpyOpCode = FIOR000_RPYOPCODE_ERROR;
041000140617           prmRpyIdMsg = FIOR000_ESITO_SIZE_DATA_ERRATO;
041100140617           exsr RoutEnd;
041200140617         ENDIF;
041300140618         IF  prmRpyFormMsg = fiora0m0.formato;
041400140620           fiora0m0 = %SUBST(prmRpyMessage:1:prmRpySizeMsg);
041500140617         ELSE;
041600140617           prmRpyOpCode = FIOR000_RPYOPCODE_ERROR;
041700140617           prmRpyIdMsg = FIOR000_ESITO_NOME_FORMATO_SCONOSCIUTO;
041800140617           exsr RoutEnd;
041900140617         ENDIF;
042000140617
042100140617       //?Formato e size Forzature
042200140617         IF  prmRpySizeForz > 0;
042300140617         ELSE;
042400140617           prmRpyOpCode = FIOR000_RPYOPCODE_ERROR;
042500140617           prmRpyIdMsg = FIOR000_ESITO_SIZE_DATA_ERRATO;
042600140617           exsr RoutEnd;
042700140617         ENDIF;
042800140617         IF  prmRpyFormForz = fiora0f0.formato;
042900141013           fiora0f0 = %SUBST(prmRpyForzatu:1:prmRpySizeForz);
043000140617         ELSE;
043100140617           prmRpyOpCode = FIOR000_RPYOPCODE_ERROR;
043200140617           prmRpyIdMsg = FIOR000_ESITO_NOME_FORMATO_SCONOSCIUTO;
043300140617           exsr RoutEnd;
043400140617         ENDIF;
043500140617
043600140617       //?Formato e size Tasti Funzione
043700140617         IF  prmRpySizeTastiFun > 0;
043800140617         ELSE;
043900140617           prmRpyOpCode = FIOR000_RPYOPCODE_ERROR;
044000140617           prmRpyIdMsg = FIOR000_ESITO_SIZE_DATA_ERRATO;
044100140617           exsr RoutEnd;
044200140617         ENDIF;
044300140618         IF  prmRpyFormTastiFun = fior000a.formato;
044400140618           fior000a = %SUBST(prmRpyTastiFun:1:prmRpySizeTastiFun);
044500140617         ELSE;
044600140617           prmRpyOpCode = FIOR000_RPYOPCODE_ERROR;
044700140617           prmRpyIdMsg = FIOR000_ESITO_NOME_FORMATO_SCONOSCIUTO;
044800140617           exsr RoutEnd;
044900140617         ENDIF;
045000140414
045100140414       ENDSR;
045200140617
045300140617       //--------------------------------------------------------------
045400140617       //?Muovo i dati da DS input a Video.
045500140617       //--------------------------------------------------------------
045600141008       BEGSR  MoveDSVideo;
045700160322
045800160322         V01pgm     = SDSpgm;
045900140617
046000140618         V01mod     = fior000i.mod;
046100140618         V01rsut    = fior000i.rsut;
046200140618         V01sisinf  = fior000i.sisinf;
046300140618         V01utente  = fior000i.utente;
046400140618         V01tit     = fior000i.titolo;
046500140617
046600160321         V01poei    = fior000i.poei;
046700160321         V01dpoei   = fior000i.dpoei;
046800140617
046900140618         V01poeu    = fior000i.poeu;
047000140618         V01nsru    = fior000i.nsru;
047100140618         V01noru    = fior000i.noru;
047200140618         V01nrvu    = fior000i.nrvu;
047300140617
047400140618         V01fao     = fior000i.fao;
047500140618         V01dfao    = fior000i.dfao;
047600140618         V01poe     = fior000i.poe;
047700140618         V01nsr     = fior000i.nsr;
047800140618         V01nor     = fior000i.nor;
047900140618         V01nrv     = fior000i.nrv;
048000140618         V01dao     = fior000i.dao;
048100140618         V01oao     = fior000i.oao;
048200140617
048300140618         V01tor     = fior000i.tor;
048400140618         V01dtor    = fior000i.dtor;
048500140618         V01cau     = fior000i.cau;
048600140618         V01dcau    = fior000i.dcau;
048700140618         V01tco     = fior000i.tco;
048800140618         V01dtco    = fior000i.dtco;
048900140618         V01pos     = fior000i.pos;
049000140618         V01ors     = fior000i.ors;
049100140618         V01ostis   = fior000i.ostis;
049200140618         V01ostfs   = fior000i.ostfs;
049300140618         V01dfcs    = fior000i.dfcs;
049400140618         V01este    = fior000i.este;
049500140617
049600140618         V01cra1    = fior000i.cra1;
049700140618         V01cra2    = fior000i.cra2;
049800140618         V01cra3    = fior000i.cra3;
049900140618         V01rsr     = fior000i.rsr;
050000140618         V01inr     = fior000i.inr;
050100140618         V01lor     = fior000i.lor;
050200140618         V01prr     = fior000i.prr;
050300140618         V01car     = fior000i.car;
050400140618         V01nar     = fior000i.nar;
050500140618         V01rer     = fior000i.rer;
050600140618         V01ter     = fior000i.ter;
050700140617
050800140618         V01dalleam = fior000i.dalleam;
050900140618         V01alleam  = fior000i.alleam;
051000140618         V01dallepm = fior000i.dallepm;
051100140618         V01allepm  = fior000i.allepm;
051200140618         V01nam     = fior000i.nam;
051300140618         V01rfa     = fior000i.rfa;
051400140618         V01nota1   = fior000i.nota1;
051500140618         V01nota2   = fior000i.nota2;
051600140617
051700140618         V01orr     = fior000i.orr;
051800170403         V01dpm     = fior000i.dpm;
051900140618         V01dar     = fior000i.dar;
052000140618         V1anticipa = fior000i.anticipa;
052100140617
052200140618         V01ntwd    = fior000i.ntwdpd;
052300141020         V01por     = fior000i.por;
052400140618         V01dpor    = fior000i.dpor;
052500140617
052600140618         V01ncl     = fior000i.ncl;
052700140618         V01pkg     = fior000i.pkg;
052800140618         V01bnc     = fior000i.bnc;
052900140618         V01vlm     = fior000i.vlm;
053000140618         V01spi     = fior000i.spi;
053100140617
053200140618         V01pag     = fior000i.pag;
053300140618         V01ksc     = fior000i.ksc;
053400140618         V01ctr     = fior000i.ctr;
053500140617
053600140618         V01comm    = fior000i.comm;
053700140617
053800140618         V01crc1    = fior000i.crc1;
053900140618         V01crc2    = fior000i.crc2;
054000140618         V01crc3    = fior000i.crc3;
054100140618         V01rsc     = fior000i.rsc;
054200140618         V01inc     = fior000i.inc;
054300140618         V01loc     = fior000i.loc;
054400160607         savloc     = fior000i.loc;
054500140618         V01prc     = fior000i.prc;
054600140618         V01cac     = fior000i.cac;
054700140618         V01nac     = fior000i.nac;
054800141020         V01poc     = fior000i.poc;
054900140618         V01dpoc    = fior000i.dpoc;
055000140617
055100140618         V01cor1    = fior000i.cor1;
055200140618         V01cor2    = fior000i.cor2;
055300140618         V01cor3    = fior000i.cor3;
055400140618         V01rso     = fior000i.rso;
055500140618         V01ino     = fior000i.ino;
055600140618         V01loo     = fior000i.loo;
055700160607         savloo     = fior000i.loo;
055800140618         V01pro     = fior000i.pro;
055900140618         V01cao     = fior000i.cao;
056000140618         V01nao     = fior000i.nao;
056100140617
056200140618         V02blc     = fior000i.blc;
056300140618         V02att     = fior000i.att;
056400140618         V02mtc     = fior000i.mtc;
056500140618         V02sto     = fior000i.sto;
056600140618         V02dsto    = fior000i.dsto;
056700170419         V02tgi     = fior000i.tgi;
056800140618         V02cgi     = fior000i.cgi;
056900140618         V02dcgi    = fior000i.dcgi;
057000141008         IF  fior000i.ffd = 'N';
057100141008           clear V02ffd;
057200141008         ELSE;
057300141008           V02ffd = fior000i.ffd;
057400141008         ENDIF;
057500140617
057600141008         V03mail = fior000i.mail;
057700141008         V03sms  = fior000i.sms;
057800141008
057900141008         V01ostis = fior000i.orainizio;
058000141008         V01ostfs = fior000i.orafine;
058100160531
058200160613         V04mailcon = fior000i.mailconf;
058300160613         V04smsconf = fior000i.smsconf;
058400170404
058500170404         V01ddt = fior000i.ddt;
058600140414
058700140414       ENDSR;
058800140415
058900140415       //--------------------------------------------------------------
059000140415       //?Imposto i tasti funzione.
059100140415       //--------------------------------------------------------------
059200141008       BEGSR  TastiFunzione;
059300140617
059400140617       //?Abilito i tasti funzione
059500140617         NrTasto = 1;
059600140618         FOR  NrTasto by 1 to %elem(fior000a.skTastiFun);
059700140618           %subst(AbilitaF:NrTasto:1) = fior000a.skTastiFun(NrTasto);
059800140617         ENDFOR;
059900140415
060000140617       //?Imposto l'ultima riga dei tasti
060100140618         V01tf1 = fior000a.rigatfmob;
060200140618         V01tf2 = fior000a.rigatf24;
060300140415
060400140415       ENDSR;
060500140618
060600140618       //--------------------------------------------------------------
060700140618       //?Imposto attributi del video.
060800140618       //--------------------------------------------------------------
060900141008       BEGSR  Attributi;
061000140620
061100140620         WindDspF    = IndDspF;
061200140620         reset WindDspFb;
061300140620         IndDspF     = WindDspF;
061400140618
061500140619         NrAttributo = 1;
061600140619         FOR  NrAttributo by 1 to fior000a.NrAttr;
061700140619           SELECT;
061800160321           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRTCO';
061900160321             ATTRtco = fior000a.skIdAttr(NrAttributo);
062000160321           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRPOEI';
062100160321             ATTRpoei = fior000a.skIdAttr(NrAttributo);
062200160321           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRDPOEI';
062300160321             ATTRdpoei = fior000a.skIdAttr(NrAttributo);
062400170403           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRDAR';
062500170403             ATTRdar = fior000a.skIdAttr(NrAttributo);
062600140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRORSR';
062700140619             ATTRorsr = fior000a.skIdAttr(NrAttributo);
062800140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRNORU';
062900140619             ATTRnoru = fior000a.skIdAttr(NrAttributo);
063000140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRFASE';
063100140619             ATTRfase = fior000a.skIdAttr(NrAttributo);
063200140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRCRA1';
063300140619             ATTRcra1 = fior000a.skIdAttr(NrAttributo);
063400140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRCRA2';
063500140619             ATTRcra2 = fior000a.skIdAttr(NrAttributo);
063600140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRCOR1';
063700140619             ATTRcor1 = fior000a.skIdAttr(NrAttributo);
063800140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRCRC1';
063900140619             ATTRcrc1 = fior000a.skIdAttr(NrAttributo);
064000140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRNOTA';
064100140619             ATTRnota = fior000a.skIdAttr(NrAttributo);
064200140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRTOR';
064300140619             ATTRtor  = fior000a.skIdAttr(NrAttributo);
064400140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRCRA';
064500140619             ATTRcra  = fior000a.skIdAttr(NrAttributo);
064600140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRNOR';
064700140619             ATTRnor  = fior000a.skIdAttr(NrAttributo);
064800140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRCAU';
064900140619             ATTRcau  = fior000a.skIdAttr(NrAttributo);
065000140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRQTA';
065100140619             ATTRqta  = fior000a.skIdAttr(NrAttributo);
065200140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRCRC';
065300140619             ATTRcrc  = fior000a.skIdAttr(NrAttributo);
065400140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRCOR';
065500140619             ATTRcor  = fior000a.skIdAttr(NrAttributo);
065600140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRPOR';
065700140619             ATTRpor  = fior000a.skIdAttr(NrAttributo);
065800140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRPOC';
065900140619             ATTRpoc  = fior000a.skIdAttr(NrAttributo);
066000140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRNAM';
066100140619             ATTRnam  = fior000a.skIdAttr(NrAttributo);
066200140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRRFA';
066300140619             ATTRrfa  = fior000a.skIdAttr(NrAttributo);
066400140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRPAG';
066500140619             ATTRpag  = fior000a.skIdAttr(NrAttributo);
066600141013           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRCOMM';
066700141013             ATTRcomm  = fior000a.skIdAttr(NrAttributo);
066800141010           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRF04';
066900141010             ATTRf04  = fior000a.skIdAttr(NrAttributo);
067000141010           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRF05';
067100141010             ATTRf05  = fior000a.skIdAttr(NrAttributo);
067200141010           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRF17';
067300141010             ATTRf17  = fior000a.skIdAttr(NrAttributo);
067400141010           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRF18';
067500141010             ATTRf18  = fior000a.skIdAttr(NrAttributo);
067600141014           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRBLC';
067700141014             ATTRblc  = fior000a.skIdAttr(NrAttributo);
067800141014           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRATT';
067900141014             ATTRatt  = fior000a.skIdAttr(NrAttributo);
068000141014           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRMTC';
068100141014             ATTRmtc  = fior000a.skIdAttr(NrAttributo);
068200141014           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRSTO';
068300141014             ATTRsto  = fior000a.skIdAttr(NrAttributo);
068400141014           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRCGI';
068500141014             ATTRcgi  = fior000a.skIdAttr(NrAttributo);
068600141014           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRFFD';
068700141014             ATTRffd  = fior000a.skIdAttr(NrAttributo);
068800160610           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRSMS';
068900160610             ATTRsms  = fior000a.skIdAttr(NrAttributo);
069000160610           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRMAIL';
069100160610             ATTRmail = fior000a.skIdAttr(NrAttributo);
069200160613           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRSMSC';
069300160613             ATTRsmsc = fior000a.skIdAttr(NrAttributo);
069400160613           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRMAILC';
069500160613             ATTRmailc = fior000a.skIdAttr(NrAttributo);
069600140619           ENDSL;
069700140618         ENDFOR;
069800140618
069900140618       ENDSR;
070000140617
070100140618       //--------------------------------------------------------------
070200140618       //?Imposto gli errori a video.
070300140618       //--------------------------------------------------------------
070400141008       BEGSR  EmetteErrore;
070500140618
070600140618       //?Emetto il messaggio di errore ne devo emettere 1 alla volta
070700140618         NrErrore = 1;
070800140618         FOR  NrErrore by 1 to fiora0m0.NrMsg;
070900141013
071000160531         //?Se è un Warning lo scrivo nella sk delle forzature come già forzato
071100141013           IF  fiora0m0.skErrWarn(NrErrore) = FIORA00_MSG_WARNING;
071200141013             clear NrForza;
071300141013             NrForza =
071400160531             %lookup(*blanks:fiora0f0.skIdmsg);
071500160531             IF  NrForza > 0;
071600160531               fiora0f0.skIdMsg(NrForza) = fiora0m0.skIdMsg(NrErrore);
071700160531               fiora0f0.skIdCampo(NrForza) = fiora0m0.skIdCampo(NrErrore);
071800160531               fiora0f0.skGiaForza(NrForza) = FIORA00_GIAFORZA_SI;
071900141013             ENDIF;
072000160531           //?In alcuni casi devo impostare che msg. forzabile
072100160531             exsr ImpostaMsg;
072200141013           ENDIF;
072300141013
072400140618         //?In base al nome campo imposto il posizionamento cursore e
072500140618         //?il campo messaggi
072600140618           exsr ImpostaPC;
072700141010           IF  fiora0m0.skTextMsg(NrErrore) = FIOR000_IMPOSTA_PC;
072800141010             leavesr;
072900141010           ENDIF;
073000160610           IF  wMsg = *blanks;
073100160610             wMsg = fiora0m0.skTextMsg(NrErrore);
073200140620           ENDIF;
073300140618           ErrGenerico = *on;
073400160613           SELECT;
073500160616           WHEN  fior000i.video = FIOR000_VIDEO_PRINCIPALE;
073600160613             ErrMessage = *on;
073700160616           WHEN  fior000i.video = FIOR000_VIDEO_ALTRIDATI;
073800160613             ErrMessage02 = *on;
073900160616           WHEN  fior000i.video = FIOR000_VIDEO_ALERTCOMM;
074000160613             ErrMessage03 = *on;
074100160616           WHEN  fior000i.video = FIOR000_VIDEO_ALERTCONF;
074200160613             ErrMessage04 = *on;
074300160613           ENDSL;
074400160613           leavesr;
074500140618         ENDFOR;
074600140618
074700140618       ENDSR;
074800140618
074900140618       //--------------------------------------------------------------
075000140618       //?Emetto la videata.
075100140618       //--------------------------------------------------------------
075200141008       BEGSR  EmetteVideo;
075300140618
075400140618         SELECT;
075500160616         WHEN  fior000i.video = FIOR000_VIDEO_PRINCIPALE;
075600140717       //?Se non ci sono errori
075700140717           IF  not ErrMessage;
075800141015             SELECT;
075900140717         //?se il mittente non è ancora stato immesso
076000141015             WHEN  V01cra1 = 0 and V01rsr = *blanks;
076100140717           //?mi posiziono sul codice mittente
076200140717               PosCurCRA = *on;
076300141015         //?se i colli/peso/bancali non ci sono mi posiziono sui colli
076400141015             WHEN  V01ncl = 0 and V01pkg = 0 and V01bnc = 0;
076500141015               PosCurNCL = *on;
076600160608         //?se ho il tipo ordinante (videata WHO) impostato ad A=Altro
076700160608         //?mi posiziono sull'ordinante
076800160608             WHEN  fior000i.tipoord = 'A';
076900160608               PosCurCOR = *on;
077000140717         //?altrimenti sulle note
077100141015             OTHER;
077200141015               PosCurNO1 = *on;
077300141015             ENDSL;
077400140717           ENDIF;
077500160610           V01msg = wMsg;
077600140618           exfmt FIOR0001;
077700160616         WHEN  fior000i.video = FIOR000_VIDEO_ALTRIDATI;
077800160610           V02msg = wMsg;
077900160610           write FIOR0001;
078000140618           exfmt FIOR0002;
078100160616         WHEN  fior000i.video = FIOR000_VIDEO_ALERTCOMM;
078200160620           V03msg = wMsg;
078300140618           write FIOR0001;
078400140618           exfmt FIOR0003;
078500160616         WHEN  fior000i.video = FIOR000_VIDEO_ALERTCONF;
078600160620           V04msg = wMsg;
078700160613           write FIOR0001;
078800160613           exfmt FIOR0004;
078900141022         WHEN  fior000i.video = FIOR000_VIDEO_WHOISORD;
079000141022           write FIOR0001;
079100141022           exfmt FIOR000WO;
079200140618         ENDSL;
079300140618
079400140618       ENDSR;
079500160530
079600160530       //--------------------------------------------------------------
079700160530       //?Imposto 'Forzabile' nel messaggio di errore.
079800160530       //--------------------------------------------------------------
079900160530       BEGSR  ImpostaMsg;
080000160530
080100160530         SELECT;
080200160531         WHEN  fiora0m0.skIdMsg(NrErrore) = 'TIS0726';
080300160530           fiora0m0.skTextMsg(NrErrore) =
080400160530           %trim(fiora0m0.skTextMsg(NrErrore)) + ' ENTER x Forzare';
080500160531         WHEN  fiora0m0.skIdMsg(NrErrore) = 'TIS0727';
080600160530           fiora0m0.skTextMsg(NrErrore) =
080700160530           %trim(fiora0m0.skTextMsg(NrErrore)) + ' ENTER x Forzare';
080800160531         WHEN  fiora0m0.skIdMsg(NrErrore) = 'TIS0728';
080900160530           fiora0m0.skTextMsg(NrErrore) =
081000160530           %trim(fiora0m0.skTextMsg(NrErrore)) + ' ENTER x Forzare';
081100160531         WHEN  fiora0m0.skIdMsg(NrErrore) = 'TIS0722';
081200160530           fiora0m0.skTextMsg(NrErrore) =
081300160530           %trim(fiora0m0.skTextMsg(NrErrore)) + ' ENTER x Forzare';
081400160530         ENDSL;
081500160530
081600160530       ENDSR;
081700140618
081800140618       //--------------------------------------------------------------
081900140618       //?Imposto posizionamento cursore.
082000140618       //--------------------------------------------------------------
082100141008       BEGSR  ImpostaPC;
082200140618
082300140618         SELECT;
082400140620         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOTOR';
082500140620           PosCurTOR = *on;
082600160519         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOTCO';
082700160519           PosCurTCO = *on;
082800140620         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOCRA';
082900140620           PosCurCRA = *on;
083000140620         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAORSR';
083100140620           PosCurRSR = *on;
083200140620         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOINR';
083300140620           PosCurINR = *on;
083400141009         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOCAR';
083500141009           PosCurCAR = *on;
083600140620         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOLOR';
083700140620           PosCurLOR = *on;
083800160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOPRR';
083900160608           PosCurPRR = *on;
084000160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAONAR';
084100160608           PosCurNAR = *on;
084200141008         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAONCL';
084300141008           PosCurNCL = *on;
084400141008         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOPKG';
084500141008           PosCurPKG = *on;
084600141008         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOBNC';
084700141008           PosCurBNC = *on;
084800141008         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOVLM';
084900141008           PosCurVLM = *on;
085000141009         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOORR';
085100141009           PosCurORR = *on;
085200170403         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOPDM';
085300170403           PosCurDPM = *on;
085400160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'DALLEAM';
085500160608           PosCurDALLEAM = *on;
085600160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'ALLEAM';
085700160608           PosCurALLEAM = *on;
085800160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'DALLEPM';
085900160608           PosCurDALLEPM = *on;
086000160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'ALLEPM';
086100160608           PosCurALLEPM = *on;
086200141010         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAORER';
086300141010           PosCurRER = *on;
086400160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOTER';
086500160608           PosCurTER = *on;
086600160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAONO1';
086700160608           PosCurNO1 = *on;
086800160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOPAG';
086900160608           PosCurPAG = *on;
087000141013         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOPOR';
087100141013           PosCurPOR = *on;
087200141016         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOCRC';
087300141016           PosCurCRC = *on;
087400160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOCOR';
087500160608           PosCurCOR = *on;
087600160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAORSC';
087700160608           PosCurRSC = *on;
087800160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAORSO';
087900160608           PosCurRSO = *on;
088000160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOINC';
088100160608           PosCurINC = *on;
088200160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOINO';
088300160608           PosCurINO = *on;
088400160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOLOC';
088500160608           PosCurLOC = *on;
088600160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOPRC';
088700160608           PosCurPRC = *on;
088800160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOCAC';
088900160608           PosCurCAC = *on;
089000160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAONAC';
089100160608           PosCurNAC = *on;
089200160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOLOO';
089300160608           PosCurLOO = *on;
089400160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOPRO';
089500160608           PosCurPRO = *on;
089600160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOCAO';
089700160608           PosCurCAO = *on;
089800160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAONAO';
089900160608           PosCurNAO = *on;
090000141020         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOPOC';
090100141020           PosCurPOC = *on;
090200141021         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOKSC';
090300141021           PosCurKSC = *on;
090400141021         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOCTR';
090500141021           PosCurCTR = *on;
090600160610         WHEN  fiora0m0.skIdCampo(NrErrore) = 'CGI';
090700160610           PosCurCGI = *on;
090800160613         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOBLC';
090900160613           PosCurBLC = *on;
091000160613         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOMTC';
091100160613           PosCurMTC = *on;
091200160613         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOATT';
091300160613           PosCurATT = *on;
091400160620         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOATT';
091500160620           PosCurATT = *on;
091600160620         WHEN  fiora0m0.skIdCampo(NrErrore) = 'SMS';
091700160620           PosCurSMS = *on;
091800160620         WHEN  fiora0m0.skIdCampo(NrErrore) = 'MAIL';
091900160620           PosCurMAIL = *on;
092000160620         WHEN  fiora0m0.skIdCampo(NrErrore) = 'SMSCONF';
092100160620           PosCurSMSC = *on;
092200160620         WHEN  fiora0m0.skIdCampo(NrErrore) = 'MAILCONF';
092300160620           PosCurMAILC = *on;
092400170522         WHEN  fiora0m0.skIdCampo(NrErrore) = 'CONTATTOT';
092500170522           PosCurCOMM = *on;
092600140620         ENDSL;
092700140618
092800140618       ENDSR;
092900140618
093000140617       //--------------------------------------------------------------
093100140617       //?Ricerca dati '?' sui campi.
093200140617       //--------------------------------------------------------------
093300141008       BEGSR  RicercaDati;
093400160404
093500160404       //?Ricerca tipo Comunicazione ORM
093600160404         IF  %scan('?':V01tco) > 0;
093700160404           clear TIBS02ds;
093800160404           tibs02ds.T02mod = 'R';
093900160404           tibs02ds.T02cod = 'TCO';
094000160404           TNTBE_RicercaControllo (kpjba:tibs02ds);
094100160404           IF  tibs02ds.T02ke1 <> *blanks;
094200160404             V01tco  = tibs02ds.T02ke1;
094300160404             V01dtco = tibs02ds.T02uni;
094400160404           ENDIF;
094500160404           PosCurTCO = *on;
094600160404           exsr EmetteVideo;
094700160404           leavesr;
094800160404         ENDIF;
094900140617
095000140617       //?Ricerca tipo ORM
095100140617         IF  %scan('?':V01tor) > 0;
095200140617           clear TIBS02ds;
095300140617           tibs02ds.T02mod = 'R';
095400140617           tibs02ds.T02cod = 'TOR';
095500140617           TNTBE_RicercaControllo (kpjba:tibs02ds);
095600140617           IF  tibs02ds.T02ke1 <> *blanks;
095700140617             V01tor  = tibs02ds.T02ke1;
095800140617             V01dtor = tibs02ds.T02uni;
095900140617           ENDIF;
096000140620           PosCurTOR = *on;
096100140618           exsr EmetteVideo;
096200140617           leavesr;
096300140617         ENDIF;
096400140617
096500140617       //?Ricerca Filiale Ritiro
096600140617         IF  %scan('?':V01por) > 0;
096700140617           clear pinFIL;
096800140617           clear pinFAG;
096900140617           clear pinDES;
097000140617           clear pinDIT;
097100140617           tnsd24r (pinFIL:pinFAG:pinDES:pinDIT);
097200140617           IF pinFIL > *zeros;
097300140617             V01por  = pinFIL;
097400140617             V01dpor = pinDES;
097500140617           ENDIF;
097600141014           PosCurPOR = *on;
097700140618           exsr EmetteVideo;
097800140617           leavesr;
097900140617         ENDIF;
098000140617
098100140617       //?Ricerca Filiale Consegna
098200140617         IF  %scan('?':V01poc) > 0;
098300140617           clear pinFIL;
098400140617           clear pinFAG;
098500140617           clear pinDES;
098600140617           clear pinDIT;
098700140617           tnsd24r (pinFIL:pinFAG:pinDES:pinDIT);
098800140617           IF pinFIL > *zeros;
098900140617             V01poc  = pinFIL;
099000140617             V01dpoc = pinDES;
099100140617           ENDIF;
099200141020           PosCurPOC = *on;
099300140618           exsr EmetteVideo;
099400140617           leavesr;
099500140617         ENDIF;
099600140617
099700140617       //?Ricerca Giro di ritiro
099800140617         IF  %scan('?':V02cgi) > 0;
099900140617           clear fidg09ds;
100000140617           fidg09ds.D09iop0 = 'P01';
100100140618           fidg09ds.D09ifgs = %dec(V01por:3:0);
100200140617           fidg09ds.D09idat = %dec(%date());
100300140617           fidg09ds.D09itug = 'R';
100400140617           kpjbu = FIDG09DS;
100500140617           fidg09r (kpjba);
100600140617           FIDG09DS = kpjbu;
100700140617           IF  fidg09ds.D09ocgi <> *blanks;
100800170419             V02tgi  = 'M';
100900140617             V02cgi  = fidg09ds.D09ocgi;
101000140617             V02dcgi = fidg09ds.D09odes;
101100140617           ENDIF;
101200141014           PosCurCGI = *on;
101300141014           exsr EmetteVideo;
101400140617           leavesr;
101500140617         ENDIF;
101600140617
101700140617       ENDSR;
101800160523
101900160523       //--------------------------------------------------------------
102000160523       //?Interrogazione Cappario.
102100160523       //--------------------------------------------------------------
102200160523       BEGSR  IntCappario;
102300160523
102400160523       //?Interrogo cappario quando ho i dati dell'indirizzo ma no
102500160523       //?Cap e Provincia
102600160523
102700160523       //?Mittente non codificato
102800160523         IF  V01cra1 = 0 and V01cra2 = 0 and
102900160523             V01rsr <> *blanks and V01inr <> *blanks and
103000160523             V01lor <> *blanks and V01car = *blanks and
103100160523             V01prr = *blanks;
103200160523           wRagSoc    = V01rsr;
103300160523           wCap       = V01car;
103400160523           wNazione   = V01nar;
103500160523           wIndirizzo = V01inr;
103600160523           wLocalita  = V01lor;
103700160523           wProvincia = V01prr;
103800160523           exsr call_FNLV14R;
103900160523           V01car = fnlv14ds.E14cap;
104000160523           V01lor = fnlv14ds.E14loc;
104100160523           V01prr = fnlv14ds.E14prv;
104200160523           V01nar = fnlv14ds.E14nar;
104300160523         //?Se errore o dato F12 quindi non ho caricato nessun cap e provincia
104400160523         //?demando il tutto ai pgm di controllo
104500160523           IF  (fnlv14ds.O14err <> *blanks and fnlv14ds.O14msg <> *blanks) or
104600160523               (V01car = *blanks and V01prr = *blanks);
104700160523             leavesr;
104800160523           ENDIF;
104900160523         //?Se arrivo qua tutto OK quindi riemetto il video
105000160523         //?e mi posiziono sui colli
105100160523           PosCurNCL = *on;
105200160523           exsr EmetteVideo;
105300160523         ENDIF;
105400160523
105500160523       //?Ordinante non codificato
105600160523         IF  V01cor1 = 0 and V01cor2 = 0 and
105700160523             V01rso <> *blanks and V01ino <> *blanks and
105800160523             V01loo <> *blanks and V01cao = *blanks and
105900160523             V01pro = *blanks;
106000160523           wRagSoc    = V01rso;
106100160523           wCap       = V01cao;
106200160523           wNazione   = V01nao;
106300160523           wIndirizzo = V01ino;
106400160523           wLocalita  = V01loo;
106500160523           wProvincia = V01pro;
106600160523           exsr call_FNLV14R;
106700160523           V01cao = fnlv14ds.E14cap;
106800160523           V01loo = fnlv14ds.E14loc;
106900160607           savloo = fnlv14ds.E14loc;
107000160523           V01pro = fnlv14ds.E14prv;
107100160523           V01nao = fnlv14ds.E14nar;
107200160523           IF  fnlv14ds.O14err <> *blanks and fnlv14ds.O14msg <> *blanks;
107300160523             leavesr;
107400160523           ENDIF;
107500160523           exsr EmetteVideo;
107600160523         ENDIF;
107700160523
107800160523       //?Destinatario non codificato
107900160523         IF  V01crc1 = 0 and V01crc2 = 0 and
108000160523             V01rsc <> *blanks and V01inc <> *blanks and
108100160523             V01loc <> *blanks and V01cac = *blanks and
108200160523             V01prc = *blanks;
108300160523           wRagSoc    = V01rsc;
108400160523           wCap       = V01cac;
108500160523           wNazione   = V01nac;
108600160523           wIndirizzo = V01inc;
108700160523           wLocalita  = V01loc;
108800160523           wProvincia = V01prc;
108900160523           exsr call_FNLV14R;
109000160523           V01cac = fnlv14ds.E14cap;
109100160523           V01loc = fnlv14ds.E14loc;
109200160607           savloc = fnlv14ds.E14loc;
109300160523           V01prc = fnlv14ds.E14prv;
109400160523           V01nac = fnlv14ds.E14nar;
109500160523           IF  fnlv14ds.O14err <> *blanks and fnlv14ds.O14msg <> *blanks;
109600160523             leavesr;
109700160523           ENDIF;
109800160523         ENDIF;
109900160523
110000160523       ENDSR;
110100160523
110200160523       //--------------------------------------------------------------
110300160523       //?Richiamo Interrogazione cappario.
110400160523       //--------------------------------------------------------------
110500160523       BEGSR  call_FNLV14R;
110600160523
110700160523         clear FNLV14DS;
110800160523         fnlv14ds.I14dri = dataoggi;
110900160523         fnlv14ds.I14rsc = wRagSoc;
111000160523         fnlv14ds.I14ind = wIndirizzo;
111100160523         fnlv14ds.E14cap = wCap;
111200160523         fnlv14ds.E14loc = wLocalita;
111300160523         fnlv14ds.E14prv = wProvincia;
111400160523         fnlv14ds.E14nar = wNazione;
111500160523
111600160523         fnlv14r (kpjba:fnlv14ds);
111700160523
111800160523       ENDSR;
111900160615
112000160615       //--------------------------------------------------------------
112100160621       //?F11 - Richiamo pgm calcolo Volume.
112200160615       //--------------------------------------------------------------
112300160621       BEGSR  TastoF11;
112400160615
112500160615         clear UL36volume;
112600160615         trul36r (UL36volume);
112700160615         V01vlm += UL36volume;
112800160615
112900160615       ENDSR;
113000160621
113100160621       //--------------------------------------------------------------
113200160621       //?F14 - Richiamo pgm calcolo Orari Servizio.
113300160621       //--------------------------------------------------------------
113400160621       BEGSR  TastoF14;
113500160621
113600160621         clear wData;
113700170403         IF  fior000i.dpm <> 0;
113800170403           wAnno = %dec(%subst(%editc(fior000i.dpm:'X'):5:4):4:0);
113900170403           wMese = %dec(%subst(%editc(fior000i.dpm:'X'):3:2):2:0);
114000170403           wGiorno = %dec(%subst(%editc(fior000i.dpm:'X'):1:2):2:0);
114100160621         ELSE;
114200160621           wAnno = %dec(%subst(%editc(fior000i.dao:'X'):5:4):4:0);
114300160621           wMese = %dec(%subst(%editc(fior000i.dao:'X'):3:2):2:0);
114400160621           wGiorno = %dec(%subst(%editc(fior000i.dao:'X'):1:2):2:0);
114500160621         ENDIF;
114600160621
114700160621         clear trulorsds;
114800160621         trulorsds.IOREfil = %dec(fior000i.por:3:0);
114900160621         trulorsds.IOREcap = fior000i.car;
115000160621         trulorsds.IOREloc = fior000i.lor;
115100160621         trulorsds.IOREnar = fior000i.nar;
115200160621         trulorsds.IOREdta = wData;
115300160621         trulorsds.IOREtser = 'R';
115400160621         trulorsds.IOREgf2 = fior000i.pickupcalc;
115500160621         trulorsds.IOREorp = fior000i.orapresrit;
115600160621
115700160621         trulorsr (kpjba:trulorsds);
115800160621
115900160621       ENDSR;
116000140617
116100140617       //--------------------------------------------------------------
116200140617       //?Sposto i dati dal video alla DS.
116300140617       //--------------------------------------------------------------
116400141008       BEGSR  MoveVideoDS;
116500140617
116600140618         reset fior000o;
116700140617
116800140618         fior000o.dao      = V01dao;
116900140618         fior000o.oao      = V01oao;
117000140617
117100140618         fior000o.tor      = V01tor;
117200140618         fior000o.tco      = V01tco;
117300140617
117400140618         fior000o.cra1     = V01cra1;
117500140618         fior000o.cra2     = V01cra2;
117600140618         fior000o.cra3     = V01cra3;
117700140618         fior000o.rsr      = V01rsr;
117800140618         fior000o.inr      = V01inr;
117900140618         fior000o.lor      = V01lor;
118000140618         fior000o.prr      = V01prr;
118100140618         fior000o.car      = V01car;
118200140618         fior000o.nar      = V01nar;
118300140618         fior000o.rer      = V01rer;
118400140618         fior000o.ter      = V01ter;
118500140617
118600140618         fior000o.dalleam  = V01dalleam;
118700140618         fior000o.alleam   = V01alleam;
118800140618         fior000o.dallepm  = V01dallepm;
118900140618         fior000o.allepm   = V01allepm;
119000140618         fior000o.nam      = V01nam;
119100140618         fior000o.rfa      = V01rfa;
119200140618         fior000o.nota1    = V01nota1;
119300140618         fior000o.nota2    = V01nota2;
119400140617
119500140618         fior000o.orr      = V01orr;
119600170403         fior000o.dpm      = V01dpm;
119700140618         fior000o.dar      = V01dar;
119800140617
119900140618         fior000o.ntwdpd   = V01ntwd;
120000141020         fior000o.por      = V01por;
120100140617
120200140618         fior000o.ncl      = V01ncl;
120300140618         fior000o.pkg      = V01pkg;
120400140618         fior000o.bnc      = V01bnc;
120500140618         fior000o.vlm      = V01vlm;
120600140618         fior000o.spi      = V01spi;
120700140617
120800140618         fior000o.pag      = V01pag;
120900140618         fior000o.ksc      = V01ksc;
121000140618         fior000o.ctr      = V01ctr;
121100140617
121200140618         fior000o.comm     = V01comm;
121300140617
121400140618         fior000o.crc1     = V01crc1;
121500140618         fior000o.crc2     = V01crc2;
121600140618         fior000o.crc3     = V01crc3;
121700140618         fior000o.rsc      = V01rsc;
121800140618         fior000o.inc      = V01inc;
121900160607         fior000o.loc      = savloc;
122000140618         fior000o.prc      = V01prc;
122100140618         fior000o.cac      = V01cac;
122200140618         fior000o.nac      = V01nac;
122300141020         fior000o.poc      = V01poc;
122400140617
122500140618         fior000o.cor1     = V01cor1;
122600140618         fior000o.cor2     = V01cor2;
122700140618         fior000o.cor3     = V01cor3;
122800140618         fior000o.rso      = V01rso;
122900140618         fior000o.ino      = V01ino;
123000160607         fior000o.loo      = savloo;
123100140618         fior000o.pro      = V01pro;
123200140618         fior000o.cao      = V01cao;
123300140618         fior000o.nao      = V01nao;
123400140617
123500140618         fior000o.blc      = V02blc;
123600140618         fior000o.att      = V02att;
123700140618         fior000o.mtc      = V02mtc;
123800140618         fior000o.sto      = V02sto;
123900170419         fior000o.tgi      = V02tgi;
124000140618         fior000o.cgi      = V02cgi;
124100170419       //?se il giro è stato variato
124200170419         IF  fior000i.cgi <> fior000o.cgi and
124300170419             fior000i.cgi <> *blanks;
124400170419         //?imposto tipo giro 'M' se è stato impostato un nuovo giro
124500170419           IF  fior000o.cgi <> *blanks;
124600170419             fior000o.tgi = 'M';
124700170419           ENDIF;
124800170419         //?pulisco il tipo giro se non è stato impostato un nuovo giro
124900170419           IF  fior000o.cgi = *blanks;
125000170419             clear fior000o.tgi;
125100170419           ENDIF;
125200170419         ENDIF;
125300160404         IF  V02ffd = *blanks;
125400160404           fior000o.ffd    = 'N';
125500160404         ELSE;
125600160404           fior000o.ffd    = V02ffd;
125700160404         ENDIF;
125800160608
125900160608         fior000o.tipoord = tipoord;
126000140617
126100140618         fior000o.mail     = V03mail;
126200140618         fior000o.sms      = V03sms;
126300160613
126400160613         fior000o.mailconf = V04mailcon;
126500160613         fior000o.smsconf  = V04smsconf;
126600140619
126700140619       //?Imposto il tasto premuto dall'utente
126800140619         fior000a.TastoVideo = dsp_aid;
126900160622
127000160622       //?Imposto SEMPRE in positivo le quanatità
127100160622         IF  fior000o.ncl < 0;
127200160622           fior000o.ncl = fior000o.ncl * -1;
127300160622         ENDIF;
127400160622         IF  fior000o.pkg < 0;
127500160622           fior000o.pkg = fior000o.pkg * -1;
127600160622         ENDIF;
127700160622         IF  fior000o.vlm < 0;
127800160622           fior000o.vlm = fior000o.vlm * -1;
127900160622         ENDIF;
128000160622         IF  fior000o.bnc < 0;
128100160622           fior000o.bnc = fior000o.bnc * -1;
128200160622         ENDIF;
128300160622         IF  fior000o.blc < 0;
128400160622           fior000o.blc = fior000o.blc * -1;
128500160622         ENDIF;
128600160622         IF  fior000o.mtc < 0;
128700160622           fior000o.mtc = fior000o.mtc * -1;
128800160622         ENDIF;
128900160622         IF  fior000o.att < 0;
129000160622           fior000o.att = fior000o.att * -1;
129100160622         ENDIF;
129200140617
129300140617       ENDSR;
129400140414
129500140414       //--------------------------------------------------------------
129600140414       //?Operazioni finali.
129700140414       //--------------------------------------------------------------
129800140414       BEGSR  RoutEnd;
129900140414
130000140618         %subst(prmRpyData:1:prmRpySize) = fior000o;
130100140618         %subst(prmRpyTastiFun:1:prmRpySizeTastiFun) = fior000a;
130200141013         %subst(prmRpyForzatu:1:prmRpySizeForz) = fiora0f0;
130300140617
130400140414         return;
130500140414
130600140414       ENDSR;
130700140414
130800140414       //--------------------------------------------------------------
130900140415      /end-free

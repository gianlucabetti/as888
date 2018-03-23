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
037000170530             prmRqsOpCode <> FIOR000_RQSOPCODE_CONFERMA_FISSI and
037100141010             prmRqsOpCode <> FIOR000_RQSOPCODE_CONFERMA_INTERNET and
037200141010             prmRqsOpCode <> FIOR000_RQSOPCODE_MANUTENZIONE;
037300140414           prmRpyOpCode = FIOR000_RPYOPCODE_ERROR;
037400140414           prmRpyIdMsg  = FIOR000_ESITO_RQSOPCODE_SCONOSCIUTO;
037500140414           exsr RoutEnd;
037600140414         ENDIF;
037700140414
037800140414       //?Formato e size RQS
037900140617         IF  prmRqsSize > 0;
038000140617         ELSE;
038100140617           prmRpyOpCode = FIOR000_RPYOPCODE_ERROR;
038200140617           prmRpyIdMsg = FIOR000_ESITO_SIZE_DATA_ERRATO;
038300140617           exsr RoutEnd;
038400140617         ENDIF;
038500140618         IF  prmRqsFormato = fior000i.formato;
038600140618           fior000i = %SUBST(prmRqsData:1:prmRqsSize);
038700140414         ELSE;
038800140414           prmRpyOpCode = FIOR000_RPYOPCODE_ERROR;
038900140414           prmRpyIdMsg = FIOR000_ESITO_NOME_FORMATO_SCONOSCIUTO;
039000140414           exsr RoutEnd;
039100140414         ENDIF;
039200140414
039300140414       //?Formato e size RPY
039400140617         IF  prmRpySize > 0;
039500140617         ELSE;
039600140617           prmRpyOpCode = FIOR000_RPYOPCODE_ERROR;
039700140617           prmRpyIdMsg = FIOR000_ESITO_SIZE_DATA_ERRATO;
039800140617           exsr RoutEnd;
039900140617         ENDIF;
040000140618         IF  prmRpyFormato = fior000o.formato;
040100140414         ELSE;
040200140414           prmRpyOpCode = FIOR000_RPYOPCODE_ERROR;
040300140414           prmRpyIdMsg = FIOR000_ESITO_NOME_FORMATO_SCONOSCIUTO;
040400140414           exsr RoutEnd;
040500140414         ENDIF;
040600140617
040700140617       //?Formato e size MSG
040800140617         IF  prmRpySizeMsg > 0;
040900140617         ELSE;
041000140617           prmRpyOpCode = FIOR000_RPYOPCODE_ERROR;
041100140617           prmRpyIdMsg = FIOR000_ESITO_SIZE_DATA_ERRATO;
041200140617           exsr RoutEnd;
041300140617         ENDIF;
041400140618         IF  prmRpyFormMsg = fiora0m0.formato;
041500140620           fiora0m0 = %SUBST(prmRpyMessage:1:prmRpySizeMsg);
041600140617         ELSE;
041700140617           prmRpyOpCode = FIOR000_RPYOPCODE_ERROR;
041800140617           prmRpyIdMsg = FIOR000_ESITO_NOME_FORMATO_SCONOSCIUTO;
041900140617           exsr RoutEnd;
042000140617         ENDIF;
042100140617
042200140617       //?Formato e size Forzature
042300140617         IF  prmRpySizeForz > 0;
042400140617         ELSE;
042500140617           prmRpyOpCode = FIOR000_RPYOPCODE_ERROR;
042600140617           prmRpyIdMsg = FIOR000_ESITO_SIZE_DATA_ERRATO;
042700140617           exsr RoutEnd;
042800140617         ENDIF;
042900140617         IF  prmRpyFormForz = fiora0f0.formato;
043000141013           fiora0f0 = %SUBST(prmRpyForzatu:1:prmRpySizeForz);
043100140617         ELSE;
043200140617           prmRpyOpCode = FIOR000_RPYOPCODE_ERROR;
043300140617           prmRpyIdMsg = FIOR000_ESITO_NOME_FORMATO_SCONOSCIUTO;
043400140617           exsr RoutEnd;
043500140617         ENDIF;
043600140617
043700140617       //?Formato e size Tasti Funzione
043800140617         IF  prmRpySizeTastiFun > 0;
043900140617         ELSE;
044000140617           prmRpyOpCode = FIOR000_RPYOPCODE_ERROR;
044100140617           prmRpyIdMsg = FIOR000_ESITO_SIZE_DATA_ERRATO;
044200140617           exsr RoutEnd;
044300140617         ENDIF;
044400140618         IF  prmRpyFormTastiFun = fior000a.formato;
044500140618           fior000a = %SUBST(prmRpyTastiFun:1:prmRpySizeTastiFun);
044600140617         ELSE;
044700140617           prmRpyOpCode = FIOR000_RPYOPCODE_ERROR;
044800140617           prmRpyIdMsg = FIOR000_ESITO_NOME_FORMATO_SCONOSCIUTO;
044900140617           exsr RoutEnd;
045000140617         ENDIF;
045100140414
045200140414       ENDSR;
045300140617
045400140617       //--------------------------------------------------------------
045500140617       //?Muovo i dati da DS input a Video.
045600140617       //--------------------------------------------------------------
045700141008       BEGSR  MoveDSVideo;
045800160322
045900160322         V01pgm     = SDSpgm;
046000140617
046100140618         V01mod     = fior000i.mod;
046200140618         V01rsut    = fior000i.rsut;
046300140618         V01sisinf  = fior000i.sisinf;
046400140618         V01utente  = fior000i.utente;
046500140618         V01tit     = fior000i.titolo;
046600140617
046700160321         V01poei    = fior000i.poei;
046800160321         V01dpoei   = fior000i.dpoei;
046900140617
047000140618         V01poeu    = fior000i.poeu;
047100140618         V01nsru    = fior000i.nsru;
047200140618         V01noru    = fior000i.noru;
047300140618         V01nrvu    = fior000i.nrvu;
047400140617
047500140618         V01fao     = fior000i.fao;
047600140618         V01dfao    = fior000i.dfao;
047700140618         V01poe     = fior000i.poe;
047800140618         V01nsr     = fior000i.nsr;
047900140618         V01nor     = fior000i.nor;
048000140618         V01nrv     = fior000i.nrv;
048100140618         V01dao     = fior000i.dao;
048200140618         V01oao     = fior000i.oao;
048300140617
048400140618         V01tor     = fior000i.tor;
048500140618         V01dtor    = fior000i.dtor;
048600140618         V01cau     = fior000i.cau;
048700140618         V01dcau    = fior000i.dcau;
048800140618         V01tco     = fior000i.tco;
048900140618         V01dtco    = fior000i.dtco;
049000140618         V01pos     = fior000i.pos;
049100140618         V01ors     = fior000i.ors;
049200140618         V01ostis   = fior000i.ostis;
049300140618         V01ostfs   = fior000i.ostfs;
049400140618         V01dfcs    = fior000i.dfcs;
049500140618         V01este    = fior000i.este;
049600140617
049700140618         V01cra1    = fior000i.cra1;
049800140618         V01cra2    = fior000i.cra2;
049900140618         V01cra3    = fior000i.cra3;
050000140618         V01rsr     = fior000i.rsr;
050100140618         V01inr     = fior000i.inr;
050200140618         V01lor     = fior000i.lor;
050300140618         V01prr     = fior000i.prr;
050400140618         V01car     = fior000i.car;
050500140618         V01nar     = fior000i.nar;
050600140618         V01rer     = fior000i.rer;
050700140618         V01ter     = fior000i.ter;
050800140617
050900140618         V01dalleam = fior000i.dalleam;
051000140618         V01alleam  = fior000i.alleam;
051100140618         V01dallepm = fior000i.dallepm;
051200140618         V01allepm  = fior000i.allepm;
051300140618         V01nam     = fior000i.nam;
051400140618         V01rfa     = fior000i.rfa;
051500140618         V01nota1   = fior000i.nota1;
051600140618         V01nota2   = fior000i.nota2;
051700140617
051800140618         V01orr     = fior000i.orr;
051900170403         V01dpm     = fior000i.dpm;
052000140618         V01dar     = fior000i.dar;
052100140618         V1anticipa = fior000i.anticipa;
052200140617
052300140618         V01ntwd    = fior000i.ntwdpd;
052400141020         V01por     = fior000i.por;
052500140618         V01dpor    = fior000i.dpor;
052600140617
052700140618         V01ncl     = fior000i.ncl;
052800140618         V01pkg     = fior000i.pkg;
052900140618         V01bnc     = fior000i.bnc;
053000140618         V01vlm     = fior000i.vlm;
053100140618         V01spi     = fior000i.spi;
053200140617
053300140618         V01pag     = fior000i.pag;
053400140618         V01ksc     = fior000i.ksc;
053500140618         V01ctr     = fior000i.ctr;
053600140617
053700140618         V01comm    = fior000i.comm;
053800140617
053900140618         V01crc1    = fior000i.crc1;
054000140618         V01crc2    = fior000i.crc2;
054100140618         V01crc3    = fior000i.crc3;
054200140618         V01rsc     = fior000i.rsc;
054300140618         V01inc     = fior000i.inc;
054400140618         V01loc     = fior000i.loc;
054500160607         savloc     = fior000i.loc;
054600140618         V01prc     = fior000i.prc;
054700140618         V01cac     = fior000i.cac;
054800140618         V01nac     = fior000i.nac;
054900141020         V01poc     = fior000i.poc;
055000140618         V01dpoc    = fior000i.dpoc;
055100140617
055200140618         V01cor1    = fior000i.cor1;
055300140618         V01cor2    = fior000i.cor2;
055400140618         V01cor3    = fior000i.cor3;
055500140618         V01rso     = fior000i.rso;
055600140618         V01ino     = fior000i.ino;
055700140618         V01loo     = fior000i.loo;
055800160607         savloo     = fior000i.loo;
055900140618         V01pro     = fior000i.pro;
056000140618         V01cao     = fior000i.cao;
056100140618         V01nao     = fior000i.nao;
056200140617
056300140618         V02blc     = fior000i.blc;
056400140618         V02att     = fior000i.att;
056500140618         V02mtc     = fior000i.mtc;
056600140618         V02sto     = fior000i.sto;
056700140618         V02dsto    = fior000i.dsto;
056800170419         V02tgi     = fior000i.tgi;
056900140618         V02cgi     = fior000i.cgi;
057000140618         V02dcgi    = fior000i.dcgi;
057100141008         IF  fior000i.ffd = 'N';
057200141008           clear V02ffd;
057300141008         ELSE;
057400141008           V02ffd = fior000i.ffd;
057500141008         ENDIF;
057600140617
057700141008         V03mail = fior000i.mail;
057800141008         V03sms  = fior000i.sms;
057900141008
058000141008         V01ostis = fior000i.orainizio;
058100141008         V01ostfs = fior000i.orafine;
058200160531
058300160613         V04mailcon = fior000i.mailconf;
058400160613         V04smsconf = fior000i.smsconf;
058500170404
058600170404         V01ddt = fior000i.ddt;
058700140414
058800140414       ENDSR;
058900140415
059000140415       //--------------------------------------------------------------
059100140415       //?Imposto i tasti funzione.
059200140415       //--------------------------------------------------------------
059300141008       BEGSR  TastiFunzione;
059400140617
059500140617       //?Abilito i tasti funzione
059600140617         NrTasto = 1;
059700140618         FOR  NrTasto by 1 to %elem(fior000a.skTastiFun);
059800140618           %subst(AbilitaF:NrTasto:1) = fior000a.skTastiFun(NrTasto);
059900140617         ENDFOR;
060000140415
060100140617       //?Imposto l'ultima riga dei tasti
060200140618         V01tf1 = fior000a.rigatfmob;
060300140618         V01tf2 = fior000a.rigatf24;
060400140415
060500140415       ENDSR;
060600140618
060700140618       //--------------------------------------------------------------
060800140618       //?Imposto attributi del video.
060900140618       //--------------------------------------------------------------
061000141008       BEGSR  Attributi;
061100140620
061200140620         WindDspF    = IndDspF;
061300140620         reset WindDspFb;
061400140620         IndDspF     = WindDspF;
061500140618
061600140619         NrAttributo = 1;
061700140619         FOR  NrAttributo by 1 to fior000a.NrAttr;
061800140619           SELECT;
061900160321           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRTCO';
062000160321             ATTRtco = fior000a.skIdAttr(NrAttributo);
062100160321           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRPOEI';
062200160321             ATTRpoei = fior000a.skIdAttr(NrAttributo);
062300160321           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRDPOEI';
062400160321             ATTRdpoei = fior000a.skIdAttr(NrAttributo);
062500170403           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRDAR';
062600170403             ATTRdar = fior000a.skIdAttr(NrAttributo);
062700140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRORSR';
062800140619             ATTRorsr = fior000a.skIdAttr(NrAttributo);
062900140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRNORU';
063000140619             ATTRnoru = fior000a.skIdAttr(NrAttributo);
063100140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRFASE';
063200140619             ATTRfase = fior000a.skIdAttr(NrAttributo);
063300140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRCRA1';
063400140619             ATTRcra1 = fior000a.skIdAttr(NrAttributo);
063500140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRCRA2';
063600140619             ATTRcra2 = fior000a.skIdAttr(NrAttributo);
063700140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRCOR1';
063800140619             ATTRcor1 = fior000a.skIdAttr(NrAttributo);
063900140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRCRC1';
064000140619             ATTRcrc1 = fior000a.skIdAttr(NrAttributo);
064100140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRNOTA';
064200140619             ATTRnota = fior000a.skIdAttr(NrAttributo);
064300140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRTOR';
064400140619             ATTRtor  = fior000a.skIdAttr(NrAttributo);
064500140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRCRA';
064600140619             ATTRcra  = fior000a.skIdAttr(NrAttributo);
064700140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRNOR';
064800140619             ATTRnor  = fior000a.skIdAttr(NrAttributo);
064900140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRCAU';
065000140619             ATTRcau  = fior000a.skIdAttr(NrAttributo);
065100140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRQTA';
065200140619             ATTRqta  = fior000a.skIdAttr(NrAttributo);
065300140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRCRC';
065400140619             ATTRcrc  = fior000a.skIdAttr(NrAttributo);
065500140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRCOR';
065600140619             ATTRcor  = fior000a.skIdAttr(NrAttributo);
065700140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRPOR';
065800140619             ATTRpor  = fior000a.skIdAttr(NrAttributo);
065900140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRPOC';
066000140619             ATTRpoc  = fior000a.skIdAttr(NrAttributo);
066100140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRNAM';
066200140619             ATTRnam  = fior000a.skIdAttr(NrAttributo);
066300140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRRFA';
066400140619             ATTRrfa  = fior000a.skIdAttr(NrAttributo);
066500140619           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRPAG';
066600140619             ATTRpag  = fior000a.skIdAttr(NrAttributo);
066700141013           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRCOMM';
066800141013             ATTRcomm  = fior000a.skIdAttr(NrAttributo);
066900141010           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRF04';
067000141010             ATTRf04  = fior000a.skIdAttr(NrAttributo);
067100141010           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRF05';
067200141010             ATTRf05  = fior000a.skIdAttr(NrAttributo);
067300141010           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRF17';
067400141010             ATTRf17  = fior000a.skIdAttr(NrAttributo);
067500141010           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRF18';
067600141010             ATTRf18  = fior000a.skIdAttr(NrAttributo);
067700141014           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRBLC';
067800141014             ATTRblc  = fior000a.skIdAttr(NrAttributo);
067900141014           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRATT';
068000141014             ATTRatt  = fior000a.skIdAttr(NrAttributo);
068100141014           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRMTC';
068200141014             ATTRmtc  = fior000a.skIdAttr(NrAttributo);
068300141014           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRSTO';
068400141014             ATTRsto  = fior000a.skIdAttr(NrAttributo);
068500141014           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRCGI';
068600141014             ATTRcgi  = fior000a.skIdAttr(NrAttributo);
068700141014           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRFFD';
068800141014             ATTRffd  = fior000a.skIdAttr(NrAttributo);
068900160610           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRSMS';
069000160610             ATTRsms  = fior000a.skIdAttr(NrAttributo);
069100160610           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRMAIL';
069200160610             ATTRmail = fior000a.skIdAttr(NrAttributo);
069300160613           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRSMSC';
069400160613             ATTRsmsc = fior000a.skIdAttr(NrAttributo);
069500160613           WHEN  fior000a.skIdCampo(NrAttributo) = 'ATTRMAILC';
069600160613             ATTRmailc = fior000a.skIdAttr(NrAttributo);
069700140619           ENDSL;
069800140618         ENDFOR;
069900140618
070000140618       ENDSR;
070100140617
070200140618       //--------------------------------------------------------------
070300140618       //?Imposto gli errori a video.
070400140618       //--------------------------------------------------------------
070500141008       BEGSR  EmetteErrore;
070600140618
070700140618       //?Emetto il messaggio di errore ne devo emettere 1 alla volta
070800140618         NrErrore = 1;
070900140618         FOR  NrErrore by 1 to fiora0m0.NrMsg;
071000141013
071100160531         //?Se è un Warning lo scrivo nella sk delle forzature come già forzato
071200141013           IF  fiora0m0.skErrWarn(NrErrore) = FIORA00_MSG_WARNING;
071300141013             clear NrForza;
071400141013             NrForza =
071500160531             %lookup(*blanks:fiora0f0.skIdmsg);
071600160531             IF  NrForza > 0;
071700160531               fiora0f0.skIdMsg(NrForza) = fiora0m0.skIdMsg(NrErrore);
071800160531               fiora0f0.skIdCampo(NrForza) = fiora0m0.skIdCampo(NrErrore);
071900160531               fiora0f0.skGiaForza(NrForza) = FIORA00_GIAFORZA_SI;
072000141013             ENDIF;
072100160531           //?In alcuni casi devo impostare che msg. forzabile
072200160531             exsr ImpostaMsg;
072300141013           ENDIF;
072400141013
072500140618         //?In base al nome campo imposto il posizionamento cursore e
072600140618         //?il campo messaggi
072700140618           exsr ImpostaPC;
072800141010           IF  fiora0m0.skTextMsg(NrErrore) = FIOR000_IMPOSTA_PC;
072900141010             leavesr;
073000141010           ENDIF;
073100160610           IF  wMsg = *blanks;
073200160610             wMsg = fiora0m0.skTextMsg(NrErrore);
073300140620           ENDIF;
073400140618           ErrGenerico = *on;
073500160613           SELECT;
073600160616           WHEN  fior000i.video = FIOR000_VIDEO_PRINCIPALE;
073700160613             ErrMessage = *on;
073800160616           WHEN  fior000i.video = FIOR000_VIDEO_ALTRIDATI;
073900160613             ErrMessage02 = *on;
074000160616           WHEN  fior000i.video = FIOR000_VIDEO_ALERTCOMM;
074100160613             ErrMessage03 = *on;
074200160616           WHEN  fior000i.video = FIOR000_VIDEO_ALERTCONF;
074300160613             ErrMessage04 = *on;
074400160613           ENDSL;
074500160613           leavesr;
074600140618         ENDFOR;
074700140618
074800140618       ENDSR;
074900140618
075000140618       //--------------------------------------------------------------
075100140618       //?Emetto la videata.
075200140618       //--------------------------------------------------------------
075300141008       BEGSR  EmetteVideo;
075400140618
075500140618         SELECT;
075600160616         WHEN  fior000i.video = FIOR000_VIDEO_PRINCIPALE;
075700140717       //?Se non ci sono errori
075800140717           IF  not ErrMessage;
075900141015             SELECT;
076000140717         //?se il mittente non è ancora stato immesso
076100141015             WHEN  V01cra1 = 0 and V01rsr = *blanks;
076200140717           //?mi posiziono sul codice mittente
076300140717               PosCurCRA = *on;
076400141015         //?se i colli/peso/bancali non ci sono mi posiziono sui colli
076500141015             WHEN  V01ncl = 0 and V01pkg = 0 and V01bnc = 0;
076600141015               PosCurNCL = *on;
076700160608         //?se ho il tipo ordinante (videata WHO) impostato ad A=Altro
076800160608         //?mi posiziono sull'ordinante
076900160608             WHEN  fior000i.tipoord = 'A';
077000160608               PosCurCOR = *on;
077100140717         //?altrimenti sulle note
077200141015             OTHER;
077300141015               PosCurNO1 = *on;
077400141015             ENDSL;
077500140717           ENDIF;
077600160610           V01msg = wMsg;
077700140618           exfmt FIOR0001;
077800160616         WHEN  fior000i.video = FIOR000_VIDEO_ALTRIDATI;
077900160610           V02msg = wMsg;
078000160610           write FIOR0001;
078100140618           exfmt FIOR0002;
078200160616         WHEN  fior000i.video = FIOR000_VIDEO_ALERTCOMM;
078300160620           V03msg = wMsg;
078400140618           write FIOR0001;
078500140618           exfmt FIOR0003;
078600160616         WHEN  fior000i.video = FIOR000_VIDEO_ALERTCONF;
078700160620           V04msg = wMsg;
078800160613           write FIOR0001;
078900160613           exfmt FIOR0004;
079000141022         WHEN  fior000i.video = FIOR000_VIDEO_WHOISORD;
079100141022           write FIOR0001;
079200141022           exfmt FIOR000WO;
079300140618         ENDSL;
079400140618
079500140618       ENDSR;
079600160530
079700160530       //--------------------------------------------------------------
079800160530       //?Imposto 'Forzabile' nel messaggio di errore.
079900160530       //--------------------------------------------------------------
080000160530       BEGSR  ImpostaMsg;
080100160530
080200160530         SELECT;
080300160531         WHEN  fiora0m0.skIdMsg(NrErrore) = 'TIS0726';
080400160530           fiora0m0.skTextMsg(NrErrore) =
080500160530           %trim(fiora0m0.skTextMsg(NrErrore)) + ' ENTER x Forzare';
080600160531         WHEN  fiora0m0.skIdMsg(NrErrore) = 'TIS0727';
080700160530           fiora0m0.skTextMsg(NrErrore) =
080800160530           %trim(fiora0m0.skTextMsg(NrErrore)) + ' ENTER x Forzare';
080900160531         WHEN  fiora0m0.skIdMsg(NrErrore) = 'TIS0728';
081000160530           fiora0m0.skTextMsg(NrErrore) =
081100160530           %trim(fiora0m0.skTextMsg(NrErrore)) + ' ENTER x Forzare';
081200160531         WHEN  fiora0m0.skIdMsg(NrErrore) = 'TIS0722';
081300160530           fiora0m0.skTextMsg(NrErrore) =
081400160530           %trim(fiora0m0.skTextMsg(NrErrore)) + ' ENTER x Forzare';
081500160530         ENDSL;
081600160530
081700160530       ENDSR;
081800140618
081900140618       //--------------------------------------------------------------
082000140618       //?Imposto posizionamento cursore.
082100140618       //--------------------------------------------------------------
082200141008       BEGSR  ImpostaPC;
082300140618
082400140618         SELECT;
082500140620         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOTOR';
082600140620           PosCurTOR = *on;
082700160519         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOTCO';
082800160519           PosCurTCO = *on;
082900140620         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOCRA';
083000140620           PosCurCRA = *on;
083100140620         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAORSR';
083200140620           PosCurRSR = *on;
083300140620         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOINR';
083400140620           PosCurINR = *on;
083500141009         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOCAR';
083600141009           PosCurCAR = *on;
083700140620         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOLOR';
083800140620           PosCurLOR = *on;
083900160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOPRR';
084000160608           PosCurPRR = *on;
084100160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAONAR';
084200160608           PosCurNAR = *on;
084300141008         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAONCL';
084400141008           PosCurNCL = *on;
084500141008         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOPKG';
084600141008           PosCurPKG = *on;
084700141008         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOBNC';
084800141008           PosCurBNC = *on;
084900141008         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOVLM';
085000141008           PosCurVLM = *on;
085100141009         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOORR';
085200141009           PosCurORR = *on;
085300170403         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOPDM';
085400170403           PosCurDPM = *on;
085500160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'DALLEAM';
085600160608           PosCurDALLEAM = *on;
085700160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'ALLEAM';
085800160608           PosCurALLEAM = *on;
085900160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'DALLEPM';
086000160608           PosCurDALLEPM = *on;
086100160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'ALLEPM';
086200160608           PosCurALLEPM = *on;
086300141010         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAORER';
086400141010           PosCurRER = *on;
086500160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOTER';
086600160608           PosCurTER = *on;
086700160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAONO1';
086800160608           PosCurNO1 = *on;
086900160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOPAG';
087000160608           PosCurPAG = *on;
087100141013         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOPOR';
087200141013           PosCurPOR = *on;
087300141016         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOCRC';
087400141016           PosCurCRC = *on;
087500160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOCOR';
087600160608           PosCurCOR = *on;
087700160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAORSC';
087800160608           PosCurRSC = *on;
087900160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAORSO';
088000160608           PosCurRSO = *on;
088100160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOINC';
088200160608           PosCurINC = *on;
088300160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOINO';
088400160608           PosCurINO = *on;
088500160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOLOC';
088600160608           PosCurLOC = *on;
088700160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOPRC';
088800160608           PosCurPRC = *on;
088900160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOCAC';
089000160608           PosCurCAC = *on;
089100160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAONAC';
089200160608           PosCurNAC = *on;
089300160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOLOO';
089400160608           PosCurLOO = *on;
089500160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOPRO';
089600160608           PosCurPRO = *on;
089700160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOCAO';
089800160608           PosCurCAO = *on;
089900160608         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAONAO';
090000160608           PosCurNAO = *on;
090100141020         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOPOC';
090200141020           PosCurPOC = *on;
090300141021         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOKSC';
090400141021           PosCurKSC = *on;
090500141021         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOCTR';
090600141021           PosCurCTR = *on;
090700160610         WHEN  fiora0m0.skIdCampo(NrErrore) = 'CGI';
090800160610           PosCurCGI = *on;
090900160613         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOBLC';
091000160613           PosCurBLC = *on;
091100160613         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOMTC';
091200160613           PosCurMTC = *on;
091300160613         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOATT';
091400160613           PosCurATT = *on;
091500160620         WHEN  fiora0m0.skIdCampo(NrErrore) = 'VAOATT';
091600160620           PosCurATT = *on;
091700160620         WHEN  fiora0m0.skIdCampo(NrErrore) = 'SMS';
091800160620           PosCurSMS = *on;
091900160620         WHEN  fiora0m0.skIdCampo(NrErrore) = 'MAIL';
092000160620           PosCurMAIL = *on;
092100160620         WHEN  fiora0m0.skIdCampo(NrErrore) = 'SMSCONF';
092200160620           PosCurSMSC = *on;
092300160620         WHEN  fiora0m0.skIdCampo(NrErrore) = 'MAILCONF';
092400160620           PosCurMAILC = *on;
092500170522         WHEN  fiora0m0.skIdCampo(NrErrore) = 'CONTATTOT';
092600170522           PosCurCOMM = *on;
092700140620         ENDSL;
092800140618
092900140618       ENDSR;
093000140618
093100140617       //--------------------------------------------------------------
093200140617       //?Ricerca dati '?' sui campi.
093300140617       //--------------------------------------------------------------
093400141008       BEGSR  RicercaDati;
093500160404
093600160404       //?Ricerca tipo Comunicazione ORM
093700160404         IF  %scan('?':V01tco) > 0;
093800160404           clear TIBS02ds;
093900160404           tibs02ds.T02mod = 'R';
094000160404           tibs02ds.T02cod = 'TCO';
094100160404           TNTBE_RicercaControllo (kpjba:tibs02ds);
094200160404           IF  tibs02ds.T02ke1 <> *blanks;
094300160404             V01tco  = tibs02ds.T02ke1;
094400160404             V01dtco = tibs02ds.T02uni;
094500160404           ENDIF;
094600160404           PosCurTCO = *on;
094700160404           exsr EmetteVideo;
094800160404           leavesr;
094900160404         ENDIF;
095000140617
095100140617       //?Ricerca tipo ORM
095200140617         IF  %scan('?':V01tor) > 0;
095300140617           clear TIBS02ds;
095400140617           tibs02ds.T02mod = 'R';
095500140617           tibs02ds.T02cod = 'TOR';
095600140617           TNTBE_RicercaControllo (kpjba:tibs02ds);
095700140617           IF  tibs02ds.T02ke1 <> *blanks;
095800140617             V01tor  = tibs02ds.T02ke1;
095900140617             V01dtor = tibs02ds.T02uni;
096000140617           ENDIF;
096100140620           PosCurTOR = *on;
096200140618           exsr EmetteVideo;
096300140617           leavesr;
096400140617         ENDIF;
096500140617
096600140617       //?Ricerca Filiale Ritiro
096700140617         IF  %scan('?':V01por) > 0;
096800140617           clear pinFIL;
096900140617           clear pinFAG;
097000140617           clear pinDES;
097100140617           clear pinDIT;
097200140617           tnsd24r (pinFIL:pinFAG:pinDES:pinDIT);
097300140617           IF pinFIL > *zeros;
097400140617             V01por  = pinFIL;
097500140617             V01dpor = pinDES;
097600140617           ENDIF;
097700141014           PosCurPOR = *on;
097800140618           exsr EmetteVideo;
097900140617           leavesr;
098000140617         ENDIF;
098100140617
098200140617       //?Ricerca Filiale Consegna
098300140617         IF  %scan('?':V01poc) > 0;
098400140617           clear pinFIL;
098500140617           clear pinFAG;
098600140617           clear pinDES;
098700140617           clear pinDIT;
098800140617           tnsd24r (pinFIL:pinFAG:pinDES:pinDIT);
098900140617           IF pinFIL > *zeros;
099000140617             V01poc  = pinFIL;
099100140617             V01dpoc = pinDES;
099200140617           ENDIF;
099300141020           PosCurPOC = *on;
099400140618           exsr EmetteVideo;
099500140617           leavesr;
099600140617         ENDIF;
099700140617
099800140617       //?Ricerca Giro di ritiro
099900140617         IF  %scan('?':V02cgi) > 0;
100000140617           clear fidg09ds;
100100140617           fidg09ds.D09iop0 = 'P01';
100200140618           fidg09ds.D09ifgs = %dec(V01por:3:0);
100300140617           fidg09ds.D09idat = %dec(%date());
100400140617           fidg09ds.D09itug = 'R';
100500140617           kpjbu = FIDG09DS;
100600140617           fidg09r (kpjba);
100700140617           FIDG09DS = kpjbu;
100800140617           IF  fidg09ds.D09ocgi <> *blanks;
100900170419             V02tgi  = 'M';
101000140617             V02cgi  = fidg09ds.D09ocgi;
101100140617             V02dcgi = fidg09ds.D09odes;
101200140617           ENDIF;
101300141014           PosCurCGI = *on;
101400141014           exsr EmetteVideo;
101500140617           leavesr;
101600140617         ENDIF;
101700140617
101800140617       ENDSR;
101900160523
102000160523       //--------------------------------------------------------------
102100160523       //?Interrogazione Cappario.
102200160523       //--------------------------------------------------------------
102300160523       BEGSR  IntCappario;
102400160523
102500160523       //?Interrogo cappario quando ho i dati dell'indirizzo ma no
102600160523       //?Cap e Provincia
102700160523
102800160523       //?Mittente non codificato
102900160523         IF  V01cra1 = 0 and V01cra2 = 0 and
103000160523             V01rsr <> *blanks and V01inr <> *blanks and
103100160523             V01lor <> *blanks and V01car = *blanks and
103200160523             V01prr = *blanks;
103300160523           wRagSoc    = V01rsr;
103400160523           wCap       = V01car;
103500160523           wNazione   = V01nar;
103600160523           wIndirizzo = V01inr;
103700160523           wLocalita  = V01lor;
103800160523           wProvincia = V01prr;
103900160523           exsr call_FNLV14R;
104000160523           V01car = fnlv14ds.E14cap;
104100160523           V01lor = fnlv14ds.E14loc;
104200160523           V01prr = fnlv14ds.E14prv;
104300160523           V01nar = fnlv14ds.E14nar;
104400160523         //?Se errore o dato F12 quindi non ho caricato nessun cap e provincia
104500160523         //?demando il tutto ai pgm di controllo
104600160523           IF  (fnlv14ds.O14err <> *blanks and fnlv14ds.O14msg <> *blanks) or
104700160523               (V01car = *blanks and V01prr = *blanks);
104800160523             leavesr;
104900160523           ENDIF;
105000160523         //?Se arrivo qua tutto OK quindi riemetto il video
105100160523         //?e mi posiziono sui colli
105200160523           PosCurNCL = *on;
105300160523           exsr EmetteVideo;
105400160523         ENDIF;
105500160523
105600160523       //?Ordinante non codificato
105700160523         IF  V01cor1 = 0 and V01cor2 = 0 and
105800160523             V01rso <> *blanks and V01ino <> *blanks and
105900160523             V01loo <> *blanks and V01cao = *blanks and
106000160523             V01pro = *blanks;
106100160523           wRagSoc    = V01rso;
106200160523           wCap       = V01cao;
106300160523           wNazione   = V01nao;
106400160523           wIndirizzo = V01ino;
106500160523           wLocalita  = V01loo;
106600160523           wProvincia = V01pro;
106700160523           exsr call_FNLV14R;
106800160523           V01cao = fnlv14ds.E14cap;
106900160523           V01loo = fnlv14ds.E14loc;
107000160607           savloo = fnlv14ds.E14loc;
107100160523           V01pro = fnlv14ds.E14prv;
107200160523           V01nao = fnlv14ds.E14nar;
107300160523           IF  fnlv14ds.O14err <> *blanks and fnlv14ds.O14msg <> *blanks;
107400160523             leavesr;
107500160523           ENDIF;
107600160523           exsr EmetteVideo;
107700160523         ENDIF;
107800160523
107900160523       //?Destinatario non codificato
108000160523         IF  V01crc1 = 0 and V01crc2 = 0 and
108100160523             V01rsc <> *blanks and V01inc <> *blanks and
108200160523             V01loc <> *blanks and V01cac = *blanks and
108300160523             V01prc = *blanks;
108400160523           wRagSoc    = V01rsc;
108500160523           wCap       = V01cac;
108600160523           wNazione   = V01nac;
108700160523           wIndirizzo = V01inc;
108800160523           wLocalita  = V01loc;
108900160523           wProvincia = V01prc;
109000160523           exsr call_FNLV14R;
109100160523           V01cac = fnlv14ds.E14cap;
109200160523           V01loc = fnlv14ds.E14loc;
109300160607           savloc = fnlv14ds.E14loc;
109400160523           V01prc = fnlv14ds.E14prv;
109500160523           V01nac = fnlv14ds.E14nar;
109600160523           IF  fnlv14ds.O14err <> *blanks and fnlv14ds.O14msg <> *blanks;
109700160523             leavesr;
109800160523           ENDIF;
109900160523         ENDIF;
110000160523
110100160523       ENDSR;
110200160523
110300160523       //--------------------------------------------------------------
110400160523       //?Richiamo Interrogazione cappario.
110500160523       //--------------------------------------------------------------
110600160523       BEGSR  call_FNLV14R;
110700160523
110800160523         clear FNLV14DS;
110900160523         fnlv14ds.I14dri = dataoggi;
111000160523         fnlv14ds.I14rsc = wRagSoc;
111100160523         fnlv14ds.I14ind = wIndirizzo;
111200160523         fnlv14ds.E14cap = wCap;
111300160523         fnlv14ds.E14loc = wLocalita;
111400160523         fnlv14ds.E14prv = wProvincia;
111500160523         fnlv14ds.E14nar = wNazione;
111600160523
111700160523         fnlv14r (kpjba:fnlv14ds);
111800160523
111900160523       ENDSR;
112000160615
112100160615       //--------------------------------------------------------------
112200160621       //?F11 - Richiamo pgm calcolo Volume.
112300160615       //--------------------------------------------------------------
112400160621       BEGSR  TastoF11;
112500160615
112600160615         clear UL36volume;
112700160615         trul36r (UL36volume);
112800160615         V01vlm += UL36volume;
112900160615
113000160615       ENDSR;
113100160621
113200160621       //--------------------------------------------------------------
113300160621       //?F14 - Richiamo pgm calcolo Orari Servizio.
113400160621       //--------------------------------------------------------------
113500160621       BEGSR  TastoF14;
113600160621
113700160621         clear wData;
113800170403         IF  fior000i.dpm <> 0;
113900170403           wAnno = %dec(%subst(%editc(fior000i.dpm:'X'):5:4):4:0);
114000170403           wMese = %dec(%subst(%editc(fior000i.dpm:'X'):3:2):2:0);
114100170403           wGiorno = %dec(%subst(%editc(fior000i.dpm:'X'):1:2):2:0);
114200160621         ELSE;
114300160621           wAnno = %dec(%subst(%editc(fior000i.dao:'X'):5:4):4:0);
114400160621           wMese = %dec(%subst(%editc(fior000i.dao:'X'):3:2):2:0);
114500160621           wGiorno = %dec(%subst(%editc(fior000i.dao:'X'):1:2):2:0);
114600160621         ENDIF;
114700160621
114800160621         clear trulorsds;
114900160621         trulorsds.IOREfil = %dec(fior000i.por:3:0);
115000160621         trulorsds.IOREcap = fior000i.car;
115100160621         trulorsds.IOREloc = fior000i.lor;
115200160621         trulorsds.IOREnar = fior000i.nar;
115300160621         trulorsds.IOREdta = wData;
115400160621         trulorsds.IOREtser = 'R';
115500160621         trulorsds.IOREgf2 = fior000i.pickupcalc;
115600160621         trulorsds.IOREorp = fior000i.orapresrit;
115700160621
115800160621         trulorsr (kpjba:trulorsds);
115900160621
116000160621       ENDSR;
116100140617
116200140617       //--------------------------------------------------------------
116300140617       //?Sposto i dati dal video alla DS.
116400140617       //--------------------------------------------------------------
116500141008       BEGSR  MoveVideoDS;
116600140617
116700140618         reset fior000o;
116800140617
116900140618         fior000o.dao      = V01dao;
117000140618         fior000o.oao      = V01oao;
117100140617
117200140618         fior000o.tor      = V01tor;
117300140618         fior000o.tco      = V01tco;
117400140617
117500140618         fior000o.cra1     = V01cra1;
117600140618         fior000o.cra2     = V01cra2;
117700140618         fior000o.cra3     = V01cra3;
117800140618         fior000o.rsr      = V01rsr;
117900140618         fior000o.inr      = V01inr;
118000140618         fior000o.lor      = V01lor;
118100140618         fior000o.prr      = V01prr;
118200140618         fior000o.car      = V01car;
118300140618         fior000o.nar      = V01nar;
118400140618         fior000o.rer      = V01rer;
118500140618         fior000o.ter      = V01ter;
118600140617
118700140618         fior000o.dalleam  = V01dalleam;
118800140618         fior000o.alleam   = V01alleam;
118900140618         fior000o.dallepm  = V01dallepm;
119000140618         fior000o.allepm   = V01allepm;
119100140618         fior000o.nam      = V01nam;
119200140618         fior000o.rfa      = V01rfa;
119300140618         fior000o.nota1    = V01nota1;
119400140618         fior000o.nota2    = V01nota2;
119500140617
119600140618         fior000o.orr      = V01orr;
119700170403         fior000o.dpm      = V01dpm;
119800140618         fior000o.dar      = V01dar;
119900140617
120000140618         fior000o.ntwdpd   = V01ntwd;
120100141020         fior000o.por      = V01por;
120200140617
120300140618         fior000o.ncl      = V01ncl;
120400140618         fior000o.pkg      = V01pkg;
120500140618         fior000o.bnc      = V01bnc;
120600140618         fior000o.vlm      = V01vlm;
120700140618         fior000o.spi      = V01spi;
120800140617
120900140618         fior000o.pag      = V01pag;
121000140618         fior000o.ksc      = V01ksc;
121100140618         fior000o.ctr      = V01ctr;
121200140617
121300140618         fior000o.comm     = V01comm;
121400140617
121500140618         fior000o.crc1     = V01crc1;
121600140618         fior000o.crc2     = V01crc2;
121700140618         fior000o.crc3     = V01crc3;
121800140618         fior000o.rsc      = V01rsc;
121900140618         fior000o.inc      = V01inc;
122000160607         fior000o.loc      = savloc;
122100140618         fior000o.prc      = V01prc;
122200140618         fior000o.cac      = V01cac;
122300140618         fior000o.nac      = V01nac;
122400141020         fior000o.poc      = V01poc;
122500140617
122600140618         fior000o.cor1     = V01cor1;
122700140618         fior000o.cor2     = V01cor2;
122800140618         fior000o.cor3     = V01cor3;
122900140618         fior000o.rso      = V01rso;
123000140618         fior000o.ino      = V01ino;
123100160607         fior000o.loo      = savloo;
123200140618         fior000o.pro      = V01pro;
123300140618         fior000o.cao      = V01cao;
123400140618         fior000o.nao      = V01nao;
123500140617
123600140618         fior000o.blc      = V02blc;
123700140618         fior000o.att      = V02att;
123800140618         fior000o.mtc      = V02mtc;
123900140618         fior000o.sto      = V02sto;
124000170419         fior000o.tgi      = V02tgi;
124100140618         fior000o.cgi      = V02cgi;
124200170419       //?se il giro è stato variato
124300170419         IF  fior000i.cgi <> fior000o.cgi and
124400170419             fior000i.cgi <> *blanks;
124500170419         //?imposto tipo giro 'M' se è stato impostato un nuovo giro
124600170419           IF  fior000o.cgi <> *blanks;
124700170419             fior000o.tgi = 'M';
124800170419           ENDIF;
124900170419         //?pulisco il tipo giro se non è stato impostato un nuovo giro
125000170419           IF  fior000o.cgi = *blanks;
125100170419             clear fior000o.tgi;
125200170419           ENDIF;
125300170419         ENDIF;
125400160404         IF  V02ffd = *blanks;
125500160404           fior000o.ffd    = 'N';
125600160404         ELSE;
125700160404           fior000o.ffd    = V02ffd;
125800160404         ENDIF;
125900160608
126000160608         fior000o.tipoord = tipoord;
126100140617
126200140618         fior000o.mail     = V03mail;
126300140618         fior000o.sms      = V03sms;
126400160613
126500160613         fior000o.mailconf = V04mailcon;
126600160613         fior000o.smsconf  = V04smsconf;
126700140619
126800140619       //?Imposto il tasto premuto dall'utente
126900140619         fior000a.TastoVideo = dsp_aid;
127000160622
127100160622       //?Imposto SEMPRE in positivo le quanatità
127200160622         IF  fior000o.ncl < 0;
127300160622           fior000o.ncl = fior000o.ncl * -1;
127400160622         ENDIF;
127500160622         IF  fior000o.pkg < 0;
127600160622           fior000o.pkg = fior000o.pkg * -1;
127700160622         ENDIF;
127800160622         IF  fior000o.vlm < 0;
127900160622           fior000o.vlm = fior000o.vlm * -1;
128000160622         ENDIF;
128100160622         IF  fior000o.bnc < 0;
128200160622           fior000o.bnc = fior000o.bnc * -1;
128300160622         ENDIF;
128400160622         IF  fior000o.blc < 0;
128500160622           fior000o.blc = fior000o.blc * -1;
128600160622         ENDIF;
128700160622         IF  fior000o.mtc < 0;
128800160622           fior000o.mtc = fior000o.mtc * -1;
128900160622         ENDIF;
129000160622         IF  fior000o.att < 0;
129100160622           fior000o.att = fior000o.att * -1;
129200160622         ENDIF;
129300140617
129400140617       ENDSR;
129500140414
129600140414       //--------------------------------------------------------------
129700140414       //?Operazioni finali.
129800140414       //--------------------------------------------------------------
129900140414       BEGSR  RoutEnd;
130000140414
130100140618         %subst(prmRpyData:1:prmRpySize) = fior000o;
130200140618         %subst(prmRpyTastiFun:1:prmRpySizeTastiFun) = fior000a;
130300141013         %subst(prmRpyForzatu:1:prmRpySizeForz) = fiora0f0;
130400140617
130500140414         return;
130600140414
130700140414       ENDSR;
130800140414
130900140414       //--------------------------------------------------------------
131000140415      /end-free

000100131007       //==============================================================
000200131007       //
000300150504       //?tisi795r - Tempi di consegna  da Internet
000400131007       //
000500131007       //==============================================================
000600131009
000700131009       //--------------------------------------------------------------
000800131009       //?Specifiche di controllo.                                     ?
000900131009       //--------------------------------------------------------------
001000150518     H dftactgrp(*no) actgrp(*caller) BNDDIR('TIS':'UBBNDDIR':'TIBS')
001100131009
001200131009       //--------------------------------------------------------------
001300131009       //?Dichiarazione file.                                          ?
001400131009       //--------------------------------------------------------------
001500131010
001600131010       // -?File organigramma
001700131010     fAZORG01L  if   e           k disk
001800131009
001900131009       // -?File Tabelle
002000131010     fTNTBE01L  if   e           k disk
002100131008
002200131007       //--------------------------------------------------------------
002300131007       //?Definizione costanti.                                        ?
002400131007       //--------------------------------------------------------------
002500150504      /copy gaitrasrc/srcconst,TISI795R
002600140520
002700140520     D digits          c                   '0123456789'
002800140617
002900140617     D MSGID_LUNEDI...
003000140617     D                 C                   'TIS0351'
003100140617     D MSGID_MARTEDI...
003200140617     D                 C                   'TIS0354'
003300140617     D MSGID_MERCOLEDI...
003400140617     D                 C                   'TIS0356'
003500140617     D MSGID_GIOVEDI...
003600140617     D                 C                   'TIS0203'
003700140617     D MSGID_VENERDI...
003800140617     D                 C                   'TIS0607'
003900140617     D MSGID_SABATO...
004000140617     D                 C                   'TIS0533'
004100140617     D MSGID_DOMENICA...
004200140617     D                 C                   'TIS0159'
004300140114
004400131007       //--------------------------------------------------------------
004500131007       //?Definizione strutture dati.                                  ?
004600131007       //--------------------------------------------------------------
004700131007       // -?Dati INPUT
004800150504     d tisi795i0     e ds                  QUALIFIED INZ(*EXTDFT)
004900131007
005000131007       // -?Dati OUTPUT
005100150504     d tisi795o0     e ds                  QUALIFIED INZ(*EXTDFT)
005200150522     D  ktspSN                        1a   dim(18) overlay(TSP_DISPO)
005300150522     d  Ktsp                          1a   dim(18) overlay(TSP_COD)
005400150522     d  KtspD                        25a   dim(18) overlay(TSP_DESCR)
005500150522     d  KtspNT                      100a   dim(18) overlay(TSP_NOT)
005600131007
005700131007       // -?Messaggi
005800150504     d tisi795m0     e ds                  QUALIFIED INZ(*EXTDFT)
005900140522     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
006000140522     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
006100140522     d  skErrWarn                     1a   dim(99) overlay(ErrWarn)
006200140522     d  skTextMsg                   255a   dim(99) overlay(TextMsg)
006300131007
006400131007       // -?Forzature
006500150504     d tisi795f0     e ds                  QUALIFIED INZ(*EXTDFT)
006600140522     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
006700140522     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
006800140522     d  skGiaForza                    1a   dim(99) overlay(GiaForzato)
006900150514
007000150514     D tisi95Ds_partenza...
007100150514     D                 DS                  LIKEDS(tisi95Ds)
007200150514     D                                     INZ
007300150518     D tisi95Ds_arrivo...
007400150514     D                 DS                  LIKEDS(tisi95Ds)
007500150514     D                                     INZ
007600140604
007700150518     D tisi95ds      E DS                  BASED(nullptr)
007800140522     D tnsd99ds      e ds
007900150515     D trulorsds     E DS                  INZ
008000150515     D trulor2ds     E DS                  INZ
008100150515     D trulor3ds     E DS                  INZ
008200150518     D fnlv55ds      E DS                  QUALIFIED INZ
008300150522     D Kpjba         E DS                  inz
008400150515     D tabel00f      E DS                  QUALIFIED INZ
008500150515     D azorg00f      E DS                  QUALIFIED INZ
008600150515     D ds5E          E DS                  QUALIFIED INZ
008700150518     d dvpooraris    e ds
008800150518     d tibs02ds      e ds
008900150526     d xgiolavds     e ds
009000150729     d diore01       e ds
009100150515     d
009200150515      * scomposizione ora generica
009300150515     d dsora           DS                  INZ                                  *ora
009400150515     d  dshh                          2  0                                       -ore
009500150515     d  dsmm                          2  0                                       -minuti
009600150515      * composizione ora generica editata
009700150515     d dsorae          DS                  INZ                                  *ora
009800150515     d  dshhe                         2  0                                       -ore
009900150515     d  dsvo1                         1                                          -':'
010000150515     d  dsmme                         2  0                                       -minuti
010100150515      * scomposizione data generica
010200150515     d dsdat           DS                  INZ                                  *data
010300150515     d  dsann                         4  0                                       -anno
010400150515     d  dsmes                         2  0                                       -mese
010500150515     d  dsgio                         2  0                                       -giorno
010600150515      * composizione data generica editata
010700150515     d dsdate          DS                  INZ                                  *data
010800150515     d  dsgioe                        2  0                                       -giorno
010900150515     d  dsvd1                         1                                          -'/'
011000150515     d  dsmese                        2  0                                       -mese
011100150515     d  dsvd2                         1                                          -'/'
011200150515     d  dsanne                        4  0                                       -anno
011300140521
011400140528     D WDAT8           DS
011500140528     D  DATADA                 1      8  0
011600140528     D  DATAA                  9     16  0
011700140528     D  GGL                   17     21  0
011800150515     d
011900140617     D dayOfWeek       DS                  QUALIFIED
012000140617     D                                7A   INZ(MSGID_LUNEDI)
012100140617     D                                7A   INZ(MSGID_MARTEDI)
012200140617     D                                7A   INZ(MSGID_MERCOLEDI)
012300140617     D                                7A   INZ(MSGID_GIOVEDI)
012400140617     D                                7A   INZ(MSGID_VENERDI)
012500140617     D                                7A   INZ(MSGID_SABATO)
012600140617     D                                7A   INZ(MSGID_DOMENICA)
012700140617     D  msgId                         7A   DIM(7) OVERLAY(dayOfWeek)
012800140312
012900131009       //--------------------------------------------------------------
013000131009       //?Definizione schiere.
013100131009       //--------------------------------------------------------------
013200140210
013300140210     d skFIdMsg        s              7a   dim(99)
013400140210     d skFIdCampo      s             10a   dim(99)
013500140210     d skFGiaForza     s              1a   dim(99)
013600131007
013700131007       //--------------------------------------------------------------
013800131007       //?Definizione campi.
013900131007       //--------------------------------------------------------------
014000140527     d wIdMsg          s              7a
014100140527     d wIdCampo        s             10a
014200140527     d wcampo          s             10a
014300150514     d wIdlingua       s                   like(tisi795i0.lingua)
014400150514     D LnaPartenza     S                   LIKE(o95lna)
014500150514     D LnaArrivo       S                   LIKE(o95lna)
014600150514     D Tfp             S                   LIKE(fnlv55ds.d55tfp)
014700150518     d wksu            s                   like(tisi795i0.ksu)
014800150529     d wsun            s                   like(tisi795i0.sun)
014900150514     D MailPartenza    S            255A   VARYING
015000150514     D MailArrivo      S            255A   VARYING
015100150514     D LinguaBartTab   S              1P 0
015200150629     D LinguaBartTbe   S              1
015300150515     D TempiResaCli    S              3P 0
015400150515     D GgResaCli       S              1S 0
015500150515     D OreResaCli      S              2S 0
015600150515     D GgResaCliAlfa   S              1A
015700150515     D OreResaCliAlfa  S              5A
015800150515     d wora            S              4  0                                      *ora  (hhmm)
015900150515     d worae           S              5                                         *ora  (hh:mm)
016000150515     d wdat            S              8  0                                      *data (aaaammgg)
016100150515     d wdate           S             10                                         *data (gg/mm/aaaa)
016200150515     d oraDAL          S              5                                         *ora  (hh:mm)
016300150515     d ora_AL          S              5                                         *ora  (hh:mm)
016400150518     d dataoggi        s              8s 0
016500150529     d Wdatarit        s              8s 0
016600150518     d annocur         s              4s 0
016700150518     d wParm           s            512a   inz
016800150518     d wTextMsg        s            255
016900150518     d Capnum          S              5  0                                      *data (aaaammgg)
017000150601     d wdesgio         s             20a
017100150601     d dw              s              1s 0
017200150529     d Datasys         s               d   datfmt(*iso)  inz(*SYS)
017300150529     d dataiso         s               d   datfmt(*iso)
017400150514     D skTabelKey      S                   LIKE(tabel00f.tblkey)
017500150514     D                                     DIM(18)
017600150514     D skTabelUni      S                   LIKE(tabel00f.tbluni)
017700150514     D                                     DIM(18)
017800150514     D jTabel          S              2S 0
017900150514     D CurrTipoServ    S                   LIKE(tabel00f.tblkey)
018000131008
018100131008       //--------------------------------------------------------------
018200131008       //?Definizione procedure.
018300131008       //--------------------------------------------------------------
018400140603     d tibs02r         pr                  extpgm('TIBS02R')
018500140604     d  kpjba                              likeds(kpjba)
018600140603     d  tibs02ds                           likeds(tibs02ds)
018700150526     D xgiolav         pr                  extpgm('XGIOLAV')
018800150526     D  xgiolavds                          likeds(xgiolavds)
018900150629     D*--------------------------------------------------
019000150629     D* Procedure name: GetFilUrl
019100150629     D* Purpose:        Restituisce l'URL della filiale.
019200150629     D* Returns:        URL filiale.
019300150629     D* Parameter:      piFiliale => ID filiale
019400150629     D*--------------------------------------------------
019500150629     D GetFilUrl       PR           256A
019600150629     D  piFiliale                     3P 0 CONST
019700150629
019800131008
019900131008       //--------------------------------------------------------------
020000131008       //?Definizione prototipi.
020100131008       //--------------------------------------------------------------
020200150518      /COPY GAITRASRC/SRCPROTOPR,TIBSORG00
020300150518      /COPY GAITRASRC/SRCPROTOPR,TIBS0800R
020400150504      /copy gaitrasrc/srcprotopr,tisi795r
020500140522      /copy gaitrasrc/srcprotopr,tnsd99r
020600150515      /copy gaitrasrc/srcprotopr,trulorsr
020700131009      /copy gaitrasrc/srcprotopr,RTVMSGLANG
020800140528      /copy gaitrasrc/srcprotopr,xsrlav8
020900140603      /copy gaitrasrc/srcprotopr,fnlv55r
021000150514      /COPY GAITRASRC/SRCPROTOPR,TISI95R1
021100150514      /COPY GAITRASRC/SRCPROTOPI,TISI95R1
021200131219
021300131219       //--------------------------------------------------------------
021400131219       //?Definizione parametri programma.
021500131219       //--------------------------------------------------------------
021600150504     D Tisi795_GetTempi2...
021700140519     D                 PI
021800140519     D prmRqsOpCode...
021900140519     D                               10I 0 CONST
022000140519     D prmRpyOpCode...
022100140519     D                               10I 0
022200140519     D prmRpyIdMsg...
022300140519     D                               10I 0
022400140519     D prmRqsFormato...
022500140519     D                               10A   CONST
022600140519     D prmRqsData...
022700140519     D                            32767A   OPTIONS(*VARSIZE)
022800140519     D prmRqsDataSize...
022900140519     D                               10I 0 CONST
023000140519     D prmRpyFormato...
023100140520     D                               10A   CONST
023200140519     D prmRpyData...
023300140519     D                            32767A   OPTIONS(*VARSIZE)
023400140519     D prmRpyDataSize...
023500140519     D                               10I 0 CONST
023600140519     D prmRpyFormMsg...
023700140519     D                               10A   CONST OPTIONS(*NOPASS)
023800140519     D prmRpyMessage...
023900140519     D                            32767A   OPTIONS(*VARSIZE : *NOPASS)
024000140519     D prmRpyMsgSize...
024100140519     D                               10I 0 CONST OPTIONS(*NOPASS)
024200140519     D prmRpyFormForz...
024300140519     D                               10A   CONST OPTIONS(*NOPASS)
024400140519     D prmRpyForzatu...
024500140519     D                            32767A   OPTIONS(*VARSIZE : *NOPASS)
024600140519     D prmRpyForSize...
024700140519     D                               10I 0 CONST OPTIONS(*NOPASS)
024800131007
024900131007       //--------------------------------------------------------------
025000131007       //?MAIN.
025100131007       //--------------------------------------------------------------
025200131008
025300131007      /free
025400131008
025500131008       //?Operazioni iniziali
025600131008       exsr RoutInz;
025700131008
025800140523       //?Controllo formale dei dati di accesso
025900131008       exsr Controlla;
026000140529
026100150514       //?Calcola i tempi di consegna con gli orari
026200150514       exsr Calcola ;
026300131010
026400131010       //?Operazioni finali
026500131010       exsr RoutEnd;
026600131008
026700131008       //--------------------------------------------------------------
026800131008       //?Operazioni iniziali.                                         ?
026900131008       //--------------------------------------------------------------
027000131008       BEGSR  RoutInz;
027100150522         knsif='GAITRA201'    ;
027200140211
027300140211         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
027400131008
027500150504         prmRpyOpCode = TISI795_RPYOPCODE_DONE;
027600150504         prmRpyIdMsg  = TISI795_ESITO_OK;
027700131009
027800131009       //?Data del giorno
027900131009         dataoggi = %dec(%date());
028000140604         annocur  = %subdt(%date():*years);
028100131009
028200131009       //?Pulisco ds di output
028300150504         reset TISI795o0;
028400150504         reset TISI795m0;
028500140528
028600140924       // Reperimento dati tabella VPO - ORARISER
028700140924       clear tibs02ds;
028800140925       clear dvpooraris;
028900140924       t02mod='C';
029000140924       t02cod='VPO';
029100140925       t02ke1='ORARISER';
029200140924       t02tla='L  ';
029300140924       tibs02r(kpjba:tibs02ds);
029400140924       if t02err=*blanks;
029500140924          dvpooraris=t02uni;
029600140924       endif;
029700131008
029800150514         Organigramma_init();
029900150514
030000131008         *inLR = *on;
030100131008
030200131008       ENDSR;
030300131008
030400131008       //--------------------------------------------------------------
030500131008       //?Controllo formale dei dati passati.
030600131008       //--------------------------------------------------------------
030700131008       BEGSR  Controlla;
030800131008
030900140519         clear wksu;
031000150528         clear wsun;
031100140527         clear widlingua;
031200140519
031300131008       //?OpCode
031400150514         IF  prmRqsOpCode <> TISI795_RQSOPCODE_GET_TEMPI2    ;
031500150504           prmRpyOpCode = TISI795_RPYOPCODE_ERROR;
031600150504           prmRpyIdMsg  = TISI795_ESITO_RQSOPCODE_SCONOSCIUTO;
031700140210           exsr RoutEnd;
031800131008         ENDIF;
031900140408
032000140408       //?Formato e size RQS
032100150504           IF  prmRqsFormato = TISI795I0.formato;
032200150504             TISI795I0 = %SUBST(prmRqsData:1:prmRqsDataSize);
032300150504             wksu=TISI795i0.ksu;
032400150528             wsun=TISI795i0.sun;
032500140408           ELSE;
032600150504             prmRpyOpCode = TISI795_RPYOPCODE_ERROR;
032700150504             prmRpyIdMsg = TISI795_ESITO_NOME_FORMATO_SCONOSCIUTO;
032800140408             exsr RoutEnd;
032900140408           ENDIF;
033000150514
033100140408           IF  prmRqsDataSize > 0;
033200140408           ELSE;
033300150504             prmRpyOpCode = TISI795_RPYOPCODE_ERROR;
033400150504             prmRpyIdMsg = TISI795_ESITO_SIZE_DATA_ERRATO;
033500140408             exsr RoutEnd;
033600140408           ENDIF;
033700140408         //?Formato e size RPY
033800150504           IF  prmRpyFormato = TISI795O0.formato;
033900140408           ELSE;
034000150504             prmRpyOpCode = TISI795_RPYOPCODE_ERROR;
034100150504             prmRpyIdMsg = TISI795_ESITO_NOME_FORMATO_SCONOSCIUTO;
034200140408             exsr RoutEnd;
034300140408           ENDIF;
034400140408           IF  prmRpyDataSize > 0;
034500140408           ELSE;
034600150504             prmRpyOpCode = TISI795_RPYOPCODE_ERROR;
034700150504             prmRpyIdMsg = TISI795_ESITO_SIZE_DATA_ERRATO;
034800140408             exsr RoutEnd;
034900140408           ENDIF;
035000140210
035100140522       //?Formato e size MSG
035200150504         IF  prmRpyFormMsg = TISI795M0.formato;
035300140522         ELSE;
035400150504           prmRpyOpCode = TISI795_RPYOPCODE_ERROR;
035500150504           prmRpyIdMsg = TISI795_ESITO_NOME_FORMATO_SCONOSCIUTO;
035600140522           exsr RoutEnd;
035700140522         ENDIF;
035800140522         IF  prmRpyMsgSize > 0;
035900140522         ELSE;
036000150504           prmRpyOpCode = TISI795_RPYOPCODE_ERROR;
036100150504           prmRpyIdMsg = TISI795_ESITO_SIZE_DATA_ERRATO;
036200140522           exsr RoutEnd;
036300140522         ENDIF;
036400140624
036500140624         //?Formato e size RPY
036600150514       //?Formato e size MSG
036700150504         IF  prmRpyFormMsg = TISI795M0.formato;
036800140624         ELSE;
036900150504           prmRpyOpCode = TISI795_RPYOPCODE_ERROR;
037000150504           prmRpyIdMsg = TISI795_ESITO_NOME_FORMATO_SCONOSCIUTO;
037100140624           exsr RoutEnd;
037200140624         ENDIF;
037300140624         IF  prmRpyMsgSize > 0;
037400140624         ELSE;
037500150504           prmRpyOpCode = TISI795_RPYOPCODE_ERROR;
037600150504           prmRpyIdMsg = TISI795_ESITO_SIZE_DATA_ERRATO;
037700140624           exsr RoutEnd;
037800140624         ENDIF;
037900150514
038000150514          //?Conversione codice lingua da ISO2 a "formato Bartolini"
038100150514
038200150514          clear LinguaBartTab;
038300150514          cvtLinguaISO2ToTabel( tisi795i0.lingua                     :
038400150514                                *omit                                :
038500150514                                *omit                                :
038600150514                                LinguaBartTab                       );
038700150514
038800150514          if LinguaBartTab = *zeros;
038900150514           prmRpyOpCode = TISI795_RPYOPCODE_ERROR;
039000150514           prmRpyIdMsg = TISI795_ESITO_CODICE_LINGUA_NONREPERITO;
039100150514           exsr RoutEnd;
039200150514          ENDIF  ;
039300150629
039400150629          clear LinguaBartTbe;
039500150629          cvtLinguaISO2ToTntbe( tisi795i0.lingua                     :
039600150629                                *omit                                :
039700150629                                *omit                                :
039800150629                                LinguaBartTbe                       );
039900150514
040000150518             widlingua=TISI795i0.lingua             ;
040100150514
040200150514          //?CARICAMENTO TIPI servizio in lingua
040300150514             exsr RTVTIPISERVIZI;
040400150514             if skTabelKey(1) = *blanks;
040500150514           prmRpyOpCode = TISI795_RPYOPCODE_ERROR;
040600150514           prmRpyIdMsg = TISI795_ESITO_TIPISERVIZIO_NONREPERITI;
040700150514           exsr RoutEnd;
040800150514          ENDIF  ;
040900150529
041000150529          //?DATA RITIRO: se sbagliata o vuota imposto data del giorno
041100150529         monitor  ;
041200150529         dataiso=%date(tisi795i0.data_rit:  *eur)   ;
041300150529         on-error ;
041400150529         dataiso  = datasys      ;
041500150529         endmon  ;
041600150601
041700150601           EXEC SQL SET :dw = DAYOFWEEK_ISO(:dataiso);
041800150601           wdesgio=rtvMsgLang(dayOfWeek.msgId(dw) : widlingua);
041900150601           // giorno della settimana della data ritiro
042000150601          Tisi795O0.day_rit = wdesgio     ;
042100150601
042200150529
042300150529         WDATARIT=%DEC(DATAISO)  ;
042400140522
042500131008       ENDSR;
042600150514       //--------------------------------------------------------------
042700150514       //?Calcolta tempi
042800150514       //--------------------------------------------------------------
042900150514       BEGSR  Calcola  ;
043000150514          //?Partenza.
043100150514
043200150522       clear tisi95Ds_partenza;
043300150522       tisi95Ds_partenza.i95Tcn = '7';
043400150518       tisi95Ds_partenza.i95Cap = tisi795I0.cap_p      ;
043500150518       tisi95Ds_partenza.i95Loc = Tisi795I0.loc_p      ;
043600150518       tisi95Ds_partenza.i95Prv = Tisi795I0.PRV_P      ;
043700150529       tisi95Ds_partenza.i95DAT = WDATARIT             ;
043800150514
043900150514       CLEAR tisi95r1ds;
044000150514       tisi95r1ds.tisi95ds  = tisi95Ds_partenza;
044100150514       tisi95r1ds.si95ilang = widlingua       ;
044200150514
044300150514       ChkCapLocalitaLang(tisi95r1ds);
044400150514       tisi95Ds_partenza = tisi95r1ds.tisi95ds;
044500150514       tisi95Ds_partenza.o95msg = tisi95r1ds.si95odmsg;
044600150514       if tisi95Ds_partenza.o95Lia >= '2' AND
044700150514          tisi95Ds_partenza.o95Lid >= '3';
044800150514       else;
044900150514          tisi95Ds_partenza.o95Err = '*';
045000150514       endif;
045100150514
045200150514          //?Arrivo.
045300150514
045400150522       clear tisi95Ds_arrivo;
045500150522       tisi95Ds_arrivo.i95Tcn = '7';
045600150518       tisi95Ds_arrivo.i95Cap = tisi795I0.cap_A      ;
045700150518       tisi95Ds_arrivo.i95Loc = Tisi795I0.loc_A      ;
045800150518       tisi95Ds_arrivo.i95Prv = Tisi795I0.PRV_A      ;
045900150518       tisi95Ds_arrivo.i95lkg = Tisi795I0.pesokg   ;
046000150518       tisi95Ds_arrivo.i95lmc = Tisi795I0.vol_mc   ;
046100150518       tisi95Ds_arrivo.i95ffd = Tisi795I0.cons_fd  ;
046200150529       tisi95Ds_arrivo.i95DAT = WDATARIT             ;
046300150514
046400150514       CLEAR tisi95r1ds;
046500150514       tisi95r1ds.tisi95ds  = tisi95Ds_arrivo;
046600150514       tisi95r1ds.si95ilang = widlingua      ;
046700150514
046800150514       ChkCapLocalitaLang(tisi95r1ds);
046900150514       tisi95Ds_arrivo = tisi95r1ds.tisi95ds;
047000150514       tisi95Ds_arrivo.o95msg = tisi95r1ds.si95odmsg;
047100150514       if tisi95Ds_arrivo.o95Lia >= '2' AND
047200150514          tisi95Ds_arrivo.o95Lid >= '3';
047300150514       else;
047400150514          tisi95Ds_arrivo.o95Err = '*';
047500150514       endif;
047600150514
047700150514          //?Errori.
047800150514
047900150514       IF tisi95Ds_partenza.o95Err <> *BLANK
048000150514       OR tisi95Ds_arrivo.o95Err   <> *BLANK;
048100150518         prmRpyOpCode = tisi795_RPYOPCODE_ERROR;
048200150514         IF tisi95Ds_partenza.o95Err <> *BLANK;
048300150514
048400150518           clear widmsg ;
048500150518           wtextmsg= %TRIMR(tisi95Ds_partenza.o95Msg);
048600150630           widcampo= 'CAP_P'                      ;
048700150514              clear wParm;
048800150514              exsr Messaggi;
048900150514              exsr RoutEnd;
049000150514         ENDIF;
049100150514         IF tisi95Ds_arrivo.o95Err <> *BLANK;
049200150518           clear widmsg ;
049300150518           wtextmsg= %TRIMR(tisi95Ds_arrivo.o95Msg);
049400150630           widcampo= 'CAP_A'                      ;
049500150514              clear wParm;
049600150514              exsr Messaggi;
049700150514              exsr RoutEnd;
049800150514         ENDIF;
049900150514       ENDIF;
050000150514
050100150514
050200150518       if prmRpyOpCode <> tisi795_RPYOPCODE_ERROR;
050300150514
050400150514          //?Stabilisco la filiale di PARTENZA
050500150514
050600150514          if tisi95Ds_partenza.o95liv = 'C';
050700150514             LnaPartenza = tisi95Ds_partenza.o95cla;
050800150514          else;
050900150514             LnaPartenza = tisi95Ds_partenza.o95lla;
051000150514          endif;
051100150514
051200150514
051300150514          //?Reperisco il terminal della filiale partenza
051400150514
051500150514          CLEAR fnlv55ds;
051600150514          fnlv55ds.d55tpt = 'P';
051700150514          fnlv55ds.d55lin = LnaPartenza;
051800150514          fnlv55ds.d55drf = %dec(%date():*ISO);
051900150514          fnlv55r(fnlv55ds);
052000150514          if fnlv55ds.d55err <> *blanks;
052100150514             Tfp = LnaPartenza;
052200150514          else;
052300150514             Tfp = fnlv55ds.d55tfp;
052400150514          endif;
052500150514
052600150514
052700150514          //?Stabilisco la filiale di arrivo
052800150514
052900150526          //if tisi95Ds_arrivo.o95liv = 'C';
053000150526          //   LnaArrivo = tisi95Ds_arrivo.o95cla;
053100150526          //else;
053200150526          //   LnaArrivo = tisi95Ds_arrivo.o95lla;
053300150526          //endif;
053400150526
053500150526             LnaArrivo = tisi95Ds_arrivo.o95lna;
053600150514
053700150514          //?Reperisco attributi filiale di partenza
053800150514
053900150518          Tisi795O0.FIL_FP = LnaPartenza;
054000150518          clear Capnum   ;
054100150514
054200150514          Organigramma_getDescrizioneFiliale( LnaPartenza                      :
054300150518                                          tisi795O0.DES_FP);
054400150514
054500150514          Organigramma_getIndirizzoFiliale( LnaPartenza                        :
054600150518                                          tisi795O0.IND_FP                     :
054700150518                                          capnum                               :
054800150518                                          tisi795O0.LOC_FP                     :
054900150518                                          tisi795O0.PRV_FP)                    ;
055000150514
055100150514          Organigramma_getRecapitiFiliale( LnaPartenza                         :
055200150518                                          tisi795O0.TEL_FP                     :
055300150518                                          tisi795O0.FAX_FP                     :
055400150514                                          *omit                                :
055500150514                                          MailPartenza                       );
055600150629          tisi795O0.mail_FP =MailPartenza                                     ;
055700150518          tisi795O0.cap_FP  =%editc(Capnum:'X')                               ;
055800150629          tisi795o0.url_FP  = GetFilUrl(lnaPartenza)                          ;
055900150518
056000150514          //?Reperisco attributi filiale di arrivo
056100150514
056200150629          Tisi795O0.FIL_FA = LnaArrivo  ;
056300150518          clear Capnum   ;
056400150514
056500150518          Organigramma_getDescrizioneFiliale( LnaArrivo                       :
056600150518                                          tisi795O0.DES_Fa)                    ;
056700150518
056800150518          Organigramma_getIndirizzoFiliale( LnaArrivo                         :
056900150518                                          tisi795O0.IND_FA                     :
057000150518                                          Capnum                               :
057100150518                                          tisi795O0.LOC_FA                     :
057200150518                                          tisi795O0.PRV_FA)                    ;
057300150514
057400150514          Organigramma_getRecapitiFiliale( LnaArrivo                           :
057500150518                                          tisi795O0.TEL_FA                     :
057600150518                                          tisi795O0.FAX_FA                     :
057700150514                                          *omit                                :
057800150514                                          MailArrivo                         );
057900150518          tisi795O0.mail_FA  =MailArrivo                                       ;
058000150518          tisi795O0.cap_FA  =%editc(Capnum:'X')                               ;
058100150629          tisi795o0.url_FA  = GetFilUrl(lnaArrivo)                            ;
058200150514
058300150514                //?Ciclo sulla schiera dei tipi servizio
058400150514                jTabel = *zeros;
058500150514                dow jTabel <= %elem(skTabelKey);
058600150514                    jTabel = jTabel + 1;
058700150514                    if skTabelKey(jTabel) = *blanks;
058800150514                       leave;
058900150514                    else;
059000150514                       CurrTipoServ = skTabelKey(jTabel);
059100150514                       ds5E         = skTabelUni(jTabel);
059200150515                       tisi795o0.TSP_TOT= tisi795o0.TSP_TOT +1 ;
059300150514
059400150522                       tisi795o0.ktsp(tisi795o0.TSP_TOT)= CurrTipoServ ;
059500150522                       tisi795o0.ktspd(tisi795o0.TSP_TOT)= ds5E.§5EDES;
059600150514
059700150514                       exsr RTVTEMPICONSEGNA;
059800150514                    endif;
059900150514                enddo;
060000150514             endif;
060100150526
060200150526        // Dati  di  RITIRO
060300150526        //  non li visualizzo se merce portata a magazzino
060400150526        if tisi795i0.merce_mag <>'S'  ;
060500150526             clear trulorsds    ;
060600150526             clear trulor2ds    ;
060700150526             clear trulor3ds    ;
060800150529              ioredta=wdatarit       ;
060900150526              ioretser='R'           ;
061000150526              iorefil =lnaPartenza   ;
061100150529              ioredta =wdatarit          ;
061200150526              iorecap=tisi795I0.cap_P     ;
061300150526              ioreloc=tisi795I0.loc_P     ;
061400150526              oor3loc_N=tisi795I0.loc_P   ;
061500150526              ioregf2=tisi95Ds_partenza.o95gf2  ;
061600150526              trulorsr (kpjba:trulorsds:trulor2ds:trulor3ds) ;
061700150526              wdat=ioredta  ;
061800150526              exsr edtdat   ;
061900150526
062000150526              wora=OOR2STIS   ;
062100150526              exsr edtora  ;
062200150526              oraDAL=worae  ;
062300150526              wora=OOR2STFS   ;
062400150526              exsr edtora  ;
062500150526              ora_AL=worae  ;
062600150526
062700150601              wparm=tisi795o0.day_rit + wdate + oradal + ora_al ;
062800150526              tisi795o0.not1_rit = rtvMsgLang('TIS0823':widlingua:wparm);
062900150526
063000150526              // Richiesto ORM commissioneato
063100150603              if   tisi795i0.orm_comm='S' or wsun<=*zeros  ;
063200150526              wora=OOR2LRSC   ;
063300150603              else            ;
063400150603              wora=OOR2LRNC   ;
063500150603              endif  ;
063600150526              exsr edtora  ;
063700150526              oraDAL=worae  ;
063800150526              wparm=oradal+wdate ;
063900150526              tisi795o0.not2_rit = rtvMsgLang('TIS0824':widlingua:wparm);
064000150526
064100150526        endif  ;
064200150514
064300150515        exsr Routend ;
064400150518        ENDSR  ;
064500150514
064600150514       //***********************************************************************
064700150514       //
064800150514       // Reperimento tempi consegna
064900150514       //
065000150514       //***********************************************************************
065100150514
065200150514       BEGSR RTVTEMPICONSEGNA;
065300150514
065400150514          TempiResaCli   = *zeros;
065500150514          GgResaCli      = *zeros;
065600150514          OreResaCli     = *zeros;
065700150514          GgResaCliAlfa  = *blanks;
065800150514          OreResaCliAlfa = *blanks;
065900150514
066000150514          exec sql select dlytrc into :TempiResaCli from wadly00f
066100150514               where dlylnp = :Tfp                         and
066200150514                     dlytsp = :CurrTipoServ                and
066300150514                     dlynar = ' '                          and
066400150514                     dlycap = :tisi95Ds_arrivo.o95Cap      and
066500150514                     dlyloc = :tisi95Ds_arrivo.o95Loc ;
066600150514
066700150515                //?servizio sul web non disponibile al momento
066800150522          if sqlcod < *zeros or sqlcod=100  ;
066900150518               prmRpyOpCode = tisi795_RPYOPCODE_ERROR;
067000150515               wIdMsg   = 'TIS0548';
067100150515               wIdCampo = '*TEMPI    ';
067200150515                clear wParm ;
067300150515               exsr Messaggi;
067400150515               exsr routend;
067500150515            endif;
067600150514
067700150514             GgResaCli  = %div(TempiResaCli:24);  // "giornalizzo" i tempi resa cliente
067800150514             OreResaCli = %rem(TempiResaCli:24);  // Ottengo le eventuali frazioni di giorno
067900150514             if OreResaCli <> *zeros;
068000150514                GgResaCli  = GgResaCli + 1;       // Se frazione di giorno => + 1 giorno
068100150514             endif;
068200150514
068300150514             if CurrTipoServ = 'H';
068400150514                select;
068500150514                  when TempiResaCli = 10;
068600150514                       GgResaCliAlfa  = '1';
068700150514                       OreResaCliAlfa = '10:30';
068800150514                  when TempiResaCli = 34;
068900150514                       GgResaCliAlfa  = '2';
069000150514                       OreResaCliAlfa = '10:30';
069100150514                  other;
069200150514                       GgResaCliAlfa  = 'N';
069300150514                       OreResaCliAlfa = *blanks;
069400150514                endsl;
069500150514
069600150514             else;
069700150514
069800150514                GgResaCliAlfa  = %char(GgResaCli);
069900150514                if OreResaCli = 12;
070000150514                   OreResaCliAlfa = '12:00';
070100150514                else;
070200150514                   OreResaCliAlfa = *blanks;
070300150514                endif;
070400150514
070500150514             endif;
070600150514
070700150514
070800150515             //?
070900150515             //?Compondo la descrizione delle tempistiche
071000150515
071100150515             //?Il servizio non è previsto
071200150522              tisi795o0.ktspSN(tisi795o0.TSP_TOT)= '1' ;
071300150515              if GgResaCliAlfa = 'N';
071400150522              tisi795o0.ktspSN(tisi795o0.TSP_TOT)= '0' ;
071500150526              clear wparm  ;
071600150526              tisi795o0.ktspnt(tisi795o0.TSP_TOT)=
071700150526                                          rtvMsgLang('TIS0822':widlingua:wparm);
071800150515              else      ;
071900150515
072000150515             //?Richiamo il TNSD99R per avere il giorno di consegna
072100150515             clear tnsd99ds   ;
072200150515             clear trulorsds    ;
072300150515             clear trulor2ds    ;
072400150515             clear trulor3ds    ;
072500150515
072600150515             d98lnp=lnapartenza ;
072700150515             d98tfp=tfp         ;
072800150515             d98lna=lnaarrivo   ;
072900150515          CLEAR fnlv55ds;
073000150515          fnlv55ds.d55tpt = 'A';
073100150529          fnlv55ds.d55drf = wdatarit    ;
073200150515          fnlv55ds.d55lin = LnaArrivo  ;
073300150515          fnlv55ds.d55lnp = LnaPartenza;
073400150515          fnlv55ds.d55drf = %dec(%date():*ISO);
073500150515          fnlv55r(fnlv55ds);
073600150515          if fnlv55ds.d55err <> *blanks;
073700150515             d98Tfa = LnaArrivo  ;
073800150515          else;
073900150515             d98Tfa = fnlv55ds.d55tfa;
074000150515          endif;
074100150515
074200150515          d98tsp=CurrtipoServ  ;
074300150518          d98cad=tisi795I0.cap_A     ;
074400150518          d98lod=tisi795I0.loc_A     ;
074500150529          d98dsp=wdatarit            ;
074600150529          d98aas=%int(%subst(%editc(wdatarit:'X'):1:4))  ;
074700150529          d98mgs=%int(%subst(%editc(wdatarit:'X'):5:4))  ;
074800150515          tnsd99r(tnsd99ds);
074900150515
075000150515          if  d98dee>0  ;
075100150515             //?richiamo pgm degli orari servizi
075200150515              ioretser='C'           ;
075300150515              iorefil =lnaArrivo     ;
075400150529              ioredta=d98dee         ;
075500150518              iorecap=tisi795I0.cap_A     ;
075600150518              ioreloc=tisi795I0.loc_A     ;
075700150515              ioretfp=tfp  ;
075800150515              ioretfa=d98tfa ;
075900150515              ioretsp =CurrtipoServ  ;
076000150515              ioredei=d98dei         ;
076100150518              ioreoei=d98oei         ;
076200150729              clear diore01 ;
076300150729              §IORETRAZC= %editc(d98ttc:'X') ;
076400150729              §IOREconsC= %editc(d98tcc:'X') ;
076500150729               ioreflo=diore01  ;
076600150518              oor3loc_N=tisi795I0.loc_A     ;
076700150515              trulorsr (kpjba:trulorsds:trulor2ds:trulor3ds) ;
076800150515              wora=OOR2STIS   ;
076900150515              exsr edtora  ;
077000150515              oraDAL=worae  ;
077100150515
077200150515              wora=OOR2STFS   ;
077300150515              exsr edtora  ;
077400150515              ora_AL=worae  ;
077500150515
077600150526              // alla data prevista consegna aggiungo un giorno lavorativo se ritiro in pickp2
077700150526              // e non è merce portata a magazzino
077800150526              if tisi795i0.merce_mag=' ' and tisi95Ds_partenza.o95gf2='02' ;
077900150526              Clear                   xgiolavds              ;
078000150526              Ixglpa  = 'A'                  ;
078100150526              IxglFil = lnaArrivo            ;
078200150526              Ixgldata = d98dee              ;
078300150526              Ixgladd = 'S'                  ;
078400150526              Ixgllav = 'S'                  ;
078500150526              ixglgg  = 1                    ;
078600150526              Callp  XGIOLAV    (xgiolavds)  ;
078700150526              wdat=oxgldata ;
078800150526              else   ;
078900150526              wdat=d98dee  ;
079000150526              endif   ;
079100150515              exsr edtdat   ;
079200150601               dataiso=%date(wdat:*iso);
079300150601               EXEC SQL SET :dw = DAYOFWEEK_ISO(:dataiso);
079400150601               wdesgio=rtvMsgLang(dayOfWeek.msgId(dw) : widlingua);
079500150601
079600150601              wparm=wdesgio + wdate + oradal + ora_al ;
079700150522              tisi795o0.ktspnt(tisi795o0.TSP_TOT)=
079800150601                                          rtvMsgLang('TIS0825':widlingua:wparm);
079900150515
080000150515              endif     ;
080100150515         endif     ;
080200150514
080300150514       ENDSR;
080400150514
080500150514       //***********************************************************************
080600150514       // Caricamento tabelle tipi servizio (in lungua)
080700150514       //***********************************************************************
080800150514
080900150514       BEGSR RTVTIPISERVIZI;
081000150514
081100150514         // Caricamento tipi servizi internet
081200150514
081300150514         exec sql declare tabel no scroll cursor for
081400150514              select * from tabel00f
081500150514              where tblkut = :LinguaBartTab    and
081600150514                    tblcod = '5E'              and
081700150514                    substr(tbluni, 83, 1) = 'S'
081800150514              order by substr(tbluni, 81, 2);
081900150514
082000150514         exec sql open tabel;
082100150514
082200150514         exec sql fetch next from tabel into :tabel00f;
082300150514
082400150514         dow sqlcod >= *zeros;
082500150514             if sqlcod = 100;
082600150514                leave;
082700150514             else;
082800150514                jTabel             = jTabel + 1;
082900150514                skTabelKey(jTabel) = tabel00f.tblkey;
083000150514                skTabelUni(jTabel) = tabel00f.tbluni;
083100150514             endif;
083200150514             exec sql fetch next from tabel into :tabel00f;
083300150514         enddo;
083400150514
083500150514         exec sql close tabel;
083600150514
083700150514       ENDSR;
083800150514
083900140527       //--------------------------------------------------------------
084000150504       //?Routine per schierare i messaggi nella ds TISI795M0
084100140527       //--------------------------------------------------------------
084200140527       BEGSR  Messaggi;
084300140527
084400140527       //?Se ho già caricato 99 messaggi esco
084500150504         IF  TISI795M0.nrmsg = 99;
084600140527           exsr RoutEnd;
084700150504           clear TISI795M0.nrmsg;
084800140527         ENDIF;
084900140527
085000150504         TISI795M0.nrmsg += 1;
085100150504         TISI795M0.skIdMsg(TISI795M0.nrmsg) = wIdMsg;
085200150504         TISI795M0.skIdCampo(TISI795M0.nrmsg) = wIdCampo;
085300150504         TISI795M0.skErrWarn(TISI795M0.nrmsg) = TISI795_MSG_ERRORE;
085400140527         IF  wParm <> *blanks;
085500150504           TISI795M0.skTextMsg (TISI795M0.nrmsg) =
085600140527           rtvMsgLang(wIdMsg:wIdlingua:wParm);
085700140527         ELSE;
085800140611       // Se non ho ID messaggio significa che ho già il testo del messaggio
085900140611           if wIdMsg=*blanks;
086000150504             TISI795M0.skTextMsg (TISI795M0.nrmsg)=wTextMsg;
086100140611           else;
086200150504             TISI795M0.skTextMsg (TISI795M0.nrmsg)=
086300140611             rtvMsgLang(wIdMsg:wIdlingua);
086400140611           endif;
086500140527         ENDIF;
086600140527
086700140527       ENDSR;
086800140527
086900140521       //--------------------------------------------------------------
087000140408       //?Carico tabelle.
087100140408       //--------------------------------------------------------------
087200140408       BEGSR  CaricaTab;
087300140408
087400140408
087500140408       ENDSR;
087600131010
087700131010       //--------------------------------------------------------------
087800131010       //?Operazioni finali.
087900131010       //--------------------------------------------------------------
088000131010       BEGSR  RoutEnd;
088100140408
088200150515          %subst(prmRpyData:1:prmRpyDataSize) = TISI795o0;
088300150504          %subst(prmRpyMessage:1:prmRpyMsgSize) = TISI795M0;
088400131010
088500131010         return;
088600131010
088700131010       ENDSR;
088800131009
088900131007      /end-free
089000150515
089100150515     c*--------------------------------------------------------------------------------------------*
089200150515     c* edita data da aaaammgg in gg/mm/aaaa
089300150515     c*--------------------------------------------------------------------------------------------*
089400150515     c     edtdat        BEGSR
089500150515     c*
089600150515     c                   MOVEL     *blanks       wdate
089700150515     c*
089800150515     c                   MOVEL     wdat          dsdat
089900150515     c                   MOVEL     dsgio         dsgioe
090000150515     c                   MOVEL     '.'           dsvd1
090100150515     c                   MOVEL     dsmes         dsmese
090200150515     c                   MOVEL     '.'           dsvd2
090300150515     c                   MOVEL     dsann         dsanne
090400150515     c                   MOVEL     dsdate        wdate
090500150515     c*
090600150515     c                   ENDSR
090700150515     c*--------------------------------------------------------------------------------------------*
090800150515     c* edita ora da hhmm in hh:mm
090900150515     c*--------------------------------------------------------------------------------------------*
091000150515     c     edtora        BEGSR
091100150515     c*
091200150515     c                   MOVEL     *blanks       worae
091300150515     c*
091400150515     c                   MOVEL     wora          dsora
091500150515     c                   MOVEL     dshh          dshhe
091600150515     c                   MOVEL     '.'           dsvo1
091700150515     c                   MOVEL     dsmm          dsmme
091800150515     c                   MOVEL     dsorae        worae
091900150515     c*
092000150515     c                   ENDSR
092100150629     P*--------------------------------------------------
092200150629     P* Procedure name: GetFilUrl
092300150629     P* Purpose:        Restituisce l'URL della filiale.
092400150629     P* Returns:        URL filiale.
092500150629     P* Parameter:      piFiliale => ID filiale
092600150629     P*--------------------------------------------------
092700150629     P GetFilUrl       B
092800150629     D GetFilUrl       PI           256A
092900150629     D  piFiliale                     3P 0 CONST
093000150629
093100150629     C                   IF        piFiliale = *ZERO
093200150629     C                   RETURN    *BLANK
093300150629     C                   ENDIF
093400150629     C                   clear                   tibs02ds
093500150629     c                   eval      t02mod='C'
093600150629     C                   EVAL      t02Cod = 'UFI'
093700150629     C                   EVAL      t02Ke1 = %EDITC(piFiliale : 'X')
093800150629     C                   EVAL      t02Lin = linguaBartTbe
093900150629     C                   CALLP     tibs02r(kpjba : tibs02ds)
094000150629     C                   IF        t02Err = 'E'
094100150629     C                   RETURN    *BLANK
094200150629     C                   ENDIF
094300150629     C                   RETURN    t02Uni
094400150629
094500150629     P GetFilUrl       E
094600150629

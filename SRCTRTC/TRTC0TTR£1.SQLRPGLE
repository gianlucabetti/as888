000100131007       //==============================================================
000200131007       //
000300160310       //?TRTC0TTR - Link al T&T del Partner
000500131007       //
000600131007       //==============================================================
000700131009
000800131009       //--------------------------------------------------------------
000900131009       //?Specifiche di controllo.                                     ?
001000131009       //--------------------------------------------------------------
001100140528     H dftactgrp(*no) actgrp(*caller) BNDDIR('TIS':'UBBNDDIR')
001200131009
001300131009       //--------------------------------------------------------------
001400131009       //?Dichiarazione file.                                          ?
001500131009       //--------------------------------------------------------------
002900140521     FFnarb01l  IF   E           K DISK    extfile(wlibfil) usropn
003600131008
003700131007       //--------------------------------------------------------------
003800131007       //?Definizione costanti.                                        ?
003900131007       //--------------------------------------------------------------
003901160311      /copy gaitrasrc/srcconst,TRTC0TTR
003902160311
004200140520     D digits          c                   '0123456789'
004202160311
004203160311     D chklib          S             35    DIM(1) CTDATA PERRCD(1)
004204160311     D HTTP            S             80    DIM(8) CTDATA PERRCD(1)
005900131007       //--------------------------------------------------------------
006000131007       //?Definizione strutture dati.                                  ?
006100131007       //--------------------------------------------------------------
006200131007       // -?Dati INPUT
006300160310     d Trtc0TTi0     e ds                  QUALIFIED INZ(*EXTDFT)
006600131007
006700131007       // -?Dati OUTPUT
006800160310     d Trtc0TTo0     e ds                  QUALIFIED INZ(*EXTDFT)
007200131007
010400140604     d KPJBA         e ds
015100131007       //--------------------------------------------------------------
015200131007       //?Definizione campi.
015300131007       //--------------------------------------------------------------
015601160311     d wIdlingua       s                   like(Trtc0TTi0.liniso2)
015800160315     d wIdbolla        s                   like(Trtc0TTi0.idbolla)
015801160311     d wLink           s                   like(Trtc0TTo0.LinkPTN)
015901160311
016600140528     d dataoggi        s              8s 0
016800140604     d annocur         s              4s 0
017000140605     d wdata           s              8s 0
017100140528     d wora            s              4s 0
017101160311     d su_as888        s              1A
017102160311
017400140521     d kaas            s                   like(arbaas)
017500140521     d klnp            s                   like(arblnp)
017600140521     d knrs            s                   like(arbnrs)
017700140521     d knsp            s                   like(arbnsp)
017801160311
018400140521     D Wlibfil         S             21
018500140521     D Wlibfilprd      S             21    inz('FILTRAPRD/        ')
018600140521     D Wlibfil201      S             21    INZ('FILTRA201/        ')
018601160311
018700140521     d dataeur         s               d   datfmt(*eur)
018800140528     d dataiso         s               d   datfmt(*iso)
018900140528     d wParm           s            512a   inz
019400140604     D Esito           s              1a
026600140611      *
027100131008       //--------------------------------------------------------------
027200131008       //?Definizione procedure.
027300131008       //--------------------------------------------------------------
031400131008
031500131008       //--------------------------------------------------------------
031600131008       //?Definizione prototipi.
031700131008       //--------------------------------------------------------------
031800160310      /copy gaitrasrc/srcprotopr,Trtc0TTr
033000131219
033100131219       //--------------------------------------------------------------
033200131219       //?Definizione parametri programma.
033300131219       //--------------------------------------------------------------
033400160311     D Trtc0TT_HTTP...
033500140519     D                 PI
033600140519     D prmRqsOpCode...
033700140519     D                               10I 0 CONST
033800140519     D prmRpyOpCode...
033900140519     D                               10I 0
034000140519     D prmRpyIdMsg...
034100140519     D                               10I 0
034200140519     D prmRqsFormato...
034300140519     D                               10A   CONST
034400140519     D prmRqsData...
034500140519     D                            32767A   OPTIONS(*VARSIZE)
034600140519     D prmRqsDataSize...
034700140519     D                               10I 0 CONST
034800140519     D prmRpyFormato...
034900140520     D                               10A   CONST
035000140519     D prmRpyData...
035100140519     D                            32767A   OPTIONS(*VARSIZE)
035200140519     D prmRpyDataSize...
035300140519     D                               10I 0 CONST
036600131007
036700131007       //--------------------------------------------------------------
036800131007       //?MAIN.
036900131007       //--------------------------------------------------------------
036901160311
036902160311     c* controllo se sono in AS888
036903160311     c                   eval      comman=chklib(1)
036904160311     c                   eval      lengh=35
036905160311     c                   call      'QCMDEXC'                            99
036906160311     c                   parm                    comman           80
036907160311     c                   parm                    lengh            15 5
036909160311      *
036910160311     c                   eval      su_AS888 ='S'
036911160311     c   99              eval      su_AS888 ='N'
036912160311      *
037100131007      /free
037101160315       //--------------------------------------------------------------
037300131008       //?Operazioni iniziali
037301160315       //--------------------------------------------------------------
037400131008       exsr RoutInz;
037401160315
037402160315       //?Controllo formale dei dati di accesso
037403160315       exsr Controlla;
037500131008
037600160311       //?Controlla la Bolla e prende la LNA per determinare a quale partner
037601160311       //?fare riferimento per il T&T.
037602160311       exsr sr_chainBolla;
039900131010
040000131010       //?Operazioni finali
040100131010       exsr RoutEnd;
040101160311
040102160311       //--------------------------------------------------------------
040103160311       //?Operazioni iniziali.                                         ?
040104160311       //--------------------------------------------------------------
040105160311       BEGSR  RoutInz;
040107160311
040108160311         *inLR = *on;
040119160311
040120160311         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
040121160311
040122160311         prmRpyOpCode = Trtc0TT_RPYOPCODE_DONE;
040123160311         prmRpyIdMsg  = Trtc0TT_ESITO_OK;
040124160311
040125160311       //?Data del giorno
040126160311         dataoggi = %dec(%date());
040127160311         annocur  = %subdt(%date():*years);
040128160311
040129160311       //?Pulisco ds di output
040130160311         reset Trtc0TTo0;
040131160311
040132160311       //?Apertura file x reperire la LNA della Spedizione
040133160311         wlibfil=wlibfil201;
040134160311         %subst(wlibfil:11:8)='FNARB01L';
040135160311         open(e) fnarb01l;
040136160311         if not %open(fnarb01l);
040137160311            wlibfil=wlibfilprd;
040138160311            %subst(wlibfil:11:8)='FNARB01L';
040139160311            open fnarb01l;
040140160311            if not %open(fnarb01l);
040141160311            return;
040142160311            endif;
040143160311         endif;
040153160311
040156160311       ENDSR;
040157160315       //--------------------------------------------------------------
040158160315       //?Controllo formale dei dati passati.
040159160315       //--------------------------------------------------------------
040160160315       BEGSR  Controlla;
040161160315
040163160315         clear widbolla;
040168160315
040169160315       //?Formato e size RQS
040170160315           IF  prmRqsFormato = trtc0TTI0.formato;
040171160315             trtc0TTI0 = %SUBST(prmRqsData:1:prmRqsDataSize);
040173160315             widbolla=trtc0TTI0.idbolla;
040175160315             widlingua=trtc0TTI0.liniso2;
040176160315           ELSE;
040177160315             prmRpyOpCode = trtc0TT_RPYOPCODE_ERROR;
040178160315             prmRpyIdMsg = trtc0TT_ESITO_NOME_FORMATO_SCONOSCIUTO;
040179160315             exsr RoutEnd;
040180160315           ENDIF;
040181160315
040182160315           IF  prmRqsDataSize > 0;
040183160315           ELSE;
040184160315             prmRpyOpCode = trtc0TT_RPYOPCODE_ERROR;
040185160315             prmRpyIdMsg = trtc0TT_ESITO_SIZE_DATA_ERRATO;
040186160315             exsr RoutEnd;
040187160315           ENDIF;
040188160315
040189160315         //?Formato e size RPY
040190160315           IF  prmRpyFormato = trtc0TTO0.formato;
040191160315           ELSE;
040192160315             prmRpyOpCode = trtc0TT_RPYOPCODE_ERROR;
040193160315             prmRpyIdMsg = trtc0TT_ESITO_NOME_FORMATO_SCONOSCIUTO;
040194160315             exsr RoutEnd;
040195160315           ENDIF;
040196160315
040197160315           IF  prmRpyDataSize > 0;
040198160315           ELSE;
040199160315             prmRpyOpCode = trtc0TT_RPYOPCODE_ERROR;
040200160315             prmRpyIdMsg = trtc0TT_ESITO_SIZE_DATA_ERRATO;
040201160315             exsr RoutEnd;
040202160315           ENDIF;
040335160315
040336160315         //?Formato e size RPY
040352160315
040373160315       //?Accesso per  ID Bolla
040375160315       // ID Bolla mancante
040376160315           if widbolla=*blanks;
040377160315              prmRpyOpCode = TRTC0TT_RPYOPCODE_ERROR;
040378160315              prmRpyIdMsg = TRTC0TT_ESITO_ID_BOLLA_ERRATO;
040382160315              exsr RoutEnd;
040383160315           endif;
040407160315
040413160315       // Verifico correttezza dell'ID bolla
040414160315       // Deve contenere solo numeri
040415160315         if %check(digits:widbolla)>0;
040416160315              prmRpyOpCode = TRTC0TT_RPYOPCODE_ERROR;
040417160315              prmRpyIdMsg = TRTC0TT_ESITO_ID_BOLLA_ERRATO;
040421160315              exsr RoutEnd;
040422160315         endif;
040491160315
040492160315       ENDSR;
040493160311       //--------------------------------------------------------------
040494160311       //?Operazioni finali.
040495160311       //--------------------------------------------------------------
040496160311       BEGSR  RoutEnd;
040497160311
040499160311         return;
040500160311
040501160311       ENDSR;
040502160311
127800140521       //--------------------------------------------------------------
127900140521       //?Chain FNARB
128000140521       //--------------------------------------------------------------
128100140521       BEGSR  sr_chainbolla;
128200140527
128201160311       // Ricevuto il Parametro
128202160315          wIdbolla=Trtc0TTi0.idbolla;
128203160311
128300140521       // imposto la chiave di accesso
128400160315          kaas=2000+%dec(%subst(wIdbolla:1:2):2:0);
128500160315          klnp=%dec(%subst(wIdbolla:3:3):3:0);
128600160315          knrs=%dec(%subst(wIdbolla:6:2):2:0);
128700160315          knsp=%dec(%subst(wIdbolla:8:7):7:0);
128800140521          chain (kaas:klnp:knrs:knsp) fnarb01l;
128900140521          if not %found(fnarb01l);
129000160311              prmRpyOpCode = TRTC0TT_RPYOPCODE_ERROR;
129500140521              exsr RoutEnd;
129600140521          endif;
129601160311
129700160311       // se TROVATA la BOLLA   deve interpretare la LNA
129701160311              prmRpyOpCode = TRTC0TT_RPYOPCODE_DONE;
129702160311
129703160311       // AZKAR
129704160311         IF  arbLNA = 340 or arbLNA = 345;
129705160315             wLink = %trim(HTTP(1)) + wIdbolla +
129706160316               %trim(%subst(HTTP(2):15:50)) + %trim(wIdlingua);
129707160311         endif;
129708160311
129709160311       // GEL
129710160311         IF  arbLNA = 311;
129711160315             wLink = %trim(HTTP(3)) + wIdbolla;
129712160311         endif;
129713160311
129714160311       // GEODIS
129800160311         IF  arbLNA = 333 or arbLNA = 334;
129801160311             wLink = %trim(HTTP(5)) + %trim(HTTP(6))     +
129802160315                     %trim(HTTP(7)) + wIdbolla +
129803160316               %trim(%subst(HTTP(8):15:50));
130300141211         endif;
130301160316
130302160316            trtc0TTo0.LINKPTN = wLink;
130303160316           %subst(prmRpyData:1:prmRpyDataSize) = trtc0TTo0;
130304160311
130400140521       endsr;
172300140522
285100131007      /end-free
289100140814**
289200141006CHKOBJ OBJ(FILTRAPRD) OBJTYPE(*LIB)
289300160311**
289400160311http://azkarview.azkar.com/trackingdirecto/detalle?CLI=560004&TIPO=R&VALOR=         340/345 ES
289401160316<--BRT CODE-->&IDI=                                                                 340/345 ES
289500160311http://srv4.thalmaier.de/GEL/BRT/EN-en/TrackingAndTracing.aspx?snr=                 311     DE
289501160311<--BRT CODE-->                                                                      311     DE
289600160311http://espace.geodis.com/agence/client/webservices/suivi/expeditionsSecurise.do?    333/334 FR
289700160311userid=38213c3b3b2e3f&password=38213c53425044&rca=&codesa=&compte=&date=&produit    333/334 FR
289800160311=&recepisse=&filiale=&prestation=&reference=                                        333/334 FR
289801160311<--BRT CODE-->&accessimg=&method=executeQuery                                       333/334 FR

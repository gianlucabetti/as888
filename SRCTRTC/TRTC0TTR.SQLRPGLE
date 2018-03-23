000100131007       //==============================================================
000200131007       //
000300160310       //?TRTC0TTR - Link al T&T del Partner
000400131007       //
000500131007       //==============================================================
000600131009
000700131009       //--------------------------------------------------------------
000800131009       //?Specifiche di controllo.                                     ?
000900131009       //--------------------------------------------------------------
001000140528     H dftactgrp(*no) actgrp(*caller) BNDDIR('TIS':'UBBNDDIR')
001100131009
001200131009       //--------------------------------------------------------------
001300131009       //?Dichiarazione file.                                          ?
001400131009       //--------------------------------------------------------------
001500160317     FTITAS30C  IF   E           K DISK
001600131008
001700131007       //--------------------------------------------------------------
001800131007       //?Definizione costanti.                                        ?
001900131007       //--------------------------------------------------------------
002000160311      /copy gaitrasrc/srcconst,TRTC0TTR
002100160311
002200140520     D digits          c                   '0123456789'
002300160311
002400160311     D chklib          S             35    DIM(1) CTDATA PERRCD(1)
002500160311     D HTTP            S             80    DIM(8) CTDATA PERRCD(1)
002600131007       //--------------------------------------------------------------
002700131007       //?Definizione strutture dati.                                  ?
002800131007       //--------------------------------------------------------------
002900131007       // -?Dati INPUT
003000160310     d Trtc0TTi0     e ds                  QUALIFIED INZ(*EXTDFT)
003100131007
003200131007       // -?Dati OUTPUT
003300160310     d Trtc0TTo0     e ds                  QUALIFIED INZ(*EXTDFT)
003400131007
003500140604     d KPJBA         e ds
003600131007       //--------------------------------------------------------------
003700131007       //?Definizione campi.
003800131007       //--------------------------------------------------------------
003900160311     d wIdlingua       s                   like(Trtc0TTi0.liniso2)
004000160315     d wIdbolla        s                   like(Trtc0TTi0.idbolla)
004100160311     d wLink           s                   like(Trtc0TTo0.LinkPTN)
004200160311
004300140528     d dataoggi        s              8s 0
004400140604     d annocur         s              4s 0
004500140605     d wdata           s              8s 0
004600140528     d wora            s              4s 0
004700160311     d su_as888        s              1A
004800160311
004900160317     d kaas            s                   like(tasaas)
005000160317     d klnp            s                   like(taslnp)
005100160317     d knrs            s                   like(tasnrs)
005200160317     d knsp            s                   like(tasnsp)
005300160311
005400140521     d dataeur         s               d   datfmt(*eur)
005500140528     d dataiso         s               d   datfmt(*iso)
005600140528     d wParm           s            512a   inz
005700140604     D Esito           s              1a
005800140611      *
005900131008       //--------------------------------------------------------------
006000131008       //?Definizione procedure.
006100131008       //--------------------------------------------------------------
006200131008
006300131008       //--------------------------------------------------------------
006400131008       //?Definizione prototipi.
006500131008       //--------------------------------------------------------------
006600160310      /copy gaitrasrc/srcprotopr,Trtc0TTr
006700131219
006800131219       //--------------------------------------------------------------
006900131219       //?Definizione parametri programma.
007000131219       //--------------------------------------------------------------
007100160311     D Trtc0TT_HTTP...
007200140519     D                 PI
007300140519     D prmRqsOpCode...
007400140519     D                               10I 0 CONST
007500140519     D prmRpyOpCode...
007600140519     D                               10I 0
007700140519     D prmRpyIdMsg...
007800140519     D                               10I 0
007900140519     D prmRqsFormato...
008000140519     D                               10A   CONST
008100140519     D prmRqsData...
008200140519     D                            32767A   OPTIONS(*VARSIZE)
008300140519     D prmRqsDataSize...
008400140519     D                               10I 0 CONST
008500140519     D prmRpyFormato...
008600140520     D                               10A   CONST
008700140519     D prmRpyData...
008800140519     D                            32767A   OPTIONS(*VARSIZE)
008900140519     D prmRpyDataSize...
009000140519     D                               10I 0 CONST
009100131007
009200131007       //--------------------------------------------------------------
009300131007       //?MAIN.
009400131007       //--------------------------------------------------------------
009500160311
009600160311     c* controllo se sono in AS888
009700160311     c                   eval      comman=chklib(1)
009800160311     c                   eval      lengh=35
009900160311     c                   call      'QCMDEXC'                            99
010000160311     c                   parm                    comman           80
010100160311     c                   parm                    lengh            15 5
010200160311      *
010300160311     c                   eval      su_AS888 ='S'
010400160311     c   99              eval      su_AS888 ='N'
010500160311      *
010600131007      /free
010700160315       //--------------------------------------------------------------
010800131008       //?Operazioni iniziali
010900160315       //--------------------------------------------------------------
011000131008       exsr RoutInz;
011100160315
011200160315       //?Controllo formale dei dati di accesso
011300160315       exsr Controlla;
011400131008
011500160311       //?Controlla la Bolla e prende la LNA per determinare a quale partner
011600160311       //?fare riferimento per il T&T.
011700160311       exsr sr_chainBolla;
011800131010
011900131010       //?Operazioni finali
012000131010       exsr RoutEnd;
012100160311
012200160311       //--------------------------------------------------------------
012300160311       //?Operazioni iniziali.                                         ?
012400160311       //--------------------------------------------------------------
012500160311       BEGSR  RoutInz;
012600160311
012700160311         *inLR = *on;
012800160311
012900160311         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
013000160311
013100160311         prmRpyOpCode = Trtc0TT_RPYOPCODE_DONE;
013200160311         prmRpyIdMsg  = Trtc0TT_ESITO_OK;
013300160311
013400160311       //?Data del giorno
013500160311         dataoggi = %dec(%date());
013600160311         annocur  = %subdt(%date():*years);
013700160311
013800160311       //?Pulisco ds di output
013900160311         reset Trtc0TTo0;
014000160311
014100160311       ENDSR;
014200160317       //--------------------------------------------------------------
014300160315       //?Controllo formale dei dati passati.
014400160315       //--------------------------------------------------------------
014500160315       BEGSR  Controlla;
014600160315
014700160315         clear widbolla;
014800160315
014900160315       //?Formato e size RQS
015000160315           IF  prmRqsFormato = trtc0TTI0.formato;
015100160315             trtc0TTI0 = %SUBST(prmRqsData:1:prmRqsDataSize);
015200160315             widbolla=trtc0TTI0.idbolla;
015300160315             widlingua=trtc0TTI0.liniso2;
015400160315           ELSE;
015500160315             prmRpyOpCode = trtc0TT_RPYOPCODE_ERROR;
015600160315             prmRpyIdMsg = trtc0TT_ESITO_NOME_FORMATO_SCONOSCIUTO;
015700160315             exsr RoutEnd;
015800160315           ENDIF;
015900160315
016000160315           IF  prmRqsDataSize > 0;
016100160315           ELSE;
016200160315             prmRpyOpCode = trtc0TT_RPYOPCODE_ERROR;
016300160315             prmRpyIdMsg = trtc0TT_ESITO_SIZE_DATA_ERRATO;
016400160315             exsr RoutEnd;
016500160315           ENDIF;
016600160315
016700160315         //?Formato e size RPY
016800160315           IF  prmRpyFormato = trtc0TTO0.formato;
016900160315           ELSE;
017000160315             prmRpyOpCode = trtc0TT_RPYOPCODE_ERROR;
017100160315             prmRpyIdMsg = trtc0TT_ESITO_NOME_FORMATO_SCONOSCIUTO;
017200160315             exsr RoutEnd;
017300160315           ENDIF;
017400160315
017500160315           IF  prmRpyDataSize > 0;
017600160315           ELSE;
017700160315             prmRpyOpCode = trtc0TT_RPYOPCODE_ERROR;
017800160315             prmRpyIdMsg = trtc0TT_ESITO_SIZE_DATA_ERRATO;
017900160315             exsr RoutEnd;
018000160315           ENDIF;
018100160315
018200160315         //?Formato e size RPY
018300160315
018400160315       //?Accesso per  ID Bolla
018500160315       // ID Bolla mancante
018600160315           if widbolla=*blanks;
018700160315              prmRpyOpCode = TRTC0TT_RPYOPCODE_ERROR;
018800160315              prmRpyIdMsg = TRTC0TT_ESITO_ID_BOLLA_ERRATO;
018900160315              exsr RoutEnd;
019000160315           endif;
019100160315
019200160315       // Verifico correttezza dell'ID bolla
019300160315       // Deve contenere solo numeri
019400160315         if %check(digits:widbolla)>0;
019500160315              prmRpyOpCode = TRTC0TT_RPYOPCODE_ERROR;
019600160315              prmRpyIdMsg = TRTC0TT_ESITO_ID_BOLLA_ERRATO;
019700160315              exsr RoutEnd;
019800160315         endif;
019900160315
020000160315       ENDSR;
020100160311       //--------------------------------------------------------------
020200160311       //?Operazioni finali.
020300160311       //--------------------------------------------------------------
020400160311       BEGSR  RoutEnd;
020500160311
020600160311         return;
020700160311
020800160311       ENDSR;
020900160311
021000140521       //--------------------------------------------------------------
021100160317       //?Chain TITAS
021200140521       //--------------------------------------------------------------
021300140521       BEGSR  sr_chainbolla;
021400140527
021500160311       // Ricevuto il Parametro
021600160315          wIdbolla=Trtc0TTi0.idbolla;
021700160311
021800140521       // imposto la chiave di accesso
021900160315          kaas=2000+%dec(%subst(wIdbolla:1:2):2:0);
022000160315          klnp=%dec(%subst(wIdbolla:3:3):3:0);
022100160315          knrs=%dec(%subst(wIdbolla:6:2):2:0);
022200160315          knsp=%dec(%subst(wIdbolla:8:7):7:0);
022300160317          chain (kaas:klnp:knrs:knsp) titas30c;
022400160317          if not %found(titas30c);
022500160311              prmRpyOpCode = TRTC0TT_RPYOPCODE_ERROR;
022600140521              exsr RoutEnd;
022700140521          endif;
022800160311
022900160311       // se TROVATA la BOLLA   deve interpretare la LNA
023000160311              prmRpyOpCode = TRTC0TT_RPYOPCODE_DONE;
023100160311
023200160311       // AZKAR
023300160317         IF  tasLNA = 340 or tasLNA = 345;
023400160315             wLink = %trim(HTTP(1)) + wIdbolla +
023500160316               %trim(%subst(HTTP(2):15:50)) + %trim(wIdlingua);
023600160311         endif;
023700160311
023800160311       // GEL
023900160317         IF  tasLNA = 311;
024000160315             wLink = %trim(HTTP(3)) + wIdbolla;
024100160311         endif;
024200160311
024300160311       // GEODIS
024400160317         IF  tasLNA = 333 or tasLNA = 334;
024500160311             wLink = %trim(HTTP(5)) + %trim(HTTP(6))     +
024600160315                     %trim(HTTP(7)) + wIdbolla +
024700160316               %trim(%subst(HTTP(8):15:50));
024800141211         endif;
024900160316
025000160316            trtc0TTo0.LINKPTN = wLink;
025100160316           %subst(prmRpyData:1:prmRpyDataSize) = trtc0TTo0;
025200160311
025300140521       endsr;
025400140522
025500131007      /end-free
025600140814**
025700141006CHKOBJ OBJ(FILTRAPRD) OBJTYPE(*LIB)
025800160311**
025900180108http://eview.dachser.com/trackingdirecto/detalle?CLI=560004&TIPO=R&VALOR=           340/345 ES
026000160316<--BRT CODE-->&IDI=                                                                 340/345 ES
026100160311http://srv4.thalmaier.de/GEL/BRT/EN-en/TrackingAndTracing.aspx?snr=                 311     DE
026200160311<--BRT CODE-->                                                                      311     DE
026300160311http://espace.geodis.com/agence/client/webservices/suivi/expeditionsSecurise.do?    333/334 FR
026400160311userid=38213c3b3b2e3f&password=38213c53425044&rca=&codesa=&compte=&date=&produit    333/334 FR
026500160311=&recepisse=&filiale=&prestation=&reference=                                        333/334 FR
026600160311<--BRT CODE-->&accessimg=&method=executeQuery                                       333/334 FR

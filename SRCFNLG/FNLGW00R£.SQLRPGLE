000100131007       //==============================================================
000200131007       //
000300170213       //?FNLGW00R - Disposizioni di giacenza da internet
000400140519       //
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
001600131010
001700170217     FFnarb01l  IF   E           K DISK    extfile(wlibfil) usropn
001800170217     ftigcp01l  if   e           k disk
001900170215     ftabel00f  if   e           k disk
002000131008
002100131007       //--------------------------------------------------------------
002200131007       //?Definizione costanti.                                        ?
002300131007       //--------------------------------------------------------------
002400170213      /copy gaitrasrc/srcconst,FNLGW00R
002500140520
002600140520     D digits          c                   '0123456789'
002700140114
002800131007       //--------------------------------------------------------------
002900131007       //?Definizione strutture dati.                                  ?
003000131007       //--------------------------------------------------------------
003100131007       // -?Dati INPUT
003200170213     d FnLGw00i0     e ds                  QUALIFIED INZ(*EXTDFT)
003300131007
003400131007       // -?Dati OUTPUT
003500170215     d FnLGw00o0     e ds                  INZ(*EXTDFT)
003600131007
003700131007       // -?Messaggi
003800170216     d FnLGw00m0     e ds                  QUALIFIED INZ(*EXTDFT)
003900140522     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
004000140522     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
004100140522     d  skErrWarn                     1a   dim(99) overlay(ErrWarn)
004200140522     d  skTextMsg                   255a   dim(99) overlay(TextMsg)
004300131007
004400140521       // -?Controllo BRTCODE
004500140521     d TRUL28DS0     e ds                  INZ(*EXTDFT)
004600140604     d KPJBA         e ds
004700170216     d fnlg071ds     e ds
004800170316     d tibs02ds      e ds
004900161128
005000170316     d dvpolgw0      e ds
005100170316     D  fil                    1     60  0   dim(20)
005200140521
005300140522     d ds7r          e ds
005400140619     d ds7w          e ds
005500140528     D WDAT8           DS
005600140528     D  DATADA                 1      8  0
005700140528     D  DATAA                  9     16  0
005800140528     D  GGL                   17     21  0
005900140312
006000131009       //--------------------------------------------------------------
006100131009       //?Definizione schiere.
006200131009       //--------------------------------------------------------------
006300140210
006400140210     d skFIdMsg        s              7a   dim(99)
006500140210     d skFIdCampo      s             10a   dim(99)
006600131007
006700131007       //--------------------------------------------------------------
006800131007       //?Definizione campi.
006900131007       //--------------------------------------------------------------
007000140527     d wIdMsg          s              7a
007100140527     d wIdCampo        s             10a
007200140527     d wcampo          s             10a
007300170216     d kaas            s                   like(arbaas)
007400170216     d klnp            s                   like(arblnp)
007500170216     d knrs            s                   like(arbnrs)
007600170216     d knsp            s                   like(arbnsp)
007700170317     d ksu             s                   like(fnLGw00i0.ksu)
007800170216     d wksu            s                   like(fnLGw00i0.ksu)
007900170216     d widbolla        s                   like(fnLGw00i0.idbolla)
008000140521     d wbolla          s                   like(widbolla)
008100170216     d wbrtcode        s                   like(fnLGw00i0.brtcode)
008200170216     d wkeygiac        s                   like(fnLGw00i0.keygiac)
008300170216     d wIdlingua       s                   like(fnLGw00i0.liniso2)
008400140521     d dataeur         s               d   datfmt(*eur)
008500140528     d dataiso         s               d   datfmt(*iso)
008600170316     D Esito           s             10I 0
008700170317     D esito10         S             10I 0
008800170216     D wchi            s              1a
008900170215     d wParm           s            512a   inz
009000170215     d w008a           s              8a   inz
009100140611      *
009200170316     d count           s             10I 0 inz
009300140611     d RTNesito        s             10I 0 inz
009400140611     d RTNopcode       s             10    inz
009500140611     d RTNstatus       s             10I 0 inz
009600170216     d wDecofiAll      s               n   inz
009700170216     d wTextMsg        s            255
009800170217     D Wlibfil         S             21
009900170217     D Wlibfilprd      S             21    inz('FILTRAPRD/        ')
010000170217     D Wlibfil201      S             21    INZ('FILTRA201/        ')
010100131008
010200131008       //--------------------------------------------------------------
010300131008       //?Definizione procedure.
010400131008       //--------------------------------------------------------------
010500131008
010600131008       //--------------------------------------------------------------
010700131008       //?Definizione prototipi.
010800131008       //--------------------------------------------------------------
010900170213      /copy gaitrasrc/srcprotopr,FnLGw00r
011000140521      /copy gaitrasrc/srcprotopr,TRUL28R0
011100170216      /copy gaitrasrc/srcprotopr,RTVMSGLANG
011200131219
011300131219       //--------------------------------------------------------------
011400131219       //?Definizione parametri programma.
011500131219       //--------------------------------------------------------------
011600170213     D FnLGw00_dispo...
011700140519     D                 PI
011800140519     D prmRqsOpCode...
011900140519     D                               10I 0 CONST
012000140519     D prmRpyOpCode...
012100140519     D                               10I 0
012200140519     D prmRpyIdMsg...
012300140519     D                               10I 0
012400140519     D prmRqsFormato...
012500140519     D                               10A   CONST
012600140519     D prmRqsData...
012700140519     D                            32767A   OPTIONS(*VARSIZE)
012800140519     D prmRqsDataSize...
012900140519     D                               10I 0 CONST
013000140519     D prmRpyFormato...
013100140520     D                               10A   CONST
013200140519     D prmRpyData...
013300140519     D                            32767A   OPTIONS(*VARSIZE)
013400140519     D prmRpyDataSize...
013500140519     D                               10I 0 CONST
013600140519     D prmRpyFormMsg...
013700140519     D                               10A   CONST OPTIONS(*NOPASS)
013800140519     D prmRpyMessage...
013900140519     D                            32767A   OPTIONS(*VARSIZE : *NOPASS)
014000140519     D prmRpyMsgSize...
014100140519     D                               10I 0 CONST OPTIONS(*NOPASS)
014200131007       //--------------------------------------------------------------
014300131007       //?MAIN.
014400131007       //--------------------------------------------------------------
014500131008
014600131007      /free
014700170217       //?Apertura file
014800170217         wlibfil=wlibfil201;
014900170217         %subst(wlibfil:11:8)='FNARB01L';
015000170217         open(e) fnarb01l;
015100170217         if not %open(fnarb01l);
015200170217            wlibfil=wlibfilprd;
015300170217            %subst(wlibfil:11:8)='FNARB01L';
015400170217            open fnarb01l;
015500170217         endif;
015600131008
015700131008       //?Operazioni iniziali
015800131008       exsr RoutInz;
015900170215
016000170215       //?Controllo formale dei dati di accesso
016100170215       exsr Controlla;
016200131010
016300131010       //?Operazioni finali
016400131010       exsr RoutEnd;
016500131008
016600131008       //--------------------------------------------------------------
016700131008       //?Operazioni iniziali.                                         ?
016800131008       //--------------------------------------------------------------
016900131008       BEGSR  RoutInz;
017000140211
017100140211         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
017200131008
017300170215         prmRpyOpCode = FNLGw00_RPYOPCODE_DONE;
017400170215         prmRpyIdMsg  = FNLGw00_ESITO_OK;
017500131009
017600131009       //?Pulisco ds di output
017700170213         reset FnLGw00o0;
017800170215         reset fnLGw00m0;
017900140521
018000170213      /end-free
018100161128     c* carico le filiali abilitate all'immissione dispo di consegna su bolle in
018200170316      * Aggancio tabella "VPO" "FNLGW00R" per recupero filiali attive
018300170316     C                   CLEAR                   tibs02ds
018400170316     c                   clear                   dvpolgw0
018500170316     C                   MOVEL     'C'           T02MOD
018600170316     C                   MOVEL     KNSIF         T02SIF
018700170316     C                   MOVEL     'VPO'         T02COD
018800170316     c                   movel(P)  'LGW0'        t02ke1
018900170316     C                   CALL      'TIBS02R'
019000170316     C                   PARM                    KPJBA
019100170316     C                   PARM                    tibs02ds
019200170316    2C                   IF        T02ERR = *BLANKS
019300170316     C                   MOVEL     T02uni        dvpolgw0
019400170316    2C                   ENDIF
019500161128     c
019600140521      /free
019700131008
019800131008         *inLR = *on;
019900131008
020000131008       ENDSR;
020100131008
020200170215       //--------------------------------------------------------------
020300170215       //?Check chiamata possibili disposizioni
020400170215       //--------------------------------------------------------------
020500170215       BEGSR  CheckDispo;
020600170215      /end-free
020700170215     c                   eval      BUTRESO     = *off
020800170215     c                   eval      BUTCON      = *off
020900170215     c                   eval      BUTCONAPP   = *off
021000170215     c                   eval      BUTCONDCRI  = *off
021100170215     c                   eval      BUTCONDCRD  = *off
021200170215     c                   eval      BUTCONIND   = *off
021300170215     c                   eval      BUTFD       = *off
021400170215     c                   eval      BUTNOTE     = *off
021500170215     c                   eval      BUTGEN      = *off
021600170317
021700170320     C/EXEC SQL
021800170320     C+ SELECT COUNT(*) INTO :count FROM TIVPI00F WHERE VPITIP = 'TR'
021900170320     C+ AND SUBSTR(VPIDTA, 22, 4) = DIGITS(:gcpagc) AND
022000170320     C+ SUBSTR(VPIDTA, 27, 3) = DIGITS(:gcpfgc) AND SUBSTR(VPIDTA, 31, 7) =
022100170320     C+ DIGITS(:gcpngc)
022200170320     C/END-EXEC
022300170320     C                   IF        sqlCod < 0
022400170320     C                   EVAL      esito = sqlCod
022500170320     C                   DUMP(A)
022600170320     C                   EXSR      routend
022700170320     C                   ENDIF
022800170320
022900170320     C                   EVAL      esito = count
023000170320
023100170320     C                   IF        esito <> 0
023200170320     C                   EXSR      routend
023300170320     C                   ENDIF
023400170316
023500170215      * chiamata per abilitazione bottoni
023600170215     c                   clear                   fnlg071ds
023700170215     c                   move      'C'           i071CHIA
023800170215     c                   movel     gcpAAS        i071AAS
023900170215     c                   movel     gcpLNP        i071LNP
024000170215     c                   movel     gcpNRS        i071NRS
024100170215     c                   move      gcpNSP        i071NSP
024200170215     c                   movel     gcpLNa        i071LNa
024300170215     c                   move      wchi          i071CHI
024400170215     c                   movel     gcpcmc        i071cmc
024500170215     c                   call      'FNLG07R1'
024600170215     c                   parm                    kpjba
024700170215     c                   parm                    fnlg071ds
024800170215     c                   if        o071esito = *blank
024900170215      *generico
025000170215     c                   select
025100170215     c                   when      i071chi = 'P'
025200170324     c                   eval      BUTNOTE     = *on
025300170324     c                   eval      BUTGEN      = *on
025400170215      *reso
025500170215     c                   if        o071disr = 'S'
025600170215     c                   eval      BUTRESO     = *on
025700170215     c                   endif
025800170215      *consegna
025900170215     c                   if        o071disc = 'S'
026000170215     c                   eval      BUTCON      = *on
026100170215     c                   eval      BUTCONDCRI  = *on
026200170215     c                   eval      BUTCONAPP   = *on
026300170215     c                   eval      BUTCONDCRD  = *on
026400170215     c                   eval      BUTCONIND   = *on
026500170215     c                   eval      BUTFD       = *on
026600170215     c                   endif
026700170215     c                   when      i071chi = 'A'
026800170324     c                   eval      BUTNOTE     = *off
026900170324     c                   if        o071disc = 'N' and
027000170324     c                             o071disd = 'N' and
027100170324     c                             o071disr = 'N'
027200170324     c                   eval      BUTGEN      = *off
027300170324     c                   else
027400170324     c                   eval      BUTGEN      = *on
027500170324     c                   endif
027600170215      *reso
027700170215     c                   if        o071disr = 'S'
027800170215     c                   eval      BUTRESO     = *on
027900170215     c                   endif
028000170215      *consegna
028100170327     c                   exsr      partic
028200170215     c                   if        o071disc = 'S'
028300170215     c                   eval      BUTCON      = *on
028400170215     c                   eval      BUTCONDCRI  = *on
028500170327     c                   endif
028600170327     c                   if        §7wdes = 'S' and
028700170327     c                             (o071disc = 'S' or o071disd = 'S')
028800170215     c                   eval      BUTCONIND   = *on
028900170215     c                   endif
029000170324     c                   if        §7rfd = *blank
029100170215     c                   eval      BUTFD       = *on
029200170215     c                   endif
029300170215     c                   endsl
029400170216     c                   endif
029500170215      /free
029600170215       ENDSR;
029700170215       //--------------------------------------------------------------
029800170316       //?Controllo se esistono particolarità
029900170215       //--------------------------------------------------------------
030000170215       BEGSR  Partic;
030100170215       //NON AMMESSO AL DESTINATARIO SE NON ABILITATO MEDIANTE TABELLA 7W
030200170215         clear ds7W;
030300170215         if  arbgga<>*blanks;
030400170215            w008a=ARBGGA;
030500170215            chain (1 : '7W' : W008A) tabel00f;
030600170215            if %found(tabel00f);
030700170215               ds7w=tbluni;
030800170215            endif;
030900170215         endif;
031000170215
031100170215       //abilitare il fermo deposito  TABELLA 7R
031200170215         clear ds7r;
031300170215         if arbffd=*blanks and arbgma<>*blanks;
031400170215           clear ds7r;
031500170215            if  arbgma<>*blanks;
031600170215             w008a=ARBGmA;
031700170215             chain (1 : '7R' : W008A) tabel00f;
031800170215              if %found(tabel00f);
031900170215               ds7R=tbluni;
032000170215              endif;
032100170215            endif;
032200170215         endif;
032300170215       ENDSR;
032400131008       //--------------------------------------------------------------
032500131008       //?Controllo formale dei dati passati.
032600131008       //--------------------------------------------------------------
032700131008       BEGSR  Controlla;
032800131008
032900140519         clear wksu;
033000140520         clear widbolla;
033100140520         clear wbrtcode;
033200170215         clear wkeygiac;
033300140522         clear wchi    ;
033400140527         clear widlingua;
033500140527         clear wcampo;
033600140519
033700140408       //?Formato e size RQS
033800170213           IF  prmRqsFormato = fnLGw00I0.formato;
033900170213             fnLGw00I0 = %SUBST(prmRqsData:1:prmRqsDataSize);
034000170213             wksu=fnLGw00i0.ksu;
034100170213             widbolla=fnLGw00i0.idbolla;
034200170213             wbrtcode=fnLGw00i0.brtcode;
034300170215             wkeygiac=fnLGw00i0.keygiac;
034400170213             widlingua=fnLGw00i0.liniso2;
034500140408           ELSE;
034600170213             prmRpyOpCode = FNLGw00_RPYOPCODE_ERROR;
034700170213             prmRpyIdMsg = FNLGw00_ESITO_NOME_FORMATO_SCONOSCIUTO;
034800140408             exsr RoutEnd;
034900140408           ENDIF;
035000140408           IF  prmRqsDataSize > 0;
035100140408           ELSE;
035200170213             prmRpyOpCode = FNLGw00_RPYOPCODE_ERROR;
035300170213             prmRpyIdMsg = FNLGw00_ESITO_SIZE_DATA_ERRATO;
035400140408             exsr RoutEnd;
035500140408           ENDIF;
035600140408         //?Formato e size RPY
035700170216           IF  prmRpyFormato = formato;
035800140408           ELSE;
035900170213             prmRpyOpCode = FNLGw00_RPYOPCODE_ERROR;
036000170213             prmRpyIdMsg = FNLGw00_ESITO_NOME_FORMATO_SCONOSCIUTO;
036100140408             exsr RoutEnd;
036200140408           ENDIF;
036300140408           IF  prmRpyDataSize > 0;
036400140408           ELSE;
036500170213             prmRpyOpCode = FNLGw00_RPYOPCODE_ERROR;
036600170213             prmRpyIdMsg = FNLGw00_ESITO_SIZE_DATA_ERRATO;
036700140408             exsr RoutEnd;
036800140408           ENDIF;
036900140210
037000170215             if wbrtcode<>*blanks;
037100170215              wchi='A';
037200140627             exsr sr_trul28;
037300140627             wbrtcode=o280cod ;
037400140627             else;
037500170215              wchi='P';
037600140624             clear wbrtcode;
037700140627             endif;
037800140624             widlingua='it';
037900140624           IF  prmRqsDataSize > 0;
038000140624           ELSE;
038100170213             prmRpyOpCode = FNLGw00_RPYOPCODE_ERROR;
038200170213             prmRpyIdMsg = FNLGw00_ESITO_SIZE_DATA_ERRATO;
038300140624             exsr RoutEnd;
038400140624           ENDIF;
038500140624       //?Formato e size MSG
038600170213         IF  prmRpyFormMsg = FNLGw00M0.formato;
038700140624         ELSE;
038800170213           prmRpyOpCode = FNLGw00_RPYOPCODE_ERROR;
038900170213           prmRpyIdMsg = FNLGw00_ESITO_NOME_FORMATO_SCONOSCIUTO;
039000140624           exsr RoutEnd;
039100140624         ENDIF;
039200140624         IF  prmRpyMsgSize > 0;
039300140624         ELSE;
039400170213           prmRpyOpCode = FNLGw00_RPYOPCODE_ERROR;
039500170213           prmRpyIdMsg = FNLGw00_ESITO_SIZE_DATA_ERRATO;
039600140624           exsr RoutEnd;
039700140624         ENDIF;
039800140210
039900170215       //?devo ricevere ID cliente unificante o BRTCODE o keygiac
040000170215         if wksu=*blanks and wbrtcode=*blanks and wkeygiac=*blank;
040100170213           prmRpyOpCode = FNLGw00_RPYOPCODE_ERROR;
040200170213           prmRpyIdMsg = FNLGw00_ESITO_ID_RICHIAMO_ERRATO;
040300140520           exsr RoutEnd;
040400140520         endif;
040500140520       //?Accesso per cliente unificante e ID Bolla
040600140519         if wksu <>*blanks;
040700140520       // ID Bolla mancante
040800140520           if widbolla=*blanks;
040900170213              prmRpyOpCode = FNLGw00_RPYOPCODE_ERROR;
041000140527              wIdMsg   = 'TIS0734';
041100140527              wIdCampo = 'IDBOLLA   ';
041200140527              clear wParm;
041300140527              exsr Messaggi;
041400140520              exsr RoutEnd;
041500140520           endif;
041600141003       // KSU può contenere solo numeri
041700141003           if %check(digits:wksu)>0;
041800170216               prmRpyOpCode = FNLGw00_RPYOPCODE_ERROR;
041900170216               wIdMsg   = 'TIS0736';
042000170216               wIdCampo = 'KSU       ';
042100170216               wParm = widbolla;
042200170216               exsr Messaggi;
042300170216               exsr RoutEnd;
042400141003           endif;
042500140520       // Verifico correttezza dell'ID bolla
042600140520       // Deve contenere solo numeri
042700140520         if %check(digits:widbolla)>0;
042800170216              prmRpyOpCode = FNLGw00_RPYOPCODE_ERROR;
042900140527              wIdMsg   = 'TIS0734';
043000140527              wIdCampo = 'IDBOLLA   ';
043100140527              clear wParm;
043200140527              exsr Messaggi;
043300140520              exsr RoutEnd;
043400170217            else;
043500170215          wbolla=widbolla;
043600140520         endif;
043700170216        endif;
043800170215
043900140529       // Se ricevuto anche il BRTCODE lo controllo
044000140529         if wbrtcode<>*blanks;
044100140529            exsr sr_trul28;
044200170215          wbolla=wbrtcode;
044300140529         endif;
044400170215       // Se ricevuto keygiac imposto wbolla per lettura giacenza
044500170215         if wkeygiac<>*blanks;
044600170220          wbolla=%subst(wkeygiac:3:14);
044700170215         endif;
044800170215       //?Imposto riferimenti bolla per lettura giacenza
044900170215          kaas=2000+%dec(%subst(wbolla:1:2):2:0);
045000170215          klnp=%dec(%subst(wbolla:3:3):3:0);
045100170215          knrs=%dec(%subst(wbolla:6:2):2:0);
045200170215          knsp=%dec(%subst(wbolla:8:7):7:0);
045300170215           chain (kaas:klnp:knrs:knsp) tigcp01l ;
045400170215            if %found(tigcp01l);
045500170316       //?Controllo se arrivo quindi destinatario se VPO attiva
045600170316               if wchi = 'A' and
045700170316                   (%lookup(gcpfgc:Fil)=0  and
045800170316                    %lookup(999:fil)=0 );
045900170316                prmRpyOpCode = FNLGw00_RPYOPCODE_ERROR;
046000170403             //   wIdMsg   = 'TISXXXX';
046100170403             // wIdCampo = 'IDBOLLA   ';
046200170403             //   wParm=wbolla;
046300170403             //   exsr Messaggi;
046400170316                exsr RoutEnd;
046500170316               endif;
046600170316       //
046700170215              chain (kaas:klnp:knrs:knsp) fnarb01l ;
046800170215               if %found(fnarb01l);
046900170215                exsr CheckDispo;
047000170217               else;
047100170217                prmRpyOpCode = FNLGw00_RPYOPCODE_ERROR;
047200170217                wIdMsg   = 'TIS0562';
047300170217                wIdCampo = 'IDBOLLA   ';
047400170217                wParm=wbolla;
047500170217                exsr Messaggi;
047600170217                exsr RoutEnd;
047700170217               endif;
047800170217             else;
047900170217               prmRpyOpCode = FNLGw00_RPYOPCODE_ERROR;
048000170217               wIdMsg   = 'TIS0198';
048100170217               wIdCampo = 'IDBOLLA   ';
048200170217               wParm=wbolla;
048300170217               exsr Messaggi;
048400170217               exsr RoutEnd;
048500170215            endif;
048600131008       ENDSR;
048700140529       //--------------------------------------------------------------
048800140529       //?Controllo correttezza BRTCODE
048900140529       //--------------------------------------------------------------
049000140529       BEGSR  sr_trul28 ;
049100140529          clear trul28ds0;
049200140529          i280cod=wbrtcode;
049300140529          trul28r0(trul28ds0);
049400140529          if o280err='1';
049500170213             prmRpyOpCode = FNLGw00_RPYOPCODE_ERROR;
049600140529             wIdMsg   = 'TIS0737';
049700140529             wIdCampo = 'BRTCODE   ';
049800140530             wparm = wbrtcode ;
049900140529             exsr Messaggi;
050000140529             exsr RoutEnd;
050100140529          endif;
050200140529       ENDSR;
050300140527       //--------------------------------------------------------------
050400170216       //?Routine per schierare i messaggi nella ds FNLGw00M0
050500140527       //--------------------------------------------------------------
050600140527       BEGSR  Messaggi;
050700140527
050800140527       //?Se ho già caricato 99 messaggi esco
050900170213         IF  fnLGw00M0.nrmsg = 99;
051000140527           exsr RoutEnd;
051100170213           clear fnLGw00M0.nrmsg;
051200140527         ENDIF;
051300140527
051400170213         fnLGw00M0.nrmsg += 1;
051500170213         fnLGw00M0.skIdMsg(fnLGw00M0.nrmsg) = wIdMsg;
051600170213         fnLGw00M0.skIdCampo(fnLGw00M0.nrmsg) = wIdCampo;
051700170213         fnLGw00M0.skErrWarn(fnLGw00M0.nrmsg) = FNLGw00_MSG_ERRORE;
051800140527         IF  wParm <> *blanks;
051900170213           fnLGw00M0.skTextMsg (fnLGw00M0.nrmsg) =
052000140527           rtvMsgLang(wIdMsg:wIdlingua:wParm);
052100140527         ELSE;
052200140611       // Se non ho ID messaggio significa che ho già il testo del messaggio
052300140611           if wIdMsg=*blanks;
052400170213             fnLGw00M0.skTextMsg (fnLGw00M0.nrmsg)=wTextMsg;
052500140611           else;
052600170213             fnLGw00M0.skTextMsg (fnLGw00M0.nrmsg)=
052700140611             rtvMsgLang(wIdMsg:wIdlingua);
052800140611           endif;
052900140527         ENDIF;
053000140527
053100140527       ENDSR;
053200131010
053300131010       //--------------------------------------------------------------
053400131010       //?Operazioni finali.
053500131010       //--------------------------------------------------------------
053600131010       BEGSR  RoutEnd;
053700140408
053800170213       //?Se richiamato per disposizioni
053900170213           %subst(prmRpyData:1:prmRpyDataSize) = fnLGw00o0;
054000131010
054100170213          %subst(prmRpyMessage:1:prmRpyMsgSize) = fnLGw00M0;
054200131010
054300131010         return;
054400131010
054500131010       ENDSR;
054600131009
054700131007      /end-free

000100091228       //==============================================================
000200140415       // Driver scrittura FNVAOEWR
000300091228       //==============================================================
000400091228
000500091228       //--------------------------------------------------------------
000600121106       // Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700091228       //--------------------------------------------------------------
000800091228
000900100325     /*PRM dbgview(*source)
001000091223     /*END
001100091228
001200091228       //--------------------------------------------------------------
001300121106       // Specifiche di controllo.
001400091228       //--------------------------------------------------------------
001500091223
001600091223     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700140415     H DFTACTGRP(*NO) BNDDIR('TRUL')
001800091223     h alwnull(*inputonly)
001900091223
002000091223       //--------------------------------------------------------------
002100121106       // Dichiarazione file.
002200091223       //--------------------------------------------------------------
002300100112
002400140415     FFNVAOEWR  O    E             DISK    usropn prefix(x_)
002500140416     FTIORE00F  O    E             DISK    usropn prefix(t_)
002600091223
002700091223       //--------------------------------------------------------------
002800121106       // Definizione costanti.
002900091223       //--------------------------------------------------------------
003000100302
003100091223
003200091223       //--------------------------------------------------------------
003300121106       // Definizione schiere.
003400091223       //--------------------------------------------------------------
003500091223
003600091223
003700091223       //--------------------------------------------------------------
003800121106       // Definizione aree dati.
003900091223       //--------------------------------------------------------------
004000091223
004100091223
004200091223       //--------------------------------------------------------------
004300121106       // Definizione strutture dati.
004400091223       //--------------------------------------------------------------
004500110516
004600140415     D TIVLR00F      e ds
004700151106     D FNVAOEIIR     e ds
004800170615     D DOREDT        e ds
004900170615     D DOREGEN       e ds
005000140415     D DORESMS       e ds
005100140415     D DOREORARI     e ds
005200151106     D DOREOR        e ds
005300140415     D dscmz         e ds                  inz
005400140415
005500091223
005600091223       //--------------------------------------------------------------
005700121106       // Definizione variabili.
005800091223       //--------------------------------------------------------------
005900091223
006000121106       // - Parametri ricevuti:
006100130307     d pIn_Opz         s              1a
006200140416     d pIn_OREPRG      s                   like(t_orePRG)
006300140528     d pIn_Dst         s              3a
006400130307     d pOut_Esito      s              1a
006500121106
006600121106       // Campi di comodo
006700121106     d wDate           s              8  0 inz
006800130307     d wEsito          s              1a
006900140415     d wHDL            s                   like(VLRHDL)
007000091223
007100091223       //--------------------------------------------------------------
007200121106       // Definizione prototipi procedure.
007300091223       //--------------------------------------------------------------
007400140415
007500140415     d TITVVOEC        pr                  extpgm('TITVVOEC')
007600140415     d  parccm                        8
007700140415     d  parmbr                       10
007800140415     d  paropz                        1
007900140415
008000140415     d TIS711C         pr                  extpgm('TIS711C')
008100140415     d  cmz711                      110
008200140415     d  esito711                      1
008300130307
008400091223       //--------------------------------------------------------------
008500121106       // Definizione key-list.
008600091223       //--------------------------------------------------------------
008700091223
008800091223
008900091223       //--------------------------------------------------------------
009000121107       // Definizione parametri procedura.
009100091223       //--------------------------------------------------------------
009200091223
009300091223     c     *Entry        plist
009400130307     c                   parm                    pIn_Opz
009500140415     c                   parm                    TIVLR00F
009600151106     c                   parm                    FNVAOEIIR
009700140416     c                   parm                    pIn_OREPRG
009800140528     c                   parm                    pIn_Dst
009900130307     c                   parm                    pOut_Esito
010000091223
010100130307      /free
010200091223
010300091223       //--------------------------------------------------------------
010400121106       // M A I N - L I N E
010500091223       //--------------------------------------------------------------
010600130307
010700130307       // Operazioni iniziali?
010800130307       exsr sr_RoutInz;
010900130307
011000130307       // attività richiesta dal chiamante
011100130307       select;
011200140415         // OPEN
011300130307         when pIn_Opz = 'O';
011400140416              exsr sr_open;
011500130307         // WRITE
011600130307         when pIn_Opz = 'W';
011700140416              exsr sr_write;
011800130307         // CLOSE
011900130307         when pIn_Opz = 'C';
012000140416              exsr sr_close;
012100140528         // SEND
012200140528         when pIn_Opz = 'S';
012300140528              exsr sr_send;
012400130307         // richiesta errata
012500130307         other;
012600140416              pOut_Esito = '2';
012700140416              exsr sr_RoutEnd;
012800130307       endsl;
012900091223
013000121106       // Operazioni finali?
013100091223       exsr sr_RoutEnd;
013200091223
013300091223       //--------------------------------------------------------------
013400121107       // Operazioni iniziali.
013500091223       //--------------------------------------------------------------
013600091223       BEGSR  sr_RoutInz;
013700130307
013800130307         pOut_Esito = '1';
013900121108
014000121108         // Reperimento data odierna (fmt aaaa/mm/gg)
014100121108         wDate = %dec( %date() );
014200091223
014300091223       ENDSR;
014400110523
014500110523       //--------------------------------------------------------------
014600130307       // OPEN
014700110523       //--------------------------------------------------------------
014800130307       BEGSR  sr_open;
014900140415
015000140415         wHDL = VLRHDL;
015100140415         %subst(wHDL:1:1) = 'P';
015200140415         wEsito = '1';
015300140415
015400140415         monitor;
015500140415           TITVVOEC(VLRKSC:wHDL:wEsito);
015600140415         on-error;
015700140415           pOut_Esito = '2';
015800140415           exsr sr_RoutEnd;
015900140415         endmon;
016000121106
016100121106       ENDSR;
016200130307
016300130307       //--------------------------------------------------------------
016400130307       // WRITE
016500130307       //--------------------------------------------------------------
016600130307       BEGSR  sr_write;
016700130307
016800140415         if not %open(fnvaoewr);
016900140415           open fnvaoewr;
017000130307         endif;
017100140416         if not %open(tiore00f);
017200140416           open tiore00f;
017300140416         endif;
017400140415
017500140415         PiStr=VAOCOR;
017600140415         exsr CHKNUM;
017700140415         if PiInt=*on;
017800140415           x_VAOECOR = PiVal;
017900140415         else;
018000140415           pOut_Esito = '2';
018100140415           exsr sr_RoutEnd;
018200140415         endif;
018300140415         PiStr=VAOPOE;
018400140415         exsr CHKNUM;
018500140415         if PiInt=*on;
018600140415           x_VAOEPOE = PiVal;
018700140415         else;
018800140415           pOut_Esito = '2';
018900140415           exsr sr_RoutEnd;
019000140415         endif;
019100140415         PiStr=VAONSR;
019200140416         if %trim(PiStr) <> *blanks AND %trim(PiStr) <> *zeros;
019300140416            exsr CHKNUM;
019400140416            if PiInt=*on;
019500140416              x_VAOENSR = PiVal;
019600140416            else;
019700140416              pOut_Esito = '2';
019800140416              exsr sr_RoutEnd;
019900140416            endif;
020000140416         endif;
020100140415         PiStr=VAONOR;
020200140416         if %trim(PiStr) <> *blanks AND %trim(PiStr) <> *zeros;
020300140416            exsr CHKNUM;
020400140416            if PiInt=*on;
020500140416              x_VAOENOR = PiVal;
020600140416            else;
020700140416              pOut_Esito = '2';
020800140416              exsr sr_RoutEnd;
020900140416            endif;
021000140416         endif;
021100140415         PiStr=VAONRV;
021200140416         if %trim(PiStr) <> *blanks AND %trim(PiStr) <> *zeros;
021300140416            exsr CHKNUM;
021400140416            if PiInt=*on;
021500140416              x_VAOENRV = PiVal;
021600140416            else;
021700140416              pOut_Esito = '2';
021800140416              exsr sr_RoutEnd;
021900140416            endif;
022000140416         endif;
022100140415
022200140415         //TRC = G
022300140415         x_VAOETRC = 'G';
022400140415         clear DOREGEN;
022500140415         §ORETSP = VAOTSPS;
022600140415         §OREFIMV = VAOEMLRF;
022700140415         §OREFISV = VAOSMSRF;
022800140415         §OREORIP = VAOORIP ;
022900140415         §OREDOIO = VAODOIO ;
023000151106         §OREFMCO = VAOICAO ;
023100160321         §OREFSCO = VAOFCAO ;
023200160727         §OREMEMO = VAOMEMO ;
023300140415         x_VAOEDATI = DOREGEN;
023400140416         if x_VAOEDATI <> *blanks;
023500140416            write fnvaoe00;
023600140416            exsr sr_WriTIORE;
023700140416         endif;
023800140415
023900140415         //TRC = MA
024000140415         x_VAOETRC = 'MA';
024100140415         x_VAOEDATI = VAOEMLR;
024200140416         if x_VAOEDATI <> *blanks;
024300140416            write fnvaoe00;
024400140416            exsr sr_WriTIORE;
024500140416         endif;
024600140415
024700140415         //TRC = N
024800140415         x_VAOETRC = 'N';
024900140415         x_VAOEDATI = VAONOTE;
025000140416         if x_VAOEDATI <> *blanks;
025100140416            write fnvaoe00;
025200140416            exsr sr_WriTIORE;
025300140416         endif;
025400140415
025500140415         //TRC = S
025600140415         x_VAOETRC = 'S';
025700140415         clear DORESMS;
025800140415         §ORESMS = VAOSMSR;
025900140416         x_VAOEDATI = DORESMS;
026000140416         if x_VAOEDATI <> *blanks;
026100140416            write fnvaoe00;
026200140416            exsr sr_WriTIORE;
026300140416         endif;
026400151106
026500151106         //TRC = MC = Mail Conferma
026600151106         x_VAOETRC = 'MC';
026700151106         x_VAOEDATI = VAOECAO;
026800151106         if x_VAOEDATI <> *blanks;
026900151106            write fnvaoe00;
027000151106            exsr sr_WriTIORE;
027100151106         endif;
027200160321
027300160321         //TRC = SC = Alert accettazione ORM via SMS
027400160321         x_VAOETRC = 'SC';
027500160321         x_VAOEDATI = VAOSCAO;
027600160321         if x_VAOEDATI <> *blanks;
027700160321            write fnvaoe00;
027800160321            exsr sr_WriTIORE;
027900160321         endif;
028000160225
028100160225         //TRC = NP = Numero Prenotato
028200160225         x_VAOETRC = 'NP';
028300160225         x_VAOEDATI = VAONPR;
028400160225         if x_VAOEDATI <> *blanks;
028500160225            write fnvaoe00;
028600160225            exsr sr_WriTIORE;
028700160225         endif;
028800151106
028900151106         //TRC = OR = Orari
029000151106         x_VAOETRC = 'OR';
029100151106         clear DOREOR;
029200151116         *in50 = *on;
029300151116
029400151116         // Verifico se nessun orario indicato => non scrivo il record
029500151116         if VAOORARD = *blanks AND
029600151116            VAOORARA = *blanks;
029700151116            *in50 = *off;
029800151116         endif;
029900151116
030000151106         PiStr=VAOORARD;
030100151106         exsr CHKNUM;
030200151106         if PiInt=*on;
030300151106           §OREORI = PiVal;
030400151106         else;
030500151106           // scrivo quelli corretti
030600151106         endif;
030700151106         PiStr=VAOORARA;
030800151106         exsr CHKNUM;
030900151106         if PiInt=*on;
031000151119           §OREORF = PiVal;
031100151106         else;
031200151106           // scrivo quelli corretti
031300151106         endif;
031400151106         x_VAOEDATI = DOREOR;
031500151116         if *in50 AND x_VAOEDATI <> *blanks;
031600151106            write fnvaoe00;
031700151106            exsr sr_WriTIORE;
031800151106         endif;
031900140415
032000140415         //TRC = O
032100140415         x_VAOETRC = 'O';
032200140415         clear DOREORARI;
032300140417         *in50 = *on;
032400140417
032500140417         // Verifico se nessun orario indicato => non scrivo il record
032600140417         if VAOORDA1 = *blanks AND
032700140417            VAOORAA1 = *blanks AND
032800140417            VAOORDA2 = *blanks AND
032900140417            VAOORAA1 = *blanks;
033000140417            *in50 = *off;
033100140417         endif;
033200140417
033300140417         PiStr=VAOORDA1;
033400140417         exsr CHKNUM;
033500140417         if PiInt=*on;
033600140417           §OREORAMDA = PiVal;
033700140417         else;
033800140417           // scrivo quelli corretti
033900140417         endif;
034000140417         PiStr=VAOORAA1;
034100140417         exsr CHKNUM;
034200140417         if PiInt=*on;
034300140417           §OREORAMA  = PiVal;
034400140417         else;
034500140417           // scrivo quelli corretti
034600140417         endif;
034700140417         PiStr=VAOORDA2;
034800140417         exsr CHKNUM;
034900140417         if PiInt=*on;
035000140417           §OREORAPDA = PiVal;
035100140417         else;
035200140417           // scrivo quelli corretti
035300140417         endif;
035400140417         PiStr=VAOORAA2;
035500140417         exsr CHKNUM;
035600140417         if PiInt=*on;
035700140417           §OREORAPA  = PiVal;
035800140417         else;
035900140417           // scrivo quelli corretti
036000140417         endif;
036100140417
036200140415         x_VAOEDATI = DOREORARI;
036300140417         if *in50 AND x_VAOEDATI <> *blanks;
036400140416            write fnvaoe00;
036500140416            exsr sr_WriTIORE;
036600140416         endif;
036700170615
036800170615         //TRC = DT
036900170615         x_VAOETRC = 'DT';
037000170615         clear DOREDT;
037100170615         if VAODDPM > *zeros;
037200170615           §OREDPM = %dec(VAODDPM:8:0);
037300170615         endif;
037400170615         if VAODDAR <> *blank;
037500170615           §OREDAR = VAODDAR;
037600170615         endif;
037700170615         if VAODPOSD <> *blank;
037800170615           §OREPOSD = VAODPOSD;
037900170615         endif;
038000170615         if VAODANT <> *blank;
038100170615           §OREANT = VAODANT;
038200170615         endif;
038300170615         x_VAOEDATI = DOREDT;
038400170615         if x_VAOEDATI <> *blanks;
038500170615            write fnvaoe00;
038600170615            exsr sr_WriTIORE;
038700170615         endif;
038800130307
038900130307       ENDSR;
039000140416
039100140416       //--------------------------------------------------------------
039200140416       // Scrittura TIORE00F
039300140416       //--------------------------------------------------------------
039400140416       BEGSR  sr_WriTIORE;
039500140416
039600140416           if %subst(vlrppt:4:1) <> 'T';
039700140416              t_OREPRG   = pIn_OREPRG;
039800140416              t_VAOETRC  = x_VAOETRC;
039900140416              t_VAOEDATI = x_VAOEDATI;
040000140416              write tiore000;
040100140416           endif;
040200140416
040300140416       ENDSR;
040400140528
040500140528       //--------------------------------------------------------------
040600140528       // CLOSE
040700140528       //--------------------------------------------------------------
040800140528       BEGSR  sr_close;
040900140528
041000140528         //chiudo il file
041100140528         if %open(fnvaoewr);
041200140528           close fnvaoewr;
041300140528         endif;
041400140528         if %open(tiore00f);
041500140528           close tiore00f;
041600140528         endif;
041700140528
041800140528       ENDSR;
041900140528
042000140528       //--------------------------------------------------------------
042100140528       // SEND
042200140528       //--------------------------------------------------------------
042300140528       BEGSR  sr_send;
042400140528
042500140528         wHDL = VLRHDL;
042600140528         %subst(wHDL:1:1) = 'P';
042700140528         wEsito=*blank;
042800140528
042900140528         reset dscmz;
043000140528         cmzdst = pIn_Dst;
043100140528         cmzfld = 'FNVAOEWR';
043200140528         cmzmbd = wHDL;
043300140528         cmzfla = 'FNVAOE0F';
043400140528         cmzmba = 'FNVAOE0F';
043500140528         cmznrr = *zeros;
043600140528         cmzlba = vlrfl1;
043700140528         monitor;
043800140528           TIS711C(dscmz:wEsito);
043900140528         on-error;
044000140528           pOut_Esito = '2';
044100140528           exsr sr_RoutEnd;
044200140528         endmon;
044300140528         if cmzerr = '1';
044400140528           pOut_Esito = '2';
044500140528           exsr sr_RoutEnd;
044600140528         endif;
044700140528
044800140528         // se tutto OK
044900140528         wEsito = '2';
045000140528         TITVVOEC(VLRKSC:wHDL:wEsito);
045100140528         if %error;
045200140528           pOut_Esito = '2';
045300140528           exsr sr_RoutEnd;
045400140528         endif;
045500140528
045600140528       ENDSR;
045700121106
045800091223       //--------------------------------------------------------------
045900121107       // Operazioni finali.
046000091223       //--------------------------------------------------------------
046100091223       BEGSR  sr_RoutEnd;
046200130109
046300130109         // Chiusura pgm
046400130307         if pIn_Opz = 'C' or pOut_Esito='2';
046500130307           *inlr = *on;
046600130307         else;
046700130307           *inrt = *on;
046800130307         endif;
046900130109         return;
047000091223
047100091223       ENDSR;
047200091223
047300091223      /end-free
047400140415
047500140415     C*----------------------------------------------------*
047600140415     C*  CONTROLLO NUMERICITA' CAMPI
047700140415     C*----------------------------------------------------*
047800140415     C     CHKNUM        BEGSR
047900140415     C*
048000140415     C                   call(e)   'ISNUMERIC'
048100140415     C                   PARM                    PiStr            30
048200140415     C                   PARM      '.'           PiDecChr          1
048300140415     C                   PARM      *ZEROS        PiVal            30 9
048400140415     C                   PARM      '0'           PiInt             1
048500140415     C                   PARM      '0'           PiNum             1
048600140415     C                   IF        %error
048700140415     C                   EVAL      PiInt=*off
048800140415     C                   ENDIF
048900140415     C*
049000140415     C                   ENDSR

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
004700140519     D FNVAO01R      e ds
004800140415     D DOREGEN       e ds
004900140415     D DORESMS       e ds
005000140415     D DOREORARI     e ds
005100140415     D dscmz         e ds                  inz
005200140415
005300091223
005400091223       //--------------------------------------------------------------
005500121106       // Definizione variabili.
005600091223       //--------------------------------------------------------------
005700091223
005800121106       // - Parametri ricevuti:
005900130307     d pIn_Opz         s              1a
006000140416     d pIn_OREPRG      s                   like(t_orePRG)
006100140528     d pIn_Dst         s              3a
006200130307     d pOut_Esito      s              1a
006300121106
006400121106       // Campi di comodo
006500121106     d wDate           s              8  0 inz
006600130307     d wEsito          s              1a
006700140415     d wHDL            s                   like(VLRHDL)
006800091223
006900091223       //--------------------------------------------------------------
007000121106       // Definizione prototipi procedure.
007100091223       //--------------------------------------------------------------
007200140415
007300140415     d TITVVOEC        pr                  extpgm('TITVVOEC')
007400140415     d  parccm                        8
007500140415     d  parmbr                       10
007600140415     d  paropz                        1
007700140415
007800140415     d TIS711C         pr                  extpgm('TIS711C')
007900140415     d  cmz711                      110
008000140415     d  esito711                      1
008100130307
008200091223       //--------------------------------------------------------------
008300121106       // Definizione key-list.
008400091223       //--------------------------------------------------------------
008500091223
008600091223
008700091223       //--------------------------------------------------------------
008800121107       // Definizione parametri procedura.
008900091223       //--------------------------------------------------------------
009000091223
009100091223     c     *Entry        plist
009200130307     c                   parm                    pIn_Opz
009300140415     c                   parm                    TIVLR00F
009400140519     c                   parm                    FNVAO01R
009500140416     c                   parm                    pIn_OREPRG
009600140528     c                   parm                    pIn_Dst
009700130307     c                   parm                    pOut_Esito
009800091223
009900130307      /free
010000091223
010100091223       //--------------------------------------------------------------
010200121106       // M A I N - L I N E
010300091223       //--------------------------------------------------------------
010400130307
010500130307       // Operazioni iniziali?
010600130307       exsr sr_RoutInz;
010700130307
010800130307       // attivit� richiesta dal chiamante
010900130307       select;
011000140415         // OPEN
011100130307         when pIn_Opz = 'O';
011200140416              exsr sr_open;
011300130307         // WRITE
011400130307         when pIn_Opz = 'W';
011500140416              exsr sr_write;
011600130307         // CLOSE
011700130307         when pIn_Opz = 'C';
011800140416              exsr sr_close;
011900130307         // richiesta errata
012000130307         other;
012100140416              pOut_Esito = '2';
012200140416              exsr sr_RoutEnd;
012300130307       endsl;
012400091223
012500121106       // Operazioni finali?
012600091223       exsr sr_RoutEnd;
012700091223
012800091223       //--------------------------------------------------------------
012900121107       // Operazioni iniziali.
013000091223       //--------------------------------------------------------------
013100091223       BEGSR  sr_RoutInz;
013200130307
013300130307         pOut_Esito = '1';
013400121108
013500121108         // Reperimento data odierna (fmt aaaa/mm/gg)
013600121108         wDate = %dec( %date() );
013700091223
013800091223       ENDSR;
013900110523
014000110523       //--------------------------------------------------------------
014100130307       // OPEN
014200110523       //--------------------------------------------------------------
014300130307       BEGSR  sr_open;
014400140415
014500140415         wHDL = VLRHDL;
014600140415         %subst(wHDL:1:1) = 'P';
014700140415         wEsito = '1';
014800140415
014900140415         monitor;
015000140415           TITVVOEC(VLRKSC:wHDL:wEsito);
015100140415         on-error;
015200140415           pOut_Esito = '2';
015300140415           exsr sr_RoutEnd;
015400140415         endmon;
015500121106
015600121106       ENDSR;
015700130307
015800130307       //--------------------------------------------------------------
015900130307       // WRITE
016000130307       //--------------------------------------------------------------
016100130307       BEGSR  sr_write;
016200130307
016300140415         if not %open(fnvaoewr);
016400140415           open fnvaoewr;
016500130307         endif;
016600140416         if not %open(tiore00f);
016700140416           open tiore00f;
016800140416         endif;
016900140415
017000140519         x_VAOECOR = VAOCOR;
017100140519         x_VAOEPOE = VAOPOE;
017200140519         x_VAOENSR = VAONSR;
017300140519         x_VAOENOR = VAONOR;
017400140519         x_VAOENRV = VAONRV;
017500140415
017600140415         //TRC = G
017700140415         x_VAOETRC = 'G';
017800140415         clear DOREGEN;
017900140415         �ORETSP = VAOTSPS;
018000140415         �OREFIMV = VAOEMLRF;
018100140415         �OREFISV = VAOSMSRF;
018200140415         �OREORIP = VAOORIP ;
018300140415         �OREDOIO = VAODOIO ;
018400140415         x_VAOEDATI = DOREGEN;
018500140416         if x_VAOEDATI <> *blanks;
018600140416            write fnvaoe00;
018700140416            exsr sr_WriTIORE;
018800140416         endif;
018900140415
019000140415         //TRC = MA
019100140415         x_VAOETRC = 'MA';
019200140415         x_VAOEDATI = VAOEMLR;
019300140416         if x_VAOEDATI <> *blanks;
019400140416            write fnvaoe00;
019500140416            exsr sr_WriTIORE;
019600140416         endif;
019700140415
019800140415         //TRC = N
019900140415         x_VAOETRC = 'N';
020000140415         x_VAOEDATI = VAONOTE;
020100140416         if x_VAOEDATI <> *blanks;
020200140416            write fnvaoe00;
020300140416            exsr sr_WriTIORE;
020400140416         endif;
020500140415
020600140415         //TRC = S
020700140415         x_VAOETRC = 'S';
020800140415         clear DORESMS;
020900140415         �ORESMS = VAOSMSR;
021000140416         x_VAOEDATI = DORESMS;
021100140416         if x_VAOEDATI <> *blanks;
021200140416            write fnvaoe00;
021300140416            exsr sr_WriTIORE;
021400140416         endif;
021500140415
021600140415         //TRC = O
021700140415         x_VAOETRC = 'O';
021800140415         clear DOREORARI;
021900140417         *in50 = *on;
022000140417
022100140417         // Verifico se nessun orario indicato => non scrivo il record
022200140519         if VAOORDA1 = *zero   AND
022300140519            VAOORAA1 = *zero   AND
022400140519            VAOORDA2 = *zero   AND
022500140519            VAOORAA1 = *zero  ;
022600140417            *in50 = *off;
022700140417         endif;
022800140417
022900140519         �OREORAMDA = VAOORDA1;
023000140519         �OREORAMA  = VAOORAA1;
023100140519         �OREORAPDA = VAOORDA2;
023200140519         �OREORAPA  = VAOORAA2;
023300140417
023400140415         x_VAOEDATI = DOREORARI;
023500140417         if *in50 AND x_VAOEDATI <> *blanks;
023600140416            write fnvaoe00;
023700140416            exsr sr_WriTIORE;
023800140416         endif;
023900130307
024000130307       ENDSR;
024100140416
024200140416       //--------------------------------------------------------------
024300140416       // Scrittura TIORE00F
024400140416       //--------------------------------------------------------------
024500140416       BEGSR  sr_WriTIORE;
024600140416
024700140416           if %subst(vlrppt:4:1) <> 'T';
024800140416              t_OREPRG   = pIn_OREPRG;
024900140416              t_VAOETRC  = x_VAOETRC;
025000140416              t_VAOEDATI = x_VAOEDATI;
025100140416              write tiore000;
025200140416           endif;
025300140416
025400140416       ENDSR;
025500130307
025600130307       //--------------------------------------------------------------
025700130307       // CLOSE
025800130307       //--------------------------------------------------------------
025900130307       BEGSR  sr_close;
026000130318
026100130318         //chiudo il file
026200140415         if %open(fnvaoewr);
026300140415           close fnvaoewr;
026400130318         endif;
026500140416         if %open(tiore00f);
026600140416           close tiore00f;
026700140416         endif;
026800140415
026900140415         wHDL = VLRHDL;
027000140415         %subst(wHDL:1:1) = 'P';
027100140415         wEsito=*blank;
027200140415
027300140415         reset dscmz;
027400140528         cmzdst = pIn_Dst;
027500140415         cmzfld = 'FNVAOEWR';
027600140415         cmzmbd = wHDL;
027700140415         cmzfla = 'FNVAOE0F';
027800140415         cmzmba = 'FNVAOE0F';
027900140415         cmznrr = *zeros;
028000140415         cmzlba = vlrfl1;
028100140415         monitor;
028200140415           TIS711C(dscmz:wEsito);
028300140415         on-error;
028400140415           pOut_Esito = '2';
028500140415           exsr sr_RoutEnd;
028600140415         endmon;
028700140415         if cmzerr = '1';
028800140415           pOut_Esito = '2';
028900140415           exsr sr_RoutEnd;
029000140415         endif;
029100140415
029200140415         // se tutto OK
029300140415         wEsito = '2';
029400140415         TITVVOEC(VLRKSC:wHDL:wEsito);
029500140415         if %error;
029600140415           pOut_Esito = '2';
029700140415           exsr sr_RoutEnd;
029800140415         endif;
029900130307
030000130307       ENDSR;
030100121106
030200091223       //--------------------------------------------------------------
030300121107       // Operazioni finali.
030400091223       //--------------------------------------------------------------
030500091223       BEGSR  sr_RoutEnd;
030600130109
030700130109         // Chiusura pgm
030800130307         if pIn_Opz = 'C' or pOut_Esito='2';
030900130307           *inlr = *on;
031000130307         else;
031100130307           *inrt = *on;
031200130307         endif;
031300130109         return;
031400091223
031500091223       ENDSR;
031600091223
031700091223      /end-free
031800140415
031900140415     C*----------------------------------------------------*
032000140415     C*  CONTROLLO NUMERICITA' CAMPI
032100140415     C*----------------------------------------------------*
032200140415     C     CHKNUM        BEGSR
032300140415     C*
032400140415     C                   call(e)   'ISNUMERIC'
032500140415     C                   PARM                    PiStr            30
032600140415     C                   PARM      '.'           PiDecChr          1
032700140415     C                   PARM      *ZEROS        PiVal            30 9
032800140415     C                   PARM      '0'           PiInt             1
032900140415     C                   PARM      '0'           PiNum             1
033000140415     C                   IF        %error
033100140415     C                   EVAL      PiInt=*off
033200140415     C                   ENDIF
033300140415     C*
033400140415     C                   ENDSR

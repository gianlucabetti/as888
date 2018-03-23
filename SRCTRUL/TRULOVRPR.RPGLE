000100080819      *PARMS DFTACTGRP(*NO) ACTGRP(*CALLER) OPTION(*NOXREF)
000200080819       //--------------------------------------------------------------
000300080819       //?Gestione OVERRIDE ai file di stampa delle LdV
000400080819       //--------------------------------------------------------------
000500080819
000600080819     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700080819     h dftactgrp(*no) actgrp(*caller)
000800080819
000900080819       //--------------------------------------------------------------
001000080819       //?Dichiarazione file.
001100080819       //--------------------------------------------------------------
001200080819
001300080819
001400080819       //--------------------------------------------------------------
001500080819       //?Definizione costanti.
001600080819       //--------------------------------------------------------------
001700080819
001800080819       // - Lettere maiuscole
001900080820     d c_Up            c                   const('ABCDEFGHIJKLMNOPQRST+
002000080820     d                                            UVWXYZ')
002100080819       // - Lettere minuscole
002200080820     d c_Lo            c                   const('abcdefghijklmnopqrst+
002300080820     d                                            uvwxyz')
002400080819
002500080819       //--------------------------------------------------------------
002600080819       //?Definizione schiere.
002700080819       //--------------------------------------------------------------
002800080819
002900080819
003000080819       //--------------------------------------------------------------
003100080819       //?Definizione aree dati.
003200080819       //--------------------------------------------------------------
003300080819
003400080819
003500080819       //--------------------------------------------------------------
003600080819       //?Definizione strutture dati.
003700080819       //--------------------------------------------------------------
003800080819
003900080819       // - Parametri
004000080819     d trulOVRPds    e ds
004100080819
004200080819       // - Status
004300080819     d Status         sds
004400080819     d   SDSpgm          *proc
004500080819     d   SDSprm          *parms
004600080819     d***JobName             244    253                                         Job name
004700080819     d***JobUser             254    263                                         User name
004800080819     d***JobNumber           264    269s 0                                      Job number
004900080819
005000080819       //--------------------------------------------------------------
005100080819       //?Definizione variabili globali.
005200080819       //--------------------------------------------------------------
005300080819
005400080819       // - Campi per QCMDEXC
005500080902     d Qcmd            s                   like(uopCmd)      inz
005600080819
005700080819       // - Variabili di comodo
005800080820     d wPrtF           s                   like(uopPrtF)     inz
005900080820     d wOutQ           s                   like(uopOutQ)     inz
006000080820     d wFormType       s                   like(uopFrmTyp)   inz
006100080819     d wUsrDta         s                   like(uopUsrDta)   inz
006200080819
006300080819       //--------------------------------------------------------------
006400080819       //?Definizione procedure usate.
006500080819       //--------------------------------------------------------------
006600080819
006700080819       // - Esecuzione comando di sistema
006800080819     d qCmdExc         pr                  extpgm('QCMDEXC')
006900080819     d  Qcmd                        512    const  options(*varsize)
007000080819     d  Qlen                         15  5 const
007100080819
007200080819       //--------------------------------------------------------------
007300080819       //?Definizione key-list.
007400080819       //--------------------------------------------------------------
007500080819
007600080819
007700080819       //--------------------------------------------------------------
007800080819       //?M A I N - L I N E
007900080819       //--------------------------------------------------------------
008000080819
008100080819     c     *Entry        plist
008200080819     c                   parm                    trulOVRPds
008300090921
008400080819      /free
008500080819
008600080819       // Operazioni iniziali
008700080819       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
008800080819       exsr sr_RoutInz;
008900080819
009000080819       // Gestione comando di override richiesto
009100080819       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
009200080819       select;
009300090921
009400090921         // - Rilevato errore nei parametri
009500090921         when  uopErr = *on;
009600090921           exsr  sr_RoutEnd;
009700080820
009800080820         // - Override al singolo PrtF
009900090921         when (uopToDo = 'A'   or   uopToDo = '1');
010000080819           exsr  sr_Ovr_1;
010100080820
010200080820         // - Cancellazione override dal singolo PrtF
010300090921         when (uopToDo = 'B'   or   uopToDo = '2');
010400080819           exsr  sr_DltOvr_1;
010500080820
010600080819       endsl;
010700080819
010800080819       // Operazioni finali
010900080819       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
011000080819       exsr sr_RoutEnd;
011100080819
011200080819       //--------------------------------------------------------------
011300080819       //?Operazioni iniziali.
011400080819       //--------------------------------------------------------------
011500080819       BEGSR sr_RoutInz;
011600080819
011700080819         *inLR = *on;
011800080819
011900080820         if  SDSprm = *zero;
012000080819           exsr  sr_RoutEnd;
012100080819         endif;
012200080819
012300080819         clear  Qcmd;
012400080820         clear  uopCmd;
012500080820         uopErr = *off;
012600080819
012700080819         // Impostazione solo caratteri maiuscoli nei parametri
012800080819         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
012900080820         uopPrtF    = %xlate( c_lo : c_up : uopPrtF );
013000080820         uopOutQ    = %xlate( c_lo : c_up : uopOutQ );
013100080819         uopHold    = %xlate( c_lo : c_up : uopHold );
013200080819         uopSave    = %xlate( c_lo : c_up : uopSave );
013300080819         uopOvrScop = %xlate( c_lo : c_up : uopOvrScop );
013400080819         uopShare   = %xlate( c_lo : c_up : uopShare );
013500080820         uopOpnScop = %xlate( c_lo : c_up : uopOpnScop );
013600080820         uopLvl     = %xlate( c_lo : c_up : uopLvl );
013700080819
013800080819         // Controllo parametri ricevuti
013900080819         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
014000080819         select;
014100080820
014200080820           // Operazione richiesta errata
014300090921           when  uopToDo <> 'A'   and   uopToDo <> 'B'   and
014400090921                 uopToDo <> '1'   and   uopToDo <> '2';
014500080819             uopErr = *on;
014600080820
014700080820           // Parametri mancanti
014800090921           when  uopPrtF = *blank;
014900080819             uopErr = *on;
015000080820
015100080820           // Parametri errati x OVRPRTF
015200090921           when  (uopToDo   = 'A'   or   uopToDo = '1')   and
015300090921                  uopCopies < *zero;
015400080819             uopErr = *on;
015500090921           when  (uopToDo =  'A'     or   uopToDo =  '1')   and
015600090921                 (uopHold <> *blank  and  uopHold <> '*NO '
015700090921                                     and  uopHold <> '*YES');
015800080819             uopErr = *on;
015900090921           when  (uopToDo =  'A'     or   uopToDo =  '1')   and
016000090921                 (uopSave <> *blank  and  uopSave <> '*NO'
016100090921                                     and  uopSave <> '*YES');
016200080819             uopErr = *on;
016300090921           when  (uopToDo    =  'A'     or   uopToDo    =  '1')   and
016400090921                 (uopOvrScop <> *blank  and  uopOvrScop <> '*ACTGRPDFN'
016500090921                                        and  uopOvrScop <> '*CALLLVL  '
016600090921                                        and  uopOvrScop <> '*JOB      ');
016700080820             uopErr = *on;
016800090921           when  (uopToDo  =  'A'     or   uopToDo  =  '1')   and
016900090921                 (uopShare <> *blank  and  uopShare <> '*NO '
017000090921                                      and  uopShare <> '*YES');
017100080819             uopErr = *on;
017200080820
017300080820           // Parametri errati x DLTOVR
017400090921           when  (uopToDo = 'B'     or   uopToDo = '2')   and
017500090921                 (uopLvl <> *blank  and  uopLvl <> '*ACTGRPDFN'
017600090921                                    and  uopLvl <> '*         '
017700090921                                    and  uopLvl <> '*JOB      ');
017800080820             uopErr = *on;
017900080820
018000080819         endsl;
018100080819
018200080819       ENDSR;
018300080819
018400080819       //--------------------------------------------------------------
018500080820       //?Impostazione parametri per comando di override ad un
018600080820       //?  singolo file di stampa.
018700080819       //--------------------------------------------------------------
018800080819       BEGSR sr_Ovr_1;
018900080820
019000080820         //?Preparazione comando di override sull'unico prtf?
019100080820
019200090921         // - Impostazione variabili utilizzate al singolo giro
019300090921         wPrtF     = uopPrtf;
019400090921         wOutQ     = uopOutQ;
019500090921         wFormType = uopFrmTyp;
019600090921         wUsrDta   = uopUsrDta;
019700080820
019800090921         // - Creazione comando da eseguire
019900080820         exsr  sr_OvrPrtF;
020000080820         uopCmd = Qcmd;
020100080819
020200080819       ENDSR;
020300080819
020400080819       //--------------------------------------------------------------
020500080820       //?Impostazione parametri per comando di cancellazione override
020600080820       //?  da un singolo file di stampa.
020700080819       //--------------------------------------------------------------
020800080819       BEGSR sr_DltOvr_1;
020900080820
021000080820         //?Preparazione comando di cancellazione override sull'unico prtf?
021100080820
021200090921         // - Impostazione variabili utilizzate all'unico giro
021300090921         wPrtF = uopPrtf;
021400080820
021500090921         // - Creazione comando da eseguire
021600080820         exsr  sr_DltOvr;
021700080820         uopCmd = Qcmd;
021800080819
021900080819       ENDSR;
022000080820
022100080820       //--------------------------------------------------------------
022200080820       //?Preparazione comando per
022300080820       //?esecuzione override ad un singolo file di stampa.
022400080820       //--------------------------------------------------------------
022500080820       BEGSR sr_OvrPrtF;
022600080820
022700080820         Qcmd = 'OVRPRTF file(' + %trimr(wPrtF) + ')';
022800080820
022900080820         if  wOutQ <> *blank;
023000090921           Qcmd += ' outq(' + %trimr(wOutQ) + ')';
023100080820         endif;
023200080820
023300080820         if  wFormType <> *blank;
023400090921           Qcmd += ' formtype(''' + %trimr(wFormType) + ''')';
023500080820         endif;
023600080820
023700080820         if  uopCopies > *zero;
023800090921           Qcmd += ' copies(' + %trim( %editc(uopCopies : 'Z') ) + ')';
023900080820         endif;
024000080820
024100080820         if  uopHold > *blank;
024200090921           Qcmd += ' hold(' + %trim(uopHold) + ')';
024300080820         endif;
024400080820
024500080820         if  uopSave > *blank;
024600090921           Qcmd += ' save(' + %trim(uopSave) + ')';
024700080820         endif;
024800080820
024900080820         if  wUsrDta <> *blank;
025000090921           Qcmd += ' usrdta(''' + %trimr(wUsrDta) + ''')';
025100080820         endif;
025200080820
025300080820         if  uopOvrScop <> *blank;
025400090921           Qcmd += ' ovrscope(' + %trimr(uopOvrScop) + ')';
025500080820         endif;
025600080820
025700080820         if  uopShare <> *blank;
025800090921           Qcmd += ' share(' + %trimr(uopShare) + ')';
025900080820         endif;
026000080820
026100080820         if  uopOpnScop <> *blank;
026200090921           Qcmd += ' opnscope(' + %trimr(uopOpnScop) + ')';
026300080820         endif;
026400080820
026500080820       ENDSR;
026600080820
026700080820       //--------------------------------------------------------------
026800080820       //?Preparazione comando per
026900080820       //?cancellazione override da un singolo file di stampa.
027000080820       //--------------------------------------------------------------
027100080820       BEGSR sr_DltOvr;
027200080820
027300080820         Qcmd = 'DLTOVR file(' + %trimr(wPrtF) + ')';
027400080820
027500080820         if  uopLvl <> *blank;
027600090921           Qcmd += ' lvl(' + %trimr(uopLvl) + ')';
027700080820         endif;
027800080820
027900080820       ENDSR;
028000080819
028100080819       //--------------------------------------------------------------
028200080819       //?Operazioni finali.
028300080819       //--------------------------------------------------------------
028400080819       BEGSR sr_RoutEnd;
028500080819
028600090921         //?Esecuzione del comando prima impostato - SE richiesta?
028700080820
028800090921         if  uopToDo > *zero  and
028900090921             uopErr  = *off   and
029000090921             uopCmd <> *blank;
029100090921           Qcmd = uopCmd;
029200090921           callp(e) qCmdExc(Qcmd : %size(Qcmd));
029300090921           if  %error;
029400090921             uopErr = *on;
029500090921             //else;
029600090921             //  clear  uopCmd;
029700080820           endif;
029800080819         endif;
029900080819
030000080819         // Fine
030100080819         return;
030200080819
030300080819       ENDSR;
030400080819
030500080819      /end-free

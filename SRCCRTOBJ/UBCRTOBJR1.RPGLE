000100100413       //==============================================================
000200100804       //? UBCRTOBJR1 - Compilatore SETRAS Srl (BARTOLINI S.p.A.)      ?
000300100414       //? 2° passo: controllo iniziale parametri   e                  ?
000400100414       //?           verifica esistenza oggetto nella libreria.        ?
000500100413       //==============================================================
000600100413
000700100413       //--------------------------------------------------------------
000800100413       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000900100413       //--------------------------------------------------------------
001000100413
001100100413     /*END
001200100413
001300100413       //--------------------------------------------------------------
001400100413       //?Specifiche di controllo.                                     ?
001500100413       //--------------------------------------------------------------
001600100413
001700100413     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001800100413     h dftactgrp(*no)
001900100413
002000100413       //--------------------------------------------------------------
002100100413       //?Dichiarazione file.                                          ?
002200100413       //--------------------------------------------------------------
002300100413
002400100413
002500100413       //--------------------------------------------------------------
002600100413       //?Definizione costanti.                                        ?
002700100413       //--------------------------------------------------------------
002800100413
002900100413       // -?Errori gestiti?
003000100413       // - -?Tipo sorgente non previsto / non gestito?
003100100413     d c_Err_MbrTypeNotOK...
003200100413     d                 c                   const(-1)
003300100413       // - -?File sorgente inesistente?
003400100413     d c_Err_NotFoundSrcF...
003500100413     d                 c                   const(-2)
003600100413       // - -?Membro sorgente inesistente?
003700100413     d c_Err_NotFoundSrcMbr...
003800100413     d                 c                   const(-3)
003900100413       // - -?Oggetto già esistente (e non rimosso)?
004000100413     d c_Err_NoDltExistObj...
004100100413     d                 c                   const(-4)
004200100413
004300100413       //--------------------------------------------------------------
004400100413       //?Definizione schiere.                                         ?
004500100413       //--------------------------------------------------------------
004600100413
004700100413
004800100413       //--------------------------------------------------------------
004900100413       //?Definizione aree dati.                                       ?
005000100413       //--------------------------------------------------------------
005100100413
005200100413
005300100413       //--------------------------------------------------------------
005400100413       //?Definizione strutture dati.                                  ?
005500100413       //--------------------------------------------------------------
005600100413
005700100413     d Status         sds
005800100413     d*//SDSpgm          *proc
005900100413     d*//SDSprm          *parms
006000100413     d*//SDSdta              191    198
006100100413     d*//SDSjob              244    253
006200100413     d   SDSusr              254    263
006300100413
006400100413       //--------------------------------------------------------------
006500100413       //?Definizione variabili globali.                               ?
006600100413       //--------------------------------------------------------------
006700100413
006800100413       // -?Parametri ricevuti?
006900100413     d pIn_SrcFile     ds            20
007000100413     d   pIn_SrcF_F                  10
007100100413     d   pIn_SrcF_L                  10
007200100413     d pIn_SrcMbr      s             10
007300100413     d pIn_MbrType     s             10
007400100413     d pIn_ObjLib      s             10
007500180302     d pIn_Replace     s              5
007600180302     d pOut_Replace    s               n
007700100413     d pOut_Err        s             10i 0
007800100413     d pOut_MsgTxt     s            160
007900100413
008000100413       // -?Parametri API QCAPCMD (Process Commands)?
008100100413      /copy qsysinc/qrpglesrc,QCAPCMD
008200100413     d Qcmd            s           2048    inz  varying
008300100413
008400100413       // -?Parametri gestione errori API.?
008500100413      /copy qsysinc/qrpglesrc,QUSEC
008600100413
008700100413       // -?Flags booleani?
008800100413     d $Module         s               n    inz
008900100413
009000100413       // -?Campi di comodo?
009100100413     d w_MsgTxt        s            160     inz
009200100414     d w_MsgRpy        s              1     inz
009300100413
009400100413       //--------------------------------------------------------------
009500100413       //?Definizione prototipi procedure.                             ?
009600100413       //--------------------------------------------------------------
009700100413
009800100413       // -?API QCAPCMD (Process Commands)?
009900100413      /copy gaitrasrc/srcProtoPr,QCAPCMD
010000100413
010100100413       // -?Messaggio x richiesta sostituzione oggetto?
010200100413     d ubCrtObjC2      pr                   extpgm('UBCRTOBJC2')
010300100413     d   p_MsgTxt                           like(w_MsgTxt)
010400100414     d   p_MsgRpy                           like(w_MsgRpy)
010500100413
010600100413       //--------------------------------------------------------------
010700100413       //?Definizione key-list.                                        ?
010800100413       //--------------------------------------------------------------
010900100413
011000100413
011100100413       //--------------------------------------------------------------
011200100413       //?Specifiche di Input ("I")                                    ?
011300100413       //--------------------------------------------------------------
011400100413
011500100413
011600100413       //--------------------------------------------------------------
011700100413       //?M A I N - L I N E                                            ?
011800100413       //--------------------------------------------------------------
011900100413
012000100413     c     *Entry        plist
012100100413     c                   parm                    pIn_SrcFile
012200100413     c                   parm                    pIn_SrcMbr
012300100413     c                   parm                    pIn_MbrType
012400100413     c                   parm                    pIn_ObjLib
012500180302     c                   parm                    pIn_Replace
012600180302     c                   parm                    pOut_Replace
012700100413     c                   parm                    pOut_Err
012800100413     c                   parm                    pOut_MsgTxt
012900100413
013000100413      /free
013100100413
013200100413       // -?Operazioni iniziali?
013300100413       exsr  sr_RoutInz;
013400100413
013500100413       // -?Verifica esistenza oggetto?
013600100413       exsr  sr_CtrlExistObj;
013700100413
013800100413       // -?Operazioni finali?
013900100413       exsr  sr_RoutEnd;
014000100413
014100100413       //--------------------------------------------------------------
014200100413       //?Operazioni iniziali.                                         ?
014300100413       //--------------------------------------------------------------
014400100413       BEGSR  sr_RoutInz;
014500100413
014600100413         *inLR = *on;
014700100413
014800180302         pOut_Replace = *off;
014900100413         clear  pOut_Err;
015000100413         clear  pOut_MsgTxt;
015100100413         clear  Qcmd;
015200100413         clear  w_MsgTxt;
015300100414         clear  w_MsgRpy;
015400100413
015500100413         // -?a) Controllo tipologia di oggetto?
015600100413         if  pIn_MbrType <> 'RPGLE     ' and
015700100413             pIn_MbrType <> 'SQLRPGLE  ' and
015800100413             pIn_MbrType <> 'CLLE      ' and
015900100413             pIn_MbrType <> 'DSPF      ' and
016000100413             pIn_MbrType <> 'PRTF      ' and
016100100413             pIn_MbrType <> 'PF        ' and
016200100413             pIn_MbrType <> 'LF        ' and
016300100413             pIn_MbrType <> 'CMD       ' and
016400100413             pIn_MbrType <> 'BND       ' and
016500100413             pIn_MbrType <> 'CLP       ' and
016600100413             pIn_MbrType <> 'RPG       ' and
016700100413             pIn_MbrType <> 'SQLRPG    ' and
016800100413           pOut_Err = c_Err_MbrTypeNotOK;
016900100413           exsr  sr_RoutEnd;
017000100413         endif;
017100100413
017200100413         // -?b) Verifica esistenza file sorgente?
017300100413         Qcmd = 'CHKOBJ obj(' +
017400100413                        %trim(pIn_SrcF_L) + '/' + %trim(pIn_SrcF_F) +
017500100413                        ') objtype(*file)';
017600100413         exsr  sr_qCapCmd;
017700100413         if  Qusei <> *blank;
017800100413           pOut_Err = c_Err_NotFoundSrcF;
017900100413           exsr  sr_RoutEnd;
018000100413         endif;
018100100413
018200100413         // -?c) Verifica esistenza membro sorgente?
018300100413         Qcmd = 'CHKOBJ obj(' +
018400100413                        %trim(pIn_SrcF_L) + '/' + %trim(pIn_SrcF_F) +
018500100413                        ') objtype(*file) mbr(' +
018600100413                        %trim(pIn_SrcMbr) + ')';
018700100413         exsr  sr_qCapCmd;
018800100413         if  Qusei <> *blank;
018900100413           pOut_Err = c_Err_NotFoundSrcMbr;
019000100413           exsr  sr_RoutEnd;
019100100413         endif;
019200100413
019300100413       ENDSR;
019400100413
019500100413       //--------------------------------------------------------------
019600100413       //?Verifica esistenza dell'oggetto in compilazione              ?
019700100413       //--------------------------------------------------------------
019800100413       BEGSR  sr_CtrlExistObj;
019900100413
020000100413         clear  Qcmd;
020100100413
020200100413         select;
020300100413
020400100413           when  pIn_MbrType = 'DSPF'   or
020500100413                 pIn_MbrType = 'PRTF'   or
020600100413                 pIn_MbrType = 'PF'     or
020700100413                 pIn_MbrType = 'LF';
020800100413             Qcmd = 'CHKOBJ obj(' +
020900100413                            %trimr(pIn_ObjLib) + '/' +
021000100413                            %trimr(pIn_SrcMbr) +
021100100413                            ') objtype(*file)';
021200100413             exsr  sr_qCapCmd;
021300100413             if  Qusei = *blank;
021400100413               w_MsgTxt = 'Il file '
021500100413                        + %trim(pIn_ObjLib) + '/' + %trim(pIn_SrcMbr)
021600100413                        + ' esiste già. Immettere "S" per';
021700100413               if  pIn_MbrType = 'DSPF'   or
021800100413                   pIn_MbrType = 'PRTF';
021900100413                 w_MsgTxt = %trimr(w_MsgTxt) + ' sostituirlo.';
022000100413               else;
022100100413                 w_MsgTxt = %trimr(w_MsgTxt) + ' CANCELLARLO.';
022200100413               endif;
022300100413             endif;
022400100413
022500100413           when (pIn_MbrType = 'RPGLE'      or
022600100413                 pIn_MbrType = 'SQLRPGLE'   or
022700100413                 pIn_MbrType = 'CLLE'       or
022800100413                 pIn_MbrType = 'RPG'        or
022900100413                 pIn_MbrType = 'SQLRPG'     or
023000100413                 pIn_MbrType = 'CLP')       and
023100100413                 Not  $Module;
023200100413             Qcmd = 'CHKOBJ obj(' +
023300100413                            %trimr(pIn_ObjLib) + '/' +
023400100413                            %trimr(pIn_SrcMbr) +
023500100413                            ') objtype(*pgm)';
023600100413             exsr  sr_qCapCmd;
023700100413             if  Qusei = *blank;
023800100413               w_MsgTxt = 'Il programma '
023900100413                        + %trim(pIn_ObjLib) + '/' + %trim(pIn_SrcMbr)
024000100413                        + ' esiste già. Immettere "S" per sostituirlo.';
024100100413             endif;
024200100413
024300100413           when (pIn_MbrType = 'RPGLE'      or
024400100413                 pIn_MbrType = 'SQLRPGLE'   or
024500100413                 pIn_MbrType = 'CLLE'       or
024600100413                 pIn_MbrType = 'RPG'        or
024700100413                 pIn_MbrType = 'SQLRPG'     or
024800100413                 pIn_MbrType = 'CLP')       and
024900100413                 $Module;
025000100413             Qcmd = 'CHKOBJ obj(' +
025100100413                            %trimr(pIn_ObjLib) + '/' +
025200100413                            %trimr(pIn_SrcMbr) +
025300100413                            ') objtype(*module)';
025400100413             exsr  sr_qCapCmd;
025500100413             if  Qusei = *blank;
025600100413               w_MsgTxt = 'Il modulo '
025700100413                        + %trim(pIn_ObjLib) + '/' + %trim(pIn_SrcMbr)
025800100413                        + ' esiste già. Immettere "S" per sostituirlo.';
025900100413             endif;
026000100413
026100100413           when  pIn_MbrType = 'BND';
026200100413             Qcmd = 'CHKOBJ obj(' +
026300100413                            %trimr(pIn_ObjLib) + '/' +
026400100413                            %trimr(pIn_SrcMbr) +
026500100413                            ') objtype(*srvpgm)';
026600100413             exsr  sr_qCapCmd;
026700100413             if  Qusei = *blank;
026800100413               w_MsgTxt = 'Il programma di servizio '
026900100413                        + %trim(pIn_ObjLib) + '/' + %trim(pIn_SrcMbr)
027000100413                        + ' esiste già. Immettere "S" per aggiornarlo.';
027100100413             endif;
027200100413
027300100413           when  pIn_MbrType = 'CMD';
027400100413             Qcmd = 'CHKOBJ obj(' +
027500100413                            %trimr(pIn_ObjLib) + '/' +
027600100413                            %trimr(pIn_SrcMbr) +
027700100413                            ') objtype(*cmd)';
027800100413             exsr  sr_qCapCmd;
027900100413             if  Qusei = *blank;
028000100413               w_MsgTxt = 'Il comando '
028100100413                        + %trim(pIn_ObjLib) + '/' + %trim(pIn_SrcMbr)
028200100413                        + ' esiste già. Immettere "S" per sostituirlo.';
028300100413             endif;
028400100413
028500100413         endsl;
028600100413
028700100413
028800100413         // -?Invio del messaggio per la richiesta di sostituzione?
028900100413         //  ?dell'oggetto già esistente?
029000180302         //  ?(SE la risposta non è già ricevuta come parametro:?
029100180302         //  ?· pIn_Replace = '*'    => Richiedere?
029200180302         //  ?· pIn_Replace = '*YES' => Sostituire?
029300180302         //  ?· pIn_Replace = '*NO'  => NON sostituire)?
029400100413         if  w_MsgTxt <> *blank;
029500180302
029600180302           if  pIn_Replace = '*    ';
029700180302
029800180302             ubCrtObjC2 ( w_MsgTxt : w_MsgRpy );
029900180302             pOut_Replace = (w_MsgRpy = 'S');
030000180302
030100180302           else;
030200180302
030300180302             pOut_Replace = ( pIn_Replace = '*YES ' );
030400180302
030500180302           endif;
030600100413
030700100413           // -?Uscita perchè negata la sostituzione?
030800100414           if  pOut_Replace = *off;
030900100413             pOut_Err = c_Err_NoDltExistObj;
031000100413             exsr  sr_RoutEnd;
031100100413           endif;
031200100413
031300100413         endif;
031400100413
031500100413       ENDSR;
031600100413
031700100413       //--------------------------------------------------------------
031800100413       //?Esecuzione del comando (già impostato): API QCAPCMD.         ?
031900100413       //--------------------------------------------------------------
032000100413       BEGSR  sr_qCapCmd;
032100100413
032200100413         clear Qcap0100;
032300100413         Qcabcsdh = *off;
032400100413         Qcapa    = *off;
032500100413         Qcacmdss = *off;
032600100413         Qcaerved = *allX'00';
032700100413
032800100413         clear Qusec;
032900100413         Qusbprv  = %size(Qusec);
033000100413
033100100413         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
033200100413                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
033300100413                           0 : 0 : Qusec);
033400100413
033500100413         // -?(l'eventuale errore verrà testato dopo e restituito al?
033600100413         //  ?chiamante - non emesso qui)?
033700100413         //if  Qusei <> *blank;
033800100413         //  exsr  sr_MsgErr;
033900100413         //endif;
034000100413
034100100413       ENDSR;
034200100413
034300100413       //--------------------------------------------------------------
034400100413       //?Invio messaggio per segnalazione errori.                     ?
034500100413       //?(l'eventuale errore verrà testato dopo e restituito al?
034600100413       //?chiamante - non emesso qui)?
034700100413       //--------------------------------------------------------------
034800100413       //BEGSR  sr_MsgErr;
034900100413       //
035000100413       //  sndmsg  msg(&MsgTxt) tousr(&User)
035100100413       //
035200100413       //ENDSR;
035300100413
035400100413       //--------------------------------------------------------------
035500100413       //?Operazioni finali.                                           ?
035600100413       //--------------------------------------------------------------
035700100413       BEGSR  sr_RoutEnd;
035800100413
035900100413
036000100413         // -?Impostazione messaggio di uscita?
036100100413         select;
036200100413
036300100413           // -?Creato l'oggetto?
036400100413           when  pOut_Err = *zero;
036500100414
036600100414           // -?(-1) Tipo membro sorgente NON gestito?
036700100414           when  pOut_Err = c_Err_MbrTypeNotOK;
036800100414             pOut_MsgTxt = 'Il tipo membro '
036900100414                         + %trim( pIn_MbrType )
037000100414                         + ' non è gestito dal comando UBCRTOBJ';
037100100413
037200100414           // -?(-2) File sorgente inesistente?
037300100413           when  pOut_Err = c_Err_NotFoundSrcF;
037400100413             pOut_MsgTxt = 'Non trovato il file-sorgente '
037500100413                         + %trim( pIn_SrcF_L )
037600100413                         + '/'
037700100413                         + %trim( pIn_SrcF_F );
037800100413
037900100414           // -?(-3) Membro sorgente inesistente?
038000100413           when  pOut_Err = c_Err_NotFoundSrcMbr;
038100100413             pOut_MsgTxt = 'Non trovato il membro '
038200100413                         + %trim( pIn_SrcMbr )
038300100413                         + ' nel file-sorgente '
038400100413                         + %trim( pIn_SrcF_L )
038500100413                         + '/'
038600100413                         + %trim( pIn_SrcF_F );
038700100413
038800100414           // -?(-4) Oggetto già esistente e non cancellato?
038900100413           when  pOut_Err = c_Err_NoDltExistObj;
039000100413             pOut_MsgTxt = 'Oggetto '
039100100413                         + %trim( pIn_SrcMbr )
039200100413                         + ' già esistente in '
039300100413                         + %trim(pIn_ObjLib);
039400100413
039500100413         endsl;
039600100413
039700100413         return;
039800100413
039900100413       ENDSR;
040000100413
040100100413      /end-free

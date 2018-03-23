000100100317       //==============================================================
000200100804       //? UBCRTOBJR2 - Compilatore SETRAS Srl (BARTOLINI S.p.A.)      ?
000300100414       //? 5∞ passo: compilazione   ed                                 ?
000400100414       //?           emissione messaggio finale.                       ?
000500100317       //==============================================================
000600100317
000700100317       //--------------------------------------------------------------
000800100317       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000900100317       //--------------------------------------------------------------
001000100317
001100100317     /*CMD ovrdbf file(SrcF_Mbr) tofile(GaiTraSrc/SrcUB) mbr(*first)
001200100317     /*//* dltovr file(SrcF_Mbr)
001300100317     /*END
001400100317
001500100317       //?N.B.:?Per questo sorgente NON sar‡ possibile eseguire il *cmd
001600100317       //       DLTOVR finchË il compilatore Ë in esecuzione, perchÈ
001700100317       //       Ë un'operazione che gestisce il compilatore stesso!
001800100317       //       Pertanto, SOLO X QUESTO SORGENTE, non Ë da richiedere!
001900091007
002000100317       //--------------------------------------------------------------
002100100317       //?Specifiche di controllo.                                     ?
002200100317       //--------------------------------------------------------------
002300100317
002400091007     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002500091022     h dftactgrp(*no)
002600091007
002700091007       //--------------------------------------------------------------
002800091007       //?Dichiarazione file.                                          ?
002900091007       //--------------------------------------------------------------
003000091007
003100100225       // -?Membro sorgente?
003200100318     fSrcF_Mbr  if   f  112        disk    usropn
003300091007
003400091007       //--------------------------------------------------------------
003500091007       //?Definizione costanti.                                        ?
003600091007       //--------------------------------------------------------------
003700091022
003800100114     d c_Maiuscole     c                   const('ABCDEFGHIJKLM+
003900100114     d                                            NOPQRSTUVWXYZ+
004000100225     d                                            ¿¡ƒ¬√≈+
004100100225     d                                            »…À +
004200100225     d                                            ÃÕœŒ+
004300100225     d                                            “”÷‘’+
004400100225     d                                            Ÿ⁄‹€+
004500100225     d                                            «')
004600100114     d c_Minuscole     c                   const('abcdefghijklm+
004700100114     d                                            nopqrstuvwxyz+
004800100225     d                                            ‡·‰‚„Â+
004900100225     d                                            ËÈÎÍ+
005000100225     d                                            ÏÌÔÓ+
005100100225     d                                            ÚÛˆÙı+
005200100225     d                                            ˘˙¸˚+
005300100225     d                                            Á')
005400091007
005500100225       // -?Errori gestiti?
005600100225       // - -?Fallita override al membro sorgente?
005700100413     d c_Err_FailedOvrSrcMbr...
005800100414     d                 c                   const(-21)
005900100225       // - -?Fallita esecuzione del comando richiesto (/*CMD)?
006000100413     d c_Err_FailedRiquestCmd...
006100100415     d                 c                   const(-22)
006200100415       // - -?Oggetto gi‡ esistente (e non rimosso)?
006300100415     d c_Err_NoDltExistObj...
006400100415     d                 c                   const(-23)
006500100804       // - -?File gi‡ esistente in Qtemp (e non rimosso)?
006600100804     d c_Err_NoDltQtempFile...
006700100804     d                 c                   const(-24)
006800100804       // - -?Fallito spostamento file dalla QTEMP
006900100804     d c_Err_NoMoveFile...
007000100804     d                 c                   const(-25)
007100100225       // - -?Compilazione non riuscita?
007200100413     d c_Err_FailedCrtObj...
007300100414     d                 c                   const(-29)
007400100225       // - -?Fallite operazioni finali?
007500100413     d c_Err_FailedEndCmd...
007600100414     d                 c                   const(+21)
007700091020
007800100225       // -?Tipologia di dati?
007900100225       // - -?Impostata opzione di compilazione?
008000160822     d c_Mod           c                   const('/*MODULE ')
008100120321     d c_DltMod        c                   const('*DELETE')
008200160822     d c_Pgm           c                   const('/*CRTPGM ')
008300160822     d c_Prm           c                   const('/*PRM ')
008400100413       // - -?Richiesto comando iniziale (override)?
008500160822     d c_Cmd           c                   const('/*CMD ')
008600100225       // - -?Richiesto comando finale (delete override) / Fine opzioni?
008700160822     d c_End           c                   const('/*END ')
008800091007
008900091007       //--------------------------------------------------------------
009000091007       //?Definizione schiere.                                         ?
009100091007       //--------------------------------------------------------------
009200091007
009300100413     d $CmdEnd         s           2048    varying  dim(99)  inz
009400091007
009500091007       //--------------------------------------------------------------
009600091007       //?Definizione aree dati.                                       ?
009700091007       //--------------------------------------------------------------
009800091007
009900091007
010000091007       //--------------------------------------------------------------
010100091007       //?Definizione strutture dati.                                  ?
010200091007       //--------------------------------------------------------------
010300091007
010400091021     d Status         sds
010500091021     d*//SDSpgm          *proc
010600091021     d*//SDSprm          *parms
010700091021     d*//SDSdta              191    198
010800091021     d*//SDSjob              244    253
010900091021     d   SDSusr              254    263
011000091007
011100091007       //--------------------------------------------------------------
011200091007       //?Definizione variabili globali.                               ?
011300091007       //--------------------------------------------------------------
011400091007
011500100225       // -?Parametri ricevuti?
011600091007     d pIn_SrcFile     ds            20
011700091020     d   pIn_SrcF_F                  10
011800091020     d   pIn_SrcF_L                  10
011900091007     d pIn_SrcMbr      s             10
012000091007     d pIn_MbrType     s             10
012100091007     d pIn_ObjLib      s             10
012200100414     d pIn_Replace     s               n
012300091021     d pOut_Err        s             10i 0
012400091022     d pOut_MsgTxt     s            160
012500091007
012600100225       // -?Parametri API QCAPCMD (Process Commands)?
012700091007      /copy qsysinc/qrpglesrc,QCAPCMD
012800091007     d Qcmd            s           2048    inz  varying
012900091020     d QcmdCrt         s           2048    inz  varying
013000120319     d QcmdCrtPgm      s           2048    inz  varying
013100091007
013200100225       // -?Parametri API QDBRTVFD (Retrieve Database File Description)?
013300091007      /copy qsysinc/qrpglesrc,QDBRTVFD
013400091007     d Qdbrv           s           2048    inz
013500091007
013600100225       // -?Parametri API QUSRMBRD (Retrieve Member Description)?
013700091007      /copy qsysinc/qrpglesrc,QUSRMBRD
013800091007     d Qmbrd01         s                   like(QUSM0200)  inz
013900091007     d Qmbrd02         s              9b 0 inz(%size(Qmbrd01))
014000091007     d Qmbrd03         s              8    inz('MBRD0200')
014100091007     d Qmbrd04         s             20    inz('          *LIBL     ')
014200091007     d Qmbrd05         s             10    inz('*FIRST')
014300091007     d Qmbrd06         s              1    inz(*off)
014400091007
014500100225       // -?Parametri gestione errori API.?
014600091007      /copy qsysinc/qrpglesrc,QUSEC
014700091020
014800100225       // -?Flags booleani?
014900091020     d $Fine           s               n    inz
015000100113     d $Module         s               n    inz
015100120321     d $DltMod         s               n    inz
015200100413     d $Prosieguo      s               n    inz
015300100413     d $Prosegue       s               n    inz
015400091007
015500100225       // -?Indici di schiera / Posizioni?
015600091007     d xx              s             10i 0  inz
015700091022     d yy              s             10i 0  inz
015800091007
015900100225       // -?Campi di comodo?
016000100225     d w_SrcDta        s                    like(SrcDta)  inz
016100100317     d w_ObjTyp        s              7     inz
016200091007
016300091007       //--------------------------------------------------------------
016400091007       //?Definizione prototipi procedure.                             ?
016500091007       //--------------------------------------------------------------
016600091007
016700100225       // -?API QCAPCMD (Process Commands)?
016800091007      /copy gaitrasrc/srcProtoPr,QCAPCMD
016900091007
017000100225       // -?API QDBRTVFD (Retrieve Database File Description)?
017100091007     d pr_QDbRtvFd     pr                   extpgm('QDBRTVFD')
017200091007     d   p_Qdbrv                            like(Qdbrv   )
017300091007     d   p_Qdblorv                          like(Qdblorv )
017400091007     d   p_Qdbrfal                          like(Qdbrfal )
017500091007     d   p_Qdbfn00                          like(Qdbfn00 )
017600091007     d   p_Qdbfaln                          like(Qdbfaln )
017700091007     d   p_Qdbrfn00                         like(Qdbrfn00)
017800091007     d   p_Qdbfilof                         like(Qdbfilof)
017900091007     d   p_Qdbystem                         like(Qdbystem)
018000091007     d   p_Qdbft                            like(Qdbft   )
018100091007     d   p_Qusec                            like(Qusec   )
018200091007
018300100225       // -?API QUSRMBRD (Retrieve Member Description)?
018400091007     d pr_QUsrMbrD     pr                   extpgm('QUSRMBRD')
018500091007     d   p_Qmbrd01                          like(Qmbrd01)
018600091007     d   p_Qmbrd02                          like(Qmbrd02)
018700091007     d   p_Qmbrd03                          like(Qmbrd03)
018800091007     d   p_Qmbrd04                          like(Qmbrd04)
018900091007     d   p_Qmbrd05                          like(Qmbrd05)
019000091007     d   p_Qmbrd06                          like(Qmbrd06)
019100091007
019200091007       //--------------------------------------------------------------
019300091007       //?Definizione key-list.                                        ?
019400091007       //--------------------------------------------------------------
019500091007
019600091020
019700091020       //--------------------------------------------------------------
019800091020       //?Specifiche di Input ("I")                                    ?
019900091020       //--------------------------------------------------------------
020000091007
020100091020     iSrcF_Mbr  nn
020200091020     i*//                               1    6 2SrcSeq
020300091020     i*//                               7   12 0SrcDat
020400100318     i                                 13  112  SrcDta
020500091007
020600091007       //--------------------------------------------------------------
020700091007       //?M A I N - L I N E                                            ?
020800091007       //--------------------------------------------------------------
020900091007
021000091007     c     *Entry        plist
021100091007     c                   parm                    pIn_SrcFile
021200091007     c                   parm                    pIn_SrcMbr
021300091007     c                   parm                    pIn_MbrType
021400091007     c                   parm                    pIn_ObjLib
021500100413     c                   parm                    pIn_Replace
021600091022     c                   parm                    pOut_Err
021700091022     c                   parm                    pOut_MsgTxt
021800091007
021900091007      /free
022000091007
022100100225       // -?Operazioni iniziali?
022200091007       exsr  sr_RoutInz;
022300091007
022400100225       // -?Lettura sorgente per reperire eventuali comandi e/o opzioni?
022500100225       //  ?di compilazione?
022600091007       exsr  sr_ReadSrc;
022700091007
022800100225       // -?Operazioni finali?
022900091007       exsr  sr_RoutEnd;
023000091007
023100091007       //--------------------------------------------------------------
023200091007       //?Operazioni iniziali.                                         ?
023300091007       //--------------------------------------------------------------
023400091007       BEGSR  sr_RoutInz;
023500091007
023600091007         *inLR = *on;
023700091020
023800091022         clear  pOut_Err;
023900091022         clear  pOut_MsgTxt;
024000091020         clear  Qcmd;
024100100804
024200100804         // -?*) Eventuale cancellazione file in QTEMP?
024300100804         if  pIn_Replace = *on    and
024400100804            (pIn_MbrType = 'PF'    or   pIn_MbrType = 'LF')   and
024500100804             pIn_ObjLib <> 'QTEMP';
024600101102           if  %len( %trim( pIn_SrcMbr ) ) > 5;
024700101102             Qcmd = 'DLTF FILE(QTEMP/' +
024800101102                    %subst( %trim(pIn_SrcMbr) : 1 : 5 ) + '*)';
024900101102           else;    // -?DS?
025000101102             Qcmd = 'DLTF FILE(QTEMP/' + %trim(pIn_SrcMbr) + ')';
025100101102           endif;
025200100804           exsr  sr_qCapCmd;
025300101102           if  Qusei <> *blank   and   Qusei <> 'CPF2105'
025400101102                                 and   Qusei <> 'CPF2125';
025500100804             pOut_Err = c_Err_NoDltQtempFile;
025600100804             exsr  sr_RoutEnd;
025700100804           endif;
025800100804         endif;
025900091007
026000100413         // -?a) Esecuzione override al membro sorgente?
026100091007         Qcmd = 'OVRDBF file(SrcF_Mbr) +
026200091020                        tofile(' + %trim(pIn_SrcF_L) + '/'
026300091020                                 + %trim(pIn_SrcF_F) + ') +
026400091007                        mbr(' + %trim(pIn_SrcMbr) + ') +
026500091021                        secure(*yes) +
026600091007                        ovrscope(*actgrpdfn) +
026700091007                      //share(*no) +
026800091007                      //opnscope(*actgrpdfn) +
026900091007                        ';
027000091021         exsr  sr_qCapCmd;
027100091007         if  Qusei <> *blank;
027200091007           pOut_Err = c_Err_FailedOvrSrcMbr;
027300091007           exsr  sr_RoutEnd;
027400091007         endif;
027500091007
027600100413         // -?b) Apertura del file sorgente?
027700091007         open  SrcF_Mbr;
027800091007
027900091007       ENDSR;
028000091007
028100091007       //--------------------------------------------------------------
028200091007       //?Lettura file sorgente per reperire comandi e/o opzioni di    ?
028300091007       //?  compilazione                                               ?
028400091007       //--------------------------------------------------------------
028500091007       BEGSR  sr_ReadSrc;
028600100113
028700100113         clear  $Fine;
028800100113         clear  $Module;
028900120321         clear  $DltMod;
029000100113         clear  QcmdCrt;
029100120319         clear  QcmdCrtPgm;
029200100113
029300100413         // -?c) 1™ lettura?
029400100113         read  SrcF_Mbr;
029500100113
029600091007
029700100413         // -?d) Ciclo di lettura src x impostazione cmd di compilazione?
029800091020         DoW  Not %eof(SrcF_Mbr)   and   Not $Fine;
029900091026
030000091029           if  pIn_MbrType = 'BND  '   or
030100091029               pIn_MbrType = 'CLLE '   or
030200091029               pIn_MbrType = 'CLP  '   or
030300091029               pIn_MbrType = 'CMD  ';
030400100225             xx = %scan( '*/' : SrcDta );
030500091026             if  xx > *zero;
030600100225               %subst( SrcDta : xx : 2 ) = *blank;
030700091026             endif;
030800091026           endif;
030900100225
031000100225           w_SrcDta = %xlate( c_Minuscole : c_Maiuscole : SrcDta );
031100091007
031200091007           select;
031300091020
031400100225             // -?Impostata opzione di compilazione?
031500100225             when  %scan( c_Prm : w_SrcDta ) > *zero;
031600091020               exsr  sr_Prm;
031700120319
031800120319             // -?Impostati parametri per comando CRTPGM?
031900120319             when  %scan( c_Pgm : w_SrcDta ) > *zero;
032000120319               exsr  sr_Pgm;
032100091020
032200100225             // -?Richiesto comando (override)?
032300100225             when  %scan( c_Cmd : w_SrcDta ) > *zero;
032400091020               exsr  sr_Cmd;
032500091020
032600100225             // -?Comandi finali (dltovr) / Fine opzioni?
032700100225             when  %scan( c_End : w_SrcDta ) > *zero;
032800100225               if  %subst( w_SrcDta :
032900100225                           %scan( c_End : w_SrcDta ) + %len(c_End) )
033000091022                                    <> *blank;
033100091022                 exsr  sr_End;
033200091022               else;
033300091022                 $Fine = *on;
033400091022                 leave;
033500091022               endif;
033600100113
033700100225             // -?Impostata tipologia oggetto?
033800100225             when  %scan( c_Mod : w_SrcDta ) > *zero;
033900100113               $Module = *on;
034000120321               $DltMod = ( %scan( c_DltMod : w_SrcDta ) > *zero );
034100091007
034200091007           endsl;
034300091007
034400100113           read  SrcF_Mbr;
034500091007
034600091007         EndDo;
034700091022
034800100413         // -?e) Chiusura del file sorgente?
034900091022         close  SrcF_Mbr;
035000091022
035100100413         // -?f) Cancellazione override al membro sorgente?
035200091022         Qcmd = 'DLTOVR file(SrcF_Mbr)';
035300091022         exsr  sr_qCapCmd;
035400091022         //if  Qusei <> *blank;
035500091022         //  pOut_Err = c_Err_FailedDltOvrSrcMbr;
035600091022         //  exsr  sr_RoutEnd;
035700091022         //endif;
035800091020
035900100413         // -?g) Impostazione parametri base se non reperito?
036000100318         //     ?alcun parametro nel sorgente?
036100100115         if  QcmdCrt = *blank;
036200100113           exsr  sr_CrtCmd;
036300100113         endif;
036400100318         if  QcmdCrt = *blank;
036500100318           leavesr;
036600100318         endif;
036700100804
036800100804         // -?h) Impostazione opzione di sostituzione?
036900100804         IF  pIn_Replace =  *on;
037000110523           if  pIn_MbrType <> 'PF'   and   pIn_MbrType <> 'LF'   and
037100110523               pIn_MbrType <> 'BND';
037200100804             QcmdCrt += ' replace(*yes)';
037300100804             //clear  pIn_Replace;
037400100804           endif;
037500100804           if  (pIn_MbrType = 'PF'    or   pIn_MbrType = 'LF')   and
037600100804                pIn_ObjLib  = 'QTEMP';
037700100804             Qcmd = 'DLTF FILE('
037800100804                  + %trim(pIn_ObjLib) + '/' + %trim(pIn_SrcMbr)
037900100804                  + ')';
038000100804             exsr  sr_qCapCmd;
038100100804             if  Qusei <> *blank;
038200100804               pOut_Err = c_Err_NoDltExistObj;
038300100804               exsr  sr_RoutEnd;
038400100804             endif;
038500100804           endif;
038600100804         ENDIF;
038700100318
038800100804         // -?j) Esecuzione comando di compilazione?
038900100318         Qcmd = QcmdCrt;
039000100318         exsr  sr_qCapCmd;
039100100318         if  Qusei <> *blank;
039200100318           pOut_Err = c_Err_FailedCrtObj;
039300100413           //exsr  sr_RoutEnd;       //?NO: vedi operazioni finali.?
039400120319         else;
039500120321           if  $Module  and  $DltMod  and  QcmdCrtPgm <> *blank;
039600120319             Qcmd = QcmdCrtPgm;
039700120319             exsr  sr_qCapCmd;
039800120319             if  Qusei <> *blank;
039900120319               pOut_Err = c_Err_FailedCrtObj;
040000120319               //exsr  sr_RoutEnd;       //?NO: vedi operazioni finali.?
040100120319             endif;
040200120319           endif;
040300120319         endif;
040400100804
040500100804         // -?k) Gestione sostituzione nel caso di file?
040600120319         IF  pIn_Replace = *on    and
040700100804            (pIn_MbrType = 'PF'    or   pIn_MbrType = 'LF')   and
040800100804             pIn_ObjLib <> 'QTEMP';
040900100804           Qcmd = 'DLTF FILE('
041000100804                + %trim(pIn_ObjLib) + '/' + %trim(pIn_SrcMbr)
041100100804                + ')';
041200100804           exsr  sr_qCapCmd;
041300100804           if  Qusei <> *blank;
041400100804             pOut_Err = c_Err_NoDltExistObj;
041500100804             exsr  sr_RoutEnd;
041600100804           endif;
041700100804           Qcmd = 'MOVOBJ OBJ(QTEMP/' + %trim(pIn_SrcMbr) + ') +
041800100804                   OBJTYPE(*FILE) TOLIB(' + %trim(pIn_ObjLib) + ')';
041900100804           exsr  sr_qCapCmd;
042000100804           if  Qusei <> *blank;
042100100804             pOut_Err = c_Err_NoMoveFile;
042200100804             exsr  sr_RoutEnd;
042300100804           endif;
042400120319         ENDIF;
042500120319         //  ?   o cancellazione *module dalla libr. QTEMP?
042600120319         if  $Module  and  $DltMod;
042700120319           Qcmd = 'DLTMOD MODULE(QTEMP/' + %trim(pIn_SrcMbr)
042800120319                + ')';
042900120319           exsr  sr_qCapCmd;
043000120319           //if  Qusei <> *blank;
043100120319           //  ...
043200120319           //endif;
043300120319         endif;
043400091007
043500091007       ENDSR;
043600091007
043700091020       //--------------------------------------------------------------
043800120319       //?Impostazione iniziale del comando di compilazione            ?
043900091020       //--------------------------------------------------------------
044000091020       BEGSR  sr_CrtCmd;
044100100118
044200100225         // -?Creazione comando di compilazione?
044300091020         select;
044400091020
044500100113           when  pIn_MbrType = 'RPGLE'   and   not $Module;
044600100317             w_ObjTyp = '*PGM   ';
044700100318             QcmdCrt  = 'CRTBNDRPG pgm(';
044800100113           when  pIn_MbrType = 'RPGLE'   and   $Module;
044900120321             if  Not  $DltMod;
045000120321               w_ObjTyp = '*MODULE';
045100120321             else;
045200120321               w_ObjTyp = '*PGM   ';
045300120321             endif;
045400100318             QcmdCrt  = 'CRTRPGMOD module(';
045500091020
045600100113           when  pIn_MbrType = 'SQLRPGLE'   and   not $Module;
045700100317             w_ObjTyp = '*PGM   ';
045800100318             QcmdCrt  = 'CRTSQLRPGI objtype(*PGM) obj(';
045900100113           when  pIn_MbrType = 'SQLRPGLE'   and   $Module;
046000120321             if  Not  $DltMod;
046100120321               w_ObjTyp = '*MODULE';
046200120321             else;
046300120321               w_ObjTyp = '*PGM   ';
046400120321             endif;
046500100318             QcmdCrt  = 'CRTSQLRPGI objtype(*MODULE) obj(';
046600091020
046700100113           when  pIn_MbrType = 'CLLE'   and   not $Module;
046800100317             w_ObjTyp = '*PGM   ';
046900100318             QcmdCrt  = 'CRTBNDCL pgm(';
047000100113           when  pIn_MbrType = 'CLLE'   and   $Module;
047100100317             w_ObjTyp = '*MODULE';
047200100318             QcmdCrt  = 'CRTCLMOD module(';
047300091020
047400091020           when  pIn_MbrType = 'DSPF';
047500100317             w_ObjTyp = '*FILE  ';
047600100318             QcmdCrt  = 'CRTDSPF file(';
047700091020
047800091020           when  pIn_MbrType = 'PRTF';
047900100317             w_ObjTyp = '*FILE  ';
048000100318             QcmdCrt  = 'CRTPRTF file(';
048100091020
048200091020           when  pIn_MbrType = 'PF';
048300100317             w_ObjTyp = '*FILE  ';
048400100318             QcmdCrt  = 'CRTPF file(';
048500091020
048600091020           when  pIn_MbrType = 'LF';
048700100317             w_ObjTyp = '*FILE  ';
048800100318             QcmdCrt  = 'CRTLF file(';
048900091020
049000091020           when  pIn_MbrType = 'CMD';
049100100317             w_ObjTyp = '*CMD   ';
049200100317             QcmdCrt  = 'CRTCMD cmd(';
049300091020
049400100414           when  pIn_MbrType = 'BND'  and  pIn_Replace = *off;
049500100317             w_ObjTyp = '*SRVPGM';
049600100318             QcmdCrt  = 'CRTSRVPGM srvpgm(';
049700100414           when  pIn_MbrType = 'BND'  and  pIn_Replace = *on;
049800100317             w_ObjTyp = '*SRVPGM';
049900100318             QcmdCrt  = 'UPDSRVPGM srvpgm(';
050000100415             //clear  pIn_Replace;
050100091020
050200091020           when  pIn_MbrType = 'CLP';
050300100317             w_ObjTyp = '*PGM   ';
050400100318             QcmdCrt  = 'CRTCLPGM pgm(';
050500091020
050600091020           when  pIn_MbrType = 'RPG';
050700100317             w_ObjTyp = '*PGM   ';
050800100318             QcmdCrt  = 'CRTRPGPGM pgm(';
050900091020
051000091020           when  pIn_MbrType = 'SQLRPG';
051100100317             w_ObjTyp = '*PGM   ';
051200100318             QcmdCrt  = 'CRTSQLRPG pgm(';
051300091020
051400091020           other;
051500100413             //pOut_Err = c_Err_MbrTypeNotOK; ?(gi‡ controllato)?
051600100118             //exsr  sr_RoutEnd;
051700091020
051800091020         endsl;
051900091020
052000100804         //QcmdCrt += %trim(pIn_ObjLib) + '/' + %trim(pIn_SrcMbr)
052100100804         //         + ') srcfile(' + %trim(pIn_SrcF_L) + '/'
052200100804         //                        + %trim(pIn_SrcF_F)
052300100804         //         + ') srcmbr('  + %trim(pIn_SrcMbr) + ')';
052400100804
052500100804         // -?Impostazione libreria di compilazione?
052600120319         select;
052700120319           when  pIn_Replace = *on   and
052800120319                (pIn_MbrType = 'PF'   or   pIn_MbrType = 'LF');
052900120319             QcmdCrt += 'QTEMP';
053000120319           when  $Module  and  $DltMod;
053100120319             QcmdCrt += 'QTEMP';
053200120319           other;
053300120319             QcmdCrt += %trim(pIn_ObjLib);
053400120319         endsl;
053500100804
053600100804         // -?Impostazione dati del sorgente?
053700100804         QcmdCrt += '/' + %trim(pIn_SrcMbr)
053800100804                  + ') srcfile(' + %trim(pIn_SrcF_L) + '/'
053900100804                                 + %trim(pIn_SrcF_F)
054000100804                  + ') srcmbr('  + %trim(pIn_SrcMbr) + ')';
054100091020
054200091020       ENDSR;
054300120319
054400120319       //--------------------------------------------------------------
054500120319       //?Impostazione iniziale del comando "CRTPGM"                   ?
054600120319       //--------------------------------------------------------------
054700120319       BEGSR  sr_CrtCmdPgm;
054800120319
054900120319         // -?Creazione comando di creazione *pgm da *module?
055000120319         w_ObjTyp = '*PGM   ';
055100120319         QcmdCrtPgm = 'CRTPGM pgm('
055200120319                    +  %trim(pIn_ObjLib) + '/' + %trim(pIn_SrcMbr)
055300120319                    + ') module(QTEMP/' + %trim(pIn_SrcMbr) + ')';
055400120319
055500120319       ENDSR;
055600091020
055700091007       //--------------------------------------------------------------
055800091007       //?Esecuzione del comando richiesto per la compilazione         ?
055900091007       //--------------------------------------------------------------
056000091007       BEGSR  sr_Cmd;
056100091007
056200100225         xx = %scan( c_Cmd : w_SrcDta );
056300100413
056400100413         // -?Verifica se il comando termina su questo record o se?
056500100413         //  ?prosegue su quello successivo?
056600100413         $Prosegue = *off;
056700100413         if  %subst( SrcDta : %len( %trimr( SrcDta ) ) : 1 ) = '+'   or
056800100413             %subst( SrcDta : %len( %trimr( SrcDta ) ) : 1 ) = '-';
056900100413           %subst( SrcDta : %len( %trimr( SrcDta ) ) : 1 ) = *blank;
057000100413           $Prosegue = *on;
057100100413         endif;
057200100413
057300100413         if  $Prosieguo;
057400100413           // -?Aggiunge i parametri di questo record al comando gi‡?
057500100413           //  ?"iniziato" precedentemente?
057600100413           Qcmd += ' ' + %trim( %subst( SrcDta : xx + %len(c_Cmd) ) );
057700100413         else;
057800100413           // -?Prapara il comando?
057900100413           Qcmd = %trim( %subst( SrcDta : xx + %len(c_Cmd) ) );
058000100413         endif;
058100100413
058200100413         // -?Comando NON terminato in questo record: uscita?
058300100413         if  $Prosegue;
058400100413           $Prosieguo = *on;
058500100413           leavesr;
058600100413         endif;
058700100413
058800100413         $Prosieguo = *off;
058900100413
059000100413         // -?Comando terminato: sua esecuzione?
059100091021         exsr  sr_qCapCmd;
059200091007
059300091007         if  Qusei <> *blank;
059400091007           pOut_Err = c_Err_FailedRiquestCmd;
059500091007           exsr  sr_RoutEnd;
059600091007         endif;
059700091007
059800091007       ENDSR;
059900091007
060000091007       //--------------------------------------------------------------
060100091007       //?Impostazione comando di compilazione per il sorgente         ?
060200091007       //--------------------------------------------------------------
060300091007       BEGSR  sr_Prm;
060400100113
060500100413         // -?Eventuale esecuzione del comando di cui non Ë stata?
060600100413         //  ?terminata l'impostazione dei parametri?
060700100413         if  $Prosieguo;
060800100413           reset $Prosieguo;
060900100413           exsr  sr_qCapCmd;
061000100413           if  Qusei <> *blank;
061100100413             pOut_Err = c_Err_FailedRiquestCmd;
061200100413             exsr  sr_RoutEnd;
061300100413           endif;
061400100413         endif;
061500100413
061600100225         // -?Impostazione iniziale del comando di compilazione?
061700100113         if  QcmdCrt = *blank;
061800100113           exsr  sr_CrtCmd;
061900100113         endif;
062000091007
062100100225         // -?Aggiunta dei parametri in lettura?
062200100225         xx = %scan( c_Prm : w_SrcDta );
062300091020         QcmdCrt += ' ' + %trim( %subst( SrcDta : xx + %len(c_Prm) ) );
062400091007
062500091007       ENDSR;
062600120319
062700120319       //--------------------------------------------------------------
062800120319       //?Impostazione comando di creazione *pgm da *module            ?
062900120319       //--------------------------------------------------------------
063000120319       BEGSR  sr_Pgm;
063100120319
063200120319         // -?Eventuale esecuzione del comando di cui non Ë stata?
063300120319         //  ?terminata l'impostazione dei parametri?
063400120319         if  $Prosieguo;
063500120319           reset $Prosieguo;
063600120319           exsr  sr_qCapCmd;
063700120319           if  Qusei <> *blank;
063800120319             pOut_Err = c_Err_FailedRiquestCmd;
063900120319             exsr  sr_RoutEnd;
064000120319           endif;
064100120319         endif;
064200120319
064300120319         // -?Impostazione iniziale del comando di compilazione?
064400120319         if  QcmdCrtPgm = *blank;
064500120319           exsr  sr_CrtCmdPgm;
064600120319         endif;
064700120319
064800120319         // -?Aggiunta dei parametri in lettura?
064900120319         xx = %scan( c_Pgm : w_SrcDta );
065000120319         QcmdCrtPgm += ' ' +
065100120319                       %trim( %subst( SrcDta : xx + %len(c_Pgm) ) );
065200120319
065300120319       ENDSR;
065400091022
065500091022       //--------------------------------------------------------------
065600091022       //?Memorizzazione del comando richiesto a fine compilazione     ?
065700091022       //--------------------------------------------------------------
065800091022       BEGSR  sr_End;
065900100413
066000100413         xx = %scan( c_End : w_SrcDta );
066100100413
066200100413         // -?Verifica se il comando termina su questo record o se?
066300100413         //  ?prosegue su quello successivo?
066400100413         $Prosegue = *off;
066500100413         if  %subst( SrcDta : %len( %trimr( SrcDta ) ) : 1 ) = '+'   or
066600100413             %subst( SrcDta : %len( %trimr( SrcDta ) ) : 1 ) = '-';
066700100413           %subst( SrcDta : %len( %trimr( SrcDta ) ) : 1 ) = *blank;
066800100413           $Prosegue = *on;
066900100413         endif;
067000100413
067100100413         if  $Prosieguo;
067200100413           // -?Aggiunge i parametri di questo record all'ultimo?
067300100413           //  ?comando gi‡ "iniziato"?
067400100413           $CmdEnd(yy) += ' ' +
067500100413                          %trim( %subst( SrcDta : xx + %len(c_Cmd) ) );
067600100413         else;
067700100413           // -?Prapara un nuovo comando?
067800100413           yy += 1;
067900100413           $CmdEnd(yy) = %trim( %subst( SrcDta : xx + %len(c_End) ) );
068000100413         endif;
068100100413
068200100413         // -?Indicazione del proseguimento dello stesso comando?
068300100413         $Prosieguo = $Prosegue;
068400091022
068500091022       ENDSR;
068600091007
068700091007       //--------------------------------------------------------------
068800091021       //?Esecuzione del comando (gi‡ impostato): API QCAPCMD.         ?
068900091007       //--------------------------------------------------------------
069000091021       BEGSR  sr_qCapCmd;
069100091007
069200091007         clear Qcap0100;
069300091007         Qcabcsdh = *off;
069400091007         Qcapa    = *off;
069500091007         Qcacmdss = *off;
069600091007         Qcaerved = *allX'00';
069700091007
069800091007         clear Qusec;
069900091007         Qusbprv  = %size(Qusec);
070000091007
070100091007         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
070200091007                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
070300091007                           0 : 0 : Qusec);
070400091007
070500100413         // -?(l'eventuale errore verr‡ testato dopo e restituito al?
070600100413         //  ?chiamante - non emesso qui)?
070700091007         //if  Qusei <> *blank;
070800091007         //  exsr  sr_MsgErr;
070900091007         //endif;
071000091007
071100091007       ENDSR;
071200091007
071300091007       //--------------------------------------------------------------
071400091007       //?Invio messaggio per segnalazione errori.                     ?
071500100413       //?(l'eventuale errore verr‡ testato dopo e restituito al?
071600100413       //?chiamante - non emesso qui)?
071700091007       //--------------------------------------------------------------
071800091007       //BEGSR  sr_MsgErr;
071900091007       //
072000100413       //  sndmsg  msg(&MsgTxt) tousr(&User)
072100091007       //
072200091007       //ENDSR;
072300091007
072400091007       //--------------------------------------------------------------
072500091007       //?Operazioni finali.                                           ?
072600091007       //--------------------------------------------------------------
072700091007       BEGSR  sr_RoutEnd;
072800100113
072900100225         // -?Esecuzione eventuali comandi finali?
073000100225         //  ?(eseguiti anche per compilazione con esito negativo)?
073100100113         For  xx = 1  To  yy;
073200100113
073300100113           Qcmd = $CmdEnd(xx);
073400100113           exsr  sr_qCapCmd;
073500100113
073600100113           if  Qusei <> *blank   and   pOut_Err = *zero;
073700100113             pOut_Err = c_Err_FailedEndCmd;
073800100413             //leave;    //?l'errore verr‡ segnalato ma non Ë bloccante?
073900100113           endif;
074000100113
074100100113         EndFor;
074200091007
074300100113
074400100225         // -?Impostazione messaggio di uscita?
074500091007         select;
074600091007
074700100225           // -?Creato l'oggetto?
074800091007           when  pOut_Err = *zero;
074900091026             pOut_MsgTxt = 'Creato l''oggetto '
075000091022                         + %trim( pIn_SrcMbr )
075100100317                         + ' di tipo '
075200100317                         + %trim(w_ObjTyp)
075300091022                         + ' in '
075400091022                         + %trim(pIn_ObjLib)
075500091022                         + '.';
075600091007
075700100414           // -?(-21) Fallita override al membro sorgente?
075800091007           when  pOut_Err = c_Err_FailedOvrSrcMbr;
075900091022             pOut_MsgTxt = 'Non trovato il file-sorgente '
076000091022                         + %trim( pIn_SrcF_L )
076100091022                         + '/'
076200091029                         + %trim( pIn_SrcF_F );
076300091020
076400100414           // -?(-22) Fallita esecuzione del comando richiesto (/*CMD)?
076500091007           when  pOut_Err = c_Err_FailedRiquestCmd;
076600091022             pOut_MsgTxt = 'Fallita esecuzione di un comando richiesto +
076700091022                            per la compilazione del sorgente '
076800091029                         + %trim( pIn_SrcMbr );
076900100416
077000100416           // -?(-23) Rimozione file gi‡ esistente NON riuscita?
077100100416           when  pOut_Err = c_Err_NoDltExistObj;
077200100416             pOut_MsgTxt = 'Fallita la cancellazione del file '
077300100416                         + %trim( pIn_ObjLib )
077400100416                         + '/'
077500100416                         + %trim( pIn_SrcMbr );
077600100804
077700100804           // -?(-24) Rimozione file da QTEMP NON riuscita?
077800100804           when  pOut_Err = c_Err_NoDltQtempFile;
077900100804             pOut_MsgTxt = 'Fallita la cancellazione del file +
078000100804                           QTEMP/'
078100100804                         + %trim( pIn_SrcMbr );
078200100804
078300100804           // -?(-25) Fallito spostamento file dalla QTEMP?
078400100804           when  pOut_Err = c_Err_NoMoveFile;
078500100804             pOut_MsgTxt = 'Fallito spostamento del file '
078600100804                         + %trim( pIn_SrcMbr )
078700100804                         + ' dalla libr. QTEMP alla libr. '
078800100804                         + %trim( pIn_ObjLib );
078900091007
079000100414           // -?(-29) Compilazione non riuscita?
079100091007           when  pOut_Err = c_Err_FailedCrtObj;
079200091022             pOut_MsgTxt = 'Compilazione non riuscita. Oggetto '
079300091022                         + %trim( pIn_SrcMbr )
079400091029                         + ' non creato';
079500091022
079600100414           // -?(+21) Fallita esecuzione del comando richiesto (/*END)?
079700091022           when  pOut_Err = c_Err_FailedEndCmd;
079800091022             pOut_MsgTxt = 'Fallita esecuzione di un comando richiesto +
079900091022                            DOPO la compilazione del sorgente '
080000091029                         + %trim( pIn_SrcMbr );
080100091007
080200091007         endsl;
080300091007
080400100414         // -?Uscita?
080500091007         return;
080600091007
080700091007       ENDSR;
080800091007
080900091007      /end-free

000100100915     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR':'TRUL') ACTGRP('BARTVAS')
000200100916     H decedit('0,') datedit(*ymd-) option(*nodebugio)
000300990901     F*------------------------------------------------------------------------*
000400990901     F* DATA BASE
000500990901     F*------------------------------------------------------------------------*
000600100915     FTIVIR02L  UF A E           K DISK
000700001005     F*------------------------------------------------------------------------*
000800100914
000900100914     D tivssds       e ds                  extname(tivss00f) prefix(§21_)
001000100914     D tivirds       e ds                  extname(tivir00f) prefix(§21_)
001100100915     D tivirdsw      e ds                  extname(tivir00f)
001200100914     D tis725ds21    e ds
001300100914     D kpjba         e ds
001400100913
001500100826     D*------------------------------------------------------------------------*
001600100913
001700100913     D* Reperimento dati del PGM
001800100913     D PGMSTATUS      SDS
001900100913     D JOB_USER              254    263
002000100913
002100100913     D* Variabili d wrk
002200100915     D wvirKSC         S                   like(§21_virKSC) inz
002300100915     D wvirTIP         S                   like(§21_virTIP) inz
002400100916     D wData           S               D   inz
002500100916     D wPercorso       S            100A   inz
002600101013     D wProcesso       S              5I 0 inz
002700100916     D wKeywParam      S             50A   inz
002800100913
002900100913
003000100827
003100100827     D*-------------------
003200100827     D* DEFINIZIONI PROCEDURE ESTERNE
003300100827     D*-------------------
003400101004     D/COPY GAITRASRC/SRCPROTOPI,UBBVSQL01
003500101004     D/COPY GAITRASRC/SRCPROTOPR,UBBVSQL01
003600100913     D/COPY GAITRASRC/SRCPROTOPI,UBSOCKET
003700100913     D/COPY GAITRASRC/SRCPROTOPR,UBSOCKET
003800100827
003900100827
004000100914     c*---------------------------------------------------------------*
004100100914     c* /MAIN
004200100914     c*---------------------------------------------------------------*
004300100914      *
004400100914      * Verifico il tipo passaggio parametri ricevuto
004500100914     c                   select
004600100920      *
004700101004      * - gestione tipo "2"
004800101004     c                   when      ds_typ = '2'
004900100914     c                   eval      tis725ds21 = ds_param
005000100914     c                   exsr      procedi_01
005100100914     c                   eval      ds_param = tis725ds21
005200100920     c*
005300110210     c                   if        §21_FILE   = 'ESY'
005400100920     c                   eval      kpjbu = %subst(§21_vssKSU:2:7)
005500100920     c                   call      'TRTB28R'
005600100920     c                   parm                    kpjba
005700100920     c*
005800100920     c                   call      'TIS7EMLR5'
005900100920     c                   parm                    §21_vssKSU
006000110210     c                   endif
006100101004      *
006200101004      * - gestione tipo "3"
006300101004     c                   when      ds_typ = '3'
006400101004     c                   eval      tis725ds21 = ds_param
006500101004     c                   exsr      procedi_01
006600101004     c                   eval      ds_param = tis725ds21
006700101013      *
006800101013      * - gestione tipo "4"
006900101013     c                   when      ds_typ = '4'
007000101013     c                   eval      tis725ds21 = ds_param
007100101013 xxx c                   exsr      procedi_02
007200101013     c                   eval      ds_param = tis725ds21
007300101004      *
007400100914     c                   endsl
007500100914      *
007600100914     c                   SETON                                        LR
007700100914     c*---------------------------------------------------------------*
007800100914
007900100914
008000100914
008100100914
008200100914     c*---------------------------------------------------------------*
008300100914     c     PROCEDI_01    BEGSR
008400100913      *
008500100913     c                   setoff                                       40
008600100826      *
008700100913      * Inizializzo i parametri d output
008800100913     c                   if        not *in40
008900101013     c                   exsr      inzOUT
009000100826     c                   endif
009100100913      *
009200100913      * Verifico parametri in ingresso
009300100913     c                   if        not *in40
009400101013     c                   exsr      chkPAR
009500100913     c                   endif
009600100914      *
009700100914      * Verifico l'opzione d entrata richiesta
009800100914     c                   if        not *in40
009900100916      *
010000100916      * Apro la connessione
010100100916     c                   exsr      opnCONN
010200100916      *
010300100916      * Reperisco i parametri d procedura BartVAS necessari
010400100916     c                   eval      wKeywParam  = 'DirUtenti'
010500100916     c                   exsr      rtvPARAM
010600100916     c                   eval      wPercorso   = pOutValParam
010700100916      *
010800101013     c                   select
010900101013     c                   when      §21_vssISV = 'IE'
011000100916     c                   eval      wKeywParam  = 'DefaultProcIE'
011100101013     c                   when      §21_vssISV = 'IB'
011200101013     c                   eval      wKeywParam  = 'DefaultProcIB'
011300101013     c                   when      §21_vssISV = 'IF'
011400101013     c                   eval      wKeywParam  = 'DefaultProcIF'
011500101013     c                   when      §21_vssISV = 'OE'
011600101013     c                   eval      wKeywParam  = 'DefaultProcOE'
011700101013     c                   when      §21_vssISV = 'OB'
011800101013     c                   eval      wKeywParam  = 'DefaultProcOB'
011900101013     c                   when      §21_vssISV = 'OF'
012000101013     c                   eval      wKeywParam  = 'DefaultProcOF'
012100101013     c                   endsl
012200101013      *
012300100916     c                   exsr      rtvPARAM
012400101013     c                   eval      wProcesso = %dec(pOutValParam:4:0)
012500100916      *
012600100916     c                   if        not *in40
012700100914     c                   select
012800100914      *
012900100914      * Verifica/inserimento UTENTE
013000100914     c                   when      §21_FILE   = 'VSS' and
013100100914     c                             §21_OPERAZ = 'INS'
013200100914     c                   exsr      insVSS
013300100914      *
013400100914      * Verifica/inserimento UTENTE + TIPI FILE
013500100914     c                   when      §21_FILE   = 'ESY'
013600100915     c                   exsr      insESYSP
013700100914      *
013800100914     c                   endsl
013900100916     c                   endif
014000100916      *
014100100916      * Chiudo la connessione
014200100916     c                   exsr      clsCONN
014300100914     c                   endif
014400100914      *
014500100914     c                   endsr
014600100914     c*------------------------------------------------------------------------*
014700101013
014800101013
014900101013
015000101013
015100101013     c*---------------------------------------------------------------*
015200101013     c     PROCEDI_02    BEGSR
015300101013      *
015400101013     c                   setoff                                       40
015500101013      *
015600101013      * Inizializzo i parametri d output
015700101013     c                   if        not *in40
015800101013     c                   exsr      inzOUT
015900101013     c                   endif
016000101013      *
016100101013      * Verifico parametri in ingresso
016200101013     c                   if        not *in40
016300101013     c                   exsr      chkPAR
016400101013     c                   endif
016500101013      *
016600101013      * Verifico l'opzione d entrata richiesta
016700101013     c                   if        not *in40
016800101013      *
016900101013      * Apro la connessione
017000101013     c                   exsr      opnCONN
017100101013      *
017200101013      * Reperisco i parametri d procedura BartVAS necessari
017300101013     c                   eval      wKeywParam  = 'DirUtenti'
017400101013     c                   exsr      rtvPARAM
017500101013     c                   eval      wPercorso   = pOutValParam
017600101013      *
017700101013     c                   select
017800101013     c                   when      §21_virFI1 = 'IE'
017900101013     c                   eval      wKeywParam  = 'DefaultProcIE'
018000101013     c                   when      §21_virFI1 = 'IB'
018100101013     c                   eval      wKeywParam  = 'DefaultProcIB'
018200101013     c                   when      §21_virFI1 = 'IF'
018300101013     c                   eval      wKeywParam  = 'DefaultProcIF'
018400101013     c                   when      §21_virFI1 = 'OE'
018500101013     c                   eval      wKeywParam  = 'DefaultProcOE'
018600101013     c                   when      §21_virFI1 = 'OB'
018700101013     c                   eval      wKeywParam  = 'DefaultProcOB'
018800101013     c                   when      §21_virFI1 = 'OF'
018900101013     c                   eval      wKeywParam  = 'DefaultProcOF'
019000101013     c                   endsl
019100101013      *
019200101013     c                   exsr      rtvPARAM
019300101013     c                   eval      wProcesso = %dec(pOutValParam:4:0)
019400101013      *
019500101013     c                   if        not *in40
019600101013     c                   select
019700101013      *
019800101013      * Verifica/inserimento TIPI FILE
019900101013     c                   when      §21_FILE   = 'VIR' and
020000101013     c                             §21_OPERAZ = 'INS'
020100101013      *
020200101013     c                   select
020300101013     c                   when      §21_virFI1 = 'IE'
020400101013     c                   exsr      insVIR_IE
020500101013     c                   when      §21_virFI1 = 'IB'
020600101013     c                   exsr      insVIR_IB
020700101013     c                   when      §21_virFI1 = 'IF'
020800110209 xxx c***                exsr      insVIR_IF
020900101013     c                   when      §21_virFI1 = 'OE'
021000110209 xxx c***                exsr      insVIR_OE
021100101013     c                   when      §21_virFI1 = 'OB'
021200110209 xxx c***                exsr      insVIR_OB
021300101013     c                   when      §21_virFI1 = 'OF'
021400110209 xxx c***                exsr      insVIR_OF
021500101013     c                   endsl
021600101013      *
021700101013     c                   endsl
021800101013     c                   endif
021900101013      *
022000101013      * Chiudo la connessione
022100101013     c                   exsr      clsCONN
022200101013     c                   endif
022300101013      *
022400101013     c                   endsr
022500101013     c*------------------------------------------------------------------------*
022600100916
022700100916
022800100916
022900100916     c*------------------------------------------------------------------------*
023000100916     c* rtvPARAM - REPERIMNTO PARAMETRO DI PROCEDURA
023100100916     c*------------------------------------------------------------------------*
023200100916     c     RTVPARAM      BEGSR
023300100916      /FREE
023400100916
023500100916        if not *in40;
023600101004           if UBBVSQL01_RtvParametro(pInConn : wKeywParam : pOutValParam) = 0;
023700100916           else;
023800100916              *in40 = *on;
023900100916              §21_ERRFLG = 'E';
024000100916              §21_ERRMSG = 'Errore BartVAS: fatal error ';
024100100916           endif;
024200100916        endif;
024300100916
024400100916      /END-FREE
024500100916      *
024600100916     c                   ENDSR
024700100916     c*------------------------------------------------------------------------*
024800100916
024900100916
025000100916
025100100916     c*------------------------------------------------------------------------*
025200100916     c* opnCONN - Apertura connessione
025300100916     c*------------------------------------------------------------------------*
025400100916     c     OPNCONN       BEGSR
025500100916      /FREE
025600100916
025700100916        if not *in40;
025800101004           if UBBVSQL01_Connect('BARTVAS' : pOutConn) = 0;
025900100916              pInConn = pOutConn;
026000100916           else;
026100100916              *in40 = *on;
026200100916              §21_ERRFLG = 'E';
026300100916              §21_ERRMSG = 'Errore BartVAS: fatal error ';
026400100916           endif;
026500100916        endif;
026600100916
026700100916      /END-FREE
026800100916      *
026900100916     c                   ENDSR
027000100916     c*------------------------------------------------------------------------*
027100100916
027200100916
027300100916
027400100916     c*------------------------------------------------------------------------*
027500100916     c* clsCONN - Chiusura connessione
027600100916     c*------------------------------------------------------------------------*
027700100916     c     CLSCONN       BEGSR
027800100916      /FREE
027900100916
028000101004           if UBBVSQL01_Disconnect(pInConn) = 0;
028100100916           else;
028200100916              *in40 = *on;
028300100916           endif;
028400100916
028500100916      /END-FREE
028600100916      *
028700100916     c                   ENDSR
028800100916     c*------------------------------------------------------------------------*
028900100827
029000100827
029100100827
029200100914     c*------------------------------------------------------------------------*
029300100914     c* insVSS - INSERIMENTO UTENTE
029400100914     c*------------------------------------------------------------------------*
029500100914     c     INSVSS        BEGSR
029600100913      /FREE
029700100913
029800100916         if not *in40;
029900100913
030000101004              if UBBVSQL01_ChkUtente(pInConn : §21_UTENTE : pOutNumRec) = 0;
030100100913                 if pOutNumRec = 0;
030200101004                    if UBBVSQL01_InsUtente(
030300100916                                    pInConn
030400100914                                  : §21_UTENTE
030500100914                                  : §21_vssKSU
030600100914                                  : §21_PASSW
030700100916                                  : %trim(wPercorso)+§21_UTENTE
030800100914                                  : §21_DESCR
030900100913                                  : '0'
031000100913                                  : '1'
031100100913                                  : '0'
031200100913                                  : JOB_USER ) = 0;
031300100913                    else;
031400100914                        §21_ERRFLG = 'E';
031500100914                        §21_ERRMSG = 'Errore BartVAS: inserimento UTENTE ' +
031600100914                                     'non effettuato';
031700100913                    endif;
031800100913                 else;
031900101005           //       §21_ERRFLG = 'E';
032000101005           //       §21_ERRMSG = 'Errore BartVAS: UTENTE già presente, ' +
032100101005           //                    'verificare';
032200100913                 endif;
032300100913              endif;
032400100913
032500100913         endif;
032600100913
032700100827      /END-FREE
032800100914      *
032900100914     c                   ENDSR
033000100914     c*------------------------------------------------------------------------*
033100101013
033200101013
033300101013
033400101013     c*------------------------------------------------------------------------*
033500101013     c* INSVIR - INSERIMENTO TIPI FILE "DA VIR"
033600101013     c*------------------------------------------------------------------------*
033700101013     c     INSVIR_IE     BEGSR
033800101013      *
033900101013     c                   if        not *in40
034000101013      *
034100110209     c                   eval      pInCodCli      = §21_virKSC
034200101013     c                   eval      pInTipFile     = §21_virTIP
034300101013     c                   eval      pInTipTrasf    = §21_virFI1
034400101013     c                   eval      pInMaskChk     = §21_MSKF
034500101013     c                   eval      pInMaskSem     = §21_MSKS
034600101013     c                   eval      pInMaskZip     = §21_MSKZ
034700101013     c                   eval      pInPercorso    = %trim(wPercorso)           +
034800101013     c                                              %subst(§21_virKSC:2:7)     +
034900101013     c                                              '\IN'
035000101013     c                   eval      pInSoggetto    = %subst(§21_virKSC:2:7)
035100101013     c                   eval      pInPercorsOld  = %trim(wPercorso)           +
035200101013     c                                              %subst(§21_virKSC:2:7)     +
035300101013     c                                              '\OLDIN'
035400101013     c                   eval      pInTentativi   = 100
035500101013     c                   eval      pInRitardo     = 60
035600101013     c                   eval      pInLibAS4      = 'VASUPLOAD'
035700101013     c                   eval      pInFilAS4      = '*'
035800101013     c                   eval      pInMbrAS4      = 'M!ctr1:.'
035900101013     c                   eval      pInSchedul     = '1-31@05:30-23:50+00:10'
036000101013     c                   eval      pInAccorpam    = 0
036100101013     c                   eval      pInDataStr     = %char(wData)
036200101013     c                   eval      pInDataEnd     = '2039-12-31'
036300101013     c                   eval      pInCodCompr    = 'ZIP'
036400101013     c                   eval      pInUsrIns      = JOB_USER
036500110209     c                   eval      pInNote        = 'Inserimento da AS/400'
036600101013     c                   eval      pInProcesso    = wProcesso
036700101013      *
036800101013      * Inserimento lato BartVAS
036900101013     c
037000101013      /FREE
037100101013
037200101013              if UBBVSQL01_ChkCliTipFile(pInConn : §21_virKSC : §21_virTIP :
037300101013                                         pOutNumRec) = 0;
037400101013                 if pOutNumRec = 0;
037500101013                    if UBBVSQL01_InsCliTipFile(
037600101013                                    pInConn
037700101013                                  : pInCodCli
037800101013                                  : pInTipFile
037900101013                                  : pInTipTrasf
038000101013                                  : pInMaskChk
038100101013                                  : pInMaskFile
038200101013                                  : pInMaskSem
038300101013                                  : pInPercorso
038400101013                                  : pInEmail1
038500101013                                  : pInEmail2
038600101013                                  : pInEmail3
038700101013                                  : pInEmail4
038800101013                                  : pInEmail5
038900101013                                  : pInSoggetto
039000101013                                  : pInTesto
039100101013                                  : pInFtpAddr
039200101013                                  : pInFtpDir
039300101013                                  : pInFtpUser
039400101013                                  : pInFtpPwd
039500101013                                  : pInFtpFilAS4
039600101013                                  : pInPercorsOld
039700101013                                  : pInTentativi
039800101013                                  : pInRitardo
039900101013                                  : pInLibAS4
040000101013                                  : pInFilAS4
040100101013                                  : pInMbrAS4
040200101013                                  : pInProcesso
040300101013                                  : pInSchedul
040400101013                                  : pInAccorpam
040500101013                                  : pInMaskZip
040600101013                                  : pInDataStr
040700101013                                  : pInDataEnd
040800101013                                  : pInCodCompr
040900101013                                  : pInFtpCmd1
041000101013                                  : pInEmailErr
041100101013                                  : pInHttpUrlTo
041200101013                                  : pInHttpUsrTo
041300101013                                  : pInHttpPwdTo
041400101013                                  : pInHttpConTyp
041500101013                                  : pInHttpSndEXE
041600101013                                  : pInHttpSndPrm
041700101013                                  : pInHttpUsPrxy
041800101013                                  : pInUsrIns
041900101013                                  : pInNote
042000101013                                  : pInFtpPort
042100101013                                  : pInFtpTyp
042200101013                                  ) = 0;
042300101013                    else;
042400110209                        §21_ERRFLG = 'E';
042500110209                        §21_ERRMSG = 'Errore BartVAS: ins. TIPO FILE:' +
042600110209                               §21_virKSC +'/'+ §21_virTIP + ' non effettuato';
042700110209                    endif;
042800110209                 else;
042900110209                    §21_ERRFLG = 'E';
043000110209                    §21_ERRMSG = 'Errore BartVAS: TIPO FILE:' +
043100110209                               §21_virKSC +'/'+ §21_virTIP + ' già presente';
043200101013                 endif;
043300101013              endif;
043400101013
043500101013      /END-FREE
043600101013      *
043700110209      *
043800110209     c                   endif
043900110209      *
044000101013     c                   ENDSR
044100101013     c*------------------------------------------------------------------------*
044200101013
044300101013
044400101013
044500101013     c*------------------------------------------------------------------------*
044600101013     c* INSVIR - INSERIMENTO TIPI FILE "DA VIR"
044700101013     c*------------------------------------------------------------------------*
044800101013     c     INSVIR_IB     BEGSR
044900101013      *
045000101013     c                   if        not *in40
045100101013      *
045200110209     c                   eval      pInCodCli      = §21_virKSC
045300101013     c                   eval      pInTipFile     = §21_virTIP
045400101013     c                   eval      pInTipTrasf    = §21_virFI1
045500101013     c                   eval      pInMaskChk     = §21_MSKF
045600101013     c                   eval      pInMaskSem     = §21_MSKS
045700101013     c                   eval      pInMaskZip     = §21_MSKZ
045800101013     c                   eval      pInPercorso    = %trim(wPercorso)           +
045900101013     c                                              %subst(§21_virKSC:2:7)     +
046000101013     c                                              '\IN'
046100101013     c                   eval      pInPercorsOld  = %trim(wPercorso)           +
046200101013     c                                              %subst(§21_virKSC:2:7)     +
046300101013     c                                              '\OLDIN'
046400101013     c                   eval      pInTentativi   = 100
046500101013     c                   eval      pInRitardo     = 60
046600101013     c                   eval      pInLibAS4      = 'VASUPLOAD'
046700101013     c                   eval      pInFilAS4      = '*'
046800101013     c                   eval      pInMbrAS4      = 'M!ctr1:.'
046900101013     c                   eval      pInSchedul     = '1-31@05:30-23:50+00:10'
047000101013     c                   eval      pInAccorpam    = 0
047100101013     c                   eval      pInDataStr     = %char(wData)
047200101013     c                   eval      pInDataEnd     = '2039-12-31'
047300101013     c                   eval      pInCodCompr    = 'ZIP'
047400101013     c                   eval      pInUsrIns      = JOB_USER
047500110209     c                   eval      pInNote        = 'Inserimento da AS/400'
047600101013     c                   eval      pInProcesso    = wProcesso
047700101013      *
047800101013      * Inserimento lato BartVAS
047900101013     c
048000101013      /FREE
048100101013
048200101013              if UBBVSQL01_ChkCliTipFile(pInConn : §21_virKSC : §21_virTIP :
048300101013                                         pOutNumRec) = 0;
048400101013                 if pOutNumRec = 0;
048500101013                    if UBBVSQL01_InsCliTipFile(
048600101013                                    pInConn
048700101013                                  : pInCodCli
048800101013                                  : pInTipFile
048900101013                                  : pInTipTrasf
049000101013                                  : pInMaskChk
049100101013                                  : pInMaskFile
049200101013                                  : pInMaskSem
049300101013                                  : pInPercorso
049400101013                                  : pInEmail1
049500101013                                  : pInEmail2
049600101013                                  : pInEmail3
049700101013                                  : pInEmail4
049800101013                                  : pInEmail5
049900101013                                  : pInSoggetto
050000101013                                  : pInTesto
050100101013                                  : pInFtpAddr
050200101013                                  : pInFtpDir
050300101013                                  : pInFtpUser
050400101013                                  : pInFtpPwd
050500101013                                  : pInFtpFilAS4
050600101013                                  : pInPercorsOld
050700101013                                  : pInTentativi
050800101013                                  : pInRitardo
050900101013                                  : pInLibAS4
051000101013                                  : pInFilAS4
051100101013                                  : pInMbrAS4
051200101013                                  : pInProcesso
051300101013                                  : pInSchedul
051400101013                                  : pInAccorpam
051500101013                                  : pInMaskZip
051600101013                                  : pInDataStr
051700101013                                  : pInDataEnd
051800101013                                  : pInCodCompr
051900101013                                  : pInFtpCmd1
052000101013                                  : pInEmailErr
052100101013                                  : pInHttpUrlTo
052200101013                                  : pInHttpUsrTo
052300101013                                  : pInHttpPwdTo
052400101013                                  : pInHttpConTyp
052500101013                                  : pInHttpSndEXE
052600101013                                  : pInHttpSndPrm
052700101013                                  : pInHttpUsPrxy
052800101013                                  : pInUsrIns
052900101013                                  : pInNote
053000101013                                  : pInFtpPort
053100101013                                  : pInFtpTyp
053200101013                                  ) = 0;
053300101013                    else;
053400101013                        §21_ERRFLG = 'E';
053500110209                        §21_ERRMSG = 'Errore BartVAS: ins. TIPO FILE:' +
053600110209                               §21_virKSC +'/'+ §21_virTIP + ' non effettuato';
053700101013                    endif;
053800101013                 else;
053900101013                    §21_ERRFLG = 'E';
054000110209                    §21_ERRMSG = 'Errore BartVAS: TIPO FILE:' +
054100110209                               §21_virKSC +'/'+ §21_virTIP + ' già presente';
054200101013                 endif;
054300101013              endif;
054400101013
054500101013      /END-FREE
054600101013      *
054700110209      *
054800110209     c                   endif
054900110209      *
055000101013     c                   ENDSR
055100101013     c*------------------------------------------------------------------------*
055200100914
055300100914
055400100914
055500100914     c*------------------------------------------------------------------------*
055600101013     c* INSVIR_VSS - INSERIMENTO TIPI FILE "DA VSS"
055700100914     c*------------------------------------------------------------------------*
055800101013     c     INSVIR_VSS    BEGSR
055900100916      *
056000100916     c                   if        not *in40
056100100915      *
056200100915      * Innanzitutto verifico che il record nn sia già presente su AS/400
056300100915     c     KEY_vir02_P   chain     tivir02l
056400100915     c                   if        %found(tivir02l)
056500100915     c                   eval      §21_ERRFLG = 'E'
056600100915     c                   eval      §21_ERRMSG = 'Errore AS/400: tipo file ' +
056700100915     c                             wvirTIP + ' già presente. Verificare'
056800100915      *
056900100915     c                   else
057000100915      *
057100100915      * Procedo con l'inserimento lato AS/400
057200100915     c                   eval      tivirdsw = tivirds
057300100915     c                   write     tivir000
057400100915      *
057500100915      * Quindi proseguo con l'inserimento lato BartVAS
057600100915     c
057700100914      /FREE
057800100914
057900101004              if UBBVSQL01_ChkCliTipFile(pInConn : §21_virKSC : §21_virTIP :
058000100914                                         pOutNumRec) = 0;
058100100914                 if pOutNumRec = 0;
058200101004                    if UBBVSQL01_InsCliTipFile(
058300100916                                    pInConn
058400100916                                  : pInCodCli
058500100916                                  : pInTipFile
058600100916                                  : pInTipTrasf
058700100916                                  : pInMaskChk
058800100916                                  : pInMaskFile
058900100916                                  : pInMaskSem
059000100916                                  : pInPercorso
059100100916                                  : pInEmail1
059200100916                                  : pInEmail2
059300100916                                  : pInEmail3
059400100916                                  : pInEmail4
059500100916                                  : pInEmail5
059600100916                                  : pInSoggetto
059700100916                                  : pInTesto
059800100916                                  : pInFtpAddr
059900100916                                  : pInFtpDir
060000100916                                  : pInFtpUser
060100100916                                  : pInFtpPwd
060200100916                                  : pInFtpFilAS4
060300100916                                  : pInPercorsOld
060400100916                                  : pInTentativi
060500100916                                  : pInRitardo
060600100916                                  : pInLibAS4
060700100916                                  : pInFilAS4
060800100916                                  : pInMbrAS4
060900100916                                  : pInProcesso
061000100916                                  : pInSchedul
061100100916                                  : pInAccorpam
061200100916                                  : pInMaskZip
061300100916                                  : pInDataStr
061400100916                                  : pInDataEnd
061500100916                                  : pInCodCompr
061600100916                                  : pInFtpCmd1
061700100916                                  : pInEmailErr
061800100916                                  : pInHttpUrlTo
061900100916                                  : pInHttpUsrTo
062000100916                                  : pInHttpPwdTo
062100100916                                  : pInHttpConTyp
062200100916                                  : pInHttpSndEXE
062300100916                                  : pInHttpSndPrm
062400100916                                  : pInHttpUsPrxy
062500100916                                  : pInUsrIns
062600100916                                  : pInNote
062700100916                                  : pInFtpPort
062800100916                                  : pInFtpTyp
062900100916                                  ) = 0;
063000100914                    else;
063100100914                        §21_ERRFLG = 'E';
063200100916                        §21_ERRMSG = 'Errore BartVAS: inserimento TIPO FILE ' +
063300100916                                     §21_virTIP + ' non effettuato';
063400100914                    endif;
063500100914                 else;
063600100914                    §21_ERRFLG = 'E';
063700100916                    §21_ERRMSG = 'Errore BartVAS: TIPO FILE ' +
063800100916                                     §21_virTIP + ' già presente';
063900100914                 endif;
064000100914              endif;
064100100914
064200100914      /END-FREE
064300100914      *
064400100915     c                   endif
064500100916     c                   endif
064600100915      *
064700100914     c                   ENDSR
064800100914     c*------------------------------------------------------------------------*
064900100915
065000100915
065100100915
065200100915     c*------------------------------------------------------------------------*
065300100915     c* insESYSP - INSERIMENTI X EASY SPED
065400100915     c*------------------------------------------------------------------------*
065500100916     c     INSESYSP      BEGSR
065600100916      *
065700100916     c                   if        not *in40
065800100915      *
065900100915      * Inserimento UTENTE
066000100915     c                   exsr      insVSS
066100100915      *
066200100915      * Se tutto ok procedo
066300100915     c                   if        §21_ERRFLG = *blanks
066400100915     c                   clear                   tivirds
066500100915      *
066600100915      * - "general"
066700100916     c                   eval      wvirKSC      = §21_vssKSU
066800100916     c                   eval      §21_virKSC   = §21_vssKSU
066900100916     c                   eval      §21_virSEQ   = 1
067000100916     c                   eval      §21_virDTI   = datcor
067100100916     c                   eval      §21_virDTF   = 20391231
067200100916     c                   eval      §21_virFI1   = 'IE'
067300100916      *
067400100916     c                   eval      pInCodCli      = §21_vssKSU
067500100916     c                   eval      pInTipTrasf    = 'IE'
067600100916     c                   eval      pInMaskSem     = '!file:.CHK'
067700101004     c                   eval      pInPercorso    = %trim(wPercorso)           +
067800101004     c                                              %subst(§21_vssKSU:2:7)     +
067900100916     c                                              '\IN'
068000100916     c                   eval      pInSoggetto    = %subst(§21_vssKSU:2:7)
068100101004     c                   eval      pInPercorsOld  = %trim(wPercorso)           +
068200101004     c                                              %subst(§21_vssKSU:2:7)     +
068300100916     c                                              '\OLDIN'
068400100916     c                   eval      pInTentativi   = 100
068500100916     c                   eval      pInRitardo     = 60
068600100916     c                   eval      pInLibAS4      = 'VASUPLOAD'
068700100916     c                   eval      pInFilAS4      = '*'
068800100916     c                   eval      pInMbrAS4      = 'M!ctr1:.'
068900100916     c                   eval      pInSchedul     = '1-31@05:30-23:50+00:10'
069000100916     c                   eval      pInAccorpam    = 0
069100100916     c                   eval      pInMaskZip     = 'FNVAB*.ZIP'
069200100916     c                   eval      pInDataStr     = %char(wData)
069300100916     c                   eval      pInDataEnd     = '2039-12-31'
069400100916     c                   eval      pInCodCompr    = 'ZIP'
069500100916     c                   eval      pInUsrIns      = JOB_USER
069600100916     c                   eval      pInNote        = 'EasySped da AS/400'
069700100915      *
069800100915      * - VB
069900100916     c                   eval      wvirTIP      = 'VB'
070000100916     c                   eval      §21_virTIP   = 'VB'
070100100916     c                   eval      §21_virDES   = 'TESTATA BOLLE'
070200100916     c                   eval      §21_virPOI   = %dec(%subst(wvirKSC:2:3):3:0)
070300100916     c                   eval      §21_virFI2   = 'G'
070400100916      *
070500100916     c                   eval      pInTipFile     = 'VB'
070600100916     c                   eval      pInMaskChk     = 'FNVAB*.DAT'
070700101013     c                   eval      pInProcesso    = wProcesso
070800100916      *
070900101013     c                   exsr      insVIR_VSS
071000100915      *
071100100915      * Se tutto ok procedo
071200100915     c                   if        §21_ERRFLG = *blanks
071300100915      *
071400100915      * - VT
071500100916     c                   eval      wvirTIP      = 'VT'
071600100916     c                   eval      §21_virTIP   = 'VT'
071700100916     c                   eval      §21_virDES   = 'ESTENSIONE DETTAGLIO BOLLE'
071800100916     c                   eval      §21_virPOI   = %dec(%subst(wvirKSC:2:3):3:0)
071900100916     c                   eval      §21_virFI2   = 'G'
072000100916      *
072100100916     c                   eval      pInTipFile     = 'VT'
072200100916     c                   eval      pInMaskChk     = 'FNVAT*.DAT'
072300101005     c                   eval      pInProcesso    = *zeros
072400100916      *
072500101013     c                   exsr      insVIR_VSS
072600100915      *
072700100915      * Se tutto ok procedo
072800100915     c                   if        §21_ERRFLG = *blanks
072900100915      *
073000100915      * - ES
073100100916     c                   eval      wvirTIP      = 'ES'
073200100916     c                   eval      §21_virTIP   = 'ES'
073300100916     c                   eval      §21_virDES   = 'EASY SPED - VERSIONI'
073400100916     c                   eval      §21_virPOI   = 046
073500100916     c                   eval      §21_virFI2   = *blanks
073600100916      *
073700100916     c                   eval      pInTipFile     = 'ES'
073800100916     c                   eval      pInMaskChk     = 'EasySped*.TXT'
073900101005     c                   eval      pInProcesso    = *zeros
074000100916      *
074100101013     c                   exsr      insVIR_VSS
074200100915      *
074300100915     c                   endif
074400100915     c                   endif
074500100915     c                   endif
074600100916     c                   endif
074700100915      *
074800100915     c                   ENDSR
074900100915     c*------------------------------------------------------------------------*
075000100913
075100100913
075200100913
075300100914     c*------------------------------------------------------------------------*
075400100914     c*  INIZIALIZZAZIONE PARAMETRO D OUTPUT
075500100914     c*------------------------------------------------------------------------*
075600101013     c     INZOUT        BEGSR
075700100914      *
075800100914     c                   clear                   §21_ERRFLG
075900100914     c                   clear                   §21_ERRMSG
076000100914      *
076100100914     c                   ENDSR
076200100914     c*------------------------------------------------------------------------*
076300101004
076400101004
076500101004
076600101004     c*------------------------------------------------------------------------*
076700101004     c*  VERIFICA PARAMETRI D INPUT
076800101004     c*------------------------------------------------------------------------*
076900101013     c     CHKPAR        BEGSR
077000101004      *
077100101004     c                   if        *in40 = *off
077200101004     c                   if        §21_FILE <> 'VSS' and
077300101004     c                             §21_FILE <> 'VIR' and
077400101004     c                             §21_FILE <> 'ESY'
077500101004     c                   seton                                        40
077600101004     c                   eval      §21_ERRFLG = 'E'
077700101004     c                   eval      §21_ERRMSG = 'Errore nei parametri di input'
077800101004     c                   endif
077900101004     c                   endif
078000101004      *
078100101004     c                   if        *in40 = *off
078200101004     c                   if        §21_OPERAZ <> 'INS' and
078300101004     c                             §21_OPERAZ <> *blanks
078400101004     c                   seton                                        40
078500101004     c                   eval      §21_ERRFLG = 'E'
078600101004     c                   eval      §21_ERRMSG = 'Errore nei parametri di input'
078700101004     c                   endif
078800101004     c                   endif
078900101004      *
079000101004     c                   if        *in40 = *off
079100101013     c                   if        §21_UTENTE = *blanks AND
079200101013     c                             §21_FILE   <> 'VIR'
079300101004     c                   seton                                        40
079400101004     c                   eval      §21_ERRFLG = 'E'
079500101004     c                   eval      §21_ERRMSG = 'Errore nei parametri di input'
079600101004     c                   endif
079700101004     c                   endif
079800101004      *
079900101004     c                   ENDSR
080000101004     c*------------------------------------------------------------------------*
080100100826
080200100826
080300100913
080400100914     c*------------------------------------------------------------------------*
080500100914     c* *INZSR - OPERAZIONI INIZIALI
080600100914     c*------------------------------------------------------------------------*
080700100914     c     *INZSR        BEGSR
080800100914      *
080900100914     c     *Entry        plist
081000100914     c                   parm                    ds_typ            1
081100100914     c                   parm                    ds_param       1024
081200100914     c                   parm                    tivssds
081300100914     c                   parm                    tivirds
081400100914      *
081500100914      * Chiave su file TIVIR02L - Parziale
081600100914     c     KEY_vir02_P   KLIST
081700100915     c                   KFLD                    wvirKSC
081800100915     c                   KFLD                    wvirTIP
081900100827      *
082000100827      * Calcola la data corrente
082100100914     c                   z-add     *zeros        datcor            8 0
082200100914     c                   eval      datcor   = %dec(%date() : *ISO)
082300100916     c                   eval      wData    = %date(datcor : *ISO)
082400100914      *
082500100914     c                   ENDSR

000100000000     **-- Header specifications:  --------------------------------------------**
000200090520     H OPTION(*SRCSTMT) DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('QC2LE')
000300090520
000400090520     FPRTF198   O    F  198        PRINTER USROPN
000500090520
000600000000     **-- Api error data structure:  -----------------------------------------**
000700090520     D/COPY QSYSINC/QRPGLESRC,QUSEC
000800090520     D/COPY QSYSINC/QRPGLESRC,QUSRJOBI
000900090521     D/COPY QSYSINC/QRPGLESRC,QLG
001000090521     D/COPY GAITRASRC/SRCPROTOPR,QLGCNVCS
001100090520
001200090520
001300090520     **-- Run system command:  -----------------------------------------------**
001400090520     D executeCommand...
001500090520     D                 PR            10I 0 EXTPROC('system')
001600090520     D  command                        *   VALUE OPTIONS(*STRING)
001700090520
001800090520     D exceptionMessageID...
001900090520     D                 S              7A   IMPORT('_EXCP_MSGID')
002000090520
002100090520
002200090520     **-- Job lock information:  ---------------------------------------------**
002300090520     D JBLK0200Hdr     Ds         65535    Qualified
002400090520     D  JlBytRtn                     10i 0
002500090520     D  JlBytAvl                     10i 0
002600090520     D  JlNbrObjLck                  10i 0
002700090520     D  JlOfsObjLck                  10i 0
002800090520     D  JlNbrLckObjRt                10i 0
002900090520     D  JlLckObjEntLe                10i 0
003000090520     **
003100090520     **
003200090520     D JBLK0200Inf     Ds                  Qualified  Based( pLck200Inf )
003300090520     D  LiTypEntty                   10i 0
003400090520     D  LiExdObjNam                  30a
003500090520     D  LiObjLib                     10a
003600090520     D  LiiObjAspNam                 10a
003700090520     D  LiLibAspNam                  10a
003800090520     D  LiObjAspNum                  10i 0
003900090520     D  LiLibAspNum                  10i 0
004000090520     D  LiObjTyp                     10a
004100090520     D  LiExdObjAttr                 10a
004200090520     D  LiMbrNam                     10a
004300090520     D  LiMbrLckTyp                   1a
004400090520     D                                3a
004500090520     D  LiLckStt                     10a
004600090520     D  LiLckSts                     10i 0
004700090520     D  LiMbrLcks                    10i 0
004800090520     D  LiLckCnt                     10i 0
004900090520     D  LiLckScp                      1a
005000090520     D                                3a
005100090520     D  LiSpcLckOffs                 10u 0
005200090520     D  LiThrId                       8a
005300090520     D  LiThrHdl                     10u 0
005400090520     D  LiLckSpcId                   20a
005500090520     D  LiObjLckHdl                  64a
005600090520     D  LiLckRqstHdl                 64a
005700090520     **
005800090520
005900090520
006000090520     **-- Job id:  -----------------------------------------------**
006100090520     D JlJobId         Ds                  Qualified
006200090520     D  JiJobNam                     10a   Inz( '*' )
006300090520     D  JiUsrNam                     10a
006400090520     D  JiJobNbr                      6a
006500090520     D  JiIntJobId                   16a   Inz( *Blanks )
006600090520     D                                2a   Inz( *Allx'00' )
006700090520     D  JlThrInd                     10i 0 Inz( 3 )
006800090520     D  JlThrId                       8a
006900090520     **
007000090520
007100090520
007200090520     **-- Retrieve job record locks API: -------------------------**
007300090520     D RtvJobLck       Pr                  ExtPgm( 'QWCRJBLK' )
007400090520     D  JlRcvVar                  65535a         Options( *VarSize )
007500090520     D  JlRcvVarLen                  10i 0 Const
007600090520     D  JlFmtNam                      8a   Const
007700090520     D  JlJobId                      56a   Const
007800090520     D  JlFmtJobId                    8a   Const
007900090520     D  JlError                   32767a         Options( *VarSize )
008000090520     ** (Optional)
008100090520     D  JlLckFlr                     53a   Const  Options( *NoPass )
008200090520     D  JlFlrFmt                      8a   Const  Options( *NoPass )
008300090520
008400090520
008500090520     **-- Job informations: --------------------------------------------------**
008600090520     D JOBI0100        Ds                  Qualified
008700090520     D  JiBytRtn                     10i 0
008800090520     D  JiBytAvl                     10i 0
008900090520     D  JiJobNam                     10a
009000090520     D  JiUsrNam                     10a
009100090520     D  JiJobNbr                      6a
009200090520     D  JiJobIntId                   16a
009300090520     D  JiJobSts                     10a
009400090520     D  JiJobTyp                      1a
009500090520     D  JiJobSubTyp                   1a
009600090520     **
009700090520
009800090520
009900090520     **-- Retrieve job information:
010000090520     D RtvJobInf       Pr                  ExtPgm( 'QUSRJOBI' )
010100090520     D  RjiRcvVar                 32767a         Options( *VarSize )
010200090520     D  RjiRcvVarLen                 10i 0 Const
010300090520     D  RjiFmtNam                     8a   Const
010400090520     D  RjiJobNamQ                   26a   Const
010500090520     D  RjiJobIntId                  16a   Const
010600090520     D  RjiError                  32767a         Options( *NoPass: *VarSize )
010700090520     **
010800090520
010900090520
011000090520     **-- Global variables:  -------------------------------------**
011100090520     D Idx             S              5i 0
011200090521     D IdxList         S              5i 0
011300090520     D InJobName       S             10a
011400090520     D InJobUsrName    S             10a
011500090520     D InJobNumber     S              6a
011600090520     D InOption        S             10a
011700090520     D InFilterLib     S             10a
011800090520     D InFilterFil     S             10a
011900090521     D InFilterMbr     S             10a
012000090520     D InFilterTyp     S             10a
012100090521     D InFilterLck     S             10a
012200090521     D OutListAlloc    s                   Dim(256) like(UBJOBLCKDS)
012300090520     D OutErrorCod     S             10i 0
012400090520     D OutErrorMsg     S             80a
012500090521     D RigaOutPrt      S            198a   Inz varying
012600090521     D wFilterLib      S                   Like(InFilterLib)
012700090521     D wFilterFil      S                   Like(InFilterFil)
012800090521     D wFilterMbr      S                   Like(InFilterMbr)
012900090521     D wFilterTyp      S                   Like(InFilterTyp)
013000090521     D wFilterLck      S                   Like(InFilterLck)
013100090520     D wObjTypAndAttr  S             20a
013200090521     D ubjoblckds    E DS                  Qualified
013300090520     **
013400090520     D DSIDJob         DS
013500090520     D  JobName                      10a
013600090520     D  JobUsrName                   10a
013700090520     D  JobNumber                     6a
013800090520     **
013900090520
014000090520
014100090520
014200090520     !*-- Mainline:  ---------------------------------------------**
014300090520     C*
014400090520     C     *Entry        Plist
014500090520     C                   Parm                    InJobName
014600090520     C                   Parm                    InJobUsrName
014700090520     C                   Parm                    InJobNumber
014800090520     C                   Parm                    InOption
014900090521     C                   Parm                    InFilterLib
015000090520     C                   Parm                    InFilterFil
015100090521     C                   Parm                    InFilterMbr
015200090521     C                   Parm                    InFilterTyp
015300090521     C                   Parm                    InFilterLck
015400090521     C                   Parm                    OutListAlloc
015500090520     C                   Parm                    OutErrorCod
015600090520     C                   Parm                    OutErrorMsg
015700090520     C*
015800090521     C* Inizializzo, verifico e "normalizzo" i parametri d input
015900090521     C                   Exsr      OpeInz
016000090520     C*
016100090520     C* Se richiesto (ovvero parametri in ingresso valorizzato) elaboro un job specifico
016200090520     C                   If        InJobName    <> *blanks AND
016300090520     C                             InJobUsrName <> *blanks AND
016400090520     C                             InJobNumber  <> *blanks
016500090521     C                   Exsr      RtvInfoJob
016600090520     C                   Endif
016700090520     C*
016800090520     C* Verifico correttezza formale richiesta elaborazione corrente rispetto all'opzione richiesta
016900090520     C                   If        JlJobId.JiJobNam  = '*INT'   AND
017000090520     C                             %trim(InOption)  <> '*PRINT'
017100090520     C                   Eval      OutErrorCod = -1
017200090520     C                   Eval      OutErrorMsg = 'Opzione ' + %trim(InOption) +
017300090520     C                                           ' non consentita per job'   +
017400090520     C                                           ' diverso da quello corrente.'
017500090520     C                   Else
017600090520     C*
017700090520     C* Se necessario apro il printer-file
017800090520     C                   If        %trim(InOption) = '*PRINT'
017900090520     C                   If        not %open(PRTF198)
018000090521     C                   Open      PRTF198
018100090520     C                   Endif
018200090520     C                   Endif
018300090520     C*
018400090520     C* A questo punto quindi reperisco le allocazioni sul job (o quello corrente o quello richiesto
018500090520     C                   Clear                   QUSEC
018600090520     C*
018700090520     C                   CallP     RtvJobLck( JBLK0200Hdr
018800090520     C                                      : %Size( JBLK0200Hdr )
018900090520     C                                      : 'JBLK0200'
019000090520     C                                      : JlJobId
019100090520     C                                      : 'JIDF0100'
019200090520     C                                      : QUSEC
019300090520     C                                      )
019400090520     C*
019500090520     C* Se no errori procedo...
019600090520     C                   If        QUSBAVL = *Zero
019700090520     C                   Exsr      PrcJobLck
019800090520     C                   Endif
019900090520     C*
020000090520     C                   Endif
020100090520     C*
020200090520     C* Se necessario chiudo il printer-file
020300090520     C                   If        %open(PRTF198)
020400090521     C                   Close     PRTF198
020500090520     C                   Endif
020600090520     C*
020700090520     C* Fine elaborazione
020800090520     C                   Return
020900090520     C*
021000090520     !*-----------------------------------------------------------**
021100090520
021200090520
021300090520
021400090520     !*-- Process job locks:  ------------------------------------**
021500090520     C     PrcJobLck     Begsr
021600090520     C*
021700090520     C                   Eval      pLck200Inf  = %Addr( JBLK0200Hdr ) +
021800090520     C                                           JBLK0200Hdr.JlOfsObjLck
021900090520     C*
022000090520     C* Inizializzo a OK lo status dell'elaborazione
022100090520     C                   Eval      OutErrorCod = 0
022200090520     C*
022300090521     C* Scorro le entry della lista ritornata dall'API
022400090520     C                   For       Idx         = 1 to JBLK0200Hdr.JlNbrLckObjRt
022500090521     C*
022600090521     C* Inizializzo a OK il flag d entry corrente valida
022700090521     C                   Seton                                        55
022800090520     C*
022900090521     C* Se richiesti applico filtri alla lista
023000090521     C                   Exsr      ApplyFilters
023100090521     C*
023200090520     C* Proseguo con esecuzione opzione richiesta solo se trattasi d entry valida
023300090520     C                   If        *in55
023400090521     C*
023500090521     C* *LIST    - Se richiesta lista a *pgm
023600090521     C                   If        %trim(InOption) = '*LIST'
023700090521     C                   Clear                   ubjoblckds
023800090521     C                   Eval      ubjoblckds.克LCKOLIB=JBLK0200Inf.LiObjLib
023900090521     C                   Eval      ubjoblckds.克LCKONAM=JBLK0200Inf.LiExdObjNam
024000090521     C                   Eval      ubjoblckds.克LCKOMBR=JBLK0200Inf.LiMbrNam
024100090521     C                   Eval      ubjoblckds.克LCKOTYP=wObjTypAndAttr
024200090521     C                   Eval      ubjoblckds.克LCKALLC=JBLK0200Inf.LiLckStt
024300090521     C                   Add       1             IdxList
024400090521     C                   Eval      OutListAlloc(IdxList) = ubjoblckds
024500090521     C                   Endif
024600090520     C*
024700090520     C* *PRINT   - Se richiesta stampa
024800090520     C                   If        %trim(InOption) = '*PRINT'
024900090520     C                   Eval      RigaOutPrt =
025000090520     C                                JBLK0200Inf.LiObjLib               + ' ' +
025100090520     C                                JBLK0200Inf.LiExdObjNam            + ' ' +
025200090520     C                                JBLK0200Inf.LiMbrNam               + ' ' +
025300090520     C                                wObjTypAndAttr                     + ' ' +
025400090520     C                                JBLK0200Inf.LiLckStt
025500090520     C                   Except    RigaOut
025600090520     C                   Endif
025700090520     C*
025800090520     C* *DEALLOC - Se richiesta dealocazione
025900090520     C                   If        %trim(InOption) = '*DEALLOC'
026000090521     C***                If        %subst(JBLK0200Inf.LiObjTyp:1:5) = '*FILE'
026100090520     C                   If        executeCommand('DLCOBJ OBJ(('+
026200090520     C                              %trim(JBLK0200Inf.LiObjLib)     +
026300090520     C                              '/'                             +
026400090520     C                              %trim(JBLK0200Inf.LiExdObjNam)  +
026500090520     C                              ' *FILE *EXCL '                 +
026600090520     C                              %trim(JBLK0200Inf.LiMbrNam)     +
026700090520     C                              '))') <> *zeros
026800090520     C                   Eval      OutErrorCod = -1
026900090520     C                   Eval      OutErrorMsg = exceptionMessageID
027000090520     C                   Endif
027100090521     C***                Endif
027200090520     C                   Endif
027300090520     C*
027400090520     C                   Endif
027500090520     C*
027600090520     C* proseguo scorrendo le entry successive
027700090520     C                   If        Idx         < JBLK0200Hdr.JlNbrLckObjRt
027800090520     C                   Eval      pLck200Inf  = pLck200Inf +
027900090520     C                                           JBLK0200Hdr.JlLckObjEntLe
028000090520     C                   Endif
028100090520     C*
028200090520     C                   Endfor
028300090520     C*
028400090520     C                   Endsr
028500090520     !*-----------------------------------------------------------**
028600090521
028700090521
028800090521
028900090521     !*-- Retrieve job information:  -----------------------------**
029000090521     C     RtvInfoJob    Begsr
029100090521     C*
029200090521     C                   Eval      JobName    = InJobName
029300090521     C                   Eval      JobUsrName = InJobUsrName
029400090521     C                   Eval      JobNumber  = InJobNumber
029500090521     C*
029600090521     C                   Clear                   QUSEC
029700090521     C*
029800090521     C* Chiamo l'API x reperimento informazoni sul job richiesto
029900090521     C                   CallP     RtvJobInf( JOBI0100
030000090521     C                                      : %Size( JOBI0100 )
030100090521     C                                      : 'JOBI0100'
030200090521     C                                      : DSIDJob
030300090521     C                                      : *blanks
030400090521     C                                      : QUSEC
030500090521     C                                      )
030600090521     C*
030700090521     C* Se no errori procedo...
030800090521     C                   If        QUSBAVL = *Zero
030900090521     C                   Eval      JlJobId.JiJobNam   = '*INT'
031000090521     C                   Eval      JlJobId.JiUsrNam   = *blanks
031100090521     C                   Eval      JlJobId.JiJobNbr   = *blanks
031200090521     C                   Eval      JlJobId.JiIntJobId = JOBI0100.JiJobIntId
031300090521     C                   Endif
031400090521     C*
031500090521     C                   Endsr
031600090521     !*-----------------------------------------------------------**
031700090521
031800090521
031900090521
032000090521     !*-- Operazioni iniziali:  ----------------------------------**
032100090521     C     OpeInz        Begsr
032200090521     C*
032300090521     C                   Z-add     *zeros        Idxlist
032400090521     C*
032500090521     C                   Clear                   OutListAlloc
032600090521     C                   Clear                   OutErrorCod
032700090521     C                   Clear                   OutErrorMsg
032800090521     C*
032900090521     C                   Eval      InJobName    = %trim(InJobName)
033000090521     C                   Eval      InJobUsrName = %trim(InJobUsrName)
033100090521     C                   Eval      InJobNumber  = %trim(InJobNumber)
033200090521     C                   Eval      InOption     = %trim(InOption)
033300090521     C                   Eval      InFilterLib  = %trim(InFilterLib)
033400090521     C                   Eval      InFilterFil  = %trim(InFilterFil)
033500090521     C                   Eval      InFilterLck  = %trim(InFilterLck)
033600090521     C                   Eval      InFilterTyp  = %trim(InFilterTyp)
033700090521     C*
033800090521     C                   Eval      wUCase = InJobName
033900090521     C                   Exsr      UCase
034000090521     C                   Eval      InJobName = wUCase
034100090521     C*
034200090521     C                   Eval      wUCase = InJobUsrName
034300090521     C                   Exsr      UCase
034400090521     C                   Eval      InJobUsrName = wUCase
034500090521     C*
034600090521     C                   Eval      wUCase = InOption
034700090521     C                   Exsr      UCase
034800090521     C                   Eval      InOption = wUCase
034900090521     C*
035000090521     C                   Eval      wUCase = InFilterLib
035100090521     C                   Exsr      UCase
035200090521     C                   Eval      InFilterLib = wUCase
035300090521     C*
035400090521     C                   Eval      wUCase = InFilterFil
035500090521     C                   Exsr      UCase
035600090521     C                   Eval      InFilterFil = wUCase
035700090521     C*
035800090521     C                   Eval      wUCase = InFilterMbr
035900090521     C                   Exsr      UCase
036000090521     C                   Eval      InFilterMbr = wUCase
036100090521     C*
036200090521     C                   Eval      wUCase = InFilterTyp
036300090521     C                   Exsr      UCase
036400090521     C                   Eval      InFilterTyp = wUCase
036500090521     C*
036600090521     C                   Eval      wUCase = InFilterLck
036700090521     C                   Exsr      UCase
036800090521     C                   Eval      InFilterLck = wUCase
036900090521     C*
037000090521     C                   If        InOption = *blanks
037100090521     C                   Eval      InOption = '*LIST'
037200090521     C                   Endif
037300090521     C*
037400090521     C                   Endsr
037500090521     !*-----------------------------------------------------------**
037600090521
037700090521
037800090521
037900090521     !*-- Applicazione filtri richiesti:  ------------------------**
038000090521     C     ApplyFilters  Begsr
038100090521     C*
038200090521     C* Compongo i campi "combinati"
038300090521     C                   Eval      wObjTypAndAttr =
038400090521     C                                          %trim(JBLK0200Inf.LiObjTyp)+'-'+
038500090521     C                                          %trim(JBLK0200Inf.LiExdObjAttr)
038600090521     C*
038700090521     C                   If        InFilterLib <> *blanks
038800090521     C                   If        %subst(%trim(InFilterLib):
038900090521     C                                    %len(%trim(InFilterLib)):1) = '*'
039000090521     C                   Eval      wFilterLib = %subst(%trim(InFilterLib):1:
039100090521     C                                          %len(%trim(InFilterLib))-1)
039200090521     C                   If        %scan(%trim(wFilterLib):
039300090521     C                             JBLK0200Inf.LiObjLib:1) = *zeros
039400090521     C                   Setoff                                       55
039500090521     C                   Endif
039600090521     C                   Else
039700090521     C                   If        InFilterLib <> JBLK0200Inf.LiObjLib
039800090521     C                   Setoff                                       55
039900090521     C                   Endif
040000090521     C                   Endif
040100090521     C                   Endif
040200090521     C*
040300090521     C                   If        InFilterFil <> *blanks
040400090521     C                   If        %subst(%trim(InFilterFil):
040500090521     C                                    %len(%trim(InFilterFil)):1) = '*'
040600090521     C                   Eval      wFilterFil = %subst(%trim(InFilterFil):1:
040700090521     C                                          %len(%trim(InFilterFil))-1)
040800090521     C                   If        %scan(%trim(wFilterFil):
040900090521     C                             JBLK0200Inf.LiExdObjNam:1) = *zeros
041000090521     C                   Setoff                                       55
041100090521     C                   Endif
041200090521     C                   Else
041300090521     C                   If        InFilterFil <> JBLK0200Inf.LiExdObjNam
041400090521     C                   Setoff                                       55
041500090521     C                   Endif
041600090521     C                   Endif
041700090521     C                   Endif
041800090521     C*
041900090521     C                   If        InFilterMbr <> *blanks
042000090521     C                   If        %subst(%trim(InFilterMbr):
042100090521     C                                    %len(%trim(InFilterMbr)):1) = '*'
042200090521     C                   Eval      wFilterMbr = %subst(%trim(InFilterMbr):1:
042300090521     C                                          %len(%trim(InFilterMbr))-1)
042400090521     C                   If        %scan(%trim(wFilterMbr):
042500090521     C                             JBLK0200Inf.LiMbrNam:1) = *zeros
042600090521     C                   Setoff                                       55
042700090521     C                   Endif
042800090521     C                   Else
042900090521     C                   If        InFilterMbr <> JBLK0200Inf.LiMbrNam
043000090521     C                   Setoff                                       55
043100090521     C                   Endif
043200090521     C                   Endif
043300090521     C                   Endif
043400090521     C*
043500090521     C                   If        InFilterTyp <> *blanks
043600090521     C                   If        %subst(%trim(InFilterTyp):
043700090521     C                                    %len(%trim(InFilterTyp)):1) = '*'
043800090521     C                   Eval      wFilterTyp = %subst(%trim(InFilterTyp):1:
043900090521     C                                          %len(%trim(InFilterTyp))-1)
044000090521     C                   If        %scan(%trim(wFilterTyp):
044100090521     C                             wObjTypAndAttr:1) = *zeros
044200090521     C                   Setoff                                       55
044300090521     C                   Endif
044400090521     C                   Else
044500090521     C                   If        InFilterTyp <> wObjTypAndAttr
044600090521     C                   Setoff                                       55
044700090521     C                   Endif
044800090521     C                   Endif
044900090521     C                   Endif
045000090521     C*
045100090521     C                   If        InFilterLck <> *blanks
045200090521     C                   If        %subst(%trim(InFilterLck):
045300090521     C                                    %len(%trim(InFilterLck)):1) = '*'
045400090521     C                   Eval      wFilterLck = %subst(%trim(InFilterLck):1:
045500090521     C                                          %len(%trim(InFilterLck))-1)
045600090521     C                   If        %scan(%trim(wFilterLck):
045700090521     C                             JBLK0200Inf.LiLckStt:1) = *zeros
045800090521     C                   Setoff                                       55
045900090521     C                   Endif
046000090521     C                   Else
046100090521     C                   If        InFilterLck <> JBLK0200Inf.LiLckStt
046200090521     C                   Setoff                                       55
046300090521     C                   Endif
046400090521     C                   Endif
046500090521     C                   Endif
046600090521     C*
046700090521     C                   Endsr
046800090521     !*-----------------------------------------------------------**
046900090521
047000090521
047100090521
047200090521     !*-- Conversione stringa da maiuscolo a minuscolo:  ---------**
047300090521     C     UCase         Begsr
047400090521     C*
047500090521     C                   Movel     wUCase        wUcase          256
047600090521     C*
047700090521     C                   Clear                   QUSEC
047800090521     C                   Eval      QUSBPRV = %size(QUSEC)
047900090521     C                   Clear                   QLGIDRCB00
048000090521     C                   Eval      QLGTOR02 = 1
048100090521     C                   Eval      QLGIDOID00 = 1144
048200090521     C                   Eval      QLGCR00 = 0
048300090521     C                   Eval      QLGERVED04 = *ALLX'00'
048400090521     C
048500090521     C                   CallP     ConvertCase( QLGIDRCB00
048600090521     C                                        : wUCase
048700090521     C                                        : wUCase
048800090521     C                                        : %LEN(wUCase)
048900090521     C                                        : QUSEC
049000090521     C                                        )
049100090521     C*
049200090521     C                   Endsr
049300090521     !*-----------------------------------------------------------**
049400090520
049500090521
049600090520
049700090520     C*------------------------------------------------------------------------*
049800090520     OPRTF198   E            RigaOut     1
049900090520     O                       RigaOutPrt

000100000000     **
000200081021     **  Program . . : UBIFSLCK
000300000000     **  Description : Display IFS object locks
000400000000     **
000500000000     **
000600000000     **  Program summary
000700000000     **  ---------------
000800000000     **
000900000000     **  Unix type APIs:
001000000000     **    QP0LROR       Retrieve object       For specific IFS objects access
001100000000     **                  references            or lock information is retrieved.
001200000000     **
001300000000     **                                        This information includes the
001400000000     **                                        type of lock or access as well
001500000000     **                                        as a list of the jobs holding the
001600000000     **                                        lock(s).
001700000000     **
001800000000     **                                        An IFS object can, however, be
001900000000     **                                        flagged as "in use" without a
002000000000     **                                        specific job being identified
002100000000     **                                        as currently holding a lock.
002200000000     **
002300000000     **                                        Likewise, the browsing of an IFS
002400000000     **                                        stream file does not necessarily
002500000000     **                                        generate a lock or set the object
002600000000     **                                        in use indicator.
002700000000     **
002800000000     **    QlgLstat      Get file or link      Gets status information about the
002900000000     **                  information           specified directory entry and puts
003000000000     **                                        it in the structure pointed to by
003100000000     **                                        the pBuf parameter.
003200000000     **
003300000000     **                                        The path name parameter includes
003400000000     **                                        NLS attributes (National Language
003500000000     **                                        Support) enabling the API to take
003600000000     **                                        these into account when resolving
003700000000     **                                        the actual IFS object.
003800000000     **
003900000000     **  Spooled file API:
004000000000     **    QSPRILSP      Retrieve identity of  Returns the subset of spooled file
004100000000     **                  last spooled file     attributes that uniquely identifies
004200000000     **                  created               the last spooled file created in
004300000000     **                                        the current job.
004400000000     **
004500000000     **  Work management APIs:
004600000000     **    QUSRJOBI      Retrieve job          Retrieves a variety of specific
004700000000     **                  information           information about a job.
004800000000     **
004900000000     **                                        The information is grouped in the
005000000000     **                                        various formats available.
005100000000     **
005200000000     **  Message handling API:
005300000000     **    QMHSNDPM      Send program message  Sends a message to a program stack
005400000000     **                                        entry (current, previous, etc.) or
005500000000     **                                        an external message queue.
005600000000     **
005700000000     **                                        Both messages defined in a message
005800000000     **                                        file and immediate messages can be
005900000000     **                                        used. For specific message types
006000000000     **                                        only one or the other is allowed.
006100000000     **
006200000000     **  C library function:
006300000000     **    system        Run system command    Executes a system command. In the
006400000000     **                                        event of an resulting error the
006500000000     **                                        error message ID is exported in
006600000000     **                                        the _EXCP_MSGID variable.
006700000000     **
006800000000     **  Sequence of events:
006900000000     **    1. The existence of the specified IFS object is verified using
007000000000     **       the lstat unix function and if an error is returned during
007100000000     **       this process, an escape message is sent back to the caller.
007200000000     **
007300000000     **    2. Storage is allocated for the Retrieve object reference API return
007400000000     **       variable and the API is called. If there's more object reference
007500000000     **       information available than allocated, sufficient storage is
007600000000     **       reallocated and the API is called again.
007700000000     **
007800000000     **    3. The retrieved information is formatted and written to the printer
007900000000     **       file. The printer file is closed and the allocated storage is
008000000000     **       released.
008100000000     **
008200000000     **    4. If the command is running in batch or a printed list was requested,
008300000000     **       a completion message is sent to inform the caller that list is now
008400000000     **       available - otherwise the generated spooled file is displayed, and
008500000000     **       subsequently deleted.
008600000000     **
008700000000     **
008800000000     **  Programmer's notes:
008900000000     **    Both the QP0LROR (Retrieve object references) and QSPRILSP (Retrieve
009000000000     **    identity of last spooled file created) were introduced with V5R2 and
009100000000     **    this API example will therefore not be available to earlier releases.
009200000000     **
009300000000     **    QP0LROR documentation and comprehensive usage notes can be found here:
009400000000     **    http://as400bks.rochester.ibm.com/iseries/v5r2/ic2924/info/apis/qp0lror.htm
009500000000     **
009600000000     **    The QP0LROR return format RORO0100 is not used in this utility, but
009700000000     **    a sample of how to use it and retrieve its information is included
009800000000     **    in the non-referenced subroutine RtvObjRef1 and PrcObjRef1.
009900000000     **
010000000000     **
010100000000     **  Compile options:
010200000000     **
010300081021     **    CrtRpgMod Module(UBIFSLCK)  DbgView(*LIST)
010400000000     **
010500081021     **    CrtPgm    Pgm(UBIFSLCK)
010600081021     **              Module(UBIFSLCK)
010700000000     **
010800000000     **
010900000000     **-- Control specification:  --------------------------------------------**
011000081021     H Option(*SrcStmt)  BndDir('QC2LE')
011100000000     **-- Printer file:
011200000000     FQSYSPRT   O    F  132        Printer  InfDs( PrtLinInf )  OflInd( *InOf )
011300000000     F                                      UsrOpn
011400000000
011500000000     **-- Printer file information:
011600000000     D PrtLinInf       Ds                  Qualified
011700000000     D  OvfLin                        5i 0  Overlay( PrtLinInf: 188 )
011800000000     D  CurLin                        5i 0  Overlay( PrtLinInf: 367 )
011900000000     D  CurPag                        5i 0  Overlay( PrtLinInf: 369 )
012000000000     **-- System information:
012100000000     D                SDs
012200000000     D  PsPgmNam         *Proc
012300000000     **-- Global declarations:
012400000000     D Time            s              6s 0
012500000000     D Idx             s             10u 0
012600000000     D BytAlc          s             10u 0
012700000000     D NbrRcds         s             10u 0
012800000000     D MsgKey          s              4a
012900000000     D ErrTxt          s            256a   Varying
013000000000     **
013100000000     D OutHdr          Ds                  Inz
013200000000     D  IfsObj                      112a
013300000000     D  ObjUse                        4a
013400000000     D  ChkUsr                       10a
013500000000     **
013600000000     D OutDtl          Ds                  Inz
013700000000     D  SavLck                       10u 0
013800000000     D  AtrLck                       10u 0
013900000000     D  ShrNoRdWrt                   10u 0
014000000000     D  ShrRdWrt                     10u 0
014100000000     D  ShrWrtOnly                   10u 0
014200000000     D  ShrRdOnly                    10u 0
014300000000     D  Execute                      10u 0
014400000000     D  ReadWrt                      10u 0
014500000000     D  WrtOnly                      10u 0
014600000000     D  ReadOnly                     10u 0
014700000000     D  JobNbr                        6a
014800000000     D  JobUsr                       10a
014900000000     D  JobNam                       10a
015000000000     **
015100000000     D CUR_CCSID       c                   0
015200000000     D CUR_CTRID       c                   x'0000'
015300000000     D CUR_LNGID       c                   x'000000'
015400000000     D CHR_DLM1        c                   0
015500000000
015600000000     **-- API error data structure:
015700000000     D ERRC0100        Ds                  Qualified
015800000000     D  BytPrv                       10i 0 Inz( %Size( ERRC0100 ))
015900000000     D  BytAvl                       10i 0 Inz
016000000000     D  MsgId                         7a
016100000000     D                                1a
016200000000     D  MsgDta                      128a
016300000000     **-- system function error ID:
016400000000     D SysError        s              7a   Import( '_EXCP_MSGID' )
016500000000
016600000000     **-- API path:
016700000000     D ApiPath         Ds                  Qualified
016800000000     D  CcsId                        10i 0 Inz( CUR_CCSID )
016900000000     D  CtrId                         2a   Inz( CUR_CTRID )
017000000000     D  LngId                         3a   Inz( CUR_LNGID )
017100000000     D                                3a   Inz( *Allx'00' )
017200000000     D  PthTypI                      10i 0 Inz( CHR_DLM1 )
017300000000     D  PthNamLen                    10i 0
017400000000     D  PthNamDlm                     2a   Inz( '/ ' )
017500000000     D                               10a   Inz( *Allx'00' )
017600000000     D  PthNam                     1024a
017700000000     **-- Object reference information:
017800000000     D RORO0100        Ds                  Qualified  Based( pObjRef )
017900000000     D  BytRtn                       10u 0
018000000000     D  BytAvl                       10u 0
018100000000     D  OfsSmpRef                    10u 0
018200000000     D  LenSmpRef                    10u 0
018300000000     D  RefCnt                       10u 0
018400000000     D  InUseI                       10u 0
018500000000     **
018600000000     D RORO0200        Ds                  Qualified  Based( pObjRef )
018700000000     D  BytRtn                       10u 0
018800000000     D  BytAvl                       10u 0
018900000000     D  RefCnt                       10u 0
019000000000     D  InUseI                       10u 0
019100000000     D  OfsSmpRef                    10u 0
019200000000     D  LenSmpRef                    10u 0
019300000000     D  OfsExtRef                    10u 0
019400000000     D  LenExtRef                    10u 0
019500000000     D  OfsJobLst                    10u 0
019600000000     D  NbrJobRtn                    10u 0
019700000000     D  NbrJobAvl                    10u 0
019800000000     **-- Job using object structure:
019900000000     D JobUsgObj       Ds                   Qualified  Based( pJobUsgObj )
020000000000     D  DplSmpRef                    10u 0
020100000000     D  LenSmpRef                    10u 0
020200000000     D  DplExtRef                    10u 0
020300000000     D  LenExtRef                    10u 0
020400000000     D  DplNxtJobE                   10u 0
020500000000     D  JobNam                       10a
020600000000     D  JobUsr                       10a
020700000000     D  JobNbr                        6a
020800000000     **-- Simple object reference types structure:
020900000000     D SmpObjRef       Ds                   Qualified  Based( pSmpObjRef )
021000000000     D  ReadOnly                     10u 0
021100000000     D  WrtOnly                      10u 0
021200000000     D  ReadWrt                      10u 0
021300000000     D  Execute                      10u 0
021400000000     D  ShrRdOnly                    10u 0
021500000000     D  ShrWrtOnly                   10u 0
021600000000     D  ShrRdWrt                     10u 0
021700000000     D  ShrNoRdWrt                   10u 0
021800000000     D  AtrLck                       10u 0
021900000000     D  SavLck                       10u 0
022000000000     D  SavLckInt                    10u 0
022100000000     D  LnkChgLck                    10u 0
022200000000     D  ChkOut                       10u 0
022300000000     D  ChkOutUsrNm                  10a
022400000000     D                                2a
022500000000     **-- Extended object reference types structure:
022600000000     D ExtObjRef       Ds                   Qualified  Based( pExtObjRef )
022700000000     D  RdOnShrRdOn                  10u 0
022800000000     D  RdOnShrWtOn                  10u 0
022900000000     D  RdOnShrRdWt                  10u 0
023000000000     D  RdOnShrNoRW                  10u 0
023100000000     D  WtOnShrRdOn                  10u 0
023200000000     D  WtOnShrWtOn                  10u 0
023300000000     D  WtOnShrRdWt                  10u 0
023400000000     D  WtOnShrNoRW                  10u 0
023500000000     D  RWonShrRdOn                  10u 0
023600000000     D  RWonShrWtOn                  10u 0
023700000000     D  RWonShrRdWt                  10u 0
023800000000     D  RWonShrNoRW                  10u 0
023900000000     D  ExOnShrRdOn                  10u 0
024000000000     D  ExOnShrWtOn                  10u 0
024100000000     D  ExOnShrRdWt                  10u 0
024200000000     D  ExOnShrNoRW                  10u 0
024300000000     D  XRonShrRdOn                  10u 0
024400000000     D  XRonShrWtOn                  10u 0
024500000000     D  XRonShrRdWt                  10u 0
024600000000     D  XRonShrNoRW                  10u 0
024700000000     D  AtrLck                       10u 0
024800000000     D  SavLck                       10u 0
024900000000     D  SavLckInt                    10u 0
025000000000     D  LnkChgLck                    10u 0
025100000000     D  CurDir                       10u 0
025200000000     D  RootDir                      10u 0
025300000000     D  FilSvrRef                    10u 0
025400000000     D  FilSvrWrkDi                  10u 0
025500000000     D  ChkOut                       10u 0
025600000000     D  ChkOutUsrNm                  10a
025700000000     D                                2a
025800000000     **-- Spooled file information:
025900000000     D SPRL0100        Ds                  Qualified
026000000000     D  BytRtn                       10i 0
026100000000     D  BytAvl                       10i 0
026200000000     D  SplfNam                      10a
026300000000     D  JobNam                       10a
026400000000     D  UsrNam                       10a
026500000000     D  JobNbr                        6a
026600000000     D  SplfNbr                      10i 0
026700000000     D  JobSysNam                     8a
026800000000     D  SplfCrtDat                    7a
026900000000     D                                1a
027000000000     D  SplfCrtTim                    6a
027100000000     **-- File stat-structure:
027200000000     D Buf             Ds                  Align
027300000000     D  st_mode                      10u 0
027400000000     D  st_ino                       10u 0
027500000000     D  st_nlink                      5u 0
027600000000     D                                2a
027700000000     D  st_uid                       10u 0
027800081021     D  st_gid                       10u 0
027900000000     D  st_size                      10i 0
028000000000     D  st_atime                     10i 0
028100000000     D  st_mtime                     10i 0
028200000000     D  st_ctime                     10i 0
028300000000     D  st_dev                       10u 0
028400000000     D  st_blksize                   10u 0
028500000000     D  st_allocsize                 10u 0
028600000000     D  st_objtype                   11a
028700000000     D                                1a
028800000000     D  st_codepage                   5u 0
028900000000     D  st_reserv1                   62a
029000000000     D  st_ino_gen_id                10u 0
029100000000     **
029200000000     D pBuf            s               *   Inz( %Addr( Buf ))
029300000000
029400000000     **-- Get file or link information:
029500000000     D lstat           Pr            10i 0 ExtProc( 'QlgLstat' )
029600000000     D  PthStr                     4096a   Const  Options( *VarSize )
029700000000     D  Buf                            *   Value
029800000000     **-- Retrieve object references:
029900000000     D RtvObjRef       Pr                  ExtPgm( 'QP0LROR' )
030000000000     D  RoRcvVar                  65535a          Options( *VarSize )
030100000000     D  RoRcvVarLen                  10u 0 Const
030200000000     D  RoFmtNam                      8a   Const
030300000000     D  RoPthStr                   4096a   Const  Options( *VarSize )
030400000000     D  RoError                   32767a          Options( *VarSize: *NoPass)
030500000000     **-- Retrieve job information:
030600000000     D RtvJobInf       Pr                  ExtPgm( 'QUSRJOBI' )
030700000000     D  RiRcvVar                  32767a         Options( *VarSize )
030800000000     D  RiRcvVarLen                  10i 0 Const
030900000000     D  RiFmtNam                      8a   Const
031000000000     D  RiJobNamQ                    26a   Const
031100000000     D  RiJobIntId                   16a   Const
031200000000     D  RiError                   32767a         Options( *NoPass: *VarSize )
031300000000     **-- Send program message:
031400000000     D SndPgmMsg       Pr                  ExtPgm( 'QMHSNDPM' )
031500000000     D  SpMsgId                       7a   Const
031600000000     D  SpMsgFq                      20a   Const
031700000000     D  SpMsgDta                    128a   Const
031800000000     D  SpMsgDtaLen                  10i 0 Const
031900000000     D  SpMsgTyp                     10a   Const
032000000000     D  SpCalStkE                    10a   Const  Options( *VarSize )
032100000000     D  SpCalStkCtr                  10i 0 Const
032200000000     D  SpMsgKey                      4a
032300000000     D  SpError                      10i 0 Const
032400000000     **-- Retrieve last spooled file identity:
032500000000     D RtvLstSplfId    Pr                  ExtPgm( 'QSPRILSP' )
032600000000     D  RsRcvVar                  32767a          Options( *VarSize )
032700000000     D  RsRcvVarLen                  10i 0 Const
032800000000     D  RsFmtNam                      8a   Const
032900000000     D  RsError                   32767a          Options( *VarSize )
033000000000     **-- Run system command:
033100000000     D system          Pr            10i 0 ExtProc( 'system' )
033200000000     D  command                        *   Value  Options( *String )
033300000000
033400000000     **-- Get job type:
033500000000     D GetJobTyp       Pr             1a
033600000000     **-- Send escape message:
033700000000     D SndEscMsg       Pr            10i 0
033800000000     D  PxMsgDta                    512a   Const  Varying
033900000000     **-- Send completion message:
034000000000     D SndCmpMsg       Pr            10i 0
034100000000     D  PxMsgDta                    512a   Const  Varying
034200000000     **-- Error identification:
034300000000     D errno           Pr            10i 0
034400000000     D strerror        Pr           128a   Varying
034500000000
034600000000     **-- Parameters:
034700081021     D UBIFSLCK        Pr
034800000000     D  PxPthNam                    300a   Varying
034900000000     D  PxOutOpt                      3a
035000081021     D  PxOutSts                      4a
035100000000     **
035200081021     D UBIFSLCK        Pi
035300000000     D  PxPthNam                    300a   Varying
035400000000     D  PxOutOpt                      3a
035500081021     D  PxOutSts                      4a
035600000000
035700000000      /Free
035800000000
035900000000        ApiPath.PthNam = PxPthNam;
036000000000        ApiPath.PthNamLen = %Len( PxPthNam );
036100000000
036200000000        If  lstat( ApiPath: pBuf ) = -1;
036300000000
036400000000          SndEscMsg( %Char( Errno ) + ': ' + Strerror );
036500000000        Else;
036600000000
036700081021         If PxOutOpt <> 'BCH';
036800000000          system( 'OVRPRTF FILE(QSYSPRT) HOLD(*YES) SECURE(*YES) ' +
036900081021                  'OVRSCOPE(*JOB)');
037000000000
037100000000          Open  QSYSPRT;
037200081021         Endif;
037300000000
037400000000          BytAlc = 65535;
037500000000          pObjRef = %Alloc( BytAlc );
037600000000          RORO0200.BytAvl = *Zero;
037700000000
037800000000          DoU  RORO0200.BytAvl <= BytAlc;
037900000000
038000000000            If  RORO0200.BytAvl > BytAlc;
038100000000              BytAlc = RORO0200.BytAvl;
038200000000              pObjRef = %ReAlloc( pObjRef: BytAlc );
038300000000            EndIf;
038400000000
038500000000            Monitor;
038600000000              RtvObjRef( RORO0200: BytAlc: 'RORO0200': ApiPath: ERRC0100 );
038700081021              If  RORO0200.InUseI = 1;
038800081021                  PxOutSts = '*YES';
038900081021              Else;
039000081021                  PxOutSts = '*NO';
039100081021              EndIf;
039200000000
039300000000            On-Error;
039400000000              SndEscMsg( 'Release must be V5R2 or higher.');
039500000000            EndMon;
039600000000          EndDo;
039700000000
039800081021          If PxOutOpt <> 'BCH';
039900081021             If  ERRC0100.BytAvl = *Zero;
040000081021                 ExSr  PrcObjRef2;
040100081021             EndIf;
040200081021          EndIf;
040300000000
040400000000          DeAlloc  pObjRef;
040500000000
040600081021
040700081021          If PxOutOpt <> 'BCH';
040800081021             system( 'DLTOVR  FILE(QSYSPRT) LVL(*JOB)' );
040900000000
041000081021             Close  QSYSPRT;
041100081021
041200000000
041300081021             If  PxOutOpt = 'DSP'  And  GetJobTyp() = 'I';
041400081021                 ExSr  DspLst;
041500081021             Else;
041600081021                 ExSr  WrtLst;
041700081021             EndIf;
041800081021           EndIf;
041900081021        EndIf;
042000000000
042100000000        *InLr = *On;
042200000000        Return;
042300000000
042400000000        BegSr  DspLst;
042500000000
042600000000          RtvLstSplfId( SPRL0100: %Size( SPRL0100 ): 'SPRL0100': ERRC0100 );
042700000000
042800000000          system( 'DSPSPLF '                     +
042900000000                  'FILE(' + %Trim( SPRL0100.SplfNam )   + ') ' +
043000000000                  'JOB('  + %Trim( SPRL0100.JobNbr )    + '/'  +
043100000000                            %Trim( SPRL0100.UsrNam )    + '/'  +
043200000000                            %Trim( SPRL0100.JobNam )    + ') ' +
043300000000                  'SPLNBR(' + %Char( SPRL0100.SplfNbr ) + ')'
043400000000                );
043500000000
043600000000          system( 'DLTSPLF '                     +
043700000000                  'FILE(' + %Trim( SPRL0100.SplfNam )   + ') ' +
043800000000                  'JOB('  + %Trim( SPRL0100.JobNbr )    + '/'  +
043900000000                            %Trim( SPRL0100.UsrNam )    + '/'  +
044000000000                            %Trim( SPRL0100.JobNam )    + ') ' +
044100000000                  'SPLNBR(' + %Char( SPRL0100.SplfNbr ) + ')'
044200000000                );
044300000000
044400000000        EndSr;
044500000000
044600000000        BegSr  WrtLst;
044700000000
044800000000          SndCmpMsg( 'List has been printed.' );
044900000000
045000000000        EndSr;
045100000000
045200000000        BegSr  RtvObjRef1;
045300000000
045400000000          //-- Not referenced - included only as a sample!
045500000000
045600000000          BytAlc = 65535;
045700000000          pObjRef = %Alloc( BytAlc );
045800000000          RORO0100.BytAvl = *Zero;
045900000000
046000000000          DoU  RORO0100.BytAvl <= BytAlc;
046100000000
046200000000            If  RORO0100.BytAvl > BytAlc;
046300000000              BytAlc = RORO0100.BytAvl;
046400000000              pObjRef = %ReAlloc( pObjRef: BytAlc );
046500000000            EndIf;
046600000000
046700000000            RtvObjRef( RORO0100: BytAlc: 'RORO0100': ApiPath: ERRC0100 );
046800000000          EndDo;
046900000000
047000000000          If  ERRC0100.BytAvl = *Zero;
047100000000            ExSr  PrcObjRef1;
047200000000          EndIf;
047300000000
047400000000        EndSr;
047500000000
047600000000        BegSr  PrcObjRef1;
047700000000
047800000000          If  RORO0100.OfsSmpRef > *Zero  And
047900000000              RORO0100.LenSmpRef = %Size( SmpObjRef );
048000000000
048100000000            pSmpObjRef = %Addr( RORO0100 ) + RORO0100.OfsSmpRef;
048200000000
048300000000            //--
048400000000
048500000000          EndIf;
048600000000
048700000000        EndSr;
048800000000
048900000000        BegSr  PrcObjRef2;
049000000000
049100000000          Time = %Int( %Char( %Time(): *ISO0));
049200000000
049300000000          Except  Header;
049400000000
049500000000          If  RORO0200.OfsSmpRef > *Zero  And
049600000000              RORO0200.LenSmpRef = %Size( SmpObjRef );
049700000000
049800000000            pSmpObjRef = %Addr( RORO0200 ) + RORO0200.OfsSmpRef;
049900000000
050000000000            ExSr  WrtLstHdr;
050100000000          EndIf;
050200000000
050300000000          If  RORO0200.OfsExtRef > *Zero  And
050400000000              RORO0200.LenExtRef = %Size( ExtObjRef );
050500000000
050600000000            pExtObjRef = %Addr( RORO0200 ) + RORO0200.OfsExtRef;
050700000000          EndIf;
050800000000
050900000000          If  RORO0200.OfsJobLst > *Zero;
051000000000
051100000000            ExSr  PrcJobLst;
051200000000          EndIf;
051300000000
051400000000          If  NbrRcds = *Zero;
051500000000            Except  NoRcds;
051600000000          EndIf;
051700000000
051800000000        EndSr;
051900000000
052000000000        BegSr  PrcJobLst;
052100000000
052200000000          pJobUsgObj = %Addr( RORO0200 ) + RORO0200.OfsJobLst;
052300000000
052400000000          For  Idx = 1  to RORO0200.NbrJobRtn;
052500000000
052600000000            If  JobUsgObj.DplSmpRef > *Zero;
052700000000              pSmpObjRef  = pJobUsgObj + JobUsgObj.DplSmpRef;
052800000000
052900000000              //--
053000000000
053100000000            EndIf;
053200000000
053300000000            If  JobUsgObj.DplExtRef > *Zero;
053400000000              pExtObjRef  = pJobUsgObj + JobUsgObj.DplExtRef;
053500000000
053600000000              //--
053700000000
053800000000            EndIf;
053900000000
054000000000            ExSr  WrtLckDtl;
054100000000
054200000000            If  Idx < RORO0200.NbrJobRtn;
054300000000              pJobUsgObj += JobUsgObj.DplNxtJobE;
054400000000            EndIf;
054500000000          EndFor;
054600000000
054700000000        EndSr;
054800000000
054900000000        BegSr  WrtLckDtl;
055000000000
055100000000          If  PrtLinInf.CurLin > PrtLinInf.OvfLin - 3;
055200000000            Except  Header;
055300000000            Except  DtlHdr;
055400000000          EndIf;
055500000000
055600000000          NbrRcds += 1;
055700000000
055800000000          SavLck     = SmpObjRef.SavLck;
055900000000          AtrLck     = SmpObjRef.AtrLck;
056000000000          ShrNoRdWrt = SmpObjRef.ShrNoRdWrt;
056100000000          ShrRdWrt   = SmpObjRef.ShrRdWrt;
056200000000          ShrWrtOnly = SmpObjRef.ShrWrtOnly;
056300000000          ShrRdOnly  = SmpObjRef.ShrRdOnly;
056400000000          Execute    = SmpObjRef.Execute;
056500000000          ReadWrt    = SmpObjRef.ReadWrt;
056600000000          WrtOnly    = SmpObjRef.WrtOnly;
056700000000          ReadOnly   = SmpObjRef.ReadOnly;
056800000000          JobNbr     = JobUsgObj.JobNbr;
056900000000          JobUsr     = JobUsgObj.JobUsr;
057000000000          JobNam     = JobUsgObj.JobNam;
057100000000
057200000000          Except  LckDtl;
057300000000
057400000000        EndSr;
057500000000
057600000000        BegSr  WrtLstHdr;
057700000000
057800000000          If  ApiPath.PthNamLen > %Size( IfsObj );
057900000000            EvalR  IfsObj = ApiPath.PthNam;
058000000000            %Subst( IfsObj: 1: 3 ) = '...';
058100000000
058200000000          Else;
058300000000            IfsObj = ApiPath.PthNam;
058400000000          EndIf;
058500000000
058600000000          If  RORO0200.InUseI = 1;
058700000000            ObjUse = '*YES';
058800000000          Else;
058900000000            ObjUse = '*NO ';
059000000000          EndIf;
059100000000
059200000000          If  SmpObjRef.ChkOutUsrNm > *Blanks;
059300000000            ChkUsr = SmpObjRef.ChkOutUsrNm;
059400000000          Else;
059500000000            ChkUsr = '*NONE';
059600000000          EndIf;
059700000000
059800000000          Except  LstHdr;
059900000000          Except  DtlHdr;
060000000000
060100000000        EndSr;
060200000000
060300000000      /End-Free
060400000000
060500000000     **-- Printer file definition:  ------------------------------------------**
060600000000     OQSYSPRT   EF           Header         2  2
060700000000     O                       UDATE         Y      8
060800000000     O                       Time                18 '  :  :  '
060900000000     O                                           70 'Display IFS object locks'
061000000000     O                                          107 'Program:'
061100000000     O                       PsPgmNam           118
061200000000     O                                          126 'Page:'
061300000000     O                       PAGE             +   1
061400000000     OQSYSPRT   EF           LstHdr         1
061500000000     O                                           18 'IFS object . . . :'
061600000000     O                       IfsObj             132
061700000000     OQSYSPRT   EF           LstHdr         1
061800000000     O                                           18 'Object in use  . :'
061900000000     O                       ObjUse              24
062000000000     OQSYSPRT   EF           LstHdr         1
062100000000     O                                           18 'Check out user . :'
062200000000     O                       ChkUsr              30
062300000000     OQSYSPRT   EF           DtlHdr         1
062400000000     O                                           98 '------------- shared ------
062500000000     O                                              --------'
062600000000     OQSYSPRT   EF           DtlHdr         1
062700000000     O                                            8 'Job name'
062800000000     O                                           20 'Job user'
062900000000     O                                           31 'Job nbr'
063000000000     O                                           40 'Rd only'
063100000000     O                                           49 'Wr only'
063200000000     O                                           56 'Rd/wr'
063300000000     O                                           62 'Exec'
063400000000     O                                           71 'Rd only'
063500000000     O                                           80 'Wr only'
063600000000     O                                           88 'Rd/wr'
063700000000     O                                           98 'No rd/wr'
063800000000     O                                          108 'Atr lock'
063900000000     O                                          119 'Save lock'
064000000000     **-- Write right->left (prevent overlay):
064100000000     OQSYSPRT   EF           LckDtl         1
064200000000     O                       SavLck        3    115
064300000000     O                       AtrLck        3    105
064400000000     O                       ShrNoRdWrt    3     95
064500000000     O                       ShrRdWrt      3     86
064600000000     O                       ShrWrtOnly    3     78
064700000000     O                       ShrRdOnly     3     69
064800000000     O                       Execute       3     61
064900000000     O                       ReadWrt       3     54
065000000000     O                       WrtOnly       3     47
065100000000     O                       ReadOnly      3     38
065200000000     O                       JobNbr              30
065300000000     O                       JobUsr              22
065400000000     O                       JobNam              10
065500000000     **
065600000000     OQSYSPRT   EF           NoRcds      1
065700000000     O                                           26 '(No IFS locks found)'
065800000000     **-- Get job type:  -----------------------------------------------------**
065900000000     P GetJobTyp       B
066000000000     D                 Pi             1a
066100000000
066200000000     D JOBI0400        Ds                  Qualified
066300000000     D  BytRtn                       10i 0
066400000000     D  BytAvl                       10i 0
066500000000     D  JobNam                       10a
066600000000     D  UsrNam                       10a
066700000000     D  JobNbr                        6a
066800000000     D  JobIntId                     16a
066900000000     D  JobSts                       10a
067000000000     D  JobTyp                        1a
067100000000     D  JobSubTyp                     1a
067200000000
067300000000      /Free
067400000000
067500000000        RtvJobInf( JOBI0400
067600000000                 : %Size( JOBI0400 )
067700000000                 : 'JOBI0400'
067800000000                 : '*'
067900000000                 : *Blank
068000000000                 : ERRC0100
068100000000                 );
068200000000
068300000000        If  ERRC0100.BytAvl > *Zero;
068400000000          Return  *Blank;
068500000000
068600000000        Else;
068700000000          Return  JOBI0400.JobTyp;
068800000000        EndIf;
068900000000
069000000000      /End-Free
069100000000
069200000000     P GetJobTyp       E
069300000000     **-- Send escape message:  ----------------------------------------------**
069400000000     P SndEscMsg       B
069500000000     D                 Pi            10i 0
069600000000     D  PxMsgDta                    512a   Const  Varying
069700000000
069800000000      /Free
069900000000
070000000000        CallP(e)  SndPgmMsg( 'CPF9897'
070100000000                           : 'QCPFMSG   *LIBL'
070200000000                           : PxMsgDta
070300000000                           : %Len( PxMsgDta )
070400000000                           : '*ESCAPE'
070500000000                           : '*PGMBDY'
070600000000                           : 1
070700000000                           : MsgKey
070800000000                           : *Zero
070900000000                           );
071000000000
071100000000        If  %Error;
071200000000          Return  -1;
071300000000
071400000000        Else;
071500000000          Return  0;
071600000000        EndIf;
071700000000
071800000000      /End-Free
071900000000
072000000000     P SndEscMsg       E
072100000000     **-- Send completion message:  ------------------------------------------**
072200000000     P SndCmpMsg       B
072300000000     D                 Pi            10i 0
072400000000     D  PxMsgDta                    512a   Const  Varying
072500000000
072600000000      /Free
072700000000
072800000000        CallP(e)  SndPgmMsg( 'CPF9897'
072900000000                           : 'QCPFMSG   *LIBL'
073000000000                           : PxMsgDta
073100000000                           : %Len( PxMsgDta )
073200000000                           : '*COMP'
073300000000                           : '*PGMBDY'
073400000000                           : 1
073500000000                           : MsgKey
073600000000                           : *Zero
073700000000                           );
073800000000
073900000000        If  %Error;
074000000000          Return  -1;
074100000000
074200000000        Else;
074300000000          Return  0;
074400000000        EndIf;
074500000000
074600000000      /End-Free
074700000000
074800000000     P SndCmpMsg       E
074900000000     **-- Get runtime error number:  -----------------------------------------**
075000000000     P Errno           B
075100000000     D                 Pi            10i 0
075200000000
075300000000     D sys_errno       Pr              *    ExtProc( '__errno' )
075400000000
075500000000     D Error           s             10i 0  Based( pError )  NoOpt
075600000000
075700000000      /Free
075800000000
075900000000        pError = sys_errno;
076000000000
076100000000        Return  Error;
076200000000
076300000000      /End-Free
076400000000
076500000000     P Errno           E
076600000000     **-- Get runtime error text:  -------------------------------------------**
076700000000     P Strerror        B
076800000000     D                 Pi           128a    Varying
076900000000
077000081021     D sys_strerror    Pr              *    ExtProc('strerror')
077100000000     D                               10i 0  Value
077200000000
077300000000      /Free
077400000000
077500081021        Return  %Str(sys_strerror(Errno));
077600000000
077700000000      /End-Free
077800000000
077900081021     P Strerror        E

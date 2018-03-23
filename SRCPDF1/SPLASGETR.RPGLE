000100000000      *CIPHER
000200000000     H Dftactgrp(*NO) Actgrp(*NEW) BNDDIR('QC2LE') debug
000300130206     H COPYRIGHT('Giuseppe Costagliola 2013 - beppecosta@yahoo.it')
000400000000
000500000000      //-------------------------------------------------------------------
000600000000      // Parameters
000700000000      //-------------------------------------------------------------------
000800000000     DEntryParms       PR                  extpgm('SPLASGETR')
000900000000     D SpfName                       10A
001000000000     D JobName                       26A
001100000000     D SpfNbr                         6  0
001200130206     D PclPath                       40A
001300000000     DEntryParms       PI
001400000000     D SpfName                       10A
001500000000     D JobName                       26A
001600000000     D SpfNbr                         6  0
001700130206     D PclPath                       40A
001800000000
001900000000      //Error Code Parameter
002000000000      /COPY QSYSINC/QRPGLESRC,QUSEC
002100000000     DQUSED01                        60
002200000000
002300000000      //Get Spooled File Data
002400000000      /COPY QSYSINC/QRPGLESRC,QSPGETSP
002500000000
002600000000      //Stream file APIs                                                   .
002700000000     Dunlink           PR             9B 0 EXTPROC('unlink')
002800000000     D                                 *   VALUE
002900000000     Dopen             PR            10I 0 EXTPROC('open')
003000000000     D                                 *   VALUE
003100000000     D                               10I 0 VALUE
003200000000     D                               10U 0 VALUE OPTIONS(*NOPASS)
003300000000     D                               10U 0 VALUE OPTIONS(*NOPASS)
003400000000     D O_CREAT         S             10I 0 INZ(8)
003500000000     D O_WRONLY        S             10I 0 INZ(2)
003600000000     D O_TRUNC         S             10I 0 INZ(64)
003700000000     D O_CODEPAGE      S             10I 0 INZ(8388608)
003800000000     D S_IRWXU         S             10I 0 INZ(448)
003900000000     D S_IROTH         S             10I 0 INZ(4)
004000000000     Dwrite            PR            10I 0 EXTPROC('write')
004100000000     D                               10I 0 VALUE
004200000000     D                                 *   VALUE
004300000000     D                               10I 0 VALUE
004400000000     Dclose            PR            10I 0 EXTPROC('close')
004500000000     D                               10I 0 VALUE
004600000000      //
004700000000     Dfd               S             10I 0
004800000000     Dbytesw           S             10I 0
004900000000
005000000000     D* Copy Bytes left adjusted (MemCpy)
005100000000     Dcpybla           pr                  ExtProc('cpybla')
005200000000     D Receiver                        *   value
005300000000     D Source                          *   value
005400000000     D Size                          10i 0 value
005500000000
005600000000      // QSPOPNSP/QSPGETSP/QSPCLOSP                                         .
005700000000     DSpfHandle        S              9B 0
005800000000     DSpfBuffer        S               *
005900000000     DInfBuffer        S               *
006000000000     DPrtBuffer        S               *
006100000000
006200000000      //Local procedures
006300000000     DCrtUsrSpaces     PR
006400000000     DGetSpoolFile     PR
006500000000     DCrtSPLStmf       PR
006600000000     DGetSPLData       PR
006700000000     DCloseSPLStmf     PR
006800000000     DGetErrInfo       PR           128
006900000000     DSndPgmMsg        PR
007000000000     D type                          10A   VALUE
007100000000     D function                      20A   VALUE
007200000000     D msgId                          7A   VALUE
007300000000     D msg                          128A   VALUE
007400000000
007500000000      // Local variables
007600000000     Dmsg              S            128
007700000000     Dbuffer           S             10i 0
007800000000
007900000000      //--------------------------------------------------------------------
008000000000      //Main
008100000000      //--------------------------------------------------------------------
008200000000
008300000000      /free
008400000000
008500000000       QUSBPRV = %len(QUSEC);
008600000000
008700000000       //Prepare user space
008800000000       CrtUsrSpaces ();
008900000000
009000000000       //Retrieve spool file
009100000000       GetSpoolFile ();
009200000000
009300000000       //Create SPL Stmf
009400000000       CrtSPLStmf ();
009500000000
009600000000       //Get SPL Data
009700000000       GetSPLData ();
009800000000
009900000000       //Close SPL Stmf
010000000000       CloseSPLStmf ();
010100000000
010200000000       //End program
010300000000       SndPgmMsg ('*COMP':' ':' ':'Spool retrieved');
010400000000       *InLR = *On;
010500000000       Return;
010600000000
010700000000      /end-free
010800000000
010900000000      //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
011000000000      // Get SPL Data
011100000000      //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
011200000000     PGetSPLData       B
011300000000     DGetSPLData       PI
011400000000
011500000000     DErrMsg           S            128A
011600000000
011700000000      /free
011800000000
011900000000       //Set pointer to first buffer
012000000000       InfBuffer = SpfBuffer + QSPOFB;  // Offset First Buffer
012100000000
012200000000       //..loop thru buffer
012300000000       for buffer = 1 to QSPBRTN01;   // Buffers Returned
012400000000         //retrieve 'Buffer information'
012500000000         cpybla (%addr(QSPSPFRB) : InfBuffer : %size(QSPSPFRB));
012600000000         //retrieve 'General data (information buffer)'
012700000000         cpybla (%addr(QSPSPFRG) : SpfBuffer + QSPOGI : %size(QSPSPFRG));
012800000000         //Write SPL Data.
012900000000         bytesw = write(fd : SpfBuffer + QSPOPD00 : QSPSPD00);
013000000000         If bytesw <> QSPSPD00;
013100000000           ErrMsg = 'write() failed. ' + geterrinfo;
013200000000         Endif;
013300000000         //Address to next buffer
013400000000         InfBuffer += QSPLBI;   // Length Buffer Info
013500000000       EndFor;
013600000000
013700000000      /end-free
013800000000
013900000000     PGetSPLData       E
014000000000
014100000000      //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
014200000000      // Get C error data
014300000000      //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
014400000000     Pgeterrinfo       B
014500000000     Dgeterrinfo       PI           128
014600000000     Dgeterrno         PR              *   EXTPROC('__errno')
014700000000     Dstrerror         PR              *   EXTPROC('strerror')
014800000000     D errno                         10I 0 VALUE
014900000000     Derrnum           S             10I 0 BASED(errnum_p)
015000000000      /free
015100000000       errnum_p = geterrno;
015200000000       Return %TRIM(%EDITC(errnum : '3')) + ' : ' +
015300000000           %STR(strerror(errnum));
015400000000      /end-free
015500000000     Pgeterrinfo       E
015600000000
015700000000      //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
015800000000      // Create user spaces
015900000000      //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
016000000000     PCrtUsrSpaces     B
016100000000     DCrtUsrSpaces     PI
016200000000
016300000000     D QUSCRTUS        PR                  extpgm('QUSCRTUS')
016400000000     D  UserSpace                    20    const
016500000000     D  ExtendedAtr                  10    const
016600000000     D  InitialSize                   9B 0 const
016700000000     D  InitialValue                  1    const
016800000000     D  PublicAuth                   10    const
016900000000     D  Text                         50    const
017000000000     D  Replace                      10    const
017100000000     D  ApiError                    120    options(*varsize)
017200000000     D  Domain                       10    options(*nopass)
017300000000     D QUSPTRUS        PR                  extpgm('QUSPTRUS')
017400000000     D  UserSpace                    20    const
017500000000     D  UserSpacePtr                   *
017600000000      /free
017700000000
017800000000       // create the spool space and address to it
017900000000       QUSCRTUS ('QSPGETSP  QTEMP     ' : ' ' : 1024 : X'00' :
018000000000                 '*ALL' : 'Space per SPL2TIFF' : '*NO' : QUSEC);
018100000000       QUSPTRUS ('QSPGETSP  QTEMP     ' : SpfBuffer);
018200000000
018300000000      /end-free
018400000000     PCrtUsrSpaces     E
018500000000
018600000000      //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
018700000000      // Get the spool file into the user space
018800000000      //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
018900000000     PGetSpoolFile     B
019000000000     DGetSpoolFile     PI
019100000000
019200000000     DQSPOPNSP         PR                  extpgm('QSPOPNSP')
019300000000     D SpfHandle                      9B 0 const
019400000000     D JobName                       26A   const
019500000000     D IntJobId                      16A   const
019600000000     D IntSpfId                      16A   const
019700000000     D SfpName                       10A   const
019800000000     D SpfNumber                      9B 0 const
019900000000     D BufferToGet                    9B 0 const
020000000000     D ApiError                     120    options(*varsize)
020100000000     D JobSystemName                  8A   const
020200000000     D SpfCreateDate                  7A   const
020300000000     D SpfCreateTime                  6A   const
020400000000     DQSPGETSP         PR                  extpgm('QSPGETSP')
020500000000     D SpfHandle                      9B 0 const
020600000000     D UserSpace                     20A   const
020700000000     D FormatName                     8A   const
020800000000     D BufferToGet                    9B 0 const
020900000000     D EndOfSpf                      10A   const
021000000000     D ApiError                     120    options(*varsize)
021100000000     DQSPCLOSP         PR                  extpgm('QSPCLOSP')
021200000000     D SpfHandle                      9B 0 const
021300000000     D ApiError                     120    options(*varsize)
021400000000      /free
021500000000
021600000000       // Open spool file
021700000000       QSPOPNSP (SpfHandle : JobName : ' ' : ' ' :
021800000000                 SpfName : SpfNbr : -1 : QUSEC :
021900000000                 '*CURRENT' : '*ONLY' : ' ');
022000000000       If QUSBAVL > 0;
022100000000         SndPgmMsg ('*ESCAPE':'QSPGETSP':QUSEI:%subst(QUSED01:1:QUSBAVL-16));
022200000000       Endif;
022300000000
022400000000       // Get spool data
022500000000       QSPGETSP (SpfHandle : 'QSPGETSP  QTEMP     ':
022600000000                 'SPFR0200' : -1 : '*WAIT' : QUSEC);
022700000000       If QUSBAVL > 0;
022800000000         SndPgmMsg ('*ESCAPE':'QSPGETSP':QUSEI:%subst(QUSED01:1:QUSBAVL-16));
022900000000       Endif;
023000000000
023100000000       // retrieve 'General header'
023200000000       cpybla(%addr(QSPSPFRH) : SpfBuffer : %size(QSPSPFRH));
023300000000
023400000000       // Close spool file
023500000000       QSPCLOSP (SpfHandle : QUSEC);
023600000000       If QUSBAVL > 0;
023700000000         SndPgmMsg ('*ESCAPE':'QSPCLOSP':QUSEI:%subst(QUSED01:1:QUSBAVL-16));
023800000000       Endif;
023900000000
024000000000       // Return spool if complete else exit
024100000000       if QSPICI <> 'C';
024200000000         SndPgmMsg ('*ESCAPE':'QSPOPNSP' : ' ' : 'Spool too big !!');
024300000000       endif;
024400000000
024500000000      /end-free
024600000000
024700000000     PGetSpoolFile     E
024800000000
024900000000      //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
025000000000      // Create an empty Tiff file
025100000000      //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
025200000000     PCrtSPLStmf       B
025300000000     DCrtSPLStmf       PI
025400000000
025500000000     DSPL              S            128A
025600000000     DErrMsg           S            128A
025700000000
025800000000      /free
025900000000
026000130206       SPL = %TRIM(PclPath) + '/ascii.pcl';
026100000000       SPL = %TRIM(SPL) + X'00';
026200000000
026300000000       // Check whether stream file exists or not.
026400000000       fd = open(%Addr(SPL) : 1);
026500000000
026600000000       // If Stream file exists, close the stream file and unlink
026700000000       If fd <> -1;
026800000000         If close(fd) = -1;
026900000000           ErrMsg = 'close() failed. ' + geterrinfo;
027000000000         EndIf;
027100000000         If unlink(%Addr(SPL)) = -1;
027200000000           ErrMsg = 'unlink() failed. ' + geterrinfo;
027300000000         Endif;
027400000000       EndIf;
027500000000
027600000000       // if errors exit
027700000000       if ErrMsg <> ' ';
027800000000         SndPgmMsg ('*ESCAPE':'CrtSPLStmf': ' ' : ErrMsg);
027900000000       Endif;
028000000000
028100000000       // Open(Create) stream file
028200000000       fd = open(%Addr(SPL)
028300000000            : O_CREAT + O_WRONLY + O_TRUNC + O_CODEPAGE
028400000000            : S_IRWXU + S_IROTH : 280);
028500000000       If fd = -1;
028600000000         ErrMsg = 'open() failed. ' + geterrinfo;
028700000000       EndIf;
028800000000
028900000000       // if errors exit
029000000000       if ErrMsg <> ' ';
029100000000         SndPgmMsg ('*ESCAPE':'CrtSPLStmf': ' ' : ErrMsg);
029200000000       Endif;
029300000000
029400000000      /end-free
029500000000
029600000000     PCrtSPLStmf       E
029700000000
029800000000      //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
029900000000      // Close
030000000000      //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
030100000000     PCloseSPLStmf     B
030200000000     DCloseSPLStmf     PI
030300000000
030400000000     DErrMsg           S            128A
030500000000
030600000000      /free
030700000000
030800000000       If close(fd) = -1;
030900000000         ErrMsg = 'close() failed. ' + geterrinfo;
031000000000         SndPgmMsg ('*ESCAPE':'CloseSPLStmf': ' ' : ErrMsg);
031100000000       EndIf;
031200000000
031300000000      /end-free
031400000000
031500000000     PCloseSPLStmf     E
031600000000
031700000000
031800000000      //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
031900000000      // Send Program Message
032000000000      //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
032100000000     PSndPgmMsg        B
032200000000     DSndPgmMsg        PI
032300000000     D msgType                       10A   VALUE
032400000000     D function                      20A   VALUE
032500000000     D msgId                          7A   VALUE
032600000000     D msgErr                       128A   VALUE
032700000000
032800000000     D QMHSNDPM        PR                  extpgm('QMHSNDPM')
032900000000     D  MessageId                     7    const
033000000000     D  MessageFile                  20    const
033100000000     D  MessageData                 512    const options(*varsize)
033200000000     D  MessageDataL                  9b 0 const
033300000000     D  MessageType                  10    const
033400000000     D  CallStkEntry                128    const options(*varsize)
033500000000     D  CallStkCount                  9b 0 const
033600000000     D  MessageKey                    4    const
033700000000     D  ApiError                    120    options(*varsize)
033800000000
033900000000     D idMsg           S              7A
034000000000     D errMsg          S            128A
034100000000
034200000000      /free
034300000000
034400000000       if msgId <> ' ';
034500000000         idMsg  = msgId;
034600000000         errMsg = msgErr;
034700000000       else;
034800000000         if msgType = '*ESCAPE';
034900000000           idMsg  = 'CPF9898';
035000000000           errMsg = 'Errore ' + %trimr(function) + ' : ' + msgErr;
035100000000         else;
035200000000           errMsg = msgErr;
035300000000         endif;
035400000000       endif;
035500000000
035600000000       QMHSNDPM (idMsg: 'QCPFMSG   QSYS' : errMsg : %len(errMsg) :
035700000000                 msgType : '*CTLBDY' : 1 : ' ' : QUSEC);
035800000000
035900000000      /end-free
036000000000     PSndPgmMsg        E

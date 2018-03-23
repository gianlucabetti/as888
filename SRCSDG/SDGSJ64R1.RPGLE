000100000000     H*PARMS DFTACTGRP(*NO)
000200000000     H*PARMS ACTGRP(QILE) OPTION(*NOXREF *NODEBUGIO)
000300000000     H*PARMS DBGVIEW(*SOURCE)
000400000000
000500000000     H DECEDIT('0,') DATEDIT(*YMD.)
000600000000
000700000000      /define OS400_JVM_12
000800000000      /copy qsysinc/qrpglesrc,jni
000900000000
001000000000      * ---------
001100000000      * Definizione Procedure
001200000000     D GetOsEnv        PR              *   EXTPROC('Qp0zGetEnvNoCCSID')
001300000000     D  Name                           *   VALUE OPTIONS(*STRING)
001400000000      *
001500000000     D PutOsEnv        PR              *   EXTPROC('Qp0zPutEnvNoCCSID')
001600000000     D  name                           *   VALUE OPTIONS(*STRING)
001700000000      *
001800000000     D start_jvm       PR              *
001900000000      *
002000000000     D attach_jvm      PR              *
002100000000      *
002200000000     D status          PR             1N
002300000000     D   peMsgTxt                   256A   const
002400000000      *
002500000000     D jStr            PR                  like(jString)
002600000000     D                                     EXTPROC(*JAVA
002700000000     D                                     :'java.lang.String'
002800000000     D                                     :*CONSTRUCTOR)
002900000000     D create_from                32767A   VARYING const
003000000000
003100000000      * --------
003200000000      * Definizioni x Local Envirorment
003300000000     D EnvPtr          S               *
003400150318     D EnvVal          S           1024A   VARYING
003500000000     D EnvValChanged   S               N   Inz(*Off)
003600000000     D pEnv            s               *
003700000000     D sResult         s             10P 5
003800000000     D nResult         s                   Like(jBoolean)
003900000000     D sStringSrc      s                   Like(jString)
004000000000     D sStringOvl      s                   Like(jString)
004100000000     D sStringOut      s                   Like(jString)
004200000000
004300000000      * --------
004400000000     D                 ds
004500000000     D iJVMVer                       10I 0
004600000000     D  iHigh                         5I 0 overlay(iJVMVer:1)
004700000000     D  iLow                          5I 0 overlay(iJVMVer:*NEXT)
004800000000
004900000000
005000000000      * ----------------------------------------- *
005100000000      *  Definizioni per org.gcGroup.PDFs.OverLay *
005200000000      * ----------------------------------------- *
005300000000     D cOverLay...
005400000000     D                 PR              O   ExtProc(*JAVA
005500000000     D                                     :'org.gcGroup.PDFs.OverLay'
005600000000     D                                     :*CONSTRUCTOR)
005700000000     D                                     CLASS(*JAVA
005800000000     D                                     :'org.gcGroup.PDFs.OverLay')
005900000000      *
006000000000     D oOverLay...
006100000000     D                 S               O   CLASS(*JAVA
006200000000     D                                     :'org.gcGroup.PDFs.OverLay')
006300000000      *
006400000000     D mPDFsOverLay...
006500000000     D                 PR              n   STATIC
006600000000     D                                     ExtProc(*JAVA:
006700000000     D                                     'org.gcGroup.PDFs.OverLay':
006800000000     D                                     'PDFsOverLay')
006900000000     D srcPDF                              Like(jString)                        (File .pdf source )
007000000000     D ovlPDF                              Like(jString)                        (File .pdf overlay)
007100000000     D outPDF                              Like(jString)                        (File .pdf output )
007200000000
007300000000     D OSExeCmd        PR                  ExtPgm('QCMDEXC')
007400000000     D  PR_Cmd                     1024A   Const Options(*VarSize)
007500000000     D  PR_CmdLgh                    15P 5 Const
007600000000     D  PR_IGCProcess                 3A   Const Options(*NoPass)
007700000000
007800000000     D aCmdString      s           1024a
007900000000     D cSrcDir         c                   Const('/gcGroup/srcPDFs')
008000000000     D cOutDir         c                   Const('/gcGroup/outPDFs/')
008100150417     D cFileName       c                   Const('SDGSJ64P')
008200000000     D cApice          c                   Const(x'7d')
008300000000
008400000000     D PgmInfDS       SDS                  NoOpt Qualified                      Program Status DS
008500000000     D  Job_Name                     10a   OverLay(PgmInfDs: 244)               Name
008600000000     D  Job_User                     10a   OverLay(PgmInfDs: 254)               User
008700000000     D  Job_Number                    6s 0 OverLay(PgmInfDs: 264)               Number
008800000000     D  Job_Date                      6s 0 Overlay(PgmInfDs: 270)               Date (UDATE fmt)
008900150318     D*------------
009000150318     D* Passaggio Parametri
009100150318     D KPJBA         E DS
009200150318     D*------------
009300150318     D* Parametri ricevuti
009400150417     D SDGSJ64DS     E DS
009500150318     D*-------------
009600150318     C*
009700150318     C* Reperimento parametri
009800150318     C*
009900150318     C     *entry        plist
010000150318     C                   parm                    kpjba
010100150417     C                   eval      SdgSj64Ds = Kpjbu
010200000000
010300000000      /Free
010400000000
010500000000       // Try to remove old version ...
010600000000         aCmdString = 'RMVLNK OBJLNK(' +
010700000000                      cApice + cOutDir + cFileName + 'o.pdf' + cApice + ')';
010800000000
010900000000         Monitor;
011000000000           OsExeCmd(%Trim(aCmdString): %Len(%Trim(aCmdString)));
011100000000         On-Error;
011200000000           // Return;
011300000000         EndMon;
011400000000
011500000000       // Convert to SCS Printer File in a Sream File in .PDF format
011600000000         aCmdString =
011700150417          'CVTSPLSTMF FROMFILE(SDGSJ64P) TOSTMF(SDGSJ64Pi.pdf)'             +
011800000000                    ' TODIR(' + cApice + cSrcDir + cApice + ') JOB(' +
011900000000                             %Trim(%EditW(PgmInfDS.Job_Number: '0      '))  +
012000000000                       '/' + %Trim(PgmInfDS.Job_User) +
012100000000                       '/' + %Trim(PgmInfDS.Job_Name) + ')' +
012200150422                    ' SPLNBR(*LAST) TOFMT(*PDF) STMFOPT(*REPLACE)' ;
012300000000
012400000000         Monitor;
012500000000           OsExeCmd(%Trim(aCmdString): %Len(%Trim(aCmdString)));
012600000000         On-Error;
012700000000           Return;
012800000000         EndMon;
012900000000
013000000000       // Set up CLASSPATH before starting JVM
013100000000         EnvPtr = GetOsEnv('CLASSPATH');
013200000000
013300000000       // Check for "root"
013400000000         If EnvPtr =  *Null;
013500000000            EnvVal = '.';
013600000000            EnvValChanged = *on;
013700000000         Else;
013800000000            EnvVal = %str(GetOsEnv('CLASSPATH'));
013900000000         Endif;
014000000000
014100000000       // Check for "iText Library"
014200000000         If %Scan('/iText.jar': EnvVal) = *zeros;
014300000000            EnvVal = EnvVal +
014400000000                     ':/gcGroup/Java/iText.jar';
014500000000            EnvValChanged = *on;
014600000000         EndIf;
014700000000
014800000000       // Check for "gcGroupOverLay object"
014900000000         If %Scan('/gcPDFsOverLay.jar': EnvVal) = *zeros;
015000000000            EnvVal = EnvVal +
015100000000                     ':/gcGroup/Java/gcPDFsOverLay.jar';
015200000000            EnvValChanged = *on;
015300000000         EndIf;
015400000000
015500000000       // If set change the local envirorment
015600000000         If EnvValChanged;
015700000000            PutOsEnv('CLASSPATH=' + EnvVal);
015800000000         Endif;
015900000000
016000000000       // Start the JVM
016100000000         pEnv = start_jvm();
016200000000
016300000000       // Get the JVM version
016400000000         iJVMVer = GetVersion(pEnv);
016500000000         sResult = iHigh + (iLow / 100000);
016600000000
016700000000       // Nuova istanza dell'oggetto OverLay
016800000000         oOverLay = cOverLay;
016900000000
017000000000       // Tenta conversione ...
017100150417         sStringSrc = jstr('SDGSJ64Pi.pdf');
017200150318         Select;
017300150417           When TipoStpJ64 = '1';
017400150417            sStringOvl = jstr('DI_modello_1.pdf');
017500150417           When TipoStpJ64 = '2';
017600150417            sStringOvl = jstr('DI_modello_2.pdf');
017700150318         Endsl;
017800150417         sStringOut = jstr('SDGSJ64Po.pdf');
017900000000
018000000000         Monitor;
018100000000           //mPDFsOverlay(oOverLay: sStringSrc: sStringOvl: sStringOut);
018200000000           mPDFsOverlay(sStringSrc: sStringOvl: sStringOut);
018300000000         On-Error;
018400000000           Return;
018500000000         EndMon;
018600000000
018700000000       // Fine Programma ...
018800000000         *InLR =*on;
018900000000         Return ;
019000000000
019100000000      /end-free
019200000000
019300000000      *-----------------------------------------------------------------
019400000000      *  start_jvm():   Start the Java Virtual Machine (JVM)
019500000000      *-----------------------------------------------------------------
019600000000     P start_jvm       B
019700000000     D start_jvm       PI              *
019800000000     D wwStr           s                   like(jString)
019900000000      /free
020000000000         status('Starting Java Virtual Machine...');
020100000000         wwStr = jstr('Temp String');
020200000000         status('.');
020300000000         JNIENV_P = attach_jvm();
020400000000         DeleteLocalRef(JNIENV_P: wwStr);
020500000000         return JNIENV_P;
020600000000      /end-free
020700000000     P                 E
020800000000      *-----------------------------------------------------------------
020900000000      * attach_jvm():  Attach to JVM if it's running
021000000000      *-----------------------------------------------------------------
021100000000     P attach_jvm      B
021200000000     D attach_jvm      PI              *
021300000000     D dsAtt           ds                  likeds(JavaVMAttachArgs)
021400000000     D wwJVM           s                   like(JavaVM_p) dim(1)
021500000000     D wwJVMc          s                   like(jSize)
021600000000     D wwEnv           s               *   inz(*null) static
021700000000     D wwRC            s             10I 0
021800000000      /free
021900000000        if (wwEnv <> *NULL);
022000000000           return wwEnv;
022100000000        endif;
022200000000        status('Attaching RPG program to Java Virtual Machine...');
022300000000        monitor;
022400000000           wwRC = JNI_GetCreatedJavaVMs(wwJVM: 1: wwJVMc);
022500000000
022600000000           if (wwRC <> JNI_OK  or  wwJVMc = 0);
022700000000               return *NULL;
022800000000           endif;
022900000000           JavaVM_P = wwJVM(1);
023000000000           dsAtt = *ALLx'00';
023100000000           dsAtt.version = JNI_VERSION_1_2;
023200000000           wwRC = AttachCurrentThread (wwJVM(1): wwEnv: %addr(dsAtt));
023300000000           if (wwRC <> JNI_OK);
023400000000               wwEnv = *NULL;
023500000000           endif;
023600000000        on-error;
023700000000           wwEnv = *NULL;
023800000000        endmon;
023900000000        status('.');
024000000000        return wwEnv;
024100000000      /end-free
024200000000     P                 E
024300000000      *-----------------------------------------------------------------
024400000000     P status          B                   export
024500000000     D status          PI             1N
024600000000     D   peMsgTxt                   256A   const
024700000000
024800000000     D SndPgmMsg       PR                  ExtPgm('QMHSNDPM')
024900000000     D   MessageID                    7A   Const
025000000000     D   QualMsgF                    20A   Const
025100000000     D   MsgData                    256A   Const
025200000000     D   MsgDtaLen                   10I 0 Const
025300000000     D   MsgType                     10A   Const
025400000000     D   CallStkEnt                  10A   Const
025500000000     D   CallStkCnt                  10I 0 Const
025600000000     D   MessageKey                   4A
025700000000     D   ErrorCode                    1A
025800000000     D dsEC            DS
025900000000     D  dsECBytesP             1      4I 0 inz(256)
026000000000     D  dsECBytesA             5      8I 0 inz(0)
026100000000     D  dsECMsgID              9     15
026200000000     D  dsECReserv            16     16
026300000000     D  dsECMsgDta            17    256
026400000000     D wwMsgLen        S             10I 0
026500000000     D wwTheKey        S              4A
026600000000     c     ' '           checkr    peMsgTxt      wwMsgLen
026700000000     c                   if        wwMsgLen<1
026800000000     c                   return    *OFF
026900000000     c                   endif
027000000000     c                   callp     SndPgmMsg('CPF9897': 'QCPFMSG   *LIBL':
027100000000     c                               peMsgTxt: wwMsgLen: '*STATUS':
027200000000     c                               '*EXT': 0: wwTheKey: dsEC)
027300000000     c                   if        dsECBytesA>0
027400000000     c                   return    *off
027500000000     c                   else
027600000000     c                   return    *on
027700000000     c                   endif
027800000000     P                 E
027900000000
028000000000

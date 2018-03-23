000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200081201     H dftactgrp(*NO) bnddir('UBBNDDIR':'QC2LE')
000300991027
000400081201
000500081201     H*****************************************************************
000600081201     D* PROTOTIPI IFS
000700081201     D*****************************************************************
000800081201     D getErrno        PR              *   EXTPROC('__errno')
000900081201     D errno           s             10I 0 based(errnoP)
001000081201     D errnoP          s               *
001100081201     D*** Apertura file IFS
001200081201     Dopen             PR            10I 0 EXTPROC('open')
001300081201     D  filename                       *   VALUE OPTIONS(*STRING)
001400081201     D  openflags                    10I 0 VALUE
001500081201     D  mode                         10U 0 VALUE OPTIONS(*NOPASS)
001600081201     D  codepage                     10U 0 VALUE OPTIONS(*NOPASS)
001700081201     D  txtcreatid                   10U 0 VALUE OPTIONS(*NOPASS)
001800081201     D*** Lettura file IFS
001900081201     Dread             PR            10I 0 EXTPROC('read')
002000081201     D  filehandle                   10I 0 VALUE
002100081201     D  datareceived                   *   VALUE
002200081201     D  nbytes                       10U 0 VALUE
002300081201     D*** Scrittura file IFS
002400081201     Dwrite            PR            10I 0 EXTPROC('write')
002500081201     D  filehandle                   10I 0 VALUE
002600081201     D  datatowrite                    *   VALUE
002700081201     D  nbytes                       10U 0 VALUE
002800081201     D*** Chiusura file IFS
002900081201     Dclose            PR            10I 0 EXTPROC('close')
003000081201     D  filehandle                   10I 0 VALUE
003100081201     D*****************************************************************
003200081201     D* COSTANTI IFS
003300081201     D*****************************************************************
003400081201     D*** Modalità accesso file per open()
003500081201     D O_RDONLY        S             10I 0 INZ(1)
003600081201     D O_WRONLY        S             10I 0 INZ(2)
003700081201     D O_RDWR          S             10I 0 INZ(4)
003800081201     D*** Flag valori per open()
003900081201     D O_CREAT         S             10I 0 INZ(8)
004000081201     D O_EXCL          S             10I 0 INZ(16)
004100081201     D O_TRUNC         S             10I 0 INZ(64)
004200081201     D*** Flag stato file per open() and fcntl()
004300081201     D O_NONBLOCK      S             10I 0 INZ(128)
004400081201     D O_APPEND        S             10I 0 INZ(256)
004500081201     D*** Flag valori modalità condivisa per open()
004600081201     D O_SHARE_NONE    S             10I 0 INZ(2000000)
004700081201     D O_SHARE_RDONLY  S             10I 0 INZ(0200000)
004800081201     D O_SHARE_RDWR    S             10I 0 INZ(1000000)
004900081201     D O_SHARE_WRONLY  S             10I 0 INZ(0400000)
005000081201     D*** Permessi file
005100081202     D S_IRUSR         S             10I 0 INZ(256)
005200081202     D S_IWUSR         S             10I 0 INZ(128)
005300081202     D S_IXUSR         S             10I 0 INZ(64)
005400081202     D S_IRWXU         S             10I 0 INZ(448)
005500081202     D S_IRGRP         S             10I 0 INZ(32)
005600081202     D S_IWGRP         S             10I 0 INZ(16)
005700081202     D S_IXGRP         S             10I 0 INZ(8)
005800081202     D S_IRWXG         S             10I 0 INZ(56)
005900081202     D S_IROTH         S             10I 0 INZ(4)
006000081202     D S_IWOTH         S             10I 0 INZ(2)
006100081202     D S_IXOTH         S             10I 0 INZ(1)
006200081202     D S_IRWXO         S             10I 0 INZ(7)
006300081202     D S_RDWR          S             10I 0 INZ(438)
006400081202     D S_RDWREX        S             10I 0 INZ(510)
006500081201     D*** Varie
006600081202     D O_CCSID_CURJOB  S             10I 0 INZ(0)
006700081202     D O_CCSID         S             10I 0 INZ(32)
006800081201     D O_TEXTDATA      S             10I 0 INZ(16777216)
006900081201     D O_TEXT_CREAT    S             10I 0 INZ(33554432)
007000081201     D*** Varie "correnti"
007100081201     D C_FLAG          S             10I 0 INZ
007200081202     D C_CCSID         S             10I 0 INZ
007300081201     D C_MODE          S             10I 0 INZ
007400081202     D*** Valori specifici
007500081202     D CCSID_UTF8      S             10I 0 INZ(1208)
007600081202     D CRLF            S              2A   INZ(X'0D25')
007700081202     D Null            S              1A   INZ(X'00')
007800081201     D*****************************************************************
007900081201     D* DEFINIZIONI DEI DATI
008000081201     D*****************************************************************
008100081201     D***
008200081201     D FileDesc        S             10I 0
008300081201     D FullNameFile    s           1024    VARYING
008400110620     D depBUFFER       s          32766
008500081201     D BufferOut       s          32768    VARYING
008600081201     D BufferLen       s             10I 0
008700081201     D wrkesito        s              1A   INZ
008800081205     D sSQL            s           1024    VARYING
008900120724     D wIdx            s              4  0 INZ
009000081201
009100081201
009200081201     D/COPY QSYSINC/QRPGLESRC,QUSEC
009300081201     D/COPY GAITRASRC/SRCPROTOPR,UBUTISQL
009400071127
009500991027
009600991027
009700081201     C*
009800081201     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
009900081201     C
010000081201     C/EXEC SQL
010100081201     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
010200081201     C/END-EXEC
010300081201     C
010400170824     C*
010500170824     C                   exsr      dropALIAS
010600081202     C*
010700081202     C* Inizializzo indicatore d stato avanzamento flow
010800081202     C                   setoff                                       70
010900071205     C*
011000081202     C  N70              exsr      makeALIAS
011100080219     C*
011200081202     C  N70              exsr      readALIAS
011300080219     C*
011400081202     C  N70              exsr      dropALIAS
011500081202     C*
011600081202     C                   if        *IN70
011700081202     C                   eval      wrkesito = '2'
011800081202     C                   endif
011900071128     C*
012000081201     C                   seton                                        LR
012100071205
012200071205
012300071205
012400081201     C**************
012500081201      /TITLE Eccezioni a run-time
012600081201     C     *pssr         BEGSR
012700071205     C*
012800081201     C                   eval      wrkesito = '2'
012900081201     C                   exsr      dropALIAS
013000071205     C*
013100081201     C                   ENDSR
013200081201     C**************
013300071204
013400071204
013500071128
013600071128
013700081201     C**************
013800081201     C     makeALIAS     BEGSR
013900071128     C*
014000081205     C                   if        SQL_CreaAlias(InAlias
014100071204     C                                          :'QTEMP'
014200081201     C                                          :InFile
014300081201     C                                          :InLib
014400081201     C                                          :InMbr)
014500071205     C                                          = 0
014600071204     C                   else
014700081202     C                   seton                                        70
014800071203     C                   endif
014900071128     C*
015000071128     C                   ENDSR
015100081201     C**************
015200071204
015300071204
015400071204
015500071204
015600081201     C**************
015700081201     C     dropALIAS     BEGSR
015800071204     C*
015900170824     C                   callp(e)  SQL_DropAlias(InAlias:'QTEMP')
016000071204     C*
016100071204     C                   ENDSR
016200081201     C**************
016300071204
016400071128
016500071128
016600071128
016700081201     C**************
016800081201     C     readALIAS     BEGSR
016900081205     C*
017000110613     C                   if        InFldSelect = *blanks
017100110613     C                   eval      InFldSelect = '*'
017200110613     C                   endif
017300110613     C*
017400110613     C                   eval      sSQL = 'select ' + InFldSelect +
017500170824     C                                    ' from ' + InAlias +
017600170824     C                                    ' order by rrn('+%trim(InAlias)+') ' +
017700170824     C                                    ' for read only'
017800081205     C*
017900081205     C/EXEC SQL
018000081205     C+ prepare S1 from :sSQL
018100081205     C/END-EXEC
018200081205     C
018300081205     C
018400081205     C/EXEC SQL
018500081205     C+ declare C1 cursor for S1
018600081205     C/END-EXEC
018700081205     C
018800081201     C
018900081201     C/EXEC SQL
019000081201     C+ open C1
019100081201     C/END-EXEC
019200081201     C
019300081201     C                   if        sqlcod >= *zeros
019400081201     C*
019500081201     C* Creazione file in IFS
019600081201     C                   exsr      createfileIFS
019700081202     C*
019800081202     C                   if        not *IN70
019900081201     C
020000081201     C/EXEC SQL
020100110613     C+ Fetch C1 into :depBUFFER
020200081201     C/END-EXEC
020300081201     C*
020400081201     C                   dow       sqlcod = *zeros
020500081201     C*
020600081201     C* Scrivo il file nell'IFS
020700081201     C                   exsr      writefileIFS
020800081201     C*
020900081201     C/EXEC SQL
021000110613     C+ Fetch C1 into :depBUFFER
021100081201     C/END-EXEC
021200081201     C*
021300081201     C                   enddo
021400081201     C*
021500081201     C* Chiudo il file nell'IFS
021600081201     C                   exsr      closefileIFS
021700081202     C*
021800081202     C                   endif
021900081201     C*
022000081201     C/EXEC SQL
022100081201     C+ close C1
022200081201     C/END-EXEC
022300081201     C*
022400081201     C                   endif
022500071128     C*
022600071128     C                   ENDSR
022700081201     C**************
022800071127
022900081201
023000081201
023100081201
023200081201     C**************
023300081201     C     createfileIFS BEGSR
023400081201     C*
023500081201     C
023600081201      /free
023700081205        C_FLAG   = O_CREAT + O_WRONLY + O_CCSID + O_TEXTDATA + O_TEXT_CREAT;
023800081202        C_MODE   = S_RDWREX;
023900081201
024000081201
024100081201        FullNameFile = %trim(InPathIFS) + %trim(InFileIFS) +
024200081201                       %trim(InExtIFS) + Null;
024300081201
024400081201
024500081201        FileDesc = open(  FullNameFile
024600081201                        : C_FLAG
024700081201                        : C_MODE
024800081202                        : C_CCSID
024900081202                        : O_CCSID_CURJOB     );
025000081201
025100081201
025200081202        if (FileDesc <> 0);
025300081202           errnoP = getErrno();
025400081202        endif;
025500081201        if (FileDesc = -1);
025600081202           *in70 = *on;
025700081201        endif;
025800081201
025900081202
026000081201      /end-free
026100081201     C
026200081201     C*
026300081201     C                   ENDSR
026400081201     C**************
026500081201
026600081201
026700081201
026800081201
026900081201     C**************
027000081201     C     closefileIFS  BEGSR
027100081201     C*
027200081201     C
027300081201      /free
027400120724         wIdx = 1;
027500120724         dow wIdx < 5000;
027600120724           if (close(FileDesc) < 0);
027700120724           else;
027800120724             leave;
027900120724           endif;
028000120724           wIdx = wIdx + 1;
028100120724         enddo;
028200120724         if wIdx >= 5000;
028300120724            *in70 = *on;
028400120724         endif;
028500081201      /end-free
028600081201     C
028700081201     C*
028800081201     C                   ENDSR
028900081201     C**************
029000081201
029100081201
029200081201
029300081201
029400081201
029500081201     C**************
029600081201     C     writefileIFS  BEGSR
029700081201     C*
029800081201     C
029900081201      /free
030000081201
030100110613         BufferOut = %subst(depBUFFER:1:InLung);
030200110620         if InOutTrim = 'Y';
030300110620            BufferOut = %trimr(BufferOut);
030400110620            BufferLen = %len(BufferOut);
030500110620         else;
030600110620            BufferLen = InLung;
030700110620         endif;
030800081201
030900081201         if InEndLine = *blanks OR InEndLine = '*FIXED';
031000110620         elseif InEndLine = '*CRLF';
031100081201            BufferOut = BufferOut + CRLF;
031200081201            BufferLen = BufferLen + 2;
031300081201         endif;
031400081201
031500120724         if %len(BufferOut) > 0;
031600120724            if (write(FileDesc:%addr(BufferOut)+2:BufferLen) < 1);
031700120724               *in70 = *on;
031800120724            endif;
031900120724         endif;
032000081201
032100081201      /end-free
032200081201     C                   ENDSR
032300081201     C**************
032400081201
032500081201
032600081201
032700081201
032800991027
032900991027      /TITLE Operazioni iniziali.
033000991027     C     *inzsr        BEGSR
033100991027     C*
033200991027     C     *ENTRY        PLIST
033300081201     C                   parm                    InLib            10
033400081201     C                   parm                    InFile           10
033500081201     C                   parm                    InMbr            10
033600110620     C                   parm                    InFldSelect      20
033700081205     C                   parm                    InAlias          22
033800081201     C                   parm                    InPathIFS       255
033900081201     C                   parm                    InFileIFS       255
034000081201     C                   parm                    InExtIFS        255
034100081201     C                   parm                    InLung            5 0
034200110620     C                   parm                    InOutTrim         1
034300081201     C                   parm                    InEndLine         6
034400081201     C                   parm                    InCCSID           5 0
034500081201     C     wrkesito      parm      wrkesito      Outesito          1
034600081201     C*
034700081201     C* Verifica parametri
034800081201     C                   if        InCCSID = *zeros
034900110620     C                   eval      C_CCSID = CCSID_UTF8
035000081201     C                   else
035100081202     C                   eval      C_CCSID = InCCSID
035200081201     C                   endif
035300081201     C*
035400081201     C                   if        InLib = '*LIBL'
035500081201     C                   eval      InLIb = *blanks
035600081201     C                   endif
035700071128     C*
035800081201     C                   ENDSR

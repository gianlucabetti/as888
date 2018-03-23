000100080729      **-------------------------------------------------------------**
000200161014      ** UBIFSPR IFS - Prototipi di base                             **
000300161014      **-------------------------------------------------------------**
000400161014
000500161014     D*--------------------------------------------------------------------
000600161014     D* Creazione nome file (temporaneo) univoco assoluto in tutto il sistema
000700161014     D*--------------------------------------------------------------------
000800161014     D IFS_CreateTempFileName...
000900161014     D                 PR              *   EXTPROC('_C_IFS_tmpnam')
001000161014     D  fileName                     39A   OPTIONS(*OMIT)
001100161014     D*--------------------------------------------------------------------
001200161014
001300161014     D*--------------------------------------------------------------------
001400161014     D* Check file IFS
001500161014     D*--------------------------------------------------------------------
001600161014     D IFS_CheckFile...
001700161014     D                 PR            10I 0 EXTPROC('access')
001800161014     D  pathFileName                   *   VALUE OPTIONS(*STRING)
001900161014     D  accessMode                   10I 0 VALUE
002000161014     D*--------------------------------------------------------------------
002100161014
002200161014     D*--------------------------------------------------------------------
002300161014     D* Apertura file IFS
002400161014     D*--------------------------------------------------------------------
002500161014     D IFS_OpenFile...
002600161014     D                 PR            10I 0 EXTPROC('open')
002700161014     D  pathFileName                   *   VALUE OPTIONS(*STRING)
002800161014     D  openFlags                    10I 0 VALUE
002900161014     D  mode                         10U 0 VALUE OPTIONS(*NOPASS)
003000161014     D  ccsid                        10U 0 VALUE OPTIONS(*NOPASS)
003100161014     D  txtCreatId                   10U 0 VALUE OPTIONS(*NOPASS)
003200161014     D*--------------------------------------------------------------------
003300161014
003400161014     D*--------------------------------------------------------------------
003500161014     D* Lettura file IFS
003600161014     D*--------------------------------------------------------------------
003700161014     D IFS_ReadFile...
003800161014     D                 PR            10I 0 EXTPROC('read')
003900161014     D  fileDescr                    10I 0 VALUE
004000161014     D  dataReceived                   *   VALUE
004100161014     D  nBytes                       10U 0 VALUE
004200161014     D*--------------------------------------------------------------------
004300161014
004400161014     D*--------------------------------------------------------------------
004500161014     D* Scrittura file IFS
004600161014     D*--------------------------------------------------------------------
004700161014     D IFS_WriteFile...
004800161014     D                 PR            10I 0 EXTPROC('write')
004900161014     D  fileDescr                    10I 0 VALUE
005000161014     D  dataToWrite                    *   VALUE
005100161014     D  nBytes                       10U 0 VALUE
005200161014     D*--------------------------------------------------------------------
005300161014
005400161014     D*--------------------------------------------------------------------
005500161014     D* Chiusura file IFS
005600161014     D*--------------------------------------------------------------------
005700161014     D IFS_CloseFile...
005800161014     D                 PR            10I 0 EXTPROC('close')
005900161014     D  fileDescr                    10I 0 VALUE
006000161014     D*--------------------------------------------------------------------
006100161014
006200161014     D*--------------------------------------------------------------------
006300161014     D* Set File Read/Write Offset
006400161014     D*--------------------------------------------------------------------
006500161014     D IFS_SetFileOffset...
006600161014     D                 PR            10I 0 EXTPROC('lseek')
006700161014     D FilDes                        10I 0 VALUE
006800161014     D OffSet                        10I 0 VALUE
006900161014     D Whence                        10I 0 VALUE
007000161014     D*--------------------------------------------------------------------
007100161014
007200161014     D*--------------------------------------------------------------------
007300161014     D* Rimuove file in IFS
007400161014     D*--------------------------------------------------------------------
007500161014     D IFS_RemoveFile...
007600161014     D                 PR            10I 0 EXTPROC('unlink')
007700161014     D  pathFileName                   *   VALUE OPTIONS(*STRING)
007800161014     D*--------------------------------------------------------------------
007900161014
008000161014     D*--------------------------------------------------------------------
008100161014     D* Rinomina un file o una directory in IFS in REPLACE (non usare)
008200161014     D*--------------------------------------------------------------------
008300161014     D IFS_RenameFileOrDirUnlink...
008400161014     D                 PR            10I 0 EXTPROC('Qp0lRenameUnlink')
008500161014     D  oldName                        *   VALUE OPTIONS(*STRING)
008600161014     D  newName                        *   VALUE OPTIONS(*STRING)
008700161014     D*--------------------------------------------------------------------
008800161014
008900161014     D*--------------------------------------------------------------------
009000161014     D* Rinomina un file o una directory in IFS
009100161014     D*--------------------------------------------------------------------
009200161014     D IFS_RenameFileOrDirKeep...
009300161014     D                 PR            10I 0 EXTPROC('Qp0lRenameKeep')
009400161014     D  oldName                        *   VALUE OPTIONS(*STRING)
009500161014     D  newName                        *   VALUE OPTIONS(*STRING)
009600161014     D*--------------------------------------------------------------------
009700161014
009800161014     D*--------------------------------------------------------------------
009900161014     D* Reperimento informazioni file IFS
010000161014     D*--------------------------------------------------------------------
010100161014     D IFS_GetFileInfo...
010200161014     D                 PR            10I 0 EXTPROC('lstat')
010300161014     D  pathFileName                   *   VALUE OPTIONS(*STRING)
010400161014     D  buffer                         *
010500161014     D*--------------------------------------------------------------------
010600161014
010700161014     D*--------------------------------------------------------------------
010800161014     D* Cambia directory IFS
010900161014     D*--------------------------------------------------------------------
011000161014     D IFS_ChangeDir...
011100161014     D                 PR            10I 0 EXTPROC('chdir')
011200161014     D  path                           *   VALUE OPTIONS(*STRING)
011300161014     D*--------------------------------------------------------------------
011400161014
011500161014     D*--------------------------------------------------------------------
011600161014     D* Apre directory IFS
011700161014     D*--------------------------------------------------------------------
011800161014     D IFS_OpenDir...
011900161014     D                 PR              *   EXTPROC('opendir')
012000161014     D  dirName                        *   VALUE OPTIONS(*STRING)
012100161014     D*--------------------------------------------------------------------
012200161014
012300161014     D*--------------------------------------------------------------------
012400161014     D* Legge una directory
012500161014     D*--------------------------------------------------------------------
012600161014     D IFS_ReadDir...
012700161014     D                 PR              *   EXTPROC('readdir')
012800161014     D  dirPtr                         *   VALUE
012900161014     D*--------------------------------------------------------------------
013000161014
013100161014     D*--------------------------------------------------------------------
013200161014     D* Ri-Legge una directory
013300161014     D*--------------------------------------------------------------------
013400161014     D IFS_RewindDir...
013500161014     D                 PR                  EXTPROC('rewinddir')
013600161014     D  dirPtr                         *   VALUE
013700161014     D*--------------------------------------------------------------------
013800161014
013900161014     D*--------------------------------------------------------------------
014000161014     D* Crea directory IFS
014100161014     D*--------------------------------------------------------------------
014200161014     D IFS_MakeDir...
014300161014     D                 PR            10I 0 EXTPROC('mkdir')
014400161014     D  path                           *   VALUE OPTIONS(*STRING)
014500161014     D  mode                         10U 0 VALUE
014600161014     D*--------------------------------------------------------------------
014700161014
014800161014     D*--------------------------------------------------------------------
014900161014     D* Elimina directory IFS
015000161014     D*--------------------------------------------------------------------
015100161014     D IFS_DelDir...
015200161014     D                 PR            10I 0 EXTPROC('rmdir')
015300161014     D  path                           *   VALUE OPTIONS(*STRING)
015400161014     D*--------------------------------------------------------------------
015500161014
015600161014     D*--------------------------------------------------------------------
015700161014     D* Chiude una directory
015800161014     D*--------------------------------------------------------------------
015900161014     D IFS_CloseDir...
016000161014     D                 PR            10I 0 EXTPROC('closedir')
016100161014     D  dirPtr                         *   VALUE
016200161014     D*--------------------------------------------------------------------
016300161014
016400161014
016500161014     D*****************************************************************
016600161014     D* COSTANTI IFS
016700161014     D*****************************************************************
016800161014
016900161014     D*** Modalità accesso file per open()
017000161014     D O_RDONLY        S             10I 0 INZ(1)                               Read only
017100161014     D O_WRONLY        S             10I 0 INZ(2)                               Write only
017200161014     D O_RDWR          S             10I 0 INZ(4)                               Read & Write
017300161014     D O_CREAT         S             10I 0 INZ(8)                               Create file if it
017400161014     D                                                                          does not exist.
017500161014     D O_EXCL          S             10I 0 INZ(16)                              Exclusively create.
017600161014     D                                                                          Open fails if file
017700161014     D                                                                          already exists.
017800161014     D O_TRUNC         S             10I 0 INZ(64)                              Truncate file to 0
017900161014     D                                                                          byte.
018000161014     D O_NONBLOCK      S             10I 0 INZ(128)                             Open without lock
018100161014     D O_APPEND        S             10I 0 INZ(256)                             Append to file
018200161014
018300161014     D*** Modalità accesso file per open()
018400161014     D O_SYNC          S             10I 0 INZ(1024)                            Synchonous write
018500161014     D
018600161014     D O_DSYNC         S             10I 0 INZ(2048)                            Synchonous write,
018700161014     D                                                                          data only.
018800161014     D O_RSYNC         S             10I 0 INZ(4096)                            Synchonous read
018900161014     D
019000161014     D O_NOCTTY        S             10I 0 INZ(32768)                           No controlling
019100161014     D                                                                          terminal.
019200161014     D O_INHERITMODE   S             10I 0 INZ(134217728)                       Inherit authorities
019300161014     D                                                                          from parent directoy
019400161014
019500161014     D*** Flag valori modalità condivisa per open()
019600161014     D O_SHARE_RDONLY  S             10I 0 INZ(65536)                           Share with readers
019700161014     D                                                                          only.
019800161014     D O_SHARE_WRONLY  S             10I 0 INZ(131072)                          Share with writers
019900161014     D                                                                          only.
020000161014     D O_SHARE_RDWR    S             10I 0 INZ(262144)                          Share with read &
020100161014     D                                                                          write.
020200161014     D O_SHARE_NONE    S             10I 0 INZ(524288)                          Share with nobody
020300161014     D
020400161014
020500161014     D*** Flag valori per "CCSID" e "CODEPAGE" per open()
020600161014     D O_CCSID_CURJOB  S             10I 0 INZ(0)
020700161014     D O_CCSID         S             10I 0 INZ(32)
020800161014     D O_CODEPAGE      S             10I 0 INZ(8388608)
020900161014     D O_TEXTDATA      S             10I 0 INZ(16777216)
021000161014     D O_TEXT_CREAT    S             10I 0 INZ(33554432)
021100161014     D O_INHERITMODE   S             10I 0 INZ(134217728)
021200161014     D O_LARGEFILE     S             10I 0 INZ(536870912)
021300161014     D*
021400161014     D CP_CURJOB       S             10I 0 INZ(0)
021500161014     D CP_MSDOS        S             10I 0 INZ(437)
021600161014     D CP_ASCII        S             10I 0 INZ(819)
021700161014     D CP_ISO8859_1    S             10I 0 INZ(819)
021800161014     D CP_WINDOWS      S             10I 0 INZ(1252)
021900161014     D CP_UTF8         S             10I 0 INZ(1208)
022000161014     D CP_UCS2         S             10I 0 INZ(1200)
022100161014
022200161014     D*** Accesso file
022300161014     D M_RDONLY        S             10I 0 INZ(292)
022400161014     D M_RDWR          S             10I 0 INZ(438)
022500161014     D M_RDWREX        S             10I 0 INZ(510)
022600161014     D M_RWX           S             10I 0 INZ(511)
022700161014
022800161014     D*** Permessi file - owner authority
022900161014     D S_IRUSR         S             10I 0 INZ(256)
023000161014     D S_IWUSR         S             10I 0 INZ(128)
023100161014     D S_IXUSR         S             10I 0 INZ(64)
023200161014     D S_IRWXU         S             10I 0 INZ(448)
023300161014
023400161014     D*** Permessi file - group authority
023500161014     D S_IRGRP         S             10I 0 INZ(32)
023600161014     D S_IWGRP         S             10I 0 INZ(16)
023700161014     D S_IXGRP         S             10I 0 INZ(8)
023800161014     D S_IRWXG         S             10I 0 INZ(56)
023900161014
024000161014     D*** Permessi file - other people
024100161014     D S_IROTH         S             10I 0 INZ(4)
024200161014     D S_IWOTH         S             10I 0 INZ(2)
024300161014     D S_IXOTH         S             10I 0 INZ(1)
024400161014     D S_IRWXO         S             10I 0 INZ(7)
024500161014
024600161014     D*** Flag valori per parametro "WHENCE" di lseek()
024700161014     D SEEK_SET        S             10I 0 INZ(0)                               Start of the file
024800161014     D SEEK_CUR        S             10I 0 INZ(1)                               Current file offset
024900161014     D SEEK_END        S             10I 0 INZ(2)                               End of the file
025000161014
025100161014     D*** Variabili varie di work
025200161014     D C_FLAG          S             10I 0 INZ
025300161014     D C_CCSID         S             10I 0 INZ
025400161014     D C_MODE          S             10I 0 INZ
025500161014
025600161014     D*** Valori ricorrenti
025700161014     D CRLF            S              2A   INZ(X'0D25')
025800161014     D Null            S              1A   INZ(X'00')
025900161014
026000161014     D*** Flag valori per access()
026100161014     D F_OK            S             10I 0 INZ(0)                               File exists
026200161014     D R_OK            S             10I 0 INZ(4)                               Read access
026300161014     D W_OK            S             10I 0 INZ(2)                               Write access
026400161014     D X_OK            S             10I 0 INZ(1)                               Execute or search
026500161014
026600161014
026700161014     D*** Flag valori per access()
026800161014     D filaddr         S               *
026900161014     D dirent          DS                  based(filaddr)
027000161014     D   d_reserv1                   16A
027100161014     D   d_reserv2                   10U 0
027200161014     D   d_fileno                    10U 0
027300161014     D   d_reclen                    10U 0
027400161014     D   d_reserv3                   10I 0
027500161014     D   d_reserv4                    8A
027600161014     D   d_nlsinfo                   12A
027700161014     D     nls_ccsid                 10I 0 OVERLAY(d_nlsinfo:1)
027800161014     D     nls_cntry                  2A   OVERLAY(d_nlsinfo:5)
027900161014     D     nls_lang                   3A   OVERLAY(d_nlsinfo:7)
028000161014     D     nls_reserv                 3A   OVERLAY(d_nlsinfo:10)
028100161014     D   d_namelen                   10U 0
028200161014     D   d_name                     640A
028300161014
028400161014     D p_statds        S               *
028500161014     D statds          DS                  BASED(p_statds)
028600161014     D  st_mode                      10U 0                                      File mode
028700161014     D  st_ino                       10U 0                                      File serial number
028800161014     D  st_nlink                      5U 0                                      Number of links
028900161014     D  st_reserved2                  5U 0
029000161014     D  st_uid                       10U 0 OVERLAY(statds:13)                   User ID of the owner
029100161014     D                                                                          of file
029200161014     D  st_gid                       10U 0                                      Group ID of the
029300161014     D                                                                          group of file
029400161014     D  st_size                      10I 0                                      For regular files,
029500161014     D                                                                          file size in byte
029600161014     D  st_atime                     10I 0                                      Time of last access
029700161014     D  st_mtime                     10I 0                                      Time of last data
029800161014     D                                                                          modification
029900161014     D  st_ctime                     10I 0                                      Time of last file
030000161014     D                                                                          status change
030100161014     D  st_dev                       10U 0                                      ID of device
030200161014     D                                                                          containing file
030300161014     D  st_blksize                   10U 0                                      Size of a block of
030400161014     D                                                                          the file
030500161014     D  st_allocsize                 10U 0                                      Allocation size of
030600161014     D                                                                          the file
030700161014     D  st_objtype                   11A                                        AS/400 object type
030800161014     D  st_reserved3                  1A
030900161014     D  st_codepage                   5U 0                                      Object data CodePage
031000161014     D  st_ccsid                      5U 0                                      Object data CCSID
031100161014     D  st_rdev                      10U 0                                      Device ID if special
031200161014     D  st_nlink32                   10U 0                                      Number of links to
031300161014     D                                                                          the file
031400161014     D  st_rdev64                    20U 0                                      Dev ID 64bit if spec
031500161014     D  st_dev64                     20U 0                                      File system ID 64bit
031600161014     D  st_reserved1                 36A                                        Reserved
031700161014     D  st_ino_gen_id                10U 0                                      File generation ID

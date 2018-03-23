000100010104      *BIND
000200010104
000300010104     H*------------------------------------------------------------------------*
000400010104     H* Cancellazione vecchi file dall'IFS
000500010104     H*------------------------------------------------------------------------*
000600010104     H NOMAIN
000700010104
000800010104     H bnddir('QC2LE')
000900001123
001000100316     Fwas7ifs0f O    E             DISK    USROPN
001100001123     F
001200001123
001300010104
001400010104
001500010104     D*--------------------------------------------------------------------
001600010104     D* Passaggio parametri tra medesima sub-procedure
001700010104     D*--------------------------------------------------------------------
001800080910     D TIS783R2        PR
001900080910     D PATHTOLIST                  1024
002000080910     D PATHDEST                    1024
002100080910     D KEY                           30
002200080910     D OPZ                            4
002300010104     D NUMGIOVAL                      3  0
002400140709     D TYP                            3
002500140708     D LIVELLO                        1  0
002600010104     D*--------------------------------------------------------------------
002700010104
002800010104
002900010104     P*--------------------------------------------------------------------
003000010104     P* Inizio dichiarazione sub-procedure
003100010104     P*--------------------------------------------------------------------
003200080910     P TIS783R2        b
003300010104     P                                     export
003400010104
003500010104     D*--------------------------------------------------------------------
003600010104     D* Passaggio parametri tra medesima sub-procedure
003700010104     D*--------------------------------------------------------------------
003800080910     D TIS783R2        PI
003900080910     D PATHTOLIST                  1024
004000080910     D PATHDEST                    1024
004100080910     D KEY                           30
004200080910     D OPZ                            4
004300080910     D NUMGIOVAL                      3  0
004400140709     D TYP                            3
004500140708     D LIVELLO                        1  0
004600010104     D*--------------------------------------------------------------------
004700010104
004800010104
004900001123     D*--------------------------------------------------------------------
005000001123     D* Apre una directory
005100001123     D*
005200001123     D* opendir(string af a dirname) ==> return memory *address of the directory
005300001123     D*--------------------------------------------------------------------
005400001123     D opendir         PR              *   EXTPROC('opendir')
005500001123     D                                 *   VALUE
005600001123     D*--------------------------------------------------------------------
005700001123
005800010108     D*--------------------------------------------------------------------
005900010108     D* Chiude una directory
006000010108     D*
006100010109     D* closedir(string af a dirname) ==> return a value of operation's result
006200010108     D*--------------------------------------------------------------------
006300010109     D closedir        PR            10I 0 EXTPROC('closedir')
006400010108     D                                 *   VALUE
006500010108     D*--------------------------------------------------------------------
006600010108
006700001123     D*--------------------------------------------------------------------
006800001123     D* Legge una directory
006900001123     D*
007000001123     D* readdir(*address of a dirname) ==> return list main file attribute into a dsinfo
007100001123     D*--------------------------------------------------------------------
007200001123     D readdir         PR              *   EXTPROC('readdir')
007300001123     D                                 *   VALUE
007400001123     D*--------------------------------------------------------------------
007500001123
007600001123     D*--------------------------------------------------------------------
007700001123     D* Attributi di un file
007800001123     D*
007900001123     D* stat(*address of a pathfilename, *address of memory area into a dsinfo)
008000001123     D*--------------------------------------------------------------------
008100001123     D stat            PR            10i 0 EXTPROC('stat')
008200001123     D                                 *   VALUE
008300001123     D                                 *   VALUE
008400001123     D*--------------------------------------------------------------------
008500001123
008600001123
008700001123     D filaddr         S               *
008800001123     D dirent          DS                  based(filaddr)
008900001122     D   d_reserv1                   16A
009000001122     D   d_reserv2                   10U 0
009100001122     D   d_fileno                    10U 0
009200001122     D   d_reclen                    10U 0
009300001122     D   d_reserv3                   10I 0
009400001122     D   d_reserv4                    8A
009500001122     D   d_nlsinfo                   12A
009600001122     D     nls_ccsid                 10I 0 OVERLAY(d_nlsinfo:1)
009700001122     D     nls_cntry                  2A   OVERLAY(d_nlsinfo:5)
009800001122     D     nls_lang                   3A   OVERLAY(d_nlsinfo:7)
009900001122     D     nls_reserv                 3A   OVERLAY(d_nlsinfo:10)
010000001122     D   d_namelen                   10U 0
010100001122     D   d_name                     640A
010200001123
010300001123
010400001123     Dinfo             DS                  inz
010500001123     D st_mode                       10U 0
010600001123     D st_ino                        10U 0
010700001123     D st_nlink                       5U 0
010800001123     D st_uid                        10U 0 OVERLAY(info:13)
010900001123     D st_gid                        10U 0
011000001123     D st_size                       10U 0
011100001123     D st_atime                      10U 0
011200001123     D st_mtime                      10U 0
011300001123     D st_ctime                      10U 0
011400001123     D st_dev                        10U 0
011500001123     D st_blksize                    10U 0
011600001123     D st_allocsize                  10U 0
011700001123     D st_objtype                    10A
011800001123     D st_codepage                    5U 0
011900001123     D st_reserved1                  61A
012000001123     D st_ino_gen_id                 10U 0
012100001123
012200001123
012300090324     D dataIso         S               D   datfmt(*iso)
012400090324     D dataEur         S               D   datfmt(*eur)
012500140709
012600140709
012700140709     DDS_ifsfl1        DS                  inz
012800140709     D §ifsfl1_liv                    1A
012900001123
013000001123
013100001123     D*-------------------
013200001123     D* VARIABILI DI WRK
013300001123     D*-------------------
013400080910     D PathName        S           1024A   INZ(*blanks)
013500080910     D RetVal          S             10i 0 INZ
013600080910     D diraddr         S               *
013700080910     D WrkPathName     S           1024A   INZ(*blanks)
013800080910     D FileName        S           1024A   INZ(*blanks)
013900080910     D PathFileName    S           1024A   INZ(*blanks)
014000001123     D depdata         S              8  0 INZ(*zeros)
014100001123     D dataFile        S               D   DATFMT(*ISO)
014200001123     D wrkdataiso      S               D   DATFMT(*ISO)
014300001123     D wrkdata         S              8  0 INZ(*zeros)
014400001123     D datcor          S              8  0 INZ(*zeros)
014500001123     D wrkgg           S             10  0 INZ(*zeros)
014600001123     D EPOCH           S               D   DATFMT(*ISO) INZ(D'1970-01-01')
014700010109     D rv              S             10I 0
014800140708     D Livello_Call    S                   LIKE(LIVELLO) INZ(*zeros)
014900000000
015000010104
015100010104
015200001123     C*--------------------------------------------------------------------
015300001123     C*  MAIN
015400001123     C*--------------------------------------------------------------------
015500010104     C* Eseguo routine iniziale
015600010104     C                   exsr      inzsr
015700140708     C* Verifico e gestisco backslash finale
015800140708     C                   eval      PATHTOLIST = %trim(PATHTOLIST)
015900140708     C                   if        PATHTOLIST <> *blanks AND
016000140708     C                             %subst(PATHTOLIST:
016100140708     C                                    %len(%trim(PATHTOLIST)):1) = '/'
016200140708     C                   eval      %subst(PATHTOLIST:
016300140708     C                                    %len(%trim(PATHTOLIST)):1) = ' '
016400140708     C                   endif
016500001123     C* Apro la directory...
016600001212     C                   eval      PathName= %trim(PATHTOLIST) + x'00'
016700001123     C                   eval      diraddr = opendir(%addr(PathName))
016800001123     C                   if        diraddr = *NULL
016900001213     C                   z-add     0             a                 1 0
017000001212     C*                  eval      Msg = 'Impossibile aprire directory'
017100001212     C*                  dsply                   Msg              50
017200001123     C                   else
017300001123     C* Legge in ciclo gli elementi peresenti nella directory
017400001123     C                   eval      filaddr = readdir(diraddr)
017500001123     C                   dow       filaddr <> *NULL
017600001123     C                   eval      FileName = %subst(d_name:1:d_namelen)
017700001123     C* Reperisco gli attributi dell'elemento corrente
017800001212     C                   eval      PathFileName=%trim(PATHTOLIST)+'/'+
017900001212     C                                          %trim(FileName)  + x'00'
018000001123     C                   eval      RetVal=stat(%addr(PathFileName):%addr(info))
018100001123     C                   if        RetVal = *zeros
018200001212     C* Verifico se trattasi di elemento di tipo "FILE" o di tipo "DIR" (e comunque non di sys)
018300001212     C                   if        %trim(FileName) <> '.'    and
018400010109     C                             %trim(FileName) <> '..'   and
018500010109     C                             %trim(FileName) <> 'updflg'
018600140709     C*
018700140709     C                   select
018800140709     C                   when      st_objtype = '*DIR      ' AND
018900140709     C                             TYP <> 'FLR'
019000140709     C                   eval      WrkPathName=%trim(PATHTOLIST)+'/'+
019100140709     C                                         %trim(FileName)
019200140709     C* Qiundi richiamo me stesso ricorsivamente
019300140709     C                   eval      Livello_Call = 2
019400140709     C                   callp     TIS783R2(WrkPathName:
019500140709     C                                      PATHDEST:
019600140709     C                                      KEY:
019700140709     C                                      OPZ:
019800140709     C                                      NumGioVal:
019900140709     C                                      TYP:
020000140709     C                                      Livello_Call)
020100140709     C*
020200140709     C                   when      st_objtype = '*DIR      ' AND
020300140709     C                             TYP  = 'FLR'
020400140709     C                   exsr      caldat
020500140709     C                   eval      WrkPathName=%trim(PATHTOLIST)+'/'+
020600140709     C                                         %trim(FileName)
020700140709     C* Qiundi richiamo me stesso ricorsivamente
020800140709     C                   eval      Livello_Call = 2
020900140709     C                   callp     TIS783R2(WrkPathName:
021000140709     C                                      PATHDEST:
021100140709     C                                      KEY:
021200140709     C                                      OPZ:
021300140709     C                                      NumGioVal:
021400140709     C                                      TYP:
021500140709     C                                      Livello_Call)
021600140709     C*
021700140709     C                   when      st_objtype <> '*DIR      ' AND
021800140709     C                             TYP  = 'FLR'
021900140709     C*
022000140709     C                   when      st_objtype <> '*DIR      ' AND
022100140709     C                             TYP <> 'FLR'
022200140709     C                   exsr      caldat
022300140709     C*
022400140709     C                   endsl
022500140709     C*
022600001123     C                   endif
022700001212     C                   endif
022800001123     C                   eval      filaddr = readdir(diraddr)
022900001122     C                   enddo
023000010109     C                   eval      rv = closedir(diraddr)
023100001123     C                   endif
023200010104     C* Chiudo il wrk-file...
023300100316     C                   if        %open(was7ifs0f)
023400100316     C                   close     was7ifs0f
023500010104     C                   endif
023600010104     C*
023700001123     C                   seton                                        LR
023800001123
023900001123     C*--------------------------------------------------------------------
024000001123     C*  CALDAT  - Routine di calcolo data di creazione del file
024100001123     C*--------------------------------------------------------------------
024200001123     C     caldat        begsr
024300001123     C*
024400001123     C                   z-add     *zeros        depdata
024500001123     C*
024600001123     C* "Giornalizzo" i secondi ritornati dalla API
024700001123     C                   eval      wrkgg = st_mtime/60/60/24
024800001123     C*
024900001123     C     epoch         adddur    wrkgg:*D      dataFile
025000001123     C     dataFile      adddur    NUMGIOVAL:*D  wrkdataiso
025100001123     C     *ISO          move      wrkdataiso    wrkdata
025200140708     C     *ISO          move      dataFile      depdata
025300001123     C                   if        wrkdata < datcor
025400001123     C                   exsr      wriWF
025500001123     C                   endif
025600001123     C*
025700001123     C                   endsr
025800001123
025900001123
026000001123     C*--------------------------------------------------------------------
026100001123     C*  WRIWF   - Routine di riempimento buffer del record e scrittura sul file di wrk
026200001123     C*--------------------------------------------------------------------
026300140709     C     wriWF         begsr
026400140709     C*
026500140709     C                   clear                   DS_ifsfl1
026600100316     C                   clear                   was7ifs0
026700090324     C                   eval      ifsins    = datcor*1000000+oracor
026800090324     C                   eval      ifskey    = KEY
026900090324     C                   eval      ifspths   = %trim(PATHTOLIST)+'/'
027000090324     C                   eval      ifsfils   = %trim(FileName)
027100090324     C                   eval      ifspthd   = %trim(PATHDEST)
027200090324     C                   eval      ifsopz    = OPZ
027300090324     C                   eval      ifsdat    = depdata
027400090324     C                   eval      ifssiz    = st_size
027500140924     C                   eval      ifstrytot = 100
027600140709     C                   eval      §ifsfl1_liv = %trim(%editc(Livello:'X'))
027700140709     C                   eval      ifsfl1    = DS_ifsfl1
027800140709     C* Apro il wrk-file...
027900140709     C                   if        not %open(was7ifs0f)
028000140709     C                   open      was7ifs0f
028100140709     C                   endif
028200140709     C* Scarico il buffer di output
028300100316     C                   write     was7ifs0
028400001123     C*
028500001123     C                   endsr
028600001123
028700001123     C*--------------------------------------------------------------------
028800010104     C*  INZSR   - Routine di inizializzazione *PGM
028900001123     C*--------------------------------------------------------------------
029000010104     C     inzsr         begsr
029100090324     C*
029200090324     C* Data di elaborazione
029300090324     C                   time                    w0140            14 0
029400090324     C                   move      w0140         w0080             8 0          *DATA (8) IN G/M/AA
029500090324     C                   movel     w0140         oracor            6 0          *ORA  (6) IN H/M/SS
029600090324     C     *eur          movel     w0080         dataeur
029700090324     C                   movel     dataeur       dataiso
029800090324     C     *iso          movel     dataiso       datcor            8 0
029900001123     C*
030000001123     C                   endsr
030100010104
030200010104
030300010104
030400080910     P tis783r2        e

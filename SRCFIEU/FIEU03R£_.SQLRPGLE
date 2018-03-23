000100981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*--------------------------------------------------------------*
000300160531     Ffieu03D   CF   E             WORKSTN
000400160531     ftabel00f  if   e           k disk
000500170922     ffiPND01L  if   e           k disk
000600170925      *--
000700160623     fdppdc01l  if   e           k disk
000800170925      *--
000900160531     fwdpdmanf  o    e           k disk     usropn
001000160531      *
001100160531     D kpjba         e DS
001200170925     d fipndds       e ds                  extname(fipnd00f)
001300170927     d titasds       e ds                  extname(titas00f)
001400170925      *----
001500170922     d dPNDflo       e ds
001600160531     d ds$3          e ds
001700020624     D*
001800160623     d dataiso                         d
001900160623     d dataeur                         d   datfmt(*eur)
002000160531     d coman           s             99
002100160531     d luncmd          s             15  5
002200160623     d DigitN          c                   const('0123456789')
002300160531     D cmd1            S              1    DIM(99) CTDATA PERRCD(99)
002400000000     C*---------------------------------------------------------------*
002500160531     c                   do        *hival
002600160531     c                   exfmt     video1
002700160531     c                   clear                   $msg
002800160531     c                   setoff                                       99
002900160531     c   kc              leave
003000160531     c                   exsr      contr
003100160531     c   99              iter
003200160531     c                   if        *inkf
003300160531      *pulisce file di work
003400160531     c                   exsr      pulisci
003500160531      *elabora rekord da esportare
003600160531     c                   exsr      elabora
003700160531     c                   leave
003800160531     c                   endif
003900160531     c                   enddo
004000020624
004100160531     c                   seton                                        lr
004200160531      *______________________________________________________________
004300160531     c     contr         begsr
004400160531      *______________________________________________________________
004500160531     c                   if        vanno > annomx or vanno < annomi
004600160531     c                   seton                                        99
004700160531     c                   eval      $msg = 'Anno non corretto'
004800160531     c                   leavesr
004900160531     c                   endif
005000160531     c                   movel     vanno         vanmes            6 0
005100160531     c                   move      vmese         vanmes
005200160531     c                   if        vanmes > finoa or vanmes < da
005300160531     c                   seton                                        99
005400160531     c                   eval      $msg = 'Periodo scelto fuori dai limiti'
005500160531     c                   leavesr
005600160531     c                   endif
005700160531     c                   endsr
005800160531      *________________________________________________________________
005900160531     c     elabora       begsr
006000160531      *________________________________________________________________
006100030109      *
006200170925     C/EXEC SQL
006300171012     C+ DECLARE A1 CURSOR FOR
006400171012     C+ with FILE1 as
006500171012     C+ (SELECT * FROM titaS00F JOIN FIPND00F
006600171012     C+  ON TASAAS = PNDAAS and TASLNP = PNDLNP and
006700171012     C+     TASNRS = PNDNRS and TASNSP = PNDNSP
006800171012     C+  WHERE substr(PNDFLO, 1, 1) in ('M', '1', '2')  ),
006900171012     C+ FILE2 AS (SELECT * FROM titaS10F JOIN FIPND00F
007000171012     C+  ON TASAAS = PNDAAS and TASLNP = PNDLNP and
007100171012     C+     TASNRS = PNDNRS and TASNSP = PNDNSP
007200171012     C+  WHERE substr(PNDFLO, 1, 1) in ('M', '1', '2')  )
007300171012     C+ select * from  file1
007400171012     C+ union all
007500171012     C+ select * from  file2
007600170925     C/END-EXEC
007700170925      *
007800030109     C/EXEC SQL
007900030109     C+ OPEN A1
008000030109     C/END-EXEC
008100030109      *
008200160531     c                   do        *hival
008300030109     C/EXEC SQL
008400170925     C+ FETCH  next FROM A1 INTO :titasds, :fipndds
008500030109     C/END-EXEC
008600160531     c                   if        sqlcod < 0 or sqlcod = 100
008700160531     c                   leave
008800160531     c                   endif
008900170926      **
009000160531     c                   movel     tasmgs        mesbol            2 0
009100170925      **
009200160531     c                   if        mesbol = vmese and
009300160531     c                             tasaas = vanno
009400170925      **
009500160624     c                   move      tasmgs        datasped          8 0
009600160624     c                   movel     tasaas        datasped
009700160531     c                   move      tasaas        wannosp
009800160531     c                   move      taslnp        wlnpsp
009900160531     c                   move      tasnrs        wserie
010000160531     c                   move      tasnsp        wsped
010100160531     c                   move      vanno         wanno
010200160531     c                   move      vmese         wmese
010300170925      **
010400170925     c                   movel     pndIPN        wparcel
010500170922      **
010600170922      **    Flag di tipo ricezione BOLLE (sostituito nel FIPND00F)
010700170922     c                   movel     PNDflo        dPNDflo
010800170922     c                   movel     §PNDTRD       wflag
010900170922      **
011000160623     c                   exsr      chkdepot
011100160623     c   35              iter
011200160623     c                   write     wdpdman
011300030129     c                   endif
011400170925      *
011500160531     c                   enddo
011600030109     C/EXEC SQL
011700030109     C+ CLOSE A1
011800030109     C/END-EXEC
011900160531     c                   endsr
012000160531      *______________________________________________________________
012100160531     c     pulisci       begsr
012200160531      *______________________________________________________________
012300160531     c                   movea     cmd1          coman
012400160531     C                   z-add     99            luncmd
012500160531     C                   CALL      'QCMDEXC'                            34
012600160531     C                   parm                    coman
012700160531     C                   parm                    luncmd
012800160531     c                   open      wdpdmanf
012900160531     c                   endsr
013000160623      *______________________________________________________________
013100160623     c     chkdepot      begsr
013200160623      *______________________________________________________________
013300160623     c                   setoff                                       35
013400160623     c     digitN        check     wparcel                                35
013500160623     C                   if        not *in35
013600160623     c                   movel     wparcel       k4                4
013700160623     c     k4            chain     dppdc01l
013800160623     c                   if        not%found(dppdc01l)
013900160624     c                             or  datasped > pdcdsc
014000160623     c                   seton                                        35
014100160623     C                   endif
014200160623     C                   endif
014300160623     c                   endsr
014400160531      *______________________________________________________________
014500160531     c     *inzsr        begsr
014600160531      *______________________________________________________________
014700160531     c     *entry        plist
014800160531     c                   parm                    kpjba
014900170925      *
015000170925     c                   clear                   fipndds
015100160531      *
015200160531     c     ktab          klist
015300160531     c                   kfld                    TBLKUT
015400160531     c                   kfld                    TBLCOD
015500170922      *
015600160531     c                   z-add     1             tblkut
015700160531     c                   move      '$3'          tblcod
015800160623     C                   TIME                    W0140            14 0
015900160623     C                   MOVE      W0140         oggi              8 0
016000160623     c                   move      oggi          dataeur
016100160623     c                   move      dataeur       dataiso
016200160623     c                   move      dataiso       oggig             8 0
016300160531      *
016400160531     c     ktab          chain     tabel00f
016500160531     c                   if        %found(tabel00f)
016600160531     c                   movel     tbluni        ds$3
016700160531     c                   movel     §$3D00        finoa             6 0
016800160531     c                   movel     §$3D10        da                6 0
016900160531     c                   endif
017000160531     c                   movel     finoa         vanno
017100160531     c                   move      finoa         vmese
017200160531     c                   movel     finoa         annomx            4 0
017300160531     c                   movel     da            annomi            4 0
017400160531      *
017500160531     c                   endsr
017600011129     C*--------------------------------------------------------------------
017700160531**  CMD1
017800160531CLRPFM    FILE(wdpdmanf)

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
006300170925     C+ DECLARE A1 CURSOR FOR SELECT *
006400170927     C+       FROM titaS00F JOIN FIPND00F
006500170925     C+  ON TASAAS = PNDAAS and TASLNP = PNDLNP and
006600170925     C+     TASNRS = PNDNRS and TASNSP = PNDNSP
006700170925     C+   WHERE
006800170925     C+     substr(PNDFLO, 1, 1) in ('M', '1', '2')
006900170925     C/END-EXEC
007000170925      *
007100030109     C/EXEC SQL
007200030109     C+ OPEN A1
007300030109     C/END-EXEC
007400030109      *
007500160531     c                   do        *hival
007600030109     C/EXEC SQL
007700170925     C+ FETCH  next FROM A1 INTO :titasds, :fipndds
007800030109     C/END-EXEC
007900160531     c                   if        sqlcod < 0 or sqlcod = 100
008000160531     c                   leave
008100160531     c                   endif
008200170926      **
008300160531     c                   movel     tasmgs        mesbol            2 0
008400170925      **
008500160531     c                   if        mesbol = vmese and
008600160531     c                             tasaas = vanno
008700170925      **
008800160624     c                   move      tasmgs        datasped          8 0
008900160624     c                   movel     tasaas        datasped
009000160531     c                   move      tasaas        wannosp
009100160531     c                   move      taslnp        wlnpsp
009200160531     c                   move      tasnrs        wserie
009300160531     c                   move      tasnsp        wsped
009400160531     c                   move      vanno         wanno
009500160531     c                   move      vmese         wmese
009600170925      **
009700170925     c                   movel     pndIPN        wparcel
009800170922      **
009900170922      **    Flag di tipo ricezione BOLLE (sostituito nel FIPND00F)
010000170922     c                   movel     PNDflo        dPNDflo
010100170922     c                   movel     §PNDTRD       wflag
010200170922      **
010300160623     c                   exsr      chkdepot
010400160623     c   35              iter
010500160623     c                   write     wdpdman
010600030129     c                   endif
010700170925      *
010800160531     c                   enddo
010900030109     C/EXEC SQL
011000030109     C+ CLOSE A1
011100030109     C/END-EXEC
011200160531     c                   endsr
011300160531      *______________________________________________________________
011400160531     c     pulisci       begsr
011500160531      *______________________________________________________________
011600160531     c                   movea     cmd1          coman
011700160531     C                   z-add     99            luncmd
011800160531     C                   CALL      'QCMDEXC'                            34
011900160531     C                   parm                    coman
012000160531     C                   parm                    luncmd
012100160531     c                   open      wdpdmanf
012200160531     c                   endsr
012300160623      *______________________________________________________________
012400160623     c     chkdepot      begsr
012500160623      *______________________________________________________________
012600160623     c                   setoff                                       35
012700160623     c     digitN        check     wparcel                                35
012800160623     C                   if        not *in35
012900160623     c                   movel     wparcel       k4                4
013000160623     c     k4            chain     dppdc01l
013100160623     c                   if        not%found(dppdc01l)
013200160624     c                             or  datasped > pdcdsc
013300160623     c                   seton                                        35
013400160623     C                   endif
013500160623     C                   endif
013600160623     c                   endsr
013700160531      *______________________________________________________________
013800160531     c     *inzsr        begsr
013900160531      *______________________________________________________________
014000160531     c     *entry        plist
014100160531     c                   parm                    kpjba
014200170925      *
014300170925     c                   clear                   fipndds
014400160531      *
014500160531     c     ktab          klist
014600160531     c                   kfld                    TBLKUT
014700160531     c                   kfld                    TBLCOD
014800170922      *
014900160531     c                   z-add     1             tblkut
015000160531     c                   move      '$3'          tblcod
015100160623     C                   TIME                    W0140            14 0
015200160623     C                   MOVE      W0140         oggi              8 0
015300160623     c                   move      oggi          dataeur
015400160623     c                   move      dataeur       dataiso
015500160623     c                   move      dataiso       oggig             8 0
015600160531      *
015700160531     c     ktab          chain     tabel00f
015800160531     c                   if        %found(tabel00f)
015900160531     c                   movel     tbluni        ds$3
016000160531     c                   movel     §$3D00        finoa             6 0
016100160531     c                   movel     §$3D10        da                6 0
016200160531     c                   endif
016300160531     c                   movel     finoa         vanno
016400160531     c                   move      finoa         vmese
016500160531     c                   movel     finoa         annomx            4 0
016600160531     c                   movel     da            annomi            4 0
016700160531      *
016800160531     c                   endsr
016900011129     C*--------------------------------------------------------------------
017000160531**  CMD1
017100160531CLRPFM    FILE(wdpdmanf)

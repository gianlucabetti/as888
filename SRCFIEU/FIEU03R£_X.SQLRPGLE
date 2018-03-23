000100981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*--------------------------------------------------------------*
000300160531     Ffieu03D   CF   E             WORKSTN
000400160531     ftabel00f  if   e           k disk
000500160623     fdppdc01l  if   e           k disk
000600160531     ftitas30c  if   e           k disk
000700160531     f                                      ignore(titas000)
000800160531     f                                      ignore(titasp00)
000900160531     fwdpdmanf  o    e           k disk     usropn
001000160531      *
001100160531     D kpjba         e DS
001200160531     d tita4ds       e ds                  extname(tita400f)
001300160531     d dsbl4i        e ds
001400160531     d ds$3          e ds
001500020624     D*
001600160623     d dataiso                         d
001700160623     d dataeur                         d   datfmt(*eur)
001800160531     d coman           s             99
001900160531     d luncmd          s             15  5
002000160623     d DigitN          c                   const('0123456789')
002100160531     D cmd1            S              1    DIM(99) CTDATA PERRCD(99)
002200011026
002300000000     C*---------------------------------------------------------------*
002400160531      *
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
006100020313     C/EXEC SQL
006200160531     C+ DECLARE A1 CURSOR FOR SELECT * FROM tita400F WHERE
006300160531     C+ ta4trc = 'I' and  substr(ta4not, 34, 1) in('M', '1', '2')
006400020313     C/END-EXEC
006500030109      *
006600030109     C/EXEC SQL
006700030109     C+ OPEN A1
006800030109     C/END-EXEC
006900030109      *
007000160531     c                   do        *hival
007100030109     C/EXEC SQL
007200160531     C+ FETCH  next FROM A1 INTO :tita4ds
007300030109     C/END-EXEC
007400160531     c                   if        sqlcod < 0 or sqlcod = 100
007500160531     c                   leave
007600160531     c                   endif
007700160531     c* verifico se su titas il rekord mi interessa nel caso lo scrivo
007800160531     c     ktas30        chain     titas30c
007900160531     c                   if        %found(titas30c)
008000160531     c                   movel     tasmgs        mesbol            2 0
008100160531     c                   if        mesbol = vmese and
008200160531     c                             tasaas = vanno
008300160624     c                   move      tasmgs        datasped          8 0
008400160624     c                   movel     tasaas        datasped
008500160531     c                   move      tasaas        wannosp
008600160531     c                   move      taslnp        wlnpsp
008700160531     c                   move      tasnrs        wserie
008800160531     c                   move      tasnsp        wsped
008900160531     c                   move      vanno         wanno
009000160531     c                   move      vmese         wmese
009100160531     c                   movel     ta4not        dsbl4i
009200160531     c                   move      §b4ipn        wparcel
009300160531     c                   move      §b4trd        wflag
009400160623     c                   exsr      chkdepot
009500160623     c   35              iter
009600160623     c                   write     wdpdman
009700030109     c                   endif
009800030129     c                   endif
009900160531     c                   enddo
010000020624      *
010100030109     C/EXEC SQL
010200030109     C+ CLOSE A1
010300030109     C/END-EXEC
010400160531     c                   endsr
010500160531      *______________________________________________________________
010600160531     c     pulisci       begsr
010700160531      *______________________________________________________________
010800160531     c                   movea     cmd1          coman
010900160531     C                   z-add     99            luncmd
011000160531     C                   CALL      'QCMDEXC'                            34
011100160531     C                   parm                    coman
011200160531     C                   parm                    luncmd
011300160531     c                   open      wdpdmanf
011400160531     c                   endsr
011500160623      *______________________________________________________________
011600160623     c     chkdepot      begsr
011700160623      *______________________________________________________________
011800160623     c                   setoff                                       35
011900160623     c     digitN        check     wparcel                                35
012000160623     C                   if        not *in35
012100160623     c                   movel     wparcel       k4                4
012200160623     c     k4            chain     dppdc01l
012300160623     c                   if        not%found(dppdc01l)
012400160624     c                             or  datasped > pdcdsc
012500160623     c                   seton                                        35
012600160623     C                   endif
012700160623     C                   endif
012800160623     c                   endsr
012900160531      *______________________________________________________________
013000160531     c     *inzsr        begsr
013100160531      *______________________________________________________________
013200160531     c     *entry        plist
013300160531     c                   parm                    kpjba
013400160531     c                   clear                   tita4ds
013500160531      *
013600160531     c     ktab          klist
013700160531     c                   kfld                    TBLKUT
013800160531     c                   kfld                    TBLCOD
013900160531     c                   z-add     1             tblkut
014000160531     c                   move      '$3'          tblcod
014100160623     C                   TIME                    W0140            14 0
014200160623     C                   MOVE      W0140         oggi              8 0
014300160623     c                   move      oggi          dataeur
014400160623     c                   move      dataeur       dataiso
014500160623     c                   move      dataiso       oggig             8 0
014600160531      *
014700160531     c     ktas30        klist
014800160531     c                   kfld                    TA4AAS
014900160531     c                   kfld                    TA4LNP
015000160531     c                   kfld                    TA4NRS
015100160531     c                   kfld                    TA4NSP
015200160531     c                   kfld                    TTBL              2
015300160531     c                   move      'F1'          ttbl
015400160531      *
015500160531     c     ktab          chain     tabel00f
015600160531     c                   if        %found(tabel00f)
015700160531     c                   movel     tbluni        ds$3
015800160531     c                   movel     §$3D00        finoa             6 0
015900160531     c                   movel     §$3D10        da                6 0
016000160531     c                   endif
016100160531     c                   movel     finoa         vanno
016200160531     c                   move      finoa         vmese
016300160531     c                   movel     finoa         annomx            4 0
016400160531     c                   movel     da            annomi            4 0
016500160531      *
016600160531     c                   endsr
016700011129     C*--------------------------------------------------------------------
016800160531**  CMD1
016900160531CLRPFM    FILE(wdpdmanf)

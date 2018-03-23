000100110801     H* FIDG51R *-----------------------------------------------------*
000200110801     H* imposta lo stesso codice giro su tutte le sped. del perfect order
000300930225     H*---------------------------------------------------------------*
000400110802     Ffiarg00f  uf   E             DISK    prefix(£)
000500900514     D KPJBA         E DS
000600110801     d fidg51ds      e ds
000700110801     d trul82ds      e ds
000800110801     d fnarbds       e ds                  extname(fnarb00f)
000900110801     d fiargds       e ds                  extname(fiarg00f)
001000110803     d fnlvstds      e ds
001100110803     D WLBDA8          DS
001200110803     D  G02DAT                 1      8  0
001300110803     D  G02INV                 9     16  0
001400110803     D  G02ERR                17     17
001500110803     D  G02TGI                18     22  0
001600110801     drrnarg           s              9  0
001700110801     d comksci         s              7  0
001800110801     d comkscf         s              7  0
001900110801     d comrmai         s             15
002000110801     d comrmaf         s             15
002100110801     d comlna          s              3  0
002200110801     d savcgi          s             10
002300110801     d savrma          s             15
002400110801     D DSUL06        E DS                  EXTNAME(TRUL06DS)
002500110801     D  LIN                    1     90  0
002600110801     D                                     DIM(30)
002700110801     D  L6             S              3  0 DIM(30) ASCEND
002800070130     C**********************************************************************
002900070131     c                   exsr      srinz
003000110801     c*
003100110801     c                   do        30            xx                2 0
003200110801     c                   if        l6(xx) = 0
003300110801     c                   leave
003400110801     c                   end
003500110801     c                   eval      comlna = l6(xx)
003600110801     c                   clear                   savcgi
003700110801     c                   clear                   savrma
003800130221      *** Wurth adesso manda nel campo non solo numeri quindi per identificare
003900130221      ***  il Perfect Order si deve sostituire il test da diverso da *blank a
004000130221      ***  maggiore di '0'.
004100130221     *******C/EXEC SQL
004200130221     *******C+ DECLARE B1 CURSOR FOR SELECT rrn(fiarg01l), arbrma, arbddc,
004300130221     *******C+ arbfin, arbmgs, argcgi FROM fiarg01l join fnarb01l on arbaas=
004400130221     *******C+ argaas and arblnp=arglnp and arbnrs= argnrs and arbnsp= argnsp
004500130221     *******C+ WHERE arblna=:comlna and arbdcm = 0 and arbxco <> ' ' and arbrma
004600130221     *******C+ <> ' ' and arbrma between :comrmai and :comrmaf and arbksc
004700130221     *******C+ between :comksci and :comkscf ORDER BY arbrma, arbpkb desc
004800130221     *******C/END-EXEC
004900110726
005000130221     C/EXEC SQL
005100130221     C+ DECLARE B1 CURSOR FOR SELECT rrn(fiarg01l), arbrma, arbddc,
005200130221     C+ arbfin, arbmgs, argcgi FROM fiarg01l join fnarb01l on arbaas=
005300130221     C+ argaas and arblnp=arglnp and arbnrs= argnrs and arbnsp= argnsp
005400130221     C+ WHERE arblna=:comlna and arbdcm = 0 and arbxco > '0' and arbrma
005500130221     C+ <> ' ' and arbrma between :comrmai and :comrmaf and arbksc
005600130221     C+ between :comksci and :comkscf ORDER BY arbrma, arbpkb desc
005700130221     C/END-EXEC
005800130221
005900110726     C/EXEC SQL
006000110726     C+ OPEN b1
006100110726     C/END-EXEC
006200110726
006300110726     C                   do        *hival
006400110726
006500110726     C/EXEC SQL
006600110803     C+ FETCH NEXT FROM B1 INTO :rrnarg, :arbrma, :arbddc, :arbfin,
006700110803     C+ :arbmgs, :£argcgi
006800110726     C/END-EXEC
006900110726
007000110726     C                   select
007100110726     C                   when      sqlcod = 100
007200110726     c                   leave
007300110726     C                   WHEN      SqlCod < 0
007400110726     c                   if        sqlcod <> -913
007500110726     C                   seton                                        H1
007600110726     c                   end
007700110726     c                   exsr      srfine
007800110726     C                   other
007900110801     c* memorizzo il primo giro a rottura di perfect order
008000110801     c* perchè ho ordinato per peso e prendo quello che lo ha maggiore
008100110802     c                   if        d51cgii = ' '
008200110801     c                   if        savrma <> arbrma and £argcgi <> ' '
008300110801     c                   eval      savcgi = £argcgi
008400110801     c                   eval      savrma = arbrma
008500110801     c                   exsr      srupdate
008600110801     c                   else
008700110801     c                   iter
008800110801     c                   end
008900110801     c* se passato il codice giro lo imposto
009000110801     c                   else
009100110801     c     RRNARG        chain(e)  fiarg00f
009200110801     c                   if        %error
009300110801     c                   iter
009400110801     c                   end
009500110905     c                   if        %found(fiarg00f) and £argcgi <> 'M'
009600110802     c                   if        £argcgi <> d51cgii
009700110803     C                   eval      £argtgi = 'M'
009800110802     C                   eval      £argcgi = d51cgii
009900110801     c                   update    fiarg000
010000110922     c                   exsr      srztl
010100110801     c                   iter
010200110801     c                   end
010300110801     c                   end
010400110801     c* un solo perfect order con codice giro passato
010500110801     c                   end
010600110726     C                   endsl
010700110726     C*
010800110726     C                   enddo
010900110726
011000110726     C/EXEC SQL
011100110726     C+ CLOSE b1
011200110726     C/END-EXEC
011300110801     C                   enddo
011400110801     c                   exsr      srfine
011500110801     C**********************************************************************
011600110801     c     srupdate      begsr
011700110801     C**********************************************************************
011800130221      *** Wurth adesso manda nel campo non solo numeri quindi per identificare
011900130221      ***  il Perfect Order si deve sostituire il test da diverso da *blank a
012000130221      ***  maggiore di '0'.
012100130221     *****C/EXEC SQL
012200130221     *****C+ DECLARE A1 CURSOR FOR SELECT rrn(fiarg01l), arbfin, arbddc,
012300130221     *****C+ arbmgs FROM fiarg01l join fnarb01l on arbaas= argaas and
012400130221     *****C+ arblnp=arglnp and arbnrs= argnrs and arbnsp= argnsp WHERE
012500130221     *****C+ arblna=:comlna and arbdcm = 0 and arbxco <> ' ' and arbrma <> ' '
012600130221     *****C+ and arbrma = :savrma and arbksc between :comksci and :comkscf and
012700130221     *****C+ argcgi <> :savcgi and argtgi <> 'M'
012800130221     *****C/END-EXEC
012900130221
013000130221     C/EXEC SQL
013100130221     C+ DECLARE A1 CURSOR FOR SELECT rrn(fiarg01l), arbfin, arbddc,
013200130221     C+ arbmgs FROM fiarg01l join fnarb01l on arbaas= argaas and
013300130221     C+ arblnp=arglnp and arbnrs= argnrs and arbnsp= argnsp WHERE
013400130221     C+ arblna=:comlna and arbdcm = 0 and arbxco > '0' and arbrma <> ' '
013500130221     C+ and arbrma = :savrma and arbksc between :comksci and :comkscf and
013600130221     C+ argcgi <> :savcgi and argtgi <> 'M'
013700130221     C/END-EXEC
013800130221
013900110801     C/EXEC SQL
014000110801     C+ OPEN a1
014100110801     C/END-EXEC
014200110801
014300110801     C                   do        *hival
014400110801
014500110801     C/EXEC SQL
014600110803     C+ FETCH NEXT FROM A1 INTO :rrnarg, :arbfin, :arbddc, :arbmgs
014700110801     C/END-EXEC
014800110801
014900110801     C                   select
015000110801     C                   when      sqlcod = 100
015100110801     c                   leave
015200110801     C                   WHEN      SqlCod < 0
015300110801     c                   if        sqlcod <> -913
015400110801     C                   seton                                        H1
015500110801     c                   end
015600110801     c                   exsr      srfine
015700110801     C                   other
015800110801     c     RRNARG        chain(e)  fiarg00f
015900110801     c                   if        %error
016000110801     c                   iter
016100110801     c                   end
016200110801     c                   if        %found(fiarg00f)
016300110801     C                   eval      £argcgi = savcgi
016400110803     C                   eval      £argtgi = 'M'
016500110801     c                   update    fiarg000
016600110922     c                   exsr      srztl
016700110801     c                   iter
016800110801     c                   end
016900110801     C                   endsl
017000110801     C*
017100110801     C                   enddo
017200110801
017300110801     C/EXEC SQL
017400110801     C+ CLOSE a1
017500110801     C/END-EXEC
017600110802     c                   endsr
017700070131     C**********************************************************************
017800110803     c     srztl         begsr
017900070131     C**********************************************************************
018000110803     c                   clear                   fnlvstds
018100110803     c                   eval      ilvstaas=£argaas
018200110803     c                   eval      ilvstlnp=£arglnp
018300110803     c                   eval      ilvstnrs=£argnrs
018400110803     c                   eval      ilvstnsp=£argnsp
018500110803     c                   eval      ilvstcgi=£argcgi
018600110803     c                   eval      ilvstmgs=arbmgs
018700110803     c                   eval      ilvstfin=arbfin
018800110803     c                   eval      ilvstpoc=d51fgsi
018900110803     c                   if        arbddc>0
019000110803     c                   eval      ilvstdat=arbddc
019100110803     c                   else
019200110803     c                   eval      ilvstdat=wuda
019300110803     c                   endif
019400110803     c                   call      'FNLVSTR'
019500110803     c                   parm                    kpjba
019600110803     c                   parm                    fnlvstds
019700110803     c                   endsr
019800110803     C**********************************************************************
019900110803     c     srinz         begsr
020000110803     C**********************************************************************
020100061002     C     *ENTRY        PLIST
020200070130     C                   PARM                    kpjba
020300110801     c                   movel     kpjbu         fidg51ds
020400110801     c*
020500110801     c                   if        d51fgsi = 0
020600110801     c                   exsr      srfine
020700110801     c                   end
020800110801     c*
020900110801     c                   clear                   l6
021000110801     c                   if        d51rmai = ' '
021100110801     C                   MOVE      '£6'          D06COD
021200110801     c                   movel     d51fgsi       D06KEY
021300110801     C                   MOVEL     DSUL06        KPJBU
021400110801     C                   CALL      'TRUL06R'
021500110801     C                   PARM                    KPJBA
021600110801     C                   MOVEL     KPJBU         DSUL06
021700110801     C                   MOVEA     LIN           L6
021800110801     c                   eval      COMrmai= *loval
021900110801     c                   eval      COMrmaf= *hival
022000110801     c                   else
022100110801     c                   eval      l6(1) = d51fgsi
022200110801     c                   eval      COMrmai= d51rmai
022300110801     c                   eval      COMrmaf= d51rmai
022400110801     c                   end
022500110801     c* se passato il codice cliente lo imposto
022600110801     c                   if        d51ksci <> 0
022700110801     c                   eval      COMksci= d51ksci
022800110801     c                   eval      COMkscf= d51ksci
022900110801     c                   else
023000110801     c                   eval      COMksci= d51ksci
023100110801     c                   eval      COMkscf= 9999999
023200110801     c                   end
023300110803     C                   TIME                    W0120            14 0
023400110803     C                   MOVE      W0120         WDAT              8 0
023500110803     C*
023600110803     C                   Z-ADD     WDAT          G02DAT
023700110803     C                   MOVEL     *BLANK        G02ERR
023800110803     C                   CALL      'XSRDA8'
023900110803     C                   PARM                    WLBDA8
024000110803     C* UDATE A 8 IN AAAA/MM/GG
024100110803     C                   Z-ADD     G02INV        WUDA              8 0
024200070130     c                   endsr
024300110726     C**********************************************************************
024400110726     c     srfine        begsr
024500110726     C**********************************************************************
024600110801     c                   eval      kpjbu = fidg51ds
024700110726     c                   seton                                        lr
024800110916     c                   return
024900110726     C                   ENDSR

000100090402     h Decedit('0,') Datedit(*dmy/) Option(*nodebugio)
000200090402
000300090402      * -------------------------------------------------------------*
000400090714      * Crea file visite TIVIS
000500090402      * -------------------------------------------------------------*
000600090402
000700090714     fTnofm01l  if   e           k Disk
000800090714     fTnvis05l  if   e           k Disk
000900100330     fTfaco00f  if   e           k Disk    rename(cnaco000:tfaco000)
001000100330     fCnaco00f  if   e           k Disk
001100100330     fTfntc01l  uf   e           k Disk
001200090715     fTivis00f  uf a e             Disk
001300090715     fTivof00f  uf a e             Disk
001400100330     fTicpn00f  o  a e             Disk
001500090715
001600090715      * ---------------------------------------------------------
001700090715     d dsta01        e ds
001800090402
001900100330     D                 DS
002000100330     D  CCC                    1      4  0
002100100330     D  NOTNRV                 5     11  0
002200100330     D  NOTKE1                 1     11
002300100330     d data_iso        s               d   Datfmt(*iso)
002400100330     d savpno          s                   like(cpnpno)
002500090715      * ---------------------------------------------------------
002600090715     iTnvis000
002700090715     i              visesi                      ex_visesi
002800090715
002900090715      * ---------------------------------------------------------
003000090402
003100090715     c     *loval        setll     tnvis05l
003200090715     c                   do        *hival
003300090715     c                   read      tnvis05l
003400090715     c                   if        %eof(tnvis05l)
003500090715     c                   leave
003600090715     c                   endif
003700090715      *
003800090715     c                   eval      viscmmi= viscmm
003900090715     c                   eval      visdat = visdvi
004000090715      * se visita chiusa imposto data chiusura con al data conferma visita + grande tra l'estera
004100090715      * e l'italia
004200090715
004300090715     c                   clear                   visdch
004400090715
004500090715     c                   if        visfsv = 'C' and visdcv > visdne
004600090715     c                   eval      visdch = visdcv
004700090715     c                   endif
004800090715
004900090715     c                   if        visfsv = 'C' and visdne > visdcv
005000090715     c                   eval      visdch = visdne
005100090715     c                   endif
005200090715
005300090715     c                   movel     viscmm        visfil
005400100330      * controllo tipo visita
005500100330      * Incremento diventa aumento
005600100330     c                   If        vistpv = 'I'
005700100330     c                   eval      vistpv = 'A'
005800100330     c                   endif
005900100330      * se tipo visita diverso da N e da A verifico se ha delle tariffe e le trasformo in A
006000100330     c                   If        vistpv <> 'A' and vistpv <> 'N'
006100100330     c     visnrv        setll     tnofm01l
006200100330     c                   If        %equal(tnofm01l)
006300100330     c                   eval      vistpv = 'A'
006400100330     c                   endif
006500100330     c                   endif
006600100330
006700100330     C     KCLI          KLIST
006800100330     C                   KFLD                    CODUT             1 0
006900100330     C                   KFLD                    CCC               4 0
007000100330     C                   KFLD                    CLI               7 0
007100100330     C** ACCESSO X CHIAVE A FILE NOTE TFNTC
007200100330     C     KNTC          KLIST
007300100330     C                   KFLD                    APPL              1
007400100330     C                   KFLD                    NOTKE1           11
007500100330     C                   KFLD                    NOTKE2            4
007600100330     C                   KFLD                    NOTTNT            2
007700100330     C*
007800100330     C     KNTC2         KLIST
007900100330     C                   KFLD                    APPL              1
008000100330     C                   KFLD                    NOTKE1
008100100330
008200100330     c                   eval      codut = 1
008300100330     c                   eval      ccc   = 0151
008400100330     c                   eval      cli   = visnrv
008500100330
008600100330     c     kcli          chain     tfaco00f
008700100330     c                   if        not %found(tfaco00f)
008800100330     c                   eval      cli   = visksc
008900100330     c     kcli          chain     cnaco00f
009000100330     c                   if        not %found(cnaco00f)
009100100330     c                   clear                   acolib
009200100330     c                   endif
009300100330     c                   endif
009400100330
009500100330     c                   eval      viscpo = acolib
009600100330
009700100330     c                   if        viscpo > 0
009800090715     c                   write     tivis000
009900100330     c                   else
010000100330     c                   iter
010100100330     c                   endif
010200090715      * verifico se esistono delle tariffe legate alla visita
010300090402
010400090715     c     visnrv        setll     tnofm01l
010500090402     c                   Do        *hival
010600090402      * Leggo il file
010700090715     c     visnrv        Reade     tnofm01l
010800090402      * Fine file esco
010900090715     c                   If        %Eof(Tnofm01l)
011000090402     c                   Leave
011100090402     c                   EndIf
011200090402
011300090715     c                   clear                   tivof000
011400090715      * valorizzo con i campi delle tariffe
011500090715     c                   eval      vofnrv = tamksc
011600090715     c                   eval      vofctr = tamctr
011700100126     c                   eval      vofprg = tamprg
011800090715      * il progressivo non lo valorizzo in quanto è sempre a zero
011900090715      * il flag del tipo tariffa viene impostato in base ad italia e network in caso di estero
012000090715     c                   eval      voftpt = tamfie
012100090715
012200090715     c                   eval      dsta01 = tamflo
012300090715      * il flag del tipo tariffa viene impostato DPD se italia e network DPD
012400090715     c                   if        §tadpd =  'S'
012500090715     c                   eval      voftpt = 'D'
012600090715     c                   endif
012700090715      * il flag del tipo tariffa viene impostato FEDEX  se network FEDEX
012800090715     c                   if        §tafed =  'S'
012900090715     c                   eval      voftpt = 'F'
013000090715     c                   endif
013100090715      * l'esito non lo memorizzo in quanto se offerte confermate non esistono + nel file
013200090715
013300090715      * data presentazione offerta la prendo dalla testata in base al tipo tariffa italia e network
013400090715      * esteri
013500090715     c                   if        voftpt = 'I'
013600090715     c                   eval      vofdpo = visdpo
013700090715     c                   else
013800090715     c                   eval      vofdpo = visdoe
013900090715     c                   endif
014000090715
014100090715     c                   write     tivof000
014200090715
014300090715     c                   enddo
014400090715
014500100330     c                   eval      savpno = 0
014600100330      * note trattativa trasformo con "T" se visita legata a cliente solo le note 10
014700100330     c                   If        visksc > 0
014800100330     C                   MOVEL     'V'           APPL
014900100330     c                   eval      notnrv = visnrv
015000100330     c     kntc2         setll     tfntc01l
015100100330     c     kntc2         reade     tfntc01l
015200100330     c                   dow       not %eof(tfntc01l)
015300100330     c                   if        ntctnt = '10'
015400100330     c                   eval      ntcapl = 'T'
015500100330     c                   update    tfntc
015600100330     c                   endif
015700100330      * se note interne le scrivo in ticpn
015800100330     c                   if        ntctnt = 'IN'  and ntcntr > 0
015900100330     c                   clear                   ticpn000
016000100330     c                   eval      cpncpo  = viscpo
016100100330     c                   eval      cpnksc = visksc
016200100330     c                   eval      cpnnrv = visnrv
016300100330     c     *ymd          move      ntcntr        data_iso
016400100330     c                   move      data_iso      CPNDIM
016500100330     c                   eval      CPNHIM = 235800
016600100330     c                   eval      CPNSNS = 'S'
016700100330     c                   eval      CPNNOT = ntcrnt
016800100330     c                   eval      CPNPNO = savpno + 1
016900100330     c                   write     ticpn000
017000100330     c                   eval      savpno = cpnpno
017100100330     c                   endif
017200100330     c     kntc2         reade     tfntc01l
017300100330     c                   enddo
017400100330      * se cliente uguale a zero
017500100330     c                   else
017600100330
017700100330     C                   MOVEL     'V'           APPL
017800100330     c                   eval      notnrv = visnrv
017900100330     c     kntc2         setll     tfntc01l
018000100330     c     kntc2         reade     tfntc01l
018100100330     c                   dow       not %eof(tfntc01l)
018200100330     c                   eval      ntcapl = 'T'
018300100330     c                   if        ntctnt = '10' and ntcnk2 = '    '
018400100330     c                   eval      ntcnk2 = 'CLI '
018500100330     c                   endif
018600100330     c                   if        ntctnt <> 'IN' and ntctnt <> '30'
018700100330     c                   update    tfntc
018800100330     c                   endif
018900100330      * se note interne le scrivo in ticpn
019000100330     c                   if        ntctnt = 'IN' and ntcntr > 0
019100100330     c                   clear                   ticpn000
019200100330     c                   eval      cpncpo  = viscpo
019300100330     c                   eval      cpnksc = visksc
019400100330     c                   eval      cpnnrv = visnrv
019500100330     c     *ymd          move      ntcntr        data_iso
019600100330     c                   move      data_iso      CPNDIM
019700100330     c                   eval      CPNHIM = 235800
019800100330     c                   eval      CPNSNS = 'S'
019900100330     c                   eval      CPNNOT = ntcrnt
020000100330     c                   eval      CPNPNO = savpno + 1
020100100330     c                   write     ticpn000
020200100330     c                   eval      savpno = cpnpno
020300100330     c                   endif
020400100330     c     kntc2         reade     tfntc01l
020500100330     c                   enddo
020600100330     c
020700100330     c                   endif
020800090715     c                   enddo
020900090402
021000090402     c                   Eval      *InLr = *On

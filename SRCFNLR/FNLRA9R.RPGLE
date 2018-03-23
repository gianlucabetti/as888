000100030714     H DECEDIT('0,') DATEDIT(*ymd.)
000200010315      * FNLRA9R *----------------------------------------------------*
000300010315      *       - CHIUSURA AUTOMATICA FOGLI DISTINTA CON SOLI O.R.M.
000400010315      *--------------------------------------------------------------*
000500020918     fFnfvv03l  if   e           k Disk    Rename(fnfvv000:fnfvv03)
000600010315     FFNFVV01L  UF   E           K DISK
000700010315     FFNORM06L  IF   E           K DISK
000800070118     FFNBRV01L  IF   E           K DISK
000900020910     FFNARB78L  IF   E           K DISK
001000010316     FTNTBE01L  IF   E           K DISK
001100070510     ffidst01l  uf   e           k disk
001200010315     D DFAR          E DS
001300010315     D KPJBA         E DS
001400020910      *--------
001500020910      * Ds di riferimento al file esterno AzUte00F
001600020910      *--------
001700020910     d AzuteDs       e ds                  ExtName(AzUte00F)
001800020910      *--------
001900020910      * Ds per dati organigramma
002000020910      *--------
002100020910     d DDatiUte      e ds
002200020910      *--------
002300020910      * Parametri per richiamo al pgm di controllo profilo utenti
002400020910      *--------
002500020910     d Tibs34Ds      e ds                  Inz
002600020910      *--------
002700020910      * Ds di caricamento p.o. dell'area
002800020910      *--------
002900020910     d Trul06Ds      e ds
003000020910     d  Lin                    1     90  0 Dim(30)
003100140207
003200140207       //?Pulizia telefonate AUT
003300140207     d FNLRX4DS      e ds
003400010316
003500020910     d kfgs            s                   Like(fvvfgs)
003600020910     d knpg            s                   Like(fvvnpg)
003700020910     d karbifp         s                   Like(arbifp)
003800020910     d karbndc         s                   Like(arbndc)
003900070511     d kdstnfv         s                   like(dstnfv)
004000010316     D FAR             s              3s 0 dim(10)
004100010316     D II              S              1s 0 inz
004200020910     d xx              s              2s 0 inz
004300010316     D wbol            S              1
004400010316     D wspu            S              1
004500010316     D wormsic         S              1
004600010316     D wormnoc         S              1
004700010316     D w0060           S                   like(brvnfv)
004800010316     D w0070           S                   like(arbndc)
004900010316     D wnpg            S                   like(fvvnpg)
005000010316     c*
005100020910      * Ciclo all'interno della sk dei p.o. dell'area
005200020910     c                   do        30            xx
005300020910     c                   If        Lin(xx) = *Zeros
005400020910     c                   Leave
005500020910     c                   EndIf
005600020910     c                   Eval      Knpg = 4
005700020910     c                   Z-Add     Lin(xx)       Kfgs
005800010316     c* Leggo tutti i fogli di categoria 4-Consegna
005900020918     c     Kfvv3         setll     fnfvv03l
006000020918     c     Kfvv3         reade     fnfvv03l
006100020918    1c                   dow       not %eof(fnfvv03l)
006200010316
006300010316     c                   clear                   wbol
006400010316     c                   clear                   wspu
006500030714      * con data inferiore ad oggi
006600030714If  2c                   If        FvvDfv < *Date
006700010316     c* controllo se ci sono bolle
006800020910     c                   Z-Add     Lin(xx)       karbifp
006900020910     c                   Z-Add     Fvvnfv        Karbndc
007000020910     c     karb78        setll     fnarb78l                               31
007100010316     c   31              eval      wbol = 'S'
007200010316     c* controllo se ci sono spunte
007300010316     c                   eval      w0060 = fvvnfv
007400070118     c     kbrv          setll     fnbrv01l                               31
007500010316     c   31              eval      wspu = 'S'
007600010316     c* controllo se ci sono O.R.M.
007700010316    3c                   if        wbol = ' ' and wspu = ' '
007800010316     c                   clear                   wormnoc
007900010316     c                   clear                   wormsic
008000010316     c     korm          setll     fnorm06l
008100010316     c     korm          reade     fnorm06l
008200010316    4c                   dow       not %eof(fnorm06l) and wormnoc = *blanks
008300010316     c     ormfao        lookup    far                                    31
008400010316     c* memorizzo se presente orm in fase per cui non si pò chiudere il fo-
008500010316     c* glio
008600010316     c  n31              eval      wormnoc = 'S'
008700010316     c   31              eval      wormsic = 'S'
008800010316     c*
008900010316     c     korm          reade     fnorm06l
009000010316    4c                   enddo
009100010316     c* posso chiudere il foglio + relativo foglio di categoria 8
009200010316     c* se ho trovato almeno un orm in fase >= 900 e non ho trovato
009300010316     c* orm in altre fasi  (fasi per cui non posso chiudere il foglio)
009400010316    4c                   if        wormnoc = *blanks and wormsic = 'S'
009500010316     c                   exsr      chiufv
009600140207      * devo pulire anche il file delle telefonate
009700140207     c                   clear                   FNLRX4DS
009800140207     c                   eval      IX4fgs = FVVfgs
009900140207     c                   eval      IX4nfv = FVVnfv
010000140207     c                   call      'FNLRX4R'
010100140207     c                   parm                    kpjba
010200140207     c                   parm                    FNLRX4DS
010300010316    4c                   endif
010400030714    3c                   endif
010500030714    2c                   EndIf
010600020918     c     Kfvv3         reade     fnfvv03l
010700010316    1c                   enddo
010800020910
010900020910     c                   EndDo
011000010316     c*
011100010316     c                   seton                                        lr
011200010316     c*
011300010316     c**********************************************************************
011400010316     c     chiufv        begsr
011500010316     c*
011600010316     c* prima chiudo relativo foglio di categoria 8
011700010316     c                   eval      wnpg = 8
011800010316     c*
011900010316     c     leggi         tag
012000010316     c*
012100010316     c     kfvv          chain     fnfvv01l
012200010316     c                   if        %found(fnfvv01l)
012300010316     c                   eval      fvvfcf = 'S'
012400010316     c                   update    fnfvv000
012500010316     c                   endif
012600010316     c* ripeto per categoria 4
012700010316     c                   if        wnpg = 8
012800010316     c                   eval      wnpg = 4
012900010316     c                   goto      leggi
013000010316     c                   endif
013100070511
013200070510      * chiudo anche fidst se esite
013300070511     c                   eval      wnpg = 4
013400070511     c                   eval      kdstnfv = fvvnfv
013500070511     c     kfidst        chain     fidst01l
013600070511     c                   if        %found(fidst01l)
013700070511     c                   eval      dstfcf = 'S'
013800070511     c                   update    fidst000
013900070511     c                   endif
014000010316     c
014100010316     c                   endsr
014200010316     c*
014300010316     c**********************************************************************
014400010316     c     *inzsr        begsr
014500010316     c*
014600010316     C     *entry        plist
014700010316     C                   parm                    kpjba
014800020910
014900020910      * reperisco i dati utente
015000020910     c     *Dtaara       Define    §azute        AzuteDs
015100020910     c     *Dtaara       Define    §datiute      DDatiUte
015200020910
015300020910     c                   In(E)     *Dtaara
015400020910     c                   IF        %Error or RSUT = *Blanks
015500020910     c                   Call      'TIBS34R'
015600020910     c                   Parm                    Tibs34Ds
015700020910     c                   In        *Dtaara
015800020910     c                   EndIf
015900020910
016000020910      * Carico i p.o. dell'area
016100020910     c                   Clear                   Trul06Ds
016200020910     c                   Eval      D06Cod = '£1'
016300020910     c                   Movel     Simfel        D06Key
016400020910     c                   Movel(p)  Trul06Ds      Kpjbu
016500020910     c                   Call      'TRUL06R'
016600020910     c                   Parm                    Kpjba
016700020910     c                   Movel     Kpjbu         Trul06Ds
016800020910
016900020910     c     Karb78        Klist
017000020910     c                   Kfld                    Karbifp
017100020910     c                   Kfld                    Karbndc
017200010316     c*
017300010316     c     kbrv          klist
017400010316     c                   kfld                    fvvnpg
017500010316     c                   kfld                    w0060
017600010316     c                   kfld                    fvvfgs
017700010316     c*
017800010316     c     korm          klist
017900010316     c                   kfld                    fvvfgs
018000010316     c                   kfld                    w0060
018100010316     c*
018200020918     c     Kfvv3         Klist
018300020910     c                   Kfld                    Kfgs
018400020910     c                   Kfld                    Knpg
018500020910
018600010316     c     kfvv          klist
018700010316     c                   kfld                    wnpg
018800010316     c                   kfld                    fvvnfv
018900010316     c                   kfld                    fvvfgs
019000070511
019100070511     c     kfidst        klist
019200070511     c                   kfld                    wnpg
019300070511     c                   kfld                    kdstnfv
019400070511     c                   kfld                    fvvfgs
019500010316     c*
019600010316     c* carico schiera delle fasi O.R.M. che possono chiudere un foglio
019700010316     c                   clear                   far
019800010316     c     'FAR'         setll     tntbe01l
019900010316     c     'FAR'         reade     tntbe01l
020000010316    1c                   dow       not %eof(tntbe01l)
020100010316    2c                   if        tbeatb = *blanks and
020200010316     c                             (tbesif = knsif or tbesif = *blanks)
020300010316     c                   movel     tbeuni        dfar
020400010316    3c                   if        d§farcfvv = 'S'
020500010316     c                   add       1             II
020600010316     c                   movel     tbeke1        far(II)
020700010316    3c                   endif
020800010316    2c                   endif
020900010316     c     'FAR'         reade     tntbe01l
021000010316    1c                   enddo
021100010316     c*
021200010316     c                   endsr

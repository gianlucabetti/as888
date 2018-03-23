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
003100010316
003200020910     d kfgs            s                   Like(fvvfgs)
003300020910     d knpg            s                   Like(fvvnpg)
003400020910     d karbifp         s                   Like(arbifp)
003500020910     d karbndc         s                   Like(arbndc)
003600070511     d kdstnfv         s                   like(dstnfv)
003700010316     D FAR             s              3s 0 dim(10)
003800010316     D II              S              1s 0 inz
003900020910     d xx              s              2s 0 inz
004000010316     D wbol            S              1
004100010316     D wspu            S              1
004200010316     D wormsic         S              1
004300010316     D wormnoc         S              1
004400010316     D w0060           S                   like(brvnfv)
004500010316     D w0070           S                   like(arbndc)
004600010316     D wnpg            S                   like(fvvnpg)
004700010316     c*
004800020910      * Ciclo all'interno della sk dei p.o. dell'area
004900020910     c                   do        30            xx
005000020910     c                   If        Lin(xx) = *Zeros
005100020910     c                   Leave
005200020910     c                   EndIf
005300020910     c                   Eval      Knpg = 4
005400020910     c                   Z-Add     Lin(xx)       Kfgs
005500010316     c* Leggo tutti i fogli di categoria 4-Consegna
005600020918     c     Kfvv3         setll     fnfvv03l
005700020918     c     Kfvv3         reade     fnfvv03l
005800020918    1c                   dow       not %eof(fnfvv03l)
005900010316
006000010316     c                   clear                   wbol
006100010316     c                   clear                   wspu
006200030714      * con data inferiore ad oggi
006300030714If  2c                   If        FvvDfv < *Date
006400010316     c* controllo se ci sono bolle
006500020910     c                   Z-Add     Lin(xx)       karbifp
006600020910     c                   Z-Add     Fvvnfv        Karbndc
006700020910     c     karb78        setll     fnarb78l                               31
006800010316     c   31              eval      wbol = 'S'
006900010316     c* controllo se ci sono spunte
007000010316     c                   eval      w0060 = fvvnfv
007100070118     c     kbrv          setll     fnbrv01l                               31
007200010316     c   31              eval      wspu = 'S'
007300010316     c* controllo se ci sono O.R.M.
007400010316    3c                   if        wbol = ' ' and wspu = ' '
007500010316     c                   clear                   wormnoc
007600010316     c                   clear                   wormsic
007700010316     c     korm          setll     fnorm06l
007800010316     c     korm          reade     fnorm06l
007900010316    4c                   dow       not %eof(fnorm06l) and wormnoc = *blanks
008000010316     c     ormfao        lookup    far                                    31
008100010316     c* memorizzo se presente orm in fase per cui non si pò chiudere il fo-
008200010316     c* glio
008300010316     c  n31              eval      wormnoc = 'S'
008400010316     c   31              eval      wormsic = 'S'
008500010316     c*
008600010316     c     korm          reade     fnorm06l
008700010316    4c                   enddo
008800010316     c* posso chiudere il foglio + relativo foglio di categoria 8
008900010316     c* se ho trovato almeno un orm in fase >= 900 e non ho trovato
009000010316     c* orm in altre fasi  (fasi per cui non posso chiudere il foglio)
009100010316    4c                   if        wormnoc = *blanks and wormsic = 'S'
009200010316     c                   exsr      chiufv
009300010316    4c                   endif
009400030714    3c                   endif
009500030714    2c                   EndIf
009600020918     c     Kfvv3         reade     fnfvv03l
009700010316    1c                   enddo
009800020910
009900020910     c                   EndDo
010000010316     c*
010100010316     c                   seton                                        lr
010200010316     c*
010300010316     c**********************************************************************
010400010316     c     chiufv        begsr
010500010316     c*
010600010316     c* prima chiudo relativo foglio di categoria 8
010700010316     c                   eval      wnpg = 8
010800010316     c*
010900010316     c     leggi         tag
011000010316     c*
011100010316     c     kfvv          chain     fnfvv01l
011200010316     c                   if        %found(fnfvv01l)
011300010316     c                   eval      fvvfcf = 'S'
011400010316     c                   update    fnfvv000
011500010316     c                   endif
011600010316     c* ripeto per categoria 4
011700010316     c                   if        wnpg = 8
011800010316     c                   eval      wnpg = 4
011900010316     c                   goto      leggi
012000010316     c                   endif
012100070511
012200070510      * chiudo anche fidst se esite
012300070511     c                   eval      wnpg = 4
012400070511     c                   eval      kdstnfv = fvvnfv
012500070511     c     kfidst        chain     fidst01l
012600070511     c                   if        %found(fidst01l)
012700070511     c                   eval      dstfcf = 'S'
012800070511     c                   update    fidst000
012900070511     c                   endif
013000010316     c
013100010316     c                   endsr
013200010316     c*
013300010316     c**********************************************************************
013400010316     c     *inzsr        begsr
013500010316     c*
013600010316     C     *entry        plist
013700010316     C                   parm                    kpjba
013800020910
013900020910      * reperisco i dati utente
014000020910     c     *Dtaara       Define    §azute        AzuteDs
014100020910     c     *Dtaara       Define    §datiute      DDatiUte
014200020910
014300020910     c                   In(E)     *Dtaara
014400020910     c                   IF        %Error or RSUT = *Blanks
014500020910     c                   Call      'TIBS34R'
014600020910     c                   Parm                    Tibs34Ds
014700020910     c                   In        *Dtaara
014800020910     c                   EndIf
014900020910
015000020910      * Carico i p.o. dell'area
015100020910     c                   Clear                   Trul06Ds
015200020910     c                   Eval      D06Cod = '£1'
015300020910     c                   Movel     Simfel        D06Key
015400020910     c                   Movel(p)  Trul06Ds      Kpjbu
015500020910     c                   Call      'TRUL06R'
015600020910     c                   Parm                    Kpjba
015700020910     c                   Movel     Kpjbu         Trul06Ds
015800020910
015900020910     c     Karb78        Klist
016000020910     c                   Kfld                    Karbifp
016100020910     c                   Kfld                    Karbndc
016200010316     c*
016300010316     c     kbrv          klist
016400010316     c                   kfld                    fvvnpg
016500010316     c                   kfld                    w0060
016600010316     c                   kfld                    fvvfgs
016700010316     c*
016800010316     c     korm          klist
016900010316     c                   kfld                    fvvfgs
017000010316     c                   kfld                    w0060
017100010316     c*
017200020918     c     Kfvv3         Klist
017300020910     c                   Kfld                    Kfgs
017400020910     c                   Kfld                    Knpg
017500020910
017600010316     c     kfvv          klist
017700010316     c                   kfld                    wnpg
017800010316     c                   kfld                    fvvnfv
017900010316     c                   kfld                    fvvfgs
018000070511
018100070511     c     kfidst        klist
018200070511     c                   kfld                    wnpg
018300070511     c                   kfld                    kdstnfv
018400070511     c                   kfld                    fvvfgs
018500010316     c*
018600010316     c* carico schiera delle fasi O.R.M. che possono chiudere un foglio
018700010316     c                   clear                   far
018800010316     c     'FAR'         setll     tntbe01l
018900010316     c     'FAR'         reade     tntbe01l
019000010316    1c                   dow       not %eof(tntbe01l)
019100010316    2c                   if        tbeatb = *blanks and
019200010316     c                             (tbesif = knsif or tbesif = *blanks)
019300010316     c                   movel     tbeuni        dfar
019400010316    3c                   if        d§farcfvv = 'S'
019500010316     c                   add       1             II
019600010316     c                   movel     tbeke1        far(II)
019700010316    3c                   endif
019800010316    2c                   endif
019900010316     c     'FAR'         reade     tntbe01l
020000010316    1c                   enddo
020100010316     c*
020200010316     c                   endsr

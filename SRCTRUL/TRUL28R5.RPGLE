000100060421     h decedit('0,') datedit(*dmy/)
000200060421
000300060421      ****************************************************************
000400060421      ** TRUL28R ** Calcolo del CHECK DIGIT :                       **
000500060421      **            ISO/IES 7064 mod. 27/36                         **
000600060421      ****************************************************************
000700060421
000800060421      *
000900060421      * COSTANTI
001000060421      *
001100060421      * Modello   &   Valore iniziale del Check-Digit
001200060421     d $Mod            c                   const(36)
001300060421      * Costante per controllo "caratteri ammessi"
001400060421     d Digits          c                   const('0123456789+
001500060421     d                                     ABCDEFGHIJKLMNOPQRSTUVWXYZ')
001600060421
001700060421      *
001800060421      * SCHIERE
001900060421      *
002000060421     d $Char           s              1    dim(36) ctdata perrcd(36)
002100060421
002200060421      *
002300060421      * DS definite ESTERNAMENTE
002400060421      *
002500060421     d TRUL28ds4     e ds
002600060421
002700060421      *
002800060421      * DS definite INTERNAMENTE
002900060421      *
003000060421
003100060421      *
003200060421      * VARIABILI
003300060421      *
003400060502     d $CDnr           s              2  0 inz
003500060421     d $Val            s              2  0 inz
003600060502     d wChar           s              1    inz
003700060421     d wLen            s              2  0 inz
003800060421     d XX              s              2  0 inz
003900060421
004000060421      ****************************************************************
004100060428      * INDICATORI UTILIZZATI                                        *
004200060428      ****************************************************************
004300060428      *                                                              *
004400060428      * 10 - Comodo                                                  *
004500060428      *                                                              *
004600060428      ****************************************************************
004700060421
004800060421      * Operazioni iniziali
004900060421     c                   exsr      OpeIni
005000060421      *
005100060421      * Controllo dati
005200060421     c                   exsr      CtrM3736
005300060421      *
005400060421      * Calcolo Check Digit
005500060421cas 1c     O284err       caseq     *off          ChkDgt
005600060421e   1c                   endcs
005700060421      *
005800060421      * Fine
005900060421     c                   movel     *on           *inLR
006000060421
006100060421      ****************************************************************
006200060421      ** OPEINI ** Operazioni Iniziali                              **
006300060421      ****************************************************************
006400060421     c     OpeIni        BEGSR
006500060421      *
006600060421     c     *Entry        plist
006700060421     c                   parm                    TRUL28ds4
006800060421      *
006900060421      * Inizializzazione campi di output
007000060421     c                   clear                   O284cod
007100060421     c                   clear                   O284ckd
007200060421     c                   eval      O284err = *off
007300060421      *
007400060421     c                   ENDSR
007500060421
007600060421      ****************************************************************
007700060421      ** CtrM3736   ** Controlli standard su ISO 7064 mod. 37/36    **
007800060421      **               (Segnacollo DPD)                             **
007900060421      ****************************************************************
008000060421     c     CtrM3736      BEGSR
008100060421      *
008200060421if  1c                   if        I284cod = *blanks
008300060421     c                   eval      O284err = *on
008400060421     c                   leavesr
008500060421e   1c                   endif
008600060421      *
008700060421      * Calcolo lunghezza del codice di cui calcolare il check-digit
008800060421     c                   eval      wLen    = %len(%trimr(I284cod))
008900060421      *
009000060421      * Verifica che non ci siano caratteri "speciali"
009100060428     c                   eval      *in10   =  *off
009200060428     c     Digits        checkr    I284cod : wLen                         10
009300060428if  1c                   if        *in10
009400060421     c                   eval      O284err = *on
009500060421     c                   leavesr
009600060421e   1c                   endif
009700060421      *
009800060421     c                   ENDSR
009900060421
010000060421      ****************************************************************
010100060421      ** CHKDGT ** Calcolo CHECK DIGIT                              **
010200060421      ****************************************************************
010300060421     c     ChkDgt        BEGSR
010400060421      *
010500060502     c                   eval      $CDnr   = $Mod
010600060421     c                   clear                   xx
010700060421      *
010800060421do  1c                   dow       xx      < wLen
010900060421     c                   add       1             xx
011000060421     c                   exsr      M3736
011100060421     c                   if        O284err = *on
011200060421     c                   leave
011300060421     c                   endif
011400060421e   1c                   enddo
011500060421      *
011600060502     c                   eval      $CDnr   = $Mod + 1 - $CDnr
011700060502     c                   if        $CDnr   = $Mod
011800060502     c                   eval      $CDnr   = *zeros
011900060421     c                   endif
012000060421      *
012100060502     c                   eval      O284ckd = $Char($CDnr + 1)
012200060421     c                   eval      O284cod = %trimr(I284cod)
012300060421     c                                     + O284ckd
012400060421      *
012500060421     c                   ENDSR
012600060421
012700060421      ****************************************************************
012800060421      ** M3736  ** Calcolo cifra di controllo secondo mod. 37/36    **
012900060421      ****************************************************************
013000060421     c     M3736         BEGSR
013100060421      *
013200060502     c                   eval      wChar   = %subst(I284cod : xx : 1)
013300060421     c                   eval      $Val    = 1
013400060421     c                   eval      *in10   = *off
013500060502     c     wChar         lookup    $Char($Val)                            10
013600060421if  1c                   if        NOT *in10
013700060421     c                   eval      O284err = *on
013800060421     c                   leavesr
013900060421e   1c                   endif
014000060502     c                   eval      $Val    = $Val  - 1
014100060421      *
014200060502     c                   eval      $CDnr   = $CDnr + $Val
014300060421      *
014400060502if  1c                   if        $CDnr   > $Mod
014500060502     c                   eval      $CDnr   = $CDnr - $Mod
014600060421e   1c                   endif
014700060421      *
014800060502     c                   eval      $CDnr   = $CDnr * 2
014900060421      *
015000060502if  1c                   if        $CDnr   > $Mod
015100060502     c                   eval      $CDnr   = $CDnr - $Mod - 1
015200060421e   1c                   endif
015300060421      *
015400060421     c                   ENDSR
015500060421
015600060421      *
015700060421      * SCHIERE A TEMPO DI COMPILAZIONE
015800060421      *
015900060421** $Char
0160000604210123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ

000100010911      *
000200010907      *  Routine editazione di un numero
000300010907      * ---------------------------------
000400010907      *
000500010907      *  Errori previsti (OedFlgErr <> *off) :
000600010907      *  >>>>>> Errori bloccanti:
000700010907      *  '1' = Codice di editazione errato o mancante
000800010910      *  '2' = Lunghezza del campo di output errato (min 0, max 50)
000900010910      *  '3' = Numero dei decimali errato (min 0, max 30)
001000010907      *  '4' = Errore nel richiamo della prima api (QECCVTEC)
001100010907      *  '5' = Errore nel richiamo della seconda api (QECEDT)
001200010907      *  >>>>>> Errori non bloccanti:
001300010907      *  'A' = Rilevato errore dalla prima api (QECCVTEC)
001400010907      *  'B' = Rilevato errore dalla seconda api (QECEDT)
001500010907      *  'C' = Lunghezza minima richiesta superiore a quella indicata
001600010907
001700010911     H DECEDIT('0,') DATEDIT(*DMY.)
001800010911
001900010907      * Parametri I/O
002000010907     D XEDITds       e ds
002100010907      * Parametri aggiuntivi QECCVTEC/QECEDT
002200010907     D $EdtMask        s            256    inz
002300010907     D $EdtMaskL       s              9B 0 inz
002400010907     D $NumAnL         s              9B 0 inz
002500010907     D $ZBalFill       s              1    inz
002600010907     D $Floating       s              1    inz
002700010907     D $SrcVarCl       s             10    inz
002800010907     D $SrcVarPr       s              9B 0 inz(30)
002900010907     D $ErrorCod       s              9B 0 inz
003000010907
003100010907      * Campi di lavoro
003200010907     D SavEdtCode      s                   like(IedEdtCod) inz
003300010907     D SavNrDec        s                   like(IedNrDec)  inz
003400010907     D SavPrecis       s                   like($SrcVarPr) inz
003500010907     D Comodo          s                   like(OedNumAN)  inz
003600010907     D ComodoL         s                   like($NumAnL)   inz
003700010907     D PosIn           s              3S 0 inz
003800010907     D $X              s              3S 0 inz
003900010907
004000010907      * Impostazione iniziale parametri
004100010907     C                   Exsr      InzVar
004200010907
004300010907      * Controllo parametri
004400010907     C                   Exsr      CtrlParms
004500010907
004600010907      * Call prima api
004700010907      * Solo se parametri variati
004800010907     C                   If        IedEdtCod  <> SavEdtCode or
004900010907     C                             IedNrDec   <> SavNrDec   or
005000010907     C                             $SrcVarPr  <> SavPrecis
005100010907     C                   exsr      Prima
005200010907     C                   eval      SavEdtCode =  IedEdtCod
005300010907     C                   eval      SavNrDec   =  IedNrDec
005400010907     C                   eval      SavPrecis  =  $SrcVarPr
005500010907     C                   Endif
005600010907
005700010907      * Se importo ricevuto = 0 imposto il valore senza chiamare API
005800010907     C                   If        IedNumero = 0
005900010907     C                   exsr      ImpZero
006000010907     C                   else
006100010907      * Call seconda api
006200010907     C                   Exsr      Seconda
006300010907     C                   Endif
006400010907
006500010907      * Se richiesto riposiziono la stringa di output
006600010907     C                   If        IedLenFld > 0
006700010907     C                   Exsr      Output
006800010907     C                   Endif
006900010907
007000010907      * Fine programma
007100010907     C                   Exsr      Endpgm
007200010907
007300010907      *----------------------------------------------------
007400010907      * Inizializzazione variabili
007500010907      *----------------------------------------------------
007600010907     C     InzVar        Begsr
007700010907      * Impostazione iniziale parametri di output
007800010907     C                   eval      OedNumAN  = *blanks
007900010907     C                   eval      OedFlgErr = *off
008000010907     C                   eval      OedCodErr = *zeros
008100010907      * Impostazione iniziale parametri api
008200010907     C                   eval      $ErrorCod = *zeros
008300010907      * Impostazione dati "fissi"
008400010907     C                   reset                   $SrcVarPr
008500010907     C                   Endsr
008600010907      *----------------------------------------------------
008700010907      * Controllo parametri passati
008800010907      *----------------------------------------------------
008900010907     C     CtrlParms     Begsr
009000010907     C                   Select
009100010907     C                   When      IedEdtCod = *blanks
009200010907     C                   eval      OedFlgErr = '1'
009300010910     C                   When      IedLenFld <  0   or
009400010910     C                             IedLenFld > 50
009500010907     C                   eval      OedFlgErr = '2'
009600010910     C                   When      IedNrDec  <  0   or
009700010910     C                             IedNrDec  > 30
009800010907     C                   eval      OedFlgErr = '3'
009900010907     C                   EndSl
010000010907     C*
010100010907     C     OedFlgErr     casNE     *off          EndPgm
010200010907     C                   EndCs
010300010907     C                   Endsr
010400010907      *----------------------------------------------------
010500010907      * Call prima api
010600010911      * Dato        il codice   di editazione
010700010911      * Restituisce la maschera di editazione
010800010907      *----------------------------------------------------
010900010907     C     Prima         Begsr
011000010907     C                   Call      'QECCVTEC'                           99
011100010907     C                   Parm                    $EdtMask
011200010907     C                   Parm                    $EdtMaskL
011300010907     C                   Parm                    $NumAnL
011400010907     C                   Parm      *blanks       $ZBalFill
011500010907     C                   Parm                    IedEdtCod
011600010907     C                   Parm      *blanks       $Floating
011700010907     C                   Parm                    $SrcVarPr
011800010907     C                   Parm                    IedNrDec
011900010907     C                   Parm      *zeros        $ErrorCod
012000010907     C*
012100010907     C                   Select
012200010907     C                   When      *in99
012300010907     C                   eval      OedFlgErr  =  '4'
012400010907     C                   Exsr      Endpgm
012500010907     C                   When      $ErrorCod  <> *zeros
012600010907     C                   eval      OedFlgErr  =  'A'
012700010907     C                   Endsl
012800010907     C                   Endsr
012900010907      *----------------------------------------------------
013000010907      * Call seconda api
013100010911      * Data        la maschera di editazione
013200010911      * Restituisce il numero   editato
013300010907      *----------------------------------------------------
013400010907     C     Seconda       Begsr
013500010907     C                   Call      'QECEDT'                             99
013600010907     C                   Parm                    OedNumAN
013700010907     C                   Parm                    $NumAnL
013800010907     C                   Parm                    IedNumero
013900010907     C                   Parm      '*ZONED'      $SrcVarCl
014000010907     C                   Parm                    $SrcVarPr
014100010907     C                   Parm                    $EdtMask
014200010907     C                   Parm                    $EdtMaskL
014300010907     C                   Parm      *blanks       $ZBalFill
014400010907     C                   Parm      *zeros        $ErrorCod
014500010907     C*
014600010907     C                   Select
014700010907     C                   When      *in99
014800010907     C                   eval      OedFlgErr  =  '5'
014900010907     C                   Exsr      Endpgm
015000010907     C                   When      $ErrorCod  <> *zeros
015100010907     C                   eval      OedFlgErr  =  'B'
015200010907     C                   Endsl
015300010907     C                   Endsr
015400010907      *----------------------------------------------------
015500010907      * Imposta gli zeri
015600010907      *----------------------------------------------------
015700010907     C     ImpZero       Begsr
015800010907     C                   Clear                   OedNumAN
015900010907     C                   Clear                   Comodo
016000010907      * Imposto la dimensione del campo video in modo da allineare
016100010907      * valute gestite con un numero diverso di decimali
016200010907     C                   Select
016300010907     C                   When      IedNrDec = 30
016400010907     C                   Eval      Comodo   = '0,000000000000000000000000000000'
016500010907     C                   Eval      ComodoL  = 32
016600010907     C                   When      IedNrDec = 29
016700010907     C                   Eval      Comodo   = '0,00000000000000000000000000000'
016800010907     C                   Eval      ComodoL  = 31
016900010907     C                   When      IedNrDec = 28
017000010907     C                   Eval      Comodo   = '0,0000000000000000000000000000'
017100010907     C                   Eval      ComodoL  = 30
017200010907     C                   When      IedNrDec = 27
017300010907     C                   Eval      Comodo   = '0,000000000000000000000000000'
017400010907     C                   Eval      ComodoL  = 29
017500010907     C                   When      IedNrDec = 26
017600010907     C                   Eval      Comodo   = '0,00000000000000000000000000'
017700010907     C                   Eval      ComodoL  = 28
017800010907     C                   When      IedNrDec = 25
017900010907     C                   Eval      Comodo   = '0,0000000000000000000000000'
018000010907     C                   Eval      ComodoL  = 27
018100010907     C                   When      IedNrDec = 24
018200010907     C                   Eval      Comodo   = '0,000000000000000000000000'
018300010907     C                   Eval      ComodoL  = 26
018400010907     C                   When      IedNrDec = 23
018500010907     C                   Eval      Comodo   = '0,00000000000000000000000'
018600010907     C                   Eval      ComodoL  = 25
018700010907     C                   When      IedNrDec = 22
018800010907     C                   Eval      Comodo   = '0,0000000000000000000000'
018900010907     C                   Eval      ComodoL  = 24
019000010907     C                   When      IedNrDec = 21
019100010907     C                   Eval      Comodo   = '0,000000000000000000000'
019200010907     C                   Eval      ComodoL  = 23
019300010907     C                   When      IedNrDec = 20
019400010907     C                   Eval      Comodo   = '0,00000000000000000000'
019500010907     C                   Eval      ComodoL  = 22
019600010907     C                   When      IedNrDec = 19
019700010907     C                   Eval      Comodo   = '0,0000000000000000000'
019800010907     C                   Eval      ComodoL  = 21
019900010907     C                   When      IedNrDec = 18
020000010907     C                   Eval      Comodo   = '0,000000000000000000'
020100010907     C                   Eval      ComodoL  = 20
020200010907     C                   When      IedNrDec = 17
020300010907     C                   Eval      Comodo   = '0,00000000000000000'
020400010907     C                   Eval      comodoL  = 19
020500010907     C                   When      IedNrDec = 16
020600010907     C                   Eval      Comodo   = '0,0000000000000000'
020700010907     C                   Eval      ComodoL  = 18
020800010907     C                   When      IedNrDec = 15
020900010907     C                   Eval      Comodo   = '0,000000000000000'
021000010907     C                   Eval      ComodoL  = 17
021100010907     C                   When      IedNrDec = 14
021200010907     C                   Eval      Comodo   = '0,00000000000000'
021300010907     C                   Eval      ComodoL  = 16
021400010907     C                   When      IedNrDec = 13
021500010907     C                   Eval      Comodo   = '0,0000000000000'
021600010907     C                   Eval      ComodoL  = 15
021700010907     C                   When      IedNrDec = 12
021800010907     C                   Eval      Comodo   = '0,000000000000'
021900010907     C                   Eval      ComodoL  = 14
022000010907     C                   When      IedNrDec = 11
022100010907     C                   Eval      Comodo   = '0,00000000000'
022200010907     C                   Eval      ComodoL  = 13
022300010907     C                   When      IedNrDec = 10
022400010907     C                   Eval      Comodo   = '0,0000000000'
022500010907     C                   Eval      ComodoL  = 12
022600010907     C                   When      IedNrDec = 9
022700010907     C                   Eval      Comodo   = '0,000000000'
022800010907     C                   Eval      ComodoL  = 11
022900010907     C                   When      IedNrDec = 8
023000010907     C                   Eval      Comodo   = '0,00000000'
023100010907     C                   Eval      ComodoL  = 10
023200010907     C                   When      IedNrDec = 7
023300010907     C                   Eval      Comodo   = '0,0000000'
023400010907     C                   Eval      ComodoL  = 9
023500010907     C                   When      IedNrDec = 6
023600010907     C                   Eval      Comodo   = '0,000000'
023700010907     C                   Eval      ComodoL  = 8
023800010907     C                   When      IedNrDec = 5
023900010907     C                   Eval      Comodo   = '0,00000'
024000010907     C                   Eval      ComodoL  = 7
024100010907     C                   When      IedNrDec = 4
024200010907     C                   Eval      Comodo   = '0,0000'
024300010907     C                   Eval      ComodoL  = 6
024400010907     C                   When      IedNrDec = 3
024500010907     C                   Eval      Comodo   = '0,000'
024600010907     C                   Eval      ComodoL  = 5
024700010907     C                   When      IedNrDec = 2
024800010907     C                   Eval      Comodo   = '0,00'
024900010907     C                   Eval      ComodoL  = 4
025000010907     C                   When      IedNrDec = 1
025100010907     C                   Eval      Comodo   = '0,0'
025200010907     C                   Eval      ComodoL  = 3
025300010907     C                   When      IedNrDec = 0
025400010907     C                   Eval      Comodo   = '0'
025500010907     C                   Eval      ComodoL  = 1
025600010907     C                   Endsl
025700010907      *
025800010907     C                   Eval      PosIn = $NumAnL - ComodoL + 1
025900010907     C                   If        IedEdtCod = 'K' or
026000010907     C                             IedEdtCod = 'L' or
026100010907     C                             IedEdtCod = 'M' or
026200010907     C                             IedEdtCod = 'J'
026300010907     C                   If        Posin > 1
026400010907     C                   Eval      Posin = Posin - 1
026500010907     C                   Endif
026600010907     C                   Endif
026700010907     C                   Eval      %subst(OedNumAN:PosIn:ComodoL) = Comodo
026800010907      *
026900010907     C                   Endsr
027000010907      *----------------------------------------------------
027100010907      * Risistemo la stringa di output se richiesto
027200010907      *----------------------------------------------------
027300010907     C     Output        Begsr
027400010907      * Verifica spazio richiesto
027500010907     C                   exsr      CtrlOut
027600010907      * Calcolo posizione iniziale x eseguire subst
027700010907     C                   clear                   Comodo
027800010907     C                   Select
027900010907      * Posizione finale superiore alla lunghezza richiesta
028000010907     C                   When      IedLenFld < $NumAnL
028100010907     C                   eval      PosIn = $NumAnL - IedLenFld + 1
028200010907     C                   eval      Comodo = %subst(OedNumAN:PosIn:IedLenFld)
028300010907      * Posizione finale inferiore alla lunghezza richiesta
028400010907     C                   When      IedLenFld > $NumAnL
028500010907     C                   eval      PosIn = IedLenFld - $NumAnL + 1
028600010907     C                   eval      %subst(Comodo:PosIn:$NumAnL) = OedNumAN
028700010907     C                   Endsl
028800010907      *
028900010907     C                   clear                   OedNumAN
029000010907     C                   eval      OedNumAN = Comodo
029100010907      *
029200010907     C                   Endsr
029300010907      *----------------------------------------------------
029400010907      * Verifica risultato di output
029500010907      *----------------------------------------------------
029600010907     C     CtrlOut       Begsr
029700010907     C                   clear                   Comodo
029800010907     C     ' '           check     OedNumAN      $X
029900010907     C                   eval      Comodo    = %trim(%subst(OedNumAN:$X))
030000010907     C     ' '           checkr    Comodo        $X
030100010907     C                   If        $X > IedLenFld
030200010907     C                   eval      OedFlgErr = 'C'
030300010907     C                   Endif
030400010907     C                   Endsr
030500010907      *----------------------------------------------------
030600010907      * Fine programma
030700010907      *----------------------------------------------------
030800010907     C     Endpgm        Begsr
030900010907     C                   eval      OedCodErr  =  $ErrorCod
031000010907     C                   Return
031100010907     C                   Endsr
031200010907      *----------------------------------------------------
031300010907      * routine iniziale
031400010907      *----------------------------------------------------
031500010907     C     *INZSR        Begsr
031600010907     C     *ENTRY        PLIST
031700010907     C                   Parm                    XEDITDS
031800010907     C                   Endsr

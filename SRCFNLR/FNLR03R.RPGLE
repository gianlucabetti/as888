000100040317      * FNLR03R *----------------------------------------------------*
000200040317      *           PREPARA FILE NUOVI FOGLI VARI PER P.C.             *
000300040317      *--------------------------------------------------------------*
000400040317
000500040317     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600040317
000700040317     fFNFVV03L  if   e           k disk
000800040317     fAZORG01L  if   e           k disk
000900040317     fFLTR200F  o    e             disk    usropn
001000040317
001100040317      *
001200040317      * === COSTANTI =============================================== *
001300040317      *
001400040317      *
001500040317      * === SCHIERE A TEMPO DI COMPILAZIONE ======================== *
001600040317      *
001700040318     d $Cmd            s             80    dim(5)  ctdata perrcd(1)
001800040317      *
001900040317      * === SCHIERE A TEMPO DI ESECUZIONE ========================== *
002000040317      *
002100040317      * P.O. gestiti
002200040317     d L1              s              3  0 dim(30)
002300040317      *
002400040317      * === STRUTTURE DATI DEFINITE ESTERNAMENTE =================== *
002500040317      *
002600040317      * Passaggio Parametri
002700040317     d KPJBA         e ds
002800040317      * DS per TIBS55R - Reperimento dati per trasmissioni
002900040317     d TIBS55DS      e ds
003000040317      * DS per TRUL06R - Caricamento £1 (P.O. gestiti)
003100040317     d TRUL06DS      e ds
003200040317     d  LIN                    1     90  0 dim(30)
003300040317      *
003400040317      * === STRUTTURE DATI DEFINITE INTERNAMENTE =================== *
003500040317      *
003600040317      * === VARIABILI ============================================== *
003700040317      *
003800040317      * P.O. passato come unico parametro nella KPJBU
003900040319     d wPOa            s              3    inz
004000040319     d wPO             s              3  0 inz
004100040317      * SimFel di wPO
004200040317     d SIMfel          s              3  0 inz
004300040317      * Campi di comodo
004400040322     d wFxxx           s             10    inz
004500040319     d wF00F           s             10    inz
004600040317      * Parametri per pgm QCMDEXC
004700040318     d Qcmd            s             80    inz
004800040318     d Qlen            s             15  5 inz(80)
004900040317      * Flag
005000040318     d $Clr            s              1    inz(*off)
005100040318     d $Crt            s              1    inz(*off)
005200040318     d $Alc            s              1    inz(*off)
005300040317     d $NoPF2          s              1    inz(*off)
005400040322     D $Almeno1        S              1    inz(*off)
005500040317
005600040317      *--------------------------------------------------------------*
005700040317      * === MAIN LINE PROGRAM ====================================== *
005800040317      *--------------------------------------------------------------*
005900040317
006000040318     c     *Entry        plist
006100040318     c                   parm                    KPJBA
006200040318      *
006300040318     c                   exsr      OperazIniz
006400040318      *
006500040317      * Controllo quali file esistono e ne faccio una CLEAR
006600040322if  1c                   if        wPO =  SIMfel
006700040322     c                   exsr      sr_FEL_1
006800040322e   1c                   endif
006900170801      * Se il file risulta già allocato è bene NON
007000040317      *  proseguire, perchè sarebbe in uso da questo stesso
007100040317      *  programma...
007200040318     c     $Alc          cabeq     *on           Fine
007300040317      *
007400040319      *
007500040322      * Elaboro:
007600170801      * ?prima?per SIMfel per: scrivere il file FLTR2xxx
007700170802if  1c                   if        wPO =  SIMfel
007800040317     c                   exsr      sr_FEL_2
007900170802e   1c                   endif
008000040317      *
008100040322      * ?poi?per il P.O. passato: sempre file XXX (SIMfel esclusa)
008200040322if  1c                   if        wPO <> SIMfel
008300040319     c                   exsr      sr_PO2_2
008400040322e   1c                   endif
008500040317      *
008600040317      * Fine
008700040317     c     Fine          tag
008800040317     c                   eval      *inLR = *on
008900040317
009000040317      *---------------------------------------------------------------*
009100040317      * Operazioni iniziali                                           *
009200040317      *---------------------------------------------------------------*
009300040318     c     OperazIniz    BEGSR
009400040317      *
009500040319     c                   clear                   wPO
009600040322     c                   clear                   wPOa
009700040319if  1c                   if        %subst(KPJBU:1:3) > *zeros
009800040322     c                   movel     KPJBU         wPOa
009900040319e   1c                   endif
010000040322     c     wPOa          cabLE     *zeros        Fine
010100040322     c                   movel     wPOa          wPO
010200040317      *
010300040317      * Richiamo TRUL06R con "PP" & il P.O. ricevuto
010400040317     c                   clear                   TRUL06DS
010500040317     c                   movel     'PP'          D06cod
010600040317     c                   movel     'L'           D06tla
010700040317     c                   movel     wPO           D06key
010800040317     c                   movel(p)  TRUL06DS      KPJBU
010900040317     c                   call      'TRUL06R'
011000040317     c                   parm                    KPJBA
011100040317     c                   movel     KPJBU         TRUL06DS
011200040317if  1c                   if        D06err = *blanks
011300040317     c                   movea     LIN           L1
011400040317e   1c                   endif
011500040317      * Se non è stato reperito nemmeno un P.O. da elaborare è inutile
011600040317      *  proseguire...
011700040317     c     L1(1)         cabeq     0             Fine
011800040317      *
011900040317     c                   eval      SIMfel = L1(1)
012000040317      *
012100040317      * Reperisco il S.I. in cui sono e la relativa libreria dati
012200040317     c                   clear                   TIBS55DS
012300040317     c                   eval      I50tla = 'L'
012400040317     c                   call      'TIBS55R'
012500040317     c                   parm                    TIBS55DS
012600040319      *
012700040317     c                   ENDSR
012800040317
012900040317      *---------------------------------------------------------------*
013000040318      * Operazioni iniziali per SIMFEL                                *
013100040317      *---------------------------------------------------------------*
013200040317     c     sr_FEL_1      BEGSR
013300040317      *
013400040318     c                   eval      $Clr   = *on
013500040318     c                   eval      $Crt   = *on
013600040317      *
013700040317      * Controllo se esiste il file FLTR2 per SIMfel
013800040317      * - Lo ripulisco se esistente - CLRPFM   &
013900040317      * - Lo creo se inesistente    - CTRDUPOBJ
014000040318      * file FLTR2:
014100040319     c                   eval      wFxxx  = 'FLTR2' + wPOa
014200040318     c                   eval      wF00F  = 'FLTR200F  '
014300040318     c                   exsr      Ctr_Clr_PF
014400040318     c                   eval      $NoPF2 = *in40
014500040317      * Se non riesce la pulizia significa che il file è in uso !
014600040317      * ---------------------------------------------------------
014700040317     c                   if        $NoPF2 = *on
014800040318     c                   eval      $Alc   = *on
014900040317     c                   goto      EndFEL_1
015000040317     c                   endif
015100040317      *
015200040317     c     EndFEL_1      ENDSR
015300040317
015400040317      *---------------------------------------------------------------*
015500170801      * Lettura file FNFVV e scrittura file FLTR2 per SIMfel          *
015600040317      *---------------------------------------------------------------*
015700040317     c     sr_FEL_2      BEGSR
015800040317      *
015900040319      * Override:
016000040322      * -per FLTR2xxx?
016100040319     c                   eval      wF00F  = 'FLTR200F  '
016200040319     c                   eval      wFxxx  = 'FLTR2' + wPOa
016300040319     c                   eval      Qcmd   = %replace(wF00F:$Cmd(4):
016400040319     c                                      %scan('&FILE00F  ':
016500040319     c                                            $Cmd(4)))
016600040319     c                   eval      Qcmd   = %replace(wFxxx:Qcmd:
016700040319     c                                      %scan('&FILE     ':
016800040319     c                                            Qcmd))
016900040319     c                   call      'QCMDEXC'
017000040319     c                   parm                    Qcmd
017100040319     c                   parm                    Qlen
017200040319      *
017300040317     c                   open      FLTR200F
017400040319      *
017500040322      * Elaborazione:
017600040322     c     SIMfel        setll     FNFVV000
017700040322     c     SIMfel        reade     FNFVV000
017800040317      *
017900040319do  1c                   DOW       not %eof(FNFVV03L)
018000040317      *
018100040317      * Elaboro soltanto se è da scaricare
018200040319if  2c                   IF        FVVfsc = 'S'
018300040317      *
018400170802      * - Scrivo FLTR2
018500040319     c                   exsr      Write_TR2
018600040317      *
018700040319e   2c                   ENDIF
018800040317      *
018900040322     c     SIMfel        reade     FNFVV000
019000040319e   1c                   ENDDO
019100040317      *
019200040322      * Fine:
019300040322      *
019400040317     c                   close     FLTR200F
019500040319      *
019600040319      * Cancellazione override (tutte)
019700040319     c                   eval      wF00F  = '*ALL      '
019800040319     c                   eval      Qcmd   = %replace(wF00F:$Cmd(5):
019900040319     c                                      %scan('&FILE00F  ':
020000040319     c                                            $Cmd(5)))
020100040319     c                   call      'QCMDEXC'
020200040319     c                   parm                    Qcmd
020300040319     c                   parm                    Qlen
020400040317      *
020500040317     c                   ENDSR
020600040317
020700040317      *---------------------------------------------------------------*
020800170801      * Lettura file FNFVV e scrittura file FLTR2 per Poste           *
020900040317      *---------------------------------------------------------------*
021000040317     c     sr_PO2_2      BEGSR
021100040317      *
021200170801      * Override al file FLTR2xxx
021300170801     c                   exsr      New_TR2
021400040317      *
021500040319      * Elaborazione
021600040322     C                   eval      $Almeno1  =  *off
021700040319     c     wPO           setll     FNFVV000
021800040319     c     wPO           reade     FNFVV000
021900040317      *
022000040318do  1c                   DOW       not %eof(FNFVV03L)
022100040317      *
022200040319      * - Elaboro soltanto se è da scaricare
022300040319if  2c                   IF        FVVfsc =  'S'
022400040317      *
022500170801      * - - Scrivo il record al file FLTR2
022600040319     c                   exsr      Write_TR2
022700040322     C                   eval      $Almeno1 = *on
022800040317      *
022900040318e   2c                   ENDIF
023000040317      *
023100040322     c     wPO           reade     FNFVV000
023200040318      *
023300040318e   1c                   ENDDO
023400040322      *
023500040322     c                   close     FLTR200F
023600040322      *
023700040322      * Cancellazione override (tutte)
023800040322     c                   eval      wF00F  = '*ALL      '
023900040322     c                   eval      Qcmd   = %replace(wF00F:$Cmd(5):
024000040322     c                                      %scan('&FILE00F  ':
024100040322     c                                            $Cmd(5)))
024200040322     c                   call      'QCMDEXC'
024300040322     c                   parm                    Qcmd
024400040322     c                   parm                    Qlen
024500040317      *
024600040317     c                   ENDSR
024700040319
024800040319      *---------------------------------------------------------------*
024900040319      * Controllo se esiste il file (vedi parametro) WFXXX            *
025000040319      * - Lo ripulisco se esistente - CLRPFM   &                      *
025100040319      * - Lo creo se inesistente    - CRTDUPOBJ                       *
025200040319      *---------------------------------------------------------------*
025300040319     c     Ctr_Clr_PF    BEGSR
025400040319      *
025500040319     c                   eval      *in40 = *off
025600040319      *
025700040319      * Controllo se esiste
025800040319     c                   eval      Qcmd  = %replace(wFxxx:$Cmd(1):
025900040319     c                                     %scan('&FILE     ':
026000040319     c                                           $Cmd(1)))
026100040319     c                   call      'QCMDEXC'                            40
026200040319     c                   parm                    Qcmd
026300040319     c                   parm                    Qlen
026400040319      *
026500040319sel 1c                   select
026600040319      * - Lo ripulisco se esistente e se richiesto
026700040319w   1c                   when          *in40 = *off
026800040319     c                             and $Clr  = *on
026900040319     c                   eval      Qcmd  = %replace(wFxxx:$Cmd(2):
027000040319     c                                     %scan('&FILE     ':
027100040319     c                                           $Cmd(2)))
027200040319      * - Lo creo se inesistente
027300040319w   1c                   when          *in40 = *on
027400040319     c                             and $Crt  = *on
027500040319     c                   eval      Qcmd  = %replace(wF00F:$Cmd(3):
027600040319     c                                     %scan('&FILE00F  ':
027700040319     c                                           $Cmd(3)))
027800040319     c                   eval      Qcmd  = %replace(O50pla:Qcmd:
027900040319     c                                     %scan('&FLIB     ':
028000040319     c                                           Qcmd))
028100040319     c                   eval      Qcmd  = %replace(wFxxx:Qcmd:
028200040319     c                                     %scan('&FILE     ':
028300040319     c                                           Qcmd))
028400040319      *
028500040319x   1c                   other
028600040319     c                   clear                   Qcmd
028700040319e   1c                   endsl
028800040319      *
028900040319if  1c                   if        Qcmd <> *blanks
029000040319     c                   call      'QCMDEXC'                            40
029100040319     c                   parm                    Qcmd
029200040319     c                   parm                    Qlen
029300040319e   1c                   endif
029400040319      *
029500040319     c                   ENDSR
029600040319
029700040319      *---------------------------------------------------------------*
029800040319      * Eseguo la override al file (vedi parametro) WFXXX             *
029900040319      *---------------------------------------------------------------*
030000040319     c     Ovr_PF        BEGSR
030100040319      *
030200040319     c                   eval      *in40 = *off
030300040319      *
030400040319     c                   eval      Qcmd   = %replace(wF00F:$Cmd(4):
030500040319     c                                      %scan('&FILE00F  ':
030600040319     c                                            $Cmd(4)))
030700040319     c                   eval      Qcmd   = %replace(wFxxx:Qcmd:
030800040319     c                                      %scan('&FILE     ':
030900040319     c                                            Qcmd))
031000040319      *
031100040319     c                   call      'QCMDEXC'                            40
031200040319     c                   parm                    Qcmd
031300040319     c                   parm                    Qlen
031400040319      *
031500040319     c                   ENDSR
031600040319
031700040319      *---------------------------------------------------------------*
031800170801      * Gestione nuovo file FLTR2                                     *
031900040319      *---------------------------------------------------------------*
032000170801     c     New_TR2       BEGSR
032100040319      *
032200040319      *File FLTR2xxx?
032300040319      *
032400040322     c                   eval      $NoPF2 = *on
032500040319      *
032600040319      * Controllo se esiste il file specifico per il P.O.,
032700160831      *   lo creo se inesistente,  lo ripulisco se già esistente
032800040319     c                   eval      $Clr   = *on
032900040319     c                   eval      $Crt   = *on
033000040319     c                   eval      wFxxx  = 'FLTR2' + %trim(wPOa)
033100040319     c                   eval      wF00F  = 'FLTR200F  '
033200040319     c                   exsr      Ctr_Clr_PF
033300040319     c                   eval      $NoPF2 = *in40
033400040319      *
033500040319      * Punto al file del P.O. specifico - OVRDBF
033600040319if  2c                   if        $NoPF2 = *off
033700040319     c                   eval      wF00F  = 'FLTR200F  '
033800040319     c                   eval      wFxxx  = 'FLTR2' + %trim(wPOa)
033900040319     c                   exsr      Ovr_PF
034000040319     c                   eval      $NoPF2 = *in40
034100040319if  3c                   if        $NoPF2 = *off
034200040319     c                   OPEN      FLTR200F
034300040319e   3c                   endif
034400040319e   2c                   endif
034500040319      *
034600040319     c                   ENDSR
034700040317
034800040317      *---------------------------------------------------------------*
034900040319      * Scrittura record nel file FLTR2                               *
035000040317      *---------------------------------------------------------------*
035100040319     c     Write_TR2     BEGSR
035200040317      *
035300040317      * Escludo varie categorie
035400040319if  1c                   if           FVVnpg = 0
035500040319     c                             or FVVnpg = 4
035600040319     c                             or FVVnpg = 7
035700040319     c                             or FVVnpg = 8
035800160831     c                   ELSE
035900040317      *
036000040319      * Scrivo il rec. nel file su cui si sta puntando ("xxx"=FVVFGS)
036100040317     c                   movel     FVVnpg        TR2ctl
036200040317     c                   movel     FVVnfv        TR2nfv
036300040319     c                   WRITE     FLTR2
036400160831e   1 *
036500160831e   1c                   endif
036600040317      *
036700160831     c                   ENDSR
036800040317
036900040318** - $Cmd
037000040318CHKOBJ    OBJ(&FILE     )  OBJTYPE(*FILE)
037100040318CLRPFM    FILE(&FILE     )
037200040318CRTDUPOBJ OBJ(&FILE00F  )  FROMLIB(&FLIB     ) OBJTYPE(*FILE) NEWOBJ(&FILE     )
037300040318OVRDBF    FILE(&FILE00F  ) TOFILE(&FILE     )
037400040318DLTOVR    FILE(&FILE00F  )

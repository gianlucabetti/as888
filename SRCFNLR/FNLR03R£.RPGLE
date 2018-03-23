000100040317      * FNLR03R *----------------------------------------------------*
000200040317      *           PREPARA FILE NUOVI FOGLI VARI PER P.C.             *
000300040317      *--------------------------------------------------------------*
000400040317
000500040317     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600040317
000700040317     fFNFVV03L  if   e           k disk
000800040317     fAZORG01L  if   e           k disk
000900040317     fFIFAP00F  o    e             disk    usropn
001000040317     fFLTR200F  o    e             disk    usropn
001100040317
001200040317      *
001300040317      * === COSTANTI =============================================== *
001400040317      *
001500040317      *
001600040317      * === SCHIERE A TEMPO DI COMPILAZIONE ======================== *
001700040317      *
001800040318     d $Cmd            s             80    dim(5)  ctdata perrcd(1)
001900040317      *
002000040317      * === SCHIERE A TEMPO DI ESECUZIONE ========================== *
002100040317      *
002200040317      * P.O. gestiti
002300040317     d L1              s              3  0 dim(30)
002400040317      * Foglio imp / descrizione
002500040317     d IMP             s                   dim(20) inz like(FVVnfv)
002600040317     d DES             s                   dim(20) inz like(FVVdsf)
002700040317      *
002800040317      * === STRUTTURE DATI DEFINITE ESTERNAMENTE =================== *
002900040317      *
003000040317      * Passaggio Parametri
003100040317     d KPJBA         e ds
003200040317      * DS per TIBS55R - Reperimento dati per trasmissioni
003300040317     d TIBS55DS      e ds
003400040317      * DS per TRUL06R - Caricamento £1 (P.O. gestiti)
003500040317     d TRUL06DS      e ds
003600040317     d  LIN                    1     90  0 dim(30)
004100040317      *
004200040317      * === STRUTTURE DATI DEFINITE INTERNAMENTE =================== *
004300040317      *
004400040317      * === VARIABILI ============================================== *
004500040317      *
004600040317      * P.O. passato come unico parametro nella KPJBU
004700040319     d wPOa            s              3    inz
004800040319     d wPO             s              3  0 inz
004900040317      * SimFel di wPO
005000040317     d SIMfel          s              3  0 inz
005100040317      * Indici di schiera
005200040317     d XC              s              3  0 inz
005300040317      * Campi di comodo
005400070301     d** w004a           s              4    inz
005500040322     d wFxxx           s             10    inz
005600040319     d wF00F           s             10    inz
005700040317      * Parametri per pgm QCMDEXC
005800040318     d Qcmd            s             80    inz
005900040318     d Qlen            s             15  5 inz(80)
006000040317      * Flag
006100040318     d $Clr            s              1    inz(*off)
006200040318     d $Crt            s              1    inz(*off)
006300040318     d $Alc            s              1    inz(*off)
006400040317     d $NoPF1          s              1    inz(*off)
006500040317     d $NoPF2          s              1    inz(*off)
006600040322     D $Almeno1        S              1    inz(*off)
006700040317     d $Proc3          s              1    inz(*off)
006900040317
007000040317      *--------------------------------------------------------------*
007100040317      * === MAIN LINE PROGRAM ====================================== *
007200040317      *--------------------------------------------------------------*
007300040317
007400040318     c     *Entry        plist
007500040318     c                   parm                    KPJBA
007600040318      *
007700040318     c                   exsr      OperazIniz
007800040318      *
007900040317      * Controllo quali file esistono e ne faccio una CLEAR
008000040322if  1c                   if        wPO =  SIMfel
008100040322     c                   exsr      sr_FEL_1
008200040322x   1c                   else
008300040322     c                   exsr      sr_PO2_1
008400040322e   1c                   endif
008500040317      * Se uno dei file risulta già allocato è bene NON
008600040317      *  proseguire, perchè sarebbe in uso da questo stesso
008700040317      *  programma...
008800040318     c     $Alc          cabeq     *on           Fine
008900040317      *
009000040319      *
009100040322      * Elaboro:
009200040322      * ?prima?per SIMfel per: scrivere i file FIFAPxxx e FLTR2xxx
009300040322      *                     o  memorizzare i fogli IMP 2° liv.
009400040317     c                   exsr      sr_FEL_2
009500040317      *
009600040322      * ?poi?per il P.O. passato: sempre file XXX (SIMfel esclusa)
009700040322if  1c                   if        wPO <> SIMfel
009800040319     c                   exsr      sr_PO2_2
009900040322e   1c                   endif
010000040317      *
010100040317      * Fine
010200040317     c     Fine          tag
010300040317     c                   eval      *inLR = *on
010400040317
010500040317      *---------------------------------------------------------------*
010600040317      * Operazioni iniziali                                           *
010700040317      *---------------------------------------------------------------*
010800040318     c     OperazIniz    BEGSR
010900040317      *
011000040319     c                   clear                   wPO
011100040322     c                   clear                   wPOa
011200040319if  1c                   if        %subst(KPJBU:1:3) > *zeros
011300040322     c                   movel     KPJBU         wPOa
011400040319e   1c                   endif
011500040322     c     wPOa          cabLE     *zeros        Fine
011600040322     c                   movel     wPOa          wPO
011700070301     c*****              movel(p)  wPOa          w004a
011800040317      *
011900040317      * Richiamo TRUL06R con "PP" & il P.O. ricevuto
012000040317     c                   clear                   TRUL06DS
012100040317     c                   movel     'PP'          D06cod
012200040317     c                   movel     'L'           D06tla
012300040317     c                   movel     wPO           D06key
012400040317     c                   movel(p)  TRUL06DS      KPJBU
012500040317     c                   call      'TRUL06R'
012600040317     c                   parm                    KPJBA
012700040317     c                   movel     KPJBU         TRUL06DS
012800040317if  1c                   if        D06err = *blanks
012900040317     c                   movea     LIN           L1
013000040317e   1c                   endif
013100040317      * Se non è stato reperito nemmeno un P.O. da elaborare è inutile
013200040317      *  proseguire...
013300040317     c     L1(1)         cabeq     0             Fine
013400040317      *
013500040317     c                   eval      SIMfel = L1(1)
013600040317      *
013700040317      * Reperisco il S.I. in cui sono e la relativa libreria dati
013800040317     c                   clear                   TIBS55DS
013900040317     c                   eval      I50tla = 'L'
014000040317     c                   call      'TIBS55R'
014100040317     c                   parm                    TIBS55DS
014200040319      *
014300040317     c                   ENDSR
014400040317
014500040317      *---------------------------------------------------------------*
014600040318      * Operazioni iniziali per SIMFEL                                *
014700040317      *---------------------------------------------------------------*
014800040317     c     sr_FEL_1      BEGSR
014900040317      *
015000040318     c                   eval      $Clr   = *on
015100040318     c                   eval      $Crt   = *on
015200040318      *
015300040318      * file FIFAP:
015400040319     c                   eval      wFxxx  = 'FIFAP' + wPOa
015500040318     c                   eval      wF00F  = 'FIFAP00F  '
015600040318     c                   exsr      Ctr_Clr_PF
015700040317      * Se non riesce la pulizia significa che il file è in uso !
015800040317      * ---------------------------------------------------------
015900040318     c                   if        *in40  = *on
016000040318     c                   eval      $Alc   = *on
016100040317     c                   goto      EndFEL_1
016200040317     c                   endif
016300040317      *
016400040317      * Controllo se esiste il file FLTR2 per SIMfel
016500040317      * - Lo ripulisco se esistente - CLRPFM   &
016600040317      * - Lo creo se inesistente    - CTRDUPOBJ
016700040318      * file FLTR2:
016800040319     c                   eval      wFxxx  = 'FLTR2' + wPOa
016900040318     c                   eval      wF00F  = 'FLTR200F  '
017000040318     c                   exsr      Ctr_Clr_PF
017100040318     c                   eval      $NoPF2 = *in40
017200040317      * Se non riesce la pulizia significa che il file è in uso !
017300040317      * ---------------------------------------------------------
017400040317     c                   if        $NoPF2 = *on
017500040318     c                   eval      $Alc   = *on
017600040317     c                   goto      EndFEL_1
017700040317     c                   endif
017800040317      *
017900040510      * Creo i file spunte FNBA1/FIBAA-xxx
018000070301     c**                 movel(p)  SIMfel        w004a
018100070301     c**                 exsr      Crt_FILE_S
018200040317      * Creo anche il file "XXXA"
018300070301     c**                 move      'A'           w004a
018400070301     c**                 exsr      Crt_FILE_S
018500040317      *
018600040317     c     EndFEL_1      ENDSR
018700040318
018800040317      *---------------------------------------------------------------*
018900040317      * Operazioni iniziali per P.O. di 2° livello                    *
019000040317      *---------------------------------------------------------------*
019100040317     c     sr_PO2_1      BEGSR
019200040317      *
019300040319      * Controllo esistenza file FIFAPxxx
019400040319     c                   eval      $Clr   = *on
019500040319     c                   eval      $Crt   = *off
019600040319     c                   eval      wF00F  = *blanks
019700040318      *
019800040318      * file FIFAP:
019900040319     c                   eval      wFxxx  = 'FIFAP' + wPOa
020000040318     c                   exsr      Ctr_Clr_PF
020100040318      *
020200040318      * Se il file non esiste: lo memorizzo in quelli NON esistenti
020300040317      * (aspetto di aver verificato ci siano dei dati per esso, prima
020400040317      *  di crearlo).
020500040319if  1c                   if        *in40  = *on
020600040319     c                   eval      $NoPF1 = *on
020700040319e   1c                   endif
020800040317      *
020900040317     c                   ENDSR
021000040317
021100040317      *---------------------------------------------------------------*
021200040317      * Lettura file FNFVV e scrittura file FIFAP e FLTR2 per SIMfel  *
021300040317      *---------------------------------------------------------------*
021400040317     c     sr_FEL_2      BEGSR
021500040317      *
021600040322if  1c                   if        wPO    = SIMfel
021700040319      * Override:
021800040322      * -per FIFAPxxx?
021900040319     c                   eval      wF00F  = 'FIFAP00F  '
022000040319     c                   eval      wFxxx  = 'FIFAP' + wPOa
022100040319     c                   eval      Qcmd   = %replace(wF00F:$Cmd(4):
022200040319     c                                      %scan('&FILE00F  ':
022300040319     c                                            $Cmd(4)))
022400040319     c                   eval      Qcmd   = %replace(wFxxx:Qcmd:
022500040319     c                                      %scan('&FILE     ':
022600040319     c                                            Qcmd))
022700040319     c                   call      'QCMDEXC'                            40
022800040319     c                   parm                    Qcmd
022900040319     c                   parm                    Qlen
023000040322      * -per FLTR2xxx?
023100040319     c                   eval      wF00F  = 'FLTR200F  '
023200040319     c                   eval      wFxxx  = 'FLTR2' + wPOa
023300040319     c                   eval      Qcmd   = %replace(wF00F:$Cmd(4):
023400040319     c                                      %scan('&FILE00F  ':
023500040319     c                                            $Cmd(4)))
023600040319     c                   eval      Qcmd   = %replace(wFxxx:Qcmd:
023700040319     c                                      %scan('&FILE     ':
023800040319     c                                            Qcmd))
023900040319     c                   call      'QCMDEXC'
024000040319     c                   parm                    Qcmd
024100040319     c                   parm                    Qlen
024200040319      *
024300040317     c                   open      FIFAP00F
024400040317     c                   open      FLTR200F
024500040322      *
024600040322e   1c                   endif
024700040319      *
024800040322      * Elaborazione:
024900040317     c                   eval      XC = 1
025000040322     c     SIMfel        setll     FNFVV000
025100040322     c     SIMfel        reade     FNFVV000
025200040317      *
025300040319do  1c                   DOW       not %eof(FNFVV03L)
025400040317      *
025500040317      * Elaboro soltanto se è da scaricare
025600040319if  2c                   IF        FVVfsc = 'S'
025700040317      *
025800040319      * - Scrivo FIFAP e FLTR2 se sono SIMFEL
025900040319if  3c                   if        wPO    = SIMfel
026000040317     c                   eval      $Proc3 = *off
026100040319     c                   exsr      Write_FAP
026200040319     c                   exsr      Write_TR2
026300040319e   3c                   endif
026400040317      *
026500040319      * - Memorizzo comunque fogli IMP per P.O. di secondo livello
026600040317if  3c                   if            FVVnpg = 3
026700040317     c                             and FVVspg = 'P'
026800040317     c                   movel     FVVnfv        IMP(XC)
026900040317     c                   movel     FVVdsf        DES(XC)
027000040317     c                   add       1             XC
027100040319      * - - Se oltrepassato il limite, memorizzo gli ultimi ripartendo
027200040319      *       da 1
027300040317if  4c                   if        XC > 20
027400040317     c                   z-add     1             XC
027500040317e   4c                   endif
027600040317e   3c                   endif
027700040317      *
027800040319e   2c                   ENDIF
027900040317      *
028000040322     c     SIMfel        reade     FNFVV000
028100040319e   1c                   ENDDO
028200040317      *
028300040322      * Fine:
028400040322if  1c                   if        wPO    = SIMfel
028500040322      *
028600040317     c                   close     FIFAP00F
028700040317     c                   close     FLTR200F
028800040319      *
028900040319      * Cancellazione override (tutte)
029000040319     c                   eval      wF00F  = '*ALL      '
029100040319     c                   eval      Qcmd   = %replace(wF00F:$Cmd(5):
029200040319     c                                      %scan('&FILE00F  ':
029300040319     c                                            $Cmd(5)))
029400040319     c                   call      'QCMDEXC'
029500040319     c                   parm                    Qcmd
029600040319     c                   parm                    Qlen
029700040322      *
029800040322e   1c                   endif
029900040317      *
030000040317     c                   ENDSR
030100040317
030200040317      *---------------------------------------------------------------*
030300040317      * Lettura file FNFVV e scrittura file FIFAP per 2° livello      *
030400040318      *                    e scrittura file FLTR2 per Poste           *
030500040317      *---------------------------------------------------------------*
030600040317     c     sr_PO2_2      BEGSR
030700040317      *
030800040322      * Override ai file FIFAPxxx & FLTR2xxx
030900040322     c                   exsr      New_FAPTR2
031000040317      *
031100040319      * Elaborazione
031200040322     C                   eval      $Almeno1  =  *off
031300040319     c     wPO           setll     FNFVV000
031400040319     c     wPO           reade     FNFVV000
031500040317      *
031600040318do  1c                   DOW       not %eof(FNFVV03L)
031700040317      *
031800040319      * - Elaboro soltanto se è da scaricare
031900040319if  2c                   IF        FVVfsc =  'S'
032000040317      *
032100040318      * - - Scrivo i record ai file FIFAP e FLTR2
032200040319if  3c                   if        $NoPF1 = *off
032300040322     c                   eval      $Proc3 = *off
032400040319     c                   exsr      Write_FAP
032500040319e   3c                   endif
032600040319     c                   exsr      Write_TR2
032700040322     C                   eval      $Almeno1 = *on
032800040317      *
032900040318e   2c                   ENDIF
033000040317      *
033100040322     c     wPO           reade     FNFVV000
033200040318      *
033300040318e   1c                   ENDDO
033400040317      *
033500040319      * Registro i fogli IMP come ultimi in FIFAP
033600040317if  1c                   if            $NoPF1    =  *off
033700040317     c                             and $Almeno1  =  *on
033800040317     c                   eval      $Proc3 = *on
033900040317do  2c                   do        20            XC
034000040319cas 3c     IMP(XC)       casne     *zeros        Write_FAP
034100040317e   3c                   endcs
034200040317e   2c                   enddo
034300040317e   1c                   endif
034400040322      *
034500040322     c                   close     FIFAP00F
034600040322     c                   close     FLTR200F
034700040322      *
034800040322      * Cancellazione override (tutte)
034900040322     c                   eval      wF00F  = '*ALL      '
035000040322     c                   eval      Qcmd   = %replace(wF00F:$Cmd(5):
035100040322     c                                      %scan('&FILE00F  ':
035200040322     c                                            $Cmd(5)))
035300040322     c                   call      'QCMDEXC'
035400040322     c                   parm                    Qcmd
035500040322     c                   parm                    Qlen
035600040317      *
035700040317     c                   ENDSR
035800040319
035900040319      *---------------------------------------------------------------*
036000040319      * Controllo se esiste il file (vedi parametro) WFXXX            *
036100040319      * - Lo ripulisco se esistente - CLRPFM   &                      *
036200040319      * - Lo creo se inesistente    - CRTDUPOBJ                       *
036300040319      *---------------------------------------------------------------*
036400040319     c     Ctr_Clr_PF    BEGSR
036500040319      *
036600040319     c                   eval      *in40 = *off
036700040319      *
036800040319      * Controllo se esiste
036900040319     c                   eval      Qcmd  = %replace(wFxxx:$Cmd(1):
037000040319     c                                     %scan('&FILE     ':
037100040319     c                                           $Cmd(1)))
037200040319     c                   call      'QCMDEXC'                            40
037300040319     c                   parm                    Qcmd
037400040319     c                   parm                    Qlen
037500040319      *
037600040319sel 1c                   select
037700040319      * - Lo ripulisco se esistente e se richiesto
037800040319w   1c                   when          *in40 = *off
037900040319     c                             and $Clr  = *on
038000040319     c                   eval      Qcmd  = %replace(wFxxx:$Cmd(2):
038100040319     c                                     %scan('&FILE     ':
038200040319     c                                           $Cmd(2)))
038300040319      * - Lo creo se inesistente
038400040319w   1c                   when          *in40 = *on
038500040319     c                             and $Crt  = *on
038600040319     c                   eval      Qcmd  = %replace(wF00F:$Cmd(3):
038700040319     c                                     %scan('&FILE00F  ':
038800040319     c                                           $Cmd(3)))
038900040319     c                   eval      Qcmd  = %replace(O50pla:Qcmd:
039000040319     c                                     %scan('&FLIB     ':
039100040319     c                                           Qcmd))
039200040319     c                   eval      Qcmd  = %replace(wFxxx:Qcmd:
039300040319     c                                     %scan('&FILE     ':
039400040319     c                                           Qcmd))
039500040319      *
039600040319x   1c                   other
039700040319     c                   clear                   Qcmd
039800040319e   1c                   endsl
039900040319      *
040000040319if  1c                   if        Qcmd <> *blanks
040100040319     c                   call      'QCMDEXC'                            40
040200040319     c                   parm                    Qcmd
040300040319     c                   parm                    Qlen
040400040319e   1c                   endif
040500040319      *
040600040319     c                   ENDSR
040700040319
040800040319      *---------------------------------------------------------------*
040900040319      * Eseguo la override al file (vedi parametro) WFXXX             *
041000040319      *---------------------------------------------------------------*
041100040319     c     Ovr_PF        BEGSR
041200040319      *
041300040319     c                   eval      *in40 = *off
041400040319      *
041500040319     c                   eval      Qcmd   = %replace(wF00F:$Cmd(4):
041600040319     c                                      %scan('&FILE00F  ':
041700040319     c                                            $Cmd(4)))
041800040319     c                   eval      Qcmd   = %replace(wFxxx:Qcmd:
041900040319     c                                      %scan('&FILE     ':
042000040319     c                                            Qcmd))
042100040319      *
042200040319     c                   call      'QCMDEXC'                            40
042300040319     c                   parm                    Qcmd
042400040319     c                   parm                    Qlen
042500040319      *
042600040319     c                   ENDSR
042700040319
042800040319      *---------------------------------------------------------------*
042900040319      * Creo FLBAR del P.O. che sto gestendo (se non esiste)          *
043000040319      * ma NON lo ripulisco (se già esiste)                           *
043100040319      *---------------------------------------------------------------*
043200070301     c***  Crt_FILE_S    BEGSR
043300040319      *
043400070301     c***                eval      $Clr  = *off
043500070301     c***                eval      $Crt  = *on
043600040319      *
043700040319      * file FNBA1xxx(A):
043800070301     c***                eval      wFxxx = 'FNBA1' + %trim(w004a)
043900070301     c***                eval      wF00F = 'FNBA100F  '
044000070301     c***                exsr      Ctr_Clr_PF
044100040319      *
044200040319      * file FIBAAxxx(a):
044300070301     c***                eval      wFxxx = 'FIBAA' + %trim(w004a)
044400070301     c***                eval      wF00F = 'FIBAA00F  '
044500070301     c***                exsr      Ctr_Clr_PF
044600040319      *
044700070301     c***                ENDSR
044800040319
044900040319      *---------------------------------------------------------------*
045000040319      * Gestione nuovo file FIFAP                                     *
045100040319      *---------------------------------------------------------------*
045200040319     c     New_FAPTR2    BEGSR
045300040319      *
045400040319      *File FIFAPxxx?
045500040319      *
045600040322     c                   eval      $NoPF1 = *on
045700040319      * Controllo se esiste il file specifico per il P.O.
045800040319      *   e lo creo se inesistente
045900040319     c                   eval      wFxxx  = 'FIFAP' + %trim(wPOa)
046000040319     c                   eval      wF00F  = 'FIFAP00F  '
046100040319     c                   eval      $Clr   = *off
046200040319     c                   eval      $Crt   = *on
046300040319     c                   exsr      Ctr_Clr_PF
046400040319     c                   eval      $NoPF1 = *in40
046800040319      *
046900040319      * Punto al file del P.O. specifico - OVRDBF
047000040319if  1c                   if        $NoPF1 = *off
047100040319     c                   eval      wF00F  = 'FIFAP00F  '
047200040319     c                   eval      wFxxx  = 'FIFAP' + %trim(wPOa)
047300040319     c                   exsr      Ovr_PF
047400040319     c                   eval      $NoPF1 = *in40
047500040319if  2c                   if        $NoPF1 = *off
047600040319     c                   OPEN      FIFAP00F
047700040319e   2c                   endif
047800040319e   1c                   endif
047900040319      *
048000040319      *File FLTR2xxx?
048100040319      *
048200040322     c                   eval      $NoPF2 = *on
049300040319      *
049400040319      * Controllo se esiste il file specifico per il P.O.,
049500160831      *   lo creo se inesistente,  lo ripulisco se già esistente
049800040319     c                   eval      $Clr   = *on
049900040319     c                   eval      $Crt   = *on
050000040319     c                   eval      wFxxx  = 'FLTR2' + %trim(wPOa)
050100040319     c                   eval      wF00F  = 'FLTR200F  '
050200040319     c                   exsr      Ctr_Clr_PF
050300040319     c                   eval      $NoPF2 = *in40
050400040319      *
050500040319      * Punto al file del P.O. specifico - OVRDBF
050600040319if  2c                   if        $NoPF2 = *off
050700040319     c                   eval      wF00F  = 'FLTR200F  '
050800040319     c                   eval      wFxxx  = 'FLTR2' + %trim(wPOa)
050900040319     c                   exsr      Ovr_PF
051000040319     c                   eval      $NoPF2 = *in40
051100040319if  3c                   if        $NoPF2 = *off
051200040319     c                   OPEN      FLTR200F
051300040319e   3c                   endif
051400040319e   2c                   endif
051600040319      *
051700040319     c                   ENDSR
051800040317
051900040317      *---------------------------------------------------------------*
052000040317      * Scrittura record nel file FIFAP                               *
052100040317      *---------------------------------------------------------------*
052200040319     c     Write_FAP     BEGSR
052300040318      *
052400040318if  1c                   if        $Proc3 = *off
052500040317     c                   movel     FVVnpg        FAPpro
052600040317     c                   z-add     FVVnfv        FAPnfg
052700040317     c                   movel     FVVdsf        FAPdfg
052800040318x   1c                   else
052900040317      * procedura '3'
053000040317     c                   movel     '3'           FAPpro
053100040317     c                   z-add     IMP(XC)       FAPnfg
053200040317     c                   movel     DES(XC)       FAPdfg
053300040318e   1c                   endif
053400040317      *
053500040317     c                   write     FIFAP
053600040317      *
053700040317     c                   ENDSR
053800040317
053900040317      *---------------------------------------------------------------*
054000040319      * Scrittura record nel file FLTR2                               *
054100040317      *---------------------------------------------------------------*
054200040319     c     Write_TR2     BEGSR
054500040317      *
054600040317      * Escludo varie categorie
054700040319if  1c                   if           FVVnpg = 0
054800040319     c                             or FVVnpg = 4
054900040319     c                             or FVVnpg = 7
055000040319     c                             or FVVnpg = 8
055100160831     c                   ELSE
056300040317      *
056400040319      * Scrivo il rec. nel file su cui si sta puntando ("xxx"=FVVFGS)
056600040317     c                   movel     FVVnpg        TR2ctl
056700040317     c                   movel     FVVnfv        TR2nfv
056800040319     c                   WRITE     FLTR2
056900160831e   1 *
056901160831e   1c                   endif
057000040317      *
057100160831     c                   ENDSR
057200040317
057300040318** - $Cmd
057400040318CHKOBJ    OBJ(&FILE     )  OBJTYPE(*FILE)
057500040318CLRPFM    FILE(&FILE     )
057600040318CRTDUPOBJ OBJ(&FILE00F  )  FROMLIB(&FLIB     ) OBJTYPE(*FILE) NEWOBJ(&FILE     )
057700040318OVRDBF    FILE(&FILE00F  ) TOFILE(&FILE     )
057800040318DLTOVR    FILE(&FILE00F  )

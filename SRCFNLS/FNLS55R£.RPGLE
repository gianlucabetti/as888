000100040401      * FNLS55R *----------------------------------------------------*
000200040401      *         PREPARA FILE FOGLI DI VIAGGIO PER P.C. FIFVP         *
000300040401      *--------------------------------------------------------------*
000400040401
000500040401     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600040401
000700040401     fFNFGV06L  if   e           k disk
000800040401     fFNFGW01L  if   e           k disk
000900040401     fAZORG01L  if   e           k disk
001000040401     fFIFVP00F  o    e             disk    usropn
001100040401
001200040401      *
001300040401      * === COSTANTI =============================================== *
001400040401      *
001500040401      *
001600040401      * === SCHIERE A TEMPO DI COMPILAZIONE ======================== *
001700040401      *
001800040401     d $Cmd            s             80    dim(5)  ctdata perrcd(1)
001900040401      *
002000040401      * === SCHIERE A TEMPO DI ESECUZIONE ========================== *
002100040401      *
002200040401     d $FFV            s              3    dim(300) inz(*zeros)
002300040401     d wFFV            s              3    dim(50)  inz(*zeros)
002400040401      *
002500040401      * === STRUTTURE DATI DEFINITE ESTERNAMENTE =================== *
002600040401      *
002700040401      * Passaggio Parametri
002800040401     d KPJBA         e ds
002900040401      * DS per TIBS55R - Reperimento dati per trasmissioni
003000040401     d TIBS55DS      e ds                  inz
003100040401     d  I50tla       e                     inz('L')
003200040401      * DS per FNLV55R - Reperimento terminal di partenza/arrivo
003300040401     d FNLV55DS      e ds                  inz
003400040401     d  D55tla       e                     inz('L')
003500040401     d  D55tpt       e                     inz('P')
003600040401      * Descrizione 143 dell'organigramma (AZORG)
003700040401     d OG143         e ds                  inz
003800040401      *
003900040401      * === STRUTTURE DATI DEFINITE INTERNAMENTE =================== *
004000040401      *
004100040401      *
004200040401      * === VARIABILI ============================================== *
004300040401      *
004400040401      * P.O. passato come unico parametro nella KPJBU
004500040401     d wPOa            s              3    inz
004600040401     d wPO             s              3  0 inz
004700040401      * Indici di schiera
004800040401     d xx              s              3  0 inz
004900040401     d yy              s              3  0 inz
005000040401      * Campi di comodo
005100040401     d WdateISO        s               d   datfmt(*ISO) inz
005200040401     d wFxxx           s             10    inz
005300040401     d wF00F           s             10    inz
005400040401      * Parametri per pgm QCMDEXC
005500040401     d Qcmd            s             80    inz
005600040401     d Qlen            s             15  5 inz(80)
005700040401      * Flag
005800040401     d $Clr            s              1    inz(*off)
005900040401     d $Crt            s              1    inz(*off)
006000040401     d $Alc            s              1    inz(*off)
006100040401     d $NoPF1          s              1    inz(*off)
006200040401      *
006300040401      * === KEY-LIST =============================================== *
006400040401      *
006500040401     c     K02FGW01      klist
006600040401     c                   kfld                    FGVnfv
006700040401     c                   kfld                    FGVlnp
006800040401
006900040401      *--------------------------------------------------------------*
007000040401      * === MAIN LINE PROGRAM ====================================== *
007100040401      *--------------------------------------------------------------*
007200040401
007300040401     c     *Entry        plist
007400040401     c                   parm                    KPJBA
007500040401      *
007600040401     c                   exsr      OperazIniz
007700040401      *
007800040401      * Controllo quali file esistono e ne faccio una CLEAR
007900040401if  1c                   if        wPO =  D55tfp
008000040401     c                   exsr      sr_FEL
008100040401x   1c                   else
008200040401     c                   exsr      sr_PO2
008300040401e   1c                   endif
008400040401      * Se uno dei file risulta già allocato è bene NON
008500040401      *  proseguire, perchè sarebbe in uso da questo stesso
008600040401      *  programma...
008700040401     c     $Alc          cabeq     *on           Fine
008800040401      *
008900040401      *
009000040401      * Elaboro per SIMfel per scrivere il file FIFVPxxx
009100040401     c                   exsr      sr_POE
009200040401      *
009300040401      * Fine
009400040401     c     Fine          tag
009500040401     c                   eval      *inLR = *on
009600040401
009700040401      *---------------------------------------------------------------*
009800040401      * Operazioni iniziali                                           *
009900040401      *---------------------------------------------------------------*
010000040401     c     OperazIniz    BEGSR
010100040401      *
010200040401     c                   clear                   wPO
010300040401     c                   clear                   wPOa
010400040401if  1c                   if        %subst(KPJBU:1:3) > *zeros
010500040401     c                   movel     KPJBU         wPOa
010600040401e   1c                   endif
010700040401     c     wPOa          cabLE     *zeros        Fine
010800040401     c                   movel     wPOa          wPO
010900040401      *
011000040401     c     wPO           chain     AZORG
011100040401if  1c                   if        NOT %found(AZORG01L)
011200040401     c                             or  ORGfva <> *blanks
011300040401     c                   goto      Fine
011400040401e   1c                   endif
011500040401     c                   movel     ORGde3        OG143
011600040401      *
011700040401     c                   move      *date         WdateISO
011800040401      *
011900040401      * Richiamo FNLV55R per reperire il terminal di partenza
012000040401     c                   reset                   FNLV55DS
012100040401     c                   z-add     wPO           D55lin
012200040401     c                   move      WdateISO      D55drf
012300040401     c                   call      'FNLV55R'
012400040401     c                   parm                    FNLV55DS
012500040401      * Se non è stato reperito il terminal di partenza è inutile
012600040401      *  proseguire...
012700040401     c     D55err        cabne     *blanks       Fine
012800040401     c     D55tfp        cabeq     0             Fine
012900051215     *** * ...Neanche se è stato passato un p.o. né di partenza né Poste
013000051215      * Se ricevuto un p.o. né di partenza né Poste: elaboro x suo tfp
013100051215      *  (scrivendo nel file FIFVPxxx del p.o. ricevuto come parametro)
013200051215      *  Quindi: imposto wPO = D55tfp   e
013300051215      *          lascio wPOa = parametro ricevuto.
013400040401if  1c                   if            wPO    <> D55tfp
013500040401     c                             and §OGntw <> 'PPT'
013600051215     ***c                   goto      Fine
013700051215     c                   eval      wPO  = D55tfp
013800040401e   1c                   endif
013900040401      *
014000040401      * Reperisco il S.I. in cui sono e la relativa libreria dati
014100040401     c                   reset                   TIBS55DS
014200040401     c                   call      'TIBS55R'
014300040401     c                   parm                    TIBS55DS
014400040401      *
014500040401     c                   ENDSR
014600040401
014700040401      *---------------------------------------------------------------*
014800040401      * Operazioni iniziali per SIMFEL                                *
014900040401      *---------------------------------------------------------------*
015000040401     c     sr_FEL        BEGSR
015100040401      *
015200040401     c                   eval      $Clr   = *on
015300040401     c                   eval      $Crt   = *on
015400040401      *
015500040401      * Verifica esistenza/allocazione di FIFVP:
015600040401      *   eventuale creazione  o  pulizia
015700040401     c                   eval      wFxxx  = 'FIFVP' + wPOa
015800040401     c                   eval      wF00F  = 'FIFVP00F  '
015900040401     c                   exsr      Ctr_Clr_PF
016000040401      * Se non riesce la pulizia significa che il file è in uso !
016100040401      * ---------------------------------------------------------
016200040401     c                   if        *in40  = *on
016300040401     c                   eval      $Alc   = *on
016400040401     c                   goto      EndFEL
016500040401     c                   endif
016600040401      *
016700040401      * Override:
016800040401     c                   eval      wF00F  = 'FIFVP00F  '
016900040401     c                   eval      wFxxx  = 'FIFVP' + wPOa
017000040401     c                   eval      Qcmd   = %replace(wF00F:$Cmd(4):
017100040401     c                                      %scan('&FILE00F  ':
017200040401     c                                            $Cmd(4)))
017300040401     c                   eval      Qcmd   = %replace(wFxxx:Qcmd:
017400040401     c                                      %scan('&FILE     ':
017500040401     c                                            Qcmd))
017600040401     c                   call      'QCMDEXC'                            40
017700040401     c                   parm                    Qcmd
017800040401     c                   parm                    Qlen
017900040401     c                   if        *in40  = *on
018000040401     c                   eval      $Alc   = *on
018100040401     c                   goto      EndFEL
018200040401     c                   endif
018300040401      *
018400040401      * Apertura:
018500040401     c                   open      FIFVP00F
018600040401      *
018700040401     c     EndFEL        ENDSR
018800040401
018900040401      *---------------------------------------------------------------*
019000040401      * Operazioni iniziali per P.O. di 2° livello                    *
019100040401      *---------------------------------------------------------------*
019200040401     c     sr_PO2        BEGSR
019300040401      *
019400040401      * Controllo esistenza file FIFVPxxx
019500040401     c                   eval      $Clr   = *on
019600040401     c                   eval      $Crt   = *off
019700040401     c                   eval      wF00F  = *blanks
019800040401      *
019900040401      * Verifica esistenza/allocazione di FIFVP:
020000040401      *   memorizzazione inesistenza  o  pulizia
020100040401     c                   eval      wFxxx  = 'FIFVP' + wPOa
020200040401     c                   exsr      Ctr_Clr_PF
020300040401      *
020400040401      * Se il file non esiste: lo memorizzo in quelli NON esistenti
020500040401      * (aspetto di aver verificato ci siano dei dati per esso, prima
020600040401      *  di crearlo).
020700040401      * Altrimenti lo apro.
020800040401if  1c                   if        *in40  = *on
020900040401     c                   eval      $NoPF1 = *on
021000040401x   1c                   else
021100040401     c                   open      FIFVP00F
021200040401e   1c                   endif
021300040401      *
021400040401     c                   ENDSR
021500040401
021600040401      *---------------------------------------------------------------*
021700040401      * Lettura file FNFGV e scrittura file FIFVP per P.O.            *
021800040401      *---------------------------------------------------------------*
021900040401     c     sr_POE        BEGSR
022000040401      *
022100040401     c     D55tfp        setll     FNFGV000
022200040401     c     D55tfp        reade     FNFGV000
022300040401      *
022400040401do  1c                   DOW       not %eof(FNFGV06L)
022500040401      *
022600040401      * Elaboro soltanto se?NON?è "TIPO TRAINO = INOLTRO"
022700040401if  2c                   IF        FGVttr <> 'I'
022800040401      *
022900040401     c     K02FGW01      chain     FNFGW000
023000040401if  3c                   if        not %found(FNFGW01L)
023100040401     c                             or  FGWATB <> *blank
023200040401     c                   clear                   FGWff3
023300040401     c                   clear                   FGWff4
023400040401     c                   clear                   FGWfgs
023500040401e   3c                   endif
023600040401      *
023700040401      * - Scrivo FIFVP x P.O. in elaborazione
023800040401if  3c                   IF            FGWfgs <> *zeros
023900040401     c                             and FGWfgs =  wPO
024000040401     c                             OR  FGWfgs =  *zeros
024100040401     c                             and FGVlnp =  wPO
024200040401      * - - Se non esiste ancora: lo creo
024300040401if  4c                   if        $NOpf1 =  *on
024400040401     c                   exsr      sr_FEL
024500040401      * - - - Se l'eventuale creazione ha avuto problemi: esco
024600040401     c     $Alc          cabeq     *on           Fine
024700040401     c                   eval      $NOpf1 = *off
024800040401e   4c                   endif
024900040401      * - - Scrivo
025000040401     c                   exsr      Write_FVP
025100040401e   3c                   ENDIF
025200040401      *
025300040401e   2c                   ENDIF
025400040401      *
025500040401     c     D55tfp        reade     FNFGV000
025600040401      *
025700040401e   1c                   ENDDO
025800040401      *
025900040401      * Fine:
026000040401if  1c                   if        %open(FIFVP00F)
026100040401      *
026200040401     c                   close     FIFVP00F
026300040401      *
026400040401      * Cancellazione override
026500040401     c                   eval      wF00F  = 'FIFVP00F  '
026600040401     c                   eval      Qcmd   = %replace(wF00F:$Cmd(5):
026700040401     c                                      %scan('&FILE00F  ':
026800040401     c                                            $Cmd(5)))
026900040401     c                   call      'QCMDEXC'                            40
027000040401     c                   parm                    Qcmd
027100040401     c                   parm                    Qlen
027200040401      *
027300040401e   1c                   endif
027400040401      *
027500040401     c                   ENDSR
027600040401
027700040401      *---------------------------------------------------------------*
027800040401      * Controllo se esiste il file (vedi parametro) WFXXX            *
027900040401      * - Lo ripulisco se esistente - CLRPFM   &                      *
028000040401      * - Lo creo se inesistente    - CRTDUPOBJ                       *
028100040401      *---------------------------------------------------------------*
028200040401     c     Ctr_Clr_PF    BEGSR
028300040401      *
028400040401     c                   eval      *in40 = *off
028500040401      *
028600040401      * Controllo se esiste
028700040401     c                   eval      Qcmd  = %replace(wFxxx:$Cmd(1):
028800040401     c                                     %scan('&FILE     ':
028900040401     c                                           $Cmd(1)))
029000040401     c                   call      'QCMDEXC'                            40
029100040401     c                   parm                    Qcmd
029200040401     c                   parm                    Qlen
029300040401      *
029400040401sel 1c                   select
029500040401      * - Lo ripulisco se esistente e se richiesto
029600040401w   1c                   when          *in40 = *off
029700040401     c                             and $Clr  = *on
029800040401     c                   eval      Qcmd  = %replace(wFxxx:$Cmd(2):
029900040401     c                                     %scan('&FILE     ':
030000040401     c                                           $Cmd(2)))
030100040401      * - Lo creo se inesistente
030200040401w   1c                   when          *in40 = *on
030300040401     c                             and $Crt  = *on
030400040401     c                   eval      Qcmd  = %replace(wF00F:$Cmd(3):
030500040401     c                                     %scan('&FILE00F  ':
030600040401     c                                           $Cmd(3)))
030700040401     c                   eval      Qcmd  = %replace(O50pla:Qcmd:
030800040401     c                                     %scan('&FLIB     ':
030900040401     c                                           Qcmd))
031000040401     c                   eval      Qcmd  = %replace(wFxxx:Qcmd:
031100040401     c                                     %scan('&FILE     ':
031200040401     c                                           Qcmd))
031300040401      *
031400040401x   1c                   other
031500040401     c                   clear                   Qcmd
031600040401e   1c                   endsl
031700040401      *
031800040401if  1c                   if        Qcmd <> *blanks
031900040401     c                   call      'QCMDEXC'                            40
032000040401     c                   parm                    Qcmd
032100040401     c                   parm                    Qlen
032200040401e   1c                   endif
032300040401      *
032400040401     c                   ENDSR
032500040401
032600040401      *---------------------------------------------------------------*
032700040401      * Scrittura record nel file FIFVP                               *
032800040401      *---------------------------------------------------------------*
032900040401     c     Write_FVP     BEGSR
033000040401      *
033100040401     c                   reset                   $FFV
033200040401     c                   reset                   XX
033300040401     c                   reset                   WFFV
033400040401     c                   reset                   YY
033500040401      *
033600040401     c     FGVlna        chain     AZORG
033700040401if  1c                   if        %found(AZORG01L)
033800040401     c                   movel     ORGdes        FVPdfa
033900040401x   1c                   else
034000040401     c                   clear                   FVPdfa
034100040401e   1c                   endif
034200040401      *
034300040401     c                   move      FGVlna        $FFV(1)
034400040401     c                   movea     FGVffv        $FFV(2)
034500040401     c                   movea     FGVff2        $FFV(82)
034600040401     c                   movea     FGWff3        $FFV(151)
034700040401     c                   movea     FGWff4        $FFV(231)
034800040401      *
034900040401do  1c                   DOW       xx       < %elem($FFV)
035000040401      *
035100040401     c                   add       1             xx
035200040401      *
035300040401      * Imposto schiera dei P.O. del foglio di viaggio
035400040401      * (50 elementi) per il campo FVPFFV
035500040401if  2c                   if        $FFV(xx) > *zeros
035600040401     c                             and  yy  < %elem(wFFV)
035700040401     c                   add       1             yy
035800040401     c                   move      $FFV(xx)      wFFV(yy)
035900040401e   2c                   endif
036000040401      *
036100040401      * A completamento della schiera scrivo il record e
036200040401      *  ripulisco la schiera
036300040401if  2c                   if             yy  = %elem(wFFV)
036400040401     c                             or  (xx  = %elem($FFV)
036500040401     c                              and yy  > *zeros)
036600040401     c                   z-add     FGVnfv        FVPnfv
036700040401     c                   movea     wFFV          FVPffv
036800040401      *
036900040401     c                   WRITE     FIFVP
037000040401      *
037100041005     c                   reset                   wFFV
037200040401     c                   clear                   yy
037300040401e   2c                   endif
037400040401      *
037500040401e   1c                   ENDDO
037600040401      *
037700040401     c                   ENDSR
037800040401
037900040401** - $Cmd
038000040401CHKOBJ    OBJ(&FILE     )  OBJTYPE(*FILE)
038100040401CLRPFM    FILE(&FILE     )
038200040401CRTDUPOBJ OBJ(&FILE00F  )  FROMLIB(&FLIB     ) OBJTYPE(*FILE) NEWOBJ(&FILE     )
038300040401OVRDBF    FILE(&FILE00F  ) TOFILE(&FILE     )
038400040401DLTOVR    FILE(&FILE00F  )

000100021023     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200030102      * TRTR01R  *---------------------------------------------------*
000300030102      * - TRASMISSIONE File appoggio x invio date                    *
000400021023      *--------------------------------------------------------------*
000500030102     fFidta00f  uf   e             Disk
000600030102     fFidta00t  uf a e             Disk    Usropn
000700030102     f                                     Rename(Fidta000:FidtaT)
000800030102     fTntbe01l  if   e           k Disk
000900021023
001000021023      *---------------------------------------------------------------*
001100021023      * Riepilogo indicatori                                          *
001200021023      *---------------------------------------------------------------*
001300030102      * 30 = Comodo
001400021023      * 99 = Errore su QCMDEXC
001500021023      *---------------------------------------------------------------*
001600021023
001700030102     d Cmd             s             60    Dim(5) Ctdata Perrcd(1)
001800030102     d Dta             s              3    Dim(10)
001900030102     d Trp             s              1    Dim(10)
002000030102     d Tra             s              1    Dim(10)
002100030102     d Trs             s              1    Dim(10)
002200021023
002300021023     d Comman          s             80
002400030102     d DataMax         s              8  0 Inz
002500030102     d DataOggi        s              8  0 Inz
002600030102     d Dateu           s               d   datfmt(*iso)
002700030102     d Kcod            s                   Like(TbeCod) Inz('DTA')
002800021023     d Lenght          s             15  5 Inz(80)
002900030102     d RecDta          s              7  0 Inz
003000030102     d WapeFile        s              1    Inz
003100030102     d W003a           s              3
003200021023     d W236            s            236
003300021023     d xx              s              4  0 Inz
003400021023
003500021023     d Wmbr            ds
003600030102     d  Emme                   1      1    Inz('M')
003700030102     d  Simfela                2      4
003800030102     d  Sede                   5      7    Inz('046')
003900021023
004000030102
004100030102      * Ds di riferimento al file esterno AzUte00F
004200030102     d AzuteDs       e ds                  ExtName(AzUte00F)
004300030102      * Ds per dati organigramma
004400030102     d DDatiUte      e ds
004500030102      * Parametri per richiamo al pgm di controllo profilo utenti
004600030102     d Tibs34Ds      e ds                  Inz
004700030102      * Ds tabella DTA
004800030102     d dDta          e ds
004900021023      * Cerco le librerie di trasmissione
005000021023     d Tibs55Ds      e ds
005100021023      * Trasmissione
005200021023     d DsCmz         e ds
005300021023
005400021023     d Kpjba         e ds
005500021023
005600021023      *---------------------------------------------------------------*
005700021023      *    M A I N
005800021023      *---------------------------------------------------------------*
005900021023
006000021024      * Leggo il file
006100030102     c                   Exsr      LetDta
006200030102      * Trasmetto il file a sede
006300030102If  1c                   If        RecDta > *Zeros
006400021023     c                   Exsr      Trasmetti
006500030102    1c                   EndIf
006600021023
006700021023      * Chiudo i file aperti
006800021023     c                   Clear                   Tibs55Ds
006900021023     c                   Eval      I50Tla = 'C'
007000021023     c                   Call      'TIBS55R'
007100021023     c                   Parm                    Tibs55Ds
007200021023
007300021023     c                   Eval      *InLr = *On
007400021023
007500021023      *---------------------------------------------------------------*
007600030102      * Routine di lettura Fidta                                      *
007700021023      *---------------------------------------------------------------*
007800030102     c     LetDta        BegSr
007900021023
008000021023     c                   Eval      *In99 = *Off
008100021023
008200021023Do  1c                   Do        *Hival
008300030102     c                   Read      Fidta00f
008400021023      * Fine file esce dal ciclo
008500030102     c                   If        %Eof(Fidta00f)
008600021023     c                   Leave
008700021023     c                   EndIf
008800030102      * Se record non trasmesso devo trasmettere
008900030102If  2c                   If        DtaDtr = *Zeros
009000030109     c                   Z-Add     1             xx
009100030102     c     DtaTrd        Lookup    Dta(xx)                                30
009200030102      * per ora solo a sede
009300030102If  3c                   If        *In30 and Trs(xx) = 'S'
009400030102      * Apro il membro di trasmissione se non è già stato aperto
009500030102     c                   If        WapeFile = *Blanks
009600030102     c                   Exsr      Caex
009700030102     c                   EndIf
009800030102      * Apertura del file riuscita
009900030102If  4c                   If        Not *In99
010000030102      * Scrivo il file
010100030102     c                   Eval      DtaDtr = DataOggi
010200030102     c                   Add       1             RecDta
010300030102     c                   Write     FidtaT
010400030102      * Aggiorno Fidta
010500030102     c                   Eval      DtaFtr = 'T'
010600030102     c                   Update    Fidta000
010700030102    4c                   EndIf
010800030102    3c                   EndIf
010900030102      * Se record trasmesso da + di 7 gg cancello
011000030102   x2c                   Else
011100030102     c                   If        DtaDtr <= DataMax
011200030102     c                   Delete    Fidta000
011300021023     c                   EndIf
011400030102    2c                   EndIf
011500021024
011600021023    1c                   EndDo
011700030102
011800030102      * Chiudo il file
011900030102     c                   If        WapeFile = 'S'
012000030102     c                   Close     Fidta00T
012100021023     c                   EndIf
012200021024      * Disalloco i file
012300021023     c                   Clear                   Comman
012400021023     c                   Movel     Cmd(5)        Comman
012500030102     c                   Eval      %Subst(Comman:43:3) = Simfela
012600021023     c                   Call      'QCMDEXC'                            99
012700021023     c                   Parm                    Comman
012800021023     c                   Parm                    Lenght
012900021023
013000021023     c                   EndSr
013100021023
013200021023      *---------------------------------------------------------------*
013300021024      * Esecuzione QCMDEXC sui File
013400021023      *---------------------------------------------------------------*
013500021024     c     Caex          BegSr
013600021024
013700021023      * Controllo se esiste il membro
013800021023     c                   Clear                   Comman
013900021023     c                   Movel     Cmd(1)        Comman
014000030102     c                   Eval      %Subst(Comman:43:3) = Simfela
014100021023     c                   Call      'QCMDEXC'                            99
014200021023     c                   Parm                    Comman
014300021023     c                   Parm                    Lenght
014400021023      * Se non esiste lo creo
014500021023     c                   If        *In99
014600021023     c                   Clear                   Comman
014700021023     c                   Movel     Cmd(2)        Comman
014800030102     c                   Eval      %Subst(Comman:43:3) = Simfela
014900021023     c                   Call      'QCMDEXC'
015000021023     c                   Parm                    Comman
015100021023     c                   Parm                    Lenght
015200021023     c                   EndIf
015300021023      * Controllo se il membro è allocato
015400021023     c                   Clear                   Comman
015500021023     c                   Movel     Cmd(3)        Comman
015600030102     c                   Eval      %Subst(Comman:43:3) = Simfela
015700030102     c                   Call      'QCMDEXC'                            99
015800021023     c                   Parm                    Comman
015900021023     c                   Parm                    Lenght
016000021023      * Se Ok OVRDBF e Open
016100021023     c                   If        Not *In99
016200030102     c                   Clear                   Comman
016300030102     c                   Movel     Cmd(4)        Comman
016400030102     c                   Eval      %Subst(Comman:43:3) = Simfela
016500030102     c                   Call      'QCMDEXC'                            99
016600030102     c                   Parm                    Comman
016700030102     c                   Parm                    Lenght
016800030102     c                   If        Not *In99
016900030102     c                   Open      Fidta00t
017000030102     c                   Eval      WapeFile = 'S'
017100030102     c                   EndIf
017200021023     c                   EndIf
017300021024
017400021023     c                   EndSr
017500021023
017600021023      *---------------------------------------------------------------*
017700021023      * Trasmetto i file                                              *
017800021023      *---------------------------------------------------------------*
017900021023     c     Trasmetti     BegSr
018000021023
018100021023      * Cerco le librerie di trasmissione
018200021023     c                   Clear                   Tibs55Ds
018300030102     c                   Movel     Simfel        I50Ppo
018400030102     c                   Movel     046           I50Apo
018500021023     c                   Call      'TIBS55R'
018600021023     c                   Parm                    Tibs55Ds
018700030102
018800030102     c                   Movel     046           CmzDst
018900030102     c                   Movel     'FIDTA00T'    CmzFld
019000030102     c                   Clear                   CmzMbd
019100030102     c                   Movel     Wmbr          CmzMbd
019200030102     c                   Movel     O50PLa        CmzLbd
019300030102     c                   Movel     'FIDTA00R'    CmzFla
019400030102     c                   Clear                   CmzMba
019500030102     c                   Movel     Wmbr          CmzMba
019600030109     c                   Movel     O50ALg        CmzLba
019700030102     c                   Eval      CmzOri = '0000'
019800030102     c                   Eval      CmzDti = '0000000'
019900030102     c                   Eval      CmzUrg = 'R'
020000030102     c                   Eval      CmzAdd = 'A'
020100030102     c                   Eval      CmzClr = 'S'
020200030102     c                   Eval      CmzFtr = 'S'
020300030102     c                   Clear                   CmzErr
020400030102     c                   Eval      CmzAzn = 'TR02'
020500030102     c                   Movel     O50Asi        CmzSyf
020600030102     c                   Movel     RecDta        CmzNrr
020700030102     c                   Clear                   CmzPrr
020800030102     c                   Call      'TIBS53C'
020900030102     c                   Parm                    O50Lcl
021000030102     c                   Parm                    CmzDst
021100030102     c                   Parm                    CmzFld
021200030102     c                   Parm                    CmzMbd
021300030102     c                   Parm                    CmzLbd
021400030102     c                   Parm                    CmzFla
021500030102     c                   Parm                    CmzMba
021600030102     c                   Parm                    CmzLba
021700030102     c                   Parm                    CmzOri
021800030102     c                   Parm                    CmzDti
021900030102     c                   Parm                    CmzUrg
022000030102     c                   Parm                    CmzAdd
022100030102     c                   Parm                    CmzClr
022200030102     c                   Parm                    CmzFtr
022300030102     c                   Parm                    CmzErr
022400030102     c                   Parm                    CmzAzn
022500030102     c                   Parm                    CmzSyf
022600030102     c                   Parm                    CmzNrr
022700030102     c                   Parm                    CmzPrr
022800030102     c                   Parm                    W236
022900021023
023000021023     c                   EndSr
023100021023
023200021023      *---------------------------------------------------------------*
023300021023      * Sr Iniziale                                                   *
023400021023      *---------------------------------------------------------------*
023500030102     c     *inzsr        BegSr
023600021023
023700021023     c     *Entry        Plist
023800021023     c                   Parm                    Kpjba
023900030102
024000030102      * Reperisco i dati dell'utente
024100030102     c     *DtaAra       Define    §Azute        AzuteDs
024200030102     c     *DtaAra       Define    §Datiute      DDatiUte
024300030102
024400030102     c                   In(E)     *DtaAra
024500030102     c                   If        %Error or Rsut = *Blanks
024600030102     c                   Call      'TIBS34R'
024700030102     c                   Parm                    Tibs34Ds
024800030102     c                   In        *DtaAra
024900030102     c                   EndIf
025000030102
025100030102     c                   Movel     Simfel        Simfela
025200021023
025300021023      * Data del Giorno
025400030102     c                   Movel     *date         DataOggi
025500030102     c     *iso          Movel     *date         Dateu
025600030102      * Data max per cancellazione record già inviati
025700030102     c                   Subdur    7:*d          Dateu
025800030102     c                   Move      Dateu         DataMax
025900021023
026000030102     c                   Clear                   xx
026100030102      * Carico tabella DTA in schiera
026200030102     c     Kcod          Setll     Tntbe01l
026300030102     c                   Do        *Hival
026400030102     c     Kcod          Reade     Tntbe01l
026500030102      * Fine file esce dal ciclo
026600030102     c                   If        %Eof(Tntbe01l)
026700030102     c                   Leave
026800030102     c                   EndIf
026900030102      * Escludo gli annullati
027000030102     c                   If        TbeAtb <> *Blanks
027100030102     c                   Iter
027200030102     c                   EndIf
027300030102
027400030102     c                   Movel     TbeKe1        W003a
027500030102     c                   Movel     TbeUni        dDta
027600030102     c     W003a         Lookup    Dta                                    30
027700030102     c                   If        Not *In30
027800030102     c                   Add       1             xx
027900030102     c                   Movel     W003a         Dta(xx)
028000030102     c                   Movel     §DtaTrpa      Trp(xx)
028100030102     c                   Movel     §DtaTrar      Tra(xx)
028200030102     c                   Movel     §DtaTrse      Trs(xx)
028300030102     c                   EndIf
028400030102     c                   EndDo
028500021023
028600021023     c                   EndSr
028700021023
028800021023**   Cmd
028900030102CHKOBJ  OBJ(FIDTA00T) OBJTYPE(*FILE) MBR(M000046)
029000030102ADDPFM FILE(FIDTA00T)                MBR(M000046)
029100030102ALCOBJ OBJ((FIDTA00T *FILE *EXCL         M000046))
029200030102OVRDBF FILE(FIDTA00T)                MBR(M000046)
029300030102DLCOBJ OBJ((FIDTA00T *FILE *EXCL         M000046))

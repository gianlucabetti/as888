000100041116     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200041116
000300041116      *------------------------------------------------------------------------*
000400041116      *                                                                        *
000500050512      *  INTERROGAZIONE CODICI COLLEGATI X UNIFICAZIONE FATTURA                *
000600041116      *                                                                        *
000700041116      *------------------------------------------------------------------------*
000800041116
000900041116      * ATTENZIONE
001000041116      * CARICO IL SUBFILE SOLO FINO A 9999
001100041116
001200041116      *------------------------------------------------------------------------*
001300041116
001400050512     fCnclp02l  if   e           k disk     rename(Cnclp000:Cnclp02)
001500050512     fCnclp00f  if   e           k disk
001600041116     fCnaco00f  if   e           k disk
001700050512     fFncls01l  if   e           k disk
001800041116     fTabel00f  if   e           k disk
001900051201     Ffnspe03l  if   e           k disk
002000051201     Ffnsp201l  if   e           k disk
002100051201     Ftfntc01l  if   e           k disk
002200050512     fTnta83D   cf   e             workstn  sfile(ta8301S:nrr)
002300041116
002400041116      *------------------------------------------------------------------------*
002500041116      *  RIEPILOGO INDICATORI
002600041116      *------------------------------------------------------------------------*
002700050512      * 01 - Visualizzo sottoconto fatturazione diverso dal cliente passato
002800041116      * 20 - GESTIONE SUBFILE
002900041116      * 21 - GESTIONE SUBFILE
003000050512      * 28 - ERRORE GENERICO DSPF
003100041116      * 30 - Comodo
003200041116      *------------------------------------------------------------------------*
003300041116
003400041116      *   V A R I A B I L I
003500041116     d codut           s                   Like(TblKut) Inz(1)
003600041116     d kCod            s                   Like(TblCod)
003700041116     d kKey            s                   Like(TblKey)
003800050512     d kKsc            s                   Like(AcoKsc)
003900051201     D Kapl            s                   LIKE(NTCapl) inz('C')
004000051201     D Knk1            s                   LIKE(NTCnk1)
004100051201     D Knk2            s                   LIKE(NTCnk2)
004200051201     D Ktnt            s                   LIKE(NTCtnt) inz('84')
004300051201     D Kfls            s                   LIKE(spefls) inz('L')
004400051201     D Kcods           s                   LIKE(specod) inz('001')
004500051201     D comtpe          s                   like(sp2tpe) inz('EM')
004600041116     d nrr             s              4  0
004700041116
004800041116      *   S C H I E R E
004900050512     d msg             s             78    Dim(10) ctdata perrcd(1)
005000041116
005100041116      *   D S   I N T E R N E / E S T E R N E
005200050512     d Param83         ds
005300050513     d  parcli                        7  0
005400041122
005500041116     d                sds
005600041116     d  Vtcpgm                 1     10
005700041116
005800041116     d Azuteds       e ds                  Extname(Azute00f)
005900041116     d dDatiute      e ds
006000050512     d dsff          e ds
006100050513     d ds13          e ds
006200050512     d dtft          e ds
006300041116     d Kpjba         e ds
006400041116     d Tibs02ds      e ds
006500041116     d Tibs34ds      e ds
006600041116
006700041117      *   C O S T A N T I
006800050513      *  titolo videata (lunghezza massima 35)
006900050513     d VtcTit          C                   CONST('* Interrogazione codici colle-
007000050513     d                                     gati *')
007100041116
007200041116      *------------------------------------------------------------------------*
007300041116
007400050513     c                   Eval      recsf = 1
007500050513
007600050512      * Pulisco subfile
007700050512     c                   Clear                   nrr
007800050512     c                   Eval      *In21 = *Off
007900050512     c                   Write     Ta8301c
008000050512     c                   Eval      *In20 = *On
008100050512     c                   Eval      *In21 = *On
008200050513
008300050513      * Carico i dati del cliente passato nel control
008400050512     c                   ExSr      Sr_Carctl
008500050512
008600050513      * Leggo il file
008700050513     c     kClp          Setll     Cnclp02l
008800050512do  2c                   Do        *Hival
008900050513     c     kClp          Reade     Cnclp02l
009000050512
009100050512if  3c                   If        %Eof(Cnclp02l)
009200050512     c                   Leave
009300050512e   3c                   EndIf
009400050513      * se sto leggendo lo stesso codice cliente passato non lo carico
009500050513     c                   If        clpksc = parcli
009600050513     c                   Iter
009700050513     c                   EndIf
009800050512
009900050512if  3c                   If        nrr >= 9998
010000050512     c                   Eval      *In28 = *On
010100050512     c                   Eval      V1cmsg = msg(01)
010200050512     c                   Leave
010300050512e   3c                   EndIf
010400050512
010500050513      * Carico il subfile in base al cliente passato
010600050513      * solo 9999 record xchè il file video è definito così
010700050512     c                   ExSr      Sr_Carsfl
010800050513
010900050513e   3c                   EndDo
011000041116
011100050512      * Emissione del subfile
011200050512     c                   Do        *Hival
011300050512
011400050512      * Se non ho caricato emetto una videata di segnalazione.
011500050512     c                   If        nrr = *Zeros
011600050512     c                   Exfmt     Ta8303d
011700050512     c                   EndIf
011800050512
011900050512      * Se ho caricato emetto il subfile
012000050512     c                   If        nrr > *Zeros
012100050513     c                   Write     Ta8302z
012200050512     c                   Exfmt     Ta8301c
012300050512     c                   EndIf
012400050512
012500050513      * F12=Fine
012600050513     c   kl              Leave
012700050512
012800050513e   3c                   EndDo
012900041116
013000041116     c                   Eval      *InLr = *On
013100050512
013200050512      *------------------------------------------------------------------------*
013300050512      * CARICO CONTROL
013400050512      *------------------------------------------------------------------------*
013500050512     c     Sr_Carctl     BegSr
013600050512
013700050512     c                   Eval      *In01 = *Off
013800050512     c                   Eval      kksc = parcli
013900041117
014000050512      * Anagrafico cliente
014100050512     c     kAco          Chain     Cnaco00f
014200050512     c                   If        Not %Found(Cnaco00f)
014300050512     c                   Leavesr
014400050512     c                   EndIf
014500050512     c                   Eval      v1cksc = acoksc
014600050512     c                   Eval      v1crag = acorag
014700050512     c                   Eval      v1cabl = acoabl
014800050512      * Recupero i dati della fatturazione
014900050512     c     kAco          Chain     Cnclp00f
015000050512     c                   If        Not %Found(Cnaco00f)
015100050512     c                   Leavesr
015200050512     c                   EndIf
015300050512      * Se sottoconto intestazione fattura è diverso dal codice passato
015400050512      * lo visualizzo
015500050512     c                   If        clpscf <> kksc
015600050512     c                   Eval      *In01 = *On
015700050513     c                   Eval      v1cscf = clpscf
015800050512     c                   EndIf
015900050512      * Tipo fattura
016000050512     c                   Clear                   v1ctft
016100050513     c                   Clear                   dtft
016200050512     c                   Clear                   Tibs02ds
016300050512     c                   Eval      t02mod = 'C'
016400050512     c                   Eval      t02sif = knsif
016500050512     c                   Eval      t02cod = 'TFT'
016600050513     c                   Movel(p)  clptft        t02ke1
016700050512     c                   Call      'TIBS02R'
016800050512     c                   Parm                    kpjba
016900050512     c                   Parm                    Tibs02ds
017000050512     c                   If        t02err = *Blanks
017100050512     c                   Eval      dtft = t02uni
017200050512     c                   EndIf
017300050513     c                   Eval      v1ctft = §tftdes1
017400050512
017500050512      * Frequenza fattura
017600050512     c                   Clear                   v1cfft
017700050513     c                   Clear                   dsff
017800050513     c                   Eval      kCod = 'FF'
017900050513     c                   Movel(p)  clpfft        kKey
018000050513     c     kTab          Chain     Tabel00f
018100050513if  2c                   If        %Found(Tabel00f) and
018200050513     c                             TblFlg = *Blanks
018300050513     c                   Eval      dsff = TblUni
018400050513     c                   EndIf
018500050513     c                   Eval      v1cfft = §ffdes1
018600050512
018700050512      * Tipo data Fattura
018800050513     c                   Clear                   v1ctdf
018900050513     c                   Clear                   ds13
019000050513     c                   Eval      kCod = '13'
019100050513     c                   Eval      kKey = %subst(clpfun:1:1)
019200050513     c     kTab          Chain     Tabel00f
019300050513if  2c                   If        %Found(Tabel00f) and
019400050513     c                             TblFlg = *Blanks
019500050513     c                   Eval      ds13 = TblUni
019600050513     c                   EndIf
019700050513     c                   Eval      v1ctdf = §13des1
019800051201     c* Luogo 001 e nota 84
019900051201     C                   EXSR      SR_SPENOTA
020000051201     c                   eval      v1cluo = wfluo
020100051201     c                   eval      v1cnota = wfnota
020200050512
020300050513      * Fattura Unificata
020400050513     c     acoksc        Chain     Fncls01l
020500050513     c                   If        Not %Found(Fncls01l)
020600050513     c                   Leavesr
020700050513     c                   EndIf
020800050513     c                   If        %Subst(clsflo:2:1) = 'S'
020900050513     c                   Eval      v1cfl2 = 'SI'
021000050513     c                   Else
021100050513     c                   Eval      v1cfl2 = 'NO'
021200050513     c                   EndIF
021300051201     C*
021400050513
021500050512     c                   EndSr
021600050513
021700041117      *------------------------------------------------------------------------*
021800041117      * CARICO SUBFILE
021900041117      *------------------------------------------------------------------------*
022000041117     c     Sr_Carsfl     BegSr
022100041117
022200050513     c                   Eval      kksc = clpksc
022300050513
022400050513      * Anagrafico cliente
022500050513     c     kAco          Chain     Cnaco00f
022600050513     c                   If        Not %Found(Cnaco00f)
022700050513     c                   Leavesr
022800050513     c                   EndIf
022900050513     c                   Eval      v1sksc = acoksc
023000050513     c                   Eval      v1srag = acorag
023100050513     c                   Eval      v1sabl = acoabl
023200050513
023300050513      * Tipo fattura
023400050513     c                   Clear                   v1stft
023500050513     c                   Clear                   dtft
023600050513     c                   Clear                   Tibs02ds
023700050513     c                   Eval      t02mod = 'C'
023800050513     c                   Eval      t02sif = knsif
023900050513     c                   Eval      t02cod = 'TFT'
024000050513     c                   Movel(p)  clptft        t02ke1
024100050513     c                   Call      'TIBS02R'
024200050513     c                   Parm                    kpjba
024300050513     c                   Parm                    Tibs02ds
024400050513     c                   If        t02err = *Blanks
024500050513     c                   Eval      dtft = t02uni
024600050513     c                   EndIf
024700050513     c                   Eval      v1stft = §tftdes1
024800050513
024900050513      * Frequenza fattura
025000050513     c                   Clear                   v1sfft
025100050513     c                   Clear                   dsff
025200050513     c                   Eval      kCod = 'FF'
025300050513     c                   Movel(p)  clpfft        kKey
025400050513     c     kTab          Chain     Tabel00f
025500050513if  2c                   If        %Found(Tabel00f) and
025600050513     c                             TblFlg = *Blanks
025700050513     c                   Eval      dsff = TblUni
025800050513     c                   EndIf
025900050513     c                   Eval      v1sfft = §ffdes1
026000050513
026100050513      * Tipo data Fattura
026200050513     c                   Clear                   v1stdf
026300050513     c                   Clear                   ds13
026400050513     c                   Eval      kCod = '13'
026500050513     c                   Eval      kKey = %subst(clpfun:1:1)
026600050513     c     kTab          Chain     Tabel00f
026700050513if  2c                   If        %Found(Tabel00f) and
026800050513     c                             TblFlg = *Blanks
026900050513     c                   Eval      ds13 = TblUni
027000050513     c                   EndIf
027100050513     c                   Eval      v1stdf = §13des1
027200051201     c* Luogo 001 e nota 84
027300051201     C                   EXSR      SR_SPENOTA
027400051201     c                   eval      v1sluo = wfluo
027500051201     c                   eval      v1snota = wfnota
027600050513
027700050513      * Fattura Unificata
027800050513     c     acoksc        Chain     Fncls01l
027900050513     c                   If        Not %Found(Fncls01l)
028000050513     c                   Leavesr
028100050513     c                   EndIf
028200050513     c                   Eval      v1sfl2 = %Subst(clsflo:2:1)
028300041117
028400041117     c                   Add       1             nrr
028500050513     c                   Write     Ta8301s
028600041117
028700041117     c                   EndSr
028800051201     c**********************************************************************
028900051201     c     Sr_spenota    begsr
029000051201     c                   clear                   wfluo             1
029100051201     c                   clear                   wfnota            1
029200051201     c     kspe          chain     fnspe03l
029300051201      *
029400051201    1c                   if        %found(fnspe03l)
029500051201     c                   movel     'I'           wfluo
029600051201      * verifico se c'è indirizzo mail
029700051201     c     ksp2          chain     fnsp201l
029800051201    2c                   if        %found(fnsp201l) and SP2EST <> *BLANKS
029900051201     c                   movel     'M'           wfluo
030000051201    2c                   endif
030100051201    1c                   endif
030200051201
030300051201     c                   eval      Knk1 = '0151' + %editc(acoksc:'X')
030400051201     c     Kntc          chain     Tfntc01l
030500051201    1c                   If        %found(tfntc01l) and ntcrnt <> *blanks
030600051201     c                   eval      wfnota = 'S'
030700051201    1c                   endif
030800051201     c                   endsr
030900041117
031000041116      *------------------------------------------------------------------------*
031100041117      * ROUTINE INIZIALE
031200041116      *------------------------------------------------------------------------*
031300041116     c     *Inzsr        BegSr
031400041116
031500041116     c     *Entry        Plist
031600041116     c                   Parm                    Kpjba
031700050512     c                   Eval      Param83 = Kpjbu
031800041117
031900041116     c     *dtaara       define    §azute        azuteds
032000041116     c     *dtaara       define    §datiute      ddatiute
032100041116     c                   in(E)     *dtaara
032200041116     c                   If        %error  or RSUT = *blanks
032300041116     c                   Clear                   Tibs34ds
032400041116     c                   Call      'TIBS34R'
032500041116     c                   Parm                    Tibs34ds
032600041116     c                   In        *dtaara
032700041116     c                   EndIf
032800041116
032900041116      * KLIST
033000041116     c     kTab          Klist
033100041116     c                   Kfld                    Codut
033200041116     c                   Kfld                    kCod
033300041116     c                   Kfld                    kKey
033400041116
033500041116     c     kAco          Klist
033600041116     c                   Kfld                    Codut
033700041116     c                   Kfld                    DutKci
033800050512     c                   Kfld                    kKsc
033900050513
034000050513     c     kClp          Klist
034100050513     c                   Kfld                    Codut
034200050513     c                   Kfld                    DutKci
034300050513     c                   Kfld                    parcli
034400051201      ** ACCESSO ANAGRAFICHE CLIENTI
034500051201     C     KSPE          KLIST
034600051201     C                   KFLD                    kFLS
034700051201     C                   KFLD                    acoksc
034800051201     C                   KFLD                    kcods
034900051201      * accesso al file luoghi con indirizzo mail
035000051201     c     Ksp2          Klist
035100051201     c                   kfld                    specli
035200051201     c                   kfld                    specod
035300051201     c                   kfld                    comtpe
035400051201      *  file note tfntc01l
035500051201     C     Kntc          KLIST
035600051201     C                   KFLD                    Kapl
035700051201     C                   KFLD                    Knk1
035800051201     C                   KFLD                    Knk2
035900051201     C                   KFLD                    Ktnt
036000041117
036100041116     c                   EndSr
036200041116      *------------------------------------------------------------------------*
036300041116
036400041117** MSG  Lungh. 78                                                            *
036500050512Ampiezza massima subfile superata                                             01

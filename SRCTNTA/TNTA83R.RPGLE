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
014800130321      * Visualizzo sempre blocco "8" anche se il blocco sul file è "7"
014900130321     c                   IF        V1Cabl = '7'
015000130321     c                   eval      V1Cabl = '8'
015100130321     c                   ENDIF
015200050512      * Recupero i dati della fatturazione
015300050512     c     kAco          Chain     Cnclp00f
015400050512     c                   If        Not %Found(Cnaco00f)
015500050512     c                   Leavesr
015600050512     c                   EndIf
015700050512      * Se sottoconto intestazione fattura è diverso dal codice passato
015800050512      * lo visualizzo
015900050512     c                   If        clpscf <> kksc
016000050512     c                   Eval      *In01 = *On
016100050513     c                   Eval      v1cscf = clpscf
016200050512     c                   EndIf
016300050512      * Tipo fattura
016400050512     c                   Clear                   v1ctft
016500050513     c                   Clear                   dtft
016600050512     c                   Clear                   Tibs02ds
016700050512     c                   Eval      t02mod = 'C'
016800050512     c                   Eval      t02sif = knsif
016900050512     c                   Eval      t02cod = 'TFT'
017000050513     c                   Movel(p)  clptft        t02ke1
017100050512     c                   Call      'TIBS02R'
017200050512     c                   Parm                    kpjba
017300050512     c                   Parm                    Tibs02ds
017400050512     c                   If        t02err = *Blanks
017500050512     c                   Eval      dtft = t02uni
017600050512     c                   EndIf
017700050513     c                   Eval      v1ctft = §tftdes1
017800050512
017900050512      * Frequenza fattura
018000050512     c                   Clear                   v1cfft
018100050513     c                   Clear                   dsff
018200050513     c                   Eval      kCod = 'FF'
018300050513     c                   Movel(p)  clpfft        kKey
018400050513     c     kTab          Chain     Tabel00f
018500050513if  2c                   If        %Found(Tabel00f) and
018600050513     c                             TblFlg = *Blanks
018700050513     c                   Eval      dsff = TblUni
018800050513     c                   EndIf
018900050513     c                   Eval      v1cfft = §ffdes1
019000050512
019100050512      * Tipo data Fattura
019200050513     c                   Clear                   v1ctdf
019300050513     c                   Clear                   ds13
019400050513     c                   Eval      kCod = '13'
019500050513     c                   Eval      kKey = %subst(clpfun:1:1)
019600050513     c     kTab          Chain     Tabel00f
019700050513if  2c                   If        %Found(Tabel00f) and
019800050513     c                             TblFlg = *Blanks
019900050513     c                   Eval      ds13 = TblUni
020000050513     c                   EndIf
020100050513     c                   Eval      v1ctdf = §13des1
020200051201     c* Luogo 001 e nota 84
020300051201     C                   EXSR      SR_SPENOTA
020400051201     c                   eval      v1cluo = wfluo
020500051201     c                   eval      v1cnota = wfnota
020600050512
020700050513      * Fattura Unificata
020800050513     c     acoksc        Chain     Fncls01l
020900050513     c                   If        Not %Found(Fncls01l)
021000050513     c                   Leavesr
021100050513     c                   EndIf
021200050513     c                   If        %Subst(clsflo:2:1) = 'S'
021300050513     c                   Eval      v1cfl2 = 'SI'
021400050513     c                   Else
021500050513     c                   Eval      v1cfl2 = 'NO'
021600050513     c                   EndIF
021700051201     C*
021800050513
021900050512     c                   EndSr
022000050513
022100041117      *------------------------------------------------------------------------*
022200041117      * CARICO SUBFILE
022300041117      *------------------------------------------------------------------------*
022400041117     c     Sr_Carsfl     BegSr
022500041117
022600050513     c                   Eval      kksc = clpksc
022700050513
022800050513      * Anagrafico cliente
022900050513     c     kAco          Chain     Cnaco00f
023000050513     c                   If        Not %Found(Cnaco00f)
023100050513     c                   Leavesr
023200050513     c                   EndIf
023300050513     c                   Eval      v1sksc = acoksc
023400050513     c                   Eval      v1srag = acorag
023500050513     c                   Eval      v1sabl = acoabl
023600130321      * Visualizzo sempre blocco "8" anche se il blocco sul file è "7"
023700130321     c                   IF        V1Sabl = '7'
023800130321     c                   eval      V1Sabl = '8'
023900130321     c                   ENDIF
024000050513
024100050513      * Tipo fattura
024200050513     c                   Clear                   v1stft
024300050513     c                   Clear                   dtft
024400050513     c                   Clear                   Tibs02ds
024500050513     c                   Eval      t02mod = 'C'
024600050513     c                   Eval      t02sif = knsif
024700050513     c                   Eval      t02cod = 'TFT'
024800050513     c                   Movel(p)  clptft        t02ke1
024900050513     c                   Call      'TIBS02R'
025000050513     c                   Parm                    kpjba
025100050513     c                   Parm                    Tibs02ds
025200050513     c                   If        t02err = *Blanks
025300050513     c                   Eval      dtft = t02uni
025400050513     c                   EndIf
025500050513     c                   Eval      v1stft = §tftdes1
025600050513
025700050513      * Frequenza fattura
025800050513     c                   Clear                   v1sfft
025900050513     c                   Clear                   dsff
026000050513     c                   Eval      kCod = 'FF'
026100050513     c                   Movel(p)  clpfft        kKey
026200050513     c     kTab          Chain     Tabel00f
026300050513if  2c                   If        %Found(Tabel00f) and
026400050513     c                             TblFlg = *Blanks
026500050513     c                   Eval      dsff = TblUni
026600050513     c                   EndIf
026700050513     c                   Eval      v1sfft = §ffdes1
026800050513
026900050513      * Tipo data Fattura
027000050513     c                   Clear                   v1stdf
027100050513     c                   Clear                   ds13
027200050513     c                   Eval      kCod = '13'
027300050513     c                   Eval      kKey = %subst(clpfun:1:1)
027400050513     c     kTab          Chain     Tabel00f
027500050513if  2c                   If        %Found(Tabel00f) and
027600050513     c                             TblFlg = *Blanks
027700050513     c                   Eval      ds13 = TblUni
027800050513     c                   EndIf
027900050513     c                   Eval      v1stdf = §13des1
028000051201     c* Luogo 001 e nota 84
028100051201     C                   EXSR      SR_SPENOTA
028200051201     c                   eval      v1sluo = wfluo
028300051201     c                   eval      v1snota = wfnota
028400050513
028500050513      * Fattura Unificata
028600050513     c     acoksc        Chain     Fncls01l
028700050513     c                   If        Not %Found(Fncls01l)
028800050513     c                   Leavesr
028900050513     c                   EndIf
029000050513     c                   Eval      v1sfl2 = %Subst(clsflo:2:1)
029100041117
029200041117     c                   Add       1             nrr
029300050513     c                   Write     Ta8301s
029400041117
029500041117     c                   EndSr
029600051201     c**********************************************************************
029700051201     c     Sr_spenota    begsr
029800051201     c                   clear                   wfluo             1
029900051201     c                   clear                   wfnota            1
030000051201     c     kspe          chain     fnspe03l
030100051201      *
030200051201    1c                   if        %found(fnspe03l)
030300051201     c                   movel     'I'           wfluo
030400051201      * verifico se c'è indirizzo mail
030500051201     c     ksp2          chain     fnsp201l
030600051201    2c                   if        %found(fnsp201l) and SP2EST <> *BLANKS
030700051201     c                   movel     'M'           wfluo
030800051201    2c                   endif
030900051201    1c                   endif
031000051201
031100051201     c                   eval      Knk1 = '0151' + %editc(acoksc:'X')
031200051201     c     Kntc          chain     Tfntc01l
031300051201    1c                   If        %found(tfntc01l) and ntcrnt <> *blanks
031400051201     c                   eval      wfnota = 'S'
031500051201    1c                   endif
031600051201     c                   endsr
031700041117
031800041116      *------------------------------------------------------------------------*
031900041117      * ROUTINE INIZIALE
032000041116      *------------------------------------------------------------------------*
032100041116     c     *Inzsr        BegSr
032200041116
032300041116     c     *Entry        Plist
032400041116     c                   Parm                    Kpjba
032500050512     c                   Eval      Param83 = Kpjbu
032600041117
032700041116     c     *dtaara       define    §azute        azuteds
032800041116     c     *dtaara       define    §datiute      ddatiute
032900041116     c                   in(E)     *dtaara
033000041116     c                   If        %error  or RSUT = *blanks
033100041116     c                   Clear                   Tibs34ds
033200041116     c                   Call      'TIBS34R'
033300041116     c                   Parm                    Tibs34ds
033400041116     c                   In        *dtaara
033500041116     c                   EndIf
033600041116
033700041116      * KLIST
033800041116     c     kTab          Klist
033900041116     c                   Kfld                    Codut
034000041116     c                   Kfld                    kCod
034100041116     c                   Kfld                    kKey
034200041116
034300041116     c     kAco          Klist
034400041116     c                   Kfld                    Codut
034500041116     c                   Kfld                    DutKci
034600050512     c                   Kfld                    kKsc
034700050513
034800050513     c     kClp          Klist
034900050513     c                   Kfld                    Codut
035000050513     c                   Kfld                    DutKci
035100050513     c                   Kfld                    parcli
035200051201      ** ACCESSO ANAGRAFICHE CLIENTI
035300051201     C     KSPE          KLIST
035400051201     C                   KFLD                    kFLS
035500051201     C                   KFLD                    acoksc
035600051201     C                   KFLD                    kcods
035700051201      * accesso al file luoghi con indirizzo mail
035800051201     c     Ksp2          Klist
035900051201     c                   kfld                    specli
036000051201     c                   kfld                    specod
036100051201     c                   kfld                    comtpe
036200051201      *  file note tfntc01l
036300051201     C     Kntc          KLIST
036400051201     C                   KFLD                    Kapl
036500051201     C                   KFLD                    Knk1
036600051201     C                   KFLD                    Knk2
036700051201     C                   KFLD                    Ktnt
036800041117
036900041116     c                   EndSr
037000041116      *------------------------------------------------------------------------*
037100041116
037200041117** MSG  Lungh. 78                                                            *
037300050512Ampiezza massima subfile superata                                             01

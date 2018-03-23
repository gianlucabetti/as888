000100021023     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200021023      * TRTR96R  *---------------------------------------------------*
000300021024      * - TRASMISSIONE ANAGRAFICA TESTI                              *
000400021024      * - SCRITTURA TABEL00T X TABELLA 1H                            *
000500021023      *--------------------------------------------------------------*
000600021023     fFntxt01l  uf   e           k Disk
000700021023     fTabel00f  uf   e           k Disk
000800021023     fFntxt00t  uf a e             Disk    Usropn
000900021023     f                                     Rename(Fntxt000:FntxtT)
001000021023     fTabel00t  uf a e             Disk    Usropn
001100021023     f                                     Rename(Tabel:TabelT)
001200021023
001300021023      *---------------------------------------------------------------*
001400021023      * Riepilogo indicatori                                          *
001500021023      *---------------------------------------------------------------*
001600021023      * 99 = Errore su QCMDEXC
001700021023      *---------------------------------------------------------------*
001800021023
001900021023     d cmd             s             60    Dim(5) Ctdata Perrcd(1)
002000021023
002100021023     d Codut           s              1  0 Inz(1)
002200021023     d Comman          s             80
002300021024     d Conta           s              2  0 Inz
002400021023     d Dateu           s              8  0
002500021023     d kcod            s                   Like(TblCod) Inz('1H')
002600021023     d kkey            s                   Like(TblKey)
002700021023     d Lenght          s             15  5 Inz(80)
002800021023     d RecTxt          s              7  0 Inz
002900021024     d RecTab          s              7  0 Inz
003000021024     d Sav_TblFlg      s                   Like(TblFlg)
003100021024     d Sav_TxtAtb      s                   Like(TxtAtb)
003200021023     d Sav_TxtLet      s                   Like(TxtLet)
003300021024     d WapeTab         s              1    Inz
003400021024     d WapeTxt         s              1    Inz
003500021024     d Wfile           s              8    Inz
003600021023     d Wgiro           s              1  0 Inz
003700021024     d Wtabel          s              1    Inz
003800021023     d W236            s            236
003900021023     d xx              s              4  0 Inz
004000021023
004100021023     d Wmbr            ds
004200021023     d                         1      1    Inz('M')
004300021023     d                         2      7    Inz('000000')
004400021023
004500021023      * AS a cui trasmettere
004600021023     d Tibs56Ds      e ds
004700021023     d  Ski                    7    756  0 Dim(250)
004800021023      * Cerco le librerie di trasmissione
004900021023     d Tibs55Ds      e ds
005000021023      * Trasmissione
005100021023     d DsCmz         e ds
005200021023
005300021023     d Kpjba         e ds
005400021023
005500021023      *---------------------------------------------------------------*
005600021023      *    M A I N
005700021023      *---------------------------------------------------------------*
005800021023
005900021024      * Leggo il file
006000021023     c                   Exsr      LetTxt
006100021023      * Trasmetto i file
006200021023     c                   Exsr      Trasmetti
006300021023
006400021023      * Pulisco i membri
006500021023     c                   Exsr      Cancella
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
007600021023      * Routine di lettura Fntxt                                      *
007700021023      *---------------------------------------------------------------*
007800021023     c     LetTxt        BegSr
007900021023
008000021023     c                   Clear                   Wgiro
008100021023     c                   Eval      *In99 = *Off
008200021023
008300021023     c     *Loval        Setll     Fntxt01l
008400021023Do  1c                   Do        *Hival
008500021023     c                   Read      Fntxt01l
008600021023      * Fine file esce dal ciclo
008700021023     c                   If        %Eof(Fntxt01l)
008800021023     c                   Leave
008900021023     c                   EndIf
009000021023      * Leggo solo i record da trasmettere
009100021023     c                   If        TxtFtr <> *Blanks
009200021023     c                   Iter
009300021023     c                   EndIf
009400021024
009500021024      * Per cambio testo lettera cerco la relativa tabella 1H
009600021024If  2c                   If        TxtLet <> Sav_TxtLet
009700021024     c                   Movel     TxtLet        Kkey
009800021023     c     Ktab          Chain     Tabel00f
009900021023     c                   If        %Found(Tabel00f)
010000021024     c                   Clear                   Wtabel
010100021024     c                   Else
010200021024     c                   Eval      Wtabel = 'N'
010300021023     c                   EndIf
010400021023     c                   Eval      Sav_TxtLet = TxtLet
010500021024    2c                   EndIf
010600021024
010700021024      * Se nel testo è stato modificato il tipo trasmissione rispetto alla tabella
010800021024      * devo inviare i record con il flag di annullamento
010900021024
011000021024      * Ma lo faccio solo se esiste la tabella
011100021024If  2c                   If        Wtabel <> 'N'
011200021024
011300021024If  3c                   If        TblFtt <> TxtFtt
011400021024      * Apro il membro di trasmissione se non è già stato aperto
011500021024     c                   If        Wgiro = *Zeros
011600021024     c                   Eval      Wfile = 'FNTXT00T'
011700021024     c                   Exsr      Caex
011800021024     c                   If        Not *In99
011900021024     c                   Open      Fntxt00t
012000021024     c                   Eval      WapeTxt = 'S'
012100021024     c                   EndIf
012200021024     c                   Eval      Wfile = 'TABEL00T'
012300021024     c                   Exsr      Caex
012400021024     c                   If        Not *In99
012500021024     c                   Open      Tabel00t
012600021024     c                   Eval      WapeTab = 'S'
012700021024     c                   EndIf
012800021024     c                   Eval      Wgiro = 1
012900021024     c                   EndIf
013000021024      * Apertura dei file riuscita
013100021024If  4c                   If        Not *In99
013200021024      * Scrivo il file
013300021024If  5c                   If        TblFtt = '1'
013400021024     c                   Eval      Sav_TxtAtb = TxtAtb
013500021024     c                   Eval      TxtAtb = '*'
013600021024     c                   Write     FntxtT
013700021024     c                   Add       1             RecTxt
013800021024     c                   Eval      TxtAtb = Sav_TxtAtb
013900021024
014000021024     c                   Eval      Sav_TBlFlg = TblFlg
014100021024     c                   Eval      TblFlg = '*'
014200021024     c                   Write     TabelT
014300021024     c                   Add       1             RecTab
014400021024     c                   Eval      TblFlg = Sav_TblFlg
014500021024    5c                   EndIf
014600021024
014700021024      * Aggiorno tabella con il tipo trasmissione dei testi
014800021024     c                   If        WTabel = *Blanks
014900021024     c                   Eval      TblFtt = TxtFtt
015000021024     c                   Update    Tabel
015100021024     c                   Eval      WTabel = 'S'
015200021024     c                   EndIf
015300021024    4c                   EndIf
015400021024
015500021024    3c                   EndIf
015600021024    2c                   EndIf
015700021024
015800021024      * Se trasmissione a tutti scrivo il file
015900021024If  2c                   If        TxtFtt <> *Blanks
016000021024
016100021024      * Eseguo QcmdExc su Fntxt se primo giro
016200021023     c                   If        Wgiro = *Zeros
016300021024     c                   Eval      Wfile = 'FNTXT00T'
016400021024     c                   Exsr      Caex
016500021024     c                   If        Not *In99
016600021024     c                   Open      FNTXT00T
016700021024     c                   Eval      WapeTxt = 'S'
016800021024     c                   EndIf
016900021023     c                   Eval      Wgiro = 1
017000021023     c                   EndIf
017100021023      * Se ok QcmdExc
017200021024If  3c                   If        Not *In99
017300021023     c                   add       1             RecTxt
017400021023     c                   Write     FntxtT
017500021023      * Aggiorno Fntxt
017600021023     c                   Eval      TxtFtr = 'T'
017700021023     c                   Eval      TxtDtr = Dateu
017800021023     c                   Update    Fntxt000
017900021024    3c                   EndIf
018000021024    2c                   EndIf
018100021023    1c                   EndDo
018200021024      * Chiudo i file
018300021023     c                   If        WapeTxt = 'S'
018400021023     c                   Close     Fntxt00T
018500021023     c                   EndIf
018600021024     c                   If        WapeTab = 'S'
018700021024     c                   Close     Tabel00T
018800021024     c                   EndIf
018900021024      * Disalloco i file
019000021023     c                   Clear                   Comman
019100021023     c                   Movel     Cmd(5)        Comman
019200021024     c                   Eval      %Subst(Comman:13:8) = 'FNTXT00T'
019300021023     c                   Call      'QCMDEXC'                            99
019400021023     c                   Parm                    Comman
019500021023     c                   Parm                    Lenght
019600021024
019700021024     c                   Clear                   Comman
019800021024     c                   Movel     Cmd(5)        Comman
019900021024     c                   Eval      %Subst(Comman:13:8) = 'TABEL00T'
020000021024     c                   Call      'QCMDEXC'                            99
020100021024     c                   Parm                    Comman
020200021024     c                   Parm                    Lenght
020300021023
020400021023     c                   EndSr
020500021023
020600021023      *---------------------------------------------------------------*
020700021024      * Esecuzione QCMDEXC sui File
020800021023      *---------------------------------------------------------------*
020900021024     c     Caex          BegSr
021000021024
021100021023      * Controllo se esiste il membro
021200021023     c                   Clear                   Comman
021300021023     c                   Movel     Cmd(1)        Comman
021400021024     c                   Eval      %Subst(Comman:13:8) = Wfile
021500021023     c                   Call      'QCMDEXC'                            99
021600021023     c                   Parm                    Comman
021700021023     c                   Parm                    Lenght
021800021023      * Se non esiste lo creo
021900021023     c                   If        *In99
022000021023     c                   Clear                   Comman
022100021023     c                   Movel     Cmd(2)        Comman
022200021024     c                   Eval      %Subst(Comman:13:8) = Wfile
022300021023     c                   Call      'QCMDEXC'
022400021023     c                   Parm                    Comman
022500021023     c                   Parm                    Lenght
022600021023     c                   EndIf
022700021023      * Controllo se il membro è allocato
022800021023     c                   Clear                   Comman
022900021023     c                   Movel     Cmd(3)        Comman
023000021024     c                   Eval      %Subst(Comman:13:8) = Wfile
023100021023     c                   Call      'QCMDEXC'                            99
023200021023     c                   Parm                    Comman
023300021023     c                   Parm                    Lenght
023400021023      * Se Ok OVRDBF e Open
023500021023     c                   If        Not *In99
023600021024     c                   Exsr      Caex1
023700021023     c                   EndIf
023800021024
023900021023     c                   EndSr
024000021024
024100021023      *---------------------------------------------------------------*
024200021024      * Esecuzione QCMDEXC sui File                                   *
024300021023      *---------------------------------------------------------------*
024400021024     c     Caex1         BegSr
024500021023
024600021023     c                   Clear                   Comman
024700021023     c                   Movel     Cmd(4)        Comman
024800021024     c                   Eval      %Subst(Comman:13:8) = Wfile
024900021023     c                   Call      'QCMDEXC'                            99
025000021023     c                   Parm                    Comman
025100021023     c                   Parm                    Lenght
025200021023
025300021023     c                   EndSr
025400021023
025500021023      *---------------------------------------------------------------*
025600021023      * Trasmetto i file                                              *
025700021023      *---------------------------------------------------------------*
025800021023     c     Trasmetti     BegSr
025900021023
026000021023     c                   Z-Add     1             xx
026100021023
026200021024      * Se il file è pieno trasmetto
026300021024If  1c                   If        RecTxt > *Zeros
026400021023      * Trasmetto a tutti gli As
026500021023Do  2c                   DoW       Ski(xx) > *Zeros
026600021023      * Cerco le librerie di trasmissione
026700021023     c                   Clear                   Tibs55Ds
026800021024     c                   Movel     046           I50Ppo
026900021023     c                   Movel     Ski(xx)       I50Apo
027000021023     c                   Call      'TIBS55R'
027100021023     c                   Parm                    Tibs55Ds
027200021023
027300021024     c                   ExSr      TrasTxt
027400021023
027500021023     c                   Add       1             xx
027600021023    2c                   EndDo
027700021023     c
027800021023    1c                   EndIf
027900021023
028000021023     c                   EndSr
028100021023      *---------------------------------------------------------------*
028200021024      * Trasmissione di Fntxt                                         *
028300021023      *---------------------------------------------------------------*
028400021024     c     TrasTxt       BegSr
028500021023
028600021023     c                   Movel     Ski(xx)       CmzDst
028700021024     c                   Movel     'FNTXT00T'    CmzFld
028800021023     c                   Clear                   CmzMbd
028900021023     c                   Movel     Wmbr          CmzMbd
029000021023     c                   Movel     O50PLg        CmzLbd
029100021024     c                   Movel     'FNTXT00R'    CmzFla
029200021023     c                   Clear                   CmzMba
029300021023     c                   Movel     Wmbr          CmzMba
029400021023     c                   Movel     O50ALg        CmzLba
029500021023     c                   Eval      CmzOri = '0000'
029600021023     c                   Eval      CmzDti = '0000000'
029700021023     c                   Eval      CmzUrg = 'R'
029800021023     c                   Eval      CmzAdd = 'A'
029900021023     c                   Eval      CmzClr = 'N'
030000021023     c                   Eval      CmzFtr = 'S'
030100021023     c                   Clear                   CmzErr
030200021024     c                   Eval      CmzAzn = 'TR97'
030300021023     c                   Movel     O50Asi        CmzSyf
030400021024     c                   Movel     RecTxt        CmzNrr
030500021023     c                   Clear                   CmzPrr
030600021023     c                   Call      'TIBS53C'
030700021023     c                   Parm                    O50Lcl
030800021023     c                   Parm                    CmzDst
030900021023     c                   Parm                    CmzFld
031000021023     c                   Parm                    CmzMbd
031100021023     c                   Parm                    CmzLbd
031200021023     c                   Parm                    CmzFla
031300021023     c                   Parm                    CmzMba
031400021023     c                   Parm                    CmzLba
031500021023     c                   Parm                    CmzOri
031600021023     c                   Parm                    CmzDti
031700021023     c                   Parm                    CmzUrg
031800021023     c                   Parm                    CmzAdd
031900021023     c                   Parm                    CmzClr
032000021023     c                   Parm                    CmzFtr
032100021023     c                   Parm                    CmzErr
032200021023     c                   Parm                    CmzAzn
032300021023     c                   Parm                    CmzSyf
032400021023     c                   Parm                    CmzNrr
032500021023     c                   Parm                    CmzPrr
032600021023     c                   Parm                    W236
032700021023
032800021023     c                   EndSr
032900021023
033000021023      *---------------------------------------------------------------*
033100021023      * Pulisco i file di trasmissione                                *
033200021023      *---------------------------------------------------------------*
033300021023     c     Cancella      BegSr
033400021023
033500021024If  1c                   If        RecTxt > *Zeros
033600021024     c                   ExSr      Caex1
033700021023If  2c                   If        Not *In99
033800021024     c                   Open      Fntxt00t
033900021023Do  3c                   Do        *Hival
034000021024     c                   Read      Fntxt00t
034100021024     c                   If        %Eof(Fntxt00t)
034200021023     c                   Leave
034300021023     c                   EndIf
034400021024     c                   Delete    Fntxtt
034500021023    3c                   EndDo
034600021024     c                   Close     Fntxt00T
034700021023    2c                   EndIf
034800021023    1c                   EndIf
034900021023
035000021023     c                   EndSr
035100021023
035200021023      *---------------------------------------------------------------*
035300021023      * Sr Iniziale                                                   *
035400021023      *---------------------------------------------------------------*
035500021023     c     *InzSr        BegSr
035600021023
035700021023     c     *Entry        Plist
035800021023     c                   Parm                    Kpjba
035900021023
036000021023      * Data del Giorno
036100021023     c                   Movel     *date         Dateu
036200021023
036300021023      * Carico i p.o. a cui trasmettere
036400021023     c                   Clear                   Tibs56Ds
036500021023     c                   Eval      I56Ppo = 046
036600021023     c                   Eval      I56Mod = 'GRU'
036700021023     c                   Call      'TIBS56R'
036800021023     c                   Parm                    Tibs56Ds
036900021023
037000021023     c     Ktab          Klist
037100021023     c                   Kfld                    Codut
037200021023     c                   Kfld                    Kcod
037300021023     c                   Kfld                    Kkey
037400021023
037500021023     c                   EndSr
037600021023
037700021023**   Cmd
037800021023CHKOBJ  OBJ(xxxxx00T) OBJTYPE(*FILE) MBR(M000000)
037900021023ADDPFM FILE(xxxxx00T)                MBR(M000000)
038000021023ALCOBJ OBJ((xxxxx00T *FILE *EXCL         M000000))
038100021023OVRDBF FILE(xxxxx00T)                MBR(M000000)
038200021023DLCOBJ OBJ((xxxxx00T *FILE *EXCL         M000000))

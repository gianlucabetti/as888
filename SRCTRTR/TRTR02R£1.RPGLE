000100021024     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200030102      * TRTR02R  *---------------------------------------------------*
000300030102      * - RICEZIONE File appoggio x invio date                       *
000400021024      *--------------------------------------------------------------*
000500040607     fFidta01r  uf   e           k Disk    Usropn
000600051109     fTnbla01l  if   e           k Disk
000700051109     ffnblp01l  if   e           k Disk    usropn
000800051109     fTitas30c  uf a e           k Disk
000900040531     fFiar531c  uf a e           k Disk
001000030108     fAzorg01l  if   e           k Disk
001100030108     fFidta00e  o    e             Disk    Rename(Fidta000:Fidta0E)
001200030108     fPrtf198   o    f  198        Printer Oflind(*InOf)
001300021024
001400021024      *---------------------------------------------------------------*
001500021024      * Riepilogo indicatori                                          *
001600021024      *---------------------------------------------------------------*
001700030103      * 01 = Titas000
001800030103      * 02 = Titas010
001900030103      * 03 = Titasp00
002000030103      * 11 = Fiar5000
002100030103      * 12 = Fiar5p00
002200030108      * 20 = Aggiorno x DET
002300030108      * 21 = Aggiorno x DUT
002400021024      * 30 = Comodo
002500030108      * 98 = Stampo Testata per la prima volta
002600021024      * 99 = Errore su QCMDEXC
002700021024      *---------------------------------------------------------------*
002800021024
002900051118     d Cmd             s             60    Dim(7) Ctdata Perrcd(1)
003000040614     d Cmd1            s             79    Dim(1) Ctdata Perrcd(1)
003100051109     d Err             s             35    Dim(4) Ctdata Perrcd(1)
003200030108     d Skpo            s              3  0 Dim(5)
003300021024
003400021024     d Comman          s             80
003500030210     d Conta           s              2  0 Inz
003600030108     d DataMax         s              8  0 Inz
003700030108     d DataOggi        s              8  0 Inz
003800040609     d DataSped        s              8  0 Inz(20040719)
003900030214     d DataStp         s              8  0 Inz
004000030108     d Dateu           s               d   datfmt(*iso)
004100030108     d DesPo           s             20
004200040604     d kAr5Aas         s                   Like(Ar5Aas)
004300040604     d kAr5Lnp         s                   Like(Ar5Lnp)
004400040604     d kAr5Nrs         s                   Like(Ar5Nrs)
004500040604     d kAr5Nsp         s                   Like(Ar5Nsp)
004600040604     d kAr5Prg         s                   Like(Ar5Prg)
004700030103     d KAr5Trd         s                   Like(Ar5Trd) Inz('TRS')
004800021024     d Lenght          s             15  5 Inz(80)
004900030108     d xx              s              2  0
005000040728     d waggio          s              1    inz('0')
005100040728     d waggtitas       s              1    inz('0')
005200040728     d wdam            s              1    inz('0')
005300041129     d wduc            s              1    inz('0')
005400040728     d wDtaDam         s                   Like(DtaDta)
005500041129     d wDtaDuc         s                   Like(DtaDta)
005600030108     d wDtaDta         s                   Like(DtaDta)
005700040604     d woktitas        s              1    inz('0')
005800040728     d wtrs            s              1    inz('0')
005900030108     d w0140           s             14  0
006000051118     d libr            s              3
006100030108
006200040604     d                 ds
006300040604     d dtasped                 1     16  0
006400040604     d DtaAas                  1      4  0
006500040604     d DtaLnp                  5      7  0
006600040604     d DtaNrs                  8      9  0
006700040604     d DtaNsp                 10     16  0
006800040604
006900040607     d                 ds
007000040607     d oldsped                 1     16  0 Inz
007100040607     d oldaas                  1      4  0
007200040607     d oldlnp                  5      7  0
007300040607     d oldnrs                  8      9  0
007400040607     d oldnsp                 10     16  0
007500040604
007600040604     d                 ds
007700040604     d TasDsp                  1      8  0
007800040604     d TasAas                  1      4  0
007900040604     d TasMgs                  5      8  0
008000040604
008100030108     d WlbDat          ds                  Inz
008200030108     d  G02Dat                 1      8  0
008300030108     d  G02Inv                 9     16  0
008400030108     d  G02Err                17     17
008500030108     d  G02Tgi                18     22  0
008600030103
008700030109     d dAr5Trs       e ds                  Inz
008800040604     d newr5trs      e ds                  extname(dar5trs)
008900040604     d                                     prefix(new_)
009000021024
009100051109     d og143         e ds
009200051109     d Kpjba         e ds
009300021024     d  Libsys                92    101
009400021024     d  Legfil               483    492
009500021024     d  Mbrfil               493    502
009600030108     d  Wpo                  494    496  0
009700021024
009800030103      *------------------------------------------------------------------------*
009900030103      * INPUT DEI FILES
010000030103      *------------------------------------------------------------------------*
010100030103     iTitas000      01
010200030103     iTitas010      02
010300030103     iTitasp00      03
010400030103
010500030103     iFiar5000      11
010600030103     iFiar5p00      12
010700021024      *---------------------------------------------------------------*
010800021024
010900021024      * Verifica l'esistenza del membro di ricezione
011000021024     c                   Exsr      Esimbr
011100021024
011200021024      * Operazioni di ricezione
011300021024If  1c                   If        Not *In99
011400021024     c                   Exsr      Ricmbr
011500021024
011600021024      * Disalloca il membro di ricezione
011700021024     c                   Exsr      Dismbr
011800021024     c                   EndIf
011900021024
012000021024     c                   SETON                                        LR
012100040604
012200021024      *--------------------------------------------------------------------------------------------*
012300021024      * Verifica l'esistenza del membro di ricezione
012400021024      *--------------------------------------------------------------------------------------------*
012500021024     c     Esimbr        Begsr
012600021024
012700021024      * controlla esistenza membro di ricezione, se non esiste chiude il programma
012800021024     c                   Clear                   Comman
012900021024     c                   Movel     Cmd(1)        Comman
013000021024     c                   Eval      %Subst(Comman:45:10) = Mbrfil
013100021024     c                   Call      'QCMDEXC'                            99
013200021024     c                   Parm                    Comman
013300021024     c                   Parm                    Lenght
013400040614
013500040614      * non trovo il membro faccio addlfm
013600040614If  1c                   If        *in99
013700040614     c                   Clear                   Comman
013800040614     c                   Movel     Cmd1(1)       Comman
013900040614     c                   Eval      %Subst(Comman:27:10) = Mbrfil
014000040614     c                   Eval      %Subst(Comman:67:10) = Mbrfil
014100040614     c                   Call      'QCMDEXC'                            99
014200040614     c                   Parm                    Comman
014300040614     c                   Parm                    Lenght
014400040614    1c                   EndIf
014500021024
014600021024      * alloca il membro di ricezione, se non riesce chiude il programma
014700021024If  1c                   If        Not *in99
014800021024     c                   Clear                   Comman
014900021024     c                   Movel     Cmd(2)        Comman
015000021024     c                   Eval      %Subst(Comman:45:10) = Mbrfil
015100021024     c                   Call      'QCMDEXC'                            99
015200021024     c                   Parm                    Comman
015300021024     c                   Parm                    Lenght
015400021024
015500021024      * esegue sovrapposizione membro di ricezione, se non riesce chiude il programma
015600021024If  2c                   If        Not *in99
015700021024     c                   Clear                   Comman
015800021024     c                   Movel     Cmd(3)        Comman
015900021024     c                   Eval      %Subst(Comman:45:10) = Mbrfil
016000021024     c                   Call      'QCMDEXC'                            99
016100021024     c                   Parm                    Comman
016200021024     c                   Parm                    Lenght
016300021024    2c                   EndIf
016400021024    1c                   EndIf
016500051109    C
016600051109     C* ESEGUO OVRDBF PER FNBLP01L
016700051109     c                   Clear                   Comman
016800051118     c                   Movel     Cmd(7)        Comman
016900051118     c                   Eval      %Subst(Comman:18:3) = '201'
017000051118     c                   Eval      libr='201'
017100051109     c                   Call      'QCMDEXC'                            99
017200051109     c                   Parm                    Comman
017300051109     c                   Parm                    Lenght
017400051109     c*
017500051109     c                   if        *in99
017600051118     c                   Eval      %Subst(Comman:18:3) = 'PRD'
017700051118     c                   Eval      libr='PRD'
017800051109     c                   Call      'QCMDEXC'                            99
017900051109     c                   Parm                    Comman
018000051109     c                   Parm                    Lenght
018100060517     c                   endif
018200060517     c
018300051109     c                   if        not *in99
018400051118     c                   Clear                   Comman
018500051118     c                   Movel     Cmd(6)        Comman
018600051118     c                   Eval      %Subst(Comman:43:3) = libr
018700051118     c                   Call      'QCMDEXC'                            99
018800051118     c                   Parm                    Comman
018900051118     c                   Parm                    Lenght
019000051109     c                   open      fnblp01l
019100051109     c                   endif
019200021024
019300021024     c                   EndSr
019400021024
019500021024      *--------------------------------------------------------------------------------------------*
019600021024      * Operazioni di ricezione
019700021024      *--------------------------------------------------------------------------------------------*
019800021024     c     Ricmbr        BegSr
019900021024
020000021024      * apre il file di ricezione
020100040604     c                   Open      Fidta01r
020200021024
020300021024      * legge il file di ricezione
020400021024Do  1c                   Do        *Hival
020500040604     c                   Read      Fidta01r
020600021024      * Fine file
020700040604     c                   If        %Eof(Fidta01r)
020800021024     c                   Leave
020900021024     c                   EndIf
021000021024
021100040728     c                   If        oldsped <> dtasped
021200040728      * Aggiorno i dati su FIAR5 e su TITAS
021300040728     c                   If        waggio = *On
021400040728     c                   ExSr      Sr_Aggio
021500040728     c                   Eval      waggio = *Off
021600040728     c                   Eval      wdam = *Off
021700041129     c                   Eval      wduc = *Off
021800040728     c                   Eval      wtrs = *Off
021900040728     c                   EndIf
022000040728      * controllo se esiste la spedizione su TITAS
022100040728     c                   ExSr      Sr_Titas
022200040728      * se non ho trovato la bolla torno a leggere
022300040728     c                   If        woktitas = *Off
022400040728     c                   Iter
022500040728     c                   EndIf
022600040728      * salvo il n. di spedizione
022700040728     c                   Eval      oldsped = dtasped
022800040728     c                   EndIf
022900040728
023000040728      * scrive/aggiorna il record fisico e cancella il rcd di ricezione
023100040728     c                   Exsr      Scrpfm
023200040728    1c                   EndDo
023300040728
023400040728      * Aggiorno i dati su FIAR5 e su TITAS dell'ultima bolla
023500040728     c                   If        waggio = *On
023600040728     c                   ExSr      Sr_Aggio
023700040728     c                   EndIf
023800021024
023900021024      * chiude il file di ricezione
024000040604     c                   Close     Fidta01r
024100021024
024200021024     c                   EndSr
024300040604
024400040604      *--------------------------------------------------------------------------------------------*
024500040604      * Controllo se esiste la spedizione su TITAS
024600040604      *--------------------------------------------------------------------------------------------*
024700040604     c     Sr_Titas      BegSr
024800040604
024900040604     c                   Eval      woktitas = *On
025000040604     c                   Clear                   TasDsp
025100040604
025200040604     c     KeyTitas      Setll     Titas30c
025300040604      * Bolla non trovata
025400040604if  1c                   If        Not %Equal(Titas30c)
025500040604     c                   Eval      woktitas = *Off
025600051109     c                   clear                   dtader
025700051109     c* Se la trovo tra le bolle annullate, deleto fidta senza stampare
025800051109     c*  errore
025900051109     c
026000051109     c     keytitas      chain     tnbla01l
026100051109    2c                   if        %found(tnbla01l)
026200051109     c                   Eval      DtaDer = Err(3)
026300051109     c                   Write     Fidta0E
026400051118     c                   Delete    Fidta000
026500051109   x2c                   else
026600051109     c* Se bolla importo verifico se chiusa in partenza senza
026700051109     c*  trasmissione in arrivo e sede
026800051109    3c                   if        %open(fnblp01l)
026900051109     c     keytitas      chain     fnblp01l
027000051109    4c                   if        %found(fnblp01l) and blpft1='N'
027100060517     c*****                        and blpfbr='S'
027200051109     c* Verifico se si tratta di bolla import
027300051109     c     blplnp        chain     azorg01l
027400051109If   c                   If        %Found(Azorg01l)
027500051109     c                   Movel     OrgDe3        og143
027600051109     c                   Else
027700051109     c                   Clear                   og143
027800051109     c                   EndIf
027900060906    5c                   if        §ogntw='EEX'  or §ogntw='DPD'
028000060517     c                   Eval      DtaDer = Err(4)
028100051109     c                   Write     Fidta0E
028200051118     c                   Delete    Fidta000
028300051109    5c                   endif
028400051109    4c                   endif
028500051109    3c                   endif
028600051109    2c                   endif
028700051109     c
028800051109     c* Se la bolla non è nè annullata, nè chiusa in partenza,
028900051109     c*  allora faccio come già faceva
029000051109    2c                   if        dtader=*blanks
029100040604      * non deleto il record su fidta solo se non sono ancora trascorsi tot giorni
029200040604      * dalla ricezione e scrivo l'errore
029300051109if  3c                   If        DtaDtr <= DataMax
029400040604     c                   Eval      DtaDer = Err(1)
029500040604     c                   ExSr      Sr_Errore
029600040604     c                   Delete    Fidta000
029700051109e   3c                   EndIf
029800051109e   2c                   EndIf
029900051109e   1c                   EndIf
030000040604
030100040604     c                   EndSr
030200040728
030300040728      *-------------------------------------------------------------------------
030400040728      * scrive il record fisico
030500040728      *-------------------------------------------------------------------------
030600040728     c     Scrpfm        BegSr
030700040728
030800040728     c                   Eval      *In20 = *Off
030900040728     c                   Eval      *In21 = *Off
031000040728
031100040728      * Memorizzo i dati della DATA ARRIVO MERCE al p.o. di arrivo TITAS
031200040728      * ----------------------------------------------------------------
031300040728If  1c                   If        DtaTrd = 'DAM'
031400040728     c                   Eval      wdam = *On
031500040728     c                   Eval      wdtadam = DtaDta
031600040728      * cancella il record di ricezione
031700040728     c                   Delete    Fidta000
031800040728     c                   Eval      waggio = *On
031900040728     c                   Leavesr
032000040728e   1c                   EndIf
032100041129
032200041129      * Memorizzo i dati della DATA partenza ultimo collo          TITAS
032300041129      * ----------------------------------------------------------------
032400041129If  1c                   If        DtaTrd = 'DUC'
032500041129     c                   Eval      wduc = *On
032600041129     c                   Eval      wdtaduc = DtaDta
032700041129      * cancella il record di ricezione
032800041129     c                   Delete    Fidta000
032900041129     c                   Eval      waggio = *On
033000041129     c                   Leavesr
033100041129e   1c                   EndIf
033200040728
033300040728      * Memorizzo i dati per TRANSITI FIAR5 e TITAS
033400040728      * -------------------------------------------
033500040728     c                   Eval      *In20 = (DtaTrd = 'DET')
033600040728     c                   Eval      *In21 = (DtaTrd = 'DUT')
033700040728
033800040728      * cero l'ultimo progressivo di Fiar5 rcd TRS
033900040728     c                   If        wtrs = *Off
034000040728     c                   Exsr      Sr_Fiar5
034100040728     c                   Eval      wtrs = *On
034200040728     c                   Eval      newr5trs = dar5trs
034300040728     c                   EndIf
034400040728
034500040728      * Aggiorno il p.o. transito interessato
034600040728      * solo se la data è diversa
034700040728     c                   Z-Add     1             xx
034800040728     c     DtaPoc        Lookup    Skpo(xx)                               30
034900040728If  2c                   If        *In30
035000040728     c                   Select
035100040728W   3c                   When      xx = 1
035200040728If  4c   20              If        new_§Ar5Det <> DtaDta
035300040728     c                   Z-Add     DtaDta        new_§Ar5Det
035400040728    4c                   EndIf
035500040728If  4c   21              If        new_§Ar5Dut <> DtaDta
035600040728     c                   Z-Add     DtaDta        new_§Ar5Dut
035700040728    4c                   EndIf
035800040728W   3c                   When      xx = 2
035900040728If  4c   20              If        new_§Ar5De2 <> DtaDta
036000040728     c                   Z-Add     DtaDta        new_§Ar5De2
036100040728    4c                   EndIf
036200040728If  4c   21              If        new_§Ar5Du2 <> DtaDta
036300040728     c                   Z-Add     DtaDta        new_§Ar5Du2
036400040728    4c                   EndIf
036500040728W   3c                   When      xx = 3
036600040728If  4c   20              If        new_§Ar5De3 <> DtaDta
036700040728     c                   Z-Add     DtaDta        new_§Ar5De3
036800040728    4c                   EndIf
036900040728If  4c   21              If        new_§Ar5Du3 <> DtaDta
037000040728     c                   Z-Add     DtaDta        new_§Ar5Du3
037100040728    4c                   EndIf
037200040728W   3c                   When      xx = 4
037300040728If  4c   20              If        new_§Ar5De4 <> DtaDta
037400040728     c                   Z-Add     DtaDta        new_§Ar5De4
037500040728    4c                   EndIf
037600040728If  4c   21              If        new_§Ar5Du4 <> DtaDta
037700040728     c                   Z-Add     DtaDta        new_§Ar5Du4
037800040728    4c                   EndIf
037900040728W   3c                   When      xx = 5
038000040728If  4c   20              If        new_§Ar5De5 <> DtaDta
038100040728     c                   Z-Add     DtaDta        new_§Ar5De5
038200040728    4c                   EndIf
038300040728If  4c   21              If        new_§Ar5Du5 <> DtaDta
038400040728     c                   Z-Add     DtaDta        new_§Ar5Du5
038500040728    4c                   EndIf
038600040728    3c                   EndSl
038700040728
038800040728      * cancella il record di ricezione
038900040728     c                   Delete    Fidta000
039000040728     c                   Eval      waggio = *On
039100040728     c                   Leavesr
039200040728    2c                   EndIf
039300040728
039400040728      * Aggiungo un nuovo p.o. di transito
039500040728      * solo se la data non è a zero
039600040728If  2c                   If        DtaDta > *Zeros
039700040728     c                   Z-Add     1             xx
039800040728     c     *Zeros        Lookup    Skpo(xx)                               30
039900040728If  3c                   If        *In30
040000040728     c                   Eval      skpo(xx) = DtaPoc
040100040728     c                   Select
040200040728W   4c                   When      xx = 1
040300040728     c                   Z-Add     DtaPoc        new_§Ar5Flp
040400040728     c   20              Z-Add     DtaDta        new_§Ar5Det
040500040728     c   21              Z-Add     DtaDta        new_§Ar5Dut
040600040728W   4c                   When      xx = 2
040700040728     c                   Z-Add     DtaPoc        new_§Ar5Fl2
040800040728     c   20              Z-Add     DtaDta        new_§Ar5De2
040900040728     c   21              Z-Add     DtaDta        new_§Ar5Du2
041000040728W   4c                   When      xx = 3
041100040728     c                   Z-Add     DtaPoc        new_§Ar5Fl3
041200040728     c   20              Z-Add     DtaDta        new_§Ar5De3
041300040728     c   21              Z-Add     DtaDta        new_§Ar5Du3
041400040728W   4c                   When      xx = 4
041500040728     c                   Z-Add     DtaPoc        new_§Ar5Fl4
041600040728     c   20              Z-Add     DtaDta        new_§Ar5De4
041700040728     c   21              Z-Add     DtaDta        new_§Ar5Du4
041800040728W   4c                   When      xx = 5
041900040728     c                   Z-Add     DtaPoc        new_§Ar5Fl5
042000040728     c   20              Z-Add     DtaDta        new_§Ar5De5
042100040728     c   21              Z-Add     DtaDta        new_§Ar5Du5
042200040728    4c                   EndSl
042300040728
042400040728      * cancella il record di ricezione
042500040728     c                   Delete    Fidta000
042600040728     c                   Eval      waggio = *On
042700040728     c                   Leavesr
042800040728    3c                   EndIf
042900040728
043000040728      * Tutti i p.o. di transito sono occupati, scrivo errore
043100040728      * e non aggiorno FIAR5
043200040728     c                   Eval      DtaDer = Err(2)
043300040728     c                   Exsr      Sr_Errore
043400040728     c                   Delete    Fidta000
043500040728     c                   Leavesr
043600040728    2c                   EndIf
043700040728
043800040728      * Data a zero per un transito che non è su Fiar5 cancello il record
043900040728      * e non aggiorno FIAR5
044000040728     c                   Delete    Fidta000
044100040728     c                   Leavesr
044200040728
044300040728     c                   EndSr
044400040607
044500040607      *--------------------------------------------------------------------------------------------*
044600040607      * Cerco ultimo progressivo di FIAR5 rcd TRS
044700040607      *--------------------------------------------------------------------------------------------*
044800040607     c     Sr_Fiar5      BegSr
044900040607
045000040607     c                   Clear                   dAr5Trs
045100040607     c                   Clear                   Skpo
045200040607     c                   Clear                   kAr5Prg
045300040607
045400040607     c     keyFiar5      Setgt     Fiar531c
045500040607     c                   Do        *Hival
045600040607     c     keyFiar5      Readpe(n) Fiar531c
045700040607     c                   If        %Eof(Fiar531c)
045800040607     c                   Leave
045900040607     c                   EndIf
046000040607     c                   Movel     Ar5Uni        dAr5Trs
046100040607     c                   Eval      Skpo(1) = §Ar5Flp
046200040607     c                   Eval      Skpo(2) = §Ar5Fl2
046300040607     c                   Eval      Skpo(3) = §Ar5Fl3
046400040607     c                   Eval      Skpo(4) = §Ar5Fl4
046500040607     c                   Eval      Skpo(5) = §Ar5Fl5
046600040607     c                   Eval      kAr5Prg = Ar5Prg
046700040607     c                   Leave
046800040607     c                   EndDo
046900040607
047000040607     c                   EndSr
047100040604
047200040728      *-------------------------------------------------------------------------
047300040728      * AGGIORNO I DATI
047400040728      *-------------------------------------------------------------------------
047500040728     c     Sr_Aggio      BegSr
047600040728
047700040728     c                   Clear                   Conta
047800040728
047900040728      * Leggo TITAS
048000040728     c     keyTitas1     Setll     Titas30c
048100040728Do  1c                   Do        *Hival
048200040728     c                   Setoff                                       010203
048300040728     c     keyTitas1     Reade     Titas30c
048400040728If  2c                   If        %Eof(Titas30c)
048500040728     c                   Leave
048600040728e   2c                   EndIf
048700040728
048800040728     c                   Eval      waggtitas = *Off
048900040728     c                   add       1             conta
049000040728
049100040728      * --> DAM
049200040728if  2c                   If        wdam = *On
049300040728      * Aggiorno solo se la data e diversa
049400040728If  3c                   If        TasDti <> wDtaDam
049500040728      * Se ricevo la data a 0 aggiorno solo se la bolla non è ancora stata
049600040728      * consegnata
049700040728If  4c                   If        wDtaDam = *Zeros and TasDcm > *Zeros
049800040728     c                   Goto      noagg
049900040728    4c                   EndIf
050000040728     c                   Z-Add     wDtaDam       TasDti
050100040728     c                   Clear                   TasHti
050200040728     c                   eval      waggtitas = *On
050300040728     c     Noagg         Tag
050400040728e   3c                   EndIf
050500040728e   2c                   EndIf
050600041129     c
050700041129      * --> DUC
050800041129if  2c                   If        wduc = *On
050900041129      * Aggiorno solo se la data e diversa
051000041129      * (in realtà non si può ricevere una data part ultimo collo =0)
051100041129      *
051200041129If  3c                   If        Tasduc <> wDtaduc
051300041129      * Se ricevo la data a 0 aggiorno solo se la bolla non è ancora stata
051400041129      * consegnata
051500041129If  4c                   If        wDtaDuc = *Zeros and TasDcm > *Zeros
051600041129     c                   Goto      noaggduc
051700041129    4c                   EndIf
051800041129     c                   Z-Add     wDtaDuc       TasDuc
051900041129     c                   eval      waggtitas = *On
052000041129     c     Noaggduc      Tag
052100041129e   3c                   EndIf
052200041129e   2c                   EndIf
052300040728
052400040728      * --> DET/DUT
052500040728if  2c                   If        wtrs = *On
052600040728      * la bolla ha data spedizione <  a 19/07/2004
052700040728if  3c                   If        TasDsp < datasped
052800040728     c                             and
052900040728     c                             (Tasflp = 0 or Tasfl2 = 0)
053000040728      * Aggiorno solo se i primi 2 p.o. transito sono diversi dal primo
053100040728      * e dal secondo su TITAS
053200040728      * aggiorno il primo libero
053300040728if  4c                   If        skpo(1) > 0 and Tasflp <> skpo(1) and
053400040728     c                             skpo(1) <> Tasfl2
053500040728if  5c                   If        Tasflp = *Zeros
053600040728     c                   Eval      Tasflp = Skpo(1)
053700040728     c                   eval      waggtitas = *On
053800040728   x5c                   else
053900040728if  6c                   If        Tasfl2 = *Zeros
054000040728     c                   Eval      TasFl2 = skpo(1)
054100040728     c                   eval      waggtitas = *On
054200040728    6c                   endif
054300040728    5c                   endif
054400040728    4c                   endif
054500040728if  4c                   If        skpo(2) > 0 and Tasflp <> skpo(2) and
054600040728     c                             skpo(2) <> Tasfl2
054700040728if  5c                   If        Tasflp = *Zeros
054800040728     c                   Eval      Tasflp = Skpo(2)
054900040728     c                   eval      waggtitas = *On
055000040728   x5c                   else
055100040728if  6c                   If        Tasfl2 = *Zeros
055200040728     c                   Eval      TasFl2 = skpo(2)
055300040728     c                   eval      waggtitas = *On
055400040728    6c                   endif
055500040728    5c                   endif
055600040728    4c                   endif
055700040728      * la bolla ha data spedizione >= a 19/07/2004
055800040728   x3c                   Else
055900040728if  4c                   If        dAr5Trs <> newr5Trs
056000040728     c                   Eval      TasFlp = kAr5Prg
056100040728     c                   Add       1             TasFlp
056200040728     c                   eval      waggtitas = *On
056300040728e   4c                   EndIf
056400040728e   3c                   EndIf
056500040728e   2c                   EndIf
056600040728
056700040728      * Aggiorno TITAS
056800040728if  2c                   If        waggtitas = *On
056900040728     c   01              Update    Titas000
057000040728     c   02              Update    Titas010
057100040728     c   03              Update    Titasp00
057200040728   x2c                   Else
057300040728     c                   unlock    Titas30c
057400040728    2c                   EndIf
057500040728      * Aggiorno FIAR5
057600040728if  2c                   If        dAr5Trs <> newr5Trs and conta = 1
057700040728     c                             and wtrs = *On
057800040728      * la bolla ha data spedizione <  a 19/07/2004
057900040728if  3c                   If        TasDsp < datasped
058000040803     c                   Setoff                                       1112
058100040728     c     keyFiar51     Chain     Fiar531c
058200040728if  4c                   If        %Found(Fiar531c)
058300040728     c                   Movel     newr5Trs      Ar5Uni
058400040728     c                   Eval      Ar5Ft2 = 'R'
058500040728     c                   Z-Add     DataOggi      Ar5Dt2
058600040728     c   11              Update    Fiar5000
058700040728     c   12              Update    Fiar5p00
058800040728   x4c                   Else
058900040728      * Scrivo solo se la data non è a zero
059000040728if  5c                   If        new_§ar5det > *Zeros or new_§Ar5dut > *Zeros
059100040728     c                   Clear                   Fiar5000
059200040728     c                   Eval      Ar5Aas = oldAas
059300040728     c                   Eval      Ar5Lnp = oldLnp
059400040728     c                   Eval      Ar5Nrs = oldNrs
059500040728     c                   Eval      Ar5Nsp = oldNsp
059600040728     c                   Eval      Ar5Trd = kAr5Trd
059700040728     c                   Z-Add     DataOggi      Ar5Dac
059800040728     c                   Time                    w0140
059900040728     c                   Movel     w0140         Ar5Orc
060000040728     c                   Movel     newr5Trs      Ar5Uni
060100040728     c   01
060200040728     cor 02              Write     Fiar5000
060300040728     c   03              Write     Fiar5p00
060400040728e   5c                   EndIf
060500040728e   4c                   EndIf
060600040728      * la bolla ha data spedizione >= a 19/07/2004
060700040728   x3c                   Else
060800040728     c                   Clear                   Fiar5000
060900040728     c                   Eval      Ar5Aas = oldaas
061000040728     c                   Eval      Ar5Lnp = oldlnp
061100040728     c                   Eval      Ar5Nrs = oldnrs
061200040728     c                   Eval      Ar5Nsp = oldnsp
061300040728     c                   Eval      Ar5Trd = kAr5Trd
061400040728     c                   Eval      Ar5Prg = kAr5Prg
061500040728     c                   add       1             Ar5Prg
061600040728     c                   Z-Add     DataOggi      Ar5Dac
061700040728     c                   Time                    w0140
061800040728     c                   Movel     w0140         Ar5Orc
061900040728     c                   Eval      ar5uni = newr5Trs
062000040728     c   01
062100040728     cor 02              Write     Fiar5000
062200040728     c   03              Write     Fiar5p00
062300040728e   3c                   EndIf
062400040728e   2c                   EndIf
062500040728e   1c                   EndDo
062600040728
062700040728     c                   EndSr
062800040607
062900030110      *--------------------------------------------------------------------------------------------*
063000030110      * Scrivo Errore
063100030110      *--------------------------------------------------------------------------------------------*
063200030110
063300030110     c     Sr_Errore     BegSr
063400030110     c                   Write     Fidta0E
063500030110If  1c                   If        *In98 = *Off
063600030110      * Decodifico il p.o.
063700030110     c     Wpo           Chain     Azorg01l
063800030110If  2c                   If        %Found(Azorg01l)
063900030110     c                   Movel     OrgDes        DesPo
064000030110   x2c                   Else
064100030110     c                   Clear                   DesPo
064200030110    2c                   EndIf
064300030110     c                   Except    Testa
064400030110     c                   Eval      *In98 = *On
064500030110    1c                   EndIf
064600030110     c   Of              Except    Testa
064700030110      * Inverto la data
064800030110     c                   Clear                   WlbDat
064900030110     c                   Z-Add     DtaDta        G02Inv
065000030110     c                   Movel     '3'           G02Err
065100030110     c                   Call      'XSRDA8'
065200030110     c                   Parm                    WlbDat
065300030110     c                   Z-Add     G02Dat        wDtaDta
065400030110     c                   Except    Errore
065500030110
065600030110     c                   EndSr
065700021024
065800021024      *--------------------------------------------------------------------------------------------*
065900021024      * disalloca il membro di ricezione
066000021024      *--------------------------------------------------------------------------------------------*
066100021024     c     Dismbr        BegSr
066200021024
066300021024      * cancella la sovrascrittura del membro di ricezione
066400021024     c                   Clear                   Comman
066500021024     c                   Movel     Cmd(4)        Comman
066600021024     c                   Call      'QCMDEXC'                            99
066700021024     c                   Parm                    Comman
066800021024     c                   Parm                    Lenght
066900021024
067000021024      * disalloca il membro di ricezione
067100021024     c                   Clear                   Comman
067200021024     c                   Movel     Cmd(5)        Comman
067300021024     c                   Call      'QCMDEXC'                            99
067400021024     c                   Parm                    Comman
067500021024     c                   Parm                    Lenght
067600021024
067700021024     c                   EndSr
067800021024
067900021024      *--------------------------------------------------------------------------------------------*
068000021024      * operazioni iniziali
068100021024      *--------------------------------------------------------------------------------------------*
068200021024     c     *Inzsr        BegSr
068300021024
068400021024      * ricevimento parametri
068500021024
068600021024     c     *Entry        Plist
068700021024     c                   Parm                    Kpjba
068800021024
068900021024      * se il membro da ricevere non è stato dichiarato, assume il primo del file
069000021024     c                   If        MbrFil = *Blanks
069100021024     c                   Movel     '*FIRST'      MbrFil
069200021024     c                   EndIf
069300030103
069400030103     c     KeyTitas      Klist
069500030103     c                   Kfld                    DtaAas
069600030103     c                   Kfld                    DtaLnp
069700030103     c                   Kfld                    DtaNrs
069800030103     c                   Kfld                    DtaNsp
069900040607
070000040607     c     KeyTitas1     Klist
070100040607     c                   Kfld                    oldAas
070200040607     c                   Kfld                    oldLnp
070300040607     c                   Kfld                    oldNrs
070400040607     c                   Kfld                    oldNsp
070500021024
070600030103     c     KeyFiar5      Klist
070700030103     c                   Kfld                    DtaAas
070800030103     c                   Kfld                    DtaLnp
070900030103     c                   Kfld                    DtaNrs
071000030103     c                   Kfld                    DtaNsp
071100030103     c                   Kfld                    KAr5Trd
071200040604
071300040604     c     KeyFiar51     Klist
071400040728     c                   Kfld                    oldAas
071500040728     c                   Kfld                    oldLnp
071600040728     c                   Kfld                    oldNrs
071700040728     c                   Kfld                    oldNsp
071800040604     c                   Kfld                    KAr5Trd
071900040604     c                   Kfld                    KAr5Prg
072000030108
072100030108      * Data del Giorno
072200030108     c                   Movel     *date         DataOggi
072300030108     c     *iso          Movel     *date         Dateu
072400030108      * Data max per cancellazione record già ricevuti
072500030204     c                   Subdur    10:*d         Dateu
072600030108     c                   Move      Dateu         DataMax
072700030214
072800030214      * Data del Giorno ggmmaaa
072900030214     c                   Time                    w0140
073000030214     c                   Move      w0140         DataStp
073100030103
073200021024     c                   EndSr
073300021024
073400021024      *--------------------------------------------------------------------------------------------*
073500021024      * Aggiornamento file
073600021024      *--------------------------------------------------------------------------------------------*
073700030108     oPrtf198   e            Testa          2 02
073800030108     o                                           20 'BARTOLINI S.p.A.'
073900030108     o                                           60 '**  RICEZIONE DATE **'
074000030214     o                       DataStp            110 '  /  /    '
074100030108     o                                          120 'TRTR02R'
074200030108     o                                          128 'Pag.'
074300030108     o                       Page          Z    132
074400030108     o          e            Testa          2
074500030108     o                                           16 'Punto Operativo:'
074600030108     o                       Wpo                 21
074700030108     o                                           23 '-'
074800030108     o                       DesPo               44
074900030108     o                                         + 10 'MEMBRO:'
075000030108     o                       MbrFil            +  1
075100030108     o          e            Testa          2
075200030108     o                                           15 'Spedizione'
075300030108     o                                           20 'TpR'
075400030108     o                                           25 'P.o.'
075500030108     o                                           35 'Data'
075600030108     o                                           45 'Errore'
075700030110     o          e            Errore      1
075800030108     o                       DtaAas               4
075900030108     o                       DtaLnp        z   +  1
076000030108     o                       DtaNrs        z   +  1
076100030108     o                       DtaNsp        z   +  1
076200030108     o                       DtaTrd            +  2
076300030108     o                       DtaPoc        z   +  2
076400030108     o                       wDtaDta           +  2 '  /  /    '
076500030110     o                       DtaDer            +  4
076600021024
076700021024**   cmd - comandi
076800040604CHKOBJ OBJ(FIDTA01R) OBJTYPE(*FILE)     MBR(M234567890)
076900040604ALCOBJ OBJ((FIDTA01R *FILE *EXCL            M234567890))
077000040604OVRDBF FILE(FIDTA01R)                   MBR(M234567890)
077100040604DLTOVR FILE(FIDTA01R)
077200040604DLCOBJ OBJ((FIDTA01R *FILE *EXCL))
077300051109OVRDBF FILE(fnblp01l)        TOFILE(FILTRAXXX/FNBLP01L)
077400051118CHKOBJ OBJ(FILTRAxxx/FNBLP01L) OBJTYPE(*FILE)
077500040614**   cmd1 - comandi
077600040614ADDLFM FILE(FIDTA01R) MBR(M234567890) DTAMBRS((*CURRENT/FIDTA00R (M234567890)))
077700030110**   err - errori
077800030211Bolla non trovata dopo 10 giorni
077900030110Tutti i p.o. transito sono pieni
078000051109Bolla annullata: in TNBLA00F
078100051109Bolla import chiusa in partenza

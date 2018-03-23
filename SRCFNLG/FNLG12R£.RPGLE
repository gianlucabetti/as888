000100050930     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200050930
000300050930     Ffnlg9620d CF   E             WORKSTN
000400050930     F                                     INFDS(InfDspF)
000500050930     F                                     INDDS(IndDspF)
000600050930     F                                     SFILE(S01:S01RRN)
000700051005     Ftigcp52l  IF   E           K DISK
000800051010     Ffnblp01l  IF   E           K DISK
000900050930
001000050930     ***************************************************************************
001100050930     **?
001200050930     **?Definizione costanti.
001300050930     **?
001400050930     ***************************************************************************
001500050930     D F3              C                   X'33'
001600050930     D F5              C                   X'35'
001700051005     D F6              C                   X'36'
001800050930     D F12             C                   X'3C'
001900050930     D F13             C                   X'B1'
002000050930     D Enter           C                   X'F1'
002100050930     D RollUp          C                   X'F5'
002200050930     D Blink           C                   X'28'
002300050930     D ReverseImage    C                   X'21'
002400050930     D Eseguito        C                   '0'
002500050930     D Errore          C                   '1'
002600050930     D Put             C                   'P'
002700050930     D Get             C                   'G'
002800050930     D Display         C                   'D'
002900050930     D NuovaRichiesta  C                   '1'
003000050930     D Continua        C                   '2'
003100051005     D Partenza        C                   'P'
003200051005     D Disposizioni    C                   5
003300051005     D Bolla           C                   7
003400051005     D Giacenza        C                   14
003500050930
003600050930     ***************************************************************************
003700050930     **?
003800050930     **?Definizione procedure.
003900050930     **?
004000050930     ***************************************************************************
004100050930     D tibs34r         PR
004200050930     D                                     EXTPGM('TIBS34R')
004300050930     D  a
004400050930     D                                     LIKE(tibs34ds)
004500051005     D fnls05r         PR
004600051005     D                                     EXTPGM('FNLS05R')
004700051005     D  a
004800051005     D                                     LIKE(kpjba)
004900051005     D  a
005000051005     D                                     LIKE(fidna1ds)
005100051005     D fnlg31r         PR
005200051005     D                                     EXTPGM('FNLG31R')
005300051005     D  a
005400051005     D                                     LIKE(kpjba)
005500051005     D fnlg9640r       PR
005600051005     D                                     EXTPGM('FNLG9640R')
005700051005     D  a
005800051005     D                                     LIKE(kpjba)
005900051010     D  a
006000051010     D                                     LIKE(fnlg9610s)
006100050930
006200050930     ***************************************************************************
006300050930     **?
006400050930     **?Definizione strutture dati.
006500050930     **?
006600050930     ***************************************************************************
006700050930     D                SDS
006800050930     D  ProcName         *PROC
006900050930     D IndDspF         DS
007000050930     D  SflDsp                 1      1N
007100050930     D  SflClr                 2      2N
007200050930     D  SflEnd                 3      3N
007300050930     D  SflNxtChg              4      4N
007400050930     D  RIOpzione              5      5N
007500050930     D  OpzioneNonValida...
007600050930     D                         6      6N
007700050930     D  ErroreGenerico...
007800050930     D                        99     99N
007900050930     D InfDspF         DS
008000050930     D  DSP_AID              369    369
008100050930     D  SF_RRN               376    377I 0
008200050930     D dDatiUte      E DS
008300050930     D                                     INZ
008400050930     D azute00f      E DS
008500050930     D                                     INZ
008600050930     D tibs34ds      E DS
008700050930     D                                     INZ
008800051005     D fidna1ds      E DS
008900051005     D                                     INZ
009000051005     D fnlg30ds      E DS
009100051005     D                                     INZ
009200051005     D  d30poa       E
009300051005     D                                     INZ(Partenza)
009400050930     D kpjba         E DS
009500051010     D fnlg9610s     E DS
009600051010     D                                     PREFIX(flt)
009700050930     D fnlg9620s     E DS
009800050930     D                                     INZ
009900050930     D                                     PREFIX(Lst)
010000050930     D fnlg9630s     E DS
010100050930     D                                     PREFIX(Dta)
010200050930     D  dati                         30
010300050930     D                                     DIM(99)
010400050930     D                                     OVERLAY(dtaDati)
010500051005     D fnlg9640s     E DS
010600051005     D                                     PREFIX(Dtg)
010700050930     D row             DS
010800050930     D                                     INZ
010900050930     D  s01Aas
011000050930     D  s01Lnp
011100050930     D  s01Nrs
011200050930     D  s01Nsp
011300050930     D  s01Agc
011400050930     D  s01Fgc
011500050930     D  s01Ngc
011600051005     D param0          DS
011700051005     D                                     INZ
011800051005     D  pa0aas                 1      4  0
011900051005     D  pa0lnp                 5      7  0
012000051005     D  pa0nrs                 8      9  0
012100051005     D  pa0nsp                10     16  0
012200051005     D  pa0f03                17     17
012300051005     D  pa0flg                18     18
012400051005     D  pa0a9                 19     19
012500051005     D  pa0B5                 20     20
012600051005     D  pa075                 21     21
012700051005     D  pa098                 22     22
012800051005     D  pa0rsu                23     42
012900051005     D  pa0cml                43     43
013000051005     D  pa0sim                44     46  0
013100051005     D  pa0cli                47     47
013200051005     D  pa0gia                48     48
013300050930
013400050930     ***************************************************************************
013500050930     **?
013600050930     **?Definizione variabili.
013700050930     **?
013800050930     ***************************************************************************
013900050930     D NullPtr         S               *
014000050930     D S01RRN          S              5I 0
014100050930     D S01RRNFirst     S              5I 0
014200050930     D S01RRNLast      S              5I 0
014300050930     D i               S              5I 0
014400050930     D SavOpz          S
014500050930     D                                     LIKE(S01Opz)
014600050930     D SavTasto        S
014700050930     D                                     LIKE(LstTasto)
014800051005     D wrkLeave        S              1
014900051005     D                                     INZ(*OFF)
015000050930      /COPY QSYSINC/QRPGLESRC,QUSEC
015100050930     D LngTx01         S            512
015200050930     D LngTx02         S             10I 0
015300050930     D                                     INZ(%SIZE(LngTx01))
015400050930     D LngTx03         S              7
015500050930     D                                     INZ('BAR0019')
015600050930     D LngTx04         S             20
015700050930     D                                     INZ('YBARMSG   *LIBL')
015800050930
015900051005     ***************************************************************************
016000050930     **?
016100050930     **?Definizione parametri procedura.
016200050930     **?
016300051005     ***************************************************************************
016400050930     C     *ENTRY        PLIST
016500050930     C                   PARM                    kpjba
016600050930     C                   PARM                    fnlg9630s
016700051010     C                   PARM                    fnlg9610s
016800051005
016900051005     ***************************************************************************
017000051005     **?
017100051005     **?Definizione chiavi.
017200051005     **?
017300051005     ***************************************************************************
017400051005     C     k04gcp52      KLIST
017500051005     C                   KFLD                    gcpAgc
017600051005     C                   KFLD                    gcpFgc
017700051005     C                   KFLD                    gcpNgc
017800051005     C                   KFLD                    gcpFrg
017900051010     C     k04blp01      KLIST
018000051010     C                   KFLD                    blpAas
018100051010     C                   KFLD                    blpLnp
018200051010     C                   KFLD                    blpNrs
018300051010     C                   KFLD                    blpNsp
018400050930
018500050930     ***************************************************************************
018600050930     **?
018700050930     **?Definizione aree dati.
018800050930     **?
018900050930     ***************************************************************************
019000050930     C     *DTAARA       DEFINE    §azute        azute00f
019100050930     C     *DTAARA       DEFINE    §datiUte      dDatiUte
019200050930
019300051010     ***************************************************************************
019400050930     **?
019500050930     **?Main
019600050930     **?
019700051010     ***************************************************************************
019800050930     C                   EXSR      RcvPrm
019900050930
020000050930     **?Il programma può essere chiamato in due modalità:
020100050930     **?col PUT riceve una schiera contenente i RRN di YNNCT00F,
020200050930     **?carica una pagina di subfile ed emette il display;
020300050930     **?col DISPLAY emette il display.
020400050930     C                   SELECT
020500050930     C                   WHEN      LstPutGet = Put
020600050930     C                   EXSR      WrtS01
020700050930     C                   EXSR      RunPut
020800050930     C                   WHEN      LstPutGet = Display
020900050930     C                   EXSR      RunPut
021000050930     C                   ENDSL
021100050930
021200050930     C                   EXSR      Uscita
021300050930
021400051010     ***************************************************************************
021500050930     **?
021600050930     **?Operazioni iniziali.
021700050930     **?
021800051010     ***************************************************************************
021900050930     C     *INZSR        BEGSR
022000050930
022100050930     C                   IN(E)     *DTAARA
022200050930     C                   IF        %ERROR OR rsut = *BLANK
022300051007     C                   CALLP(E)  tibs34r(tibs34ds)
022400050930     C                   IN        *DTAARA
022500050930     C                   ENDIF
022600050930
022700050930     C                   ENDSR
022800050930
022900051010     ***************************************************************************
023000050930     **?
023100050930     **?Ricezione parametri.
023200050930     **?
023300051010     ***************************************************************************
023400050930     C     RcvPrm        BEGSR
023500050930
023600050930     C                   EVAL      fnlg9620s = KPJBU
023700050930     C                   CLEAR                   LstTasto
023800050930     C                   CLEAR                   LstOpzione
023900050930     C                   CLEAR                   LstEsito
024000050930
024100050930     C                   ENDSR
024200050930
024300051010     ***************************************************************************
024400050930     **?
024500050930     **?Scrittura di una pagina con i dati ricevuti.
024600050930     **?
024700051010     ***************************************************************************
024800050930     C     WrtS01        BEGSR
024900050930
025000050930     **?Inizio caricamento di una nuova lista.
025100050930     C                   IF        LstOperazione = NuovaRichiesta
025200050930     C
025300050930     C                   EVAL      SflDsp = *OFF
025400050930     C                   EVAL      SflEnd = *OFF
025500050930     C                   EVAL      SflClr = *ON
025600050930     C                   WRITE     C01
025700050930     C                   EVAL      SflClr = *OFF
025800050930     C                   CLEAR                   S01RRN
025900050930     C                   CLEAR                   S01RRNLast
026000050930     C
026100050930     C                   ENDIF
026200050930
026300050930     **?Posiziono il cursore sulla prima riga della pagina caricanda.
026400050930     C                   EVAL      S01RRN = S01RRNLast
026500050930     C
026600050930     C                   IF        S01RRN < 9999
026700050930     C                   EVAL      C01RcdNbr = S01RRN + 1
026800050930     C                   ENDIF
026900050930
027000050930     **?Carico la pagina.
027100050930     C                   DO        DtaRigheOut   i
027200050930     C
027300050930     C                   EXSR      SetS01
027400050930     C
027500050930     C                   IF        S01RRN < 9999
027600050930     C                   EVAL      S01RRN = S01RRN + 1
027700050930     C                   WRITE     S01
027800050930     C                   ELSE
027900050930     C                   EVAL      LngTx01 =
028000050930     C                             'E'' stata raggiunta la massima capienza-
028100051010     C                              dell'' elenco. Saranno visualizzate solo-
028200050930     C                              9999 righe. Per visualizzare le righe-
028300050930     C                              mancanti tornare alla finestra di filtro-
028400050930     C                              ed impostare dei parametri più precisi.'
028500050930     C                   EXSR      CalQUILNGTX
028600050930     C                   LEAVE
028700050930     C                   ENDIF
028800050930     C
028900050930     C                   ENDDO
029000050930
029100050930     **?Memorizzo il RRN dell' ultima riga scritta.
029200050930     C                   EVAL      S01RRNLast = S01RRN
029300050930
029400050930     **?Ho scritto almeno una riga, posso visualizzare il subfile.
029500050930     C                   EVAL      SflDsp = (S01RRNLast > 0)
029600050930
029700050930     **?Il subfile è completamente caricato o pieno.
029800050930     C                   IF        LstEOF = *ON OR S01RRNLast >= 9999
029900050930     C                   EVAL      SflEnd = *ON
030000050930     C                   ENDIF
030100050930
030200050930     **?Se non so quale pagina visualizzare, forzo pagina 1.
030300050930     C                   SELECT
030400050930     C                   WHEN      C01RcdNbr < 1
030500050930     C                   EVAL      C01RcdNbr = 1
030600050930     C                   WHEN      C01RcdNbr > S01RRNLast
030700050930     C                   EVAL      C01RcdNbr = S01RRNLast
030800050930     C                   ENDSL
030900050930
031000050930     C                   ENDSR
031100050930
031200050930     ***************************************************************************
031300050930     **?
031400050930     **?Impostazione campi riga subfile.
031500050930     **?
031600050930     ***************************************************************************
031700050930     C     SetS01        BEGSR
031800050930
031900050930     C                   CLEAR                   S01
032000050930     C                   EVAL      row = dati(i)
032100051005     C                   EVAL      gcpAgc = s01Agc
032200051005     C                   EVAL      gcpFgc = s01Fgc
032300051005     C                   EVAL      gcpNgc = s01Ngc
032400051005     C     k04gcp52      CHAIN     tigcp52l
032500051005     C                   IF        %FOUND
032600051005     C                   EVAL      s01scm = gcpScm
032700051005     C                   EVAL      s01fas = gcpFas
032800051010     C                   EVAL      blpAas = s01Aas
032900051010     C                   EVAL      blpLnp = s01Lnp
033000051010     C                   EVAL      blpNrs = s01Nrs
033100051010     C                   EVAL      blpNsp = s01Nsp
033200051010     C     k04blp01      CHAIN     fnblp01l
033300051010     C                   IF        %FOUND
033400051010     C                   EVAL      s01Rsm = blpRsm
033500051010     C                   ENDIF
033600051005     C                   ENDIF
033700050930
033800050930     C                   ENDSR
033900050930
034000051010     ***************************************************************************
034100050930     **?
034200050930     **?Emissione del subfile.
034300050930     **?
034400051010     ***************************************************************************
034500050930     C     RunPut        BEGSR
034600051005
034700051005     C                   WRITE     T01
034800051005     C                   WRITE     F01
034900050930
035000050930     C                   DO        *HIVAL
035100050930
035200050930     **?Se l' utente ha premuto F13 chiedo al chiamante i dati per caricare
035300050930     **?una nuova pagina del subfile, fino a quando il chiamante mi dice
035400050930     **?che i dati sono finiti; a questo punto eseguo la ripetizione
035500050930     **?dell' opzione e finalmente emetto il display.
035600050930     C                   IF        SavTasto = F13 AND NOT SflEnd
035700050930     C                   EVAL      LstTasto = RollUp
035800050930     C                   LEAVE
035900050930     C                   ELSE
036000050930     C                   IF        SavTasto = F13
036100050930     C                   EXSR      Ripetizione
036200050930     C                   ENDIF
036300050930     C                   EXFMT     C01
036400050930     C                   EVAL      LstTasto = DSP_AID
036500050930     C                   EVAL      SavTasto = DSP_AID
036600050930     C                   ENDIF
036700050930
036800050930     C                   SELECT
036900050930     **?Invio
037000050930     C                   WHEN      LstTasto = Enter AND SflDsp
037100050930     C                   EXSR      RunGetOpzione
037200051005     C                   IF        LstTasto = F5
037300051005     C                   LEAVE
037400051005     C                   ENDIF
037500050930     **?F13=Ripetizione.
037600050930     C                   WHEN      LstTasto = F13
037700050930     C                   EVAL      LstTasto = RollUp
037800050930     C                   LEAVE
037900050930     C                   OTHER
038000050930     C                   EVAL      C01RcdNbr = C01CsrRrn
038100050930     C                   LEAVE
038200050930     C                   ENDSL
038300050930
038400050930     C                   ENDDO
038500050930
038600050930     C                   ENDSR
038700050930
038800051010     ***************************************************************************
038900050930     **?
039000050930     **?Lettura subfile ed elaborazione scelte.
039100050930     **?
039200051010     ***************************************************************************
039300050930     C     RunGetOpzione BEGSR
039400050930
039500050930     C                   EVAL      OpzioneNonValida = *OFF
039600050930     C                   EVAL      ErroreGenerico = *OFF
039700050930
039800050930     C                   DO        *HIVAL
039900050930     C
040000050930     C                   READC     S01
040100050930     C
040200050930     C                   IF        %EOF
040300050930     C                   EVAL      LstEOF = *ON
040400050930     C                   LEAVE
040500050930     C                   ENDIF
040600051005     C
040700050930     C                   EVAL      C01RcdNbr = SF_RRN
040800050930     C                   EVAL      RIOpzione = *OFF
040900051005     C                   EVAL      wrkLeave = *OFF
041000051006     C                   EVAL      savOpz = s01Opz
041100051005     C
041200050930     C                   SELECT
041300051005     C
041400051005     C                   WHEN      S01Opz = Disposizioni
041500051005     C                   EXSR      visDisposiz
041600051005     C                   IF        dtgTasto = F3 OR dtgTasto = F12
041700051005     C                   EVAL      sflNxtChg = *ON
041800051005     C                   EVAL      wrkLeave = *ON
041900051005     C                   ELSE
042000051005     C                   CLEAR                   s01Opz
042100051005     C                   ENDIF
042200051005     C
042300051005     C                   WHEN      S01Opz = Bolla
042400051005     C                   EXSR      visBolla
042500051005     C                   IF        pa0F03 = *ON
042600051005     C                   EVAL      sflNxtChg = *ON
042700051005     C                   EVAL      wrkLeave = *ON
042800051005     C                   ELSE
042900051005     C                   CLEAR                   s01Opz
043000051005     C                   ENDIF
043100051005     C
043200051005     C                   WHEN      S01Opz = Giacenza
043300051005     C                   EXSR      visGiacenza
043400051007     C                   IF        d30F03 = *ON
043500051005     C                   EVAL      sflNxtChg = *ON
043600051005     C                   EVAL      wrkLeave = *ON
043700051005     C                   ELSE
043800051005     C                   CLEAR                   s01Opz
043900051005     C                   ENDIF
044000050930     **?Opzione non valida.
044100050930     C                   OTHER
044200050930     C                   IF        S01Opz <> *ZERO
044300050930     C                   EVAL      SflNxtChg = *ON
044400050930     C                   EVAL      RIOpzione = *ON
044500050930     C                   EVAL      OpzioneNonValida = *ON
044600050930     C                   EVAL      ErroreGenerico = *ON
044700050930     C                   EVAL      C01RcdNbr = SF_RRN
044800050930     C                   ENDIF
044900050930     C
045000050930     C                   ENDSL
045100051005     C
045200051005     C                   UPDATE    S01
045300051005     C                   EVAL      sflNxtChg = *OFF
045400051005     C                   EVAL      RIOpzione = *OFF
045500051005     C
045600051005     C                   IF        wrkLeave = *ON
045700051005     C                   LEAVE
045800051005     C                   ENDIF
045900050930     C
046000050930     C                   ENDDO
046100050930
046200050930     C                   ENDSR
046300050930
046400050930     ***************************************************************************
046500050930     **?
046600050930     **?Ripetizione opzione su tutte le righe caricate.
046700050930     **?
046800050930     ***************************************************************************
046900050930     C     Ripetizione   BEGSR
047000050930
047100050930     **?Reperisco l'opzione da ripetere.
047200050930     **?Se non trovo l'opzione sulla riga del cursore cerco sulla riga
047300050930     **?precedente.
047400050930     C     C01CsrRrn     CHAIN     S01
047500050930     C
047600050930     C                   IF        NOT %FOUND
047700050930     C                             OR
047800050930     C                             %FOUND AND S01Opz = *ZERO
047900050930     C                   IF        C01CsrRrn > 1
048000050930     C                   EVAL      S01RRN = C01CsrRrn - 1
048100050930     C     S01RRN        CHAIN     S01
048200050930     C                   ENDIF
048300050930     C                   ENDIF
048400050930
048500050930     **?Se ho reperito un'opzione la ripeto sulle righe caricate
048600050930     **?e senza opzione.
048700050930     C                   IF        %FOUND AND S01Opz <> *ZERO
048800050930
048900050930     C                   EVAL      SflNxtChg = *ON
049000050930     C                   UPDATE    S01
049100050930
049200050930     C                   EVAL      SavOpz = S01Opz
049300050930     C                   EVAL      S01RRNFirst = SF_RRN + 1
049400050930
049500050930     C     S01RRNFirst   DO        S01RRNLast    S01RRN
049600050930
049700050930     C     S01RRN        CHAIN     S01
049800050930
049900050930     C                   IF        %FOUND
050000050930     C                   IF        S01Opz = *ZERO
050100050930     C                   EVAL      S01Opz = SavOpz
050200050930     C                   ENDIF
050300050930     C                   UPDATE    S01
050400050930     C                   ENDIF
050500050930
050600050930     C                   ENDDO
050700050930
050800050930     C                   EVAL      SflNxtChg = *OFF
050900050930     C                   EVAL      C01RcdNbr = S01RRNLast
051000050930
051100050930     C                   ENDIF
051200050930
051300050930     C                   ENDSR
051400051005
051500051005     ***************************************************************************
051600051005     **?
051700051005     **?Interrogazione dettaglio disposizioni.
051800051005     **?
051900051005     ***************************************************************************
052000051005     C     visDisposiz   BEGSR
052100051005
052200051005     C                   RESET                   fnlg9640s
052300051005     C                   EVAL      dtgAas = s01Aas
052400051005     C                   EVAL      dtgLnp = s01Lnp
052500051005     C                   EVAL      dtgNrs = s01Nrs
052600051005     C                   EVAL      dtgNsp = s01Nsp
052700051005     C                   EVAL      dtgAgc = s01Agc
052800051005     C                   EVAL      dtgFgc = s01Fgc
052900051005     C                   EVAL      dtgNgc = s01Ngc
053000051005     C                   EVAL      kpjbu = fnlg9640s
053100051010     C                   CALLP(E)  fnlg9640r(kpjba:fnlg9610s)
053200051005     C                   EVAL      fnlg9640s = kpjbu
053300051005
053400051005     C                   IF        dtgTasto = F6
053500051005     C                   EVAL      LstTasto = F5
053600051005     C                   ENDIF
053700051005
053800051005     C                   ENDSR
053900051005
054000051005     ***************************************************************************
054100051005     **?
054200051005     **?Interrogazione bolla partenza.
054300051005     **?
054400051005     ***************************************************************************
054500051005     C     visBolla      BEGSR
054600051005
054700051005     C                   RESET                   fidna1ds
054800051005     C                   RESET                   param0
054900051005     C                   EVAL      pa0aas = s01Aas
055000051005     C                   EVAL      pa0lnp = s01Lnp
055100051005     C                   EVAL      pa0nrs = s01Nrs
055200051005     C                   EVAL      pa0nsp = s01Nsp
055300051005     C                   EVAL      kpjbu = param0
055400051007     C                   CALLP(E)  fnls05r(kpjba:fidna1ds)
055500051005     C                   EVAL      param0 = kpjbu
055600051005
055700051005     C                   ENDSR
055800051005
055900051005     ***************************************************************************
056000051005     **?
056100051005     **?Interrogazione giacenza partenza.
056200051005     **?
056300051005     ***************************************************************************
056400051005     C     visGiacenza   BEGSR
056500051005
056600051005     C                   RESET                   fnlg30ds
056700051005     C                   EVAL      d30agc = s01Agc
056800051005     C                   EVAL      d30fgc = s01Fgc
056900051005     C                   EVAL      d30ngc = s01Ngc
057000051005     C                   EVAL      d30aas = s01Aas
057100051005     C                   EVAL      d30lnp = s01Lnp
057200051005     C                   EVAL      d30nrs = s01Nrs
057300051005     C                   EVAL      d30nsp = s01Nsp
057400051005     C                   EVAL      kpjbu = fnlg30ds
057500051007     C                   CALLP(E)  fnlg31r(kpjba)
057600051005     C                   EVAL      fnlg30ds = kpjbu
057700051005
057800051005     C                   ENDSR
057900050930
058000051010     ***************************************************************************
058100050930     **?
058200050930     **?Chiamata a QUILNGTX.
058300050930     **?
058400051010     ***************************************************************************
058500050930     C     CalQUILNGTX   BEGSR
058600050930
058700050930     C                   CLEAR                   QUSEC
058800050930     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
058900050930
059000050930     C                   CALL      'QUILNGTX'
059100050930     C                   PARM                    LngTx01
059200050930     C                   PARM                    LngTx02
059300050930     C                   PARM                    LngTx03
059400050930     C                   PARM                    LngTx04
059500050930     C                   PARM                    QUSEC
059600050930
059700050930     C                   ENDSR
059800050930
059900050930     ***************************************************************************
060000050930     **?
060100050930     **?Uscita
060200050930     **?
060300050930     ***************************************************************************
060400050930     C     Uscita        BEGSR
060500050930
060600050930     C                   EVAL      KPJBU = fnlg9620s
060700050930     C                   EVAL      *INLR = (LstTasto = F3)
060800050930     C                   RETURN
060900050930
061000050930     C                   ENDSR

000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400120927      *  Questa ROUTINE oltre a controllare la correttezza fra i
000500120927      *  tipi pagamento / tipi somma e la tariffa
000600120927      * Esegue un ulteriore controllo fra tariffe a dettaglio e tariffa
000700120927      *  a giornata sullo stesso progressivo di piano tariffario.
000800120927     H*--------------------------------------------------------------*
000900100415     Ftabel00f  if   e           k disk
001000100415     Ftntbe01l  if   e           k disk
001100100415      *
001200100416     FfifpT01l  if   e           k disk
001300100416     FfifpD01l  if   e           k disk
001400100415     H*--------------------------------------------------------------*
001500100415     d DS8A          e DS
001600100415      *
001700100415     d Cod8A           S              3    DIM(100)
001800100415     d Uni8A           S             89    DIM(100)
001900100415     d CodCTD          S              4    DIM(200)
002000100415     d UniCTD          S              5    DIM(200)
002100100415      *---------------------------------------------------------------*
002200100415     c     *entry        plist
002300100415     c                   parm                    Autista           7 0
002400100415     c                   parm                    simulazione       1
002500100415     c                   parm                    progressivo       3 0
002600100415     c                   parm                    OUT_TServ         1
002700100415     c                   parm                    OUT_Tariffa       3 0
002800100415     c                   parm                    OUT_CodDetta      3
002900100415     c                   parm                    OUT_DesCodice    25
003000100415     c                   parm                    OUT_Errore       50
003100100415      *
003200100415     c                   seton                                        LR
003300100415     c                   clear                   Se_Errore         1
003400120927     c                   clear                   a_giornata        1
003500120927     c                   clear                   a_dettaglio       1
003600100415      *--------
003700100415      * Memorizza tab.8A e CTD
003800100415     c                   exsr      Carica_tabelle
003900100415      *--------
004000100415      *
004100100415     c     kfPt          Klist
004200100415     c                   Kfld                    fptPDR
004300100415     c                   Kfld                    fptSML
004400100415     c                   Kfld                    fptTSR
004500100416      *
004600100416     c     kfPd          Klist
004700100416     c                   Kfld                    FPtPDR
004800100416     c                   Kfld                    FPtSML
004900100416     c                   Kfld                    FPtTSR
005000100416     c                   Kfld                    FPtCTR
005100100416     c                   Kfld                    FPtPRG
005200100416     c                   Kfld                    FPtCTD
005300100415      *
005400100415     c     *like         define    N_8A          sav_N_8A
005500100415      *
005600100415     c     ktab          Klist
005700100415     c                   Kfld                    TBLKUT
005800100415     c                   Kfld                    TBLCOD
005900100415      *
006000100415     c     ktbe          Klist
006100100415     c                   Kfld                    TBeCOD
006200100415     c                   Kfld                    TBeKE1
006300100415      *
006400100415      *--------
006500100415      *  prova per i 3 tipi servizio a vedere se le tariffe sono conformi
006600100415     c                   do        3             Y                 3 0
006700100415      *
006800100415     c                   if         Y = 1
006900100415     c                   move      'C'           OUT_TServ
007000100415      *
007100100415     c                   elseIf     Y = 2
007200100415     c                   move      'R'           OUT_TServ
007300100415      *
007400100415     c                   elseIf     Y = 3
007500100415     c                   move      'G'           OUT_TServ
007600100415     c                   end
007700100415      *--------
007800100415     c                   z-add     Autista       fptPDR
007900100415     c                   movel     Simulazione   fptSML
008000100415     c                   movel     OUT_TServ     fptTSR
008100100415      *
008200100415     c     kfPt          setll     fifpt01l
008300100415      *
008400100415     c                   do        *hival
008500100415     c     kfPt          reade     fifpt01l                               98
008600100415     c   98              leave
008700100415      *
008800100415      *  Deve corrispondere la tariffa del periodo
008900100415     c                   if        fptprg = progressivo
009000120927     c                               and fptATB =*blank
009100120927      *  e devono essere ATTIVE
009200100415      *
009300100415     c                   z-add     1             Y8A               3 0
009400100415     c     fptCTD        lookup    cod8A(Y8A)                             21
009500100415     c                   if        *in21
009600100415     c                   movel     uni8A(Y8A)    ds8A
009700100415     c                   z-add     fptCTR        OUT_Tariffa
009800100415     c                   movel     fptCTD        OUT_CodDetta
009900100415     c                   movel     §8Ades        OUT_DesCodice
010000100415      *
010100120927      *  Tariffa a INTERA o MEZZA giornata
010200120927     c                   if        fptCTD = 'TIG' or fptCTD = 'TMG'
010300120927     c                   eval        a_giornata = 'S'
010400120927     c                   end
010500120927      *
010600120927      *  Tariffa a Dettaglio
010700120927     c                   if        fptCTD = 'BAC' or fptCTD = 'BAR'
010800120927     c                   eval        a_dettaglio = 'S'
010900120927     c                   end
011000120927      * NON POSSONO coesistere tar.DETTAGLIO CON GIORNATA INTERA O MEZZA
011100120927     c                   if          a_dettaglio = 'S'  and
011200120927     c                               a_giornata  = 'S'
011300120927     c                   eval      se_errore = 'S'
011400120928     c                   eval      OUT_errore = 'NON ammesse con Tariffe a -
011500120928     c                             DETTAGLIO'
011600120927     c                   RETURN
011700120927     c                   leave
011800120927     c                   end
011900120927      *
012000100415     c                   exsr      Check_FPT
012100100416      *
012200100416     c                   exsr      Check_FPD
012300100415      *
012400100415      *  se presente un problema di incongruenza con le tabelle
012500100415      *  esce subito e lo segnala:
012600100415     c                   if        Se_Errore <> *blank
012700100415     c                   RETURN
012800100415     c                   leave
012900100415     c                   end
013000100415      *
013100100415     c                   end
013200100415     c                   endIf
013300100415      *--------
013400100415     c                   enddo
013500100415     c                   enddo
013600100415      *--------
013700100415      *  Restituisce i campi puliti
013800100415     c                   clear                   OUT_TServ
013900100415     c                   clear                   OUT_Tariffa
014000100415     c                   clear                   OUT_CodDetta
014100100415     c                   clear                   OUT_DesCodice
014200100415     c                   clear                   OUT_Errore
014300100415      *
014400100415     c                   RETURN
014500100415      *-------------------------------------------------
014600100415      *  per ogni tariffa immagazzinata dalla 8A
014700100415      *-------------------------------------------------
014800100415     c     Cartab_CTD    begsr
014900100415      *
015000100415      * per ogni codice tariffa caricato prende enne codici su CTD
015100100415     c                   do        sav_N_8A      N_8A
015200100415      *
015300100415     c                   movel(p)  cod8A(N_8A)   key6alfa          6
015400100415     c                   move      'TSM'         key6alfa
015500100415     c                   movel(p)  key6alfa      TBeKE1
015600100415     c                   move      'CTD'         TBeCOD
015700100415      *
015800100415     c     ktbe          setll     tntbe01l
015900100415     c     ktbe          reade     tntbe01l
016000100415      *
016100100415     c                   dow       not %EoF(tntbe01l)
016200100415      *
016300100415      *  Tabella x Autista e NON COOP
016400100415     c                   if        tbeATB = *blank
016500100415     c                   add       1             N_CTD
016600100415     c                   movel(p)  Cod8A(N_8A)   CodCTD(N_CTD)
016700100415     c                   movel     tbeKE2        cod1alfa          1
016800100415     c                   move      cod1alfa      CodCTD(N_CTD)
016900100415     c                   movel(p)  TBEUNI        UniCTD(N_CTD)
017000100415     c                   end
017100100415      *
017200100415     c     ktbe          reade     tntbe01l
017300100415     c                   endDo
017400100415      *
017500100415     c                   endDo
017600100415      *
017700100415     c                   endsr
017800100415      *---------------------------------------------------------------*
017900100415      *  imposta le schiere delle tabelle 8A e CTD
018000100415      *---------------------------------------------------------------*
018100100415     c     Carica_Tabellebegsr
018200100415      *
018300100415      *  Carica le schiere della tabella 8A
018400100415     c                   z-add     1             TBLKUT
018500100415     c                   z-add     0             N_8A              3 0
018600100415     c                   z-add     0             N_CTD             3 0
018700100415     c                   move      '8A'          TBLCOD
018800100415      *
018900100415     c     ktab          setll     tabel00f
019000100415     c     ktab          reade     tabel00f
019100100415      *
019200100415     c                   dow       not %EoF(tabel00f)
019300100415      *
019400100415      *  Tabella x Autista e NON COOP
019500100415     c                   if        tblflg = *blank and %subst(tbluni:68:1)=' '
019600100415     c                   add       1             N_8A
019700100415     c                   eval      Cod8A(N_8A) = tblKEY
019800100415     c                   eval      Uni8A(N_8A) = tblUNI
019900100415     c                   end
020000100415      *
020100100415     c     ktab          reade     tabel00f
020200100415     c                   endDo
020300100415      *
020400100415     c                   z-add     N_8A          sav_N_8A
020500100415      *
020600100415     c                   exsr      Cartab_CTD
020700100415      *
020800020116     c                   endsr
020900100415      *---------------------------------------------------------------*
021000100415      *   controlla la testata letta con le regole espresse su tabelle
021100100415      *---------------------------------------------------------------*
021200100415     c     ChecK_FPT     begsr
021300100415      *
021400100416      *   Controlla il Tipo pagamento per il dettglio
021500100415     c                   if        fptTPG <> §8ATT0 and
021600100415     c                             fptTPG <> §8ATT1 and
021700100415     c                             fptTPG <> §8ATT2 and
021800100415     c                             fptTPG <> §8ATT3 and
021900100415     c                             fptTPG <> §8ATT4 and
022000100416     c                             fptTPG <> §8ATT5 and
022100100416     c                             §8afDE =  'S'    and
022200100416     c                             fptCTR <> 999
022300100416     c                   eval      OUT_errore = 'con TIPO PAGAMENTO non ammesso'
022400100416     c
022500100415     c                   eval      se_errore = 'S'
022600100415     c                   goto      fine_Ctl
022700100415     c                   end
022800100415      *
022900100416      *   Controlla il Tipo pagamento per la prestazione
023000100416     c                   if        fptTPG <> §8ATT6 and
023100100416     c                             fptTPG <> §8ATT7 and
023200100416     c                             fptTPG <> §8ATT8 and
023300100416     c                             fptTPG <> §8ATT9 and
023400100416     c                             fptTPG <> §8ATTV and
023500100416     c                             §8afPR =  'S'    and
023600100416     c                             fptCTR =  999    and
023700100416     c                             fptTSR <> 'G'
023800100416     c                   eval      OUT_errore = 'con TIPO PAGAMENTO non ammesso'
023900100416     c
024000100416     c                   eval      se_errore = 'S'
024100100416     c                   goto      fine_Ctl
024200100416     c                   end
024300100416      *
024400100416      *   Controlla il Tipo pagamento per la giornata
024500100416     c                   if        fptTPG <> §8ATb1 and
024600100416     c                             fptTPG <> §8ATb2 and
024700100416     c                             fptTPG <> §8ATb3 and
024800100416     c                             fptTPG <> §8ATb4 and
024900100416     c                             fptTPG <> §8ATb5 and
025000100416     c                             §8afGI =  'S'    and
025100100416     c                             fptCTR =  999    and
025200100416     c                             fptTSR =  'G'
025300100416     c                   eval      OUT_errore = 'con TIPO PAGAMENTO non ammesso'
025400100416     c
025500100416     c                   eval      se_errore = 'S'
025600100416     c                   goto      fine_Ctl
025700100416     c                   end
025800100416      *
025900100416      *----------------
026000100415      * Cotrolla il primo carattere della tariffa per vedere se si tratta
026100100415      *  di dettaglio/prestazione/giornata
026200100415     c     fptCTR        div       100           digit_1           1 0
026300100415     c                   mvr                     resto             3 0
026400100415     c                   movel(p)  fptCTD        codiceCTD         4
026500100415      *
026600100415      *  se era 999 (a giornata / prestazione)
026700100415     c                   if        digit_1 = 9
026800100415     c                   if        fptTSR = 'G'
026900100415     c                   move      'G'           codiceCTD
027000100415     c                   else
027100100415     c                   move      'P'           codiceCTD
027200100415     c                   end
027300100415      *  altrimenti (a dettaglio)
027400100415     c                   else
027500100415     c                   move      'D'           codiceCTD
027600100415     c                   end
027700100415      *
027800100415      * Controlla in tabella
027900100415     c                   z-add     1             ctd8A             3 0
028000100415     c     codiceCTD     lookup    codCTD(ctd8A)                          21
028100100415     c                   if        *in21
028200100415      *
028300100415      *   Controlla il Tipo SOMMA
028400100415     c     fptTSM        scan      uniCTD(ctd8A)                          21
028500100415     c                   if        not %Found
028600100416     c                   eval      OUT_errore = 'con TIPO SOMMA non ammesso'
028700100416     c
028800100415     c                   eval      se_errore = 'S'
028900100415     c                   goto      fine_Ctl
029000100415     c                   end
029100100415     c                   end
029200100415      *
029300100415     c     fine_Ctl      endsr
029400100416      *---------------------------------------------------------------*
029500100416      *   controlla la testata letta con le regole espresse su tabelle
029600100416      *---------------------------------------------------------------*
029700100416     c     ChecK_FPD     begsr
029800100416      *
029900100416      *----------------
030000100416      *  Se NON inserito il MINIMO ed il MASSIMO dove obbligatorio
030100100416     c                   if        §8AobbMIMA = 'S'
030200100416      *
030300100416     c     kfPd          setll     fifpD01l
030400100416      *
030500100416      *--------
030600100416     c                   do        *hival
030700100416     c     kfPd          reade     fifpD01l                               98
030800100416     c   98              leave
030900100416      *
031000100416      *
031100100416     c                   if        fpdITR > 0 and (fpdMIN = 0 or fpdMAX = 0)
031200100416      *
031300100416     c                   eval      OUT_errore = 'senza MINIMO e MASSIMO'
031400100416     c
031500100416     c                   eval      se_errore = 'S'
031600100416     c                   goto      fine_CtlD
031700100416      *
031800100416     c                   end
031900100416      *
032000100416     c                   enddo
032100100416      *--------
032200100416     c                   end
032300100416      *
032400100416     c     fine_CtlD     endsr

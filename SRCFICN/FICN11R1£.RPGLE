000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400100415     Ftabel00f  if   e           k disk
000500100415     Ftntbe01l  if   e           k disk
000600100415      *
000700100416     FfifpT01l  if   e           k disk
000800100416     FfifpD01l  if   e           k disk
000900100415     H*--------------------------------------------------------------*
001000100415     d DS8A          e DS
001100100415      *
001200100415     d Cod8A           S              3    DIM(100)
001300100415     d Uni8A           S             89    DIM(100)
001400100415     d CodCTD          S              4    DIM(200)
001500100415     d UniCTD          S              5    DIM(200)
001600100415      *---------------------------------------------------------------*
001700100415     c     *entry        plist
001800100415     c                   parm                    Autista           7 0
001900100415     c                   parm                    simulazione       1
002000100415     c                   parm                    progressivo       3 0
002100100415     c                   parm                    OUT_TServ         1
002200100415     c                   parm                    OUT_Tariffa       3 0
002300100415     c                   parm                    OUT_CodDetta      3
002400100415     c                   parm                    OUT_DesCodice    25
002500100415     c                   parm                    OUT_Errore       50
002600100415      *
002700100415     c                   seton                                        LR
002800100415     c                   clear                   Se_Errore         1
002900100415      *--------
003000100415      * Memorizza tab.8A e CTD
003100100415     c                   exsr      Carica_tabelle
003200100415      *--------
003300100415      *
003400100415     c     kfPt          Klist
003500100415     c                   Kfld                    fptPDR
003600100415     c                   Kfld                    fptSML
003700100415     c                   Kfld                    fptTSR
003800100416      *
003900100416     c     kfPd          Klist
004000100416     c                   Kfld                    FPtPDR
004100100416     c                   Kfld                    FPtSML
004200100416     c                   Kfld                    FPtTSR
004300100416     c                   Kfld                    FPtCTR
004400100416     c                   Kfld                    FPtPRG
004500100416     c                   Kfld                    FPtCTD
004600100415      *
004700100415     c     *like         define    N_8A          sav_N_8A
004800100415      *
004900100415     c     ktab          Klist
005000100415     c                   Kfld                    TBLKUT
005100100415     c                   Kfld                    TBLCOD
005200100415      *
005300100415     c     ktbe          Klist
005400100415     c                   Kfld                    TBeCOD
005500100415     c                   Kfld                    TBeKE1
005600100415      *
005700100415      *--------
005800100415      *  prova per i 3 tipi servizio a vedere se le tariffe sono conformi
005900100415     c                   do        3             Y                 3 0
006000100415      *
006100100415     c                   if         Y = 1
006200100415     c                   move      'C'           OUT_TServ
006300100415      *
006400100415     c                   elseIf     Y = 2
006500100415     c                   move      'R'           OUT_TServ
006600100415      *
006700100415     c                   elseIf     Y = 3
006800100415     c                   move      'G'           OUT_TServ
006900100415     c                   end
007000100415      *--------
007100100415     c                   z-add     Autista       fptPDR
007200100415     c                   movel     Simulazione   fptSML
007300100415     c                   movel     OUT_TServ     fptTSR
007400100415      *
007500100415     c     kfPt          setll     fifpt01l
007600100415      *
007700100415     c                   do        *hival
007800100415     c     kfPt          reade     fifpt01l                               98
007900100415     c   98              leave
008000100415      *
008100100415      *  Deve corrispondere la tariffa del periodo
008200100415     c                   if        fptprg = progressivo
008300100415      *
008400100415     c                   z-add     1             Y8A               3 0
008500100415     c     fptCTD        lookup    cod8A(Y8A)                             21
008600100415     c                   if        *in21
008700100415     c                   movel     uni8A(Y8A)    ds8A
008800100415     c                   z-add     fptCTR        OUT_Tariffa
008900100415     c                   movel     fptCTD        OUT_CodDetta
009000100415     c                   movel     §8Ades        OUT_DesCodice
009100100415      *
009200100415     c                   exsr      Check_FPT
009300100416      *
009400100416     c                   exsr      Check_FPD
009500100415      *
009600100415      *  se presente un problema di incongruenza con le tabelle
009700100415      *  esce subito e lo segnala:
009800100415     c                   if        Se_Errore <> *blank
009900100415     c                   RETURN
010000100415     c                   leave
010100100415     c                   end
010200100415      *
010300100415     c                   end
010400100415     c                   endIf
010500100415      *--------
010600100415     c                   enddo
010700100415     c                   enddo
010800100415      *--------
010900100415      *  Restituisce i campi puliti
011000100415     c                   clear                   OUT_TServ
011100100415     c                   clear                   OUT_Tariffa
011200100415     c                   clear                   OUT_CodDetta
011300100415     c                   clear                   OUT_DesCodice
011400100415     c                   clear                   OUT_Errore
011500100415      *
011600100415     c                   RETURN
011700100415      *-------------------------------------------------
011800100415      *  per ogni tariffa immagazzinata dalla 8A
011900100415      *-------------------------------------------------
012000100415     c     Cartab_CTD    begsr
012100100415      *
012200100415      * per ogni codice tariffa caricato prende enne codici su CTD
012300100415     c                   do        sav_N_8A      N_8A
012400100415      *
012500100415     c                   movel(p)  cod8A(N_8A)   key6alfa          6
012600100415     c                   move      'TSM'         key6alfa
012700100415     c                   movel(p)  key6alfa      TBeKE1
012800100415     c                   move      'CTD'         TBeCOD
012900100415      *
013000100415     c     ktbe          setll     tntbe01l
013100100415     c     ktbe          reade     tntbe01l
013200100415      *
013300100415     c                   dow       not %EoF(tntbe01l)
013400100415      *
013500100415      *  Tabella x Autista e NON COOP
013600100415     c                   if        tbeATB = *blank
013700100415     c                   add       1             N_CTD
013800100415     c                   movel(p)  Cod8A(N_8A)   CodCTD(N_CTD)
013900100415     c                   movel     tbeKE2        cod1alfa          1
014000100415     c                   move      cod1alfa      CodCTD(N_CTD)
014100100415     c                   movel(p)  TBEUNI        UniCTD(N_CTD)
014200100415     c                   end
014300100415      *
014400100415     c     ktbe          reade     tntbe01l
014500100415     c                   endDo
014600100415      *
014700100415     c                   endDo
014800100415      *
014900100415     c                   endsr
015000100415      *---------------------------------------------------------------*
015100100415      *  imposta le schiere delle tabelle 8A e CTD
015200100415      *---------------------------------------------------------------*
015300100415     c     Carica_Tabellebegsr
015400100415      *
015500100415      *  Carica le schiere della tabella 8A
015600100415     c                   z-add     1             TBLKUT
015700100415     c                   z-add     0             N_8A              3 0
015800100415     c                   z-add     0             N_CTD             3 0
015900100415     c                   move      '8A'          TBLCOD
016000100415      *
016100100415     c     ktab          setll     tabel00f
016200100415     c     ktab          reade     tabel00f
016300100415      *
016400100415     c                   dow       not %EoF(tabel00f)
016500100415      *
016600100415      *  Tabella x Autista e NON COOP
016700100415     c                   if        tblflg = *blank and %subst(tbluni:68:1)=' '
016800100415     c                   add       1             N_8A
016900100415     c                   eval      Cod8A(N_8A) = tblKEY
017000100415     c                   eval      Uni8A(N_8A) = tblUNI
017100100415     c                   end
017200100415      *
017300100415     c     ktab          reade     tabel00f
017400100415     c                   endDo
017500100415      *
017600100415     c                   z-add     N_8A          sav_N_8A
017700100415      *
017800100415     c                   exsr      Cartab_CTD
017900100415      *
018000020116     c                   endsr
018100100415      *---------------------------------------------------------------*
018200100415      *   controlla la testata letta con le regole espresse su tabelle
018300100415      *---------------------------------------------------------------*
018400100415     c     ChecK_FPT     begsr
018500100415      *
018600100416      *   Controlla il Tipo pagamento per il dettglio
018700100415     c                   if        fptTPG <> §8ATT0 and
018800100415     c                             fptTPG <> §8ATT1 and
018900100415     c                             fptTPG <> §8ATT2 and
019000100415     c                             fptTPG <> §8ATT3 and
019100100415     c                             fptTPG <> §8ATT4 and
019200100416     c                             fptTPG <> §8ATT5 and
019300100416     c                             §8afDE =  'S'    and
019400100416     c                             fptCTR <> 999
019500100416     c                   eval      OUT_errore = 'con TIPO PAGAMENTO non ammesso'
019600100416     c
019700100415     c                   eval      se_errore = 'S'
019800100415     c                   goto      fine_Ctl
019900100415     c                   end
020000100415      *
020100100416      *   Controlla il Tipo pagamento per la prestazione
020200100416     c                   if        fptTPG <> §8ATT6 and
020300100416     c                             fptTPG <> §8ATT7 and
020400100416     c                             fptTPG <> §8ATT8 and
020500100416     c                             fptTPG <> §8ATT9 and
020600100416     c                             fptTPG <> §8ATTV and
020700100416     c                             §8afPR =  'S'    and
020800100416     c                             fptCTR =  999    and
020900100416     c                             fptTSR <> 'G'
021000100416     c                   eval      OUT_errore = 'con TIPO PAGAMENTO non ammesso'
021100100416     c
021200100416     c                   eval      se_errore = 'S'
021300100416     c                   goto      fine_Ctl
021400100416     c                   end
021500100416      *
021600100416      *   Controlla il Tipo pagamento per la giornata
021700100416     c                   if        fptTPG <> §8ATb1 and
021800100416     c                             fptTPG <> §8ATb2 and
021900100416     c                             fptTPG <> §8ATb3 and
022000100416     c                             fptTPG <> §8ATb4 and
022100100416     c                             fptTPG <> §8ATb5 and
022200100416     c                             §8afGI =  'S'    and
022300100416     c                             fptCTR =  999    and
022400100416     c                             fptTSR =  'G'
022500100416     c                   eval      OUT_errore = 'con TIPO PAGAMENTO non ammesso'
022600100416     c
022700100416     c                   eval      se_errore = 'S'
022800100416     c                   goto      fine_Ctl
022900100416     c                   end
023000100416      *
023100100416      *----------------
023200100415      * Cotrolla il primo carattere della tariffa per vedere se si tratta
023300100415      *  di dettaglio/prestazione/giornata
023400100415     c     fptCTR        div       100           digit_1           1 0
023500100415     c                   mvr                     resto             3 0
023600100415     c                   movel(p)  fptCTD        codiceCTD         4
023700100415      *
023800100415      *  se era 999 (a giornata / prestazione)
023900100415     c                   if        digit_1 = 9
024000100415     c                   if        fptTSR = 'G'
024100100415     c                   move      'G'           codiceCTD
024200100415     c                   else
024300100415     c                   move      'P'           codiceCTD
024400100415     c                   end
024500100415      *  altrimenti (a dettaglio)
024600100415     c                   else
024700100415     c                   move      'D'           codiceCTD
024800100415     c                   end
024900100415      *
025000100415      * Controlla in tabella
025100100415     c                   z-add     1             ctd8A             3 0
025200100415     c     codiceCTD     lookup    codCTD(ctd8A)                          21
025300100415     c                   if        *in21
025400100415      *
025500100415      *   Controlla il Tipo SOMMA
025600100415     c     fptTSM        scan      uniCTD(ctd8A)                          21
025700100415     c                   if        not %Found
025800100416     c                   eval      OUT_errore = 'con TIPO SOMMA non ammesso'
025900100416     c
026000100415     c                   eval      se_errore = 'S'
026100100415     c                   goto      fine_Ctl
026200100415     c                   end
026300100415     c                   end
026400100415      *
026500100415     c     fine_Ctl      endsr
026600100416      *---------------------------------------------------------------*
026700100416      *   controlla la testata letta con le regole espresse su tabelle
026800100416      *---------------------------------------------------------------*
026900100416     c     ChecK_FPD     begsr
027000100416      *
027100100416      *----------------
027200100416      *  Se NON inserito il MINIMO ed il MASSIMO dove obbligatorio
027300100416     c                   if        §8AobbMIMA = 'S'
027400100416      *
027500100416     c     kfPd          setll     fifpD01l
027600100416      *
027700100416      *--------
027800100416     c                   do        *hival
027900100416     c     kfPd          reade     fifpD01l                               98
028000100416     c   98              leave
028100100416      *
028200100416      *
028300100416     c                   if        fpdITR > 0 and (fpdMIN = 0 or fpdMAX = 0)
028400100416      *
028500100416     c                   eval      OUT_errore = 'senza MINIMO e MASSIMO'
028600100416     c
028700100416     c                   eval      se_errore = 'S'
028800100416     c                   goto      fine_CtlD
028900100416      *
029000100416     c                   end
029100100416      *
029200100416     c                   enddo
029300100416      *--------
029400100416     c                   end
029500100416      *
029600100416     c     fine_CtlD     endsr
029700100416      *---------------------------------------------------------------*

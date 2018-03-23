000100040625     H DECEDIT('0,') DATEDIT(*DMY.)
000200040625      * ---------------------------------------------------------
000300040625     fFNDCT00F  If   E             DISK
000400040707     fFNDCT01l  If   E           K DISK    rename(fndct000:fndct01) prefix(F_)
000500040625     fFNLBL02L  IF   E           K DISK
000600040625     fTITAS30C  IF   E           K DISK
000700040630     fWRKDCK0F  O    E             DISK
000800040625     fPRTF198   O    F  132        PRINTER  oflind(*INOF)
000900040625      * ---------------------------------------------------------
001000040625     D MSG             S             78    DIM(10) ctdata perrcd(1)             messaggi errore
001100040625
001200040625     D AAS             S              4  0 DIM(20)                              anno spedizine
001300040625     D LNP             S              3  0 DIM(20)                              lnp bolle legate
001400040707     D NRS             S              2  0 DIM(20)                              n.serie
001500040625     D NSP             S              7  0 DIM(20)                              numero spedizione
001600040625     D MGS             S              4  0 DIM(20)                              Mese giorno spediz.
001700040625
001800040625     D DDCT01        E DS
001900040625
002000040625     D msgerr          s             78
002100040625     D LL              s              3  0
002200040625      * ---------------------------------------------------------
002300040625
002400040625      * Leggo FNDCT con legame bolla
002500040707    1c                   do        *hival
002600040625
002700040625     c                   read      fndct00f
002800040708      * fine file
002900040625     c                   if        %eof(fndct00f)
003000040625     c                   leave
003100040625     c                   endif
003200040708      * no annullati
003300040708     c                   if        dctatb  = 'A'
003400040708     c                   iter
003500040708     c                   endif
003600040625      * no legami
003700040625     c                   movel     dctflo        ddct01
003800040625     c                   If        §dctlega = ' '
003900040625     c                   iter
004000040625     c                   endif
004100040625
004200040625     c                   clear                   msgerr
004300040625     c                   clear                   LL
004400040701      * Verifico se la bolla legata alla CA in filiale è diversa prendo quella come figlia
004500040701     c     Kca           chain     fndct01l
004600040707    2c                   if        %found(fndct01l) and
004700040701     c                             (f_dctaas <> dctaas or
004800040701     c                              f_dctlnp <> dctlnp or
004900040701     c                              f_dctnrs <> dctnrs or
005000040701     c                              f_dctnsp <> dctnsp)
005100040702      * aggiorno il file work con la ca numero spedizione e flag di figlia trovata in filiale
005200040702     c                   clear                   wrkdck00
005300040702     c                   eval      wrkaas = f_dctaas
005400040702     c                   eval      wrklnp = f_dctlnp
005500040702     c                   eval      wrknrs = f_dctnrs
005600040702     c                   eval      wrknsp = f_dctnsp
005700040708      * valoriozzo anche la spedizione mamma per aggiornare la CA in FILTRAGRU come in gaitragru
005800040708     c                   eval      wrkaam = dctaas
005900040708     c                   eval      wrklnm = dctlnp
006000040708     c                   eval      wrknrm = dctnrs
006100040708     c                   eval      wrknsm = dctnsp
006200040712     c                   eval      wrklam = dctlna
006300040702      * flag = 0 per trovato
006400040702      * MSG "trovato in filiale"
006500040709
006600040709      * verifico se esiste in TITAS
006700040709     c     ktas          chain     titas30c
006800040709     c                   if        %found(titas30c)
006900040702     c                   eval      wrkflg = '0'
007000040709     c                   else
007100040709     c                   eval      wrkflg = '4'
007200040709     c                   endif
007300040702     c                   eval      wrkmsg = msg(1)
007400040702     c                   exsr      SR_Work
007500040705      * leggo la CA successiva
007600040705     c                   iter
007700040705
007800040707    2c                   endif
007900040625      *
008000040625      * Verifico se ha più di una figlia
008100040625     c     kbol          chain     fnlbl02l
008200040625      * se non trovato in FNLBL stampo errore
008300040707    2c                   If        not %found(fnlbl02l)
008400040705      * aggiorno il file work con la ca  e flag errore
008500040705     c                   clear                   wrkdck00
008600040705      * flag = 1 per non trovato lbl
008700040705      * MSG "non trovato in filiale"
008800040705     c                   eval      wrkflg = '1'
008900040705     c                   eval      wrkmsg = msg(2)
009000040705     c                   exsr      SR_Work
009100040707      * leggo la ca successiva
009200040625     c                   iter
009300040707    2c                   endif
009400040625
009500040625      *
009600040625      * Figlie -
009700040707    2c                   do        *hival
009800040625     c     klbl_f        chain     fnlbl02l
009900040625      * se finita la lettura e non ho ancora caricato nessuna schiera delle bolle legate
010000040709      * significa che o ho una sola figlia e quindi posso scrivere FNDCK ed uscire oppure
010100040709      * potrebbe non esserci nessuna figlia su titas
010200040707    3c                   if        not %found(fnlbl02l)
010300040625      * indice schiera figlie
010400040707    4c                   if        ll = 0
010500040706      * aggiorno il file work con la ca numero spedizione e flag di figlia trovata in lbl unica
010600040706     c                   clear                   wrkdck00
010700040709      * SE LNP =  LNA NON CONSIDERO
010800040709      * se data spedizione maggiore di data CA non copnsidero
010900040709      * se lna bolla diversa da lna ca
011000040709    5c                   if        lbllpn <> lbllan and
011100040709     c                             (lblaan < dctaac or (lblaan = dctaac and
011200040709     c                             lblmgn < dctmgc)) and
011300040709     c                             lbllan = dctlna
011400040706     c                   eval      wrkaas = lblaan
011500040707     c                   eval      wrklnp = lbllpn
011600040706     c                   eval      wrknrs = lblnrn
011700040706     c                   eval      wrknsp = lblnsn
011800040706      * flag = 2 per trovato
011900040706      * MSG "trovato in FNLBL unica figlia"
012000040709      * verifico se esiste in TITAS
012100040709     c     ktas          chain     titas30c
012200040709     c                   if        %found(titas30c)
012300040709     c                   eval      wrkflg = '2'
012400040709     c                   eval      wrkmsg = msg(3)
012500040709     c                   else
012600040709     c                   eval      wrkflg = '4'
012700040709     c                   clear                   wrkaas
012800040709     c                   clear                   wrklnp
012900040709     c                   clear                   wrknrs
013000040709     c                   clear                   wrknsp
013100040709     c                   eval      wrkmsg = msg(5)
013200040709     c                   endif
013300040709      *
013400040709     c                   else
013500040709     c                   eval      wrkflg = '4'
013600040709     c                   eval      wrkmsg = msg(5)
013700040709     c                   endif
013800040709      *
013900040706     c                   exsr      SR_Work
014000040707      * leggo la CA successiva
014100040707     c                   leave
014200040707      * indice schiera maggiore di zero
014300040707  x4 c                   else
014400040625      * carico l'ultima figlia
014500040706      * SE LNP =  LNA NON CONSIDERO
014600040706      * se data spedizione maggiore di data CA non copnsidero
014700040706      * se lna bolla diversa da lna ca
014800040707    5c                   if        lbllpn <> lbllan and
014900040706     c                             (lblaan < dctaac or (lblaan = dctaac and
015000040707     c                             lblmgn < dctmgc)) and
015100040706     c                             lbllan = dctlna
015200040709      * AGGANCIO TITAS per verificare se bolla di reso che scarto oppure se non è presente
015300040709      * in titas scarto
015400040707     c     klbl_f        chain     titas30c
015500040709    6c                   if         %found(titas30c) and tasfbr = ' '
015600040709     c                   add       1             ll
015700040625     c                   eval      aas(ll) = lblaan
015800040630     c                   eval      lnp(ll) = lbllpn
015900040625     c                   eval      nrs(ll) = lblnrn
016000040625     c                   eval      nsp(ll) = lblnsn
016100040625     c                   eval      mgs(ll) = lblmgn
016200040707    6c                   endif
016300040707    5c                   endif
016400040707    4c                   endif
016500040625
016600040625     c                   leave
016700040625      *
016800040707    3c                   endif
016900040707
017000040625      * se ha + di una figlia carico la schiera
017100040706      * SE LNP =  LNA NON CONSIDERO
017200040706      * se data spedizione maggiore di data CA non copnsidero
017300040706      * se lna bolla diversa da lna ca
017400040707    3c                   if        lbllpp <> lbllap and
017500040706     c                             (lblaap < dctaac or (lblaap = dctaac and
017600040707     c                             lblmgp < dctmgc)) and
017700040706     c                             lbllap = dctlna
017800040709      * AGGANCIO TITAS per verificare se esiste e se bolla di reso la scarto
017900040707     c     klbl_fp       chain     titas30c
018000040709    4c                   if        (%found(titas30c) and tasfbr = ' ')
018100040625     c                   add       1             ll
018200040625     c                   eval      aas(ll) = lblaap
018300040707     c                   eval      lnp(ll) = lbllpp
018400040625     c                   eval      nrs(ll) = lblnrp
018500040625     c                   eval      nsp(ll) = lblnsp
018600040625     c                   eval      mgs(ll) = lblmgp
018700040625
018800040707    4c                   endif
018900040707    3c                   endif
019000040706
019100040707    2c                   enddo
019200040625      *
019300040625      * se indice d schiera maggiore di zero vuol dire che devo cercare la figlia
019400040707      * da scrivere in FNDCK  e quindi scrivo il file di lavoro
019500040625      * altrimenti leggo il successivo
019600040625      *
019700040625     c                   if        ll = 0
019800040625     c                   iter
019900040625     c                   endif
020000040707      *
020100040707      * aggiorno il file work con la ca  e flag di trovate + figlie
020200040707     c                   clear                   wrkdck00
020300040707     c                   if        ll = 1
020400040707     c                   eval      wrkaas = aas(1)
020500040707     c                   eval      wrklnp = lnp(1)
020600040707     c                   eval      wrknrs = nrs(1)
020700040707     c                   eval      wrknsp = nsp(1)
020800040707     c                   endif
020900040707      * flag = 3 per non trovato
021000040707      * MSG "trovato in FNLBL figlia xxx "
021100040707     c                   eval      wrkflg = '3'
021200040707     c                   eval      wrkmsg = msg(4)
021300040707     c                   move(p)   ll            numll             3
021400040707     c                   eval      %subst(wrkmsg:30:3) = numll
021500040707     c                   exsr      SR_Work
021600040707
021700040707    1c                   enddo
021800040707      *
021900040707     c                   seton                                        lr
022000040702      *****************************************************************
022100040702      *  ROUTINE SCRITTURA FILE DI LAVORO
022200040702      *****************************************************************
022300040702     C     SR_WORK       BEGSR
022400040702
022500040702     c                   eval      wrkaac = dctaac
022600040702     c                   eval      wrkfil = dctfil
022700040702     c                   eval      wrknca = dctnca
022800040702      *
022900040702     c                   write     wrkdck00
023000040702      *
023100040702     c                   endsr
023200980722      *****************************************************************
023300980623      *  ROUTINE INIZIALE
023400980722      *****************************************************************
023500951009     C     *INZSR        BEGSR
023600990412
023700980625      *
023800040625     c     kbol          klist
023900990409     c                   kfld                    dctaas
024000990409     c                   kfld                    dctlnp
024100990409     c                   kfld                    dctnrs
024200990409     c                   kfld                    dctnsp
024300981023      *
024400040708     c     ktas          klist
024500040708     c                   kfld                    wrkaas
024600040708     c                   kfld                    wrklnp
024700040708     c                   kfld                    wrknrs
024800040708     c                   kfld                    wrknsp
024900040708      *
025000980721      *
025100040630     c     kca           klist
025200040630     c                   kfld                    dctaac
025300040630     c                   kfld                    dctfil
025400040630     c                   kfld                    dctnca
025500040630      *
025600040630      *
025700981228     c     klbl_F        klist
025800981228     c                   kfld                    LBLaan
025900981228     c                   kfld                    LBLlpn
026000981228     c                   kfld                    LBLnrn
026100981228     c                   kfld                    LBLnsn
026200040706     c     klbl_Fp       klist
026300040706     c                   kfld                    LBLaap
026400040706     c                   kfld                    LBLlpp
026500040706     c                   kfld                    LBLnrp
026600040706     c                   kfld                    LBLnsp
026700981224      *
026800951009     C                   ENDSR
026900980611      *------------------------------------------------------------
027000990412     OPRTF198   E            TESTA             2
027100040625     o                                           90 'C.A. PER LE QUALI NON SI R-
027200040625     o                                              IESCE A CREARE FNDCK'
027300990412     o          E            DETTA       2
027400990412     O                       dctaac               4
027500990412     O                       dctfil           +   1
027600990412     O                       dctnca        z  +   1
027700040625     O                       msgerr           +   2
027800040625** MSG
027900040702Bolla trovata in FNDCT00F di filiale
028000040625Bolla con legami non presente in FNLBL02L
028100040706Bolla trovata in FNLBL ed unica figlia
028200040707Trovate in FNLBL più figlie
028300040709Non trovata nessuna figlia esistente su TITAS

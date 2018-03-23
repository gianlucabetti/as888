000100001102      *--------------------------------------------------------------------------------------------*
000200001102      * Creazione FNVAG da fasi C.A.
000300001102      *--------------------------------------------------------------------------------------------*
000400020912      * Elaboro fasi flaggate che siano create da un P.O. presente nel
000500020912      *   Sistema Informativo
000600001102      * La C.A. non deve essere chiusa
000700001102      *--------------------------------------------------------------------------------------------*
000800001102
000900001102      ****************************************************************
001000001102      *  FASI C.A. FISSATE A PROGRAMMA  (ricercare con 0 davanti)
001100001102      ****************************************************************
001200001102      *    900 = Chiusura C.A.                                (WFcch)
001300001102      ****************************************************************
001400001102
001500001102     H DECEDIT('0,') DATEDIT(*DMY.)
001600001102
001700001102      ****************************************************************
001800980929      *  RIEPILOGO INDICATORI
001900980929      ***************************************************************
002000020912      * 10   - comodo
002100980929      * 30   - CHAIN FNARB/FNBLP
002200001102      * 46   - presenza in 3K
002300001102      * 90   - Trovate tabelle decodifica
002400001102      * 91   - Mancata allocazione file
002500001102      ****************************************************************
002600001102
002700001102     fFNDCF02L  UF   E           K DISK
002800001102     fFNBLP01L  IF   E           K DISK
002900001106     fFNDCT01L  IF   E           K DISK
003000050610     ftivgd00f  O  A E             DISK
003100001102     fTABEL00F  IF   E           K DISK
003200001102     fTNTBE01L  IF   E           K DISK
003300010528     fFNEVB04L  IF   E           K DISK
003400001102
003500020912      *----------------
003600020912      * Schiere
003700020912      *----------------
003800020912     d SC_Ktad         S                   like(TbeKe1)   DIM(30)
003900020912     d SC_Ragtad       S                   like(§TADRAGR) DIM(30)
004000021004     d SC_K3k          S                   like(TblKey)   DIM(990)
004100050610     d sc_uni3K        S                   like(Tbluni)   DIM(990)
004200021004     d SC_cks3K        S              7                   DIM(990)
004300020912     d SC_K2a          S                   like(TblKey)   DIM(200)
004400020912     d SC_Des2a        S                   like(§2ADES)   DIM(200)
004500020912      * schiera dei P.O. presenti sull'AS/400
004600020912     d sc_POA          s              3  0 dim(250) inz
004700020912      * schiera dei P.O. presenti nel Sistema Informativo
004800020912     d sc_POS          s              3  0 dim(250) inz
004900101007     d s2A             s              3    DIM(30)                              Eventi Riserva
005000001102      *----------------
005100020912      * Strutture dati definite esternamente
005200020912      *----------------
005300020912     D KPJBA         E DS                                                       *architettura
005400020912     D UT§DSE0F      E DS
005500020912     D cncr80        E DS
005600020912     D DTAD          E DS
005700020912     D DS2A          E DS
005800020912     D DS3K          E DS
005900050610     d fnvagds       e ds
006000020912     d TIBS56DS      e ds
006100020912     d  ski                    7    756  0 dim(250)
006200020912      *----------------
006300020912      * Strutture dati definite internamente
006400020912      *----------------
006500020912     D DSmembro        ds             8    inz
006600020912     D  MbrM                   1      1    inz('M')
006700020912     D  MbrCKS                 2      8    inz(*zeros)
006800001102      *----------------
006900020912      * Variabili
007000020912      *----------------
007100001102     D WFcch           S                   LIKE(DCTfca)   INZ(900)
007200001102      *----------------
007300020912     d WtbeCod         S                   like(TbeCod)  inz('TAD')
007400020912     d Wtbeke1         S                   like(TbeKe1)
007500020912     d Wtblkey         S                   like(TblKey)
007600001102     D KUTE            S                   like(TBLKUt)  inz(1)
007700001102     D KCOD            S                   like(TBLCOD)
007800001102     d XX              s              3  0
007900001102     d YY              s              3  0
008000020912     d $POA            s              1    inz(*off)
008100020912     d $POS            s              1    inz(*off)
008200050610     d dataiso         s               d   datfmt(*iso)
008300020912      *----------------
008400020912      * Costanti
008500001102      *--------------------------------------------------------------------------------------------*
008600001102
008700001102     c     *loval        setll     Fndcf000
008800001102     c                   read      Fndcf000
008900001102      *
009000001102     c                   DOW       not %EOF(fndcf02l)
009100001102      *
009200020912      * Controllo che il P.O. che ha creato la fase sia nello stesso
009300020912      *   AS/400
009400020912     c                   eval      *in10 = *off
009500020912     c     DcfFev        lookup    sc_POA                                 10
009600020912     c                   movel     *in10         $POA
009700020912      *
009800020912      * Controllo che il P.O. che ha creato la fase sia nello stesso
009900020912      *   Sistema Informativo
010000020912     c                   eval      *in10 = *off
010100020912     c     DcfFev        lookup    sc_POS                                 10
010200020912     c                   movel     *in10         $POS
010300001102      *
010400001102      * Aggancio la C.A.
010500001102     c     Keynca        chain     FNDCT000
010600001102      *
010700021004     C                   clear                   BLPccm
010800001102      * Aggancio la spedizione in partenza
010900001102     c     Kbol01        chain     FNBLP000
011000001102      *
011100001102      * Cerco il codice nella 3K
011200001102     c                   If        BLPccm > *zeros
011300001102     c                   movel(p)  BLPCCM        WTblKey
011400001102     c                   Else
011500001102     c                   movel(p)  BLPKSC        WTblKey
011600001102     c                   EndIf
011700001102      *
011800001102     c                   eval      xx = 1
011900001102     c     WTBLKEY       lookup    SC_K3K(xx)                             46
012000050610     c   46              movel     sc_uni3k(xx)  ds3k
012100050610
012200021004     C                   SELECT
012300021004      *  - FLEGGO x fase creata da P.O. non presente nell'AS400
012400021004     C                   WHEN      $POA = *off
012500021004     c                   exsr      SBR_flag
012600021004      *  - NON ELABORO x fase creata da P.O. non presente nel S.I.
012700021004     c                   WHEN      $POS = *Off
012800021004      *  - FLEGGO x C.A. non trovata
012900021004     C                   WHEN      NOT %found(FNDCT01L)
013000021004     c                   exsr      SBR_flag
013100021004      *  - FLEGGO x bolla non trovata
013200021004     C                   WHEN      NOT %found(FNBLP01L)
013300021004     c                   exsr      SBR_flag
013400021004      *  - FLEGGO x C.A. chiusa
013500021004     C                   WHEN      DCTFCA = WFCCH
013600021004     c                   exsr      SBR_flag
013700021004      *  - FLEGGO x Cliente non presente in 3K
013800021004     C                   WHEN      *IN46 = *off
013900021004     c                   exsr      SBR_flag
014000021004      *  - ELABORO
014100021004     C                   OTHER
014200050610      *  scrivo rcd di FNVAG
014300050610     c                   exsr      SBR_WRTVAG
014400021004     c                   ENDSL
014500021004      *
014600001102     c                   read      Fndcf000
014700001102     c                   ENDDO
014800001106      *
014900001102     C                   eval       *inLR = *on
015000001102      *--------------------------------------------------------------------------------------------*
015100021004      * Aggiorno FNDCF
015200001102      *--------------------------------------------------------------------------------------------*
015300021004     C     SBR_flag      BEGSR
015400001102      *
015500021004     c                   eval      %subst(dcflet:1:1) = 'T'
015600021004     c                   update    FNDCF000
015700021004      *
015800021004     c                   ENDSR
015900021004      *--------------------------------------------------------------------------------------------*
016000021004      * Scrivo rcd di FNVAG
016100021004      *--------------------------------------------------------------------------------------------*
016200021004     C     SBR_WRTVAG    BEGSR
016300021004      *
016400050610     c                   clear                   FNVAGds
016500050610     c                   clear                   tivgd000
016600001102      *
016700001102     c                   eval      YY = 1
016800001102     c                   movel     DCTTAD        Wtbeke1
016900001102     c     Wtbeke1       lookup    SC_KTAD(yy)                            99
017000001102      *
017100001102     c                   IF        *IN99 = *on
017200001102     c                   eval      wTblKey = 'W' + SC_ragtad(yy)
017300001102     c                   If        dctnev > *zeros
017400001102     c                   eval      %subst(wTblKey:3:1) = 'E'
017500001102     c                   Endif
017600001102     c                   eval      YY = 1
017700001102     c     Wtblkey       lookup    SC_K2A(yy)                             99
017800001102      *
017900001102     c                   IF        *IN99 = *on
018000001102     c                   eval      VagAas = BlpAas
018100001102     c                   eval      VagLnp = BlpLnp
018200001102     c                   eval      VagNrs = BlpNrs
018300001102     c                   eval      VagNsp = BlpNsp
018400001102     c                   eval      VagLna = BlpLna
018500001102     c                   eval      VagCmc = WtblKey
018600001102     c                   eval      VagRmn = BlpRmn
018700001102     c                   eval      VagRma = BlpRma
018800001102     c                   eval      VagAgc = DctAac
018900001102     c                   eval      VagMgc = DctMgc
019000001106     c                   eval      VagDmc = SC_Des2a(yy)
019100001106     c                   eval      VagNot = 'nr. colli coinvolti ' +
019200001106     c                                       %editc(DctNcn:'2')
019300001102     c                   If        BLPccm > *zeros
019400001102     c                   z-add     BLPCCM        VagScm
019500001102     c                   Else
019600001102     c                   z-add     BLPKSC        VagScm
019700001102     c                   EndIf
019800001102      *
019900101007     c                   eval      xx = 1
020000101007     c                   FOR       xx by 1 to %elem(s2A)
020100101007     c                   IF        s2A(xx) <> *blanks
020200101007     c                   eval      EVBcev = s2A(xx)
020300101007     c**!!!              movel     'R  '         evbcev
020400010528     c     Kfnevb        chain     fnevb000                           31
020500001102      *
020600010528     C     *in31         Ifeq      *off
020700001102     c                   eval      VagDmm = EvbDev
020800101007     c                   leave
020900001102     c                   Endif
021000101007     c                   ENDIF
021100101007     c                   ENDFOR
021200001102      *
021300080404     c                   eval      vgddta = %subst(fnvagds:1:%size(fnvagds))
021400050610     c                   move      'VG'          vgdtip
021500050719     c                   movel     *all'0'       vgdksu
021600050610     c                   move      §3kcks        vgdksu
021700050610     c                   move      §3kcsd        vgdtsc
021800050610     c                   eval      vgdpgm = 'FIDN98R'
021900050610     c                   move      *date         dataiso
022000050610     c                   move      dataiso       vgddat
022100050610     C                   WRITE     tivgd000
022200001106      * Aggiorno fndcf00f
022300021004     c                   EXSR      SBR_flag
022400001102      *
022500001102     c                   ENDIF
022600001102     c                   ENDIF
022700001102      *
022800001102     c                   ENDSR
022900001102      *--------------------------------------------------------------------------------------------*
023000001102      * *inzsr - operazioni iniziali
023100001102      *--------------------------------------------------------------------------------------------*
023200980828     C     *inzsr        BEGSR
023300001102      *
023400001102      * ricevimento parametri
023500980828     C     *entry        PLIST
023600980828     C                   PARM                    kpjba
023700001102      *
023800980915     C                   CALL      'X§PARUT'
023900001102     C                   PARM                    UT§DSE0F
024000980915     C                   MOVEL     REC80         CNCR80
024100020912      ***
024200020912      * Carico la schiera dei P.O. presenti sull'AS/400
024300020912      ***
024400020912     c                   clear                   TIBS56DS
024500020912     c                   eval      I56ppo = SIMfel
024600020912     c                   eval      I56mod = 'POA'
024700020912     c                   call      'TIBS56R'
024800020912     c                   parm                    TIBS56DS
024900020912     c                   if        O56err <> *blanks
025000020912     c                   clear                   SKI
025100020912     c                   endif
025200020912     c                   z-add     SKI           sc_POA
025300020912      ***
025400020912      * Carico la schiera dei P.O. presenti nel S.I.
025500020912      ***
025600020912     c                   clear                   TIBS56DS
025700020912     c                   eval      I56ppo = SIMfel
025800020912     c                   eval      I56mod = 'POS'
025900020912     c                   call      'TIBS56R'
026000020912     c                   parm                    TIBS56DS
026100020912     c                   if        O56err <> *blanks
026200020912     c                   clear                   SKI
026300020912     c                   endif
026400020912     c                   z-add     SKI           sc_POS
026500001102      ***
026600001102      * CARICO TABELLA 2A
026700001102      ***
026800001102     C                   clear                   xx
026900101007     c                   clear                   yy
027000001102     C                   eval      KCOD = '2A'
027100001102     C     KTAB          SETLL     TABEL
027200001102     C     KTAB          READE     TABEL
027300001102      *
027400001102     C                   dow       NOT %eof(tabel00f)
027500001102     C                   if        TBLFLG = *blanks
027600001102     C                   ADD       1             xx
027700001102     C                   MOVEL     TBLUNI        DS2A
027800001102     C                   MOVEA     TBLKEY        sc_K2a(xx)                     SECONDARIA
027900001102     C                   MOVEA     §2ADES        sc_Des2a(xx)
028000101007     c                   IF        §2atip = 'R'
028100101007     c                   eval      yy = yy + 1
028200101007     c                   eval      s2A(yy) = TBLkey
028300101007     c                   ENDIF
028400001102     C                   endif
028500001102     C     KTAB          READE     TABEL
028600001102     C                   enddo
028700001102      ***
028800001102      * CARICO TABELLA 3K solo abilitati alla ricezione C.A.
028900001102      ***
029000001102     C                   clear                   xx
029100001102     C                   eval      KCOD = '3K'
029200001102     C     KTAB          SETLL     TABEL
029300001102     C     KTAB          READE     TABEL
029400001102      *
029500001102     C                   dow       NOT %eof(tabel00f)
029600001102     C                   MOVEL     TBLUNI        DS3K
029700001102     C                   if        TBLFLG = *blanks  and  §3kcq7 = 'S'
029800001102     C                   ADD       1             xx
029900001102     C                   MOVEA     TBLKEY        sc_K3k(xx)                     SECONDARIA
030000001106     C                   MOVEL     §3kcks        sc_Cks3K(xx)
030100050610     C                   MOVEL     tbluni        sc_uni3K(xx)
030200001102     C                   endif
030300001102     C     KTAB          READE     TABEL
030400001102     C                   enddo
030500001102      ***
030600001102      * CARICO TABELLA TAD
030700001102      ***
030800001102     c                   clear                   xx
030900001102     c     Wtbecod       setll     tntbe000
031000001102     c     Wtbecod       reade     tntbe000
031100001102      *
031200001102     c                   DOW       NOT %EOF(tntbe01l)
031300001102      * Se il S.I. è indicato deve essere uguale al mio, l'anomalia non deve essere per gli Eventi
031400001102     c                   IF        (TBEsif = knsif  or  TBEsif = *blanks)
031500001102      *                            and TBEatb = *BLANKS
031600001102     c                   add       1             xx
031700001102     c                   movel     TBEuni        DTAD
031800001102     c                   movea     TBEke1        SC_Ktad(XX)
031900001102     c                   movea     §TadRagr      SC_RagTad(XX)
032000001102     c                   endif
032100001102     c     Wtbecod       reade     tntbe000                               31
032200001102     c                   enddo
032300001102
032400001102      *
032500001102     C     KTAB          KLIST
032600001102     C                   KFLD                    KUTE
032700001102     C                   KFLD                    KCOD
032800001102      *
032900981021     C     keynca        KLIST                                                  *fndcd00f
033000001102     C                   KFLD                    dcfaac                          -anno C.A.
033100001102     C                   KFLD                    dcffil                          -P.O. C.A.
033200001102     C                   KFLD                    dcfnca                          -numero C.A.
033300001102      *
033400990203     c     kbol01        klist
033500990203     c                   kfld                    dctaas
033600990203     c                   kfld                    dctlnp
033700990203     c                   kfld                    dctnrs
033800990203     c                   kfld                    dctnsp
033900010528      *
034000010528     c     kfnevb        klist
034100010528     c                   kfld                    dctaas
034200010528     c                   kfld                    dctlnp
034300010528     c                   kfld                    dctnrs
034400010528     c                   kfld                    dctnsp
034500010528     c                   kfld                    evbcev
034600010528      *
034700980828     C                   ENDSR

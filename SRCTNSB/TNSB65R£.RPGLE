000100040119      *===============================================================*
000200050526      * TNSB65R    * Invio E-mail ad assicurazione per spedizioni     *
000300050526      *              con mandato automatico
000400040119      *===============================================================*
000500040119
000600040119     h decedit('0,') datedit(*dmy/)
000700040119     h option(*nodebugio)
000800040119
000900040119      *---------------------------------------------------------------*
001000040119
001100050526     fTita436l  UF   e           k disk
001200040119      *
001300050526     fTITAS30C  if   e           k disk
001400050527     f                                     ignore(titasp00)
001500050527     FTITAA30C  IF   E           K DISK
001600050527     f                                     ignore(titaap00)
001700050527     FTITA430C  IF   E           K DISK    prefix(A_)
001800050527     f                                     rename(tita4000:ta400)
001900050527     f                                     ignore(tita4p00)
002000050314      *
002100050314     fTNTBE01l  if   e           k disk
002200050317      *
002300050314     fTABEL00F  if   e           k disk
002400050601     Ffiar531c  if   e           k disk
002500050527     Ftnsb65p   O    E             PRINTER  oflind(*in11) USROPN
002600040119
002700040119      *---------------------------------------------------------------*
002800040331      *
002900040331      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
003000040331      *
003100050526     d ctmf8           s              2    dim(50) inz
003200050527     D CMM             S             70    DIM(1) CTDATA PERRCD(1)
003300050314      *
003400040119      *   S T R U T T U R E   D A T I   - - - - - - - - - - - - - - - *
003500040119      *
003600050330
003700050527     d Azuteds       e ds                  Extname(Azute00f)
003800050527     d dDatiute      e ds
003900050527     d Tibs34ds      e ds                  Inz
004000050526     d ds1b          e ds                  inz
004100050526     d dta4c         e ds                  inz
004200050527     d dta4a         e ds                  inz
004300050601     d dAr5Ban       e ds
004400050601     d dAr5Bnb       e ds
004500050526     d kpjba         e ds
004600050527     d dtas          e ds                  inz
004700050527     d  §tasflo      e                     extfld(tasflo)
004800060926     d dtas01        e ds
004900050527     d dtasv         e ds                  inz
005000050527     d dmradan       e ds
005100050527     d trtcm1ds      e ds
005200050527     d TIBS69DS      E DS                  INZ
005300050527     d DS_cnaco      E DS                  extname(CNACO00F)
005400050527     d DS_cnind      E DS                  extname(CNIND00F)
005500050527     d DS_cnclp      E DS                  extname(CNCLP00F)
005600050527     d DS_fncls      E DS                  extname(FNCLS00F)
005700040331      *
005800050526
005900050526     D PARAMT          DS
006000050526     D  PARCA                256    256
006100050526     D***
006200050526     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
006300050526     D***
006400050526     D WLBDAT          DS                  INZ
006500050526     D  G02DAT                 1      8  0
006600050526     D  G02INV                 9     16  0
006700050526     D  G02ERR                17     17
006800050526     D  G02TGI                18     22  0
006900040331      *
007000040331      *
007100040331      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
007200050526      *
007300040331      * - Indici di schiera
007400040331     d xx              s              3  0 inz
007500040331      * - Campi di comodo
007600050526     d kcod            s                   like(tblcod)
007700050527     d wprimav         s              1
007800050530     D svcom           s             80
007900050530     D lungm           S             15  5 inz(80)
008000050530     D lung            S              2  0 inz(0)
008100050530     d WtasRMN         s             15                 inz(*zeros)
008200050601     d kAr5Trd         s                   Like(Ar5Trd)
008300050527     D*
008400050527     D* DEFINIZIONE COSTANTI
008500050527     D*
008600050527     D Cosobj          C                   CONST('Elenco spedizioni a-
008700050527     D                                     ssicurate')
008800040119
008900040119      *---------------------------------------------------------------*
009000040119
009100040119      *
009200040119      *   K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - - *
009300040119      *
009400050526     c     Ktas          klist
009500050526     c                   kfld                    TA4aas
009600050526     c                   kfld                    TA4lnp
009700050526     c                   kfld                    TA4nrs
009800050526     c                   kfld                    TA4nsp
009900050527     c*
010000050527     c     ktaa          klist
010100050527     c                   kfld                    tasaas
010200050527     c                   kfld                    taslnp
010300050527     c                   kfld                    tasnrs
010400050527     c                   kfld                    tasnsp
010500050527     c                   kfld                    ktrc              1
010600050601     c*
010700050601     c     kFiar5        Klist
010800050601     c                   Kfld                    TasAas
010900050601     c                   Kfld                    TasLnp
011000050601     c                   Kfld                    TasNrs
011100050601     c                   Kfld                    TasNsp
011200050601     c                   Kfld                    kAr5Trd
011300050314     c*
011400050314     c     Ktab          klist
011500050314     C                   KFLD                    CODUT
011600050314     C                   KFLD                    kcod
011700050314     c                   z-add     1             codut             1 0
011800050527     c*
011900050527     C     ktbe          klist
012000050527     C                   kfld                    tbecod
012100050527     C                   kfld                    tbeke1
012200050527     C                   kfld                    tbeke2
012300050527     c
012400050527     c     *entry        plist
012500050527     c                   parm                    kpjba
012600050527     c     *dtaara       define    §azute        azuteds
012700050527     c     *dtaara       define    §datiute      ddatiute
012800050527     c                   in(E)     *dtaara
012900050527     c                   If        %error  or RSUT = *blanks
013000050527     c                   CLEAR                   tibs34ds
013100050527     c                   CALL      'TIBS34R'
013200050527     c                   PARM                    tibs34ds
013300050527     c                   in        *dtaara
013400050527     c                   EndIf
013500040331      *
013600040331      *   I N D I C A T O R I   U T I L I Z Z A T I   - - - - - - - - *
013700040331      *
013800040119
013900040119      *---------------------------------------------------------------*
014000040119
014100040331      * Operazioni Iniziali
014200040331     c                   exsr      OperazIniz
014300040331      *
014400050526      * Ciclo di lettura del file TIta436l
014500050526     c                   read      tita436l
014600040119      *
014700050526do  1c                   dow       NOT %eof(TIta436l)
014800040119      *
014900050526     c     Ktas          chain     TITAS30C
015000040119      *
015100050530    2c                   if        %found(TITAS30C)
015200050526     c* se bolla che non prevede segnacolli non devo segnalarla nella mail
015300050526     c     tasctm        lookup    ctmf8                                  30
015400050530    3c                   if        *in30 = *off and tasias > 0
015500050526     c* verifico se presente mandato
015600050526     c                   exsr      sr_sf20
015700050530    4c                   if        task88 = 'S'
015800050526     c* routine di gestione scrittura e-mail
015900050526     c                   exsr      sr_email
016000050526     c* flaggo record come inviato
016100050526     c                   movel     ta4not        dta4c
016200050526     C                   eval      §ta4mail = 'S'
016300050526     C                   z-add     dateu         §ta4dtma
016400050526     c                   movel     dta4c         ta4not
016500050526     c                   update    tita4000
016600050526     c
016700050530   x4c                   else
016800050526     c* se e-mail da non inviare flaggo tita4
016900050526     c                   movel     ta4not        dta4c
017000050526     C                   eval      §ta4mail = 'N'
017100050527     C                   z-add     99999999      §ta4dtma
017200050526     c                   movel     dta4c         ta4not
017300050526     c                   update    tita4000
017400050530    4c                   endif
017500050530   x3c                   else
017600050530     c* devo flaggare a N +9999999 anche quando bolla senza segnacolli o tas
017700050530     c* ias = 0
017800050530     c                   movel     ta4not        dta4c
017900050530     C                   eval      §ta4pias = 'N'
018000050530     C                   eval      §ta4mail = 'N'
018100050530     C                   z-add     99999999      §ta4dtma
018200050530     c                   movel     dta4c         ta4not
018300050530     c                   update    tita4000
018400050530     c
018500050530    3c                   endif
018600050530   x2c                   else
018700050530     c                   unlock    tita436l
018800050530e   2c                   endif
018900040119      *
019000050526      * - Lettura del record successivo nel file TIta436l
019100050526     c                   read      tita436l
019200040119      *
019300050530e   1c                   enddo
019400050526      *
019500050526     c                   movel     'C'           tastla
019600050526     C                   CALL      'TNSF20R'
019700050526     C                   PARM                    KPJBA
019800050526     C                   PARM                    DTAS
019900050526     C                   PARM                    DTASV
020000050526     c*
020100040119     c                   eval      *inLR = *on
020200040331      *
020300040331      **-------------------------------------------------------------**
020400040331      ** OperazIniz ** Operazioni Iniziali                           **
020500040331      **-------------------------------------------------------------**
020600040331     c     OperazIniz    BEGSR
020700040331      *
020800050527     c                   time                    W0140            14 0
020900050526     C* UDATE IN GGMMAAAA
021000050526     C                   MOVE      W0140         WDTGIO            8 0
021100050526     C*
021200050526     C* UDATE IN AAAAMMGG
021300050526     C                   Z-ADD     WDTGIO        G02DAT
021400050526     C                   MOVEL     *BLANK        G02ERR
021500050526     C                   CALL      'XSRDA8'
021600050526     C                   PARM                    WLBDAT
021700050526     C                   MOVEL     G02INV        DATEU             8 0
021800040331      *
021900050526      * Intabellamento tabella 1b-codici trattamento merce dei soli codici
022000050526     c* che non prevedono i segnacolli in bolla (§1bfg8 = 'N')
022100050314     c                   clear                   xx
022200050526     c                   clear                   ctmf8
022300050526     c                   movel     '1B'          kcod
022400050526     c     ktab          setll     tabel00f
022500050314     c                   do        *hival
022600050526     c     ktab          reade     tabel00f
022700050526     c                   if        %eof(tabel00f)
022800050314     c                   leave
022900050314     c                   endif
023000050526     c                   movel     tbluni        ds1b
023100050526     c     §1bfg8        ifeq      'N'
023200050314     c                   add       1             xx
023300050526     c                   movel     tblkey        ctmf8(xx)
023400050321     c                   endif
023500050314     c                   enddo
023600040331      *
023700040331     c                   ENDSR
023800040331      *
023900050526      *
024000050526      **-------------------------------------------------------------**
024100050526      ** Richiamo pgm tassaz. tnsf20r per sapere se presente mandato **
024200050526      **-------------------------------------------------------------**
024300050526     c     sr_sf20       BEGSR
024400050526      *
024500050601     c* imposto campi della dtas come da tnsf07r per calcolo varia R
024600050601     c* Non ho quindi bisogno di impostare le varie e i campi del contrasseg
024700050601     c* no
024800050601     c                   movel     *Blanks       tastla
024900050601     c                   clear                   dAr5Ban
025000050601     c                   Clear                   dAr5Bnb
025100050526     c                   eval      tassva = 'R'
025200050526     c                   eval      parca  = 'S'
025300050527     c                   movel     tasaas        tasdsp
025400050527     c                   move      tasmgs        tasdsp
025500050527     c                   z-add     tasdsp        tasdct
025600050601     c                   movel     Tasftc        tastc1
025700050601      * Flag SI NO DDT
025800050601    1C                   If        tasll1 = 'C' or tasll1 = 'S'
025900050601     C                   movel     'S'           Tasddt
026000050601   x1C                   Else
026100050601     C                   Clear                   Tasddt
026200050601    1C                   Endif
026300060926     c******             movel     tasfbr        §tasflo
026400060926     c                   clear                   dtas01
026500060926     c                   movel     tasfbr        §asfbr
026600060926     c                   movel     tascca        §ascca
026700060926     c                   eval      §tasflo = dtas01
026800050601     c                   clear                   tasban
026900050601      * Bancali
027000050601    1c                   If        %Subst(TasGva:1:1) = 'B'
027100050601
027200050601     c                   eval      kAr5Trd  = 'BAN'
027300050601     c     kFiar5        Chain     Fiar531c
027400050601If  1c                   If        %Found(Fiar531c)
027500050601     c                   Movel     Ar5Uni        dAr5Ban
027600050601    1c                   EndIf
027700050601      * Bancali
027800050601     c                   Eval      TasBan = §Ar5Nba + §Ar5Nb2
027900050601    1c                   EndIf
028000050601      **
028100050601      * Colli Originali
028200050601    1c                   If        %Subst(TasGva:1:1) = 'O'
028300050601     c                   eval      kAr5Trd  = 'BNB'
028400050601     c     kFiar5        Chain     Fiar531c
028500050601If  1c                   If        %Found(Fiar531c)
028600050601     c                   Movel     Ar5Uni        dAr5Bnb
028700050601    1c                   EndIf
028800050601      * Colli
028900050601     c                   Eval      TasNcl = §Ar5bcor
029000050601    1c                   EndIf
029100050601     c*
029200050526     c                   movel     paramt        kpjbu
029300050526     c*
029400050526     c                   call      'TNSF20R'
029500050526     C                   PARM                    KPJBA
029600050526     C                   PARM                    DTAS
029700050526     C                   PARM                    Dtasv
029800050526     c                   endsr
029900050526      *************************************************************************
030000050526      * ROUTINE PER gestione scrittura e-mail
030100050526      *************************************************************************
030200050526     C     sr_email      BEGSR
030300050526     c* se è la prima volta che entra in questa routine eseguo ovrprtf
030400050527     c* sul file di spool e stampo l'intestazione
030500050526     c                   if        wprimav = ' '
030600050526     c                   exsr      ovrmail
030700050526     c                   eval      wprimav = 'N'
030800050527     c                   write     sb65tes
030900050526     c                   endif
031000050527     c* data ritiro
031100050527     c                   clear                   wlbdat
031200050527     C                   MOVEL     '3'           G02ERR
031300050527     C                   move      tasdrt        g02inv
031400050527     C                   CALL      'XSRDA8'
031500050527     C                   PARM                    WLBDAT
031600050527     C                   Z-ADD     G02DAT        pdrt
031700050527     c* MITTENTE:
031800050527     c* ragione sociale mittente
031900050527     c* Località
032000050527     c* nazione o provincia
032100050527     c                   move      tasccm        w0040             4 0
032200050527     c                   clear                   prsm
032300050527     c                   clear                   ploc
032400050527     c                   clear                   pnazpr
032500050527     c                   if        w0040=8888
032600050527     c                   movel     'M'           ktrc
032700050527     c     ktaa          chain     titaa30c
032800050527     c                   if        %found(titaa30c)
032900050527     c                   eval      prsm = taarsc
033000050527     c                   eval      ploc = taaloc
033100050527     c                   if        taaprv = *blanks
033200050527     c                   eval      pnazpr = taanaz
033300050527     c                   else
033400050527     c                   eval      pnazpr = taaprv
033500050527     c                   endif
033600050527     c                   endif
033700050527     c                   else
033800050527     C                   clear                   DS_cnaco
033900050527     C                   clear                   DS_cnind
034000050527     C                   clear                   DS_cnclp
034100050527     C                   clear                   DS_fncls
034200050527     c                   eval      I69kac = tasccm
034300050527     c                   eval      I69kin = tasccm
034400050527     C                   CALL      'TIBS69R'
034500050527     C                   PARM                    tibs69DS
034600050527     C                   PARM                    DS_cnaco
034700050527     C                   PARM                    DS_cnind
034800050527     C                   PARM                    DS_cnclp
034900050527     C                   PARM                    DS_fncls
035000050527     c                   eval      prsm = acorag
035100050527     c                   eval      ploc = indcit
035200050527     c                   if        indprv = *blanks
035300050527     c                   eval      pnazpr = indsta
035400050527     c                   else
035500050527     c                   eval      pnazpr = indprv
035600050527     c                   endif
035700050527     c                   endif
035800050530     c* riferimento mittente numerico - Tolgo zeri non significativi e allin
035900050530     c* eo a dx
036000050530     c                   move      tasrmn        wtasrmn
036100050530     c     '0'           check     wtasrmn       pos               2 0
036200050530     c                   eval      lung = %len(wtasrmn) - (pos-1)
036300050530     c                   clear                   wstrrif
036400050530     c                   eval      wstrrif = %subst(wtasrmn:pos:lung)
036500050530     c
036600050527     c* riferimento mittente alfabetico
036700050527     c                   clear                   dta4a
036800050527     c                   movel     'A'           ktrc
036900050527     c     ktaa          chain     tita430c
037000050527     c                   if        %found(tita430c)
037100050527     c                   movel     A_ta4not      dta4a
037200050530     c                   eval      wstrrif = %trim(wstrrif) + ' / '
037300050530     c                             + §ta4arma
037400050527     c                   endif
037500050527     c
037600050527     c                   write     sb65spe
037700050526     c                   endsr
037800050526      *************************************************************************
037900050526      * ROUTINE PER CREARE SPOOL PER INVIO e-mail
038000050526      *************************************************************************
038100050526     C     OVRmail       BEGSR
038200050527     c
038300050527     c* Reperisco dati in tabella mra per OVRTPRTF
038400050527     c                   move      'MRA'         tbecod
038500050527     c                   movel(p)  'TNSB65R'     tbeke1
038600050527     c                   move      *blank        tbeke2
038700050527     c                   clear                   dmradan
038800050527     c                   clear                   trtcm1ds
038900050527     c     ktbe          chain     tntbe01l
039000050527     c                   if        %found
039100050527     c                   movel     tbeuni        dmradan
039200050527     c                   eval      §cm1dst = §mraddest
039300050527     c                   move      dutpou        §cm1po
039400050527     c                   move      '0'           §cm1sts
039500050527     c                   movel     §mradreg      §cm1tips
039600050527     c                   movel     §mradidpro    §cm1idp
039700050527     c                   eval      §cm1mitt = §mradmitt
039800050527     c                   eval      §cm1var= '*OBJM*' + cosobj
039900050527     c                   endif
040000050526      *
040100050527      * ESEGUO OVRPRTF PER CREARE UNO SPOOL PER LA LETTERA DA INVIARE
040200050527      *   TRAMITE E-mail all'assicurazione
040300050526      *
040400050526     c*
040500050526     c* eseguo ovprtf
040600050527     c                   eval      %subst(cmm: 23: 10) = §MRADOUTQI
040700050530     c                   movea     cmm(1)        svcom
040800050530     c                   eval      cmmm = %trim(svcom) + trtcm1ds
040900050526     c                             + ''')'
041000050530     c                   eval      lungm = %len(%trim(cmmm))
041100050526     C                   CALL      'QCMDEXC'                            H1
041200050530     C                   PARM                    CMMm            500
041300050526     C                   PARM                    LUNGM
041400050526     c
041500050526     C*
041600050527     C                   OPEN      TNSB65p
041700050526     C*
041800050526     C                   ENDSR
041900050526**
042000050527OVRPRTF TNSB65P  OUTQ(          ) USRDFNDTA('

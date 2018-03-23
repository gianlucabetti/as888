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
004800050527     d dtasv         e ds                  inz
004900050527     d dmradan       e ds
005000050527     d trtcm1ds      e ds
005100050527     d TIBS69DS      E DS                  INZ
005200050527     d DS_cnaco      E DS                  extname(CNACO00F)
005300050527     d DS_cnind      E DS                  extname(CNIND00F)
005400050527     d DS_cnclp      E DS                  extname(CNCLP00F)
005500050527     d DS_fncls      E DS                  extname(FNCLS00F)
005600040331      *
005700050526
005800050526     D PARAMT          DS
005900050526     D  PARCA                256    256
006000050526     D***
006100050526     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
006200050526     D***
006300050526     D WLBDAT          DS                  INZ
006400050526     D  G02DAT                 1      8  0
006500050526     D  G02INV                 9     16  0
006600050526     D  G02ERR                17     17
006700050526     D  G02TGI                18     22  0
006800040331      *
006900040331      *
007000040331      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
007100050526      *
007200040331      * - Indici di schiera
007300040331     d xx              s              3  0 inz
007400040331      * - Campi di comodo
007500050526     d kcod            s                   like(tblcod)
007600050527     d wprimav         s              1
007700050530     D svcom           s             80
007800050530     D lungm           S             15  5 inz(80)
007900050530     D lung            S              2  0 inz(0)
008000050530     d WtasRMN         s             15                 inz(*zeros)
008100050601     d kAr5Trd         s                   Like(Ar5Trd)
008200050527     D*
008300050527     D* DEFINIZIONE COSTANTI
008400050527     D*
008500050527     D Cosobj          C                   CONST('Elenco spedizioni a-
008600050527     D                                     ssicurate')
008700040119
008800040119      *---------------------------------------------------------------*
008900040119
009000040119      *
009100040119      *   K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - - *
009200040119      *
009300050526     c     Ktas          klist
009400050526     c                   kfld                    TA4aas
009500050526     c                   kfld                    TA4lnp
009600050526     c                   kfld                    TA4nrs
009700050526     c                   kfld                    TA4nsp
009800050527     c*
009900050527     c     ktaa          klist
010000050527     c                   kfld                    tasaas
010100050527     c                   kfld                    taslnp
010200050527     c                   kfld                    tasnrs
010300050527     c                   kfld                    tasnsp
010400050527     c                   kfld                    ktrc              1
010500050601     c*
010600050601     c     kFiar5        Klist
010700050601     c                   Kfld                    TasAas
010800050601     c                   Kfld                    TasLnp
010900050601     c                   Kfld                    TasNrs
011000050601     c                   Kfld                    TasNsp
011100050601     c                   Kfld                    kAr5Trd
011200050314     c*
011300050314     c     Ktab          klist
011400050314     C                   KFLD                    CODUT
011500050314     C                   KFLD                    kcod
011600050314     c                   z-add     1             codut             1 0
011700050527     c*
011800050527     C     ktbe          klist
011900050527     C                   kfld                    tbecod
012000050527     C                   kfld                    tbeke1
012100050527     C                   kfld                    tbeke2
012200050527     c
012300050527     c     *entry        plist
012400050527     c                   parm                    kpjba
012500050527     c     *dtaara       define    §azute        azuteds
012600050527     c     *dtaara       define    §datiute      ddatiute
012700050527     c                   in(E)     *dtaara
012800050527     c                   If        %error  or RSUT = *blanks
012900050527     c                   CLEAR                   tibs34ds
013000050527     c                   CALL      'TIBS34R'
013100050527     c                   PARM                    tibs34ds
013200050527     c                   in        *dtaara
013300050527     c                   EndIf
013400040331      *
013500040331      *   I N D I C A T O R I   U T I L I Z Z A T I   - - - - - - - - *
013600040331      *
013700040119
013800040119      *---------------------------------------------------------------*
013900040119
014000040331      * Operazioni Iniziali
014100040331     c                   exsr      OperazIniz
014200040331      *
014300050526      * Ciclo di lettura del file TIta436l
014400050526     c                   read      tita436l
014500040119      *
014600050526do  1c                   dow       NOT %eof(TIta436l)
014700040119      *
014800050526     c     Ktas          chain     TITAS30C
014900040119      *
015000050530    2c                   if        %found(TITAS30C)
015100050526     c* se bolla che non prevede segnacolli non devo segnalarla nella mail
015200050526     c     tasctm        lookup    ctmf8                                  30
015300050530    3c                   if        *in30 = *off and tasias > 0
015400050526     c* verifico se presente mandato
015500050526     c                   exsr      sr_sf20
015600050530    4c                   if        task88 = 'S'
015700050526     c* routine di gestione scrittura e-mail
015800050526     c                   exsr      sr_email
015900050526     c* flaggo record come inviato
016000050526     c                   movel     ta4not        dta4c
016100050526     C                   eval      §ta4mail = 'S'
016200050526     C                   z-add     dateu         §ta4dtma
016300050526     c                   movel     dta4c         ta4not
016400050526     c                   update    tita4000
016500050526     c
016600050530   x4c                   else
016700050526     c* se e-mail da non inviare flaggo tita4
016800050526     c                   movel     ta4not        dta4c
016900050526     C                   eval      §ta4mail = 'N'
017000050527     C                   z-add     99999999      §ta4dtma
017100050526     c                   movel     dta4c         ta4not
017200050526     c                   update    tita4000
017300050530    4c                   endif
017400050530   x3c                   else
017500050530     c* devo flaggare a N +9999999 anche quando bolla senza segnacolli o tas
017600050530     c* ias = 0
017700050530     c                   movel     ta4not        dta4c
017800050530     C                   eval      §ta4pias = 'N'
017900050530     C                   eval      §ta4mail = 'N'
018000050530     C                   z-add     99999999      §ta4dtma
018100050530     c                   movel     dta4c         ta4not
018200050530     c                   update    tita4000
018300050530     c
018400050530    3c                   endif
018500050530   x2c                   else
018600050530     c                   unlock    tita436l
018700050530e   2c                   endif
018800040119      *
018900050526      * - Lettura del record successivo nel file TIta436l
019000050526     c                   read      tita436l
019100040119      *
019200050530e   1c                   enddo
019300050526      *
019400050526     c                   movel     'C'           tastla
019500050526     C                   CALL      'TNSF20R'
019600050526     C                   PARM                    KPJBA
019700050526     C                   PARM                    DTAS
019800050526     C                   PARM                    DTASV
019900050526     c*
020000040119     c                   eval      *inLR = *on
020100040331      *
020200040331      **-------------------------------------------------------------**
020300040331      ** OperazIniz ** Operazioni Iniziali                           **
020400040331      **-------------------------------------------------------------**
020500040331     c     OperazIniz    BEGSR
020600040331      *
020700050527     c                   time                    W0140            14 0
020800050526     C* UDATE IN GGMMAAAA
020900050526     C                   MOVE      W0140         WDTGIO            8 0
021000050526     C*
021100050526     C* UDATE IN AAAAMMGG
021200050526     C                   Z-ADD     WDTGIO        G02DAT
021300050526     C                   MOVEL     *BLANK        G02ERR
021400050526     C                   CALL      'XSRDA8'
021500050526     C                   PARM                    WLBDAT
021600050526     C                   MOVEL     G02INV        DATEU             8 0
021700040331      *
021800050526      * Intabellamento tabella 1b-codici trattamento merce dei soli codici
021900050526     c* che non prevedono i segnacolli in bolla (§1bfg8 = 'N')
022000050314     c                   clear                   xx
022100050526     c                   clear                   ctmf8
022200050526     c                   movel     '1B'          kcod
022300050526     c     ktab          setll     tabel00f
022400050314     c                   do        *hival
022500050526     c     ktab          reade     tabel00f
022600050526     c                   if        %eof(tabel00f)
022700050314     c                   leave
022800050314     c                   endif
022900050526     c                   movel     tbluni        ds1b
023000050526     c     §1bfg8        ifeq      'N'
023100050314     c                   add       1             xx
023200050526     c                   movel     tblkey        ctmf8(xx)
023300050321     c                   endif
023400050314     c                   enddo
023500040331      *
023600040331     c                   ENDSR
023700040331      *
023800050526      *
023900050526      **-------------------------------------------------------------**
024000050526      ** Richiamo pgm tassaz. tnsf20r per sapere se presente mandato **
024100050526      **-------------------------------------------------------------**
024200050526     c     sr_sf20       BEGSR
024300050526      *
024400050601     c* imposto campi della dtas come da tnsf07r per calcolo varia R
024500050601     c* Non ho quindi bisogno di impostare le varie e i campi del contrasseg
024600050601     c* no
024700050601     c                   movel     *Blanks       tastla
024800050601     c                   clear                   dAr5Ban
024900050601     c                   Clear                   dAr5Bnb
025000050526     c                   eval      tassva = 'R'
025100050526     c                   eval      parca  = 'S'
025200050527     c                   movel     tasaas        tasdsp
025300050527     c                   move      tasmgs        tasdsp
025400050527     c                   z-add     tasdsp        tasdct
025500050601     c                   movel     Tasftc        tastc1
025600050601      * Flag SI NO DDT
025700050601    1C                   If        tasll1 = 'C' or tasll1 = 'S'
025800050601     C                   movel     'S'           Tasddt
025900050601   x1C                   Else
026000050601     C                   Clear                   Tasddt
026100050601    1C                   Endif
026200060424     c                   movel     tasfbr        §tasflo
026300050601     c                   clear                   tasban
026400050601      * Bancali
026500050601    1c                   If        %Subst(TasGva:1:1) = 'B'
026600050601
026700050601     c                   eval      kAr5Trd  = 'BAN'
026800050601     c     kFiar5        Chain     Fiar531c
026900050601If  1c                   If        %Found(Fiar531c)
027000050601     c                   Movel     Ar5Uni        dAr5Ban
027100050601    1c                   EndIf
027200050601      * Bancali
027300050601     c                   Eval      TasBan = §Ar5Nba + §Ar5Nb2
027400050601    1c                   EndIf
027500050601      **
027600050601      * Colli Originali
027700050601    1c                   If        %Subst(TasGva:1:1) = 'O'
027800050601     c                   eval      kAr5Trd  = 'BNB'
027900050601     c     kFiar5        Chain     Fiar531c
028000050601If  1c                   If        %Found(Fiar531c)
028100050601     c                   Movel     Ar5Uni        dAr5Bnb
028200050601    1c                   EndIf
028300050601      * Colli
028400050601     c                   Eval      TasNcl = §Ar5bcor
028500050601    1c                   EndIf
028600050601     c*
028700050526     c                   movel     paramt        kpjbu
028800050526     c*
028900050526     c                   call      'TNSF20R'
029000050526     C                   PARM                    KPJBA
029100050526     C                   PARM                    DTAS
029200050526     C                   PARM                    Dtasv
029300050526     c                   endsr
029400050526      *************************************************************************
029500050526      * ROUTINE PER gestione scrittura e-mail
029600050526      *************************************************************************
029700050526     C     sr_email      BEGSR
029800050526     c* se è la prima volta che entra in questa routine eseguo ovrprtf
029900050527     c* sul file di spool e stampo l'intestazione
030000050526     c                   if        wprimav = ' '
030100050526     c                   exsr      ovrmail
030200050526     c                   eval      wprimav = 'N'
030300050527     c                   write     sb65tes
030400050526     c                   endif
030500050527     c* data ritiro
030600050527     c                   clear                   wlbdat
030700050527     C                   MOVEL     '3'           G02ERR
030800050527     C                   move      tasdrt        g02inv
030900050527     C                   CALL      'XSRDA8'
031000050527     C                   PARM                    WLBDAT
031100050527     C                   Z-ADD     G02DAT        pdrt
031200050527     c* MITTENTE:
031300050527     c* ragione sociale mittente
031400050527     c* Località
031500050527     c* nazione o provincia
031600050527     c                   move      tasccm        w0040             4 0
031700050527     c                   clear                   prsm
031800050527     c                   clear                   ploc
031900050527     c                   clear                   pnazpr
032000050527     c                   if        w0040=8888
032100050527     c                   movel     'M'           ktrc
032200050527     c     ktaa          chain     titaa30c
032300050527     c                   if        %found(titaa30c)
032400050527     c                   eval      prsm = taarsc
032500050527     c                   eval      ploc = taaloc
032600050527     c                   if        taaprv = *blanks
032700050527     c                   eval      pnazpr = taanaz
032800050527     c                   else
032900050527     c                   eval      pnazpr = taaprv
033000050527     c                   endif
033100050527     c                   endif
033200050527     c                   else
033300050527     C                   clear                   DS_cnaco
033400050527     C                   clear                   DS_cnind
033500050527     C                   clear                   DS_cnclp
033600050527     C                   clear                   DS_fncls
033700050527     c                   eval      I69kac = tasccm
033800050527     c                   eval      I69kin = tasccm
033900050527     C                   CALL      'TIBS69R'
034000050527     C                   PARM                    tibs69DS
034100050527     C                   PARM                    DS_cnaco
034200050527     C                   PARM                    DS_cnind
034300050527     C                   PARM                    DS_cnclp
034400050527     C                   PARM                    DS_fncls
034500050527     c                   eval      prsm = acorag
034600050527     c                   eval      ploc = indcit
034700050527     c                   if        indprv = *blanks
034800050527     c                   eval      pnazpr = indsta
034900050527     c                   else
035000050527     c                   eval      pnazpr = indprv
035100050527     c                   endif
035200050527     c                   endif
035300050530     c* riferimento mittente numerico - Tolgo zeri non significativi e allin
035400050530     c* eo a dx
035500050530     c                   move      tasrmn        wtasrmn
035600050530     c     '0'           check     wtasrmn       pos               2 0
035700050530     c                   eval      lung = %len(wtasrmn) - (pos-1)
035800050530     c                   clear                   wstrrif
035900050530     c                   eval      wstrrif = %subst(wtasrmn:pos:lung)
036000050530     c
036100050527     c* riferimento mittente alfabetico
036200050527     c                   clear                   dta4a
036300050527     c                   movel     'A'           ktrc
036400050527     c     ktaa          chain     tita430c
036500050527     c                   if        %found(tita430c)
036600050527     c                   movel     A_ta4not      dta4a
036700050530     c                   eval      wstrrif = %trim(wstrrif) + ' / '
036800050530     c                             + §ta4arma
036900050527     c                   endif
037000050527     c
037100050527     c                   write     sb65spe
037200050526     c                   endsr
037300050526      *************************************************************************
037400050526      * ROUTINE PER CREARE SPOOL PER INVIO e-mail
037500050526      *************************************************************************
037600050526     C     OVRmail       BEGSR
037700050527     c
037800050527     c* Reperisco dati in tabella mra per OVRTPRTF
037900050527     c                   move      'MRA'         tbecod
038000050527     c                   movel(p)  'TNSB65R'     tbeke1
038100050527     c                   move      *blank        tbeke2
038200050527     c                   clear                   dmradan
038300050527     c                   clear                   trtcm1ds
038400050527     c     ktbe          chain     tntbe01l
038500050527     c                   if        %found
038600050527     c                   movel     tbeuni        dmradan
038700050527     c                   eval      §cm1dst = §mraddest
038800050527     c                   move      dutpou        §cm1po
038900050527     c                   move      '0'           §cm1sts
039000050527     c                   movel     §mradreg      §cm1tips
039100050527     c                   movel     §mradidpro    §cm1idp
039200050527     c                   eval      §cm1mitt = §mradmitt
039300050527     c                   eval      §cm1var= '*OBJM*' + cosobj
039400050527     c                   endif
039500050526      *
039600050527      * ESEGUO OVRPRTF PER CREARE UNO SPOOL PER LA LETTERA DA INVIARE
039700050527      *   TRAMITE E-mail all'assicurazione
039800050526      *
039900050526     c*
040000050526     c* eseguo ovprtf
040100050527     c                   eval      %subst(cmm: 23: 10) = §MRADOUTQI
040200050530     c                   movea     cmm(1)        svcom
040300050530     c                   eval      cmmm = %trim(svcom) + trtcm1ds
040400050526     c                             + ''')'
040500050530     c                   eval      lungm = %len(%trim(cmmm))
040600050526     C                   CALL      'QCMDEXC'                            H1
040700050530     C                   PARM                    CMMm            500
040800050526     C                   PARM                    LUNGM
040900050526     c
041000050526     C*
041100050527     C                   OPEN      TNSB65p
041200050526     C*
041300050526     C                   ENDSR
041400050526**
041500050527OVRPRTF TNSB65P  OUTQ(          ) USRDFNDTA('

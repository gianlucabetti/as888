000100041124      *PARMS CLOSQLCSR(*ENDMOD)
000200041124      *PARMS DYNUSRPRF(*OWNER)
000300040820     H DECEDIT('0,') DATEDIT(*dmy.)
000400020409     H**********************************************************************
000500041123?     *--------------------------------------------------------------*
000600041123      * FIDN70R                                                      *
000700041123      *     *----------------------------------------------*         *
000800041123      *         STATISTICA NR INVII PREAVVISI DI DANNO               *
000900041123      *                 nel periodo richiesto                        *
001000041123      *     *---------------------------------------------*          *
001100041123      *                                                              *
001200041123?     *--------------------------------------------------------------*
001300041123      *    -                                                         *
001400041123      *    -                                                         *
001500041123      *    -                                                         *
001600041123      *    -                                                         *
001700041123      *    -                                                         *
001800041123?     *--------------------------------------------------------------*
001900041123     fFndcf03l  if   e           k disk
002000041123     fFndct01l  if   e           k disk
002100041123     fFndka01l  if   e           k disk
002200041123     fFnspe01l  if   e           k disk
002300041123     fFnarb01l  if   e           k disk
002400041123     fCnind00f  if   e           k disk
002500041123     fCnaco00f  if   e           k disk
002600041124     fWfnrp00f  uf a e           k disk
002700041124     fFidn70P   o    E             PRINTER OFLIND(*in66)
002800041122     f
002900041123      *---------------------------------------------------------------*
003000041123      * SCHIERA
003100041123      *---------------------------------------------------------------*
003200041123?     *--------------------------------------------------------------*
003300041123?     *  DS                                                          *
003400041123?     *--------------------------------------------------------------*
003500041123     D KPJBA         E DS
003600041123      *
003700041123     D FIDN69ds      E DS
003800041123     D  lnpd69                24    113  0 dim(30)
003900041125     D*
004000041125     D ddatiute      e ds
004100041125     D azuteds       e ds                  extname(AZUTE00F)
004200041125     D tibs34ds      E DS                  inz
004300041123     D
004400041123     d trul06ds      e ds
004500041123     d  lin                    1     90  0 dim(30)
004600041123     D Tibs36ds      E DS
004700041123      *
004800041123     d ddcf01        e ds
004900041124     d ddct01        e ds
005000041124     d dswfnrp       e ds                  extname(wfnrp00f)
005100041123      *
005200041123      *
005300041123      *
005400041123     D WLBDA8          DS
005500041123     D  G02DAT                 1      8  0
005600041123     D  G02INV                 9     16  0
005700041123     D  G02ERR                17     17
005800041123     D  G02TGI                18     22  0
005900041123      *
006000041123     D WGIDAT          DS
006100041123     D  GIODAT                 1      8  0
006200041123     D  GIOINV                 9     16  0
006300041123     D  GIOTGI                17     21  0
006400041123      *---------------------------------------------------------------------------------------------
006500041123     D Kfls            S                   LIKE(SPEfls)  inz('L')
006600041123     D Kcli            S                   LIKE(SPEcli)
006700041123     D Kclu            S                   LIKE(SPEcod)  inz('006')             Luogo per danni
006800041123     D Kkcc            S                   LIKE(DCTkcc)
006900041123     D Ktrc            S                   LIKE(DKAtrc)  inz('C')
007000041123     D Kfca            S                   LIKE(DCFfca)  inz(400)
007100041123
007200041123     D W§dctport       S                   LIKE(§DCTport)
007300041123
007400041124     d sav_fil         s                   LIKE(wnpfil)
007500041124
007600041123     D wdtgio          S              8  0
007700041123     D w0140           S             14  0
007800041123     D w0040           S              4  0
007900041123     D w0030           S              3  0
008000041123     D pixfax          S             16
008100041125     D CODUT           S              1  0 INZ(1)
008200041123     D luogo           S              1
008300041124     D invio           S              1
008400041124     D prima           S              1  0
008500041124     D XX              S              2  0
008600041123
008700041123      *
008800041123      *------------------------------------------------------------------
008900041123      *
009000041123      *------------------------------------------------------------------
009100041123     c                   Exsr      SR_eladcf
009200041123      *
009300041124     c                   Exsr      SR_elawrkf
009400041123      *
009500041123     c                   seton                                        lr
009600041123      *------------------------------------------------------------------
009700041123      *
009800041123      *------------------------------------------------------------------
009900041123     c     SR_eladcf     Begsr
010000041123      *
010100041123     c     kdcf          setll     fndcf03l
010200041123     c                   do        *hival
010300041123     c     kfca          reade     fndcf03l
010400041123      *
010500041123     c                   If        %eof(fndcf03l)
010600041123     c                   leave
010700041123     c                   endif
010800041123      * controllo la data
010900041123     c                   If        dcfdfc > d69dsa
011000041123     c                   leave
011100041123     c                   endif
011200041123      * se non c'è il tipo invio cerco il cliente intestatario
011300041123     c                   move      dcflet        ddcf01
011400041123      *
011500041123      * recupero codice cliente
011600041123     c                   exsr      SR_Reccli
011700041123
011800041124     c                   If        kcli > 0
011900041123     c                   if        §dcfinvi = ' '
012000041123     c                   eval      invio = 'L'
012100041123      * se c'è il fax
012200041123     c                   if        pixfax <> *blank
012300041123     c                   eval      invio = 'F'
012400041123     c                   endif
012500041124
012600041124     c                   else
012700041124     c                   eval      invio = §dcfinvi
012800041124     c                   endif
012900041124      * controllo invio
013000041124     c                   if        d69t01 <> '9'
013100041124     c                   if        invio <> d69t01 and invio <> d69t02
013200041124     c                             and invio <> d69t03
013300041124     c                   iter
013400041124     c                   endif
013500041124     c                   endif
013600041123      *
013700041123     c                   else
013800041123      * se non c'è codice cliente leggo la fase ca successiva
013900041123     c                   iter
014000041123      *
014100041123     c                   endif
014200041123      *
014300041123     c                   exsr      SR_WFNRP
014400041123      *
014500041123     c                   enddo
014600041124      *
014700041124     c                   endsr
014800041123      *
014900041123      *****************************************************************
015000041123      *   RECUPERO CLIENTE INTESTATARIO
015100041123      *****************************************************************
015200041123     C     SR_RECCLI     BEGSR
015300041123      *
015400041123     c                   clear                   kcli
015500041123     c                   clear                   pixfax
015600041123     c                   clear                   luogo
015700041123     c                   clear                   w0030
015800041123     c                   move      DCTksc        w0040
015900041124
016000041124     c     Kdct          chain     fndct01l
016100041124     c                   movel     dctflo        ddct01
016200041123      *
016300041123    1c                   IF        DCTksc > *zeros
016400041123     c                   movel     DCTksc        w0030
016500041123      *
016600041123      *------
016700041123      * Se il Beneficiario alternativo è generico aggancio FNDKA altrimenti CNIND
016800041123    2c                   If        W0040 = 8888
016900041123     c     Kdka          chain     FNDKA01L
017000041123    3c                   If        %found(fndka01l)
017100041123     c                   movel     DKAfax        PIXfax
017200041123    3c                   endif
017300041123      *
017400041123   x2c                   Else
017500041123      *
017600041123     c                   z-add     DCTkcc        kkcc
017700041123     c                   z-add     DCTksc        kcli
017800041123
017900041123     c     KANA          CHAIN     CNIND00F
018000041123    3c                   if        %found(cnind00f)
018100041123     c                   movel     INDtlf        PIXfax
018200041123    3c                   endif
018300041123
018400041123      * Controllo se esiste il luogo per il beneficiario alternativo
018500041123     c     kSpe          Chain     Fnspe01l
018600041123    3c                   If        %Found(Fnspe01l)
018700041123     c                   movel     SPEfax        PIXfax
018800041123     c                   eval      luogo = 'S'
018900041123    3c                   endif
019000041123
019100041123    2c                   endif
019200041123      *------
019300041123   x1c                   ELSE
019400041123      *------
019500041123      *
019600041123
019700041123      * aggancio la bolla
019800041123
019900041123     c     Kbol          chain     FNARB01l
020000041123      *
020100041124    2c                   if        %found(FNARB01L)
020200041123      *  Se "Mamma di cambio porto" per ottenere il tipo porto della sped. inverto quello della C.A.
020300041123     c                   SELECT
020400110503     c***                WHEN      aRBcca <> '9'
020500110503     c                   WHEN      §DCTcca <> '9'
020600041123     c                   movel     §DCTport      W§DCTport
020700041123     c                   WHEN      §DCTport = 'A'
020800041123     c                   movel     'F'           W§DCTport
020900041123     c                   WHEN      §DCTport = 'F'
021000041123     c                   movel     'A'           W§DCTport
021100041123     c                   ENDSL
021200041123      *
021300041123      *
021400041123      * Determino il cod.cliente da utilizzare per verifica luogo 6 per danni
021500041123     c                   SELECT
021600041123      *
021700041123     c                   WHEN      DCTptr = 'D'  and  w§DCTport = 'A'           Destinatario PA
021800041123     c                   movel     ARbksc        kcli
021900041123      *
022000041123     c                   WHEN      DCTptr = 'M'  and  w§DCTport = 'F'           Mittente PF
022100041123     c                   z-add     ARBksc        kcli
022200041123      *
022300041123     c                   WHEN      DCTptr = 'M'  and  w§DCTport = 'A'           Mittente PA
022400041123     c                   z-add     ARBccm        kcli
022500041123      *
022600041123     c                   ENDSL
022700041123      *
022800041124    2c                   endif
022900041124      *
023000041123     c                   movel     kcli          w0030
023100041123      *
023200041123     c                   move      kcli          w0040
023300041123      *
023400041124    2c                   IF        Kcli > *zeros  and
023500041123     c                                   W0040 <> 8888  and  W0040 <> 9999
023600041123     c     kspe          chain     fnspe000                           30
023700041123     c                   ELSE
023800041123     c                   eval      *IN30 = *ON
023900041124    2c                   ENDIF
024000041123      *
024100041123      * Recupero il fax
024200041124    2c                   IF        *in30 = *OFF
024300041123     c                   movel     SPEfax        PIXfax
024400041123     c                   eval      luogo = 'S'
024500041123     c                   ELSE
024600041123      *
024700041123      *  aggancio anagrafico per N° fax
024800041125     c                   z-add     DUTKCI        kkcc
024900041123     c     KANA          CHAIN     CNIND000                           30
025000041123     c                   movel     INDtlf        PIXfax
025100041124    2c                   ENDIF
025200041123      *
025300041124    1c                   ENDIF
025400041123      *
025500041123     C                   ENDSR
025600041123      *****************************************************************
025700041123      *   SCRITTURA FILE DI LAVORO WFNRP00F
025800041123      *****************************************************************
025900041123     C     SR_WFNRP      BEGSR
026000041123      *
026100041123      * verifico il cliente
026200041124    1c                   if        d69ksc = 9999999  or d69ksc = kcli
026300041123
026400041123      *
026500041123      * verifico se il cliente appartiene ai P.O. che devo elaborare
026600041123      *
026700041123     c                   movel     kcli          w0030
026800041123      *
026900041123     c     w0030         lookup    lnpd69                                 30
027000041123      *
027100041124    2c                   if        %found
027200041123      *
027300041123      * vedo se ho già questo cliente in archivio
027400041123      *
027500041123     c     kwrf          chain     wfnrp00f
027600041124    3c                   if        %found(wfnrp00f)
027700041123      *
027800041123     c                   exsr      sr_calcol
027900041123      *
028000041123     c                   update    wfnrp000
028100041123      *
028200041124   x3c                   else
028300041123      *
028400041123     c                   clear                   wfnrp000
028500041123      * recupero ragione sociale
028600041125     c                   move      DUTKCI        kkcc
028700041123     c     kana          chain     CNACO00f
028800041124    4c                   if        %found(cnaco00f)
028900041123     c                   eval      wnprag = acorag
029000041124    4c                   endif
029100041123      *
029200041123     c                   eval      wnpfil = w0030
029300041123     c                   eval      wnpksc = kcli
029400041124     c                   eval      wnpdda = d69dsd
029500041124     c                   eval      wnpada = d69dsa
029600041123      *
029700041124    4C                   If        luogo = 'S'
029800041125     c                   eval      wnpnot = 'Presente luogo "006"'
029900041125    4c                   endif
030000041123      *
030100041123     c                   exsr      sr_calcol
030200041123      *
030300041123     c                   write     wfnrp000
030400041123
030500041124    3c                   endif
030600041124    2c                   endif
030700041124    1c                   endif
030800041123      *
030900041123     C                   ENDSR
031000041123      *
031100041123      *****************************************************************
031200041123      *   TOTALIZZO GLI INVII
031300041123      *****************************************************************
031400041123     C     SR_calcol     BEGSR
031500041123
031600041123     c                   add       1             wnptot
031700041123     c                   if        invio = 'F'
031800041123     c                   add       1             wnpnrf
031900041123     c                   endif
032000041123     c                   if        invio = 'L'
032100041123     c                   add       1             wnpnrl
032200041123     c                   endif
032300041123     c                   if        invio = 'M'
032400041123     c                   add       1             wnpnrm
032500041123     c                   endif
032600041123
032700041123     c                   endsr
032800041124      *
032900041124      *****************************************************************
033000041124      *   ELABORA FILE DI LAVORO DOPO ORDINAMENTO
033100041124      *****************************************************************
033200041124     C     SR_ELAWRKF    BEGSR
033300041124
033400041124     c                   feod      wfnrp00f
033500041124     C*
033600041124     C/EXEC SQL
033700041124     C+ declare C1 cursor for
033800041124     C+ select * from WFNRP00F
033900041124     C+ where WNPTOT > :D69SUP
034000041124     C+ order by WNPFIL, WNPTOT desc, WNPKSC
034100041124     C+ for fetch only
034200041124     C/END-EXEC
034300041124     C
034400041124     C/EXEC SQL
034500041124     C+ open C1
034600041124     C/END-EXEC
034700041124     C
034800041124     C/EXEC SQL
034900041124     C+ Fetch C1 into :dswfnrp
035000041124     C/END-EXEC
035100041124     C*
035200041124     C                   dow       sqlcod = *zeros
035300041124     C                   exsr      SR_stampa
035400041124     C*
035500041124     C/EXEC SQL
035600041124     C+ Fetch C1 into :dswfnrp
035700041124     C/END-EXEC
035800041124     C*
035900041124     C                   enddo
036000041124     C*
036100041124     C/EXEC SQL
036200041124     C+ close C1
036300041124     C/END-EXEC
036400041124     C*
036500041124      *
036600041124      * stampo i totali generali se > di 0
036700041124      *
036800041124     c                   If        tgtot > 0
036900041124     c                   if        *in66
037000041124      *                 * ------------------ *
037100041124     c                   write     testa
037200041124     c                   write     tesdetta
037300041124      *                 * ------------------ *
037400041124     c                   setoff                                       66
037500041124     c                   endif
037600041124      * stampo totale per P.O.
037700041124     c                   write     totxpo
037800041124     c                   write     totale
037900041124     c                   endif
038000041124      *
038100041124     C                   ENDSR
038200041124      *
038300041124      *****************************************************************
038400041124      *   STAMPA STATISTICA
038500041124      *****************************************************************
038600041124     C     SR_STAMPA     BEGSR
038700041124      *
038800041124      * se è la prima volta stampo la prima pagina
038900041124      *
039000041124     c                   If        prima = 0
039100041124     c                   write     testa
039200041124     c                   write     pripage
039300041124     c                   eval      prima = 1
039400041124     c                   seton                                        66
039500041124     c                   endif
039600041124      *
039700041124      * verifico se rottura di P.O.
039800041124      *
039900041124     c                   if        sav_fil = 0
040000041124     c                   eval      sav_fil = wnpfil
040100041124     c                   endif
040200041124
040300041124     c                   If        sav_fil <> wnpfil
040400041124     c                   if        *in66
040500041124      *                 * ------------------ *
040600041124     c                   write     testa
040700041124     c                   write     tesdetta
040800041124      *                 * ------------------ *
040900041124     c                   setoff                                       66
041000041124     c                   endif
041100041124      * stampo totale per P.O.
041200041124     c                   write     totxpo
041300041124     c                   eval      sav_fil = wnpfil
041400041124
041500041124     c                   clear                   tptot
041600041124
041700041124     c                   clear                   tpnrl
041800041124     c                   clear                   tpnrf
041900041124     c                   clear                   tpnrm
042000041124
042100041124     c                   endif
042200041124      *
042300041124      * elaboro il dettaglio
042400041124      *
042500041124     c                   If        sav_fil = wnpfil
042600041124     c                   movel     wnprag        stprag
042700041124      *
042800041124      * elaboro il dettaglio
042900041124      *
043000041124     c                   add       wnptot        tptot
043100041124     c                   add       wnptot        tgtot
043200041124
043300041124     c                   add       wnpnrl        tpnrl
043400041124     c                   add       wnpnrf        tpnrf
043500041124     c                   add       wnpnrm        tpnrm
043600041124
043700041124     c                   add       wnpnrl        tgnrl
043800041124     c                   add       wnpnrf        tgnrf
043900041124     c                   add       wnpnrm        tgnrm
044000041124     c* DETTAGLIO
044100041124     c                   if        *in66
044200041124      *                 * ------------------ *
044300041124     c                   write     testa
044400041124     c                   write     tesdetta
044500041124      *                 * ------------------ *
044600041124     c                   setoff                                       66
044700041124     c                   endif
044800041124      **  stampa la riga di dettaglio
044900041124     c                   write     dettagli
045000041124
045100041124     c                   endif
045200041124
045300041124     c                   endsr
045400041124      *
045500041123      *****************************************************************
045600041123      *   ROUTINE INIZIALE
045700041123      *****************************************************************
045800041123     C     *INZSR        BEGSR
045900041123      *
046000041123     C     *ENTRY        PLIST
046100041123     C                   PARM                    KPJBA
046200041123      *
046300041123     C                   MOVEL     KPJBU         fidn69ds
046400041123      *
046500041125     c     *dtaara       define    §azute        azuteds
046600041125     c     *dtaara       define    §datiute      ddatiute
046700041125     C                   in(E)     *dtaara
046800041125     C                   IF        %Error  or  RSUT = *blanks
046900041125     C                   call      'TIBS34R'
047000041125     C                   parm                    Tibs34Ds
047100041125     C                   in        *dtaara
047200041125     c                   ENDIF
047300041125      *
047400041125      *  imposta UDATE di elaborazione
047500041125     C                   move      *date         G02DAT
047600041125     C                   MOVE      *ZEROS        G02INV
047700041125     C                   MOVE      *BLANKS       G02ERR
047800041125     C                   CALL      'XSRDA8'
047900041125     C                   PARM                    WLBDA8
048000041125     C                   move      *date         day
048100041125     C                   TIME                    W0140            14 0
048200041125     C                   MOVEl     W0140         ore
048300041125      **  date
048400041125     C                   CLEAR                   WLBDA8
048500041125     C                   move      D69DSD        G02INV
048600041125     C                   MOVE      '3'           G02ERR
048700041125     C                   CALL      'XSRDA8'
048800041125     C                   PARM                    WLBDA8
048900041125     c                   move      G02DAT        dadata
049000041125
049100041125     C                   CLEAR                   WLBDA8
049200041125     C                   move      D69DSA        G02INV
049300041125     C                   MOVE      '3'           G02ERR
049400041125     C                   CALL      'XSRDA8'
049500041125     C                   PARM                    WLBDA8
049600041125     c                   move      G02DAT        adata
049700041123
049800041124      **  prima pagina
049900041124     c                   eval      ksc    = d69ksc
050000041124     c                   eval      dksc   = d69rag
050100041124     c                   eval      lnp01  = d69l01
050200041124     c                   eval      lnp02  = d69l02
050300041124     c                   eval      lnp03  = d69l03
050400041124     c                   eval      lnp04  = d69l04
050500041124     c                   eval      lnp05  = d69l05
050600041124     c                   eval      lnp06  = d69l06
050700041124     c                   eval      lnp07  = d69l07
050800041124     c                   eval      lnp08  = d69l08
050900041124     c                   eval      lnp09  = d69l09
051000041124     c                   eval      lnp10  = d69l10
051100041125     c                   eval      lnp11  = d69l11
051200041125     c                   eval      lnp12  = d69l12
051300041125     c                   eval      lnp13  = d69l13
051400041125     c                   eval      lnp14  = d69l14
051500041125     c                   eval      lnp15  = d69l15
051600041124     c                   eval      tc1d50 = d69t01
051700041124     c                   eval      tc2d50 = d69t02
051800041124     c                   eval      tc3d50 = d69t03
051900041124     c                   eval      sup    = d69sup
052000041123
052100041123     C     Kdcf          klist
052200041123     C                   KFLD                    Kfca
052300041123     C                   KFLD                    D69dsd
052400041123      *
052500041123     C     Kdct          klist
052600041123     C                   KFLD                    DCFaac
052700041123     C                   KFLD                    DCFfil
052800041123     C                   KFLD                    DCFnca
052900041123      *
053000041123     C     KANA          KLIST
053100041123     C                   KFLD                    codut
053200041123     C                   KFLD                    KKCC
053300041123     C                   KFLD                    KCLI
053400041123      *
053500041123     C     KDKA          klist
053600041123     C                   KFLD                    DCFaac
053700041123     C                   KFLD                    DCFfil
053800041123     C                   KFLD                    DCFnca
053900041123     C                   KFLD                    Ktrc
054000041123      *
054100041123     C     Kspe          klist
054200041123     C                   KFLD                    KFLS
054300041123     C                   KFLD                    KCLI
054400041123     C                   KFLD                    KCLU
054500041123      *
054600041123     c     kbol          klist
054700041123     c                   kfld                    DCTaas
054800041123     c                   kfld                    DCTlnp
054900041123     c                   kfld                    DCTnrs
055000041123     c                   kfld                    DCTnsp
055100041123      *
055200041123     c     kwrf          klist
055300041123     c                   kfld                    w0030
055400041123     c                   kfld                    kcli
055500041123      *
055600041123      *
055700041123
055800041123     C                   ENDSR

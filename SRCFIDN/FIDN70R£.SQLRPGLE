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
020400041123     c                   WHEN      ARBcca <> '9'
020500041123     c                   movel     §DCTport      W§DCTport
020600041123     c                   WHEN      §DCTport = 'A'
020700041123     c                   movel     'F'           W§DCTport
020800041123     c                   WHEN      §DCTport = 'F'
020900041123     c                   movel     'A'           W§DCTport
021000041123     c                   ENDSL
021100041123      *
021200041123      *
021300041123      * Determino il cod.cliente da utilizzare per verifica luogo 6 per danni
021400041123     c                   SELECT
021500041123      *
021600041123     c                   WHEN      DCTptr = 'D'  and  w§DCTport = 'A'           Destinatario PA
021700041123     c                   movel     ARbksc        kcli
021800041123      *
021900041123     c                   WHEN      DCTptr = 'M'  and  w§DCTport = 'F'           Mittente PF
022000041123     c                   z-add     ARBksc        kcli
022100041123      *
022200041123     c                   WHEN      DCTptr = 'M'  and  w§DCTport = 'A'           Mittente PA
022300041123     c                   z-add     ARBccm        kcli
022400041123      *
022500041123     c                   ENDSL
022600041123      *
022700041124    2c                   endif
022800041124      *
022900041123     c                   movel     kcli          w0030
023000041123      *
023100041123     c                   move      kcli          w0040
023200041123      *
023300041124    2c                   IF        Kcli > *zeros  and
023400041123     c                                   W0040 <> 8888  and  W0040 <> 9999
023500041123     c     kspe          chain     fnspe000                           30
023600041123     c                   ELSE
023700041123     c                   eval      *IN30 = *ON
023800041124    2c                   ENDIF
023900041123      *
024000041123      * Recupero il fax
024100041124    2c                   IF        *in30 = *OFF
024200041123     c                   movel     SPEfax        PIXfax
024300041123     c                   eval      luogo = 'S'
024400041123     c                   ELSE
024500041123      *
024600041123      *  aggancio anagrafico per N° fax
024700041125     c                   z-add     DUTKCI        kkcc
024800041123     c     KANA          CHAIN     CNIND000                           30
024900041123     c                   movel     INDtlf        PIXfax
025000041124    2c                   ENDIF
025100041123      *
025200041124    1c                   ENDIF
025300041123      *
025400041123     C                   ENDSR
025500041123      *****************************************************************
025600041123      *   SCRITTURA FILE DI LAVORO WFNRP00F
025700041123      *****************************************************************
025800041123     C     SR_WFNRP      BEGSR
025900041123      *
026000041123      * verifico il cliente
026100041124    1c                   if        d69ksc = 9999999  or d69ksc = kcli
026200041123
026300041123      *
026400041123      * verifico se il cliente appartiene ai P.O. che devo elaborare
026500041123      *
026600041123     c                   movel     kcli          w0030
026700041123      *
026800041123     c     w0030         lookup    lnpd69                                 30
026900041123      *
027000041124    2c                   if        %found
027100041123      *
027200041123      * vedo se ho già questo cliente in archivio
027300041123      *
027400041123     c     kwrf          chain     wfnrp00f
027500041124    3c                   if        %found(wfnrp00f)
027600041123      *
027700041123     c                   exsr      sr_calcol
027800041123      *
027900041123     c                   update    wfnrp000
028000041123      *
028100041124   x3c                   else
028200041123      *
028300041123     c                   clear                   wfnrp000
028400041123      * recupero ragione sociale
028500041125     c                   move      DUTKCI        kkcc
028600041123     c     kana          chain     CNACO00f
028700041124    4c                   if        %found(cnaco00f)
028800041123     c                   eval      wnprag = acorag
028900041124    4c                   endif
029000041123      *
029100041123     c                   eval      wnpfil = w0030
029200041123     c                   eval      wnpksc = kcli
029300041124     c                   eval      wnpdda = d69dsd
029400041124     c                   eval      wnpada = d69dsa
029500041123      *
029600041124    4C                   If        luogo = 'S'
029700041125     c                   eval      wnpnot = 'Presente luogo "006"'
029800041125    4c                   endif
029900041123      *
030000041123     c                   exsr      sr_calcol
030100041123      *
030200041123     c                   write     wfnrp000
030300041123
030400041124    3c                   endif
030500041124    2c                   endif
030600041124    1c                   endif
030700041123      *
030800041123     C                   ENDSR
030900041123      *
031000041123      *****************************************************************
031100041123      *   TOTALIZZO GLI INVII
031200041123      *****************************************************************
031300041123     C     SR_calcol     BEGSR
031400041123
031500041123     c                   add       1             wnptot
031600041123     c                   if        invio = 'F'
031700041123     c                   add       1             wnpnrf
031800041123     c                   endif
031900041123     c                   if        invio = 'L'
032000041123     c                   add       1             wnpnrl
032100041123     c                   endif
032200041123     c                   if        invio = 'M'
032300041123     c                   add       1             wnpnrm
032400041123     c                   endif
032500041123
032600041123     c                   endsr
032700041124      *
032800041124      *****************************************************************
032900041124      *   ELABORA FILE DI LAVORO DOPO ORDINAMENTO
033000041124      *****************************************************************
033100041124     C     SR_ELAWRKF    BEGSR
033200041124
033300041124     c                   feod      wfnrp00f
033400041124     C*
033500041124     C/EXEC SQL
033600041124     C+ declare C1 cursor for
033700041124     C+ select * from WFNRP00F
033800041124     C+ where WNPTOT > :D69SUP
033900041124     C+ order by WNPFIL, WNPTOT desc, WNPKSC
034000041124     C+ for fetch only
034100041124     C/END-EXEC
034200041124     C
034300041124     C/EXEC SQL
034400041124     C+ open C1
034500041124     C/END-EXEC
034600041124     C
034700041124     C/EXEC SQL
034800041124     C+ Fetch C1 into :dswfnrp
034900041124     C/END-EXEC
035000041124     C*
035100041124     C                   dow       sqlcod = *zeros
035200041124     C                   exsr      SR_stampa
035300041124     C*
035400041124     C/EXEC SQL
035500041124     C+ Fetch C1 into :dswfnrp
035600041124     C/END-EXEC
035700041124     C*
035800041124     C                   enddo
035900041124     C*
036000041124     C/EXEC SQL
036100041124     C+ close C1
036200041124     C/END-EXEC
036300041124     C*
036400041124      *
036500041124      * stampo i totali generali se > di 0
036600041124      *
036700041124     c                   If        tgtot > 0
036800041124     c                   if        *in66
036900041124      *                 * ------------------ *
037000041124     c                   write     testa
037100041124     c                   write     tesdetta
037200041124      *                 * ------------------ *
037300041124     c                   setoff                                       66
037400041124     c                   endif
037500041124      * stampo totale per P.O.
037600041124     c                   write     totxpo
037700041124     c                   write     totale
037800041124     c                   endif
037900041124      *
038000041124     C                   ENDSR
038100041124      *
038200041124      *****************************************************************
038300041124      *   STAMPA STATISTICA
038400041124      *****************************************************************
038500041124     C     SR_STAMPA     BEGSR
038600041124      *
038700041124      * se è la prima volta stampo la prima pagina
038800041124      *
038900041124     c                   If        prima = 0
039000041124     c                   write     testa
039100041124     c                   write     pripage
039200041124     c                   eval      prima = 1
039300041124     c                   seton                                        66
039400041124     c                   endif
039500041124      *
039600041124      * verifico se rottura di P.O.
039700041124      *
039800041124     c                   if        sav_fil = 0
039900041124     c                   eval      sav_fil = wnpfil
040000041124     c                   endif
040100041124
040200041124     c                   If        sav_fil <> wnpfil
040300041124     c                   if        *in66
040400041124      *                 * ------------------ *
040500041124     c                   write     testa
040600041124     c                   write     tesdetta
040700041124      *                 * ------------------ *
040800041124     c                   setoff                                       66
040900041124     c                   endif
041000041124      * stampo totale per P.O.
041100041124     c                   write     totxpo
041200041124     c                   eval      sav_fil = wnpfil
041300041124
041400041124     c                   clear                   tptot
041500041124
041600041124     c                   clear                   tpnrl
041700041124     c                   clear                   tpnrf
041800041124     c                   clear                   tpnrm
041900041124
042000041124     c                   endif
042100041124      *
042200041124      * elaboro il dettaglio
042300041124      *
042400041124     c                   If        sav_fil = wnpfil
042500041124     c                   movel     wnprag        stprag
042600041124      *
042700041124      * elaboro il dettaglio
042800041124      *
042900041124     c                   add       wnptot        tptot
043000041124     c                   add       wnptot        tgtot
043100041124
043200041124     c                   add       wnpnrl        tpnrl
043300041124     c                   add       wnpnrf        tpnrf
043400041124     c                   add       wnpnrm        tpnrm
043500041124
043600041124     c                   add       wnpnrl        tgnrl
043700041124     c                   add       wnpnrf        tgnrf
043800041124     c                   add       wnpnrm        tgnrm
043900041124     c* DETTAGLIO
044000041124     c                   if        *in66
044100041124      *                 * ------------------ *
044200041124     c                   write     testa
044300041124     c                   write     tesdetta
044400041124      *                 * ------------------ *
044500041124     c                   setoff                                       66
044600041124     c                   endif
044700041124      **  stampa la riga di dettaglio
044800041124     c                   write     dettagli
044900041124
045000041124     c                   endif
045100041124
045200041124     c                   endsr
045300041124      *
045400041123      *****************************************************************
045500041123      *   ROUTINE INIZIALE
045600041123      *****************************************************************
045700041123     C     *INZSR        BEGSR
045800041123      *
045900041123     C     *ENTRY        PLIST
046000041123     C                   PARM                    KPJBA
046100041123      *
046200041123     C                   MOVEL     KPJBU         fidn69ds
046300041123      *
046400041125     c     *dtaara       define    §azute        azuteds
046500041125     c     *dtaara       define    §datiute      ddatiute
046600041125     C                   in(E)     *dtaara
046700041125     C                   IF        %Error  or  RSUT = *blanks
046800041125     C                   call      'TIBS34R'
046900041125     C                   parm                    Tibs34Ds
047000041125     C                   in        *dtaara
047100041125     c                   ENDIF
047200041125      *
047300041125      *  imposta UDATE di elaborazione
047400041125     C                   move      *date         G02DAT
047500041125     C                   MOVE      *ZEROS        G02INV
047600041125     C                   MOVE      *BLANKS       G02ERR
047700041125     C                   CALL      'XSRDA8'
047800041125     C                   PARM                    WLBDA8
047900041125     C                   move      *date         day
048000041125     C                   TIME                    W0140            14 0
048100041125     C                   MOVEl     W0140         ore
048200041125      **  date
048300041125     C                   CLEAR                   WLBDA8
048400041125     C                   move      D69DSD        G02INV
048500041125     C                   MOVE      '3'           G02ERR
048600041125     C                   CALL      'XSRDA8'
048700041125     C                   PARM                    WLBDA8
048800041125     c                   move      G02DAT        dadata
048900041125
049000041125     C                   CLEAR                   WLBDA8
049100041125     C                   move      D69DSA        G02INV
049200041125     C                   MOVE      '3'           G02ERR
049300041125     C                   CALL      'XSRDA8'
049400041125     C                   PARM                    WLBDA8
049500041125     c                   move      G02DAT        adata
049600041123
049700041124      **  prima pagina
049800041124     c                   eval      ksc    = d69ksc
049900041124     c                   eval      dksc   = d69rag
050000041124     c                   eval      lnp01  = d69l01
050100041124     c                   eval      lnp02  = d69l02
050200041124     c                   eval      lnp03  = d69l03
050300041124     c                   eval      lnp04  = d69l04
050400041124     c                   eval      lnp05  = d69l05
050500041124     c                   eval      lnp06  = d69l06
050600041124     c                   eval      lnp07  = d69l07
050700041124     c                   eval      lnp08  = d69l08
050800041124     c                   eval      lnp09  = d69l09
050900041124     c                   eval      lnp10  = d69l10
051000041125     c                   eval      lnp11  = d69l11
051100041125     c                   eval      lnp12  = d69l12
051200041125     c                   eval      lnp13  = d69l13
051300041125     c                   eval      lnp14  = d69l14
051400041125     c                   eval      lnp15  = d69l15
051500041124     c                   eval      tc1d50 = d69t01
051600041124     c                   eval      tc2d50 = d69t02
051700041124     c                   eval      tc3d50 = d69t03
051800041124     c                   eval      sup    = d69sup
051900041123
052000041123     C     Kdcf          klist
052100041123     C                   KFLD                    Kfca
052200041123     C                   KFLD                    D69dsd
052300041123      *
052400041123     C     Kdct          klist
052500041123     C                   KFLD                    DCFaac
052600041123     C                   KFLD                    DCFfil
052700041123     C                   KFLD                    DCFnca
052800041123      *
052900041123     C     KANA          KLIST
053000041123     C                   KFLD                    codut
053100041123     C                   KFLD                    KKCC
053200041123     C                   KFLD                    KCLI
053300041123      *
053400041123     C     KDKA          klist
053500041123     C                   KFLD                    DCFaac
053600041123     C                   KFLD                    DCFfil
053700041123     C                   KFLD                    DCFnca
053800041123     C                   KFLD                    Ktrc
053900041123      *
054000041123     C     Kspe          klist
054100041123     C                   KFLD                    KFLS
054200041123     C                   KFLD                    KCLI
054300041123     C                   KFLD                    KCLU
054400041123      *
054500041123     c     kbol          klist
054600041123     c                   kfld                    DCTaas
054700041123     c                   kfld                    DCTlnp
054800041123     c                   kfld                    DCTnrs
054900041123     c                   kfld                    DCTnsp
055000041123      *
055100041123     c     kwrf          klist
055200041123     c                   kfld                    w0030
055300041123     c                   kfld                    kcli
055400041123      *
055500041123      *
055600041123
055700041123     C                   ENDSR

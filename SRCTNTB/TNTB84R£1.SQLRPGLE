000100040917     H DECEDIT('0,') DATEDIT(*yMd.)
000200110112      * tntb84r *----------------------------------------------------*
000300110112      *         -Tabella "SFR" per statistiche dopo fatt. x Roma
000400940915      *--------------------------------------------------------------*
000500110112     Ftntb84D   CF   E             WORKSTN
000600110112     f                                     sfile(tb84s03 : nrr1)
000700110112     FTNTBE01L  uf a E           k DISK
000800110112     Ftabel00f  if   E           k DISK
000900110112     Fcnaco00f  if   E           k DISK
001000110112     Fcnclp00f  if   E           k DISK
001100020805      *
001200020805      * DEFINIZIONE SCHIERE
001300960302     D*
001400110112     D MSG             S             78    DIM(5) CTDATA PERRCD(1)
001500110112     D ksc             S              7  0 DIM(100)
001600110112     D*
001700110112     D dsfr          E DS
001800110112     D KPJBA         E DS
001900110112     d TIBS02DS      E DS
002000110112      * - Parametri per richiamo al pgm di controllo profilo utenti
002100110112     d TIBS34ds      e ds
002200110112      * - Ds di riferimento al file esterno AzUte00F
002300110112     d AZUTEds       e ds                  ExtName(AzUte00F)
002400110112      * - Ds per dati organigramma
002500110112     d dDatiUte      e ds
002600110112      *
002700080529     d
002800080529     D WLBDAT          DS                  INZ
002900080529     D  G02DAT                 1      8  0
003000080529     D  G02INV                 9     16  0
003100080529     D  G02ERR                17     17
003200080529     D  G02TGI                18     22  0
003300080529     d
003400050103     d Dataiso         s               d   datfmt(*iso)
003500050103     d Dataeur         s               d   datfmt(*eur)
003600080529     d datadmy         s               d   datfmt(*dmy)
003700050103     d
003800080529     d nrr1            s              4  0 inz
003900110120     D kcodt           S                   LIKE(tblcod) inz('01')
004000110120     D kkey            S                   LIKE(tblkey)
004100110120     D kcod            S                   LIKE(tbecod) inz('SFR')
004200110120     D kkey1           S                   LIKE(tbeke1)
004300110112     D kkey2           S                   LIKE(tbeke2)
004400110114     D kkut            S                   LIKE(acokut) inz(1)
004500110112     D kksc            S                   LIKE(acoksc)
004600110127     D savksc          S                   LIKE(v3cksc)
004700110127     D savdksc         S                   LIKE(v3dksc)
004800110127     D savcmm          S                   LIKE(v3ccmm)
004900110127     D savdcmm         S                   LIKE(v3dcmm)
005000110127     D wnrr            S                   LIKE(nrr1)
005100110127     D savcsr          S                   LIKE(c01csr)
005200110114     d c_SflPag        c                   const(17)
005300110112     d wpag            s              6  0
005400110112     d wresto          s              3  0
005500110112     d yy              s              3  0
005600110112     d $Fine           s              1
005700041220     d
005800050103     C**********************************************************************
005900080529     c
006000080529     c     inizio        tag
006100110112     c                   clear                   v1cragr
006200110114     c                   eval      v1cragr='?'
006300040917     c
006400040917     C     FOR01         TAG
006500000828      *
006600110112     C                   EXFMT     tb84D01
006700080529     C                   SETOFF                                       289001
006800080529     c                   clear                   rivisual          1
006900110112     c                   clear                   v1cmsg
007000040917     C* F3 - FINE
007100040917     C   KC              GOTO      FINE
007200080529     c
007300041220      *  CONTROLLI
007400040917     C                   EXSR      CTRD01
007500110112     c
007600080529     c                   if        *in90
007700080529     c                   goto      for01
007800080529     c                   endif
007900110112     c
008000110112     c                   exsr      Carsfl
008100110112     c                   exsr      Gessfl
008200110114     c
008300110114     c                   if        *inkl
008400110114     c                   goto      inizio
008500110114     c                   endif
008600110112     c
008700110112     c                   if        $fine='1'
008800110112     c                   goto      fine
008900110112     c                   else
009000110112     c                   goto      inizio
009100110112     c                   endif
009200080529     c
009300900515      *
009400000000     C     FINE          TAG
009500960129     C*
009600000000     C                   SETON                                        LR
009700080529      *-----------------------------------------------------***********
009800110112      * Caricamento sfl
009900080529      *-----------------------------------------------------***********
010000110112     C     CarSFL        BEGSR
010100080529     C* PULIZIA SFL
010200080529     C                   SETON                                        3031
010300110112     C                   WRITE     tb84C03
010400080529     C                   SETOFF                                       3031
010500080529     c                   clear                   nrr1
010600080529     c                   clear                   totnrr            5 0
010700080529     c
010800110112      * caricamento subfile per codice raggruppamento
010900110112     c
011000110112     c                   eval      kkey1=v1cragr
011100110112     c
011200110112     c     ktbe          setll     tntbe01l
011300110112     c     ktbe          reade(n)  tntbe01l
011400110112     c
011500110112    1c                   dow       not %eof(tntbe01l)
011600110112     c                   if        tbeuni<>*blanks
011700110112     c
011800110112     c                   movel     tbeuni        v3cksc
011900110112     c* Chain su cnaco per la ragione sociale
012000110112    2c                   if        v3cksc>*zeros
012100110112     c                   movel     v3cksc        kksc
012200110112     c     kaco          chain     cnaco00f
012300110112    3c                   if        %found(cnaco00f)
012400110112     c                   movel     acorag        v3dksc
012500110112    3c                   endif
012600110112     c
012700110112     c     kaco          chain     cnclp00f
012800110112    3c                   if        %found(cnclp00f)
012900110112     c                   movel     clpage        v3ccmm
013000110120     c                   movel(p)  v3ccmm        kkey
013100110120     c     ktab          chain     tabel
013200110120     c                   if        %found(tabel00f)
013300110120     c                   movel     tbluni        v3dcmm
013400110120     c                   else
013500110120     c                   movel     *all'?'       v3dcmm
013600110120     c                   endif
013700110120     c
013800110112    3c                   endif
013900110112     c
014000110112    2c                   endif
014100110112     c*
014200110112     c                   add       1             nrr1
014300110112     c                   write     tb84s03
014400110112     c                   endif
014500110112     c
014600110112     c     ktbe          reade(n)  tntbe01l
014700110112     c                   enddo
014800110112     c
014900110112     c                   exsr      righevuote
015000110112     c
015100110112     c                   ENDSR
015200080529      *-----------------------------------------------------***********
015300080529      * controllo data di elaborazione
015400080529      *-----------------------------------------------------***********
015500080529     C     Ctrd01        BEGSR
015600110112     C*
015700110112     c     '?'           scan      v1cragr                                90
015800110112     c                   if        *in90
015900110114     c                   clear                   tibs02ds
016000110112     C                   MOVEL     'R'           t02mod
016100110112     C                   MOVEL     knsif         t02sif
016200110112     C                   MOVEL     'SFR'         t02cod
016300110112      *
016400110112     C                   CALL      'TIBS02R'
016500110112     C                   PARM                    KPJBA
016600110112     C                   PARM                    TIBS02DS
016700110112      *
016800110112     C                   IF        T02err = *BLANKS
016900110114     C                   movel     T02ke2        v1cragr
017000110112     c                   else
017100110112     c                   clear                   v1cragr
017200110112     c                   endif
017300110112     c
017400110112     c                   leavesr
017500110112     c                   endif
017600110112     c
017700110112     c*  controllo
017800110114     C                   CLEAR                   TIBS02DS
017900110112     C                   MOVEL     'C'           t02mod
018000110112     C                   MOVEL     knsif         t02sif
018100110112     C                   MOVEL     'SFR'         t02cod
018200110114     C                   MOVEL     v1cragr       t02ke2
018300110112      *
018400110112     C                   CALL      'TIBS02R'
018500110112     C                   PARM                    KPJBA
018600110112     C                   PARM                    TIBS02DS
018700110112      *
018800110112     C                   IF        T02err<> *BLANKS
018900110112     c                   seton                                        2890
019000110112     c                   eval      v1cmsg=msg(1)
019100110112     c                   leavesr
019200110112     c                   endif
019300110112     c*
019400080529     c                   ENDSR
019500110112      *-----------------------------------------------------***********
019600110120     c*  Carico righe vuote del sfl
019700110112      *-----------------------------------------------------***********
019800110112     c     Righevuote    BEGSR
019900110112     c* Carico le righe vuote fino a riempimento di pagina
020000110112    1c                   if        nrr1>=c_sflpag
020100110112     c                   eval      wpag=%div(nrr1:c_sflPag)
020200110112     c                   eval      wresto=%rem(nrr1:c_sflPag)
020300110112    2c                   if        wresto>0
020400110112     c                   eval      c01csr=nrr1
020500110112     c                   else
020600110112     c                   eval      c01csr=(wpag*c_sflpag)+1
020700110112    2c                   endif
020800110112     c
020900110112   x1c                   else
021000110112     c                   clear                   wpag
021100110112     c                   eval      wresto=nrr1
021200110112     c                   eval      c01csr=nrr1
021300110112    2c                   if        c01csr=0
021400110112     c                   eval      c01csr=1
021500110112    2c                   endif
021600110112    1c                   endif
021700110112     C
021800110112     C                   DOW       wresto<c_sflpag
021900110112     c                   clear                   v3cksc
022000110112     c                   clear                   v3dksc
022100110112     c                   clear                   v3ccmm
022200110112     c                   clear                   v3dcmm
022300110112     c*
022400110112     c                   eval      wresto=wresto+1
022500110112     c                   add       1             nrr1
022600110112     c                   write     tb84s03
022700110112     c                   enddo
022800110112     c
022900110112     c                   z-add     nrr1          totnrr
023000110112     c                   seton                                        33
023100110112     c                   ENDSR
023200110112      *-----------------------------------------------------***********
023300110112     c*  Gestione SFL
023400110112      *-----------------------------------------------------***********
023500110112     c     Gessfl        BEGSR
023600110112     c
023700110114     c                   setoff                                       28
023800110112     c
023900110112     c     for02         tag
024000110114     c                   eval      c01rcd=c01csr
024100110114     c
024200110112     c  n28              write     tb84z03
024300110112     c                   exfmt     tb84c03
024400110112     C                   SETOFF                                       289001
024500110112     C                   SETOFF                                       29
024600110112     c                   clear                   v1cmsg
024700110112     C* F3 - FINE
024800110112     C   KC              eval      $fine='1'
024900110112     c   kc              leavesr
025000110114     C* F12- ritorno 1 videata
025100110114     c   kl              leavesr
025200110127     C* F10- Inserimento riga vuota
025300110127     c                   if        *inkj
025400110127     c                   exsr      InserRiga
025500110127     c                   seton                                        90
025600110127     c                   goto      for02
025700110127     c                   endif
025800110120     c
025900110120     c* ROLLUP, solo se ultima riga piena
026000110120     c                   if        *in37
026100110120     c     totnrr        chain     tb84s03
026200110120     c                   if        %found and  v3cksc>=*zeros
026300110120     c                   eval      nrr1=totnrr
026400110120     c                   exsr      righevuote
026500110120     c                   else
026600110120     c                   seton                                        289029
026700110120     c                   eval      v1cmsg=msg(4)
026800110120     c                   exsr      Aggiosfl
026900110120     c                   endif
027000110120     c                   goto      for02
027100110120     c                   endif
027200110112     c*
027300110112     c                   exsr      ctrd02
027400110112     c
027500110112     c                   if        *in90 or not *inkf
027600110112     c                   goto      for02
027700110112     c                   endif
027800110112     c                   if        *inkf
027900110112     c                   exsr      ScriviTAB
028000110112     c                   endif
028100110112     c
028200110112     c                   ENDSR
028300110112      *-----------------------------------------------------***********
028400110127      * Inserimento riga vuota
028500110112      *-----------------------------------------------------***********
028600110127     C     InserRiga     BEGSR
028700110127     c
028800110127     c* Se ultima riga piena, aggiungo una pagina, poi sposto
028900110127     c                   eval      savcsr=c01csr
029000110127     c
029100110127     c     totnrr        chain     tb84s03
029200110127    1c                   if        %found and  v3cksc>=*zeros
029300110127     c                   eval      nrr1=totnrr
029400110127     c                   exsr      righevuote
029500110127    1c                   endif
029600110127
029700110127     c* sposto la penultima nell'ulitma e così via, fino al posizionamento record
029800110127     c                   eval      nrr1=totnrr-1
029900110127     c
030000110127    1c                   dow       nrr1>savcsr
030100110127     c
030200110127     c     nrr1          chain     tb84s03
030300110127    2c                   if        %found
030400110127     c                   movel     v3cksc        savksc
030500110127     c                   movel     v3dksc        savdksc
030600110127     c                   movel     v3ccmm        savcmm
030700110127     c                   movel     v3dcmm        savdcmm
030800110127     c                   eval      wnrr=nrr1+1
030900110127     c     wnrr          chain     tb84s03
031000110127     c                   clear                   v3cksc
031100110127     c                   clear                   v3dksc
031200110127     c                   clear                   v3ccmm
031300110127     c                   clear                   v3dcmm
031400110127     c                   eval      v3cksc=savksc
031500110127     c                   eval      v3dksc=savdksc
031600110127     c                   eval      v3ccmm=savcmm
031700110127     c                   eval      v3dcmm=savdcmm
031800110127     c                   exsr      Aggiosfl
031900110127    2c                   endif
032000110127     c
032100110127     c                   eval      nrr1=nrr1-2
032200110127     c                   enddo
032300110127     c
032400110127     c* clearo la riga sotto al F10
032500110127     c                   eval      wnrr=nrr1+1
032600110127     c     wnrr          chain     tb84s03
032700110127     c                   clear                   v3cksc
032800110127     c                   clear                   v3dksc
032900110127     c                   clear                   v3ccmm
033000110127     c                   clear                   v3dcmm
033100110127     c                   exsr      Aggiosfl
033200110127     c
033300110127     c                   eval      c01csr=savcsr
033400110127     c                   ENDSR
033500110127     c
033600110127      *-----------------------------------------------------***********
033700110127      * controllo dati sfl
033800110127      *-----------------------------------------------------***********
033900110127     C     ctrd02        BEGSR
034000110112     c*
034100110112     c                   eval      nrr1=1
034200110112     c                   z-add     1             yy
034300110112     c                   clear                   ksc
034400110112     c*
034500110120     c                   dow       nrr1<=totnrr
034600110120     c
034700110112     c     nrr1          chain     tb84s03
034800110120    0c                   if        %found
034900110112     C*
035000110112     c     '?'           scan      v3cksc                                 90
035100110112    1c                   if        *in90
035200110112     c                   movel     rsut          paxdut           30
035300110112     c                   movel(p)  v3dksc        paxdmt           48
035400110112      * PAXSTA=9 ESCLUDO ANNULLATI
035500110112     C                   Z-ADD     9             PAXSTA
035600110112     c                   movel     dutkci        paxccc            4 0
035700110112     c                   clear                   paxdit
035800110112     C                   z-add     1             paxnum
035900110112     C                   CALL      'XALFA3BR'
036000110112     C                   PARM                    PAXDUT
036100110112     C                   PARM      1             CODUT             1 0
036200110112     C                   PARM                    PAXDMT
036300110112     C                   PARM                    PAXCCC
036400110112     C                   PARM                    PAXSTA            1 0
036500110112     C                   PARM                    PAXFLR           90
036600110112     C                   PARM                    PAXDIT            3
036700110112     C                   PARM                    PAXNUM            2 0
036800110112     C                   PARM                    PAXKCM           80
036900110112     C                   PARM                    PAXKSM          140
037000110112     C                   PARM                    PAXKDM           60
037100110112    2C     PAXSTA        IFGT      -1
037200110112     C                   MOVEL     paxksm        v3cksc
037300110112     C                   MOVEL     paxdmt        v3dksc
037400110112     c                   endif
037500110112    2c                   exsr      Aggiosfl
037600110112     c                   leavesr
037700110112    1c                   endif
037800110112     c
037900110112    1c                   if        v3cksc<>*blanks and v3cksc<>*zeros
038000110120     c                   testn                   v3cksc               35
038100110112     c
038200110120    2c                   if        not *in35  or %subst(v3cksc:7:1)<'0'
038300110114     c                   seton                                        29
038400110112     c                   eval      v1cmsg=msg(2)
038500110112     c                   exsr      Aggiosfl
038600110112     c                   leavesr
038700110112    2c                   endif
038800110112     c
038900110112     c                   movel     v3cksc        kksc
039000110112     c     kaco          chain     cnaco00f
039100110112    2c                   if        %found(cnaco00f)
039200110112     c                   movel     acorag        v3dksc
039300110112   x2c                   else
039400110120     c                   seton                                        29
039500110112     c                   eval      v1cmsg=msg(2)
039600110112     c                   exsr      Aggiosfl
039700110112     c                   leavesr
039800110112    2c                   endif
039900110112     c
040000110112     c*  Verifico se già presente
040100110114     c     kksc          lookup    ksc                                    29
040200110114     c                   if        *in29
040300110112     c                   eval      v1cmsg=msg(3)
040400110112     c                   exsr      Aggiosfl
040500110112     c                   leavesr
040600110112     c                   else
040700110112     c                   eval      ksc(yy)=kksc
040800110112     c                   eval      yy=yy+1
040900110112    2c                   endif
041000110112     c
041100110112     c
041200110112     c     kaco          chain     cnclp00f
041300110112    2c                   if        %found(cnclp00f)
041400110112     c                   movel     clpage        v3ccmm
041500110120     c                   movel(p)  v3ccmm        kkey
041600110120     c     ktab          chain     tabel
041700110120     c                   if        %found(tabel00f)
041800110120     c                   movel     tbluni        v3dcmm
041900110120     c                   else
042000110120     c                   movel     *all'?'       v3dcmm
042100110120     c                   endif
042200110112    2c                   endif
042300110114     c*
042400110114     c                   exsr      Aggiosfl
042500110112     c*
042600110112     c                   else
042700110112     c                   clear                   v3dksc
042800110112     c                   clear                   v3dcmm
042900110112     c                   clear                   v3ccmm
043000110112     c                   exsr      Aggiosfl
043100110112    1c                   endif
043200110120     c
043300110120    1c                   endif
043400110112     c
043500110112     c                   eval      nrr1=1+nrr1
043600110112     c*
043700110112     c                   enddo
043800110120     c                   z-add     1             nrr1
043900110112     c
044000110112     c                   ENDSR
044100110112      *-----------------------------------------------------***********
044200110112      * Aggiornamento tabella
044300110112      *-----------------------------------------------------***********
044400110112     C     ScriviTAB     BEGSR
044500110112     c*
044600110112     c* Intanto cancello tutti i record
044700110112     c
044800110112     c/exec sql
044900110112     c+ delete from tntbe00f where tbecod='SFR' and tbeke2<>'               '
045000110112     c+ and tbeke1=:v1cragr
045100110112     c/end-exec sql
045200110112     c
045300110112     c
045400110112     c                   eval      nrr1=1
045500110112     c                   z-add     1             yy
045600110112     c                   clear                   ksc
045700110112     c*
045800110112     c     nrr1          chain     tb84s03
045900110112    0c                   dow       %found
046000110112     c
046100110112    1c                   if        v3cksc>*zeros
046200110112     c                   clear                   tntbe000
046300110112     c                   clear                   dsfr
046400110112     c                   eval      tbecod='SFR'
046500110112     c                   eval      tbeapl='ST'
046600110112     c                   eval      tbeke1=v1cragr
046700110112     c                   movel     yy            tbeke2
046800110112     c                   eval      §sfrdksc=v3dksc
046900110112     c                   movel     v3cksc        §sfrksc
047000110112     c                   movel     dsfr          tbeuni
047100110112     c                   write     tntbe000
047200110114     c                   eval      yy=yy+1
047300110112     c                   endif
047400110112     c
047500110112     c                   eval      nrr1=nrr1+1
047600110112     c     nrr1          chain     tb84s03
047700110112     c                   enddo
047800110112     c                   ENDSR
047900110112      *****************************************************************
048000110112     c* Aggioranemnto sfl
048100110112      *****************************************************************
048200110112     C     Aggiosfl      BEGSR
048300110112     c                   if        v1cmsg<>*blanks
048400110112     c                   seton                                        9028
048500110112     c                   eval      c01csr=nrr1
048600110112     c                   endif
048700110112     c
048800110112     c                   update    tb84s03
048900110112     c                   setoff                                       29
049000110112     c
049100110112     c                   ENDSR
049200110112      *****************************************************************
049300110112     C     *INZSR        BEGSR
049400110112      *
049500110112     C     *ENTRY        PLIST
049600110112     C                   PARM                    KPJBA
049700110112      *
049800110112      *
049900110112      * accesso tntbe
050000110112     c     ktbe2         klist
050100110112     C                   kfld                    kcod
050200110112     C                   kfld                    kkey1
050300110112     C                   kfld                    kkey2
050400110112     c     ktbe          klist
050500110112     C                   kfld                    kcod
050600110112     C                   kfld                    kkey1
050700110120     c     ktab          klist
050800110120     C                   kfld                    kkut
050900110120     C                   kfld                    kcodt
051000110120     C                   kfld                    kkey
051100110112     c     kaco          klist
051200110112     C                   kfld                    kkut
051300110112     C                   kfld                    dutkci
051400110112     C                   kfld                    kksc
051500110112     c
051600110112     c                   exsr      sr_DatiJob
051700110112     c*
051800110112      *
051900110112     C                   ENDSR
052000110112      *---------------------------------------------------------------*
052100110112      *?Reperimento dati del job (Utente/Operativi)                  ?*
052200110112      *---------------------------------------------------------------*
052300110112     c     sr_DatiJob    BEGSR
052400110112      *
052500110112     c     *dtaara       define    §azute        AZUTEds
052600110112     c     *dtaara       define    §datiute      dDatiUte
052700110112      *
052800110112     c                   in(E)     *dtaara
052900110112     c                   IF        %Error or RSUT = *blanks
053000110112     c                   clear                   TIBS34ds
053100110112     c                   call      'TIBS34R'
053200110112     c                   parm                    TIBS34ds
053300110112     c                   in        *dtaara
053400110112     c                   ENDIF
053500110112      *
053600110112     c                   ENDSR
053700020805      **************************************************************************
053800960302** SCHIERA MSG - MESSAGGI DI ERRORE
053900110112Raggruppamento errato
054000110112Codice cliente errato o inesistente
054100110112Codice cliente già inserito
054200110120Scorrimento non ammesso: ci sono ancora righe vuote nella pagina

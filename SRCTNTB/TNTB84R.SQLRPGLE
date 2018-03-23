000100140613     /*PRM  dbgview(*source)
000200140613     /*END
000300140613      *--------------------------------------------------------------*
000400040917     H DECEDIT('0,') DATEDIT(*yMd.)
000500140613     h option(*nodebugio)
000600110112      * tntb84r *----------------------------------------------------*
000700110112      *         -Tabella "SFR" per statistiche dopo fatt. x Roma
000800940915      *--------------------------------------------------------------*
000900110112     Ftntb84D   CF   E             WORKSTN
001000110112     f                                     sfile(tb84s03 : nrr1)
001100110112     FTNTBE01L  uf a E           k DISK
001200140613     fAZCMM01L  if   e           k disk
001300110112     Fcnaco00f  if   E           k DISK
001400110112     Fcnclp00f  if   E           k DISK
001500140613      *--------------------------------------------------------------*
001600020805      * DEFINIZIONE SCHIERE
001700960302     D*
001800110112     D MSG             S             78    DIM(5) CTDATA PERRCD(1)
001900140925     D ksc             S              7  0 DIM(700)
002000140613      *--------------------------------------------------------------*
002100110112     D*
002200110112     D dsfr          E DS
002300110112     D KPJBA         E DS
002400110112     d TIBS02DS      E DS
002500110112      * - Parametri per richiamo al pgm di controllo profilo utenti
002600110112     d TIBS34ds      e ds
002700110112      * - Ds di riferimento al file esterno AzUte00F
002800110112     d AZUTEds       e ds                  ExtName(AzUte00F)
002900110112      * - Ds per dati organigramma
003000110112     d dDatiUte      e ds
003100110112      *
003200080529     d
003300080529     D WLBDAT          DS                  INZ
003400080529     D  G02DAT                 1      8  0
003500080529     D  G02INV                 9     16  0
003600080529     D  G02ERR                17     17
003700080529     D  G02TGI                18     22  0
003800140613      *--------------------------------------------------------------*
003900050103     d Dataiso         s               d   datfmt(*iso)
004000050103     d Dataeur         s               d   datfmt(*eur)
004100080529     d datadmy         s               d   datfmt(*dmy)
004200050103     d
004300080529     d nrr1            s              4  0 inz
004400110120     D kcod            S                   LIKE(tbecod) inz('SFR')
004500110120     D kkey1           S                   LIKE(tbeke1)
004600110112     D kkey2           S                   LIKE(tbeke2)
004700110114     D kkut            S                   LIKE(acokut) inz(1)
004800110112     D kksc            S                   LIKE(acoksc)
004900110127     D savksc          S                   LIKE(v3cksc)
005000110127     D savdksc         S                   LIKE(v3dksc)
005100110127     D savcmm          S                   LIKE(v3ccmm)
005200110127     D savdcmm         S                   LIKE(v3dcmm)
005300110127     D wnrr            S                   LIKE(nrr1)
005400110127     D savcsr          S                   LIKE(c01csr)
005500110114     d c_SflPag        c                   const(17)
005600110112     d wpag            s              6  0
005700110112     d wresto          s              3  0
005800110112     d yy              s              3  0
005900110112     d $Fine           s              1
006000041220     d
006100050103     C**********************************************************************
006200080529     c
006300080529     c     inizio        tag
006400110112     c                   clear                   v1cragr
006500110114     c                   eval      v1cragr='?'
006600040917     c
006700040917     C     FOR01         TAG
006800000828      *
006900110112     C                   EXFMT     tb84D01
007000080529     C                   SETOFF                                       289001
007100080529     c                   clear                   rivisual          1
007200110112     c                   clear                   v1cmsg
007300040917     C* F3 - FINE
007400040917     C   KC              GOTO      FINE
007500080529     c
007600041220      *  CONTROLLI
007700040917     C                   EXSR      CTRD01
007800110112     c
007900080529     c                   if        *in90
008000080529     c                   goto      for01
008100080529     c                   endif
008200110112     c
008300110112     c                   exsr      Carsfl
008400110112     c                   exsr      Gessfl
008500110114     c
008600110114     c                   if        *inkl
008700110114     c                   goto      inizio
008800110114     c                   endif
008900110112     c
009000110112     c                   if        $fine='1'
009100110112     c                   goto      fine
009200110112     c                   else
009300110112     c                   goto      inizio
009400110112     c                   endif
009500080529     c
009600900515      *
009700000000     C     FINE          TAG
009800960129     C*
009900000000     C                   SETON                                        LR
010000080529      *-----------------------------------------------------***********
010100110112      * Caricamento sfl
010200080529      *-----------------------------------------------------***********
010300110112     C     CarSFL        BEGSR
010400080529     C* PULIZIA SFL
010500080529     C                   SETON                                        3031
010600110112     C                   WRITE     tb84C03
010700080529     C                   SETOFF                                       3031
010800080529     c                   clear                   nrr1
010900080529     c                   clear                   totnrr            5 0
011000080529     c
011100110112      * caricamento subfile per codice raggruppamento
011200110112     c
011300110112     c                   eval      kkey1=v1cragr
011400110112     c
011500110112     c     ktbe          setll     tntbe01l
011600110112     c     ktbe          reade(n)  tntbe01l
011700110112     c
011800110112    1c                   dow       not %eof(tntbe01l)
011900110112     c                   if        tbeuni<>*blanks
012000110112     c
012100110112     c                   movel     tbeuni        v3cksc
012200110112     c* Chain su cnaco per la ragione sociale
012300110112    2c                   if        v3cksc>*zeros
012400110112     c                   movel     v3cksc        kksc
012500110112     c     kaco          chain     cnaco00f
012600110112    3c                   if        %found(cnaco00f)
012700110112     c                   movel     acorag        v3dksc
012800110112    3c                   endif
012900110112     c
013000110112     c     kaco          chain     cnclp00f
013100110112    3c                   if        %found(cnclp00f)
013200110112     c                   movel     clpage        v3ccmm
013300140613     C     CLPage        chain     AZCMM000
013400140613     c                   if        %found(AZCMM01L)
013500140613     C                   movel     CMMdes        V3Dcmm
013600110120     c                   else
013700110120     c                   movel     *all'?'       v3dcmm
013800110120     c                   endif
013900110112    3c                   endif
014000110112     c
014100110112    2c                   endif
014200110112     c*
014300110112     c                   add       1             nrr1
014400110112     c                   write     tb84s03
014500110112     c                   endif
014600110112     c
014700110112     c     ktbe          reade(n)  tntbe01l
014800110112     c                   enddo
014900110112     c
015000110112     c                   exsr      righevuote
015100110112     c
015200110112     c                   ENDSR
015300080529      *-----------------------------------------------------***********
015400080529      * controllo data di elaborazione
015500080529      *-----------------------------------------------------***********
015600080529     C     Ctrd01        BEGSR
015700110112     C*
015800110112     c     '?'           scan      v1cragr                                90
015900110112     c                   if        *in90
016000110114     c                   clear                   tibs02ds
016100110112     C                   MOVEL     'R'           t02mod
016200110112     C                   MOVEL     knsif         t02sif
016300110112     C                   MOVEL     'SFR'         t02cod
016400110112      *
016500110112     C                   CALL      'TIBS02R'
016600110112     C                   PARM                    KPJBA
016700110112     C                   PARM                    TIBS02DS
016800110112      *
016900110112     C                   IF        T02err = *BLANKS
017000110114     C                   movel     T02ke2        v1cragr
017100110112     c                   else
017200110112     c                   clear                   v1cragr
017300110112     c                   endif
017400110112     c
017500110112     c                   leavesr
017600110112     c                   endif
017700110112     c
017800110112     c*  controllo
017900110114     C                   CLEAR                   TIBS02DS
018000110112     C                   MOVEL     'C'           t02mod
018100110112     C                   MOVEL     knsif         t02sif
018200110112     C                   MOVEL     'SFR'         t02cod
018300110114     C                   MOVEL     v1cragr       t02ke2
018400110112      *
018500110112     C                   CALL      'TIBS02R'
018600110112     C                   PARM                    KPJBA
018700110112     C                   PARM                    TIBS02DS
018800110112      *
018900110112     C                   IF        T02err<> *BLANKS
019000110112     c                   seton                                        2890
019100110112     c                   eval      v1cmsg=msg(1)
019200110112     c                   leavesr
019300110112     c                   endif
019400110112     c*
019500080529     c                   ENDSR
019600110112      *-----------------------------------------------------***********
019700110120     c*  Carico righe vuote del sfl
019800110112      *-----------------------------------------------------***********
019900110112     c     Righevuote    BEGSR
020000110112     c* Carico le righe vuote fino a riempimento di pagina
020100110112    1c                   if        nrr1>=c_sflpag
020200110112     c                   eval      wpag=%div(nrr1:c_sflPag)
020300110112     c                   eval      wresto=%rem(nrr1:c_sflPag)
020400110112    2c                   if        wresto>0
020500110112     c                   eval      c01csr=nrr1
020600110112     c                   else
020700110112     c                   eval      c01csr=(wpag*c_sflpag)+1
020800110112    2c                   endif
020900110112     c
021000110112   x1c                   else
021100110112     c                   clear                   wpag
021200110112     c                   eval      wresto=nrr1
021300110112     c                   eval      c01csr=nrr1
021400110112    2c                   if        c01csr=0
021500110112     c                   eval      c01csr=1
021600110112    2c                   endif
021700110112    1c                   endif
021800110112     C
021900110112     C                   DOW       wresto<c_sflpag
022000110112     c                   clear                   v3cksc
022100110112     c                   clear                   v3dksc
022200110112     c                   clear                   v3ccmm
022300110112     c                   clear                   v3dcmm
022400110112     c*
022500110112     c                   eval      wresto=wresto+1
022600110112     c                   add       1             nrr1
022700110112     c                   write     tb84s03
022800110112     c                   enddo
022900110112     c
023000110112     c                   z-add     nrr1          totnrr
023100110112     c                   seton                                        33
023200110112     c                   ENDSR
023300110112      *-----------------------------------------------------***********
023400110112     c*  Gestione SFL
023500110112      *-----------------------------------------------------***********
023600110112     c     Gessfl        BEGSR
023700110112     c
023800110114     c                   setoff                                       28
023900110112     c
024000110112     c     for02         tag
024100110114     c                   eval      c01rcd=c01csr
024200110114     c
024300110112     c  n28              write     tb84z03
024400110112     c                   exfmt     tb84c03
024500110112     C                   SETOFF                                       289001
024600110112     C                   SETOFF                                       29
024700110112     c                   clear                   v1cmsg
024800110112     C* F3 - FINE
024900110112     C   KC              eval      $fine='1'
025000110112     c   kc              leavesr
025100110114     C* F12- ritorno 1 videata
025200110114     c   kl              leavesr
025300110127     C* F10- Inserimento riga vuota
025400110127     c                   if        *inkj
025500110127     c                   exsr      InserRiga
025600110127     c                   seton                                        90
025700110127     c                   goto      for02
025800110127     c                   endif
025900110120     c
026000110120     c* ROLLUP, solo se ultima riga piena
026100110120     c                   if        *in37
026200110120     c     totnrr        chain     tb84s03
026300110120     c                   if        %found and  v3cksc>=*zeros
026400110120     c                   eval      nrr1=totnrr
026500110120     c                   exsr      righevuote
026600110120     c                   else
026700110120     c                   seton                                        289029
026800110120     c                   eval      v1cmsg=msg(4)
026900110120     c                   exsr      Aggiosfl
027000110120     c                   endif
027100110120     c                   goto      for02
027200110120     c                   endif
027300110112     c*
027400110112     c                   exsr      ctrd02
027500110112     c
027600110112     c                   if        *in90 or not *inkf
027700110112     c                   goto      for02
027800110112     c                   endif
027900110112     c                   if        *inkf
028000110112     c                   exsr      ScriviTAB
028100110112     c                   endif
028200110112     c
028300110112     c                   ENDSR
028400110112      *-----------------------------------------------------***********
028500110127      * Inserimento riga vuota
028600110112      *-----------------------------------------------------***********
028700110127     C     InserRiga     BEGSR
028800110127     c
028900110127     c* Se ultima riga piena, aggiungo una pagina, poi sposto
029000110127     c                   eval      savcsr=c01csr
029100110127     c
029200110127     c     totnrr        chain     tb84s03
029300110127    1c                   if        %found and  v3cksc>=*zeros
029400110127     c                   eval      nrr1=totnrr
029500110127     c                   exsr      righevuote
029600110127    1c                   endif
029700110127
029800110127     c* sposto la penultima nell'ulitma e così via, fino al posizionamento record
029900110127     c                   eval      nrr1=totnrr-1
030000110127     c
030100110127    1c                   dow       nrr1>savcsr
030200110127     c
030300110127     c     nrr1          chain     tb84s03
030400110127    2c                   if        %found
030500110127     c                   movel     v3cksc        savksc
030600110127     c                   movel     v3dksc        savdksc
030700110127     c                   movel     v3ccmm        savcmm
030800110127     c                   movel     v3dcmm        savdcmm
030900110127     c                   eval      wnrr=nrr1+1
031000110127     c     wnrr          chain     tb84s03
031100110127     c                   clear                   v3cksc
031200110127     c                   clear                   v3dksc
031300110127     c                   clear                   v3ccmm
031400110127     c                   clear                   v3dcmm
031500110127     c                   eval      v3cksc=savksc
031600110127     c                   eval      v3dksc=savdksc
031700110127     c                   eval      v3ccmm=savcmm
031800110127     c                   eval      v3dcmm=savdcmm
031900110127     c                   exsr      Aggiosfl
032000110127    2c                   endif
032100110127     c
032200110127     c                   eval      nrr1=nrr1-2
032300110127     c                   enddo
032400110127     c
032500110127     c* clearo la riga sotto al F10
032600110127     c                   eval      wnrr=nrr1+1
032700110127     c     wnrr          chain     tb84s03
032800110127     c                   clear                   v3cksc
032900110127     c                   clear                   v3dksc
033000110127     c                   clear                   v3ccmm
033100110127     c                   clear                   v3dcmm
033200110127     c                   exsr      Aggiosfl
033300110127     c
033400110127     c                   eval      c01csr=savcsr
033500110127     c                   ENDSR
033600110127     c
033700110127      *-----------------------------------------------------***********
033800110127      * controllo dati sfl
033900110127      *-----------------------------------------------------***********
034000110127     C     ctrd02        BEGSR
034100110112     c*
034200110112     c                   eval      nrr1=1
034300110112     c                   z-add     1             yy
034400110112     c                   clear                   ksc
034500110112     c*
034600110120     c                   dow       nrr1<=totnrr
034700110120     c
034800110112     c     nrr1          chain     tb84s03
034900110120    0c                   if        %found
035000110112     C*
035100110112     c     '?'           scan      v3cksc                                 90
035200110112    1c                   if        *in90
035300110112     c                   movel     rsut          paxdut           30
035400110112     c                   movel(p)  v3dksc        paxdmt           48
035500110112      * PAXSTA=9 ESCLUDO ANNULLATI
035600110112     C                   Z-ADD     9             PAXSTA
035700110112     c                   movel     dutkci        paxccc            4 0
035800110112     c                   clear                   paxdit
035900110112     C                   z-add     1             paxnum
036000110112     C                   CALL      'XALFA3BR'
036100110112     C                   PARM                    PAXDUT
036200110112     C                   PARM      1             CODUT             1 0
036300110112     C                   PARM                    PAXDMT
036400110112     C                   PARM                    PAXCCC
036500110112     C                   PARM                    PAXSTA            1 0
036600110112     C                   PARM                    PAXFLR           90
036700110112     C                   PARM                    PAXDIT            3
036800110112     C                   PARM                    PAXNUM            2 0
036900110112     C                   PARM                    PAXKCM           80
037000110112     C                   PARM                    PAXKSM          140
037100110112     C                   PARM                    PAXKDM           60
037200110112    2C     PAXSTA        IFGT      -1
037300110112     C                   MOVEL     paxksm        v3cksc
037400110112     C                   MOVEL     paxdmt        v3dksc
037500110112     c                   endif
037600110112    2c                   exsr      Aggiosfl
037700110112     c                   leavesr
037800110112    1c                   endif
037900110112     c
038000110112    1c                   if        v3cksc<>*blanks and v3cksc<>*zeros
038100110120     c                   testn                   v3cksc               35
038200110112     c
038300110120    2c                   if        not *in35  or %subst(v3cksc:7:1)<'0'
038400110114     c                   seton                                        29
038500110112     c                   eval      v1cmsg=msg(2)
038600110112     c                   exsr      Aggiosfl
038700110112     c                   leavesr
038800110112    2c                   endif
038900110112     c
039000110112     c                   movel     v3cksc        kksc
039100110112     c     kaco          chain     cnaco00f
039200110112    2c                   if        %found(cnaco00f)
039300110112     c                   movel     acorag        v3dksc
039400110112   x2c                   else
039500110120     c                   seton                                        29
039600110112     c                   eval      v1cmsg=msg(2)
039700110112     c                   exsr      Aggiosfl
039800110112     c                   leavesr
039900110112    2c                   endif
040000110112     c
040100110112     c*  Verifico se già presente
040200110114     c     kksc          lookup    ksc                                    29
040300110114     c                   if        *in29
040400110112     c                   eval      v1cmsg=msg(3)
040500110112     c                   exsr      Aggiosfl
040600110112     c                   leavesr
040700110112     c                   else
040800110112     c                   eval      ksc(yy)=kksc
040900110112     c                   eval      yy=yy+1
041000110112    2c                   endif
041100110112     c
041200110112     c
041300110112     c     kaco          chain     cnclp00f
041400110112    2c                   if        %found(cnclp00f)
041500110112     c                   movel     clpage        v3ccmm
041600140613     C     CLPage        chain     AZCMM000
041700140613     c                   if        %found(AZCMM01L)
041800140613     C                   movel     CMMdes        V3Dcmm
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
050700110112     c     kaco          klist
050800110112     C                   kfld                    kkut
050900110112     C                   kfld                    dutkci
051000110112     C                   kfld                    kksc
051100110112     c
051200110112     c                   exsr      sr_DatiJob
051300110112     c*
051400110112      *
051500110112     C                   ENDSR
051600110112      *---------------------------------------------------------------*
051700110112      *?Reperimento dati del job (Utente/Operativi)                  ?*
051800110112      *---------------------------------------------------------------*
051900110112     c     sr_DatiJob    BEGSR
052000110112      *
052100110112     c     *dtaara       define    §azute        AZUTEds
052200110112     c     *dtaara       define    §datiute      dDatiUte
052300110112      *
052400110112     c                   in(E)     *dtaara
052500110112     c                   IF        %Error or RSUT = *blanks
052600110112     c                   clear                   TIBS34ds
052700110112     c                   call      'TIBS34R'
052800110112     c                   parm                    TIBS34ds
052900110112     c                   in        *dtaara
053000110112     c                   ENDIF
053100110112      *
053200110112     c                   ENDSR
053300020805      **************************************************************************
053400960302** SCHIERA MSG - MESSAGGI DI ERRORE
053500110112Raggruppamento errato
053600110112Codice cliente errato o inesistente
053700110112Codice cliente già inserito
053800110120Scorrimento non ammesso: ci sono ancora righe vuote nella pagina

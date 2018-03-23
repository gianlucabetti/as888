000100941109     H*------------------------------------------------------------------------*
000200150924     H* Controalert : scrittura file spia                                      *
000300941109     H*------------------------------------------------------------------------*
000400930128     H DECEDIT('0,') DATEDIT(*DMY.)
000500140805     H DFTACTGRP(*NO) ACTGRP('BARTVAS')
000600941109     F*------------------------------------------------------------------------*
000700150922     ffnbrv03l  if   e           k disk
000800150922     ffnanm02l  if   e           k disk
000900150922     ftabel00f  if   e           k disk
001000150928     fTNTBE01L  uF a E           K DISK
001100150922     Ffiar505l  IF   E           K DISK
001200150928     Ffiar501l  iF   E           K DISK    rename(fiar5000:fiar5001)
001300060828     FFNARB01L  IF   E           K DISK
001400150924     FFNart01L  IF   E           K DISK
001500150924     Ffiar401l  IF   E           K DISK
001600150924     Ffisia01l  if   e           k disk    rename(fisia000:sia001) infds(fisia)
001700150928     FFIsia00F  if a E             DISK
001800070328     FFNLBL02L  IF   E           K DISK
001900070328     FFNLBL01L  IF   E           K DISK
002000070328     F                                     RENAME(FNlbl000:FNlbl001)
002100150928     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002200070124     d
002300070124     D REC             S              2    DIM(50)
002400060828     d
002500150914     d* DS ricevuta
002600150914     d FNLRc4DS      E ds
002700150928     D  tcal                  12     21    dim(10)
002800150928     D  riccal                12     21
002900150924     d
003000150924     d fisia           ds
003100150924     d  rrnsia               397    400b 0
003200060831     d
003300060828     D KPJBA         E DS
003400150924     D trulorsds     E DS                  INZ
003500150924     D trulor2ds     E DS                  INZ
003600150924     D trulor3ds     E DS                  INZ
003700060829     D TRUL33DS      E DS
003800150922     D tibs02ds      E DS
003900150922     D fnlv55ds      E DS
004000150922     D fnlv53ds      E DS
004100150924     D fnlv81ds      E DS
004200150922     D tnsd99ds      E DS
004300060828     D trul06ds      E DS
004400060828     D  L6                     1     90  0 dim(30)
004500110629
004600060828      * ds per il controllo della presenza di CA per la spedizione richiesta
004700060828     d FIDN12DS      e ds
004800060828     d  skKey                 26    305    dim(20)
004900060828     d  skAnn                306    325    dim(20)
005000060828     d  skDca                326    485  0 dim(20)
005100060828     d  skFca                486    545  0 dim(20)
005200060828     d  skDch                546    705  0 dim(20)
005300060828     d  skCch                706    745    dim(20)
005400060828
005500060828     d dsKey           ds
005600060828     d  dsaac                         4  0
005700060828     d  dsfil                         3  0
005800060828     d  dsnca                         7  0
005900060828      *
006000070124     D DS3A          E DS
006100150922     D Dvpo          E DS
006200150928     D DSDF          E DS
006300150924     D DSIAICA       E DS
006400150924     D Diore01       E DS                  INZ
006500150928     D Dar5emd       E DS                  INZ
006600060424     d
006700060828     d II              s              2  0 inz(0)
006800060828     d xx              s              3  0 inz(0)
006900060828     d yy              s              3  0 inz(0)
007000150924     d indx            s              3  0
007100150924     d KFIL            s              3  0
007200150925     d Kdat            s              8    inz
007300150922     d kica            s              1    inz(' ')
007400060828     d Escbolla        s              1
007500080116     d POL6            s              1
007600150928
007700150928     d kTbeCod         s                   Like(TbeCod)
007800150928     d kTbeKe1         s                   Like(TbeKe1)
007900150924     D ktrc            S                   LIKE(ar4trc) inz('X')
008000150925     D ktrd            S                   LIKE(ar5trd) inz('EMD')
008100150924     D TIPA            S                   LIKE(siatipa)
008200150924     D knpg            S                   LIKE(brvnpg)
008300150922     D kcaa            S                   LIKE(anmcaa)
008400150922     D Wtbecod         S                   LIKE(TBEcod)
008500060829     d Dataiso         s               d   datfmt(*iso)
008600060829     d Dataeur         s               d   datfmt(*eur)
008700070124     d ktbkut          s                   like(tblkut) inz(1)
008800070124     d ktbcod          s                   like(tblcod)
008900941109     C*------------------------------------------------------------------------*
009000060424     c     *entry        plist
009100060424     c                   parm                    kpjba
009200150914     c                   movel     kpjbu         fnlrc4ds
009300150922     c*
009400150922     c* non elaboro controalert 1 e 2 se passato orario limite
009500150925     c                   if        §VPOHLCOA>*zeros and §VPOHLCOA<'2400'
009600150922     c                   movel     w0140         uora              4 0
009700150925     c                   if        %editc(uora:'X') >§VPOHLCOA
009800150922     c*  '1' - IMP
009900150922     c                   eval      indx=%lookup('1':tcal)
010000150922     c                   if        indx>0
010100150922     c                   clear                   tcal(indx)
010200150922     c                   endif
010300150922     c*  '2' - DISGUIDO
010400150922     c                   eval      indx=%lookup('2':tcal)
010500150922     c                   if        indx>0
010600150922     c                   clear                   tcal(indx)
010700150922     c                   endif
010800150922     c                   endif
010900150922     c                   endif
011000070529     c
011100150922     c* se nessun controalert da elaborare --> esco
011200150924     c                   if        riccal<>*blanks
011300080115     c
011400080115     c* Verifico se è un p.o. di £6
011500080115     c                   clear                   fnlv55ds
011600150914     c                   movel     ilrc4lna      d55lin
011700150914     c                   movel     ilrc4dat      d55drf
011800080115     c                   movel     '6'           d55tpt
011900080115     c                   movel     'L'           d55tla
012000080115     c                   call      'FNLV55R'
012100080115     c                   parm                    fnlv55ds
012200080115     c
012300150928     c                   clear                   pol6
012400150928     c                   if        d55tfa<>ilrc4lna
012500150928     c                   eval      pol6='S'
012600150928     c                   endif
012700070406     c*
012800080115     c* Carico la £6 alla data di elaborazione
012900060828     c                   clear                   trul06ds
013000150924     c                   movel     IlRc4LNA      d06key
013100080115     c                   move      'S'           d06key
013200060828     c                   movel     '£6'          d06cod
013300150922     c                   movel     'L'           d06tla
013400150924     c                   movel     ilrc4dat      d06drf
013500080115     c                   movel     trul06ds      kpjbu
013600060828     c                   call      'TRUL06R'
013700060828     c                   parm                    kpjba
013800060828     c                   movel     kpjbu         trul06ds
013900150922     c
014000150922     c* Lettura file fiar505l alert ancora da inviare
014100150925     c                   eval      kdat=%editc(ilrc4dat:'X')
014200150922     c
014300150922     c     kar5          setll     fiar505l
014400150922     c     kar5          reade     fiar505l
014500070529     c
014600150922    6c                   dow       not %eof(fiar505l)
014700150922     c     karb          chain     fnarb01l
014800150922     c                   if        %found(fnarb01l)
014900060828     c                   exsr      ESCLUDI
015000101119     c
015100150922     C                   IF        ESCBOLLA=' '
015200060828     c                   exsr      ELABORA
015300070529     c                   endif
015400150922     c                   endif
015500150922
015600150922     c     kar5          reade     fiar505l
015700150922     c                   endDO
015800080116     c*
015900150922     c                   endif
016000150928     c
016100150928     c* e ho elaborato il controalert "3" allora aggiorno la tabella SDF
016200150928    1c                   if        ilrc4lna>0  and %lookup('3':tcal)>0
016300150928     c                             and pol6=' '
016400150928     c                   Eval      kTbeCod = 'SDF'
016500150928     c                   Movel(p)  ilrc4lna      kTbeKe1
016600150928     c     Ktbe          Chain     Tntbe01l
016700150928    2c                   If        %Found(Tntbe01l)
016800150928     c                   Movel     TbeUni        dSdf
016900150930     c                   monitor
017000150928    3c                   If        §SDFCA3<ilrc4dat
017100150928     c                   Z-Add     ilrc4dat      §SDFCA3
017200150928    3c                   EndIf
017300150930
017400150930     c                   on-error
017500150930     c                   Z-Add     ilrc4dat      §SDFCA3
017600150930     c                   endmon
017700150930
017800150928     c                   Movel     dSdf          TbeUni
017900150928     c                   Update    Tntbe000
018000150928   x2c                   else
018100150928
018200150928     c                   clear                   tntbe000
018300150928     c                   eval      tbeapl='OP'
018400150928     c                   eval      tbecod='SDF'
018500150928     c                   eval      tbeke1=%editc(ilrc4lna:'X')
018600150928     c                   clear                   dsdf
018700150928     c                   Z-Add     ilrc4dat      §SDFCA3
018800150928     c                   Movel     dSdf          TbeUni
018900150928     c                   write     Tntbe000
019000150928     C                   ENDIF
019100150928     c                   endif
019200150928     c
019300150922
019400150922     c                   clear                   fidn12ds
019500150922     c                   eval      i12tla = 'C'
019600150922     c                   call      'FIDN12R'
019700150922     c                   parm                    fidn12ds
019800150922     c                   clear                   fnlv53ds
019900150922     c                   eval      d53tla = 'C'
020000150922     c                   call      'FNLV53R'
020100150922     c                   parm                    fidn12ds
020200150922     c                   clear                   tnsd99ds
020300150922     c                   eval      d98tla = 'C'
020400150922     c                   call      'FNLV53R'
020500150922     c                   parm                    tnsd99ds
020600150925     c                   clear                   fnlv81ds
020700150925     c                   eval      ilv81tla = 'C'
020800150925     c                   call      'FNLV81R'
020900150925     c                   parm                    fnlv81ds
021000060829     c
021100000000     C                   SETON                                        LR
021200060828     c*------------------------------------------------------------------
021300060828     c     *INZSR        BEGSR
021400150922     c     Kar5          klist
021500150922     c                   kfld                    kdat
021600150922     c                   kfld                    kica
021700150928     c     Ktbe          Klist
021800150928     c                   Kfld                    kTbeCod
021900150928     c                   Kfld                    kTbeKe1
022000150922     C     KARB          KLIST
022100150922     C                   KFLD                    AR5AAS
022200150922     C                   KFLD                    AR5LNP
022300150922     C                   KFLD                    AR5NRS
022400150922     C                   KFLD                    AR5NSP
022500150922     C     Kbrv          KLIST
022600150922     C                   KFLD                    knpg
022700150922     C                   KFLD                    ARtfls
022800150922     C                   KFLD                    ARtlna
022900150922     C                   KFLD                    ARtnrs
023000150922     C                   KFLD                    ARtnsc
023100150922     C     KANM          KLIST
023200150922     C                   KFLD                    ARtfls
023300150922     C                   KFLD                    ARtlna
023400150922     C                   KFLD                    ARtnrs
023500150922     C                   KFLD                    ARtnsc
023600150922     C                   KFLD                    kcaa
023700070124     C     KE2TAB        KLIST
023800070124     C                   KFLD                    KTBKUT
023900070124     C                   KFLD                    KTBCOD
024000150924     c     ksia          klist
024100150924     c                   kfld                    arbaas
024200150924     c                   kfld                    arblnp
024300150924     c                   kfld                    arbnrs
024400150924     c                   kfld                    arbnsp
024500150924     c                   kfld                    TIPA
024600150924     c     kar4          klist
024700150924     c                   kfld                    arbaas
024800150924     c                   kfld                    arblnp
024900150924     c                   kfld                    arbnrs
025000150924     c                   kfld                    arbnsp
025100150924     c                   kfld                    ktrc
025200150925     c     kar5EMD       klist
025300150925     c                   kfld                    arbaas
025400150925     c                   kfld                    arblnp
025500150925     c                   kfld                    arbnrs
025600150925     c                   kfld                    arbnsp
025700150925     c                   kfld                    ktrd
025800150922     c
025900150922      * Aggancio tabella "VPO" per ora conferma idd
026000150922     C                   CLEAR                   tibs02ds
026100150922     c                   clear                   dvpo
026200150922     C                   MOVEL     'C'           T02MOD
026300150922     C                   MOVEL     'L'           T02tla
026400150922     C                   MOVEL     KNSIF         T02SIF
026500150922     C                   MOVEL     'VPO'         T02COD
026600150922     c                   movel(P)  'VPO'         t02ke1
026700150922     C                   CALL      'TIBS02R'
026800150922     C                   PARM                    KPJBA
026900150922     C                   PARM                    tibs02ds
027000150922    2C                   IF        T02ERR = *BLANKS
027100150922     C                   MOVEL     T02uni        dvpo
027200150922    2C                   ENDIF
027300060829     c* Data di elaborazione
027400060829     c                   time                    w0140            14 0
027500060829     C                   MOVE      w0140         w0080             8 0          *DATA (8) IN G/M/AA
027600060829     c     *eur          movel     w0080         dataeur
027700060829     c                   movel     dataeur       dataiso
027800060829     c     *iso          movel     dataiso       datcor            8 0
027900150922     c*
028000070124     c
028100070326     c* Carico tipi record di recupero  e C/S
028200070124     C                   Z-ADD     1             KTBKUT
028300070124     C                   MOVE      '3A'          KTBCOD
028400070124     C                   CLEAR                   IY                4 0
028500070326     C                   CLEAR                   IZ                4 0
028600070124     C     KE2TAB        SETLL     TABEL00F
028700070124     C     KE2TAB        READE     TABEL00F                               98
028800070124     C     *IN98         DOWEQ     *OFF
028900070124     C                   MOVEL     TBLUNI        DS3a
029000070412     c* Recuperi
029100070124     C     §3arbl        IFEQ      'R'
029200070124     C                   ADD       1             IY
029300070124     C                   MOVEL     TBLKEY        rec(IY)
029400070124     C                   ENDIF
029500150924
029600070124     C     KE2TAB        READE     TABEL00F                               98
029700070124     C                   ENDDO
029800070124     c
029900060829     c
030000060828     c                   ENDSR
030100060828     c*------------------------------------------------------------------
030200060828     c     ESCLUDI       BEGSR
030300060828     c                   clear                   Escbolla
030400150922     c* 1) Escludo le consegnate e quelle con lna non in £6
030500150922     c                   if        arbdcm>0
030600150922     c                   eval      Escbolla='S'
030700150922     c                   leavesr
030800150922     c                   endif
030900150922
031000150922     c                   if        %lookup(arblna:l6)=0
031100150922     c                   eval      Escbolla='S'
031200150922     c                   leavesr
031300150922     c                   endif
031400150922     c
031500150924     c* 2) con un alert qualsiasi già inviato da FISIA
031600150922     c     karb          chain     fisia01l
031700150922     c                   if        %found(fisia01l)
031800150922     c                   eval      Escbolla='S'
031900150922     c                   leavesr
032000150922     c                   endif
032100150924     c* 2) con C.A.
032200060828     c                   clear                   fidn12ds
032300060828     c                   eval      i12aas = arbaas
032400060828     c                   eval      i12lnp = arblnp
032500060828     c                   eval      i12nrs = arbnrs
032600060828     c                   eval      i12nsp = arbnsp
032700060828     c                   eval      i12fel = 'E'
032800060828     c                   eval      i12fan = 'E'
032900060828      *
033000060828     c                   call      'FIDN12R'
033100060828     c                   parm                    fidn12ds
033200060828      *
033300060828      * se non ci sono errori
033400060828     c                   if        o12err = ' '
033500060828      * se numero di CA trovate maggiore di zero
033600060828     c                   if        o12nca > 0
033700060828     c
033800060828     c                   eval      Escbolla='S'
033900150922     c                   leavesr
034000060828     c                   endif
034100060828     c                   endif
034200070507     c
034300150922     c* 3) con tipo bolla di recupero
034400070507    1c     arbcbo        lookup    REC                                    35
034500070507     c                   if        *in35
034600070507     c                   eval      Escbolla='S'
034700150922     c                   leavesr
034800070507     c                   endif
034900060828     C*
035000150922     c                   ENDSR
035100150922     c*------------------------------------------------------------------
035200150922     c* Elabora
035300150922     c*------------------------------------------------------------------
035400150922     c     ELABORA       BEGSR
035500150922     c                   clear                   fnlv53ds
035600150922     c                   clear                   CallSD99          1
035700150922     c                   clear                   tipocal           1
035800151002     c                   clear                   tipa
035900151002     c                   clear                   wdisgimp          1
036000150922     c
036100150922     c* controalert "1" - IMP
036200150922     c* controalert "2" - DISGUIDO
036300150925    1c                   if        %lookup('1':tcal)>0  or
036400150925    1c                             %lookup('2':tcal)>0
036500150922    c
036600150922     c* Se nessun collo arrivato o arrivati in data maggiore e
036700150922     c*    se presente alemno un collo in IMP richiamo tnsd99r
036800150922    2c                   if        arbdam=0 or arbdam>ilrc4dat
036900150922     c
037000150922     c     karb          setll     fnart01l
037100150922     c     karb          reade     fnart01l
037200151002    3c                   dow       not %eof(fnart01l) and wdisgimp<>'E'
037300151002     c
037400151002     c                   exsr      CTRaler2
037500151002     c                   exsr      CTRaler1
037600150922     c
037700150922     c     karb          reade     fnart01l
037800150922    3c                   enddo
037900150922    c
038000150922    2c                   endif
038100150922    1c                   endif
038200150922     c
038300150922     c* controalert "3" - NON IN CONSEGNA
038400150924     c* non devo escludere se è in distitna perchè potrebbe essere su distinta fittizia
038500150924    1c********           if        %lookup('3':tcal)  AND ARBNDC=0
038600150928    1c                   if        CallSD99=' ' and %lookup('3':tcal)>0
038700150924     c                   eval      callsd99='3'
038800150924     c                   eval      tipa='CL3'
038900150922    1c                   endif
039000150922     C
039100150924     C                   IF        CALLSD99<>' '
039200150922     C                   CLEAR                   TNSD99ds
039300150922     C                   MOVEL     'A'           D98TBO
039400150922     C                   MOVEL     ARBAAS        D98AAS
039500150922     C                   MOVEL     ARBLNP        D98LNP
039600150922     C                   MOVEL     ARBNRS        D98NRS
039700150922     C                   MOVEL     ARBNSP        D98NSP
039800150928     c                   if        arbtsp= 'D'
039900150928     c                   movel     'C'           I98TSP_FOR
040000150928     c                   endif
040100150922     C*
040200150922     C                   CALL      'TNSD99R'
040300150922     C                   PARM                    TNSD99DS
040400150922
040500150924     c* scrivo il file spia
040600150922    1c                   if        D98SPCDEE='IMP' or D98SPCDEE='OLP'
040700151002     c
040800151002     c* Se mi restituisce OLP significa che è NON IN CONSEGNA
040900151002     c                   if        D98SPCDEE='OLP'
041000151002     c                   eval      tipa='CL3'
041100151002     c                   endif
041200151002     c
041300150924     c                   exsr      WriSpia
041400150925
041500150925     c* A questo punto chaino recrdo fiar5 per aggiornare invio controalert
041600150925     c                   if        Wesitocl<>' '
041700150925     c     kar5emd       chain     fiar501l
041800150925     c                   if        %found(fiar501l)
041900150925     c                   eval      dar5emd=ar5uni
042000150925     c                   eval      §ar5ica='S'
042100150928     c                   eval      ar5uni=dar5emd
042200150928     c***                update    fiar5001
042300150925     c                   endif
042400150925     c                   endif
042500150925
042600150922     c                   endif
042700150925     c                   endif
042800150922     c
042900150922     c                   ENDSR
043000150922     c*------------------------------------------------------------------
043100150922     c     CTRaler1      BEGSR
043200150922     c                   eval      knpg=3
043300150922     c     kbrv          setll     fnbrv03l
043400150922     c     kbrv          reade     fnbrv03l
043500150922    1c                   dow       not %eof(fnbrv03l)
043600150922    2c                   if        brvatr<>'D'
043700150922     c                   clear                   fnlv53ds
043800150922     c                   eval      d53tfo='V'
043900150922     c                   eval      d53nfv=brvnfv
044000150922     c                   eval      d53npg=brvnpg
044100150922     c                   eval      d53fgs=brvfgs
044200150922     c                   call      'FNLV53R'
044300150922     c                   parm                    fnlv53ds
044400150922    3c                   if        d53err=' ' and d53spg='P'
044500151002     c                   eval      aIMP ='S'
044600150922     c                   leave
044700150922    3c                   endif
044800150922    2c                   endif
044900150922     c
045000150922     c     kbrv          reade     fnbrv03l
045100150922    1c                   enddo
045200150922     c                   ENDSR
045300150922     c*------------------------------------------------------------------
045400150922     c     CTRaler2      BEGSR
045500150922     c                   eval      kcaa=55
045600150922     c     kanm          chain     fnanm02l
045700150922     c                   if        not %found(fnanm02l)
045800150922     c                   eval      kcaa=56
045900150922     c     kanm          chain     fnanm02l
046000150922     c                   endif
046100150922     c
046200150922     c                   if        %found(fnanm02l)
046300151002     c                   eval      adisg   ='S'
046400150922     c                   endif
046500150922
046600150922     c                   ENDSR
046700150924     c*------------------------------------------------------------------
046800150924     c     WriSpia       BEGSR
046900150924     c* flag esito scittura file spia
047000150924     c                   clear                   WesitoCL          1
047100150924     C* verifico se ho l'indirizzo mail o il telefono del destinatario
047200150924     c                   clear                   fnlv81ds
047300150924     c                   eval      ILV81AAS = arbaas
047400150924     c                   eval      ILV81lnp = arblnp
047500150924     c                   eval      ILV81nrs = arbnrs
047600150924     c                   eval      ILV81nsp = arbnsp
047700150924     c                   eval      ILV81trc = 'A'
047800150924     c                   call      'FNLV81R'
047900150924     c                   parm                    fnlv81ds
048000150924     c                   if        OLV81EML = ' ' and OLV81tel = ' '
048100150924     c                   eval      WesitoCL='N'
048200150924     c                   leavesr
048300150924     c                   end
048400150924     c* scrivo la spia
048500150924     c                   clear                   rrnsia
048600150924     c     ksia          chain     fisia01l
048700150924     c* se il record è già presente da elaborare o elaborato esco
048800150924     c                   if        %found(fisia01l) and
048900150924     c                             (siasts = '0' or
049000150924     c                             siasts = '1')
049100150924     c                   eval      WesitoCL='G'
049200150924     c                   leavesr
049300150924     c                   end
049400150924     c* se il record è già presente ma non ha avuto esito corretto tento
049500150924     c* di aggiornarlo
049600150924     c                   if        %found(fisia01l)
049700150924     c     rrnsia        chain(e)  FISIA00f
049800150924     c                   if        %error
049900150924     c                   eval      WesitoCL='E'
050000150924     c                   leavesr
050100150924     c                   end
050200150924     c                   end
050300150924      *
050400150924     c                   clear                   FISIA000
050500150924     c                   time                    SIADAORI
050600150924     c                   eval      SIAAAS = arbaas
050700150924     c                   eval      SIALNP = arblnp
050800150924     c                   eval      SIANRS = arbnrs
050900150924     c                   eval      SIANSP = arbnsp
051000150924     c                   eval      SIATIPA= tipa
051100150928     c                   eval      SIASTS ='9'
051200150924     c                   eval      SIApty =50
051300150924     c
051400150924     c                   clear                   dsiaica
051500150924     c                   eval      §SIANDPC=d98dee
051600150924     c* richiamo il programma degli orari
051700150924      * se fermo deposito non richiamo pgm per reperimento orari
051800150925     c                   clear                   trulorsds
051900150925     c                   clear                   trulor2ds
052000150925     c                   clear                   trulor3ds
052100150924    2c                   if         arbffd = *blank
052200150924     c                   eval       IOREtser = 'C'
052300150924     c                   eval       IOREfil=arblna
052400150924     c                   eval       IOREdta=d98dee
052500150924     c                   eval       IOREcap=arbcad
052600150924     c                   eval       IOREloc=arblod
052700150924     c                   eval       IOREtfp=arbtfp
052800150924     c                   eval       IOREtfa=arbtfa
052900150924     c                   eval       IOREdti=arbdti
053000150924     c* Se passo HTI=0000 prendo orario sempre entro soglia
053100150924     c***                eval       IOREhti=arbhti
053200150924
053300150924     c                   eval       IOREdcr=arbdcr
053400150924     c                   eval       IOREhcr=arbhcr
053500150924     c                   eval       IOREtcr=arbtcr
053600150924     c                   eval       IOREtsp=arbtsp
053700150924     c                   eval       IOREnar=arbnzd
053800150924     c                   eval       IOREdei=d98dei
053900150924     c                   eval       IOREoei=d98oei
054000150924     c                   clear                   diore01
054100150924     c                   movel     d98ttc        §ioretrazC
054200150924     c                   movel     d98tcc        §ioreconsC
054300150924     c                   eval       IOREflo=diore01
054400150924     c* passo la località normalizzata
054500150924     c     kar4          CHAIN     fiar401l
054600150924if  1c                   IF        %found(fiar401l)
054700150924     c                   MOVEL     ar4not        OOR3LOC_N
054800150924     c                   else
054900150924     c                   movel     arblod        OOR3LOC_N
055000150924e   1c                   ENDIF
055100150924     c
055200150924     c                   call      'TRULORSR'
055300150924     C                   PARM                    kpjba
055400150924     C                   PARM                    trulorsds
055500150924     C                   PARM                    trulor2ds
055600150924     C                   PARM                    trulor3ds
055700150924     c
055800150924     c* imposto orari in DS
055900150928     c                   eval      §SIANOPCD=oor2stis
056000150928     c                   eval      §SIANOPCA=oor2stfs
056100150928     c                   endif
056200150924
056300150924     c                   eval      sianote= dsiaica
056400150928     c                   movel     sianote       stampa           30
056500150928     c                   except    det
056600150925     c
056700150924     c
056800150924     c* se l'ho trovato ma è "anomalo" provo a rimandarlo
056900150924     c                   if        %found(fisia00f)
057000150928     c**                 update    FISIA000                             01
057100150924     c                   else
057200150928     c**                 write     FISIA000                             01
057300150924     c                   end
057400150925     c                   eval      WesitoCL='S'
057500150924     c*
057600150924     c                   ENDSR
057700150928     OQSYSPRT   E            det         1
057800150928     O                       arbaas           +   1
057900150928     O                       arblnp           +   1
058000150928     O                       arbnrs           +   1
058100150928     O                       arbnsp           +   1
058200150928     O                       stampa           +   1
058300151002     O                       tipa             +   1

000100941109     H*------------------------------------------------------------------------*
000200150924     H* Controalert : scrittura file spia                                      *
000300941109     H*------------------------------------------------------------------------*
000400930128     H DECEDIT('0,') DATEDIT(*DMY.)
000500140805     H DFTACTGRP(*NO) ACTGRP('BARTVAS')
000600941109     F*------------------------------------------------------------------------*
000700150922     ffnbrv03l  if   e           k disk
000800150922     ffnanm02l  if   e           k disk
000900150922     ftabel00f  if   e           k disk
001000151013     ffnblp01l  if   e           k disk    prefix(arb:3)
001100150928     fTNTBE01L  uF a E           K DISK
001200150922     Ffiar505l  IF   E           K DISK
001300151006     Ffiar501l  uF   E           K DISK    rename(fiar5000:fiar5001)
001400060828     FFNARB01L  IF   E           K DISK
001500150924     FFNart01L  IF   E           K DISK
001600150924     Ffiar401l  IF   E           K DISK
001700150924     Ffisia01l  if   e           k disk    rename(fisia000:sia001) infds(fisia)
001800151006     FFIsia00F  uf a E             DISK
001900070328     FFNLBL02L  IF   E           K DISK
002000150928     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002100070124     d
002200070124     D REC             S              2    DIM(50)
002300060828     d
002400150914     d* DS ricevuta
002500150914     d FNLRc4DS      E ds
002600150928     D  tcal                  12     21    dim(10)
002700150928     D  riccal                12     21
002800150924     d
002900150924     d fisia           ds
003000150924     d  rrnsia               397    400b 0
003100060831     d
003200060828     D KPJBA         E DS
003300150924     D trulorsds     E DS                  INZ
003400150924     D trulor2ds     E DS                  INZ
003500150924     D trulor3ds     E DS                  INZ
003600060829     D TRUL33DS      E DS
003700150922     D tibs02ds      E DS
003800150922     D fnlv55ds      E DS
003900150922     D fnlv53ds      E DS
004000150924     D fnlv81ds      E DS
004100150922     D tnsd99ds      E DS
004200060828     D trul06ds      E DS
004300060828     D  L6                     1     90  0 dim(30)
004400110629
004500060828      * ds per il controllo della presenza di CA per la spedizione richiesta
004600060828     d FIDN12DS      e ds
004700060828     d  skKey                 26    305    dim(20)
004800060828     d  skAnn                306    325    dim(20)
004900060828     d  skDca                326    485  0 dim(20)
005000060828     d  skFca                486    545  0 dim(20)
005100060828     d  skDch                546    705  0 dim(20)
005200060828     d  skCch                706    745    dim(20)
005300060828
005400060828     d dsKey           ds
005500060828     d  dsaac                         4  0
005600060828     d  dsfil                         3  0
005700060828     d  dsnca                         7  0
005800060828      *
005900070124     D DS3A          E DS
006000150922     D Dvpo          E DS
006100150928     D DSDF          E DS
006200150924     D DSIAICA       E DS
006300150924     D Diore01       E DS                  INZ
006400150928     D Dar5emd       E DS                  INZ
006500060424     d
006600060828     d II              s              2  0 inz(0)
006700060828     d xx              s              3  0 inz(0)
006800060828     d yy              s              3  0 inz(0)
006900150924     d indx            s              3  0
007000150924     d KFIL            s              3  0
007100150925     d Kdat            s              8    inz
007200150922     d kica            s              1    inz(' ')
007300060828     d Escbolla        s              1
007400080116     d POL6            s              1
007500150928
007600150928     d kTbeCod         s                   Like(TbeCod)
007700150928     d kTbeKe1         s                   Like(TbeKe1)
007800150924     D ktrc            S                   LIKE(ar4trc) inz('X')
007900150925     D ktrd            S                   LIKE(ar5trd) inz('EMD')
008000150924     D TIPA            S                   LIKE(siatipa)
008100150924     D knpg            S                   LIKE(brvnpg)
008200150922     D kcaa            S                   LIKE(anmcaa)
008300150922     D Wtbecod         S                   LIKE(TBEcod)
008400060829     d Dataiso         s               d   datfmt(*iso)
008500060829     d Dataeur         s               d   datfmt(*eur)
008600070124     d ktbkut          s                   like(tblkut) inz(1)
008700070124     d ktbcod          s                   like(tblcod)
008800941109     C*------------------------------------------------------------------------*
008900060424     c     *entry        plist
009000060424     c                   parm                    kpjba
009100150914     c                   movel     kpjbu         fnlrc4ds
009200150922     c*
009300150922     c* non elaboro controalert 1 e 2 se passato orario limite
009400150925     c                   if        �VPOHLCOA>*zeros and �VPOHLCOA<'2400'
009500150922     c                   movel     w0140         uora              4 0
009600150925     c                   if        %editc(uora:'X') >�VPOHLCOA
009700151005     c*  '1' - IMP/DISGUIDO
009800150922     c                   eval      indx=%lookup('1':tcal)
009900150922     c                   if        indx>0
010000150922     c                   clear                   tcal(indx)
010100150922     c                   endif
010200150922     c*  '2' - DISGUIDO
010300151005     c**                 eval      indx=%lookup('2':tcal)
010400151005     c**                 if        indx>0
010500151005     c**                 clear                   tcal(indx)
010600151005     c**                 endif
010700150922     c                   endif
010800150922     c                   endif
010900070529     c
011000150922     c* se nessun controalert da elaborare --> esco
011100150924     c                   if        riccal<>*blanks
011200080115     c
011300080115     c* Verifico se � un p.o. di �6
011400080115     c                   clear                   fnlv55ds
011500150914     c                   movel     ilrc4lna      d55lin
011600150914     c                   movel     ilrc4dat      d55drf
011700080115     c                   movel     '6'           d55tpt
011800080115     c                   movel     'L'           d55tla
011900080115     c                   call      'FNLV55R'
012000080115     c                   parm                    fnlv55ds
012100080115     c
012200150928     c                   clear                   pol6
012300150928     c                   if        d55tfa<>ilrc4lna
012400150928     c                   eval      pol6='S'
012500150928     c                   endif
012600070406     c*
012700080115     c* Carico la �6 alla data di elaborazione
012800060828     c                   clear                   trul06ds
012900150924     c                   movel     IlRc4LNA      d06key
013000080115     c                   move      'S'           d06key
013100060828     c                   movel     '�6'          d06cod
013200150922     c                   movel     'L'           d06tla
013300150924     c                   movel     ilrc4dat      d06drf
013400080115     c                   movel     trul06ds      kpjbu
013500060828     c                   call      'TRUL06R'
013600060828     c                   parm                    kpjba
013700060828     c                   movel     kpjbu         trul06ds
013800150922     c
013900150922     c* Lettura file fiar505l alert ancora da inviare
014000150925     c                   eval      kdat=%editc(ilrc4dat:'X')
014100150922     c
014200150922     c     kar5          setll     fiar505l
014300150922     c     kar5          reade     fiar505l
014400070529     c
014500150922    6c                   dow       not %eof(fiar505l)
014600150922     c     karb          chain     fnarb01l
014700150922     c                   if        %found(fnarb01l)
014800060828     c                   exsr      ESCLUDI
014900101119     c
015000150922     C                   IF        ESCBOLLA=' '
015100060828     c                   exsr      ELABORA
015200070529     c                   endif
015300150922     c                   endif
015400150922
015500150922     c     kar5          reade     fiar505l
015600150922     c                   endDO
015700080116     c*
015800150922     c                   endif
015900150928     c
016000150928     c* e ho elaborato il controalert "3" allora aggiorno la tabella SDF
016100150928    1c                   if        ilrc4lna>0  and %lookup('3':tcal)>0
016200150928     c                             and pol6=' '
016300150928     c                   Eval      kTbeCod = 'SDF'
016400150928     c                   Movel(p)  ilrc4lna      kTbeKe1
016500150928     c     Ktbe          Chain     Tntbe01l
016600150928    2c                   If        %Found(Tntbe01l)
016700150928     c                   Movel     TbeUni        dSdf
016800150930     c                   monitor
016900150928    3c                   If        �SDFCA3<ilrc4dat
017000150928     c                   Z-Add     ilrc4dat      �SDFCA3
017100150928    3c                   EndIf
017200150930
017300150930     c                   on-error
017400150930     c                   Z-Add     ilrc4dat      �SDFCA3
017500150930     c                   endmon
017600150930
017700150928     c                   Movel     dSdf          TbeUni
017800150928     c                   Update    Tntbe000
017900150928   x2c                   else
018000150928
018100150928     c                   clear                   tntbe000
018200150928     c                   eval      tbeapl='OP'
018300150928     c                   eval      tbecod='SDF'
018400150928     c                   eval      tbeke1=%editc(ilrc4lna:'X')
018500150928     c                   clear                   dsdf
018600150928     c                   Z-Add     ilrc4dat      �SDFCA3
018700150928     c                   Movel     dSdf          TbeUni
018800150928     c                   write     Tntbe000
018900150928     C                   ENDIF
019000150928     c                   endif
019100150928     c
019200150922
019300150922     c                   clear                   fidn12ds
019400150922     c                   eval      i12tla = 'C'
019500150922     c                   call      'FIDN12R'
019600150922     c                   parm                    fidn12ds
019700150922     c                   clear                   fnlv53ds
019800150922     c                   eval      d53tla = 'C'
019900150922     c                   call      'FNLV53R'
020000150922     c                   parm                    fidn12ds
020100150922     c                   clear                   tnsd99ds
020200150922     c                   eval      d98tla = 'C'
020300151008     c                   call      'TNSD99R'
020400150922     c                   parm                    tnsd99ds
020500150925     c                   clear                   fnlv81ds
020600150925     c                   eval      ilv81tla = 'C'
020700150925     c                   call      'FNLV81R'
020800150925     c                   parm                    fnlv81ds
020900060829     c
021000000000     C                   SETON                                        LR
021100060828     c*------------------------------------------------------------------
021200060828     c     *INZSR        BEGSR
021300150922     c     Kar5          klist
021400150922     c                   kfld                    kdat
021500150922     c                   kfld                    kica
021600150928     c     Ktbe          Klist
021700150928     c                   Kfld                    kTbeCod
021800150928     c                   Kfld                    kTbeKe1
021900150922     C     KARB          KLIST
022000150922     C                   KFLD                    AR5AAS
022100150922     C                   KFLD                    AR5LNP
022200150922     C                   KFLD                    AR5NRS
022300150922     C                   KFLD                    AR5NSP
022400151013     C     Kblp          KLIST
022500151013     C                   KFLD                    lblAAn
022600151013     C                   KFLD                    lblLpn
022700151013     C                   KFLD                    lblNRn
022800151013     C                   KFLD                    lblNSn
022900150922     C     Kbrv          KLIST
023000150922     C                   KFLD                    knpg
023100150922     C                   KFLD                    ARtfls
023200150922     C                   KFLD                    ARtlna
023300150922     C                   KFLD                    ARtnrs
023400150922     C                   KFLD                    ARtnsc
023500150922     C     KANM          KLIST
023600150922     C                   KFLD                    ARtfls
023700150922     C                   KFLD                    ARtlna
023800150922     C                   KFLD                    ARtnrs
023900150922     C                   KFLD                    ARtnsc
024000150922     C                   KFLD                    kcaa
024100070124     C     KE2TAB        KLIST
024200070124     C                   KFLD                    KTBKUT
024300070124     C                   KFLD                    KTBCOD
024400150924     c     ksia          klist
024500150924     c                   kfld                    arbaas
024600150924     c                   kfld                    arblnp
024700150924     c                   kfld                    arbnrs
024800150924     c                   kfld                    arbnsp
024900150924     c                   kfld                    TIPA
025000150924     c     kar4          klist
025100150924     c                   kfld                    arbaas
025200150924     c                   kfld                    arblnp
025300150924     c                   kfld                    arbnrs
025400150924     c                   kfld                    arbnsp
025500150924     c                   kfld                    ktrc
025600150925     c     kar5EMD       klist
025700150925     c                   kfld                    arbaas
025800150925     c                   kfld                    arblnp
025900150925     c                   kfld                    arbnrs
026000150925     c                   kfld                    arbnsp
026100150925     c                   kfld                    ktrd
026200150922     c
026300150922      * Aggancio tabella "VPO" per ora conferma idd
026400150922     C                   CLEAR                   tibs02ds
026500150922     c                   clear                   dvpo
026600150922     C                   MOVEL     'C'           T02MOD
026700150922     C                   MOVEL     'L'           T02tla
026800150922     C                   MOVEL     KNSIF         T02SIF
026900150922     C                   MOVEL     'VPO'         T02COD
027000150922     c                   movel(P)  'VPO'         t02ke1
027100150922     C                   CALL      'TIBS02R'
027200150922     C                   PARM                    KPJBA
027300150922     C                   PARM                    tibs02ds
027400150922    2C                   IF        T02ERR = *BLANKS
027500150922     C                   MOVEL     T02uni        dvpo
027600150922    2C                   ENDIF
027700060829     c* Data di elaborazione
027800060829     c                   time                    w0140            14 0
027900060829     C                   MOVE      w0140         w0080             8 0          *DATA (8) IN G/M/AA
028000060829     c     *eur          movel     w0080         dataeur
028100060829     c                   movel     dataeur       dataiso
028200060829     c     *iso          movel     dataiso       datcor            8 0
028300150922     c*
028400070124     c
028500070326     c* Carico tipi record di recupero  e C/S
028600070124     C                   Z-ADD     1             KTBKUT
028700070124     C                   MOVE      '3A'          KTBCOD
028800070124     C                   CLEAR                   IY                4 0
028900070326     C                   CLEAR                   IZ                4 0
029000070124     C     KE2TAB        SETLL     TABEL00F
029100070124     C     KE2TAB        READE     TABEL00F                               98
029200070124     C     *IN98         DOWEQ     *OFF
029300070124     C                   MOVEL     TBLUNI        DS3a
029400070412     c* Recuperi
029500070124     C     �3arbl        IFEQ      'R'
029600070124     C                   ADD       1             IY
029700070124     C                   MOVEL     TBLKEY        rec(IY)
029800070124     C                   ENDIF
029900150924
030000070124     C     KE2TAB        READE     TABEL00F                               98
030100070124     C                   ENDDO
030200070124     c
030300060829     c
030400060828     c                   ENDSR
030500060828     c*------------------------------------------------------------------
030600060828     c     ESCLUDI       BEGSR
030700060828     c                   clear                   Escbolla
030800150922     c* 1) Escludo le consegnate e quelle con lna non in �6
030900151012     c                   if        arbdcm>0  and arbcca<>'1'
031000150922     c                   eval      Escbolla='S'
031100150922     c                   leavesr
031200150922     c                   endif
031300151013     c*    elaboro i dirottamenti solo quando chiedo i controalert NON in CONS
031400151013     c                   if        arbdcm>0  and arbcca= '1'
031500151013     c                             and %lookup('3':tcal)=0
031600151013     c                   eval      Escbolla='S'
031700151013     c                   leavesr
031800151013     c                   endif
031900151013
032000151013     c* non elaboro i dirottamenti se la data di consegna <> dalla data di elaborazione
032100151013     c                   if        arbdcm>0  and arbcca= '1'
032200151013     c                             and arbdcm<>ilrc4dat
032300151013     c                   eval      Escbolla='S'
032400151013     c                   leavesr
032500151013     c                   endif
032600150922
032700150922     c                   if        %lookup(arblna:l6)=0
032800150922     c                   eval      Escbolla='S'
032900150922     c                   leavesr
033000150922     c                   endif
033100150922     c
033200150924     c* 2) con un alert qualsiasi gi� inviato da FISIA
033300150922     c     karb          chain     fisia01l
033400150922     c                   if        %found(fisia01l)
033500150922     c                   eval      Escbolla='S'
033600150922     c                   leavesr
033700150922     c                   endif
033800150924     c* 2) con C.A.
033900060828     c                   clear                   fidn12ds
034000060828     c                   eval      i12aas = arbaas
034100060828     c                   eval      i12lnp = arblnp
034200060828     c                   eval      i12nrs = arbnrs
034300060828     c                   eval      i12nsp = arbnsp
034400060828     c                   eval      i12fel = 'E'
034500060828     c                   eval      i12fan = 'E'
034600060828      *
034700060828     c                   call      'FIDN12R'
034800060828     c                   parm                    fidn12ds
034900060828      *
035000060828      * se non ci sono errori
035100060828     c                   if        o12err = ' '
035200060828      * se numero di CA trovate maggiore di zero
035300060828     c                   if        o12nca > 0
035400060828     c
035500060828     c                   eval      Escbolla='S'
035600150922     c                   leavesr
035700060828     c                   endif
035800060828     c                   endif
035900070507     c
036000150922     c* 3) con tipo bolla di recupero
036100070507    1c     arbcbo        lookup    REC                                    35
036200070507     c                   if        *in35
036300070507     c                   eval      Escbolla='S'
036400150922     c                   leavesr
036500070507     c                   endif
036600170207     c
036700170207     c* 4) escludo le bolle con data consegna richiesta > della data prevista consegna
036800170207     c*    escludo anche le bolle in fermo deposito
036900170207     c                   if        arbdcr>ilrc4dat  or arbffd='S'
037000170207     c                   eval      Escbolla='S'
037100170207     c                   leavesr
037200170207     c                   endif
037300060828     C*
037400150922     c                   ENDSR
037500150922     c*------------------------------------------------------------------
037600150922     c* Elabora
037700150922     c*------------------------------------------------------------------
037800150922     c     ELABORA       BEGSR
037900150922     c                   clear                   fnlv53ds
038000150922     c                   clear                   CallSD99          1
038100150922     c                   clear                   tipocal           1
038200151005     c                   clear                   tipa
038300151013     c
038400151013     c* Se la bolla � gi� chiusa come dirottata, devo scrivere controalert,
038500151013     c*  elaborando la data prevista consegna dalla bolla figlia
038600151013    0c                   if        %lookup('3':tcal)>0 and arbcca='1' and
038700151013     c                             arbdcm>0
038800151013     c* leggo la bolla figlia e la chaino
038900151013     c     karb          chain     fnlbl02l
039000151013    1c                   if        %found(fnlbl02l)
039100151013     c     kblp          chain     fnblp01l
039200151013    2c                   if        %found(fnblp01l) and arbft1=' '
039300151013     C                   CLEAR                   TNSD99ds
039400151013     C                   MOVEL     'P'           D98TBO
039500151013     C                   MOVEL     arbAAS        D98AAS
039600151013     C                   MOVEL     arbLNP        D98LNP
039700151013     C                   MOVEL     arbNRS        D98NRS
039800151013     C                   MOVEL     arbNSP        D98NSP
039900151013     c                   if        arbtsp= 'D'
040000151013     c                   movel     'C'           I98TSP_FOR
040100151013     c                   endif
040200151013     C*
040300151013     C                   CALL      'TNSD99R'
040400151013     C                   PARM                    TNSD99DS
040500151013     c* reimposto il numero di spedizione originale
040600151013     c                   eval      arbaas=ar5aas
040700151013     c                   eval      arblnp=ar5lnp
040800151013     c                   eval      arbnrs=ar5nrs
040900151013     c                   eval      arbnsp=ar5nsp
041000151013     c
041100151013     c                   eval      tipa='CL3'
041200151013     c
041300151013     c                   exsr      WriSpia
041400151013     c                   exsr      Aggfiar5
041500151013    2c                   endif
041600151013    1c                   endif
041700150922     c
041800151013   x0c                   else
041900151013     c
042000151005     c* controalert "1" - IMP/disguido
042100151005    1c                   if        %lookup('1':tcal)>0
042200150922    c
042300150922     c* Se nessun collo arrivato o arrivati in data maggiore e
042400151005     c*    se presente almeno un collo in IMP richiamo tnsd99r
042500150922    2c                   if        arbdam=0 or arbdam>ilrc4dat
042600150922     c
042700150922     c     karb          setll     fnart01l
042800150922     c     karb          reade     fnart01l
042900150922    3c                   dow       not %eof(fnart01l) and callsd99=' '
043000151002     c
043100151002     c                   exsr      CTRaler2
043200151002     c* Se nessun disguido verifico IMP
043300151005    2c                   if        CallSD99=' '
043400151002     c                   exsr      CTRaler1
043500151002    2c                   endif
043600150922     c
043700150922     c     karb          reade     fnart01l
043800150922    3c                   enddo
043900150922    c
044000150922    2c                   endif
044100150922    1c                   endif
044200150922     c
044300150922     c* controalert "3" - NON IN CONSEGNA
044400150924     c* non devo escludere se � in distitna perch� potrebbe essere su distinta fittizia
044500150928    1c                   if        CallSD99=' ' and %lookup('3':tcal)>0
044600150924     c                   eval      callsd99='3'
044700150922    1c                   endif
044800150922     C
044900151013   a0C                   IF        CALLSD99<>' '
045000150922     C                   CLEAR                   TNSD99ds
045100150922     C                   MOVEL     'A'           D98TBO
045200150922     C                   MOVEL     ARBAAS        D98AAS
045300150922     C                   MOVEL     ARBLNP        D98LNP
045400150922     C                   MOVEL     ARBNRS        D98NRS
045500150922     C                   MOVEL     ARBNSP        D98NSP
045600150928     c                   if        arbtsp= 'D'
045700150928     c                   movel     'C'           I98TSP_FOR
045800150928     c                   endif
045900150922     C*
046000150922     C                   CALL      'TNSD99R'
046100150922     C                   PARM                    TNSD99DS
046200151005
046300151005     c* Se mi restituisce OLP, non vale per controalert IMP/DISGuido
046400151005    1c                   if        D98SPCDEE='OLP'   and
046500151005     c                             %lookup('3':tcal)>0
046600151005     c                   eval      tipa='CL3'
046700151005    1c                   endif
046800151013     c* Se mi restituisce IMP non vale per controalert NON IN CONS
046900151005    1c                   if        D98SPCDEE='IMP'  and
047000151005     c                             %lookup('1':tcal)>0
047100151005     c                   eval      tipa='CL1'
047200151005    1c                   endif
047300150922
047400150924     c* scrivo il file spia
047500151005    1c                   if        tipa<>*blanks
047600150924     c                   exsr      WriSpia
047700151013     c                   exsr      Aggfiar5
047800150925
047900151002    1c                   endif
048000151013   a0c                   endif
048100151013
048200151013    0c                   endif
048300150922     c
048400150922     c                   ENDSR
048500151013     c*------------------------------------------------------------------
048600151013     c* A questo punto chaino record fiar5 per aggiornare invio controalert
048700151013     c     Aggfiar5      BEGSR
048800151013    1c                   if        Wesitocl<>' '  and wesitocl<>'E'
048900151013     c     kar5emd       chain     fiar501l
049000151013    2c                   if        %found(fiar501l)
049100151013     c                   eval      dar5emd=ar5uni
049200151013     c                   eval      �ar5ica='S'
049300151013     c                   eval      ar5uni=dar5emd
049400151013     c                   update    fiar5001
049500151013    2c                   endif
049600151013    1c                   endif
049700151013     c                   ENDSR
049800150922     c*------------------------------------------------------------------
049900150922     c     CTRaler1      BEGSR
050000150922     c                   eval      knpg=3
050100150922     c     kbrv          setll     fnbrv03l
050200150922     c     kbrv          reade     fnbrv03l
050300150922    1c                   dow       not %eof(fnbrv03l)
050400150922    2c                   if        brvatr<>'D'
050500150922     c                   clear                   fnlv53ds
050600150922     c                   eval      d53tfo='V'
050700150922     c                   eval      d53nfv=brvnfv
050800150922     c                   eval      d53npg=brvnpg
050900150922     c                   eval      d53fgs=brvfgs
051000150922     c                   call      'FNLV53R'
051100150922     c                   parm                    fnlv53ds
051200150922    3c                   if        d53err=' ' and d53spg='P'
051300150924     c                   eval      callsd99='1'
051400150922     c                   leave
051500150922    3c                   endif
051600150922    2c                   endif
051700150922     c
051800150922     c     kbrv          reade     fnbrv03l
051900150922    1c                   enddo
052000150922     c                   ENDSR
052100150922     c*------------------------------------------------------------------
052200150922     c     CTRaler2      BEGSR
052300150922     c                   eval      kcaa=55
052400150922     c     kanm          chain     fnanm02l
052500150922     c                   if        not %found(fnanm02l)
052600150922     c                   eval      kcaa=56
052700150922     c     kanm          chain     fnanm02l
052800150922     c                   endif
052900150922     c
053000150922     c                   if        %found(fnanm02l)
053100151006     c                   eval      callsd99='2'
053200150922     c                   endif
053300150922
053400150922     c                   ENDSR
053500150924     c*------------------------------------------------------------------
053600150924     c     WriSpia       BEGSR
053700150924     c* flag esito scittura file spia
053800150924     c                   clear                   WesitoCL          1
053900150924     C* verifico se ho l'indirizzo mail o il telefono del destinatario
054000150924     c                   clear                   fnlv81ds
054100150924     c                   eval      ILV81AAS = arbaas
054200150924     c                   eval      ILV81lnp = arblnp
054300150924     c                   eval      ILV81nrs = arbnrs
054400150924     c                   eval      ILV81nsp = arbnsp
054500150924     c                   eval      ILV81trc = 'A'
054600150924     c                   call      'FNLV81R'
054700150924     c                   parm                    fnlv81ds
054800150924     c                   if        OLV81EML = ' ' and OLV81tel = ' '
054900150924     c                   eval      WesitoCL='N'
055000150924     c                   leavesr
055100150924     c                   end
055200150924     c* scrivo la spia
055300150924     c                   clear                   rrnsia
055400150924     c     ksia          chain     fisia01l
055500150924     c* se il record � gi� presente da elaborare o elaborato esco
055600150924     c                   if        %found(fisia01l) and
055700150924     c                             (siasts = '0' or
055800150924     c                             siasts = '1')
055900150924     c                   eval      WesitoCL='G'
056000150924     c                   leavesr
056100150924     c                   end
056200150924     c* se il record � gi� presente ma non ha avuto esito corretto tento
056300150924     c* di aggiornarlo
056400150924     c                   if        %found(fisia01l)
056500150924     c     rrnsia        chain(e)  FISIA00f
056600150924     c                   if        %error
056700150924     c                   eval      WesitoCL='E'
056800150924     c                   leavesr
056900150924     c                   end
057000150924     c                   end
057100150924      *
057200150924     c                   clear                   FISIA000
057300150924     c                   time                    SIADAORI
057400150924     c                   eval      SIAAAS = arbaas
057500150924     c                   eval      SIALNP = arblnp
057600150924     c                   eval      SIANRS = arbnrs
057700150924     c                   eval      SIANSP = arbnsp
057800150924     c                   eval      SIATIPA= tipa
057900151008     c                   eval      SIASTS ='0'
058000150924     c                   eval      SIApty =50
058100150924     c
058200150924     c                   clear                   dsiaica
058300150924     c                   eval      �SIANDPC=d98dee
058400150924     c* richiamo il programma degli orari
058500150924      * se fermo deposito non richiamo pgm per reperimento orari
058600150925     c                   clear                   trulorsds
058700150925     c                   clear                   trulor2ds
058800150925     c                   clear                   trulor3ds
058900150924    2c                   if         arbffd = *blank
059000150924     c                   eval       IOREtser = 'C'
059100150924     c                   eval       IOREfil=arblna
059200150924     c                   eval       IOREdta=d98dee
059300150924     c                   eval       IOREcap=arbcad
059400150924     c                   eval       IOREloc=arblod
059500151008     c* se il campo "terminal didisguido" � pieno, passo questo al posto
059600151008     c* del terminal di partenza originale
059700151008     c                   if         d98flp>*zeros
059800151008     c                   eval       IOREtfp=%INT(d98flp)
059900151008     c                   else
060000151008     c                   eval       IOREtfp=arbtfp
060100151008     c                   endif
060200151008     c
060300150924     c                   eval       IOREtfa=arbtfa
060400151008     c                   if         arbdti>0  and arbdti<=ILRC4DAT
060500150924     c                   eval       IOREdti=arbdti
060600150924     c* Se passo HTI=0000 prendo orario sempre entro soglia
060700150924     c***                eval       IOREhti=arbhti
060800151008     c                   else
060900151008     c                   eval       IOREdti=arbdti
061000151008     c                   eval       IOREhti=arbhti
061100151008     c                   endif
061200150924
061300150924     c                   eval       IOREdcr=arbdcr
061400150924     c                   eval       IOREhcr=arbhcr
061500150924     c                   eval       IOREtcr=arbtcr
061600150924     c                   eval       IOREtsp=arbtsp
061700150924     c                   eval       IOREnar=arbnzd
061800150924     c                   eval       IOREdei=d98dei
061900150924     c                   eval       IOREoei=d98oei
062000150924     c                   clear                   diore01
062100150924     c                   movel     d98ttc        �ioretrazC
062200150924     c                   movel     d98tcc        �ioreconsC
062300150924     c                   eval       IOREflo=diore01
062400150924     c* passo la localit� normalizzata
062500150924     c     kar4          CHAIN     fiar401l
062600150924if  1c                   IF        %found(fiar401l)
062700150924     c                   MOVEL     ar4not        OOR3LOC_N
062800150924     c                   else
062900150924     c                   movel     arblod        OOR3LOC_N
063000150924e   1c                   ENDIF
063100150924     c
063200150924     c                   call      'TRULORSR'
063300150924     C                   PARM                    kpjba
063400150924     C                   PARM                    trulorsds
063500150924     C                   PARM                    trulor2ds
063600150924     C                   PARM                    trulor3ds
063700150924     c
063800150924     c* imposto orari in DS
063900150928     c                   eval      �SIANOPCD=oor2stis
064000150928     c                   eval      �SIANOPCA=oor2stfs
064100150928     c                   endif
064200150924
064300150924     c                   eval      sianote= dsiaica
064400150928     c                   movel     sianote       stampa           30
064500151019     c                   if        callsd99='2' and %subst(knmus:1:3)='EDP'
064600151124     c                             and *in01 and not *in01
064700150928     c                   except    det
064800151006     c                   endif
064900150925     c
065000150924     c
065100150924     c* se l'ho trovato ma � "anomalo" provo a rimandarlo
065200150924     c                   if        %found(fisia00f)
065300151006     c                   update    FISIA000                             01
065400150924     c                   else
065500151006     c                   write     FISIA000                             01
065600150924     c                   end
065700150925     c                   eval      WesitoCL='S'
065800150924     c*
065900150924     c                   ENDSR
066000150928     OQSYSPRT   E            det         1
066100150928     O                       arbaas           +   1
066200150928     O                       arblnp           +   1
066300150928     O                       arbnrs           +   1
066400150928     O                       arbnsp           +   1
066500151006     O                       arblna           +   1
066600150928     O                       stampa           +   1
066700151002     O                       tipa             +   1

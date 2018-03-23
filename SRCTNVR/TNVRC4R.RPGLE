000100941109     H*------------------------------------------------------------------------*
000200150924     H* Controalert : scrittura file spia                                      *
000300941109     H*------------------------------------------------------------------------*
000400930128     H DECEDIT('0,') DATEDIT(*DMY.)
000500140805     H DFTACTGRP(*NO) ACTGRP('BARTVAS')
000600941109     F*------------------------------------------------------------------------*
000700150922     ffnbrv03l  if   e           k disk
000800151124     fazorg01l  if   e           k disk
000900150922     ffnanm02l  if   e           k disk
001000150922     ftabel00f  if   e           k disk
001100151013     ffnblp01l  if   e           k disk    prefix(arb:3)
001200151124     fTNTBE01L  iF   E           K DISK
001300150922     Ffiar505l  IF   E           K DISK
001400151124     Ffiar501l  iF   E           K DISK    rename(fiar5000:fiar5001)
001500060828     FFNARB01L  IF   E           K DISK
001600150924     FFNart01L  IF   E           K DISK
001700150924     Ffiar401l  IF   E           K DISK
001800151130     Ffisia01l  if   e           k disk    rename(fisia000:sia001) infds(fisia)
001900151130     FFIsia00F  if a E             DISK
002000070328     FFNLBL02L  IF   E           K DISK
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
006600151124     d og143         e ds
006700060424     d
006800060828     d II              s              2  0 inz(0)
006900060828     d xx              s              3  0 inz(0)
007000060828     d yy              s              3  0 inz(0)
007100150924     d indx            s              3  0
007200150924     d KFIL            s              3  0
007300150925     d Kdat            s              8    inz
007400150922     d kica            s              1    inz(' ')
007500060828     d Escbolla        s              1
007600080116     d POL6            s              1
007700150928
007800150928     d kTbeCod         s                   Like(TbeCod)
007900150928     d kTbeKe1         s                   Like(TbeKe1)
008000150924     D ktrc            S                   LIKE(ar4trc) inz('X')
008100150925     D ktrd            S                   LIKE(ar5trd) inz('EMD')
008200150924     D TIPA            S                   LIKE(siatipa)
008300150924     D knpg            S                   LIKE(brvnpg)
008400150922     D kcaa            S                   LIKE(anmcaa)
008500150922     D Wtbecod         S                   LIKE(TBEcod)
008600060829     d Dataiso         s               d   datfmt(*iso)
008700060829     d Dataeur         s               d   datfmt(*eur)
008800070124     d ktbkut          s                   like(tblkut) inz(1)
008900070124     d ktbcod          s                   like(tblcod)
009000151124     d orasys          s              6  0
009100151124     d orasys4         s              4  0
009200151124     d wlnae           s                   like(ilrc4lna)
009300151124     d wsdfca3         s                   like(datcor)
009400151124     d tibs02r         pr                  extpgm('TIBS02R')
009500151124     d  kpjba                              likeds(kpjba)
009600151124     d  tibs02ds                           likeds(tibs02ds)
009700151124      /copy gaitrasrc/srcprotopr,fnlv55r
009800941109     C*------------------------------------------------------------------------*
009900060424     c     *entry        plist
010000060424     c                   parm                    kpjba
010100151124     c                   exsr      cercafil
010200150922
010300151124     c                   except    tot
010400151124
010500150922     c                   clear                   fidn12ds
010600150922     c                   eval      i12tla = 'C'
010700150922     c                   call      'FIDN12R'
010800150922     c                   parm                    fidn12ds
010900150922     c                   clear                   fnlv53ds
011000150922     c                   eval      d53tla = 'C'
011100150922     c                   call      'FNLV53R'
011200150922     c                   parm                    fidn12ds
011300150922     c                   clear                   tnsd99ds
011400150922     c                   eval      d98tla = 'C'
011500151008     c                   call      'TNSD99R'
011600150922     c                   parm                    tnsd99ds
011700150925     c                   clear                   fnlv81ds
011800150925     c                   eval      ilv81tla = 'C'
011900150925     c                   call      'FNLV81R'
012000150925     c                   parm                    fnlv81ds
012100060829     c
012200000000     C                   SETON                                        LR
012300151124     c*------------------------------------------------------------------
012400151124     c     conta         BEGSR
012500151124     c* non elaboro controalert 1 e 2 se passato orario limite
012600151124     c                   if        §VPOHLCOA>*zeros and §VPOHLCOA<'2400'
012700151124     c                   movel     w0140         uora              4 0
012800151124     c                   if        %editc(uora:'X') >§VPOHLCOA
012900151124     c*  '1' - IMP/DISGUIDO
013000151124     c                   eval      indx=%lookup('1':tcal)
013100151124     c                   if        indx>0
013200151124     c                   clear                   tcal(indx)
013300151124     c                   endif
013400151124     c*  '2' - DISGUIDO
013500151124     c**                 eval      indx=%lookup('2':tcal)
013600151124     c**                 if        indx>0
013700151124     c**                 clear                   tcal(indx)
013800151124     c**                 endif
013900151124     c                   endif
014000151124     c                   endif
014100151124     c
014200151124     c* se nessun controalert da elaborare --> esco
014300151124     c                   if        riccal<>*blanks
014400151124     c
014500151124     c* Verifico se è un p.o. di £6
014600151124     c                   clear                   fnlv55ds
014700151124     c                   movel     ilrc4lna      d55lin
014800151124     c                   movel     ilrc4dat      d55drf
014900151124     c                   movel     '6'           d55tpt
015000151124     c                   movel     'L'           d55tla
015100151124     c                   call      'FNLV55R'
015200151124     c                   parm                    fnlv55ds
015300151124     c
015400151124     c                   clear                   pol6
015500151124     c                   if        d55tfa<>ilrc4lna
015600151124     c                   eval      pol6='S'
015700151124     c                   endif
015800151124     c*
015900151124     c* Carico la £6 alla data di elaborazione
016000151124     c                   clear                   trul06ds
016100151124     c                   movel     IlRc4LNA      d06key
016200151124     c                   move      'S'           d06key
016300151124     c                   movel     '£6'          d06cod
016400151124     c                   movel     'L'           d06tla
016500151124     c                   movel     ilrc4dat      d06drf
016600151124     c                   movel     trul06ds      kpjbu
016700151124     c                   call      'TRUL06R'
016800151124     c                   parm                    kpjba
016900151124     c                   movel     kpjbu         trul06ds
017000151124     c
017100151124     c* Lettura file fiar505l alert ancora da inviare
017200151124     c                   eval      kdat=%editc(ilrc4dat:'X')
017300151124     c
017400151124     c     kar5          setll     fiar505l
017500151124     c     kar5          reade     fiar505l
017600151124     c
017700151124    6c                   dow       not %eof(fiar505l)
017800151124     c     karb          chain     fnarb01l
017900151124     c                   if        %found(fnarb01l)
018000151124     c                   exsr      ESCLUDI
018100151124     c
018200151124     C                   IF        ESCBOLLA=' '
018300151124     c                   exsr      ELABORA
018400151124     c                   endif
018500151124     c                   endif
018600151124
018700151124     c     kar5          reade     fiar505l
018800151124     c                   endDO
018900151124     c*
019000151124     c                   endif
019100151124     c                   ENDSR
019200060828     c*------------------------------------------------------------------
019300060828     c     *INZSR        BEGSR
019400150922     c     Kar5          klist
019500150922     c                   kfld                    kdat
019600150922     c                   kfld                    kica
019700150928     c     Ktbe          Klist
019800150928     c                   Kfld                    kTbeCod
019900150928     c                   Kfld                    kTbeKe1
020000150922     C     KARB          KLIST
020100150922     C                   KFLD                    AR5AAS
020200150922     C                   KFLD                    AR5LNP
020300150922     C                   KFLD                    AR5NRS
020400150922     C                   KFLD                    AR5NSP
020500151013     C     Kblp          KLIST
020600151013     C                   KFLD                    lblAAn
020700151013     C                   KFLD                    lblLpn
020800151013     C                   KFLD                    lblNRn
020900151013     C                   KFLD                    lblNSn
021000150922     C     Kbrv          KLIST
021100150922     C                   KFLD                    knpg
021200150922     C                   KFLD                    ARtfls
021300150922     C                   KFLD                    ARtlna
021400150922     C                   KFLD                    ARtnrs
021500150922     C                   KFLD                    ARtnsc
021600150922     C     KANM          KLIST
021700150922     C                   KFLD                    ARtfls
021800150922     C                   KFLD                    ARtlna
021900150922     C                   KFLD                    ARtnrs
022000150922     C                   KFLD                    ARtnsc
022100150922     C                   KFLD                    kcaa
022200070124     C     KE2TAB        KLIST
022300070124     C                   KFLD                    KTBKUT
022400070124     C                   KFLD                    KTBCOD
022500150924     c     ksia          klist
022600150924     c                   kfld                    arbaas
022700150924     c                   kfld                    arblnp
022800150924     c                   kfld                    arbnrs
022900150924     c                   kfld                    arbnsp
023000150924     c                   kfld                    TIPA
023100150924     c     kar4          klist
023200150924     c                   kfld                    arbaas
023300150924     c                   kfld                    arblnp
023400150924     c                   kfld                    arbnrs
023500150924     c                   kfld                    arbnsp
023600150924     c                   kfld                    ktrc
023700150925     c     kar5EMD       klist
023800150925     c                   kfld                    arbaas
023900150925     c                   kfld                    arblnp
024000150925     c                   kfld                    arbnrs
024100150925     c                   kfld                    arbnsp
024200150925     c                   kfld                    ktrd
024300150922     c
024400150922      * Aggancio tabella "VPO" per ora conferma idd
024500150922     C                   CLEAR                   tibs02ds
024600150922     c                   clear                   dvpo
024700150922     C                   MOVEL     'C'           T02MOD
024800150922     C                   MOVEL     'L'           T02tla
024900150922     C                   MOVEL     KNSIF         T02SIF
025000150922     C                   MOVEL     'VPO'         T02COD
025100150922     c                   movel(P)  'VPO'         t02ke1
025200150922     C                   CALL      'TIBS02R'
025300150922     C                   PARM                    KPJBA
025400150922     C                   PARM                    tibs02ds
025500150922    2C                   IF        T02ERR = *BLANKS
025600150922     C                   MOVEL     T02uni        dvpo
025700150922    2C                   ENDIF
025800060829     c* Data di elaborazione
025900060829     c                   time                    w0140            14 0
026000060829     C                   MOVE      w0140         w0080             8 0          *DATA (8) IN G/M/AA
026100060829     c     *eur          movel     w0080         dataeur
026200060829     c                   movel     dataeur       dataiso
026300060829     c     *iso          movel     dataiso       datcor            8 0
026400150922     c*
026500070124     c
026600070326     c* Carico tipi record di recupero  e C/S
026700070124     C                   Z-ADD     1             KTBKUT
026800070124     C                   MOVE      '3A'          KTBCOD
026900070124     C                   CLEAR                   IY                4 0
027000070326     C                   CLEAR                   IZ                4 0
027100070124     C     KE2TAB        SETLL     TABEL00F
027200070124     C     KE2TAB        READE     TABEL00F                               98
027300070124     C     *IN98         DOWEQ     *OFF
027400070124     C                   MOVEL     TBLUNI        DS3a
027500070412     c* Recuperi
027600070124     C     §3arbl        IFEQ      'R'
027700070124     C                   ADD       1             IY
027800070124     C                   MOVEL     TBLKEY        rec(IY)
027900070124     C                   ENDIF
028000150924
028100070124     C     KE2TAB        READE     TABEL00F                               98
028200070124     C                   ENDDO
028300070124     c
028400060829     c
028500060828     c                   ENDSR
028600060828     c*------------------------------------------------------------------
028700060828     c     ESCLUDI       BEGSR
028800060828     c                   clear                   Escbolla
028900150922     c* 1) Escludo le consegnate e quelle con lna non in £6
029000151012     c                   if        arbdcm>0  and arbcca<>'1'
029100150922     c                   eval      Escbolla='S'
029200150922     c                   leavesr
029300150922     c                   endif
029400151013     c*    elaboro i dirottamenti solo quando chiedo i controalert NON in CONS
029500151013     c                   if        arbdcm>0  and arbcca= '1'
029600151013     c                             and %lookup('3':tcal)=0
029700151013     c                   eval      Escbolla='S'
029800151013     c                   leavesr
029900151013     c                   endif
030000151013
030100151013     c* non elaboro i dirottamenti se la data di consegna <> dalla data di elaborazione
030200151013     c                   if        arbdcm>0  and arbcca= '1'
030300151013     c                             and arbdcm<>ilrc4dat
030400151013     c                   eval      Escbolla='S'
030500151013     c                   leavesr
030600151013     c                   endif
030700150922
030800150922     c                   if        %lookup(arblna:l6)=0
030900150922     c                   eval      Escbolla='S'
031000150922     c                   leavesr
031100150922     c                   endif
031200150922     c
031300150924     c* 2) con un alert qualsiasi già inviato da FISIA
031400150922     c     karb          chain     fisia01l
031500150922     c                   if        %found(fisia01l)
031600150922     c                   eval      Escbolla='S'
031700150922     c                   leavesr
031800150922     c                   endif
031900150924     c* 2) con C.A.
032000060828     c                   clear                   fidn12ds
032100060828     c                   eval      i12aas = arbaas
032200060828     c                   eval      i12lnp = arblnp
032300060828     c                   eval      i12nrs = arbnrs
032400060828     c                   eval      i12nsp = arbnsp
032500060828     c                   eval      i12fel = 'E'
032600060828     c                   eval      i12fan = 'E'
032700060828      *
032800060828     c                   call      'FIDN12R'
032900060828     c                   parm                    fidn12ds
033000060828      *
033100060828      * se non ci sono errori
033200060828     c                   if        o12err = ' '
033300060828      * se numero di CA trovate maggiore di zero
033400060828     c                   if        o12nca > 0
033500060828     c
033600060828     c                   eval      Escbolla='S'
033700150922     c                   leavesr
033800060828     c                   endif
033900060828     c                   endif
034000070507     c
034100150922     c* 3) con tipo bolla di recupero
034200070507    1c     arbcbo        lookup    REC                                    35
034300070507     c                   if        *in35
034400070507     c                   eval      Escbolla='S'
034500150922     c                   leavesr
034600070507     c                   endif
034700060828     C*
034800150922     c                   ENDSR
034900150922     c*------------------------------------------------------------------
035000150922     c* Elabora
035100150922     c*------------------------------------------------------------------
035200150922     c     ELABORA       BEGSR
035300150922     c                   clear                   fnlv53ds
035400150922     c                   clear                   CallSD99          1
035500150922     c                   clear                   tipocal           1
035600151005     c                   clear                   tipa
035700151013     c
035800151013     c* Se la bolla è già chiusa come dirottata, devo scrivere controalert,
035900151013     c*  elaborando la data prevista consegna dalla bolla figlia
036000151013    0c                   if        %lookup('3':tcal)>0 and arbcca='1' and
036100151013     c                             arbdcm>0
036200151013     c* leggo la bolla figlia e la chaino
036300151013     c     karb          chain     fnlbl02l
036400151013    1c                   if        %found(fnlbl02l)
036500151013     c     kblp          chain     fnblp01l
036600151013    2c                   if        %found(fnblp01l) and arbft1=' '
036700151013     C                   CLEAR                   TNSD99ds
036800151013     C                   MOVEL     'P'           D98TBO
036900151013     C                   MOVEL     arbAAS        D98AAS
037000151013     C                   MOVEL     arbLNP        D98LNP
037100151013     C                   MOVEL     arbNRS        D98NRS
037200151013     C                   MOVEL     arbNSP        D98NSP
037300151013     c                   if        arbtsp= 'D'
037400151013     c                   movel     'C'           I98TSP_FOR
037500151013     c                   endif
037600151013     C*
037700151013     C                   CALL      'TNSD99R'
037800151013     C                   PARM                    TNSD99DS
037900151013     c* reimposto il numero di spedizione originale
038000151013     c                   eval      arbaas=ar5aas
038100151013     c                   eval      arblnp=ar5lnp
038200151013     c                   eval      arbnrs=ar5nrs
038300151013     c                   eval      arbnsp=ar5nsp
038400151013     c
038500151013     c                   eval      tipa='CL3'
038600151013     c
038700151013     c                   exsr      WriSpia
038800151013     c                   exsr      Aggfiar5
038900151013    2c                   endif
039000151013    1c                   endif
039100150922     c
039200151013   x0c                   else
039300151013     c
039400151005     c* controalert "1" - IMP/disguido
039500151005    1c                   if        %lookup('1':tcal)>0
039600150922    c
039700150922     c* Se nessun collo arrivato o arrivati in data maggiore e
039800151005     c*    se presente almeno un collo in IMP richiamo tnsd99r
039900150922    2c                   if        arbdam=0 or arbdam>ilrc4dat
040000150922     c
040100150922     c     karb          setll     fnart01l
040200150922     c     karb          reade     fnart01l
040300150922    3c                   dow       not %eof(fnart01l) and callsd99=' '
040400151002     c
040500151002     c                   exsr      CTRaler2
040600151002     c* Se nessun disguido verifico IMP
040700151005    2c                   if        CallSD99=' '
040800151002     c                   exsr      CTRaler1
040900151002    2c                   endif
041000150922     c
041100150922     c     karb          reade     fnart01l
041200150922    3c                   enddo
041300150922    c
041400150922    2c                   endif
041500150922    1c                   endif
041600150922     c
041700150922     c* controalert "3" - NON IN CONSEGNA
041800150924     c* non devo escludere se è in distitna perchè potrebbe essere su distinta fittizia
041900150928    1c                   if        CallSD99=' ' and %lookup('3':tcal)>0
042000150924     c                   eval      callsd99='3'
042100150922    1c                   endif
042200150922     C
042300151013   a0C                   IF        CALLSD99<>' '
042400150922     C                   CLEAR                   TNSD99ds
042500150922     C                   MOVEL     'A'           D98TBO
042600150922     C                   MOVEL     ARBAAS        D98AAS
042700150922     C                   MOVEL     ARBLNP        D98LNP
042800150922     C                   MOVEL     ARBNRS        D98NRS
042900150922     C                   MOVEL     ARBNSP        D98NSP
043000150928     c                   if        arbtsp= 'D'
043100150928     c                   movel     'C'           I98TSP_FOR
043200150928     c                   endif
043300150922     C*
043400150922     C                   CALL      'TNSD99R'
043500150922     C                   PARM                    TNSD99DS
043600151005
043700151005     c* Se mi restituisce OLP, non vale per controalert IMP/DISGuido
043800151005    1c                   if        D98SPCDEE='OLP'   and
043900151005     c                             %lookup('3':tcal)>0
044000151005     c                   eval      tipa='CL3'
044100151005    1c                   endif
044200151013     c* Se mi restituisce IMP non vale per controalert NON IN CONS
044300151005    1c                   if        D98SPCDEE='IMP'  and
044400151005     c                             %lookup('1':tcal)>0
044500151005     c                   eval      tipa='CL1'
044600151005    1c                   endif
044700150922
044800150924     c* scrivo il file spia
044900151005    1c                   if        tipa<>*blanks
045000150924     c                   exsr      WriSpia
045100151013     c                   exsr      Aggfiar5
045200150925
045300151002    1c                   endif
045400151013   a0c                   endif
045500151013
045600151013    0c                   endif
045700150922     c
045800150922     c                   ENDSR
045900151013     c*------------------------------------------------------------------
046000151013     c* A questo punto chaino record fiar5 per aggiornare invio controalert
046100151013     c     Aggfiar5      BEGSR
046200151013    1c                   if        Wesitocl<>' '  and wesitocl<>'E'
046300151013     c     kar5emd       chain     fiar501l
046400151013    2c                   if        %found(fiar501l)
046500151013     c                   eval      dar5emd=ar5uni
046600151013     c                   eval      §ar5ica='S'
046700151013     c                   eval      ar5uni=dar5emd
046800151124     c*****              update    fiar5001
046900151013    2c                   endif
047000151013    1c                   endif
047100151013     c                   ENDSR
047200150922     c*------------------------------------------------------------------
047300150922     c     CTRaler1      BEGSR
047400150922     c                   eval      knpg=3
047500150922     c     kbrv          setll     fnbrv03l
047600150922     c     kbrv          reade     fnbrv03l
047700150922    1c                   dow       not %eof(fnbrv03l)
047800150922    2c                   if        brvatr<>'D'
047900150922     c                   clear                   fnlv53ds
048000150922     c                   eval      d53tfo='V'
048100150922     c                   eval      d53nfv=brvnfv
048200150922     c                   eval      d53npg=brvnpg
048300150922     c                   eval      d53fgs=brvfgs
048400150922     c                   call      'FNLV53R'
048500150922     c                   parm                    fnlv53ds
048600150922    3c                   if        d53err=' ' and d53spg='P'
048700150924     c                   eval      callsd99='1'
048800150922     c                   leave
048900150922    3c                   endif
049000150922    2c                   endif
049100150922     c
049200150922     c     kbrv          reade     fnbrv03l
049300150922    1c                   enddo
049400150922     c                   ENDSR
049500150922     c*------------------------------------------------------------------
049600150922     c     CTRaler2      BEGSR
049700150922     c                   eval      kcaa=55
049800150922     c     kanm          chain     fnanm02l
049900150922     c                   if        not %found(fnanm02l)
050000150922     c                   eval      kcaa=56
050100150922     c     kanm          chain     fnanm02l
050200150922     c                   endif
050300150922     c
050400150922     c                   if        %found(fnanm02l)
050500151006     c                   eval      callsd99='2'
050600150922     c                   endif
050700150922
050800150922     c                   ENDSR
050900150924     c*------------------------------------------------------------------
051000150924     c     WriSpia       BEGSR
051100150924     c* flag esito scittura file spia
051200150924     c                   clear                   WesitoCL          1
051300150924     C* verifico se ho l'indirizzo mail o il telefono del destinatario
051400150924     c                   clear                   fnlv81ds
051500150924     c                   eval      ILV81AAS = arbaas
051600150924     c                   eval      ILV81lnp = arblnp
051700150924     c                   eval      ILV81nrs = arbnrs
051800150924     c                   eval      ILV81nsp = arbnsp
051900150924     c                   eval      ILV81trc = 'A'
052000150924     c                   call      'FNLV81R'
052100150924     c                   parm                    fnlv81ds
052200150924     c                   if        OLV81EML = ' ' and OLV81tel = ' '
052300150924     c                   eval      WesitoCL='N'
052400150924     c                   leavesr
052500150924     c                   end
052600150924     c* scrivo la spia
052700150924     c                   clear                   rrnsia
052800150924     c     ksia          chain     fisia01l
052900150924     c* se il record è già presente da elaborare o elaborato esco
053000150924     c                   if        %found(fisia01l) and
053100150924     c                             (siasts = '0' or
053200150924     c                             siasts = '1')
053300150924     c                   eval      WesitoCL='G'
053400150924     c                   leavesr
053500150924     c                   end
053600150924     c* se il record è già presente ma non ha avuto esito corretto tento
053700150924     c* di aggiornarlo
053800150924     c                   if        %found(fisia01l)
053900150924     c     rrnsia        chain(e)  FISIA00f
054000150924     c                   if        %error
054100150924     c                   eval      WesitoCL='E'
054200150924     c                   leavesr
054300150924     c                   end
054400150924     c                   end
054500150924      *
054600150924     c                   clear                   FISIA000
054700150924     c                   time                    SIADAORI
054800150924     c                   eval      SIAAAS = arbaas
054900150924     c                   eval      SIALNP = arblnp
055000150924     c                   eval      SIANRS = arbnrs
055100150924     c                   eval      SIANSP = arbnsp
055200150924     c                   eval      SIATIPA= tipa
055300151130     c                   eval      SIASTS ='9'
055400150924     c                   eval      SIApty =50
055500150924     c
055600150924     c                   clear                   dsiaica
055700150924     c                   eval      §SIANDPC=d98dee
055800150924     c* richiamo il programma degli orari
055900150924      * se fermo deposito non richiamo pgm per reperimento orari
056000150925     c                   clear                   trulorsds
056100150925     c                   clear                   trulor2ds
056200150925     c                   clear                   trulor3ds
056300150924    2c                   if         arbffd = *blank
056400150924     c                   eval       IOREtser = 'C'
056500150924     c                   eval       IOREfil=arblna
056600150924     c                   eval       IOREdta=d98dee
056700150924     c                   eval       IOREcap=arbcad
056800150924     c                   eval       IOREloc=arblod
056900151008     c* se il campo "terminal didisguido" è pieno, passo questo al posto
057000151008     c* del terminal di partenza originale
057100151008     c                   if         d98flp>*zeros
057200151008     c                   eval       IOREtfp=%INT(d98flp)
057300151008     c                   else
057400151008     c                   eval       IOREtfp=arbtfp
057500151008     c                   endif
057600151008     c
057700150924     c                   eval       IOREtfa=arbtfa
057800151008     c                   if         arbdti>0  and arbdti<=ILRC4DAT
057900150924     c                   eval       IOREdti=arbdti
058000150924     c* Se passo HTI=0000 prendo orario sempre entro soglia
058100150924     c***                eval       IOREhti=arbhti
058200151008     c                   else
058300151008     c                   eval       IOREdti=arbdti
058400151008     c                   eval       IOREhti=arbhti
058500151008     c                   endif
058600150924
058700150924     c                   eval       IOREdcr=arbdcr
058800150924     c                   eval       IOREhcr=arbhcr
058900150924     c                   eval       IOREtcr=arbtcr
059000150924     c                   eval       IOREtsp=arbtsp
059100150924     c                   eval       IOREnar=arbnzd
059200150924     c                   eval       IOREdei=d98dei
059300150924     c                   eval       IOREoei=d98oei
059400150924     c                   clear                   diore01
059500150924     c                   movel     d98ttc        §ioretrazC
059600150924     c                   movel     d98tcc        §ioreconsC
059700150924     c                   eval       IOREflo=diore01
059800150924     c* passo la località normalizzata
059900150924     c     kar4          CHAIN     fiar401l
060000150924if  1c                   IF        %found(fiar401l)
060100150924     c                   MOVEL     ar4not        OOR3LOC_N
060200150924     c                   else
060300150924     c                   movel     arblod        OOR3LOC_N
060400150924e   1c                   ENDIF
060500150924     c
060600150924     c                   call      'TRULORSR'
060700150924     C                   PARM                    kpjba
060800150924     C                   PARM                    trulorsds
060900150924     C                   PARM                    trulor2ds
061000150924     C                   PARM                    trulor3ds
061100150924     c
061200150924     c* imposto orari in DS
061300150928     c                   eval      §SIANOPCD=oor2stis
061400150928     c                   eval      §SIANOPCA=oor2stfs
061500150928     c                   endif
061600150924
061700150924     c                   eval      sianote= dsiaica
061800150928     c                   movel     sianote       stampa           30
061900151124     c**                 if        callsd99='2' and %subst(knmus:1:3)='EDP'
062000150928     c                   except    det
062100151124     c**                 endif
062200151130     c
062300151130     c                   if        not %found(fisia00f)
062400151130     c                   write     FISIA000                             01
062500151130     c                   endif
062600150925     c
062700151124
062800151124     c                   add       1             mancac            9 0
062900150925     c                   eval      WesitoCL='S'
063000150924     c*
063100150924     c                   ENDSR
063200151124     c*------------------------------------------------------------------
063300151124     c     cercafil      BEGSR
063400151124      /free
063500151124
063600151124       // lettura delle filiali su azorg
063700151124       setll *loval azorg01l;
063800151124       read  azorg01l;
063900151124 1     dow not %eof(azorg01l);
064000151124          og143=orgde3;
064100151124          // elaboro solo se record non annullato e filiale o agenzia
064200151124          // ed escludo i network esteri
064300151124 2        if orgfva=*blanks and (orgfag='F' or orgfag='A') and
064400151124             §ogntw<>'DPD' and §ogntw<>'EEX' and §ogntw<>'FED';
064500151124          //   elaboro solo se non è in £6
064600151124             clear fnlv55ds;
064700151124             d55tpt='6'  ;
064800151124             d55lin = orgfil;
064900151124             d55drf=datcor ;
065000151124             fnlv55r(fnlv55ds);
065100151124 3           if d55err<>*blanks;
065200151124                wlnae=orgfil  ;
065300151124
065400151124       clear fnlrc4ds;
065500151124
065600151124       // Determino i Tipi di ControAlert da elaborare
065700151124       // Verifico se è già stato elaborato il 3° Controalert
065800151124       clear tibs02ds;
065900151124       clear dsdf      ;
066000151124       t02mod='C';
066100151124       t02cod='SDF';
066200151124       t02ke1=%editc(wlnae:'X');
066300151124       t02sif=knsif;
066400151124       tibs02r(kpjba:tibs02ds);
066500151124 4     if t02err=*blanks;
066600151124          dsdf=t02uni;
066700151124 4     endif;
066800151124            clear wsdfca3;
066900151124            monitor;
067000151124 4          if §sdfca3 >0;
067100151124               wsdfca3=§sdfca3;
067200151124  4         endif;
067300151124            on-error  ;
067400151124            endmon  ;
067500151124
067600151124       // Reperisco ora di sistema per i controlli seguenti
067700151124          orasys=%dec( %time() );
067800151124          orasys4=%int(%subst(%editc(orasys:'X'):1:4));
067900151124
068000151124       // 3° CONTROALERT GIà ELABORATO  -->
068100151124       //   rielaboro
068200151124  4    if wsdfca3>=datcor  ;
068300151124             ILRC4TCAL0='3';
068400151124             ILRC4LNA=wlnae;
068500151124             ILRC4DAT=datcor ;
068600151124             exsr conta      ;
068700151124  4          endif;
068800151124  3       endif                                   ;
068900151124  2       endif                                   ;
069000151124          read azorg01l;
069100151124  1    enddo;
069200151124
069300151124       /end-free
069400151124     c                   ENDSR
069500150928     OQSYSPRT   E            det         1
069600150928     O                       arbaas           +   1
069700150928     O                       arblnp           +   1
069800150928     O                       arbnrs           +   1
069900150928     O                       arbnsp           +   1
070000151006     O                       arblna           +   1
070100150928     O                       stampa           +   1
070200151002     O                       tipa             +   1
070300151124     OQSYSPRT   E            tot         1
070400151124     O                       mancac        2  +   1

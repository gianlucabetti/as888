000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000830     H* FNLR62R *----------------------------------------------------*
000300001024     H*   Rich.  Chiusura Distinta x SPEDIZIONI NON consegnate vecchie
000400000000     H*--------------------------------------------------------------*
000500920907     FTABEL00F  IF   E           K DISK
000600000831     FAZORG01L  IF   e           k disk
000700020919     FFnarb54L  IF   E           K DISK
000800020903     FFNLR62D   CF   E             WORKSTN SFILE(LR62S01:NRR1)
000900920907     D L1              S              3  0 DIM(30)                              FIL GESTITE
001000020903     D L6S             S              3  0 DIM(30)                              FIL GEST in £6
001100071128     D msg             S             70    DIM(11) ctdata perrcd(1)             FIL. COMODO
001200001207     D fip             S              3  0 DIM(50)                              FIL. COMODO
001300921201     D*
001400920907     D CNCR80        E DS
001500020506     d trul06ds      e ds
001600020506     d  lin                    1     90  0 dim(30)
001700920907     D KPJBA         E DS
001800900523     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001900020903     D fnlv55ds      e ds
002000000831     d og143         e ds
002100000830     d dcda          e ds
002200000912     d ddcb          e ds
002300000830     d tibs02        e ds                  extname(tibs02ds)
002400000830     d dataeur         s               d   datfmt(*eur)
002500001110     d dataeur1        s               d   datfmt(*eur)
002600001024     d wdataeur        s               d   datfmt(*eur)
002700000830     d dataiso         s               d   datfmt(*iso)
002800001110     d dataiso1        s               d   datfmt(*iso)
002900001024     d udateiso        s               d   datfmt(*iso)
003000001206     d udateisoi       s               d   datfmt(*iso)
003100001206     d wudatei         s               d   datfmt(*eur)
003200000831     d param           ds
003300000831     d  parfil                        3s 0
003400000831     d  paropz                        1
003500000831     d  pardat                        8s 0
003600000922     d  paranm                        1
003700001017     d  pardtf                        8s 0
003800001026     d  parcmc                        3
003900001206     d  parpog                        3s 0
004000001206     d  parnfv                        6s 0
004100001026     d norec           s              1
004200020903     d savfil          s                   like(vidfil)
004300001206      * DS PER RICERCA FOGLI
004400001206     D DSLR01        E DS                  INZ
004500900523     C****************************************************************
004600900523     C*  RIEPILOGO INDICATORI
004700900523     C***************************************************************
004800900523     C* 32    - DI COMODO RIUTILIZZABILI
004900900523     C* 90    - TROVATO ERRORE
005000900523     C*
005100900523     C*****************************************************************
005200000830     C                   move      *blank        vtipo
005300020903     c                   setoff                                       50
005400900509     C     FOR01         TAG
005500020903     C                   EXFMT     LR62D1
005600001019     C                   move      *blank        vmsger
005700020903     c                   setoff                                       4440
005800920309     C** CMD3 - FINE LAVORO
005900900509     C   KC              GOTO      FINE
006000020903     C** CMD18- CAMBIO FILIALE in gestione solo se profilo non è REM
006100020903     C   ks              if        simTPP = '1'
006200020903      * sproteggo il campo filiale in gestione
006300020903     c                   seton                                        50
006400020903     C                   GOTO      FOR01
006500020903     c                   endif
006600020903     c* carico £6 e po di gestione
006700020903     c                   if        vidfil <> savfil
006800020903     c                   exsr      sr£6
006900020903     c                   move      vidfil        savfil
007000020903     c                   end
007100020903     c*
007200020903     C     VIDFIL        IFNE      SIMFEL
007300020903     C     VIDFIL        LOOKUP    L1                                     30
007400020903     C  N30              SETON                                        40
007500020903     c  n30              movel     msg(6)        vmsger
007600020903     C  N30              GOTO      FOR01
007700020903     C*
007800020903     C* NON POSSO mettere una filiale che è in £6
007900020903     C*
008000020903     c                   if        vidfil <> d55tfa
008100020903     c                   seton                                        40
008200020903     c                   movel     msg(7)        vmsger
008300020903     C                   GOTO      FOR01
008400020903     C                   END
008500020903     C                   END
008600020903     C*
008700020903     c* controllo la scelta del tipo di pulizia
008800020903     c     vtipo         ifeq      ' '
008900020903     c                   seton                                        4440
009000020903     c                   movel     msg(1)        vmsger
009100020903     C                   GOTO      FOR01
009200020903     c                   end
009300071108     c* controllo se la scelta del tipo è 6 gestione bolle rimpalli sede
009400071108     c* la filiale che richiama può essere solo 101
009500071108     c                   if        vtipo = '6'and simpou <> 101
009600071108     c                   seton                                        4440
009700071108     c                   movel     msg(2)        vmsger
009800071108     C                   GOTO      FOR01
009900071108     c                   end
010000020903     c* sfl po
010100020903     c                   exsr      gess01
010200001024     C* In base alla scelta fatta calcolo fino a che data si deve pulire
010300000830     c                   exsr      tipodata
010400001024      * Se non ci sono errori emetto la videata con le date impostate
010500001024     C     formdate      tag
010600020903     C                   EXFMT     LR62D2
010700001207     C** CMD3 - Fine lavoro
010800001207     C   KC              GOTO      FINE
010900020903     C** CMD12- GUIDA
011000020903     C   KL              GOTO      FOR01
011100071128
011200071128      *  controllo la data limite
011300071128     c                   setoff                                       4041
011400001024     c     *eur          test(d)                 vdtlmt                 41
011500001024     c                   if        *in41
011600001024     c     *dmy          test(d)                 vdtlmt                 41
011700001024     c  n41*dmy          move      vdtlmt        dataeur
011800001024     c                   else
011900001024     c                   move      vdtlmt        dataeur
012000001024     c                   end
012100001024     c   41              seton                                        40
012200001024     c   41              movel     msg(10)       vmsger
012300001024     c   41              goto      formdate
012400001025     c                   move      dataeur       vdtlmt
012500001024      * Aggiungo i gg previsti in tabella per calcolare la data consegna forzata
012600001024     c     dataeur       adddur    §cdagc:*d     wdataeur
012700001024     c                   move      wdataeur      vdtfor
012800001024     c                   move      dataeur       dataiso
012900001024      * controllo se la data limite è superiore alla data del giorno
013000001110     c                   if        dataiso > dataiso1
013100000830     c                   seton                                        4140
013200001017     c                   movel     msg(4)        vmsger
013300000830     c                   end
013400941216     C  NKF
013500001024     COR 40              GOTO      formdate
013600001024
013700941216     C* F6 - ELABORAZIONE
013800001024
013900001024      * Non ci sono errori imposto i parametri
014000001024     c     *eur          move      vdtlmt        dataiso
014100001024     c                   move      dataiso       pardat
014200000831     c                   move      vtipo         paropz
014300001024     c     *eur          move      vdtfor        dataiso
014400001024     c                   move      dataiso       pardtf
014500071128     c                   move      *zeros        parnfv
014600071128     c                   z-add     0             parpog
014700000831     c                   movel(p)  param         kpjbu
014800000831     c                   call      'FNLR62C1'
014900000831     c                   parm                    kpjba
015000001026     c                   movel     kpjbu         norec
015100001026     c                   if        norec = 'A'
015200001026     c                   seton                                        40
015300001026     c                   movel     msg(11)       vmsger
015400001026     c                   goto      for01
015500001026     c                   endif
015600900522     C*
015700000000     C     FINE          TAG
015800000000     C                   SETON                                        LR
015900020903      *---------------------------------------------------------*
016000020903      *  Inizializzazione del primo subfile                     *
016100020903      *---------------------------------------------------------*
016200020903     c     inzs01        begsr
016300020903     C                   Z-ADD     1             NR1KEY
016400020903     C                   clear                   NRR1
016500020903      *
016600020903     C                   eval      *in20 = *off
016700020903     C                   eval      *in21 = *off
016800020903     C                   eval      *in28 = *off
016900020903     C                   WRITE     LR62C01
017000020903     C                   eval      *in20 = *on
017100020903     C                   eval      *in21 = *on
017200020903      *
017300020903      *  Carico linee di arrivo
017400020903     C                   DO        §F            X                 3 0
017500020903     C                   clear                   ORGFL1
017600020903     C     l6s(X)        CHAIN     AZORG01L
017700020903      *
017800020903      * imposta i campi della DS
017900020903     c                   If        %found
018000020903     C                   movel     ORGDE3        OG143
018100020903     C                   MOVEL     *blank        flgest            1
018200020903     C     §OgNTW        ifeq      'FED'
018300020903     C     §OgNTW        oreq      'EEX'
018400020903     C     §OgNTW        oreq      'EUP'
018500020903     C                   MOVEL     'E'           flgest
018600020903     c                   end
018700020903     c                   select
018800020903      *scelta bolle dpd
018900020903     c     vtipo         wheneq    '1'
019000020903     c     vtipo         oreq      '5'
019100020903     c                   if        §ogntw <> 'DPD'
019200020903     c                   iter
019300020903     c                   end
019400020903      *scelta bolle estere
019500020903     c     vtipo         wheneq    '3'
019600020903     c                   if        flgest <>'E'
019700020903     c                   iter
019800020903     c                   endif
019900020903
020000020903     c                   endsl
020100020919     c* controllo se il po ha delle bolle
020200020919     c     l6s(x)        setll     fnarb54l                               66
020300020919     c                   if        *in66
020400020903     C                   add       1             NRR1              4 0
020500020903     C                   clear                   V1CSCE
020600020903     C                   move      l6s(X)        V1CLNA
020700020903     C                   movel     ORGDES        V1DLNA
020800020903     C                   WRITE     LR62s01
020900020919     C                   ENDIF
021000020903      *
021100020903     C                   ENDIF
021200020903     C                   ENDDO
021300020903      *
021400020903     C                   ENDSR
021500020903      *---------------------------------------------------------*
021600020903      *  Gestione del subfile                                   *
021700020903      *---------------------------------------------------------*
021800020903     C     GESS01        BEGSR
021900020903     c                   exsr      inzs01
022000020903      *
022100020903     c                   select
022200020903     c* incongruenza tra filiale in gestione e tipo scelta
022300020903     c                   when      nrr1 = 0
022400020903     c                   seton                                        4440
022500020903     c                   movel     msg(2)        vmsger
022600020903     C                   GOTO      FOR01
022700020903     c* non emetto il sbf dei po se ho un po solo
022800020903     c                   when      nrr1 = 1
022900020903     c                   movel     vidfil        parfil
023000020903     c                   other
023100020903     c                   do        *hival
023200020903     C                   WRITE     LR62Z01
023300020903     C                   EXFMT     LR62C01
023400020903     C                   Z-ADD     1             NR1KEY
023500020903     C*  Fine Lavoro
023600020903     C     *INKC         IFEQ      '1'
023700020903     C                   GOTO      FINe
023800020903     C                   END
023900020903     C*  guida
024000020903     C     *INKl         IFEQ      '1'
024100020903     C                   GOTO      For01
024200020903     C                   END
024300020903     C*  Controlli
024400020903     C                   EXSR      CTR01
024500020903     c                   if        parfil <> 0
024600020903     c                   leave
024700020903     c                   end
024800020903     c                   enddo
024900020903     c                   endsl
025000020903     C*
025100020903     C                   ENDSR
025200020903      *---------------------------------------------------------*
025300020903      *  Controlli                                              *
025400020903      *---------------------------------------------------------*
025500020903     C     CTR01         BEGSR
025600020903     c                   clear                   parfil
025700020903     c*
025800020903     C                   READC     LR62S01                                32
025900020903     C     *IN32         DOWEQ     '0'
026000020903     C     V1CSCE        IFEQ      '1'
026100020903     C                   Z-ADD     V1CLNA        PARFIL
026200020903     C                   leave
026300020903     c                   end
026400020903     C                   SETOn                                        22
026500020903     C                   UPDATE    LR62S01
026600020903     C                   SETOFF                                       22
026700020903     C                   READC     LR62S01                                32
026800020903     C                   ENDdo
026900020903     C*
027000020903     C                   ENDSR
027100000830     c*------------------------------------------------
027200000830     c     tipodata      begsr
027300000830     c*------------------------------------------------
027400020903     C                   MOVEL     *ZERO         Vdtlmt
027500020903     C                   MOVEL     *ZERO         Vdtfor
027600000912     C                   CLEAR                   Ddcb
027700000830     C                   CLEAR                   Dcda
027800000830     C                   CLEAR                   TIBS02
027900020903     c                   setoff                                       4140
028000001024      * Cerco in tabella CDA i dati da proporre in base al tipo scelta
028100000830     C                   MOVEL     'C'           T02MOD
028200000830     C                   MOVEL     KNSIF         T02SIF
028300000830     C                   MOVEL     'CDA'         T02COD
028400000830     C                   MOVEL     vtipo         T02KE1
028500000830      *
028600000830     C                   CALL      'TIBS02R'
028700000830     C                   PARM                    KPJBA
028800000830     C                   PARM                    TIBS02
028900000830      *
029000001024     C     T02ERR        IFne      *BLANKS
029100001024     c* non ho trovato la tabella CDA
029200001024     c                   seton                                        4140
029300001024     c   41              movel     msg(8)        vmsger
029400001024     c   41              goto      endtpdata
029500001024     C                   else
029600000830     C                   MOVEL     T02UNI        Dcda
029700001024     c* da tabella calcolo fino a che data sped. propongo di fare la chiusura
029800001024     c     udateiso      subdur    §cdagg:*d     dataeur
029900001019     c
030000001024     c* da tabella determino la consegna anomala da proporre in chiusura
030100000922     c                   move      §cdanm        paranm
030200001026     c                   move      §cdacm        parcmc
030300001024     C                   ENDIF
030400001024     c
030500001024      * Se tipo scelta = a 4 "selezione per C.A." la data da proporre
030600001024      *                  viene presa dalla tabella delle C.A. - DCB
030700001017     c                   if        vtipo = '4'
030800001017     C                   MOVEL     'DCB'         T02COD
030900020903     C                   MOVEL     PARFIL        T02KE1
031000001017      *
031100001017     C                   CALL      'TIBS02R'
031200001017     C                   PARM                    KPJBA
031300001017     C                   PARM                    TIBS02
031400001024     C     T02ERR        IFne      *BLANKS
031500001024     c* non ho trovato la tabella DCB
031600001024     c                   seton                                        4140
031700001024     c   41              movel     msg(9)        vmsger
031800001024     c   41              goto      endtpdata
031900001024     C                   else
032000000912     C                   MOVEL     T02UNI        Ddcb
032100000912     c                   move      §dcbudta      dataiso
032200000912     c                   move      dataiso       dataeur
032300001017     c                   end
032400000912     c                   end
032500001024      *imposto la data limite
032600000830     c                   if        vdtlmt = 0
032700000830     c                   move      dataeur       vdtlmt
032800001024     c                   end
032900001024
033000000830     c     *eur          test(d)                 vdtlmt                 41
033100000830     c                   if        *in41
033200001024     c     *dmy          test(d)                 vdtlmt                 41
033300001024     c  n41*dmy          move      vdtlmt        dataeur
033400001017     c                   else
033500001024     c                   move      vdtlmt        dataeur
033600000830     c                   end
033700001024     c   41              seton                                        40
033800001024     c   41              movel     msg(10)       vmsger
033900001024     c   41              goto      endtpdata
034000001024      * Aggiungo i gg previsti in tabella per calcolare la data consegna forzata
034100001024     c     dataeur       adddur    §cdagc:*d     wdataeur
034200001024     c                   move      wdataeur      vdtfor
034300001110     c                   move      dataeur       dataiso1
034400000830     c
034500001024     c     endtpdata     endsr
034600001024     C**************************************************************************
034700001024     C* --  INZSR
034800001024     C**************************************************************************
034900001024     c     *inzsr        begsr
035000001024     C     *ENTRY        PLIST
035100001024     C                   PARM                    KPJBA
035200001024     C                   Z-ADD     1             CODUT
035300001024     C                   CALL      'X§PARUT'
035400001024     C                   PARM                    UT§DSE
035500001024     C                   MOVEL     RAGUT         RSUT
035600001024     C                   MOVEL     REC80         CNCR80
035700020904     c* se primo livello abilito il cambio del po
035800020904     c     simtpp        comp      '1'                                    60
035900001024     C*---------------------------------------------------------------*
036000001024     C     KTAB          KLIST
036100001024     C                   KFLD                    CODUT
036200001024     C                   KFLD                    COD
036300001024     C     KTAB2         KLIST
036400001024     C                   KFLD                    CODUT
036500001024     C                   KFLD                    COD               2
036600001024     C                   KFLD                    KEY               8
036700001207      *
036800001206     C                   Z-ADD     3             KNPG              1 0
036900001024     C* operazioni iniziali
037000001024     C                   EXSR      OPERAZ
037100001024     c                   endsr
037200020903     C**************************************************************************
037300020903     C* carica £6
037400020903     C**************************************************************************
037500020903     C     sr£6          BEGSR
037600020903     c* carico i po da me gestiti compreso me stesso
037700020903     c                   clear                   trul06ds
037800020903     c                   eval      d06cod = '£6'
037900020903     c                   movel     vidfil        d06key
038000020903     c                   movel     trul06ds      kpjbu
038100020903     c                   call      'TRUL06R'
038200020903     c                   parm                    kpjba
038300020903     c                   movel     kpjbu         trul06ds
038400020903     c                   movea     lin           l6s
038500020903     C                   Z-ADD     0             §F                3 0
038600020903     c                   Z-ADD     1             XZ                3 0
038700020903      * Deve contare quante sono le filiali gestite per caricare in seguito il sfl
038800020903     C     *zeros        LOOKUP    l6s(XZ)                                51
038900020903     C   51              if        XZ > 0
039000020903     c     xz            sub       1             §F
039100020903     c                   endif
039200020903     c* reperisco il po di gestione
039300020903     C*
039400020903     c                   clear                   fnlv55ds
039500020903     c                   eval      d55tpt = '6'
039600020903     c                   movel     vidfil        d55lin
039700020903     c                   movel     *date         dataiso
039800020903     c                   movel     dataiso       d55drf
039900020903     c                   call      'FNLV55R'
040000020903     c                   parm                    fnlv55ds
040100020903     c                   endsr
040200001024     C*
040300001024     C**************************************************************************
040400001024     C* OPERAZIONI INIZIALI
040500001024     C**************************************************************************
040600001024     C     OPERAZ        BEGSR
040700020903     C                   z-add     SimPOU        VIDFIL
040800020903     C* CARICO TABELLA FILIALI GESTITE £1
040900020506     c                   clear                   trul06ds
041000020506     c                   eval      d06cod = '£1'
041100020506     c                   movel     simfel        d06key
041200020506     c                   movel     trul06ds      kpjbu
041300020506     c                   call      'TRUL06R'
041400020506     c                   parm                    kpjba
041500020506     c                   movel     kpjbu         trul06ds
041600020506     c                   movea     lin           l1
041700001207     C* CARICO DA AZORG LE FILIALI POSTE PER TEST SUCCESSIVI
041800001207     C                   CLEAR                   FIP
041900001207     C     *LOVAL        SETLL     AZORG01L
042000001207     C                   DO        *HIVAL
042100001207     C                   READ      AZORG01L                               95
042200001207     C   95              LEAVE
042300001207     C                   MOVEL     ORGDE3        OG143
042400020729     C     §OgNTW        IFEQ      'PPT'
042500001207     C                   Z-ADD     1             X                 3 0
042600001207     C     *ZEROS        Lookup    FIP(X)                                 23
042700001207     C   23              Z-ADD     ORGFIL        FIP(X)
042800001207     C                   END
042900001207     C                   END
043000001024     C*
043100001024     C* RILEVO LA DATA DEL GIORNO
043200001024     C                   TIME                    WTIME            14 0          ORA E DATA
043300001024     C                   MOVE      WTIME         wudate            8 0
043400001024     C     *eur          MOVE      wudate        udateiso
043500001206     C     udateiso      subdur    1:*d          udateisoi
043600001206     C                   move      udateisoi     wudatei
043700001024     C*
043800001024     C                   ENDSR
043900001017**
044000001024Tipo selezione obbligatorio effettuare una scelta !!!                           1
044100001025Incongruenza Tipo Scelta e Filiale in Gestione                                  2
044200001024Incongruenza Tipo Scelta e Filiale in Gestione non Estera !                     3
044300001024Data richiesta oltre il limite ammesso !!!                                      4
044400001024La data forzatura deve essere minore uguale alla data limite                    5
044500001024Filiale da gestire errata !!!                                                   6
044600001024Filiale in gestione ad altra filiale: cambio non effettuabile                   7
044700001025Non trovata tabella CDA : Avvisare CED  !!!                                     8
044800001025Non trovata tabella DCB : Avvisare CED  !!!                                     9
044900001024Data errata                                                                    10
045000001027Non esistono record per il tipo di estrazione richiesto                        11

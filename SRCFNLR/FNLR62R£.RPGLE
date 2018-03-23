000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000830     H* FNLR62R *----------------------------------------------------*
000300001024     H*   Rich.  Chiusura Distinta x SPEDIZIONI NON consegnate vecchie
000400000000     H*--------------------------------------------------------------*
000500920907     FTABEL00F  IF   E           K DISK
000600000831     FAZORG01L  IF   e           k disk
000700001206     FFNFVV01L  IF   E           K DISK
000800070122     FFnbrv01L  IF   E           K DISK
000900020919     FFnarb54L  IF   E           K DISK
001000020903     FFNLR62D   CF   E             WORKSTN SFILE(LR62S01:NRR1)
001100920907     D L1              S              3  0 DIM(30)                              FIL GESTITE
001200020903     D L6S             S              3  0 DIM(30)                              FIL GEST in £6
001300001207     D msg             S             70    DIM(15) ctdata perrcd(1)             FIL. COMODO
001400001207     D fip             S              3  0 DIM(50)                              FIL. COMODO
001500921201     D*
001600920907     D CNCR80        E DS
001700020506     d trul06ds      e ds
001800020506     d  lin                    1     90  0 dim(30)
001900920907     D KPJBA         E DS
002000900523     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002100020903     D fnlv55ds      e ds
002200000831     d og143         e ds
002300000830     d dcda          e ds
002400000912     d ddcb          e ds
002500000830     d tibs02        e ds                  extname(tibs02ds)
002600000830     d dataeur         s               d   datfmt(*eur)
002700001110     d dataeur1        s               d   datfmt(*eur)
002800001024     d wdataeur        s               d   datfmt(*eur)
002900000830     d dataiso         s               d   datfmt(*iso)
003000001110     d dataiso1        s               d   datfmt(*iso)
003100001024     d udateiso        s               d   datfmt(*iso)
003200001206     d udateisoi       s               d   datfmt(*iso)
003300001206     d wudatei         s               d   datfmt(*eur)
003400000831     d param           ds
003500000831     d  parfil                        3s 0
003600000831     d  paropz                        1
003700000831     d  pardat                        8s 0
003800000922     d  paranm                        1
003900001017     d  pardtf                        8s 0
004000001026     d  parcmc                        3
004100001206     d  parpog                        3s 0
004200001206     d  parnfv                        6s 0
004300001026     d norec           s              1
004400020903     d savfil          s                   like(vidfil)
004500001206      * DS PER RICERCA FOGLI
004600001206     D DSLR01        E DS                  INZ
004700900523     C****************************************************************
004800900523     C*  RIEPILOGO INDICATORI
004900900523     C***************************************************************
005000900523     C* 32    - DI COMODO RIUTILIZZABILI
005100900523     C* 90    - TROVATO ERRORE
005200900523     C*
005300900523     C*****************************************************************
005400000830     C                   move      *blank        vtipo
005500020903     c                   setoff                                       50
005600900509     C     FOR01         TAG
005700020903     C                   EXFMT     LR62D1
005800001019     C                   move      *blank        vmsger
005900020903     c                   setoff                                       4440
006000920309     C** CMD3 - FINE LAVORO
006100900509     C   KC              GOTO      FINE
006200020903     C** CMD18- CAMBIO FILIALE in gestione solo se profilo non è REM
006300020903     C   ks              if        simTPP = '1'
006400020903      * sproteggo il campo filiale in gestione
006500020903     c                   seton                                        50
006600020903     C                   GOTO      FOR01
006700020903     c                   endif
006800020903     c* carico £6 e po di gestione
006900020903     c                   if        vidfil <> savfil
007000020903     c                   exsr      sr£6
007100020903     c                   move      vidfil        savfil
007200020903     c                   end
007300020903     c*
007400020903     C     VIDFIL        IFNE      SIMFEL
007500020903     C     VIDFIL        LOOKUP    L1                                     30
007600020903     C  N30              SETON                                        40
007700020903     c  n30              movel     msg(6)        vmsger
007800020903     C  N30              GOTO      FOR01
007900020903     C*
008000020903     C* NON POSSO mettere una filiale che è in £6
008100020903     C*
008200020903     c                   if        vidfil <> d55tfa
008300020903     c                   seton                                        40
008400020903     c                   movel     msg(7)        vmsger
008500020903     C                   GOTO      FOR01
008600020903     C                   END
008700020903     C                   END
008800020903     C*
008900020903     c* controllo la scelta del tipo di pulizia
009000020903     c     vtipo         ifeq      ' '
009100020903     c                   seton                                        4440
009200020903     c                   movel     msg(1)        vmsger
009300020903     C                   GOTO      FOR01
009400020903     c                   end
009500071108     c* controllo se la scelta del tipo è 6 gestione bolle rimpalli sede
009600071108     c* la filiale che richiama può essere solo 101
009700071108     c                   if        vtipo = '6'and simpou <> 101
009800071108     c                   seton                                        4440
009900071108     c                   movel     msg(2)        vmsger
010000071108     C                   GOTO      FOR01
010100071108     c                   end
010200020903     c* sfl po
010300020903     c                   exsr      gess01
010400001024     C* In base alla scelta fatta calcolo fino a che data si deve pulire
010500000830     c                   exsr      tipodata
010600001024      * Se non ci sono errori emetto la videata con le date impostate
010700020903     C                   MOVEL     *ZERO         Vidpoi
010800020903     C                   MOVEL     *ZERO         Vidima
010900001024     C     formdate      tag
011000001206     c     vtipo         comp      '2'                                    81
011100020903     C                   EXFMT     LR62D2
011200001207     C** CMD3 - Fine lavoro
011300001207     C   KC              GOTO      FINE
011400020903     C** CMD12- GUIDA
011500020903     C   KL              GOTO      FOR01
011600001206      * controllo se richiesta opzione 2 l'immissione del P.O e foglio IPP
011700020903     c                   setoff                                       464748
011800020903     c                   setoff                                       524041
011900020903     c                   if        *in81
012000001206     c                   exsr      contrpo
012100001206     c   46              movel     msg(12)       vmsger
012200001206     c   47              movel     msg(13)       vmsger
012300001206     c   48              movel     msg(14)       vmsger
012400001207     c   52              movel     msg(15)       vmsger
012500001206     c   40              goto      formdate
012600001206     c                   endif
012700001024      *  controllo la data limite
012800001024     c     *eur          test(d)                 vdtlmt                 41
012900001024     c                   if        *in41
013000001024     c     *dmy          test(d)                 vdtlmt                 41
013100001024     c  n41*dmy          move      vdtlmt        dataeur
013200001024     c                   else
013300001024     c                   move      vdtlmt        dataeur
013400001024     c                   end
013500001024     c   41              seton                                        40
013600001024     c   41              movel     msg(10)       vmsger
013700001024     c   41              goto      formdate
013800001025     c                   move      dataeur       vdtlmt
013900001024      * Aggiungo i gg previsti in tabella per calcolare la data consegna forzata
014000001024     c     dataeur       adddur    §cdagc:*d     wdataeur
014100001024     c                   move      wdataeur      vdtfor
014200001024     c                   move      dataeur       dataiso
014300001024      * controllo se la data limite è superiore alla data del giorno
014400001110     c                   if        dataiso > dataiso1
014500000830     c                   seton                                        4140
014600001017     c                   movel     msg(4)        vmsger
014700000830     c                   end
014800941216     C  NKF
014900001024     COR 40              GOTO      formdate
015000001024
015100941216     C* F6 - ELABORAZIONE
015200001024
015300001024      *emissione videata segnalazione contrassegni per spedizioni poste
015400001024     c                   if        vtipo = '2'
015500001024     c                   do        *hival
015600001024     c                   exfmt     lr62d02
015700001024     c   kr              leave
015800001025     c   kl              goto      formdate
015900001024     c                   end
016000001024     c                   end
016100001024      * Non ci sono errori imposto i parametri
016200001024     c     *eur          move      vdtlmt        dataiso
016300001024     c                   move      dataiso       pardat
016400000831     c                   move      vtipo         paropz
016500001024     c     *eur          move      vdtfor        dataiso
016600001024     c                   move      dataiso       pardtf
016700001206     c                   move      vidima        parnfv
016800001206     c                   z-add     vidpoi        parpog
016900000831     c                   movel(p)  param         kpjbu
017000000831     c                   call      'FNLR62C1'
017100000831     c                   parm                    kpjba
017200001026     c                   movel     kpjbu         norec
017300001026     c                   if        norec = 'A'
017400001026     c                   seton                                        40
017500001026     c                   movel     msg(11)       vmsger
017600001026     c                   goto      for01
017700001026     c                   endif
017800900522     C*
017900000000     C     FINE          TAG
018000000000     C                   SETON                                        LR
018100020903      *---------------------------------------------------------*
018200020903      *  Inizializzazione del primo subfile                     *
018300020903      *---------------------------------------------------------*
018400020903     c     inzs01        begsr
018500020903     C                   Z-ADD     1             NR1KEY
018600020903     C                   clear                   NRR1
018700020903      *
018800020903     C                   eval      *in20 = *off
018900020903     C                   eval      *in21 = *off
019000020903     C                   eval      *in28 = *off
019100020903     C                   WRITE     LR62C01
019200020903     C                   eval      *in20 = *on
019300020903     C                   eval      *in21 = *on
019400020903      *
019500020903      *  Carico linee di arrivo
019600020903     C                   DO        §F            X                 3 0
019700020903     C                   clear                   ORGFL1
019800020903     C     l6s(X)        CHAIN     AZORG01L
019900020903      *
020000020903      * imposta i campi della DS
020100020903     c                   If        %found
020200020903     C                   movel     ORGDE3        OG143
020300020903     C                   MOVEL     *blank        flgest            1
020400020903     C     §OgNTW        ifeq      'FED'
020500020903     C     §OgNTW        oreq      'EEX'
020600020903     C     §OgNTW        oreq      'EUP'
020700020903     C                   MOVEL     'E'           flgest
020800020903     c                   end
020900020903     c                   select
021000020903      *scelta bolle dpd
021100020903     c     vtipo         wheneq    '1'
021200020903     c     vtipo         oreq      '5'
021300020903     c                   if        §ogntw <> 'DPD'
021400020903     c                   iter
021500020903     c                   end
021600020903      *scelta bolle poste
021700020903     c     vtipo         wheneq    '2'
021800020903     c                   if        §ogntw = 'DPD' or
021900020903     c                             flgest = 'E'
022000020903     c                   iter
022100020903     c                   end
022200020903      *scelta bolle estere
022300020903     c     vtipo         wheneq    '3'
022400020903     c                   if        flgest <>'E'
022500020903     c                   iter
022600020903     c                   endif
022700020903
022800020903     c                   endsl
022900020919     c* controllo se il po ha delle bolle
023000020919     c     l6s(x)        setll     fnarb54l                               66
023100020919     c                   if        *in66
023200020903     C                   add       1             NRR1              4 0
023300020903     C                   clear                   V1CSCE
023400020903     C                   move      l6s(X)        V1CLNA
023500020903     C                   movel     ORGDES        V1DLNA
023600020903     C                   WRITE     LR62s01
023700020919     C                   ENDIF
023800020903      *
023900020903     C                   ENDIF
024000020903     C                   ENDDO
024100020903      *
024200020903     C                   ENDSR
024300020903      *---------------------------------------------------------*
024400020903      *  Gestione del subfile                                   *
024500020903      *---------------------------------------------------------*
024600020903     C     GESS01        BEGSR
024700020903     c                   exsr      inzs01
024800020903      *
024900020903     c                   select
025000020903     c* incongruenza tra filiale in gestione e tipo scelta
025100020903     c                   when      nrr1 = 0
025200020903     c                   seton                                        4440
025300020903     c                   movel     msg(2)        vmsger
025400020903     C                   GOTO      FOR01
025500020903     c* non emetto il sbf dei po se ho un po solo
025600020903     c                   when      nrr1 = 1
025700020903     c                   movel     vidfil        parfil
025800020903     c                   other
025900020903     c                   do        *hival
026000020903     C                   WRITE     LR62Z01
026100020903     C                   EXFMT     LR62C01
026200020903     C                   Z-ADD     1             NR1KEY
026300020903     C*  Fine Lavoro
026400020903     C     *INKC         IFEQ      '1'
026500020903     C                   GOTO      FINe
026600020903     C                   END
026700020903     C*  guida
026800020903     C     *INKl         IFEQ      '1'
026900020903     C                   GOTO      For01
027000020903     C                   END
027100020903     C*  Controlli
027200020903     C                   EXSR      CTR01
027300020903     c                   if        parfil <> 0
027400020903     c                   leave
027500020903     c                   end
027600020903     c                   enddo
027700020903     c                   endsl
027800020903     C*
027900020903     C                   ENDSR
028000020903      *---------------------------------------------------------*
028100020903      *  Controlli                                              *
028200020903      *---------------------------------------------------------*
028300020903     C     CTR01         BEGSR
028400020903     c                   clear                   parfil
028500020903     c*
028600020903     C                   READC     LR62S01                                32
028700020903     C     *IN32         DOWEQ     '0'
028800020903     C     V1CSCE        IFEQ      '1'
028900020903     C                   Z-ADD     V1CLNA        PARFIL
029000020903     C                   leave
029100020903     c                   end
029200020903     C                   SETOn                                        22
029300020903     C                   UPDATE    LR62S01
029400020903     C                   SETOFF                                       22
029500020903     C                   READC     LR62S01                                32
029600020903     C                   ENDdo
029700020903     C*
029800020903     C                   ENDSR
029900000830     c*------------------------------------------------
030000000830     c     tipodata      begsr
030100000830     c*------------------------------------------------
030200020903     C                   MOVEL     *ZERO         Vdtlmt
030300020903     C                   MOVEL     *ZERO         Vdtfor
030400000912     C                   CLEAR                   Ddcb
030500000830     C                   CLEAR                   Dcda
030600000830     C                   CLEAR                   TIBS02
030700020903     c                   setoff                                       4140
030800001024      * Cerco in tabella CDA i dati da proporre in base al tipo scelta
030900000830     C                   MOVEL     'C'           T02MOD
031000000830     C                   MOVEL     KNSIF         T02SIF
031100000830     C                   MOVEL     'CDA'         T02COD
031200000830     C                   MOVEL     vtipo         T02KE1
031300000830      *
031400000830     C                   CALL      'TIBS02R'
031500000830     C                   PARM                    KPJBA
031600000830     C                   PARM                    TIBS02
031700000830      *
031800001024     C     T02ERR        IFne      *BLANKS
031900001024     c* non ho trovato la tabella CDA
032000001024     c                   seton                                        4140
032100001024     c   41              movel     msg(8)        vmsger
032200001024     c   41              goto      endtpdata
032300001024     C                   else
032400000830     C                   MOVEL     T02UNI        Dcda
032500001024     c* da tabella calcolo fino a che data sped. propongo di fare la chiusura
032600001024     c     udateiso      subdur    §cdagg:*d     dataeur
032700001019     c
032800001024     c* da tabella determino la consegna anomala da proporre in chiusura
032900000922     c                   move      §cdanm        paranm
033000001026     c                   move      §cdacm        parcmc
033100001024     C                   ENDIF
033200001024     c
033300001024      * Se tipo scelta = a 4 "selezione per C.A." la data da proporre
033400001024      *                  viene presa dalla tabella delle C.A. - DCB
033500001017     c                   if        vtipo = '4'
033600001017     C                   MOVEL     'DCB'         T02COD
033700020903     C                   MOVEL     PARFIL        T02KE1
033800001017      *
033900001017     C                   CALL      'TIBS02R'
034000001017     C                   PARM                    KPJBA
034100001017     C                   PARM                    TIBS02
034200001024     C     T02ERR        IFne      *BLANKS
034300001024     c* non ho trovato la tabella DCB
034400001024     c                   seton                                        4140
034500001024     c   41              movel     msg(9)        vmsger
034600001024     c   41              goto      endtpdata
034700001024     C                   else
034800000912     C                   MOVEL     T02UNI        Ddcb
034900000912     c                   move      §dcbudta      dataiso
035000000912     c                   move      dataiso       dataeur
035100001017     c                   end
035200000912     c                   end
035300001024      *imposto la data limite
035400000830     c                   if        vdtlmt = 0
035500000830     c                   move      dataeur       vdtlmt
035600001024     c                   end
035700001024
035800000830     c     *eur          test(d)                 vdtlmt                 41
035900000830     c                   if        *in41
036000001024     c     *dmy          test(d)                 vdtlmt                 41
036100001024     c  n41*dmy          move      vdtlmt        dataeur
036200001017     c                   else
036300001024     c                   move      vdtlmt        dataeur
036400000830     c                   end
036500001024     c   41              seton                                        40
036600001024     c   41              movel     msg(10)       vmsger
036700001024     c   41              goto      endtpdata
036800001024      * Aggiungo i gg previsti in tabella per calcolare la data consegna forzata
036900001024     c     dataeur       adddur    §cdagc:*d     wdataeur
037000001024     c                   move      wdataeur      vdtfor
037100001110     c                   move      dataeur       dataiso1
037200000830     c
037300001024     c     endtpdata     endsr
037400001206     c*------------------------------------------------
037500001206     c     contrpo       begsr
037600001206     c*------------------------------------------------
037700001206      **
037800001206      * P.O. in L1
037900001206      *
038000001206     c                   if        vidpoi = 0 or
038100001206     c                             vidima = *blank
038200001206     c                   seton                                        4840
038300001206     c                   goto      endctr
038400001206     c                   endif
038500001206     C     VIDPOI        IFNE      *ZEROS
038600001206     C     VIDPOI        LOOKUP    L1                                     31
038700001206     C  N31              SETON                                        4740
038800001206     C  N31              GOTO      ENDCTR
038900001206     C                   ENDIF
039000001206      *
039100001206      * Obbligatorietà P.O. se immesso foglio
039200001206      *
039300001206     C     VIDPOI        IFEQ      *ZEROS
039400001206     C     VIDIMA        IFNE      *BLANKS
039500001206     C     VIDIMA        ANDNE     *ZEROS
039600001206     C                   SETON                                        4740
039700001206     C                   GOTO      ENDCTR
039800001206     C                   ENDIF
039900001206     C                   ENDIF
040000001206      *
040100001206      * Obbligatorietà foglio se immesso P.O.
040200001206      *
040300001206     C     VIDPOI        IFNE      *ZEROS
040400001206     C     VIDIMA        IFEQ      *BLANKS
040500001206     C     VIDIMA        OREQ      *ZEROS
040600001206     C                   SETON                                        4640
040700001206     C                   GOTO      ENDCTR
040800001206     C                   ENDIF
040900001206     C                   ENDIF
041000001206      *
041100001206      * Richiamo programma interrogazione fogli se "?"
041200001206      *
041300001206     C     '?'           SCAN      VIDIMA                                 90
041400001206     C     *IN90         IFEQ      *ON
041500001206     C                   CLEAR                   DSLR01
041600001206     C                   Z-ADD     VIDPOI        DLRFGS                         P.O. FOGLIO
041700001206     C                   MOVEL     'S'           DLRRIC                         PGM.RICHIAMATO
041800001206     C                   MOVEL     KNPG          DLRNPG                         CATEGORIA 3
041900020919     C                   move      wudatei       DLRGAL                         MENO 2 GIORNI G/M/A
042000020919     C                   move      udateisoi     DLRAAL                         MENO 2 GIORNI A/M/G
042100001206     C                   MOVEL     'FNLR52R'     DLRPGM                         PROGRAMMA
042200001206     C                   MOVEL     DSLR01        KPJBU
042300001206     C                   CALL      'FNLR02R'
042400001206     C                   PARM                    KPJBA
042500001206     C                   MOVEL     KPJBU         DSLR01
042600001206     C                   MOVE      *BLANKS       VIDIMA                         (lungo 6)
042700001206     C     DLRNFV        IFNE      *ZEROS
042800001206     C                   MOVE      *ALL'0'       VIDIMA
042900001206     C                   MOVE      DLRNFV        VIDIMA                         NUMERO FOGLIO
043000001206     C                   ENDIF
043100001206     C                   GOTO      ENDCTR
043200001206     C                   ENDIF
043300001206      *
043400001206      * Controlli sul file fogli
043500001207     c                   move      *blank        spunta            1
043600001206      *
043700001206     C     VIDIMA        IFNE      *ZEROS
043800001206     C     VIDIMA        ANDNE     *BLANKS
043900001206     C                   TESTN                   VIDIMA               31
044000001206     C                   MOVE      VIDIMA        W001A             1
044100001206     C     *IN31         IFEQ      *OFF
044200001206     C     W001A         ORLT      '0'
044300001206     C                   SETON                                        4640
044400001206     C                   GOTO      ENDCTR
044500001206     C                   ENDIF
044600001206     C                   MOVE      VIDIMA        KNFV
044700001206     C     KFVV          CHAIN     FNFVV01L                           31
044800001206     C     *IN31         IFEQ      *ON
044900001206     C     FVVATB        ORNE      ' '
045000001206     C                   SETON                                        4640
045100001206     C                   GOTO      ENDCTR
045200001206     C                   ENDIF
045300001206     C     FVVSPG        IFNE      'A'
045400001206     C                   SETON                                        4640
045500001206     C                   GOTO      ENDCTR
045600001206     C                   ENDIF
045700001207      * legge spunte su foglio selezionato per verificare presenza almeno
045800001207      * di una spunta su una bolla poste
045900001207     c                   z-add     knfv          knfv6
046000070122     c     kbrv          setll     fnbrv01l
046100001207     c                   do        *hival
046200070122     c     kbrv          reade     fnbrv01l                               51
046300001207     c   51              leave
046400001207     c     brvlnp        lookup    fip                                    51
046500001207     c   51              move      'X'           spunta
046600001207     c   51              leave
046700001207     c                   enddo
046800001207     c                   if        spunta = *blank
046900001207     c                   seton                                        5240
047000001207     C                   GOTO      ENDCTR
047100001207     c                   endif
047200001206     C                   ENDIF
047300001206     c     endctr        endsr
047400001024     C**************************************************************************
047500001024     C* --  INZSR
047600001024     C**************************************************************************
047700001024     c     *inzsr        begsr
047800001024     C     *ENTRY        PLIST
047900001024     C                   PARM                    KPJBA
048000001024     C                   Z-ADD     1             CODUT
048100001024     C                   CALL      'X§PARUT'
048200001024     C                   PARM                    UT§DSE
048300001024     C                   MOVEL     RAGUT         RSUT
048400001024     C                   MOVEL     REC80         CNCR80
048500020904     c* se primo livello abilito il cambio del po
048600020904     c     simtpp        comp      '1'                                    60
048700001024     C*---------------------------------------------------------------*
048800001024     C     KTAB          KLIST
048900001024     C                   KFLD                    CODUT
049000001024     C                   KFLD                    COD
049100001024     C     KTAB2         KLIST
049200001024     C                   KFLD                    CODUT
049300001024     C                   KFLD                    COD               2
049400001024     C                   KFLD                    KEY               8
049500001206      *
049600001206     C     KFVV          KLIST
049700001206     C                   KFLD                    KNPG              1 0
049800001206     C                   KFLD                    KNFV              5 0
049900001206     C                   KFLD                    VIDPOI
050000001206      *
050100001207     C     Kbrv          KLIST
050200001207     C                   KFLD                    KNPG              1 0
050300001207     C                   KFLD                    KNFV6             6 0
050400001207     C                   KFLD                    VIDPOI
050500001207      *
050600001206     C                   Z-ADD     3             KNPG              1 0
050700001024     C* operazioni iniziali
050800001024     C                   EXSR      OPERAZ
050900001024     c                   endsr
051000020903     C**************************************************************************
051100020903     C* carica £6
051200020903     C**************************************************************************
051300020903     C     sr£6          BEGSR
051400020903     c* carico i po da me gestiti compreso me stesso
051500020903     c                   clear                   trul06ds
051600020903     c                   eval      d06cod = '£6'
051700020903     c                   movel     vidfil        d06key
051800020903     c                   movel     trul06ds      kpjbu
051900020903     c                   call      'TRUL06R'
052000020903     c                   parm                    kpjba
052100020903     c                   movel     kpjbu         trul06ds
052200020903     c                   movea     lin           l6s
052300020903     C                   Z-ADD     0             §F                3 0
052400020903     c                   Z-ADD     1             XZ                3 0
052500020903      * Deve contare quante sono le filiali gestite per caricare in seguito il sfl
052600020903     C     *zeros        LOOKUP    l6s(XZ)                                51
052700020903     C   51              if        XZ > 0
052800020903     c     xz            sub       1             §F
052900020903     c                   endif
053000020903     c* reperisco il po di gestione
053100020903     C*
053200020903     c                   clear                   fnlv55ds
053300020903     c                   eval      d55tpt = '6'
053400020903     c                   movel     vidfil        d55lin
053500020903     c                   movel     *date         dataiso
053600020903     c                   movel     dataiso       d55drf
053700020903     c                   call      'FNLV55R'
053800020903     c                   parm                    fnlv55ds
053900020903     c                   endsr
054000001024     C*
054100001024     C**************************************************************************
054200001024     C* OPERAZIONI INIZIALI
054300001024     C**************************************************************************
054400001024     C     OPERAZ        BEGSR
054500020903     C                   z-add     SimPOU        VIDFIL
054600020903     C* CARICO TABELLA FILIALI GESTITE £1
054700020506     c                   clear                   trul06ds
054800020506     c                   eval      d06cod = '£1'
054900020506     c                   movel     simfel        d06key
055000020506     c                   movel     trul06ds      kpjbu
055100020506     c                   call      'TRUL06R'
055200020506     c                   parm                    kpjba
055300020506     c                   movel     kpjbu         trul06ds
055400020506     c                   movea     lin           l1
055500001207     C* CARICO DA AZORG LE FILIALI POSTE PER TEST SUCCESSIVI
055600001207     C                   CLEAR                   FIP
055700001207     C     *LOVAL        SETLL     AZORG01L
055800001207     C                   DO        *HIVAL
055900001207     C                   READ      AZORG01L                               95
056000001207     C   95              LEAVE
056100001207     C                   MOVEL     ORGDE3        OG143
056200020729     C     §OgNTW        IFEQ      'PPT'
056300001207     C                   Z-ADD     1             X                 3 0
056400001207     C     *ZEROS        Lookup    FIP(X)                                 23
056500001207     C   23              Z-ADD     ORGFIL        FIP(X)
056600001207     C                   END
056700001207     C                   END
056800001024     C*
056900001024     C* RILEVO LA DATA DEL GIORNO
057000001024     C                   TIME                    WTIME            14 0          ORA E DATA
057100001024     C                   MOVE      WTIME         wudate            8 0
057200001024     C     *eur          MOVE      wudate        udateiso
057300001206     C     udateiso      subdur    1:*d          udateisoi
057400001206     C                   move      udateisoi     wudatei
057500001024     C*
057600001024     C                   ENDSR
057700001017**
057800001024Tipo selezione obbligatorio effettuare una scelta !!!                           1
057900001025Incongruenza Tipo Scelta e Filiale in Gestione                                  2
058000001024Incongruenza Tipo Scelta e Filiale in Gestione non Estera !                     3
058100001024Data richiesta oltre il limite ammesso !!!                                      4
058200001024La data forzatura deve essere minore uguale alla data limite                    5
058300001024Filiale da gestire errata !!!                                                   6
058400001024Filiale in gestione ad altra filiale: cambio non effettuabile                   7
058500001025Non trovata tabella CDA : Avvisare CED  !!!                                     8
058600001025Non trovata tabella DCB : Avvisare CED  !!!                                     9
058700001024Data errata                                                                    10
058800001027Non esistono record per il tipo di estrazione richiesto                        11
058900001206Foglio IPP errato                                                              12
059000001206Punto operativo errato                                                         13
059100001206Punto operativo e numero foglio IPP Sono OBBLIGATORI !!!                       14
059200001207Il foglio immesso non contiene spunte poste VERIFICARLO !!!                    14

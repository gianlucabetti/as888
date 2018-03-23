000100950523     H DECEDIT('0,') DATEDIT(*DMY.)
000200101216     h dftactgrp(*no) actgrp(*caller)
000300141020     H* Fnlrq9R *-----------------------------------------------------*
000400101209     H*            sistema DISTINTE CONSEGNA
000500930225     H*---------------------------------------------------------------*
000600141020     FFnlrq9D   CF   E             WORKSTN
000700141024     Ffidst04l  iF   E           K DISK
000800141029     Ffidst01l  uF   E           K DISK    rename(fidst000:fidst1) commit
000900141029     Ffnfvv01l  uF   E           K DISK    commit
001000110210     Ffiapd01L  IF   E           K DISK
001100110103     D fidstds       E DS                  extname(fidst00f)
001200141020     d fnlrq3ds      e ds
001300141020     d fnlrq9ds      e ds
001400141020     D fidg37ds      E DS
001500141020     D DSLR01        E DS
001600050519     D* DS PER APDFLR  - filler anagrafica  autotrasp.
001700050519     D dapdflr       E DS
001800141022     D* DS PER stampanti
001900141022     D trul90ds      E DS
002000050519     D* DS PER FNLV24R - ricerca autotrasp.
002100050519     D fnlv24ds      E DS
002200021203     D* DS PER FNLV50R - CONTROLLO FILIALE GESTIONE
002300021203     D DSLV50        E DS                  EXTNAME(FNLV50DS)
002400101209     d*
002500101209     D KPJBA         E DS
002600101209     d* Parametri x Controllo profilo utenti
002700101209     d TIBS34ds      e ds
002800101209     d AZUTEds       e ds                  extname(AZUTE00F)
002900101209     d* Ds per dati organigramma
003000101209     d DDatiUte      e ds
003100141029     d TRULVPODS     e ds
003200141029     d   sk_PDA               16    765    inz   dim(250)
003300950524     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
003400950524     D WLBDAT          DS                  INZ
003500950524     D  G02DAT                 1      8  0
003600950524     D  G02INV                 9     16  0
003700950524     D  G02ERR                17     17
003800950524     D  G02TGI                18     22  0
003900950523     D                 DS
004000950523     D  V1CFPD                 1      3  0
004100950523     D  V1CCPC                 4      7
004200950523     D  WPDR                   1      7
004300141023     d wdst            s                   like(dstnfv)
004400101209     D* DEFINIZIONE SCHIERE
004500101209     D MSG             S             78    DIM(07) CTDATA PERRCD(1)             MESSAGGI ERRORE
004600141022     D CMA4f6          S              1    DIM(130) CTDATA PERRCD(65)
004700141022     D CMA5f6          S              1    DIM(130) CTDATA PERRCD(65)
004800141022     D CMETf6          S              1    DIM(130) CTDATA PERRCD(65)
004900061002     C**********************************************************************
005000110210     C                   CLEAR                   V1CNFV
005100950525     C                   CLEAR                   V1CCPC
005200141020     C                   CLEAR                   fnlrq9ds
005300141029     c* ricerca P.O. abilitati da tabella VPO per step di test
005400141029     c                   clear                   trulvpods
005500141029     c                   eval      ivpoke1 = 'DECOFI816DIR'
005600141029     c                   call      'TRULVPOR'
005700141029     c                   parm                    trulvpods
005800141029     c                   move      '999'         fgsa              3
005900141029     c     fgsa          lookup    sk_pda                                 77
006000141029     c  n77              move      dutpou        fgsa
006100141029     c  n77fgsa          lookup    sk_pda                                 77
006200141029     c                   if        not *in77
006300141029     c                   exfmt     videostop
006400141029     c                   goto      fine
006500141029     c                   endif
006600950524     C* EMISSIONE FORMATO
006700060929     C                   do        *hival
006800110104     c* ricarica il sfl degli errori senza riemettre la richiesta della
006900110104     c* distinta
007000141020     c                   if        rq9cmdr <> '05'
007100950523     C                   EXFMT     LRF0D01
007200950523     C                   CLEAR                   V1CMSG
007300141103     C                   SETOFF                                       284030
007400950525     C                   SETOFF                                       414243
007500101209     C   kc              leave
007600141029      * comando di conferma chiusura foglio senza più spedizioni all'interno
007700141029     c                   if        v1cchiu = 'S' and *inkf
007800141029     c     kdst          chain     fidst01l
007900141029     c                   if        %found(fidst01l)
008000141029     c                   move      'S'           dstfcf
008100141029     c                   update    fidst1
008200141029     c                   z-add     dstnfv        fvvnfv
008300141029     c     kfvv          chain     fnfvv01l
008400141029     c                   if        %found(fnfvv01l)
008500141029     c                   move      'S'           fvvfcf
008600141029     c                   update    fnfvv000
008700141126     c                   z-add     8             dstnpg
008800141029     c     kfvv          chain     fnfvv01l
008900141029     c                   move      'S'           fvvfcf
009000141029     c                   update    fnfvv000
009100141029     c                   endif
009200141029     c                   endif
009300141029     c                   setoff                                       30
009400141029     c                   clear                   v1cchiu
009500141029     c                   leave
009600141029     c                   endif
009700110202     C** CMD18 - CAMBIO FILIALE
009800950526    1C     *INKS         IFEQ      *ON
009900950526     C                   SETON                                        04
010000060929     C                   iter
010100950526    1C                   ENDIF
010200950523     C* 04 ON  - CONTROLLO FILIALE IN GESTIONE
010300950524    1C     *IN04         IFEQ      *ON
010400110103     C                   MOVEL     V1CFGS        WFIL              3 0
010500950523     C                   EXSR      CTRFGS
010600060929     C   28              iter
010700950524    1C                   ENDIF
010800950523     C* CONTROLLI VIDEATA
010900950523     C                   EXSR      CTRD01
011000950524     C   28
011100060929     COR 90              iter
011200110202    1C                   ENDIF
011300110202     c*
011400101209     c                   exsr      srges01
011500110104     c                   select
011600101220     c* comando fine
011700141020     c                   when      rq9cmdr = '03'
011800101220     c                   leave
011900101220     c* se errore
012000141020     c                   when      rq9msgo <> ' '
012100141029     c                   eval      v1cchiu= 'S'
012200141029     c                   seton                                        2830
012300141020     c                   eval      v1cmsg = rq9msgo
012400110104     c* rivisualizza
012500141020     c                   when      rq9cmdr = '05'
012600110104     c* se tutto ok
012700141020     c                   when      rq9msgo = ' '
012800141020     c                   exsr      srfidg37
012900110104     c                   exsr      srinz
013000110104     c                   endsl
013100060929     c                   enddo
013200101216     c*
013300141105     c                   if        v1cnfv <> *blank and
013400141105     c                             v1cnfv <> *zeros
013500141103     C     '?'           SCAN      V1CNFV                                 90
013600141103     c  n90              exsr      srfidg37
013700141028     c                   endif
013800141029     c
013900110202     c                   commit
014000141029     c     fine          tag
014100101216     C                   SETON                                        LR
014200141020     C*-----------------------------------------------------*
014300141020     C* aggiorna pesi/volumi/fermate assegnate x fidst00f   *
014400141020     C*-----------------------------------------------------*
014500141020     C     srfidg37      BEGSR
014600141020     c*
014700141020     c                   clear                   fidg37ds
014800141020     c                   eval      D37FGSI = dstfgs
014900141020     c                   eval      D37DFVI = dstdfv
015000141020     c                   eval      D37NFVI = dstnfv
015100141020     c                   eval      D37cmti = 'N'
015200141020     c                   eval      kpjbu = fidg37ds
015300141020     c                   call      'FIDG37R'
015400141020     c                   parm                    kpjba
015500141020     c                   eval      fidg37ds = kpjbu
015600141020     c*
015700141020     C                   ENDSR
015800060929     C**********************************************************************
015900101216     C*-- pulisco i dati a video
016000060929     C**********************************************************************
016100101216     C     srinz         BEGSR
016200110210     C                   CLEAR                   V1CNFV
016300101216     C                   CLEAR                   V1CCPC
016400101216     C                   CLEAR                   V1CMSG
016500101216     C                   SETOFF                                       2840
016600101216     C                   SETOFF                                       414243
016700101216     c                   endsr
016800101216     C**********************************************************************
016900101216     C*-- richiamo gestione errori
017000101216     C**********************************************************************
017100101216     C     srges01       BEGSR
017200101216     c*
017300141020     C                   CLEAR                   FNLRq9ds
017400141020     C                   MOVE      V1CFGS        RQ9fgsi
017500141020     C                   MOVE      V1CNFV        RQ9ndci
017600141021     C                   MOVE      wpdr          RQ9pdr
017700141020     C                   MOVE      dstdfv        RQ9dfvI
017800141020     C                   MOVE      'S'           RQ9CMT
017900141022     C                   MOVE      d90Mdb        RQ9MOB
018000141022     C                   MOVE      d90PRB        RQ9PRB
018100141022     C                   MOVE      d90MdE        RQ9MOE
018200141022     C                   MOVE      d90PRE        RQ9PRE
018300141022     C                   MOVE      d90PSS        RQ9PSS
018400141022     C                   MOVE      d90MDB4       RQ9MDB4
018500141022     C                   MOVE      d90MDB5       RQ9MDB5
018600141022     C                   MOVE      d90BSL        RQ9BSL
018700141022     C                   MOVE      d90PSL        RQ9PSL
018800141022     C                   MOVE      d90PRB4       RQ9PRB4
018900141022     C                   MOVE      d90PRB5       RQ9PRB5
019000141027     c                   call      'FNLRQ10R'
019100101216     c                   parm                    kpjba
019200141020     c                   parm                    fnlrq9ds
019300110127     c* confermo le variazioni
019400101216     c                   endsr
019500101216     C**********************************************************************
019600101216     C*--- CONTROLLO LA FILIALE IN GESTIONE --------------------------*
019700101216     C**********************************************************************
019800101216     C     CTRFGS        BEGSR
019900950523     C                   CLEAR                   DSLV50
020000950523     C                   MOVEL     KNMUS         D50PRU
020100950523     C                   MOVEL     WFIL          D50FGS
020200950523     C                   CALL      'FNLV50R'
020300950523     C                   PARM                    DSLV50
020400950523     C*
020500950524    1C     D50ERR        IFNE      ' '
020600950523     C                   MOVEL     D50MSG        V1CMSG
020700950525     C                   SETON                                        4328
020800950524    1C                   ENDIF
020900950523     C*
021000060929     C                   ENDSR
021100060929     C**********************************************************************
021200950523     C*--- CONTROLLO DATI VIDEATA ------------------------------------*
021300060929     C**********************************************************************
021400950523     C     CTRD01        BEGSR
021500101209     C                   SETOFF                                         90
021600110211     c                   clear                   wpadr
021700950607     C****  CODICE PADRONCINO  ****
021800950607     C* RICERCA
021900950607     C     '?'           SCAN      V1CCPC                                 90
022000950607    1C     *IN90         IFEQ      *ON
022100021203     c                   clear                   fnlv24ds
022200021203     C                   Z-ADD     V1CFGS        d24FIL
022300021203     C                   MOVEL     'R'           d24FLG
022400021203     C                   MOVEL     'A'           d24tip
022500021203     C                   MOVEL(p)  fnlv24ds      KPJBU
022600950607     C                   CALL      'FNLV24R'
022700950607     C                   PARM                    KPJBA
022800021203     C                   MOVEL     KPJBU         fnlv24ds
022900950607     C* CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
023000021203     C     d24pdr        IFNE      0
023100021203     C                   MOVEL     d24pdr        WPDR
023200101216     C                   MOVEL     d24rsc        V1DPDR
023300110210     c* propongo l'ultima distinta dell'aut
023400110210     C                   eval      WPaDR = d24pdr
023500110210     c                   clear                   dstfcf
023600110210     c     kdst4         setgt     fidst04l
023700110210     c     kdst4         readpe    fidst04l
023800110210     c                   if        not %eof(fidst04l)
023900110210     c                   move      dstnfv        v1cnfv
024000110210     c                   else
024100110210     c                   clear                   v1cnfv
024200110210     c                   end
024300950607     C                   ELSE
024400950607     C                   CLEAR                   V1CCPC
024500950607     C                   ENDIF
024600110210     C                   leavesr
024700950607    1C                   ENDIF
024800950607     C* CONTROLLO VALIDITA' DEL CODICE PADRONCINO
024900950607    1C     V1CCPC        IFNE      *BLANKS
025000950607     C     V1CCPC        ANDNE     *ZEROS
025100950607     C                   TESTN                   V1CCPC               35
025200950607    2C     *IN35         IFEQ      *ON
025300950607     C                   MOVE      V1CCPC        W001              1
025400950607     C* CONTROLLO SE L'ULTIMO CARATTERE E' UNA LETTERA
025500950607    3C     W001          IFLT      '0'
025600950607     C                   SETOFF                                       35
025700950607    3C                   ENDIF
025800950607    2C                   ENDIF
025900950607    2C     *IN35         IFEQ      *OFF
026000101209     C                   MOVEL     MSG(06)       V1CMSG
026100950607     C                   SETON                                        4128
026200110210     C                   leavesr
026300950607    2C                   ENDIF
026400950607     C* CONTROLLO SE ESISTE NELL'ANAGRAFICA PADRONCINI
026500141023     C                   MOVE(P)   WPDR          WPADR
026600061002     C     kapdw         CHAIN     fiapd01l
026700061002     c                   if        not %found(fiapd01l) or
026800061002     c                             (%found(fiapd01l) and apdatb <> ' ')
026900101209     C                   MOVEL     MSG(06)       V1CMSG
027000950607     C                   SETON                                        4128
027100110210     C                   leavesr
027200070123     c                   else
027300141023     c                   if        apdpdd <> 'S'
027400141023     C                   MOVEL     MSG(04)       V1CMSG
027500141023     C                   SETON                                        4128
027600141023     C                   leavesr
027700141023     c                   endif
027800070123     c                   eval      dapdflr = apdflr
027900950607    1C                   ENDIF
028000061003    1C                   ENDIF
028100110103     C****  NUMERO DISTINTA  ****
028200110103     C* RICERCA
028300950523     C     '?'           SCAN      V1CNFV                                 90
028400950523    1C     *IN90         IFEQ      *ON
028500950523     C                   CLEAR                   DSLR01
028600950523     C                   Z-ADD     DATEU8        DLRADA
028700950523     C                   Z-ADD     V1CFGS        DLRFGS
028800141020     C                   MOVEL     'FNLRQ9R'     DLRPGM
028900950523     C                   MOVEL     'S'           DLRRIC
029000950523     C                   MOVEL     '2'           DLRTFV
029100950523     C                   Z-ADD     4             DLRNPG
029200950523     C                   MOVEL     DSLR01        KPJBU
029300070223     C                   CALL      'FNLR02R'
029400061228     C                   PARM                    KPJBA
029500950523     C                   MOVEL     KPJBU         DSLR01
029600070627     c                   movel     *all'0'       v1cnfv
029700070627     C                   move      DLRNFV        v1cnfv
029800110210     C                   leavesr
029900950523    1C                   ENDIF
030000110211     C* se gestione manuale
030100110211     c                   if        not *in36
030200950523    1C     V1CNFV        IFNE      *BLANKS
030300950523     C     V1CNFV        ANDNE     *ZEROS
030400950523     C                   TESTN                   V1CNFV               35
030500950523    2C     *IN35         IFEQ      *ON
030600950523     C                   MOVE      V1CNFV        W001
030700950523     C* CONTROLLO SE L'ULTIMO CARATTERE E' UNA LETTERA
030800950523    3C     W001          IFLT      '0'
030900950523     C                   SETOFF                                       35
031000950523    3C                   ENDIF
031100950523    2C                   ENDIF
031200950523    2C     *IN35         IFEQ      *OFF
031300950524     C                   MOVEL     MSG(1)        V1CMSG
031400950524     C                   SETON                                        4028
031500110210     C                   leavesr
031600950523    2C                   ENDIF
031700110211    2C                   ENDIF
031800110211     c                   end
031900110210     c* propongo l'ultima distinta dell'aut
032000110211     c                   if        v1cnfv = ' ' or v1cnfv =*zeros
032100110210     c                   clear                   dstfcf
032200110211     C                   MOVE(P)   WPDR          WPADR
032300110210     c     kdst4         setgt     fidst04l
032400110210     c     kdst4         readpe    fidst04l
032500110210     c                   if        not %eof(fidst04l)
032600110210     c                   move      dstnfv        v1cnfv
032700110210     c                   else
032800110210     C* NUMERO DISTINTA INESISTENTE
032900110202     C                   MOVEL     MSG(1)        V1CMSG
033000950524     C                   SETON                                        4028
033100110210     C                   leavesr
033200950524    3C                   ENDIF
033300110210    3C                   ENDIF
033400141023      *test autista
033500141024     c                   if        v1cnfv <> *blank and wpadr = 0
033600141023     C                   clear                   wdst
033700141023     C                   MOVE      v1cnfv        wdst
033800141029     c     kdst          chain(N)  fidst01l
033900141030     c                   if        %found(fidst01l)
034000141023     C                   MOVE      dstpdr        WPADR
034100141023     C     kapdw         CHAIN     fiapd01l
034200141023     c                   if        %found(fiapd01l) and
034300141023     c                             apdpdd <> 'S'
034400141023     C                   MOVEL     MSG(04)       V1CMSG
034500141023     C                   SETON                                        4128
034600141023     C                   leavesr
034700141023     c                   endif
034800141030     c                   else
034900141030     C                   MOVEL     MSG(01)       V1CMSG
035000141030     C                   SETON                                        4128
035100141030     C                   leavesr
035200141030     c                   endif
035300141023     c                   endif
035400110103     c* aggancio la distinta e faccio tutti i controlli
035500110103     c                   clear                   fnlrq3ds
035600110103     c                   clear                   fidstds
035700110103     c                   z-add     4             lrq3npgi
035800110103     c                   z-add     v1cfgs        lrq3fgsi
035900110103     c                   move      v1cnfv        lrq3ndci
036000110103     c                   eval      lrq3alci = 'S'
036100110103     c                   movel     fnlrq3ds      kpjbu
036200110103     c                   call      'FNLRQ3R'
036300110103     c                   parm                    kpjba
036400110103     c                   parm                    fidstds
036500110103     c                   movel     kpjbu         fnlrq3ds
036600110103     c                   if        lrq3erro <> ' '
036700110103     c                   eval      v1cmsg = lrq3msgo
036800110103     c                   seton                                        4028
036900110210     c                   leavesr
037000110210     c                   else
037100110210     c                   movel     dstpdr        wpdr
037200110103     c                   end
037300020910      * Il p.o. del padroncino deve essere uguale al p.o. in gestione
037400020910     c                   If        v1cfpd <> v1cfgs
037500101209     c                   Eval      V1cmsg = Msg(07)
037600020910     c                   Eval      *In41 = *On
037700020910     c                   Eval      *In28 = *On
037800110210     c                   leavesr
037900020910     c                   EndIf
038000061002     C*
038100080319     C                   ENDSR
038200141022     C**************************************************************************
038300141022      * RICHIEDO LE STAMPANTI per stampa LDV
038400141022     C**************************************************************************
038500141022     c     RICHSTAMPAN   BEGSR
038600141022     C                   CLEAR                   TRUL90DS
038700141022      *
038800141022     C                   MOVEL     'S'           d90RSB
038900141022     C                   MOVEL     'S'           d90RSE
039000141022     C                   CALL      'TRUL90R'
039100141022     C                   PARM                    KPJBA
039200141022     C                   PARM                    trul90DS
039300141022      *
039400141022     C* F3 - FINE
039500141022     c                   if        d90f3<>'1'
039600141022      *
039700141022      * OVRPRTF BOLLE A4
039800141022     C                   MOVEA     D90PRB4       CMA4f6(30)
039900141022     C                   MOVEA     D90MDB4       CMA4f6(52)
040000141022     C*
040100141022     C                   Z-ADD     130           LUNG             15 5
040200141022     C                   CLEAR                   COMMAN          130
040300141022     C                   MOVEA     CMA4f6        COMMAN
040400141022     C                   CALL      'QCMDEXC'                            91
040500141022     C                   PARM                    COMMAN
040600141022     C                   PARM                    LUNG
040700141022      *
040800141022     C                   if        not *in91
040900141022      *
041000141022      * OVRPRTF BOLLE A5
041100141022     C                   MOVEA     D90PRB5       CMA5f6(30)
041200141022     C                   MOVEA     D90MDB5       CMA5f6(52)
041300141022     C*
041400141022     C                   Z-ADD     130           LUNG
041500141022     C                   CLEAR                   COMMAN
041600141022     C                   MOVEA     CMA5f6        COMMAN
041700141022     C                   CALL      'QCMDEXC'                            91
041800141022     C                   PARM                    COMMAN
041900141022     C                   PARM                    LUNG
042000141022     c                   endif
042100141022     C                   if        not *in91
042200141022      *
042300141022      * OVRPRTF etichette
042400141022     C                   MOVEA     D90PRe        CMetf6(30)
042500141022     C                   MOVEA     D90MDE        CMetf6(52)
042600141022     C*
042700141022     C                   Z-ADD     130           LUNG
042800141022     C                   CLEAR                   COMMAN
042900141022     C                   MOVEA     CMetf6        COMMAN
043000141022     C                   CALL      'QCMDEXC'                            91
043100141022     C                   PARM                    COMMAN
043200141022     C                   PARM                    LUNG
043300141022     c                   endif
043301150114     c                   else
043302150114     c                   goto      fine
043400141022     c                   endif
043500141022      *
043600141022     c                   ENDSR
043700061002     C**********************************************************************
043800061002     C*--- INZSR INIZIALE --------------------------------------------*
043900061002     C**********************************************************************
044000061002     C     *INZSR        BEGSR
044100061002     C*
044200061002     C     *ENTRY        PLIST
044300061002     C                   PARM                    KPJBA
044400110211     c                   setoff                                       36
044500061002      * Reperisco dati job
044600061002     c                   exsr      DatiJob
044700061002     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
044800061002     C                   TIME                    W0140            14 0
044900061002     C                   MOVE      W0140         UDATE8            8 0
045000061002     C                   MOVEL     W0140         HHMM              4 0          *ORA/MINUTI
045100061002     C                   Z-ADD     UDATE8        G02DAT                         *GG/MM/AAAA
045200061002     C                   MOVEL     *BLANK        G02ERR
045300061002     C                   CALL      'XSRDA8'
045400061002     C                   PARM                    WLBDAT
045500061002     C                   Z-ADD     G02INV        DATEU8                         *AAAA/MM/GG
045600061003     C                   MOVEL     dutpou        V1CFGS                         *FIL.GESTIONE
045700061003     C                   MOVEL     dutpou        V1CFPD                         *FIL.PADRONCINO
045800061003     c                   if        dutlpo = '2'
045900061002     C                   SETON                                        05
046000061002    1C                   ENDIF
046100061002     C*
046200141020     C                   MOVEL     'FNLRQ9R'     V1CPGM
046300061003     C                   Z-ADD     4             WNPG              1 0
046400061002     C*
046500061002     C***
046600061002     C* DEFINIZIONE CAMPI
046700061002     C***
046800061002     C     *LIKE         DEFINE    UDATE8        DATEU8
046900110103     C     *LIKE         DEFINE    apdpdr        WPADR
047000101209     c     Kapdw         Klist
047100101209     c                   Kfld                    apdtip
047200101209     c                   Kfld                    wpadr
047300110202     c                   eval      apdtip = 'A'
047400110210     c     Kdst4         Klist
047500110210     c                   Kfld                    apdtip
047600110210     c                   Kfld                    wpadr
047700110210     c                   Kfld                    DSTFCF
047800141023     c     Kdst          Klist
047900141023     c                   Kfld                    dstnpg
048000141023     c                   Kfld                    wdst
048100141023     c                   Kfld                    v1cfgs
048200141023     c                   z-add     4             dstnpg
048300141029     c     Kfvv          Klist
048400141029     c                   Kfld                    dstnpg
048500141029     c                   Kfld                    fvvnfv
048600141029     c                   Kfld                    v1cfgs
048700141022     c                   exsr      RICHSTAMPAN
048800061002     C                   ENDSR
048900061002      *---------------------------------------------------------------*
049000061002      * Reperimento Dati del job (Utente/Operativi)                   *
049100061002      *---------------------------------------------------------------*
049200061002     c     DatiJob       BEGSR
049300061002      *
049400061002     c     *dtaara       define    §azute        azuteds
049500061002     c     *dtaara       define    §datiute      ddatiute
049600061002      *
049700061002     c                   in(E)     *dtaara
049800061002     c                   IF        %ERROR or RSUT = *blanks
049900061002     c                   clear                   Tibs34Ds
050000061002     c                   call      'TIBS34R'
050100061002     c                   parm                    Tibs34Ds
050200061002     c                   in        *dtaara
050300061002     c                   ENDIF
050400141029
050500061002     c                   ENDSR
050600950525     C*---------------------------------------------------------------*
050700950523**  MSG
050800970416Foglio Distinta inesistente o in gestione ad altro punto operativo            01
050900110103                                                                              02
051000110103                                                                              03
051100141023Distinta abbinata ad autista NON FITTIZIO                                     04
051200950524Foglio Distinta non aperto con questo programma: impossibile la manutenzione  05
051300101209Codice Autotrasportatore inesistente                                          06
051400101209P.O. Autotrasportatore incongruente con punto operativo apertura foglio       07
051500141022**  cmA4f6
051600141022OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
051700141022USRDTA('C.PO_DIR.') SECURE(*YES) SHARE(*YES)
051800141022**  cmA5f6
051900141022OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
052000141022             USRDTA('C.PO_DIR.') SECURE(*YES) SHARE(*YES)
052100141022**  cmETf6
052200141022OVRPRTF FILE(FNLV22P)   OUTQ(xxxxxxxxxx) FORMTYPE('XXXXXXXXXX')
052300141022USRDTA('C.PO_DIR.') SECURE(*YES) SHARE(*YES)

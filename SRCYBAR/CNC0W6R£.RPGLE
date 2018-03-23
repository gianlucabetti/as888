000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200020926     H OPTION(*NODEBUGIO)
000300060321     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000400060321     H BNDDIR('TRULCIF')
000500900606?     *--------------------------------------------------------------*
000600021119      * CNC0W1R  quadratura giornaliera                              *
000700991029?     *--------------------------------------------------------------*
000800910521      * 03 -FINE LAVORO                                              *
000900910521      * 06 -AGGIORNAMENTO                                            *
001000911106      * 30 - 39 ERRORI SINGOLI CAMPI                                 *
001100900606?     *--------------------------------------------------------------*
001200021119     FCNC0W1d   CF   E             WORKSTN
001300060202     F                                     SFILE(CN0W9S4:NR4)
001400060202     FYNTGD01l  IF   E           K DISK
001500060203     Fndppa03L  IF   E           K DISK
001600060203     Fndpas03L  IF   E           K DISK
001700060202     FFnArb71L  IF   E           K DISK
001800021118     Ffnvrt01L  IF   E           K DISK
001900060202     Fcnass01L  IF   E           K DISK
002000060202     Fyfqig01L  IF a E           K DISK
002100021115     FTNTBE01L  IF   E           K DISK
002200021121     FAZORG01L  IF   E           K DISK
002300060202     Fyfmdi01L  UF   E           K DISK
002400030318     FFNMOV00T  O  A E             DISK    USROPN commit
002500021119     Fcnc0w1p   O    E             PRINTER OFLIND(*IN99)
002600021211     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002700021211     F                                     USROPN
002800991029?     *--------------------------------------------------------------*
002900021115     D Dy4o          E DS
003000991102     D* Errori
003100060203     D ERR             S             80    DIM(9) CTDATA PERRCD(1)              SCHIERA ER
003200020925     D CMDD            S             49    DIM(5) CTDATA PERRCD(1)              CMD Exec
003300021211     D CMDP            S              1    DIM(60) CTDATA PERRCD(60)
003400060321     D ERRc            S              1    DIM(10)                              SCHIERA ER
003500991029?     *--------------------------------------------------------------*
003600021121     D ddatiute      e ds
003700021121     D azuteds       e ds                  extname(AZUTE00F)
003800060321     D cnacods       e ds                  extname(cnaco00f)
003900060321     D cnindds       E DS                  EXTNAME(cnind00f)
004000060321     D                                     BASED(nullPtr)
004100060321     D cnclpds       E DS                  EXTNAME(cnclp00f)
004200060321     D                                     BASED(nullPtr)
004300060321     D fnclsds       E DS                  EXTNAME(fncls00f)
004400060321     D                                     BASED(nullPtr)
004500060321     D cndizion      E DS
004600060321     D                                     BASED(nullPtr)
004700060321     D                                     PREFIX(diz)
004800060321     D andiz00f      E DS
004900060321     D                                     BASED(nullPtr)
005000060321     D                                     PREFIX(pjz)
005100021121     D tibs34ds      E DS                  inz
005200021122     D DSCMZ         E DS
005300021122     d tibs55ds      E DS
005400060203     D DGei          E DS
005500900514     D KPJBA         E DS
005600021121     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005700021121     D  TCU                  398    697
005800021121     D                                     DIM(50)                              TAB FLAG C
005900021121     D  KCU                  698    847P 0
006000021121     D                                     DIM(50)                              TAB CAPO C
006100021121     D                                     PACKEVEN
006200021115     D TCUDS           DS
006300021115     D  F4                     4      4
006400021121     D  F56                    5      6
006500991102     D WLBDAT          DS
006600991102     D  G02DAT                 1      8  0
006700991102     D  G02INV                 9     16  0
006800991102     D  G02ERR                17     17
006900991102     D  G02TGI                18     22  0
007000941124     D                SDS
007100941124     D  §PGM                   1     10
007200021118     D wcomo6          S              6  0                                      DS3A TIPO
007300021120     D annqig          S                   like(qigann)                         DS3A TIPO
007400021125     D savdta          S                   like(amgudt)                         DS3A TIPO
007500031003     D dtalimite       S                   like(amgudt)                         DS3A TIPO
007600060321     D esito           S             10I 0
007700021121     D dtaiso          S               d                                        DS3A TIPO
007800021121     D dtaeur          S               d   datfmt(*eur)                         DS3A TIPO
007900030318     D f8              S              1    inz('0')                             DS3A TIPO
008000060202     D WrkLstRrnS4     S              5I 0
008100060202     D tmi             S                   like(mditmi)                         DS3A TIPO
008200060321     D chkCliente      PR            10I 0
008300060321     D  prmSoc                             LIKE(diz$soc)
008400060321     D                                     OPTIONS(*OMIT)
008500060321     D  prmKcc                             LIKE(diz$Kcc)
008600060321     D                                     OPTIONS(*OMIT)
008700060321     D  prmCsc                             LIKE(diz$csc)
008800060321     D                                     OPTIONS(*OMIT)
008900060321     D  prmKsc                             LIKE(pjzKsc)
009000060321     D                                     OPTIONS(*OMIT)
009100060321     D  prmRicerca                    1A   const
009200060321     D  prmRag                             LIKE(dizAcoRag)
009300060321     D                                     OPTIONS(*OMIT)
009400060321     D  prmFlr                       90A
009500060321     D                                     OPTIONS(*OMIT)
009600060321     D  prmKpjba                           LIKE(kpjba)
009700060321     D                                     OPTIONS(*OMIT)
009800060321     D  prmEsito                     10I 0
009900060321     D  prmErr                        1A   DIM(10)
010000060321     D  prmMsg                       80A   DIM(10)
010100060321     D                                     OPTIONS(*OMIT)
010200060321     D  prmCnaco00f                        LIKE(cnacods)
010300060321     D                                     OPTIONS(*OMIT)
010400060321     D  prmCnind00f                        LIKE(cnindds)
010500060321     D                                     OPTIONS(*OMIT)
010600060321     D  prmCnclp00f                        LIKE(cnclpds)
010700060321     D                                     OPTIONS(*OMIT)
010800060321     D  prmFncls00f                        LIKE(fnclsds)
010900060321     D                                     OPTIONS(*OMIT)
011000991029?     *--------------------------------------------------------------*
011100991029      * CICLO PRINCIPALE
011200991029?     *--------------------------------------------------------------*
011300021115     c                   exsr      inz01
011400021122     c* se non ci sono errori e non è la 1° volta carico i dati a video
011500021122     c                   if        not *in28 and *in31
011600021122     c                   exsr      carica
011700021122     c                   end
011800021115     c                   do        *hival
011900021118     C                   EXFMT     CN0W101
012000021125     C   kc              leave
012100021115     c* distinte non contabilizzate
012200021115     c                   if        *in28
012300021115     c                   iter
012400021115     c                   end
012500021120     c* stampa dettaglio distinte e versamenti
012600021120     c                   if        *inkj
012700021120     c                   seton                                        99
012800021125     c                   seton                                        14
012900021125     c                   exsr      stampa
013000021120     c                   exsr      stadis
013100021120     c                   exsr      staver
013200030411     c                   close     cnc0w1p
013300030411     c                   open      cnc0w1p
013400021120     c                   end
013500060202     c* gestione sfl tagli contante
013600060202     c                   if        *inkg
013700060202     c                   exsr      srtagli
013800060202     c                   end
013900021122     c* se 1° volta controlla data cassaf. gg precedente
014000021122     c                   if        not *in31
014100021115     c                   exsr      ctr01
014200021120     c   30              iter
014300021125     c                   move      dtagpv        dtaeur
014400021125     c                   move      dtaeur        dtaiso
014500021122     c                   move      dtaiso        qigdta
014600021122     c* carica
014700021122     c                   exsr      carica
014800021122     c                   end
014900021122     c* controlli e calcoli
015000021122     c                   exsr      ctr02
015100031023     c                   if        *in28 or *in99
015200031023     c                   iter
015300031023     c                   end
015400021120     c* errore forzabile
015500030318     c                   if        *in90
015600030318     c                   if        not *inkh
015700030318     c* non è stato premuto ancora f8 di forzatura
015800030318     c                   if        f8 = *off
015900030318     c                   iter
016000030318     c                   end
016100030318     c* se esiste errore di quadratura mi deve prima forzare e poi convalida
016200030318     c                   else
016300030318     c                   movel     *on           f8
016400030318     C                   MOVEL     ERR(6)        $MSG
016500030318     c                   iter
016600030318     c                   end
016700030318     c                   else
016800030318     c                   movel     *off          f8
016900030318     c                   end
017000021115     c* aggiorna
017100030318     c                   if        *inkf
017200021120     c* chiedo la data per il giroconto della cassaforte al gg successivo
017300060202     c                   if        totcasv <> 0
017400021120     c                   exsr      srdta
017500021120     c   kl              iter
017600021120     c                   end
017700030423     c* invio msg a CLI* in sede che c'è stata un'errata quadratura
017800030423     c                   if        f8 = *on
017900030423     c                   exsr      srmsg
018000030423     c                   end
018100030423     c* aggiorna
018200021115     c                   exsr      aggio
018300030318     c                   commit
018400021115     c                   leave
018500021115     c                   end
018600991102     C                   ENDDO
018700030403     c* invia se non c'è errore
018800030320     c                   if        not *in28
018900021122      *  CONTROLLO SE HO RECORD DA TRASMETTERE
019000021122     C                   EXSR      TSTOBJ
019100021122     c* sottomette trasmissione a sede del fnmov00t
019200021122     c                   if        nrr <> 0
019300021122     C                   EXSR      SBMTRA                                       *
019400021122     c                   end
019500030320     c                   end
019600030403     C                   EXSR      DLCMOV
019700991029     C*
019800910521     C                   SETON                                        LR
019900021115     C**-------------------------------------------------------------
020000021115     C** inizializza dspf
020100021115     C**-------------------------------------------------------------
020200021115     C     inz01         BEGSR
020300021118     C                   MOVE      DUTCOU        VCDUS
020400060201     c                   z-add     0             DISPOSV
020500060201     c                   z-add     0             DISCONV
020600060201     c                   z-add     0             DISASSV
020700060201     c                   z-add     0             PREPOSV
020800060201     c                   z-add     0             PRECONV
020900060201     c                   z-add     0             PREASSV
021000060201     c                   z-add     0             FCCPOSV
021100060201     c                   z-add     0             FCCCONV
021200060201     c                   z-add     0             FCCASSV
021300060201     c                   z-add     0             CPOPOSV
021400060201     c                   z-add     0             CPOCONV
021500060201     c                   z-add     0             CPOASSV
021600060202     c                   z-add     0             raiPOSV
021700060202     c                   z-add     0             raiCONV
021800060202     c                   z-add     0             raiASSV
021900021115     c                   z-add     0             totincv
022000060201     c                   z-add     0             VERCONV
022100060201     c                   z-add     0             VERASSV
022200060201     c                   z-add     0             TOTVERV
022300021115     c                   z-add     0             casfv
022400060201     c                   z-add     0             CASCONV
022500060201     c                   z-add     0             CASASSV
022600060201     c                   z-add     0             CASTOTV
022700021115     c                   z-add     0             totcasv
022800060201     c                   z-add     0             CGPCONV
022900060201     c                   z-add     0             CGPassV
023000060201     c                   z-add     0             CGPtotV
023100060201     c                   z-add     0             RETCONV
023200060201     c                   z-add     0             RETASSV
023300060201     c                   z-add     0             RETTOTV
023400021115     c                   z-add     0             totgenv
023500021122     C* se non trovo il record vuol dire che è la prima volta e quindi
023600021122     c* rendo modificabile la data e l'importo della cassaforte del gg
023700021122     c* precedente
023800021122     c                   clear                   annqig
023900060201     c     kqig          setll     yfqig01l
024000060201     c     kqig1         readpe    yfqig01l
024100060201     c                   if        not %eof(yfqig01l)
024200021122     c                   seton                                        31
024300060201     c                   z-add     QIGICON       CGPCONV
024400060201     c                   z-add     QIGIASS       CGPassV
024500060202     c                   eval      cgptotv = qigicon+qigiass
024600021122     c                   move      qigdta        dtaiso
024700021125     c                   move      dtaiso        dtaeur
024800021125     c                   move      dtaeur        dtagpv
024900021122     c                   else
025000060201     c                   z-add     0             CGPCONV
025100060201     c                   z-add     0             CGPassV
025200060201     c                   z-add     0             CGPtotV
025300021122     c                   z-add     0             dtagpv
025400021122     c                   setoff                                       31
025500021122     c                   end
025600021115     c                   endsr
025700021115     C**-------------------------------------------------------------
025800021115     C** carica i dati a video
025900021115     C**-------------------------------------------------------------
026000021115     C     carica        BEGSR
026100060202     c* leggo tutti i record di yfmdi del giorno per po
026200060201     c                   z-add     0             DISPOSV
026300060201     c                   z-add     0             DISCONV
026400060201     c                   z-add     0             DISASSV
026500060201     c                   z-add     0             PREPOSV
026600060201     c                   z-add     0             PRECONV
026700060201     c                   z-add     0             PREASSV
026800060201     c                   z-add     0             FCCPOSV
026900060201     c                   z-add     0             FCCCONV
027000060201     c                   z-add     0             FCCASSV
027100060201     c                   z-add     0             CPOPOSV
027200060201     c                   z-add     0             CPOCONV
027300060201     c                   z-add     0             CPOASSV
027400060202     c                   z-add     0             raiPOSV
027500060202     c                   z-add     0             raiCONV
027600060202     c                   z-add     0             raiASSV
027700021125     c                   z-add     0             totincv
027800060202     c     qigdta        setgt     yfmdi01l
027900021115     c                   do        *hival
028000060202     c                   read      yfmdi01l
028100060202     c                   if        %eof(yfmdi01l)
028200021115     c                   leave
028300021115     c                   end
028400060201     c                   if        mdifle <> dutpou
028500021115     c                   iter
028600021115     c                   end
028700060201     c* chiusura operatore
028800060201     c                   select
028900060201     c                   when      mditmi = 'O'
029000060201     c                   add       MDIICON       DISPOSV
029100060201     c                   add       MDIIASS       DISCONV
029200060201     c                   add       MDIIPOS       DISASSV
029300060201     c                   when      mditmi = 'P'
029400060201     c                   add       MDIICON       PREPOSV
029500060201     c                   add       MDIIASS       PRECONV
029600060201     c                   add       MDIIPOS       PREASSV
029700060201     c                   when      mditmi = 'F'
029800060201     c                   add       MDIICON       FCCPOSV
029900060201     c                   add       MDIIASS       FCCCONV
030000060201     c                   add       MDIIPOS       FCCASSV
030100060223     c                   when      mditmi = 'Z'
030200060201     c                   add       MDIICON       CPOPOSV
030300060201     c                   add       MDIIASS       CPOCONV
030400060201     c                   add       MDIIPOS       CPOASSV
030500060214     c                   when      mditmi = 'R'
030600060214     c                   add       MDIICON       RAIPOSV
030700060214     c                   add       MDIIASS       RAICONV
030800060214     c                   add       MDIIPOS       RAIASSV
030900060202     c                   endsl
031000021115     c                   enddo
031100060214     c                   eval      totincv = disconv+disassv+preconv+preassv
031200060214     c                             +fccconv+fccassv+cpoconv+cpoassv+raiconv+
031300060214     c                             raiassv
031400021120     c* leggo tutti i record di FNVRT del giorno per po
031500021125     c                   z-add     0             totverv
031600021122     c     kvrt          setgt     fnvrt01l
031700021120     c                   do        *hival
031800021122     c     vfil          reade     fnvrt01l
031900021120     c                   if        %eof(fnvrt01l)
032000021120     c                   leave
032100021120     c                   end
032200060202     c                   add       vrtcon        verconv
032300060202     c                   add       vrtass        verassv
032400021120     c                   enddo
032500060202     c                   eval      totverv=verconv+verassv
032600060202     c* importo assegni cassaforte di servizio
032700060202     c                   clear                   okass             1 0
032800060202     c                   clear                   casassv
032900060202     c     vfil          setll     cnass01l
033000060202     c                   do        *hival
033100060202     c     vfil          reade     cnass01l
033200060202     c                   if        %eof(cnass01l)
033300060202     c                   leave
033400060202     c                   end
033500060202     c* segnalo che non sono stati presi in carico tutti i mandati perchè
033600060202     c* ci sono assegni ancora non resi versabili
033700060202     c                   if        assfve <> 'V'
033800060202     c                   eval      okass = 1
033900060202     c                   else
034000060202     c                   add       asscas        casassv
034100060202     c                   end
034200060202     c                   enddo
034300021115     c                   endsr
034400021115     C**-------------------------------------------------------------
034500021122     C** controlli
034600021115     C**-------------------------------------------------------------
034700021115     C     ctr01         BEGSR
034800021120     c* se è la prima volta controllo che ci sia la data della cassaforte
034900021120     c* del gg precedente
035000021120     c                   if        dtagpv = 0
035100021120     C                   SETON                                        30
035200021120     c                   else
035300021120     c     *eur          test(d)                 dtagpv                 30
035400021120     c                   if        *in30
035500021120     c     *dmy          test(d)                 dtagpv                 30
035600021120     c                   if        not *in30
035700021120     c     *dmy          move      dtagpv        dtaeur
035800021120     c                   move      dtaeur        dtagpv
035900021120     c                   end
036000021120     c                   end
036100021120     c                   end
036200021115     c                   endsr
036300021122     C**-------------------------------------------------------------
036400021122     C** controlli e calcoli
036500021122     C**-------------------------------------------------------------
036600021122     C     ctr02         BEGSR
036700031023     c                   setoff                                       902899
036800021122     c                   clear                   $msg
036900021211     c* ritorno a controllare che non ci siano distinte senza chiusura op.
037000060321     C*?????             exsr      cntdis
037100021211     c                   if        not *in28
037200021122     c* totale incassato
037300060214     c                   eval      totincv = disconv+disassv+preconv+preassv
037400060214     c                             +fccconv+fccassv+cpoconv+cpoassv+raiconv+
037500060214     c                             raiassv
037600021122     c* totale cassaforte
037700021122     c                   z-add     casfv         totcasv
037800060202     c     casassv       add       casconv       castotv
037900060202     c                   add       castotv       totcasv
038000021122     c* totale generale
038100060202     c     cgpassv       add       cgpconv       cgptotv
038200060202     c                   z-add     cgptotv       totgenv
038300021122     c                   add       totincv       totgenv
038400021122     c                   sub       totverv       totgenv
038500031023     c                   if        totgenv < 0
038600031023     C                   SETON                                        99
038700031023     C                   MOVEL     ERR(7)        $MSG
038800031023     c                   else
038900060202     c                   if        totgenv <> totcasv  or
039000060202     c* congruenza contante
039100060202     c                             (disconv+preconv+fccconv+cpoconv+raiconv
039200060202     c                             +cgpconv-verconv)<> (casfv+ casconv) or
039300060202     c* congruenza assegni
039400060202     c                             (disassv+preassv+fccassv+cpoassv+raiassv
039500060202     c                             +cgpassv-verassv)<> (casassv)
039600031023     C                   SETON                                        90
039700031023     C                   MOVEL     ERR(1)        $MSG
039800031023     c                   end
039900031023     c                   end
040000060202     c* se non fatto presa in carico mandati
040100060202     c                   if        okass=1
040200060202     C                   SETON                                        99
040300060202     C                   MOVEL     ERR(8)        $MSG
040400060202     c                   end
040500060203     c* controllo se ci sono partite aperte sugli incassi d'attribuire
040600060214     C* da oltre 3 gg
040700060203     c                   exsr      incatt
040800021211     c                   end
040900021122     c                   endsr
041000060203     C**----------------------------------
041100060203     C** CALCOLO SALDO INCASSI DA ATTRIBUIRE
041200060203     C**----------------------------------
041300060203     C     INCATT        BEGSR
041400060203     C                   SETOFF                                       1318
041500060203     C                   MOVE      *ZEROS        SALDPR           15 2
041600060203     C                   MOVE      '000000'      KCC               6            CAPOCONTO
041700060203     C                   MOVE      '00000000'    KSC               8            SOTTOCONTO
041800060203     C                   MOVE      §4OCC1        KCC                            CAPOCONTO
041900060203     C                   MOVE      §4OSC1        KSC                            SOTTOCONTO
042000060203      **
042100060203     C     K04ppa03      SETLL     ndppa03l
042200060203     C**
042300060203     C                   do        *hival
042400060203     C**
042500060203     C     K04ppa03      READE     ndppa03l
042600060203     c                   if        %eof(ndppa03l)
042700060203     C                   leave
042800060203     c                   end
042900060203     C**
043000060203     C     K08Pas03      SETLL     NdPas03l
043100060203do03 c                   do        *hival
043200060203     C     K08Pas03      READE     NdPas03l
043300060203      *
043400060203     c                   if        %eof(ndpas03l)
043500060203     C                   leave
043600060203     c                   end
043700060203     C                   move      pasdtpar      WDtpar            8 0
043800060214      * controllo se ho partite aperte negli incassi d'attrribuire di tre gg
043900060214      * indietro
044000060214do03 c                   if        wdtpar <= oggimeno3
044100060203if01 C     pasDAre       IFEQ      1
044200060203     C                   ADD       pasimportd    SALDPR
044300060203el01 C                   ELSE
044400060203     C                   SUB       pasimportd    SALDPR                         SALDO PART
044500060203en01 C                   ENDif
044600060203en01 C                   ENDif
044700060203     C                   enddo
044800060203     C*
044900060203     C     saldpr        iflt      0
045000060203     C                   eval      saldpr = saldpr * -1
045100060203     C                   end
045200060203     C     saldpr        IFGt      §GEpin
045300060214     C                   SETON                                        90
045400060203     C                   MOVEL     ERR(9)        $MSG
045500060203     c                   leave
045600060203     C                   End
045700060203     C                   enddo
045800060203     C                   ENDSR
045900021115     C**-------------------------------------------------------------
046000021120     C** srdta - data cassaforte al gg successivo x giroconto
046100021115     C**-------------------------------------------------------------
046200021120     C     srdta         BEGSR
046300021120     c                   do        *hival
046400021120     C                   EXFMT     CN0W102
046500030318     C   kl              leave
046600021120     c* controllo la data x giroconto
046700030514     c                   clear                   dtaeur
046800021120     c                   if        dtagsv = 0
046900021120     C                   SETON                                        32
047000021120     c                   else
047100021125     c     *eur          test(d)                 dtagsv                 32
047200021120     c                   if        *in32
047300021120     c     *dmy          test(d)                 dtagsv                 32
047400030514     c                   if        not *in32
047500030514     c     *dmy          move      dtagsv        dtaeur
047600030514     c                   end
047700030514     c                   else
047800030514     c                   move      dtagsv        dtaeur
047900021120     c                   end
048000030514     c                   end
048100030514     c*
048200030514     c                   if        not *in32
048300030514     c                   move      dtaeur        dtagsv
048400030514     c                   move      dtaeur        dtaiso
048500030514     c                   move      dtaiso        comdta            8 0
048600031003     c* controllo che la data sia maggiore di oggi
048700030514     c                   if        comdta <= amgudt
048800030514     c                   seton                                        32
048900030514     c                   end
049000031003     c* controllo che la data non sia maggiore di un mese da oggi
049100031003     c                   if        comdta > dtalimite
049200031003     c                   seton                                        32
049300031003     c                   end
049400030514     c                   end
049500030318     c                   if        not *in32 and *inkf
049600030318     c                   leave
049700030318     c                   end
049800021120     c                   enddo
049900021120     c                   endsr
050000021120     C**-------------------------------------------------------------
050100021120     C** aggiorna
050200021120     C**-------------------------------------------------------------
050300021120     C     aggio         BEGSR
050400021122     c* giroconto della cassaforte al gg successivo negli inc. attribuire
050500060202     c                   if        totcasv <> 0
050600021115     c                   exsr      cregir
050700021120     c                   end
050800021118     c* STAMPO
050900021120     c                   seton                                        99
051000021118     c* testata incassi
051100021125     c                   seton                                        14
051200021125     c                   exsr      stampa
051300021120     c* mandati
051400021120     c                   exsr      stadis
051500021118     c* totale incassi
051600021118     c                   seton                                        05
051700021118     c                   exsr      stampa
051800021118     c* testata pagamenti
051900021118     c                   seton                                        06
052000021118     c                   exsr      stampa
052100021120     c* versamenti
052200021120     c                   exsr      staver
052300021118     c* cassaforte
052400021118     c                   seton                                        10
052500021115     c                   exsr      stampa
052600021118     c* cassaforte gg precedente
052700021118     c                   seton                                        11
052800021118     c                   exsr      stampa
052900021118     c* totale generale
053000021118     c                   seton                                        12
053100021118     c                   exsr      stampa
053200021115     c* se il totale è diverso da cassaforte lo segnalo anche in stampa
053300021115     c                   if        totgenv <> totcasv
053400021118     c                   seton                                        13
053500021115     c                   exsr      stampa
053600030331     c                   else
053700060321     c                   seton                                        29
053800030331     c                   exsr      stampa
053900021115     c                   end
054000021122     c* se è stata stornata la quadratura del giorno lo segnalo in stampa
054100021125     c                   movel     'A'           annqig
054200060202     c     kqig          chain     yfqig01l
054300060202     c                   if        %found(yfqig01l)
054400021120     c                   seton                                        15
054500021120     c                   exsr      stampa
054600021120     c* controllo anche il gg precedente perchè la quadratura la può aver
054700021120     c* rimandata ad oggi
054800021120     c                   else
054900030320     c* cerco l'ultimo gg non annullato
055000030320     c                   clear                   savdta
055100030320     c                   clear                   annqig
055200060202     c     kqig          setgt     yfqig01l
055300060202     c     kqig1         readpe    yfqig01l
055400060202     c                   if        not %eof(yfqig01l)
055500030320     c                   move      qigdta        savdta
055600030320     c                   end
055700030320     c* cerco l'ultimo gg annullato
055800030320     c                   clear                   qigdta
055900030320     c                   movel     'A'           annqig
056000060202     c     kqig          setgt     yfqig01l
056100060202     c     kqig1         readpe    yfqig01l
056200030320     c                   if        savdta < qigdta
056300021120     c                   seton                                        15
056400021120     c                   exsr      stampa
056500021120     c                   end
056600021120     c                   end
056700021125     c* memorizzo l'importo della cassaforte per la prossima quadratura
056800021125     c                   clear                   qigann
056900030320     c                   move      amgudt        qigdta
057000021125     c                   move      vfil          qigfle
057100021125     c                   move      dutcou        qigcuc
057200021125     c                   movel     'EUR'         qigvca
057300060202     c                   eval      qigicon = casfv + casconv
057400060202     c                   z-add     casassv       qigiass
057500060202     c                   write     yfqig000
057600021120     c                   endsr
057700021120     C**-------------------------------------------------------------
057800021120     C** stadis
057900021120     C**-------------------------------------------------------------
058000021120     C     stadis        BEGSR
058100060202     c                   clear                   totdiss
058200060202     c                   clear                   totpres
058300060202     c                   clear                   totfccs
058400060202     c                   clear                   totcpos
058500060214     c                   clear                   totrais
058600060214     c                   do        5             xx                1 0
058700021120     c* testata mandati
058800060203     c                   select
058900060202     c                   when      xx = 1
059000021120     c                   seton                                        01
059100060202     c                   eval      tmi = 'O'
059200060202     c                   when      xx = 2
059300060202     c                   seton                                        16
059400060202     c                   eval      tmi = 'P'
059500060202     c                   when      xx = 3
059600060202     c                   seton                                        19
059700060202     c                   eval      tmi = 'F'
059800060202     c                   when      xx = 4
059900060202     c                   seton                                        22
060000060223     c                   eval      tmi = 'Z'
060100060214     c                   when      xx = 5
060200060214     c                   seton                                        25
060300060214     c                   eval      tmi = 'R'
060400060202     c                   endsl
060500021120     c                   exsr      stampa
060600021120     c* dettaglio mandati
060700060202     c     qigdta        setgt     yfmdi01l
060800021120     c                   do        *hival
060900060202     c                   read      yfmdi01l
061000060202     c                   if        %eof(yfmdi01l)
061100021120     c                   leave
061200021120     c                   end
061300060202     c                   if        mdifle <> dutpou
061400021122     c                   iter
061500021122     c                   end
061600060202     c                   if        mditmi <> tmi
061700060202     c                   iter
061800060202     c                   end
061900060202     c                   if        mdiicon<>0 or mdiiass<>0 or mdiipos<>0
062000060202     c                   select
062100060202     c                   when      xx = 1
062200060202     c                   seton                                        02
062300060202     c                   when      xx = 2
062400060202     c                   seton                                        17
062500060202     c                   when      xx = 3
062600060202     c                   seton                                        20
062700060321     c* cliente
062800060321     c                   exsr      srcli
062900060202     c                   when      xx = 4
063000060202     c                   seton                                        23
063100060214     c                   when      xx = 5
063200060214     c                   seton                                        26
063300060202     c                   endsl
063400021120     c                   exsr      stampa
063500021120     c                   end
063600021120     c                   enddo
063700021120     c* totale mandati
063800060202     c                   select
063900060202     c                   when      xx = 1
064000060202     c                   seton                                        03
064100060202     c                   when      xx = 2
064200060202     c                   seton                                        18
064300060202     c                   when      xx = 3
064400060202     c                   seton                                        21
064500060202     c                   when      xx = 4
064600060202     c                   seton                                        24
064700060214     c                   when      xx = 5
064800060214     c                   seton                                        27
064900060202     c                   endsl
065000021120     c                   exsr      stampa
065100060202     c                   enddo
065200021120     c                   endsr
065300021120     C**-------------------------------------------------------------
065400060321     C** decodifica il cliente codificato
065500021120     C**-------------------------------------------------------------
065600060321     C     srcli         BEGSR
065700060321     c                   clear                   cnacods
065800060321     c                   clear                   esito
065900060321     c                   callp     CHKcliente(*omit:*omit:mdiksc:*omit:*off
066000060321     c                             :*omit:*omit:*omit:esito:errc:*omit:cnacods
066100060321     c                             :*omit:*omit:*omit)
066200060321     c                   if        esito = 0
066300060321     c                   movel     acorag        desksc
066400060321     c                   else
066500060321     c                   clear                   desksc
066600060321     c                   end
066700060321     c                   endsr
066800060321     C**-------------------------------------------------------------
066900060321     C** staver
067000060321     C**-------------------------------------------------------------
067100060321     C     staver        BEGSR
067200060202     c                   clear                   totvrts
067300021120     c* testata versamenti
067400021120     c                   seton                                        07
067500021120     c                   exsr      stampa
067600021120     c* dettaglio versamenti
067700021122     c     kvrt          setgt     fnvrt01l
067800021120     c                   do        *hival
067900021122     c     vfil          reade     fnvrt01l
068000021120     c                   if        %eof(fnvrt01l)
068100021120     c                   leave
068200021120     c                   end
068300021120     c                   seton                                        08
068400021120     c                   exsr      stampa
068500021120     c                   enddo
068600021120     c* totale versamenti
068700021120     c                   seton                                        09
068800021120     c                   exsr      stampa
068900021115     c                   endsr
069000021115     C**-------------------------------------------------------------
069100021115     C** stampa
069200021115     C**-------------------------------------------------------------
069300021115     C     stampa        BEGSR
069400021120     c* mandato in stampa
069500060202     c                   if        *in02 or *in17 or *in20 or *in23
069600060202     c                   move      mdidmi        dtaiso
069700021125     c                   move      dtaiso        dtaeur
069800060203     c                   move      dtaeur        eurdmi
069900060202     c                   select
070000060202     c* chiusura operatore
070100060202     c                   when      *in02
070200060202     c                   z-add     mdiipos       disposs
070300060202     c                   z-add     mdiicon       discons
070400060202     c                   z-add     mdiiass       disasss
070500060202     c                   eval      distots = disposs+discons+disasss
070600060214     c                   add       distots       totdiss
070700060202     c* pre-pagati
070800060202     c                   when      *in17
070900060202     c                   z-add     mdiipos       preposs
071000060202     c                   z-add     mdiicon       precons
071100060202     c                   z-add     mdiiass       preasss
071200060202     c                   eval      pretots = preposs+precons+preasss
071300060202     c                   add       pretots       totpres
071400060202     c* fatture clienti codificati
071500060203     c                   when      *in20
071600060202     c                   z-add     mdiipos       fccposs
071700060202     c                   z-add     mdiicon       fcccons
071800060202     c                   z-add     mdiiass       fccasss
071900060202     c                   eval      fcctots = fccposs+fcccons+fccasss
072000060202     c                   add       fcctots       totfccs
072100060202     c* c/assegni parzialmente omessi
072200060202     c                   when      *in23
072300060202     c                   z-add     mdiipos       cpoposs
072400060202     c                   z-add     mdiicon       cpocons
072500060202     c                   z-add     mdiiass       cpoasss
072600060202     c                   eval      cpotots = preposs+precons+preasss
072700060202     c                   add       cpotots       totcpos
072800060202     c                   endsl
072900060202     c                   end
073000060214     c* totale incassi
073100060214     c                   if        *in05
073200060214     c                   eval      totincs = discons+disasss+precons+preasss
073300060214     c                             +fcccons+fccasss+cpocons+cpoasss+raicons+
073400060214     c                             raiasss
073500060214     c                   end
073600060214     c* versamento in stampa
073700060214     c                   if        *in08
073800021121     c                   move      vrtddv        dtaiso
073900021125     c                   move      dtaiso        dtaeur
074000021125     c                   move      dtaeur        eurddv
074100021120     c                   z-add     vrtass        vrtasss
074200021120     c                   z-add     vrtcon        vrtcons
074300060202     c     vrtasss       add       vrtcons       vrttots
074400060202     c                   add       vrttots       totvrts
074500021120     c                   end
074600021120     c* messaggio storno
074700021120     c                   if        *in15
074800021121     c                   move      qigdta        dtaiso
074900021125     c                   move      dtaiso        dtaeur
075000021125     c                   move      dtaeur        eurdta
075100021120     c                   end
075200021120     c*
075300021121     c   99              write     testa
075400021118     c                   setoff                                       99
075500021121     c   14              write     tesinc
075600021121     c   01              write     tesman
075700021121     c   02              write     detman
075800021121     c   03              write     totman
075900060202     c   16              write     tespre
076000060202     c   17              write     detpre
076100060202     c   18              write     totpre
076200060202     c   19              write     tesfcc
076300060202     c   20              write     detfcc
076400060202     c   21              write     totfcc
076500060202     c   22              write     tescpo
076600060202     c   23              write     detcpo
076700060202     c   24              write     totcpo
076800060214     c   25              write     tesrai
076900060214     c   26              write     detrai
077000060214     c   27              write     totrai
077100021121     c   05              write     totinc
077200021121     c   07              write     tesver
077300021121     c   08              write     detver
077400021121     c   09              write     totver
077500021121     c   10              write     detcas
077600021121     c   11              write     detcasg
077700021121     c   12              write     totgen
077800060321     c   29              write     quaok
077900030331     c   13              write     errore
078000021121     c   15              write     storno
078100021118     c                   movea     '000000000000'*in(01)
078200060202     c                   movea     '000000000000'*in(13)
078300060214     c                   movea     '000'         *in(25)
078400021115     c                   endsr
078500021118     C**-------------------------------------------------------------
078600021118     C** CREAZIONE MOVIMENTI CONTABILI di giroconto
078700021118     C**-------------------------------------------------------------
078800021120     C     CREGIR        BEGSR
078900021115     C* DARE
079000021115     C                   MOVE      KNMUS         MOVNMU
079100021115     C                   MOVE      KNRAZ         MOVRIF
079200021118     C                   MOVEL     vfil          MOVRIF
079300021120     C                   add       1             MOVSBN
079400021115     C                   Z-ADD     1             MOVSBR                         *PROGR.REG
079500021115     C                   MOVEL     *BLANKS       MOVFGE
079600060203     C                   MOVEL     soc           MOVSOC
079700021115     C                   MOVEL     *BLANKS       MOVFPN
079800021115     C                   MOVEL     vfil          MOVFPN
079900021115     C                   MOVEL     'D'           MOVTMO
080000021115     C                   MOVEL     'CG'          MOVCTB
080100021120     C                   MOVE      VCDUS         WCOMO6
080200021120     C                   MOVEL     vfil          WCOMO6
080300021120     C                   MOVEL     WCOMO6        movnrg
080400021120     C                   MOVE      9             movnrg
080500021118     C                   Z-ADD     AMGUDT        MOVDRG
080600021115     C                   MOVE      'COGE'        MOVSRR
080700021118     C                   MOVEL     §4OCO5        MOVCOD
080800021115     C                   Z-ADD     1             MOVDAR
080900021115     C                   Z-ADD     0             MOVAVE
081000021115     C                   MOVEL     'EUR'         MOVDIV
081100021115     C                   Z-ADD     0             MOVNDC
081200021115     C                   Z-ADD     0             MOVDDC
081300021115     C                   MOVE      *BLANKS       MOVSRD
081400021115     C                   Z-ADD     vfil          MOVNRP
081500021125     C                   move      dtagsv        dtaeur
081600021125     C                   move      dtaeur        dtaiso
081700021120     C                   move      dtaiso        MOVDRP
081800021115     C                   MOVEL     *BLANKS       MOVSRP
081900021115     C                   MOVEL     'EUR'         MOVDIP
082000021115     C                   Z-ADD     totcasv       MOVIPM
082100021115     C                   Z-ADD     totcasv       MOVIPD
082200021125     C                   Z-ADD     TOTCASV       MOVIMM
082300021125     C                   Z-ADD     TOTCASV       MOVIMD
082400021115     C                   Z-ADD     0             MOVDSC                         *
082500021115     C                   MOVEL     *BLANKS       MOVDES
082600021115     C                   MOVE      *ZEROS        MOVKCC
082700021115     C                   MOVE      *ZEROS        MOVKSC
082800021115     C                   MOVE      *ZEROS        MOVCCP
082900021115     C                   MOVE      *ZEROS        MOVSCP
083000021115     C                   MOVE      §4OCC1        MOVkcc
083100021115     C                   MOVE      §4OSC1        MOVksc
083200021122     C                   MOVE      §4OCC1        MOVccp
083300021122     C                   MOVE      §4OSC1        MOVscp
083400021115     C                   MOVEL     *BLANKS       MOVCDC
083500021115     C                   Z-ADD     0             MOVDCA
083600021115     C                   Z-ADD     MOVDRG        MOVDOP
083700021115     C                   Z-ADD     1             MOVCMB
083800021115     C                   WRITE     FNMOV000
083900021122     C* AVERE
084000021122     C                   Z-ADD     2             MOVSBR                         *PROGR.REG
084100021122     C                   Z-ADD     0             MOVDAR
084200021122     C                   Z-ADD     1             MOVAVE
084300021122     C                   move      amgudt        MOVDRP
084400021122     C                   WRITE     FNMOV000
084500021115     C                   ENDSR
084600991118     C*----------------------------------------------------*
084700991118     C* ESEGUO OVRDBF SU FNMOV
084800991118     C*----------------------------------------------------*
084900991118     C     OVRMOV        BEGSR
085000991118     C*
085100020926     C                   MOVE      '046) '       VAR               8
085200021118     C                   MOVEL     vfil          VAR
085300020925     C                   Z-ADD     49            LUNG             15 5
085400991118     C** CONTROLLO SE ESISTE IL MEMBRO X FNMOV00T
085500060321     C                   SETOFF                                       98
085600991118     C                   MOVE      VAR           CMDD(1)
085700991118     C                   MOVEL     *BLANKS       COMMAN
085800030411     C                   MOVEl(p)  CMDD(1)       COMMAN           80
085900060321     C                   CALL      'QCMDEXC'                            98
086000991118     C                   PARM                    COMMAN
086100991118     C                   PARM                    LUNG
086200991118     C** ESEGUE ADDPFM DEL MEMBRO NUOVO
086300060321     C   98              DO
086400991118     C                   MOVE      VAR           CMDD(2)
086500991118     C                   MOVEL     *BLANKS       COMMAN
086600030411     C                   MOVEl(p)  CMDD(2)       COMMAN
086700991118     C                   CALL      'QCMDEXC'
086800991118     C                   PARM                    COMMAN
086900991118     C                   PARM                    LUNG
087000991118     C                   END
087100991118     C*+ ESEGUE IN OGNI CASO OVRDBF DEL MEMBRO DESIDERATO
087200991118     C                   MOVE      VAR           CMDD(3)
087300991118     C                   MOVEL     *BLANKS       COMMAN
087400030411     C                   MOVEl(p)  CMDD(3)       COMMAN
087500991118     C                   CALL      'QCMDEXC'
087600991118     C                   PARM                    COMMAN
087700991118     C                   PARM                    LUNG
087800991118     C* GUARDO SE è ALLOCATO
087900991118     C                   MOVEL     VAR           VAR
088000991118     C                   MOVE      ')'           VAR
088100991118     C                   MOVE      VAR           CMDD(4)
088200030411     C                   MOVEl(p)  CMDD(4)       COMMAN
088300020925     C                   Z-ADD     49            LUNCMD           15 5
088400991118     C                   CALL      'QCMDEXC'                            91
088500991118     C                   PARM                    COMMAN
088600991118     C                   PARM                    LUNCMD
088700991118     C*
088800021121     C  N91              OPEN      FNMOV00T                             91
088900030318     c                   if        *in91
089000030411     C                   MOVEL     'NO'          KPJBU
089100030318     C                   SETON                                        28
089200030318     C                   MOVEL     ERR(4)        $MSG
089300030411     c                   else
089400030318     c* controllo che il membro sia vuoto
089500030318     c                   exsr      tstobj
089600030318     c                   if        nrr <> 0
089700030318     C                   SETON                                        28
089800030318     C                   MOVEL     ERR(5)        $MSG
089900030411     c                   end
090000030318     c                   end
090100991118     C*
090200991118     C                   ENDSR
090300991118     C*----------------------------------------------------*
090400991118     C* ESEGUO Dlcobj  FNMOV
090500991118     C*----------------------------------------------------*
090600991118     C     DLCMOV        BEGSR
090700991118     C*
090800991118     C* GUARDO SE è ALLOCATO
090900991118     C*
091000021122     C  N91              CLOSE     FNMOV00T                             91
091100991118     C                   MOVE      VAR           CMDD(5)
091200030411     C                   MOVEl(p)  CMDD(5)       COMMAN
091300020925     C                   Z-ADD     49            LUNCMD           15 5
091400991118     C                   CALL      'QCMDEXC'                            91
091500991118     C                   PARM                    COMMAN
091600991118     C                   PARM                    LUNCMD
091700991118     C*
091800991118     C                   ENDSR
091900021122     C*-------------------------------------------------------
092000021122     ** Sottometto trasmissioni
092100021122     **-------------------------------------------------------
092200021122     C     SBMTRA        BEGSR
092300021122     C*
092400021122     C                   MOVEL     '046'         CMZDST                          DEST.:SED
092500021122     C                   MOVEL     'FNMOV00T'    CMZFLD                          DA: >>>
092600021127     C                   EVAL      CmzMbD = 'M' + %EDITC(dutpou:'X') + '046'
092700021122     C                   MOVEL     O50PLA        CMZLBD
092800021122     C                   MOVEL     'FNMOV00R'    CMZFLA                          A: >>>
092900021122     C                   MOVEL     CmzMbD        CMZMBA
093000021122     C                   MOVEL     O50ALA        CMZLBA
093100021122     C                   MOVEL     'GAITRA'      CMZLBA
093200021122     C                   MOVE      O50ALA        WQUATR            4
093300021122     C     WQUATR        IFEQ      '101 '
093400021122     C                   MOVE      '201 '        CMZLBA
093500021122     C                   ENDIF
093600021122     C                   MOVE      '0000'        CMZORI                          ORA
093700021122     C                   MOVE      '0000000'     CMZDTI                          DATA
093800021122     C                   MOVE      'N'           CMZURG                          NOTTE
093900021122     C                   MOVE      'A'           CMZADD                          IN ADD
094000021122     C                   MOVE      'S'           CMZCLR                          CLEAR
094100021122     C                   MOVE      'S'           CMZFTR                          DA TRANS.
094200021122     C                   MOVE      *BLANK        CMZERR                          1= ERR.
094300021122     C                   MOVE      'Y029'        CMZAZN                          AZIONE
094400021122     C                   MOVEL     O50ASI        CMZSYF                          SIST.INF.
094500021122     C                   MOVEL     'GAITRA'      CMZSYF
094600021122     C                   MOVE      O50ASI        WQUATR
094700021122     C     WQUATR        IFEQ      '101 '
094800021122     C                   MOVE      '201 '        CMZSYF
094900021122     C                   ENDIF
095000021122     C                   MOVE      nrr           CMZNRR                          N.REC.
095100021122     C                   MOVE      *BLANKS       CMZPRR                          FILE PRE-
095200021122     C*
095300021122     C                   CLEAR                   W236
095400021122     C                   CALL      'TIBS53C'
095500021122     C                   PARM                    O50LCL
095600021122     C                   PARM                    CMZDST
095700021122     C                   PARM                    CMZFLD
095800021122     C                   PARM                    CMZMBD
095900021122     C                   PARM                    CMZLBD
096000021122     C                   PARM                    CMZFLA
096100021122     C                   PARM                    CMZMBA
096200021122     C                   PARM                    CMZLBA
096300021122     C                   PARM                    CMZORI
096400021122     C                   PARM                    CMZDTI
096500021122     C                   PARM                    CMZURG
096600021122     C                   PARM                    CMZADD
096700021122     C                   PARM                    CMZCLR
096800021122     C                   PARM                    CMZFTR
096900021122     C                   PARM                    CMZERR
097000021122     C                   PARM                    CMZAZN
097100021122     C                   PARM                    CMZSYF
097200021122     C                   PARM                    CMZNRR
097300021122     C                   PARM                    CMZPRR
097400021122     C                   PARM                    W236            236
097500021122     C*
097600021122     C                   ENDSR
097700021122      *-----------------------------------------------------***********
097800021122      * TEST NR RECORD FILE DA TRASMETTER                   *  TSTOBJ *
097900021122      *-----------------------------------------------------***********
098000021122     C     TSTOBJ        BEGSR
098100021122     C* Richiamo TIBS55R per conoscere librerie/S.Informativi
098200021122     C* da/a cui spedire
098300021122     C                   CLEAR                   TIBS55ds
098400021122     C                   MOVEL     'L'           I50TLA
098500021122     C                   MOVEL     dutpou        I50PPO
098600021122     C                   MOVEL     '046'         I50APO
098700021122     C                   CALL      'TIBS55R'
098800021122     C                   PARM                    TIBS55ds
098900021127     C* MEMBRO = M + dutpou + 046
099000021122     C* GUARDO SE IL MEMBRO è VUOTO - FNMOV00T
099100021122     C                   MOVEL     o50pla        LIBR
099200021122     C                   MOVEL(p)  'FNMOV00T'    FILE
099300021122     C                   EVAL      mbr = 'M' + %EDITC(dutpou:'X') + '046'
099400021122     C                   Z-ADD     0             NRR
099500021122     C                   CALL      'TRUL51C'
099600021122     C                   PARM                    LIBR             10
099700021122     C                   PARM                    FILE             10
099800021122     C                   PARM                    MBR              10
099900021122     C                   PARM                    NRR              10 0
100000021122     C                   ENDsr
100100991102      *-----------------------------------------------------***********
100200991102      * CONTROLLO DISTINTE CHIUSE NON CONTABILIZZATE        *  CNTDIS *
100300991102      *-----------------------------------------------------***********
100400991102     C     CNTDIS        BEGSR
100500021211      * OVRPRTF
100600021211     C     WOPNST        IFNE      'S'
100700021211     C                   MOVEL     'S'           WOPNST            1
100800021211     C                   MOVEA     CMDP          CMDPX            60
100900021211     C                   Z-ADD     60            LUNCMD           15 5
101000021211     C                   CALL      'QCMDEXC'                            92
101100021211     C                   PARM                    CMDPX
101200021211     C                   PARM                    LUNCMD
101300021211     C                   OPEN      QSYSPRT
101400021211     C                   END
101500991102     C* loop lettura distinte chiuse senza mandato d'introito
101600021118     C                   EVAL      ArbIPP = dutpou
101700020925     C                   CLEAR                   ArbNMI
101800021211     C                   CLEAR                   UNO
101900021211     C     K02Arb71      SETLL     FnArb71L
102000021211     C                   DO        *HIVAL
102100021211     C     K02Arb71      READE     FnArb71L
102200021211     C                   if        %EOF(fnarb71l)
102300021211     C                   LEAVE
102400021211     C                   END
102500021211     C* ESCLUDO LE DISTINTE 999999
102600021211     C                   if        ARBNDC = *ALL'9'
102700021211     C                   ITER
102800021211     C                   END
102900021211     C                   IF        UNO = 0
103000991214     C                   SETON                                        28
103100991102     C                   MOVEL     ERR(2)        $MSG
103200021211     C* Stampo testata
103300021211     C                   EXCEPT    TESARB
103400021211     C                   Z-ADD     1             UNO               1 0
103500021211     C                   END
103600021211     C* Stampo DETTAGLIO
103700030318     c                   if        arbddc <> 0
103800030318     C                   MOVE      arbddc        DtaISO
103900030318     C                   MOVE      DTAISO        DtaEUR
104000021211     C                   MOVE      DTAEUR        GMADDC            8 0
104100030318     c                   else
104200030318     c                   clear                   gmaddc
104300030318     c                   end
104400021211     C                   EXCEPT    DETARB
104500021211     C                   ENDDO
104600021211     C*
104700021211     C                   IF        UNO = 1
104800021211     C                   EXCEPT    FINEST
104900021211     C                   END
105000021119     C* controllo che ci sia una sola quadratura giornaliera
105100021120     c                   clear                   annqig
105200060202     c     kqig          chain     yfqig01l
105300060202     c                   if        %found(yfqig01l)
105400021119     C                   SETON                                        28
105500021119     C                   MOVEL     ERR(3)        $MSG
105600021119     c                   end
105700991102     C                   ENDSR
105800991102     C*-----------------------------------------------------------*
105900030318     C*  msg a cli* in sede di errata quadratura
106000991029     C*-----------------------------------------------------------*
106100030318     C     srmsg         BEGSR
106200030320     c                   move      udate         com08             6
106300030318     C                   eval      MSGTES = 'FORZATA quadratura giornaliera -
106400030318     C                             incassi e versamenti'
106500030318     C                   eval      msgdet = 'Punto operativo '
106600030318     c                             + %editc(dutpou:'X')
106700030318     c                             + ' in data '
106800030320     c                             + %subst(com08: 1: 2) + '/'
106900030320     c                             + %subst(com08: 3: 2) + '/'
107000030320     c                             + %subst(com08: 5: 2)
107100030318     C* UTENTE  A CUI INVIARE IL MESSAGGIO
107200030318     C                   MOVEL(P)  'CLI*'        MSGUTI
107300030318     C* FILIALE A CUI INVIARE IL MESSAGGIO
107400030318     C                   Z-ADD     046           MSGFIL
107500030318     C*
107600030318     C                   CALL      'TNSYM4C'
107700030318     C                   PARM                    MSGTES           55
107800030318     C                   PARM                    MSGDET          198
107900030318     C                   PARM                    MSGFIL            3 0
108000030318     C                   PARM                    MSGUTI           10
108100030318     C                   ENDSR
108200030318     C*-----------------------------------------------------------*
108300030318     C*  OPERAZIONI INIZIALI
108400030318     C*-----------------------------------------------------------*
108500030318     C     *INZSR        BEGSR
108600991029     C*
108700000000     C     *ENTRY        PLIST
108800000000     C                   PARM                    KPJBA
108900900516      *
109000000000     C                   Z-ADD     1             CODUT
109100900517     C                   CALL      'X§PARUT'
109200900517     C                   PARM                    UT§DSE
109300021118     C*
109400021118     c     *dtaara       define    §azute        azuteds
109500021118     c     *dtaara       define    §datiute      ddatiute
109600021118     C                   in(E)     *dtaara
109700021118     C                   IF        %Error  or  RSUT = *blanks
109800021118     C                   call      'TIBS34R'
109900021118     C                   parm                    Tibs34Ds
110000021118     C                   in        *dtaara
110100021118     c                   ENDIF
110200040921     c* Descrizione p.o.
110300040921     C                   EVAL      VFil = dutpou
110400040921     C     vfil          CHAIN     AZORG01L                           21
110500040921     C  N21              MOVEL     ORGDES        DFil
110600050111     c* se po contab. diverso da po gestionale forzo 2° altrimenti 1°
110700050111     c                   if        orgfc0 <> dutpou
110800040216     c                   eval      dutlpo = '2'
110900050111     c                   else
111000050111     c                   eval      dutlpo = '1'
111100040216     c                   end
111200060203     C* REPERISCO TABELLA Y4O
111300060203     C                   EXSR      SRY4O
111400021125     c* apro FNMOV00T
111500021125     c                   exsr      ovrmov
111600021118     C*--- RICERCA CAPOCONTO cassa------------------------------------*
111700021115     C                   DO        50            X                 2 0
111800021115     C                   MOVE      TCU(X)        TCUDS
111900021115     C     F56           CABNE     'CG'          END1
112000021115     C     F4            IFEQ      '4'
112100021115     C                   Z-ADD     KCU(X)        KCAS              4 0
112200021115     C                   END
112300021115     C     END1          TAG
112400021115     C                   END
112500991102     C                   TIME                    TIME              6 0
112600991102     C                   Z-ADD     UDATE         G02DAT
112700991102     C                   MOVE      *ZEROS        G02INV
112800991102     C                   MOVEL     *BLANK        G02ERR
112900991102     C                   CALL      'XSRDA8'
113000991102     C                   PARM                    WLBDAT
113100991102     C                   Z-ADD     G02INV        AMGUDT            8 0
113200031003     c* aggiungo un mese alla data di oggi
113300031003     c                   move      amgudt        dtaiso
113400031003     c                   adddur    1:*m          dtaiso
113500031003     c                   move      dtaiso        dtalimite
113600060203     c                   move      amgudt        dtaiso
113700060203     c                   subdur    3:*d          dtaiso
113800060214     c                   move      dtaiso        oggimeno3         8 0
113900060203     C* Reperisco tabella limite abbuoni
114000060203     C                   Movel     'GEI'         Tbecod
114100060203     C                   Movel     *blanks       Tbeke1
114200060203     C                   Movel     'EUR'         Tbeke1
114300060203     C                   Movel     *blanks       Tbeke2
114400060203     C     Ktbe1         Chain     Tntbe01l
114500060203     c                   if        %found(tntbe01l)
114600060203     C                   Movel     tbeuni        dgei
114700060203     c                   else
114800060203     c                   clear                   dgei
114900060203     c                   end
115000900515      *---------------------------------------------------------------*
115100900515      * CHIAVI                                                        *
115200900515      *---------------------------------------------------------------*
115300021119     C     Kqig          KLIST
115400021120     C                   KFLD                    annqig
115500021120     C                   KFLD                    vfil
115600021119     C                   KFLD                    amgudt
115700021120     C     Kqig1         KLIST
115800021120     C                   KFLD                    annqig
115900021120     C                   KFLD                    vfil
116000021118     C     KTBE1         KLIST
116100021118     C                   KFLD                    tbecod
116200021118     C                   KFLD                    tbeke1
116300021118     C                   KFLD                    tbeke2
116400021120     C     Kvrt          KLIST
116500021121     C                   KFLD                    vfil
116600021122     C                   KFLD                    qigdta
116700021121     C     K02Arb71      KLIST
116800021121     C                   KFLD                    ArbIPP
116900021121     C                   KFLD                    ArbNMI
117000060203     C     K04Ppa03      KLIST
117100060203     C                   KFLD                    soc               3
117200060203     c                   KFLD                    CTG               2
117300060203     C                   KFLD                    KCC
117400060203     C                   KFLD                    KSC
117500060203     C     K08Pas03      KLIST
117600060203     C                   KFLD                    soc
117700060203     c                   KFLD                    CTG
117800060203     C                   KFLD                    KCC
117900060203     C                   KFLD                    KSC
118000060203     C                   KFLD                    PpaDtPar
118100060203     C                   KFLD                    PpaNrPar
118200060203     C                   KFLD                    PpaSerPar
118300060203     c                   movel     'CG'          Ctg
118400060203     c                   movel     '201'         soc
118500991102     C*
118600991102     C* Controllo se ci sono distinte chiuse ancora da contabilizzare
118700060321     C*????              EXSR      CNTDIS
118800991102     C*
118900991102     C                   ENDSR
119000021115     c**********************************************************************
119100021115     C     SRY4O         BEGSR
119200021115     C*  CARICO I CONTI DA GENERARE  X FILIALE
119300021115     C                   MOVEL     'Y4O'         tbecod
119400060203     C                   MOVEl(p)  soc           tbeke1
119500021118     C                   MOVEL(p)  vfil          tbeke2
119600021118     C     ktbe1         CHAIN     Tntbe01l
119700021118     c                   if        %found(tntbe01l)
119800021118     C                   MOVEL     TBeUNI        Dy4O
119900021118     c                   else
120000021118     c                   clear                   dy4o
120100021118     c                   end
120200021115     C                   ENDSR
120300060202     C*-------------------------------------------------------
120400060202     ** GESTIONE CONTANTI
120500060202     **-------------------------------------------------------
120600060202     C     srtagli       BEGSR
120700060202      *
120800060202     C                   EXSR      PULCON
120900060202     C* CARICAMENTO SUBFILE
121000060202     C                   EXSR      CARCON
121100060202      *
121200060202     C                   do        *hival
121300060202     C                   WRITE     CN0W9Z4
121400060202     C                   EXFMT     CN0W9C4
121500060202      *
121600060202     C   kl              leave
121700060202     C* CONTROLLO CONTANTI IMMESSI
121800060202     C                   EXSR      CTRCON
121900060202     C* CALCOLO  TOTALE CONTANTI
122000060202     C                   EXSR      CALCON
122100060202     C                   enddo
122200060202     C                   ENDSR
122300060202     C**----------------------------------------------------------
122400060202     C** PULIZIA DEL SUBFILE CONTANTI
122500060202     C**----------------------------------------------------------
122600060202     C     PULCON        BEGSR
122700060202     C                   SETON                                        70
122800060202     C                   SETOFF                                       71
122900060202     C                   WRITE     CN0W9C4
123000060202     C                   SETOFF                                       70
123100060202     C                   ENDSR
123200060202     C**----------------------------------------------------------
123300060202     C** CARICAMENTO SUBFILE CONTANTI
123400060202     C**----------------------------------------------------------
123500060202     C     CARCON        BEGSR
123600060202      *
123700060202     C* CARICO I TAGLI PER IL CONTANTE
123800060202     C* L'IMPORTO IN TABELLA è 0 IL CAMPO IMPORTO è SPROTETTO
123900060202     C* LA DESCRIZ. DEI TAGLI CONTANTE è PROTETTO
124000060202     C                   CLEAR                   VNRTGL
124100060202     c                   eval      tgddivisa = 'EUR'
124200060202     C                   SETON                                        40
124300060202     C                   Z-ADD     0             NR4               4 0
124400060202     C     tgddivisa     setll     YNTGD01l
124500060202     C                   DO        *hival
124600060202     C     tgddivisa     READE     YNTGD01L
124700060202     c                   if        %eof(yntgd01l)
124800060202     c                   leave
124900060202     c                   end
125000060202      *
125100060202     C     TGDVALore     ifgt      0
125200060202     C                   MOVE      TGDDEScr      VDESCR
125300060202     C                   Z-ADD     TGDVALore     HIMTGL
125400060202     C     HIMTGL        COMP      0                                  60        PROTETTO
125500060202     C                   MOVEL     'C'           HIDTIP
125600060202     C                   ADD       1             NR4
125700060202     C                   WRITE     CN0W9S4
125800060202     C                   SETON                                        71
125900060202     C                   END
126000060202      *
126100060202     C                   END
126200060202     C                   SETOFF                                       6040
126300060202     C                   EVAL      WrkLstRrnS4 = Nr4
126400060202      *
126500060202     C                   ENDSR
126600060202     C**----------------------------------------------------------
126700060202     C** CONTROLLO  CONTANTI  IMMESSI
126800060202     C**----------------------------------------------------------
126900060202     C     CTRCON        BEGSR
127000060202      *
127100060202     C                   IF        WrkLstRrnS4 > 0
127200060202      *  AGGIORNA SEMPLICEMENTE IL SFL
127300060202     C                   Z-ADD     0             NR4
127400060202     C                   Z-ADD     0             casconv
127500060202     C                   DO        *HIVAL
127600060202     C                   ADD       1             NR4
127700060202     C     NR4           CHAIN     CN0W9S4                            63
127800060202     C     *IN63         IFEQ      *ON
127900060202     C                   LEAVE
128000060202     C                   END
128100060202      *  CALCOLA LA RIGA
128200060202     C     HIDTIP        IFEQ      'C'
128300060202     C     HIMTGL        ANDNE     *ZEROS
128400060202     C     VNRTGL        MULT      HIMTGL        VIDIM
128500060202     C                   ADD       VIDIM         casconv
128600060202     C     HIMTGL        COMP      0                                  60        PROT.VIDIM
128700060202     C                   SETON                                        40
128800060202     C                   UPDATE    CN0W9S4
128900060202     C                   END
129000060202     C                   END
129100060202     C                   ENDIF
129200060202     C                   ENDSR
129300060202     C**----------------------------------------------------------
129400060202     C** CALCOLO TOTALE  CONTANTI
129500060202     C**----------------------------------------------------------
129600060202     C     CALCON        BEGSR
129700060202      *
129800060202     C                   IF        WrkLstRrnS4 > 0
129900060202      *
130000060202     C                   Z-ADD     0             NR4
130100060202 +++ C                   Z-ADD     0             casconv
130200060202     C                   DO        *HIVAL
130300060202      *
130400060202     C                   ADD       1             NR4
130500060202     C     NR4           CHAIN     CN0W9S4                            63
130600060202     C     *IN63         IFEQ      '0'
130700060202      *
130800060202     C     HIDTIP        IFEQ      'C'
130900060202     C     HIMTGL        IFNE      *ZEROS
131000060202     C     VNRTGL        MULT      HIMTGL        VIDIM
131100060202     C                   ADD       VIDIM         casconv
131200060202     C     HIMTGL        COMP      0                                  60        PROT.VIDIM
131300060202     C                   SETON                                        40
131400060202     C                   UPDATE    CN0W9S4
131500060202     C                   ELSE
131600060202     C                   ADD       VIDIM         casconv
131700060202     C                   END
131800060202      *
131900060202     C                   END
132000060202     C*
132100060202     C                   END
132200060202     C*
132300060202     C  N63              END
132400060202      *
132500060202     C                   ENDIF
132600060202      *
132700060202     C                   ENDSR
132800021211     OQSYSPRT   E            TESARB           02
132900021211     O                       RSUT                20
133000030318     O                                         + 10 'QUADRATURA GIORNALIERA INC'
133100030318     O                                         +  0 'ASSI E VERSAMENTI'
133200021211     O                       UDATE             + 15 '  /  /  '
133300021211     O                       TIME              +  1 '  :  :  '
133400021211     O                                         + 10 'CNC0W1R'
133500021211     O                       PAGE          Z    132
133600021211     O                                          128 'PAG.'
133700021211     O          E            TESARB         1 04
133800021211     O                                         + 30 'ELENCO DISTINTE AUTOTRAS'
133900021211     O                                         +  0 'PORTATORE CHIUSE E NON C'
134000021211     O                                         +  0 'ONTABILIZZATE'
134100021211     O          E            DETARB         1
134200021211     O                                         +  1 'OPERATORE'
134300021211     O                       ARBCUC        Z   +  1
134400021211     O                                         +  1 'AUTOTRASP.'
134500021211     O                       ARBPDC        Z   +  1
134600021211     O                                         +  1 'NR.DISTINTA'
134700021211     O                       ARBNDC        Z   +  1
134800021211     O                                         +  1 'DEL'
134900021211     O                       GMADDC        8   +  1
135000021211     O          E            FINEST      2
135100021211     O                                         + 40 '** - F I N E    S T A M'
135200021211     O                                         +  1 'P A - **'
135300900606      *----------------------------------------------------------------
135400021118** ERR
135500021115ATT.NE : Gli importi non quadrano. Preme F8 per forzatura
135600021115ATT.NE : Distinte chiuse non contabilizzate
135700021119ATT.NE : Quadradura giornaliera già confermata
135800030318Non è possibile eseguire la quadratura. Attendere qualche minuto
135900031023ATT.NE : la precedente chiusura è terminata in modo anomalo
136000030318Ora si può premere F6 per confermare
136100031023ATT.NE : l'importo del TOTALE GENERALE non può essere negativo
136200060202ATT.NE : Non sono stati presi in carico tutti i mandati d'introito
136300060203ATT.NE : Esistono ci sono partite aperte nel conto inc.d'attribuire
136400991118** CMDD
136500020925CHKOBJ OBJ(FNMOV00T) OBJTYPE(*FILE) MBR(M000000)
136600020925ADDPFM FILE(FNMOV00T)               MBR(M000000)
136700020925OVRDBF FILE(FNMOV00T)               MBR(M000000)
136800020925ALCOBJ OBJ((FNMOV00T *FILE *EXCL        M000000))
136900020925DLCOBJ OBJ((FNMOV00T *FILE *EXCL        M000000))
137000021211**  CMDP
137100021211OVRPRTF FILE(QSYSPRT) USRDTA(CARICO_MAN) SAVE(*YES)

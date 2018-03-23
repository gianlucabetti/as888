000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200020926     H OPTION(*NODEBUGIO)
000300150417     H DFTACTGRP(*NO) BNDDIR('PJXBND':'PJCBND':'TRULCIF')
000400150417     H ACTGRP(*CALLER)
000500900606?     *--------------------------------------------------------------*
000600021119      * CNC0W1R  quadratura giornaliera                              *
000700991029?     *--------------------------------------------------------------*
000800910521      * 03 -FINE LAVORO                                              *
000900100820      * 06 -AGGIORNAMENTO                                            *
001000911106      * 30 - 39 ERRORI SINGOLI CAMPI                                 *
001100900606?     *--------------------------------------------------------------*
001200021119     FCNC0W1d   CF   E             WORKSTN
001300060202     FYNTGD01l  IF   E           K DISK
001400060203     Fndppa03L  IF   E           K DISK
001500060203     Fndpas03L  IF   E           K DISK
001600060202     FFnArb71L  IF   E           K DISK
001700021118     Ffnvrt01L  IF   E           K DISK
001800060327     Ffidci01l  IF   E           K DISK
001900060406     Fyfqri01l  IF   E           K DISK
002000060321     Fyfcas01L  IF   E           K DISK
002100060202     Fcnass01L  IF   E           K DISK
002200060202     Fyfqig01L  IF a E           K DISK
002300021115     FTNTBE01L  IF   E           K DISK
002400021121     FAZORG01L  IF   E           K DISK
002500060427     Fyfmdi01L  UF   E           K DISK
002600120531     Fazute02l  IF   E           K DISK    rename(azute000:azute02)
002700120531     F                                     prefix(FIL:3)
002800030318     FFNMOV00T  O  A E             DISK    USROPN commit
002900021119     Fcnc0w1p   O    E             PRINTER OFLIND(*IN99)
003000021211     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
003100021211     F                                     USROPN
003200991029?     *--------------------------------------------------------------*
003300021115     D Dy4o          E DS
003400060418     D Dyrt          E DS
003500991102     D* Errori
003600060327     D ERR             S             80    DIM(8) CTDATA PERRCD(1)              SCHIERA ER
003700020925     D CMDD            S             49    DIM(5) CTDATA PERRCD(1)              CMD Exec
003800021211     D CMDP            S              1    DIM(60) CTDATA PERRCD(60)
003900060321     D ERRc            S              1    DIM(10)                              SCHIERA ER
004000991029?     *--------------------------------------------------------------*
004100060427     D ana726ds      E DS
004200060427     D                                     INZ
004300060427     D  soc726       E
004400060427     D                                     INZ('201')
004500060427     D  lin726       E
004600060427     D                                     INZ('1')
004700060427     D  tpa726       E
004800060427     D                                     INZ('PPA')
004900060427     D  pgm726       E
005000060427     D                                     INZ('C')
005100060427     D  txt726       E
005200060427     D                                     INZ('Quadratura incassi e versamenti-
005300060427     D                                     ')
005400060322     D cnc0wa00s     e ds                  prefix(£)
005500021121     D ddatiute      e ds
005600060605     D azuteds       e ds                  extname(AZUTE00F)
005700060321     D cnacods       e ds                  extname(cnaco00f)
005800060321     D cnindds       E DS                  EXTNAME(cnind00f)
005900060321     D                                     BASED(nullPtr)
006000060321     D cnclpds       E DS                  EXTNAME(cnclp00f)
006100060321     D                                     BASED(nullPtr)
006200060321     D fnclsds       E DS                  EXTNAME(fncls00f)
006300060321     D                                     BASED(nullPtr)
006400060321     D cndizion      E DS
006500060321     D                                     BASED(nullPtr)
006600060321     D                                     PREFIX(diz)
006700060321     D andiz00f      E DS
006800060321     D                                     BASED(nullPtr)
006900060321     D                                     PREFIX(pjz)
007000021121     D tibs34ds      E DS                  inz
007100021122     D DSCMZ         E DS
007200021122     d tibs55ds      E DS
007300060203     D DGei          E DS
007400070731     D Ddgd          E DS
007500900514     D KPJBA         E DS
007600060426     D Trul52ds      E DS
007700021121     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
007800021121     D  TCU                  398    697
007900021121     D                                     DIM(50)                              TAB FLAG C
008000021121     D  KCU                  698    847P 0
008100021121     D                                     DIM(50)                              TAB CAPO C
008200021121     D                                     PACKEVEN
008300060406     D TRul06ds      E DS
008400060406     D  LIN                    1     90  0 dim(30)
008500140122      ****
008600140122     D  L72            s              3s 0 DESCEND
008700140122     d                                     DIM(60)
008800140122      ****
008900060411     D dsls01        E DS                  inz
009000060517     D soc001        E DS                  EXTNAME(xsoc001ds)
009100060517     D xsocds          DS          1000
009200021115     D TCUDS           DS
009300021115     D  F4                     4      4
009400021121     D  F56                    5      6
009500090410     D WLBDA8          DS
009600090410     D  G08DAT                 1      8  0
009700090410     D  G08INV                 9     16  0
009800090410     D  G08ERR                17     17
009900090410     D  G08TGI                18     22  0
010000060406     D PARAM           DS
010100060406     D  LIP                    1     87
010200060406     D                                     DIM(29)                              LINEE ARRI
010300060406     D  ZNC                   88    107
010400060406     D                                     DIM(10)                              ZONE VIDEO
010500060406     D  PARDAT               108    115  0
010600060406     D  PARABI               116    117
010700060406     D  PARLNP               118    120  0
010800060406     D  PARNFV               121    125
010900060406     D  PARGES               126    128  0
011000060406     D  PAROUT               129    129
011100060406     D  PARINC               130    131
011200060406     D  PARIMA               132    133
011300060406     D* DATA F. ARRIVI DI ABBINAMENTO IN AAAAMMGG
011400060406     D  PARDFA               134    141  0
011500060406     D* NUMERO FIL GESTIONE FOGLIO IMA
011600060406     D  PARFIA               142    146  0
011700060406     D  PARFGS               147    149  0
011800060406     D* SI/NO/SOLO POSTE   S/N/P
011900060406     D  PARSNP               150    150
012000060406     D* TUTTE, SOLO SI, SOLO NO ASSEGNATO
012100060406     D  PARASS               151    152
012200060406     D* TUTTE, SOLO SI, SOLO NO RESI
012300060406     D  PARRES               153    154
012400060406     D* Tutte, solo SI, solo NO - Bolle Bloccate
012500060406     D  PARblo               155    156
012600060406     d* Funzione richiesta: *blank = stampa, "WRKF " = creaz. file di lavoro
012700060406     d  PARfr                252    256
012800060411     D PARNAC          DS
012900060411     D  PFLE                   1      3
013000060411     D  PCUC                   4      6
013100060411     D  PNMI                   7     13
013200060411     D  PDMI                  14     19
013300941124     D                SDS
013400941124     D  §PGM                   1     10
013500120531     D NomUTE          S                   like(uteUTE)
013600021118     D wcomo6          S              6  0                                      DS3A TIPO
013700021120     D annqig          S                   like(qigann)                         DS3A TIPO
013800021125     D savdta          S                   like(amgudt)                         DS3A TIPO
013900031003     D dtalimite       S                   like(amgudt)                         DS3A TIPO
014000060321     D esito           S             10I 0
014100021121     D dtaiso          S               d                                        DS3A TIPO
014200021121     D dtaeur          S               d   datfmt(*eur)                         DS3A TIPO
014300030318     D f8              S              1    inz('0')                             DS3A TIPO
014400060321     D flgprima        S              1    inz('0')                             DS3A TIPO
014500060202     D WrkLstRrnS4     S              5I 0
014600060202     D tmi             S                   like(mditmi)                         DS3A TIPO
014700060321     D chkCliente      PR            10I 0
014800060321     D  prmSoc                             LIKE(diz$soc)
014900060321     D                                     OPTIONS(*OMIT)
015000060321     D  prmKcc                             LIKE(diz$Kcc)
015100060321     D                                     OPTIONS(*OMIT)
015200060321     D  prmCsc                             LIKE(diz$csc)
015300060321     D                                     OPTIONS(*OMIT)
015400060321     D  prmKsc                             LIKE(pjzKsc)
015500060321     D                                     OPTIONS(*OMIT)
015600060321     D  prmRicerca                    1A   const
015700060321     D  prmRag                             LIKE(dizAcoRag)
015800060321     D                                     OPTIONS(*OMIT)
015900060321     D  prmFlr                       90A
016000060321     D                                     OPTIONS(*OMIT)
016100060321     D  prmKpjba                           LIKE(kpjba)
016200060321     D                                     OPTIONS(*OMIT)
016300060321     D  prmEsito                     10I 0
016400060321     D  prmErr                        1A   DIM(10)
016500060321     D  prmMsg                       80A   DIM(10)
016600060321     D                                     OPTIONS(*OMIT)
016700060321     D  prmCnaco00f                        LIKE(cnacods)
016800060321     D                                     OPTIONS(*OMIT)
016900060321     D  prmCnind00f                        LIKE(cnindds)
017000060321     D                                     OPTIONS(*OMIT)
017100060321     D  prmCnclp00f                        LIKE(cnclpds)
017200060321     D                                     OPTIONS(*OMIT)
017300060321     D  prmFncls00f                        LIKE(fnclsds)
017400060321     D                                     OPTIONS(*OMIT)
017500060322     D F6              C                   X'36'
017600060327     Dflgcon           s              1
017700060421     Di                s              2  0
017800060411     Dcomconinc        s                   like(disconv)
017900060411     Dcomconver        s                   like(verconv)
018000060411     Dcomabb           s                   like(§GETQG)
018100060411     D§abb             s                   like(§GETQG)
018200060605     DSAVCUc           S                   like(casCUC)
018300060605     DSAVda8           S                   like(casda8)
018400060605     DSAVora           S                   like(casora)
018500120816     D exarbndc        S                   like(arbndc)                         DS3A TIPO
018600150417     d                                     INZ(*HIVAL)
018700991029?     *--------------------------------------------------------------*
018800991029      * CICLO PRINCIPALE
018900991029?     *--------------------------------------------------------------*
019000021115     c                   exsr      inz01
019100021122     c* se non ci sono errori e non è la 1° volta carico i dati a video
019200021122     c                   if        not *in28 and *in31
019300021122     c                   exsr      carica
019400021122     c                   end
019500021115     c                   do        *hival
019600060327     c                   if        *in90
019700060327     c                   setoff                                       90
019800060327     C                   write     CN0W101
019900060327     c                   seton                                        90
020000060327     c                   end
020100060327     C                   EXFMT     CN0W101
020200021125     C   kc              leave
020300060329     c* distinte non contabilizzate
020400021115     c                   if        *in28
020500021115     c                   iter
020600021115     c                   end
020700021120     c* stampa dettaglio distinte e versamenti
020800021120     c                   if        *inkj
020900021120     c                   seton                                        99
021000021120     c                   exsr      stadis
021100021120     c                   exsr      staver
021200030411     c                   close     cnc0w1p
021300030411     c                   open      cnc0w1p
021400021120     c                   end
021500060202     c* gestione sfl tagli contante
021600060327     c                   if        flgcon = *on
021700060327     c                   eval      *inki = *on
021800060327     c                   eval      flgcon= *off
021900060327     c                   end
022000060321     c                   if        *inki
022100060202     c                   exsr      srtagli
022200060202     c                   end
022300021122     c* se 1° volta controlla data cassaf. gg precedente
022400021122     c                   if        not *in31
022500021115     c                   exsr      ctr01
022600021120     c   30              iter
022700021125     c                   move      dtagpv        dtaeur
022800021125     c                   move      dtaeur        dtaiso
022900021122     c                   move      dtaiso        qigdta
023000021122     c* carica
023100021122     c                   exsr      carica
023200021122     c                   end
023300021122     c* controlli e calcoli
023400021122     c                   exsr      ctr02
023500031023     c                   if        *in28 or *in99
023600031023     c                   iter
023700031023     c                   end
023800021120     c* errore forzabile
023900030318     c                   if        *in90
024000030318     c                   if        not *inkh
024100030318     c* non è stato premuto ancora f8 di forzatura
024200030318     c                   if        f8 = *off
024300030318     c                   iter
024400030318     c                   end
024500030318     c* se esiste errore di quadratura mi deve prima forzare e poi convalida
024600030318     c                   else
024700060327     c                   setoff                                       414243
024800060414     c                   setoff                                       4045
024900030318     c                   movel     *on           f8
025000030318     C                   MOVEL     ERR(6)        $MSG
025100030318     c                   iter
025200030318     c                   end
025300030318     c                   else
025400030318     c                   movel     *off          f8
025500030318     c                   end
025600060327     c* richiedo il commento obbligatorio se premuto f8
025700060327     c                   if        f8 = *on
025800060327     c                   exsr      srcomme
025900060327     c                   end
026000021115     c* aggiorna
026100030318     c                   if        *inkf
026200021120     c* chiedo la data per il giroconto della cassaforte al gg successivo
026300060202     c                   if        totcasv <> 0
026400021120     c                   exsr      srdta
026500021120     c   kl              iter
026600021120     c                   end
026700030423     c* invio msg a CLI* in sede che c'è stata un'errata quadratura
026800030423     c                   if        f8 = *on
026900030423     c                   exsr      srmsg
027000030423     c                   end
027100030423     c* aggiorna
027200021115     c                   exsr      aggio
027300030318     c                   commit
027400021115     c                   leave
027500021115     c                   end
027600991102     C                   ENDDO
027700030403     c* invia se non c'è errore
027800030320     c                   if        not *in28
027900021122      *  CONTROLLO SE HO RECORD DA TRASMETTERE
028000021122     C                   EXSR      TSTOBJ
028100021122     c* sottomette trasmissione a sede del fnmov00t
028200021122     c                   if        nrr <> 0
028300021122     C                   EXSR      SBMTRA                                       *
028400021122     c                   end
028500030320     c                   end
028600030403     C                   EXSR      DLCMOV
028700060529     c* sottomette le stampe di controllo
028800060411     c                   if        *inkf
028900060529     c                   exsr      srsbm
029000060411     c                   end
029100060404     C*
029200910521     C                   SETON                                        LR
029300021115     C**-------------------------------------------------------------
029400060529     C** sottomette stampe
029500021115     C**-------------------------------------------------------------
029600060529     C     srsbm         BEGSR
029700140127      ****
029800060529     c* reperisco i po esteri gestiti dal po contabile
029900140122      ****
030000060529     c                   if        vfil = orgfc0
030100140122      ****
030200140122      ****  Sostituito il TRUL72R con il TRUL06R chiamato in modalità "£J"
030300140122      ****
030400140122     c                   clear                   L72
030500140122     c                   clear                   trul06ds
030600140122     C                   MOVEL     '£J'          D06COD
030700140122     C                   MOVEL     orgfc0        D06KEY
030800140122     C                   MOVEL     TRul06ds      KPJBU
030900140122     C                   CALL      'TRUL06R'
031000140122     C                   PARM                    KPJBA
031100140122     C                   MOVEL     KPJBU         TRul06ds
031200140122      ****
031300140122      ***  carico la schiera che utilizzava precedentemente con il TRUL72R
031400140122      **  con la schiera ricavata dal TRUL06R £J
031500140122      ***  in questo modo non viene modificato nulla sulle righe successive
031600140122     c                   do        30            cc
031700140122     c                   eval      l72(cc) = lin(cc)
031800140122     c                   end
031900161017      ****
032000161017     c                   else
032100161017      ****
032200161017      ****  se secondo Livello deve mettere se stesso e basta
032300161017     c                   eval      l72(1)=vfil
032400060529     c                   end
032500140122     c*
032600060529     c                   do        60            cc                3 0
032700161017     c*
032800161017     c*   deve uscire al primo vuoto
032900060529     c                   if        l72(cc) = 0 or cc = 60
033000161017     c                   leave
033100060529     c                   end
033200140122      ****
033300060529     C*§ SOTTOMETTO STAMPA VARIAZ. A C/ASSEGNI E P.ASSEGNATI
033400060529     c                   movel(p)  l72(cc)       kpjbu
033500060529     C                   MOVEL     'LR92'        KCOAZ
033600060529     C                   MOVEL     'KSSPECL'     KQEBI
033700060529     C                   CALL      'BCH10'
033800060529     C                   PARM                    KPJBA
033900140122      ****
034000060529     C*§ SOTTOMETTO STAMPA VARIAZ. A pre-pagati
034100060529     c                   movel(p)  l72(cc)       kpjbu
034200151019     c*                  eval      KJOST = 'H'
034300060529     C                   MOVEL     'LR96'        KCOAZ
034400060529     C                   MOVEL     'KSSPECL'     KQEBI
034500060529     C                   CALL      'BCH10'
034600060529     C                   PARM                    KPJBA
034700060529     C*§ SOTTOMETTO STAMPA Assegnati/c.assegni ancora da incassare
034800060529      * ricarico la £6 se veniva cambiato il P.O aveva i dati errati
034900060529     c                   clear                   trul06ds
035000060529     C                   MOVEL     '£6'          D06COD
035100060529     C                   MOVEL     l72(cc)       D06KEY
035200060529     C                   MOVEL     TRul06ds      KPJBU
035300060529     C                   CALL      'TRUL06R'
035400060529     C                   PARM                    KPJBA
035500060529     C                   MOVEL     KPJBU         TRul06ds
035600060529     c*
035700060529     c                   clear                   param
035800060529     c                   for       i = 1 to 29
035900060529     c                   eval      lip(i)=%editc(lin(i): 'X')
036000060529     c                   endfor
036100060529     c                   eval      parnfv = '00000'
036200060529     c                   eval      pardat = 99991231
036300060529     c                   eval      parinc = 'SI'
036400060529     c                   eval      pardfa = AMGUDT
036500060529     c                   eval      parsnp = 'N'
036600060529     c                   eval      parres = 'NO'
036700060529     c                   eval      kpjbu = param
036800060529     C                   MOVEL     'LR52'        KCOAZ
036900060529     C                   MOVEL     'KSSPECL'     KQEBI
037000151019     c*                  eval      KJOST = 'H'
037100060529     C                   CALL      'BCH10'
037200060529     C                   PARM                    KPJBA
037300060529     C*§ SOTTOMETTO STAMPA NOTE ACCREDITO OPERATORE 999 DEL GG PRECED.
037400060529     C                   Z-ADD     0             D52DFI                         *
037500060529     C                   Z-ADD     1             D52GIO                         *
037600060529     C                   MOVE      AMGUDT        D52DIN                         *
037700060529     C                   MOVE      '-'           D52SGN                         *
037800060529     C                   CALL      'TRUL52R'
037900060529     C                   PARM                    TRUL52ds
038000060529     C                   MOVE      D52DFI        DAT1GG            8 0          *
038100060529     C                   MOVE      l72(cc)       PFLE
038200060529     C                   MOVE      *ALL'9'       PCUC
038300060529     C                   MOVE      *ALL'9'       PNMI
038400060529     C                   MOVE      DAT1GG        PDMI
038500060529     C                   MOVEL     PARNAC        KPJBU
038600060529     C                   MOVEL     'LR93'        KCOAZ
038700060529     C                   MOVEL     'KSSPECL'     KQEBI
038800120628     c                   eval      KJOST = 'H'
038900060529     C                   CALL      'BCH10'
039000060529     C                   PARM                    KPJBA
039100060529     C*§ SOTTOMETTO STAMPA ELENCO SPEDIZIONI RIPRISTINATE DOPO CONTEGGI
039200060529     c                   movel(p)  l72(cc)       kpjbu
039300060529     C                   MOVEL     'LR94'        KCOAZ
039400060529     C                   MOVEL     'KSSPECL'     KQEBI
039500120628     c                   eval      KJOST = 'H'
039600060529     C                   CALL      'BCH10'
039700060529     C                   PARM                    KPJBA
039800060529     C*§ SOTTOMETTO STAMPA prepagati non incassati e non confermati
039900151019     c* n.b il parametro di congelamento spool rimane perchè nel pgm
040000151019     c* viene generata la stampa dei NON ICASSATI che deve essere congelata
040100151019     c* Nell'azione ci sarà quindi solo il relativo prtf
040200060529     c                   clear                   dsls01
040300060529     C                   eval      ds1fgs = l72(cc)
040400060529     C                   eval      ds1lnp = l72(cc)
040500060529     C                   MOVEL(p)  dsls01        KPJBU
040600060529     c                   move      'S'           kpjbu
040700060529     C                   MOVEL     'LSE8'        KCOAZ
040800060529     C                   MOVEL     'KSSPECL'     KQEBI
040900120628     c                   eval      KJOST = 'H'
041000060529     C                   CALL      'BCH10'
041100060529     C                   PARM                    KPJBA
041200160915      *
041300060529     c                   enddo
041400160915      *
041500060529     C                   ENDSR
041600060529     C**-------------------------------------------------------------
041700060529     C** inizializza dspf
041800060529     C**-------------------------------------------------------------
041900060529     C     inz01         BEGSR
042000021118     C                   MOVE      DUTCOU        VCDUS
042100060201     c                   z-add     0             DISPOSV
042200060201     c                   z-add     0             DISCONV
042300060201     c                   z-add     0             DISASSV
042400060201     c                   z-add     0             PREPOSV
042500060201     c                   z-add     0             PRECONV
042600060201     c                   z-add     0             PREASSV
042700060201     c                   z-add     0             FCCPOSV
042800060201     c                   z-add     0             FCCCONV
042900060201     c                   z-add     0             FCCASSV
043000060201     c                   z-add     0             CPOPOSV
043100060201     c                   z-add     0             CPOCONV
043200060201     c                   z-add     0             CPOASSV
043300060202     c                   z-add     0             raiPOSV
043400060202     c                   z-add     0             raiCONV
043500060202     c                   z-add     0             raiASSV
043600021115     c                   z-add     0             totincv
043700060201     c                   z-add     0             VERCONV
043800060201     c                   z-add     0             VERASSV
043900060201     c                   z-add     0             TOTVERV
044000021115     c                   z-add     0             casfv
044100060201     c                   z-add     0             CASCONV
044200060201     c                   z-add     0             CASASSV
044300060201     c                   z-add     0             CASTOTV
044400021115     c                   z-add     0             totcasv
044500060201     c                   z-add     0             CGPCONV
044600060201     c                   z-add     0             CGPassV
044700060201     c                   z-add     0             CGPtotV
044800060201     c                   z-add     0             RETCONV
044900060201     c                   z-add     0             RETASSV
045000060201     c                   z-add     0             RETTOTV
045100021115     c                   z-add     0             totgenv
045200021122     C* se non trovo il record vuol dire che è la prima volta e quindi
045300021122     c* rendo modificabile la data e l'importo della cassaforte del gg
045400021122     c* precedente
045500021122     c                   clear                   annqig
045600060201     c     kqig          setll     yfqig01l
045700060201     c     kqig1         readpe    yfqig01l
045800060201     c                   if        not %eof(yfqig01l)
045900021122     c                   seton                                        31
046000060201     c                   z-add     QIGICON       CGPCONV
046100060201     c                   z-add     QIGIASS       CGPassV
046200060202     c                   eval      cgptotv = qigicon+qigiass
046300021122     c                   move      qigdta        dtaiso
046400021125     c                   move      dtaiso        dtaeur
046500021125     c                   move      dtaeur        dtagpv
046600021122     c                   else
046700060201     c                   z-add     0             CGPCONV
046800060201     c                   z-add     0             CGPassV
046900060201     c                   z-add     0             CGPtotV
047000021122     c                   z-add     0             dtagpv
047100021122     c                   setoff                                       31
047200021122     c                   end
047300021115     c                   endsr
047400021115     C**-------------------------------------------------------------
047500021115     C** carica i dati a video
047600021115     C**-------------------------------------------------------------
047700021115     C     carica        BEGSR
047800060321     c* leggo tutti i record di yfmdi del giorno per po
047900060201     c                   z-add     0             DISPOSV
048000060201     c                   z-add     0             DISCONV
048100060201     c                   z-add     0             DISASSV
048200060201     c                   z-add     0             PREPOSV
048300060201     c                   z-add     0             PRECONV
048400060201     c                   z-add     0             PREASSV
048500060201     c                   z-add     0             FCCPOSV
048600060201     c                   z-add     0             FCCCONV
048700060201     c                   z-add     0             FCCASSV
048800060201     c                   z-add     0             CPOPOSV
048900060201     c                   z-add     0             CPOCONV
049000060201     c                   z-add     0             CPOASSV
049100060202     c                   z-add     0             raiPOSV
049200060202     c                   z-add     0             raiCONV
049300060202     c                   z-add     0             raiASSV
049400080513     c                   z-add     0             verconv
049500080513     c                   z-add     0             verassv
049600060321     c                   z-add     0             casfv
049700021125     c                   z-add     0             totincv
049800060406     c                   z-add     0             RETCONV
049900060406     c                   z-add     0             RETASSV
050000060406     c                   z-add     0             RETTOTV
050100060321     c* mandati d'introito
050200060321     c     qigdta        setgt     yfmdi01l
050300060321     c                   do        *hival
050400060321     c                   read      yfmdi01l
050500060321     c                   if        %eof(yfmdi01l)
050600060321     c                   leave
050700060321     c                   end
050800060529     c                   if        mdifle <> vfil
050900060321     c                   iter
051000060321     c                   end
051100060201     c* chiusura operatore
051200060201     c                   select
051300060201     c                   when      mditmi = 'O'
051400060321     c                   add       MDIICON       DISconV
051500060321     c                   add       MDIIASS       DISassV
051600060321     c                   add       MDIIPOS       DISposV
051700060405     c* pre-pagati
051800060201     c                   when      mditmi = 'P'
051900060321     c                   add       MDIICON       PREconV
052000060321     c                   add       MDIIASS       PREassV
052100060321     c                   add       MDIIPOS       PREposV
052200060405     c* fatture codificati
052300060201     c                   when      mditmi = 'F'
052400060321     c                   add       MDIICON       FCCconV
052500060321     c                   add       MDIIASS       FCCassV
052600060321     c                   add       MDIIPOS       FCCposV
052700060405     c* omessi parziali
052800060223     c                   when      mditmi = 'Z'
052900060321     c                   add       MDIICON       CPOconV
053000060321     c                   add       MDIIASS       CPOassV
053100060321     c                   add       MDIIPOS       CPOposV
053200060405     c* recupero assegni insoluti
053300060420     c                   when      mditmi = 'G'
053400060321     c                   add       MDIICON       RAIconV
053500060321     c                   add       MDIIASS       RAIassV
053600060321     c                   add       MDIIPOS       RAIposV
053700060202     c                   endsl
053800021115     c                   enddo
053900060214     c                   eval      totincv = disconv+disassv+preconv+preassv
054000060214     c                             +fccconv+fccassv+cpoconv+cpoassv+raiconv+
054100060214     c                             raiassv
054200021120     c* leggo tutti i record di FNVRT del giorno per po
054300021125     c                   z-add     0             totverv
054400021122     c     kvrt          setgt     fnvrt01l
054500021120     c                   do        *hival
054600021122     c     vfil          reade     fnvrt01l
054700021120     c                   if        %eof(fnvrt01l)
054800021120     c                   leave
054900021120     c                   end
055000060202     c                   add       vrtcon        verconv
055100060202     c                   add       vrtass        verassv
055200021120     c                   enddo
055300060202     c                   eval      totverv=verconv+verassv
055400060406     c* cassaforte feritoia
055500060605     c     vfil          setll     yfcas01l
055600060406     c                   do        *hival
055700060406     c     vfil          reade     yfcas01l
055800060406     c                   if        %eof(yfcas01l)
055900060406     c                   leave
056000060406     c                   end
056100060502     c                   if        casvie = 'S' or casqta < 0
056200060407     c                   iter
056300060407     c                   end
056400060406     c                   add       casICON       casfv
056500060406     c                   enddo
056600060406     c* rettifiche agli incassi
056700060406     c     kvrt          setgt     yfqri01l
056800060406     c                   do        *hival
056900060406     c     vfil          reade     yfqri01l
057000060406     c                   if        %eof(yfqri01l)
057100060406     c                   leave
057200060406     c                   end
057300060406     c                   add       qriICON       retconv
057400060406     c                   add       qriIass       retassv
057500060406     c                   enddo
057600060406     c                   eval      rettotv = retconv + retassv
057700060202     c* importo assegni cassaforte di servizio
057800060202     c                   clear                   okass             1 0
057900060202     c                   clear                   casassv
058000060202     c     vfil          setll     cnass01l
058100060202     c                   do        *hival
058200060202     c     vfil          reade     cnass01l
058300060202     c                   if        %eof(cnass01l)
058400060202     c                   leave
058500060202     c                   end
058600060202     c* segnalo che non sono stati presi in carico tutti i mandati perchè
058700060202     c* ci sono assegni ancora non resi versabili
058800060202     c                   if        assfve <> 'V'
058900060202     c                   eval      okass = 1
059000060202     c                   else
059100131129     c                   if        asstpi <> 'M'
059200060202     c                   add       asscas        casassv
059300060202     c                   end
059400131129     c                   end
059500060202     c                   enddo
059600021115     c                   endsr
059700021115     C**-------------------------------------------------------------
059800021122     C** controlli
059900021115     C**-------------------------------------------------------------
060000021115     C     ctr01         BEGSR
060100021120     c* se è la prima volta controllo che ci sia la data della cassaforte
060200021120     c* del gg precedente
060300021120     c                   if        dtagpv = 0
060400021120     C                   SETON                                        30
060500021120     c                   else
060600021120     c     *eur          test(d)                 dtagpv                 30
060700021120     c                   if        *in30
060800021120     c     *dmy          test(d)                 dtagpv                 30
060900021120     c                   if        not *in30
061000021120     c     *dmy          move      dtagpv        dtaeur
061100021120     c                   move      dtaeur        dtagpv
061200021120     c                   end
061300021120     c                   end
061400021120     c                   end
061500021115     c                   endsr
061600021122     C**-------------------------------------------------------------
061700021122     C** controlli e calcoli
061800021122     C**-------------------------------------------------------------
061900021122     C     ctr02         BEGSR
062000031023     c                   setoff                                       902899
062100060327     c                   setoff                                       404142
062200060414     c                   setoff                                       4345
062300021122     c                   clear                   $msg
062400060329     c                   do
062500021211     c* ritorno a controllare che non ci siano distinte senza chiusura op.
062600060419     C                   exsr      cntdis
062700021211     c                   if        not *in28
062800021122     c* totale incassato
062900060214     c                   eval      totincv = disconv+disassv+preconv+preassv
063000060214     c                             +fccconv+fccassv+cpoconv+cpoassv+raiconv+
063100060214     c                             raiassv
063200021122     c* totale cassaforte
063300021122     c                   z-add     casfv         totcasv
063400060202     c     casassv       add       casconv       castotv
063500060202     c                   add       castotv       totcasv
063600060329     c* cassaforte di servizio contante non deve essere > 1500 eur
063700060411     c                   if        casconv > §GETCS
063800031023     C                   SETON                                        99
063900060329     C                   MOVEL     ERR(1)        $MSG
064000060329     c                   iter
064100031023     c                   end
064200060329     c* se tutti gli assegni non sono versabili
064300060406     c                   if        okass=1
064400060406     C                   SETON                                        99
064500060406     C                   MOVEL     ERR(8)        $MSG
064600060329     c                   iter
064700060329     c                   end
064800060414     c* controllo se ci sono partite aperte sugli incassi d'attribuire
064900070731     c                   if        amgudt < §dgdtada or
065000070731     c                             amgudt > §dgdtaa
065100060502     c                   exsr      incatt
065200070731     c                   end
065300060414     c   99              iter
065400060329     c* totale generale
065500060329     c     cgpassv       add       cgpconv       cgptotv
065600060329     c                   z-add     cgptotv       totgenv
065700060329     c                   add       totincv       totgenv
065800060329     c                   sub       totverv       totgenv
065900060508     c                   sub       totcasv       totgenv
066000060508     c                   add       rettotv       totgenv
066100060511     C* LO RENDO POSITIVO
066200060511     C     totgenv       IFLT      *ZEROS
066300060511     C     totgenv       MULT      -1            §ABB
066400060511     C                   ELSE
066500060511     C                   Z-ADD     totgenv       §ABB
066600060511     C                   END
066700060511     C* CONTROLLO IL LIMITE SUPERIORE DELLA differenza
066800060511     c                   if        totgenv < 0 and §abb > §getqgt
066900060511     C                   SETON                                        99
067000060511     C                   MOVEL     ERR(7)        $MSG
067100060511     c                   iter
067200060511     c                   end
067300060511     c                   if        totgenv <> 0
067400060329     C                   SETON                                        9040
067500060329     c                   end
067600060518     c* per la partenza non controlla congruenza contante e assegni perchè
067700060518     c* la chiusura del gg prima viene messa tutta nel contante
067800060518     c                   if        AMGUDT <> 20060524
067900060327     c* congruenza contante
068000060411     c                   eval      comconinc =
068100060411     c                             ((disconv+preconv+fccconv+cpoconv+raiconv
068200060526     c                             +cgpconv)-(verconv) + retconv)
068300060411     c                   eval      comconver = (casfv+ casconv)
068400060411     C     comconinc     SUB       comconver     comABB
068500060411     C* LO RENDO POSITIVO
068600060411     C     comabb        IFLT      *ZEROS
068700060411     C     comabb        MULT      -1            §ABB
068800060411     C                   ELSE
068900060411     C                   Z-ADD     comabb        §ABB
069000060411     C                   END
069100060411     C* CONTROLLO IL LIMITE SUPERIORE DELL'ABBUOBO OPERATORE
069200060411     C     §ABB          ifgt      §GETQG
069300060411     C                   SETON                                        9041
069400060327     c                   end
069500060327     c* congruenza assegni
069600060411     c                   eval      comconinc =
069700060411     c                             ((disassv+preassv+fccassv+cpoassv+raiassv
069800060526     c                             +cgpassv)-(verassv)+ retassv)
069900060411     C     comconinc     SUB       casassv       comABB
070000060411     C* LO RENDO POSITIVO
070100060411     C     comabb        IFLT      *ZEROS
070200060411     C     comabb        MULT      -1            §ABB
070300060411     C                   ELSE
070400060411     C                   Z-ADD     comabb        §ABB
070500060411     C                   END
070600060411     C* CONTROLLO IL LIMITE SUPERIORE DELL'ABBUOBO OPERATORE
070700060411     C     §ABB          ifgt      §GETQG
070800060327     C                   SETON                                        9042
070900031023     c                   end
071000060329     c                   end
071100060518     c                   end
071200060329     c                   enddo
071300060329     c                   endsr
071400060203     C**----------------------------------
071500060203     C** CALCOLO SALDO INCASSI DA ATTRIBUIRE
071600060203     C**----------------------------------
071700060203     C     INCATT        BEGSR
071800060203     C                   SETOFF                                       1318
071900060414     C                   MOVE      *ZEROS        SALDPRF          15 2
072000060414     C                   MOVE      *ZEROS        SALDPRB          15 2
072100060203     C                   MOVE      '000000'      KCC               6            CAPOCONTO
072200060203     C                   MOVE      '00000000'    KSC               8            SOTTOCONTO
072300060203     C                   MOVE      §4OCC1        KCC                            CAPOCONTO
072400060203     C                   MOVE      §4OSC1        KSC                            SOTTOCONTO
072500060203      **
072600060203     C     K04ppa03      SETLL     ndppa03l
072700060203     C**
072800060203     C                   do        *hival
072900060203     C**
073000060203     C     K04ppa03      READE     ndppa03l
073100060203     c                   if        %eof(ndppa03l)
073200060203     C                   leave
073300060203     c                   end
073400060203     C**
073500060203     C     K08Pas03      SETLL     NdPas03l
073600060203do03 c                   do        *hival
073700060203     C     K08Pas03      READE     NdPas03l
073800060203      *
073900060203     c                   if        %eof(ndpas03l)
074000060203     C                   leave
074100060203     c                   end
074200060203     C                   move      pasdtpar      WDtpar            8 0
074300060214      * controllo se ho partite aperte negli incassi d'attrribuire di tre gg
074400060414      * indietro bloccante
074500060214do03 c                   if        wdtpar <= oggimeno3
074600060203if01 C     pasDAre       IFEQ      1
074700060414     C                   ADD       pasimportd    SALDPRB
074800060203el01 C                   ELSE
074900060414     C                   SUB       pasimportd    SALDPRB                        SALDO PART
075000060203en01 C                   ENDif
075100060203en01 C                   ENDif
075200060414      * controllo se ho partite aperte negli incassi d'attrribuire indietro
075300060414      * di 3 gg forzabile
075400060414do03 c                   if        wdtpar > oggimeno3 and wdtpar < oggimeno1
075500060414if01 C     pasDAre       IFEQ      1
075600060414     C                   ADD       pasimportd    SALDPRf
075700060414el01 C                   ELSE
075800060414     C                   SUB       pasimportd    SALDPRf                        SALDO PART
075900060414en01 C                   ENDif
076000060414en01 C                   ENDif
076100060203     C                   enddo
076200060203     C*
076300060414     C     saldprb       iflt      0
076400060414     C                   eval      saldprb= saldprb* -1
076500060203     C                   end
076600060414     C     saldprb       IFGt      §GEpin
076700060414     C                   SETON                                        9945
076800060203     c                   leave
076900060203     C                   End
077000060414     C*
077100060519     C*    saldprf       iflt      0
077200060519     C*                  eval      saldprf= saldprf* -1
077300060519     C*                  end
077400060519     C*    saldprf       IFGt      §GEpin
077500060519     C*                  SETON                                        9043
077600060519     c*                  leave
077700060519     C*                  End
077800060203     C                   enddo
077900060203     C                   ENDSR
078000021115     C**-------------------------------------------------------------
078100021120     C** srdta - data cassaforte al gg successivo x giroconto
078200021115     C**-------------------------------------------------------------
078300021120     C     srdta         BEGSR
078400021120     c                   do        *hival
078500021120     C                   EXFMT     CN0W102
078600030318     C   kl              leave
078700021120     c* controllo la data x giroconto
078800030514     c                   clear                   dtaeur
078900021120     c                   if        dtagsv = 0
079000021120     C                   SETON                                        32
079100021120     c                   else
079200021125     c     *eur          test(d)                 dtagsv                 32
079300021120     c                   if        *in32
079400021120     c     *dmy          test(d)                 dtagsv                 32
079500030514     c                   if        not *in32
079600030514     c     *dmy          move      dtagsv        dtaeur
079700030514     c                   end
079800030514     c                   else
079900030514     c                   move      dtagsv        dtaeur
080000021120     c                   end
080100030514     c                   end
080200030514     c*
080300030514     c                   if        not *in32
080400030514     c                   move      dtaeur        dtagsv
080500030514     c                   move      dtaeur        dtaiso
080600030514     c                   move      dtaiso        comdta            8 0
080700031003     c* controllo che la data sia maggiore di oggi
080800030514     c                   if        comdta <= amgudt
080900030514     c                   seton                                        32
081000030514     c                   end
081100031003     c* controllo che la data non sia maggiore di un mese da oggi
081200031003     c                   if        comdta > dtalimite
081300031003     c                   seton                                        32
081400031003     c                   end
081500030514     c                   end
081600030318     c                   if        not *in32 and *inkf
081700030318     c                   leave
081800030318     c                   end
081900021120     c                   enddo
082000021120     c                   endsr
082100060327     C**-------------------------------------------------------------
082200060327     C** srcomme - commento obbligatorio per f8 forzatura
082300060327     C**-------------------------------------------------------------
082400060327     C     srcomme       BEGSR
082500060427     c                   do        *hival
082600060427     c                   reset                   ana726ds
082700060427     c                   eval      opz726 = '01'
082800060427     c                   eval      ky1726 = 'CG'
082900060427     c                   eval      ky2726 = §4OCC1 + ' ' + §4OSC1
083000090424     c                   move      GMAUDt        anno              2 0
083100090424     c                   movel     GMAUDt        com06             6
083200090424     c                   move      anno          com06
083300060427     c                   eval      ky3726 = com06
083400060427     c                   eval      ky4726 = '000000' + %EDITC(vfil:'X') +
083500060427     c                             '      '
083600060427     c                   eval      kpjbu = ana726ds
083700060427     c                   call      'ANA726R'
083800060427     c                   parm                    kpjba
083900060427     c                   eval      ana726ds = kpjbu
084000060427     c                   if        opr726 <> '1'
084100060327     C                   SETON                                        44
084200060327     c                   else
084300060327     C                   SETOff                                       44
084400060427     c                   leave
084500060327     c                   end
084600060327     c                   enddo
084700060327     c                   endsr
084800060529     C**-------------------------------------------------------------
084900060529     C** srwin04 - richiesta per sede po da vedere
085000060529     C**-------------------------------------------------------------
085100060529     C     srwin04       BEGSR
085200060529     c                   do        *hival
085300060529     C                   EXFMT     CN0W104
085400060529     c   kc              leave
085500060529     c* controllo po
085600060529     c                   clear                   despov
085700060529     c                   if        pov = 0
085800060529     C                   iter
085900060529     c                   else
086000060529     c     pov           chain     azorg01l
086100060529     c                   if        %found(azorg01l)
086200060529     c                   movel     orgdes        despov
086300060529     c                   eval      vfil = pov
086400060529     c                   leave
086500060529     c                   else
086600060529     c                   movel     *all'?'       despov
086700060529     c                   iter
086800060529     c                   end
086900060529     c                   end
087000060529     c                   enddo
087100060529     c                   endsr
087200021120     C**-------------------------------------------------------------
087300021120     C** aggiorna
087400021120     C**-------------------------------------------------------------
087500021120     C     aggio         BEGSR
087600060404     c* leggo tutti i record di YFMDI del giorno per po
087700060404     c     qigdta        setgt     yfmdi01l
087800060404     c                   do        *hival
087900060404     c                   read      yfmdi01l
088000060404     c                   if        %eof(yfmdi01l)
088100060404     c                   leave
088200060404     c                   end
088300060404     c* solo il mio po
088400060529     c                   if        mdifle <> vfil
088500060404     c                   iter
088600060404     c                   end
088700060404     c* aggiorna i flag di presa incarico e versamente
088800060404     C                   MOVEL     'S'           mdiVIE
088900060404     c                   update    yfmdi000
089000060404     C                   ENDdo
089100021122     c* giroconto della cassaforte al gg successivo negli inc. attribuire
089200060202     c                   if        totcasv <> 0
089300021115     c                   exsr      cregir
089400021120     c                   end
089500021118     c* STAMPO
089600021120     c                   seton                                        99
089700060411     c* cassaforte gg precedente
089800060411     c                   seton                                        11
089900060411     c                   exsr      stampa
090000060411     c* rettifiche
090100060411     c                   exsr      staret
090200021120     c* mandati
090300021120     c                   exsr      stadis
090400021120     c* versamenti
090500021120     c                   exsr      staver
090600021118     c* cassaforte
090700060324     c                   exsr      stacass
090800021118     c* totale generale
090900021118     c                   seton                                        12
091000021118     c                   exsr      stampa
091100021115     c* se il totale è diverso da cassaforte lo segnalo anche in stampa
091200060511     c                   if        totgenv <> 0
091300021118     c                   seton                                        13
091400021115     c                   exsr      stampa
091500030331     c                   else
091600060321     c                   seton                                        29
091700030331     c                   exsr      stampa
091800021115     c                   end
091900021122     c* se è stata stornata la quadratura del giorno lo segnalo in stampa
092000021125     c                   movel     'A'           annqig
092100060202     c     kqig          chain     yfqig01l
092200060202     c                   if        %found(yfqig01l)
092300021120     c                   seton                                        15
092400021120     c                   exsr      stampa
092500021120     c* controllo anche il gg precedente perchè la quadratura la può aver
092600021120     c* rimandata ad oggi
092700021120     c                   else
092800030320     c* cerco l'ultimo gg non annullato
092900030320     c                   clear                   savdta
093000030320     c                   clear                   annqig
093100060202     c     kqig          setgt     yfqig01l
093200060202     c     kqig1         readpe    yfqig01l
093300060202     c                   if        not %eof(yfqig01l)
093400030320     c                   move      qigdta        savdta
093500030320     c                   end
093600030320     c* cerco l'ultimo gg annullato
093700030320     c                   clear                   qigdta
093800030320     c                   movel     'A'           annqig
093900060202     c     kqig          setgt     yfqig01l
094000060202     c     kqig1         readpe    yfqig01l
094100030320     c                   if        savdta < qigdta
094200021120     c                   seton                                        15
094300021120     c                   exsr      stampa
094400021120     c                   end
094500021120     c                   end
094600021125     c* memorizzo l'importo della cassaforte per la prossima quadratura
094700021125     c                   clear                   qigann
094800030320     c                   move      amgudt        qigdta
094900021125     c                   move      vfil          qigfle
095000021125     c                   move      dutcou        qigcuc
095100021125     c                   movel     'EUR'         qigvca
095200060202     c                   eval      qigicon = casfv + casconv
095300060202     c                   z-add     casassv       qigiass
095400060202     c                   write     yfqig000
095500021120     c                   endsr
095600021120     C**-------------------------------------------------------------
095700021120     C** stadis
095800021120     C**-------------------------------------------------------------
095900021120     C     stadis        BEGSR
096000060202     c                   clear                   totdiss
096100060202     c                   clear                   totpres
096200060202     c                   clear                   totfccs
096300060202     c                   clear                   totcpos
096400060214     c                   clear                   totrais
096500060411     c                   clear                   totcons
096600060411     c                   clear                   totasss
096700060411     c* testata incassi
096800060411     c                   seton                                        14
096900060411     c                   exsr      stampa
097000060214     c                   do        5             xx                1 0
097100021120     c* testata mandati
097200060203     c                   select
097300060202     c                   when      xx = 1
097400021120     c                   seton                                        01
097500060202     c                   eval      tmi = 'O'
097600060202     c                   when      xx = 2
097700060202     c                   seton                                        16
097800060202     c                   eval      tmi = 'P'
097900060202     c                   when      xx = 3
098000060202     c                   seton                                        19
098100060202     c                   eval      tmi = 'F'
098200060202     c                   when      xx = 4
098300060202     c                   seton                                        22
098400060223     c                   eval      tmi = 'Z'
098500060214     c                   when      xx = 5
098600060214     c                   seton                                        25
098700060420     c                   eval      tmi = 'G'
098800060202     c                   endsl
098900021120     c                   exsr      stampa
099000021120     c* dettaglio mandati
099100060202     c     qigdta        setgt     yfmdi01l
099200021120     c                   do        *hival
099300060202     c                   read      yfmdi01l
099400060202     c                   if        %eof(yfmdi01l)
099500021120     c                   leave
099600021120     c                   end
099700060529     c                   if        mdifle <> vfil
099800021122     c                   iter
099900021122     c                   end
100000060202     c                   if        mditmi <> tmi
100100060202     c                   iter
100200060202     c                   end
100300060202     c                   if        mdiicon<>0 or mdiiass<>0 or mdiipos<>0
100400060202     c                   select
100500060202     c                   when      xx = 1
100600060202     c                   seton                                        02
100700060406     c                   z-add     mdiipos       disposs
100800060406     c                   z-add     mdiicon       discons
100900060406     c                   z-add     mdiiass       disasss
101000060406     c                   eval      distots = discons+disasss
101100060406     c                   add       distots       totdiss
101200060406     c                   add       disposs       distots
101300060411     c                   add       discons       totcons
101400060411     c                   add       disasss       totasss
101500060202     c                   when      xx = 2
101600060202     c                   seton                                        17
101700060406     c                   z-add     mdiipos       preposs
101800060406     c                   z-add     mdiicon       precons
101900060406     c                   z-add     mdiiass       preasss
102000060406     c                   eval      pretots = precons+preasss
102100060406     c                   add       pretots       totpres
102200060406     c                   add       preposs       pretots
102300060411     c                   add       precons       totcons
102400060411     c                   add       preasss       totasss
102500060202     c                   when      xx = 3
102600060202     c                   seton                                        20
102700060406     c                   z-add     mdiipos       fccposs
102800060406     c                   z-add     mdiicon       fcccons
102900060406     c                   z-add     mdiiass       fccasss
103000060406     c                   eval      fcctots = fcccons+fccasss
103100060406     c                   add       fcctots       totfccs
103200060406     c                   add       fccposs       fcctots
103300060411     c                   add       fcccons       totcons
103400060411     c                   add       fccasss       totasss
103500060321     c* cliente
103600060321     c                   exsr      srcli
103700060202     c                   when      xx = 4
103800060202     c                   seton                                        23
103900060406     c                   z-add     mdiipos       cpoposs
104000060406     c                   z-add     mdiicon       cpocons
104100060406     c                   z-add     mdiiass       cpoasss
104200060406     c                   eval      cpotots = cpocons+cpoasss
104300060406     c                   add       cpotots       totcpos
104400060406     c                   add       cpoposs       cpotots
104500060411     c                   add       cpocons       totcons
104600060411     c                   add       cpoasss       totasss
104700060214     c                   when      xx = 5
104800060214     c                   seton                                        26
104900060406     c                   z-add     mdiipos       raiposs
105000060406     c                   z-add     mdiicon       raicons
105100060406     c                   z-add     mdiiass       raiasss
105200060406     c                   eval      raitots = raicons+raiasss
105300060406     c                   add       raitots       totrais
105400060406     c                   add       raiposs       raitots
105500060411     c                   add       raicons       totcons
105600060411     c                   add       raiasss       totasss
105700060202     c                   endsl
105800021120     c                   exsr      stampa
105900021120     c                   end
106000021120     c                   enddo
106100021120     c* totale mandati
106200060202     c                   select
106300060202     c                   when      xx = 1
106400060202     c                   seton                                        03
106500060202     c                   when      xx = 2
106600060202     c                   seton                                        18
106700060202     c                   when      xx = 3
106800060202     c                   seton                                        21
106900060202     c                   when      xx = 4
107000060202     c                   seton                                        24
107100060214     c                   when      xx = 5
107200060214     c                   seton                                        27
107300060202     c                   endsl
107400021120     c                   exsr      stampa
107500060202     c                   enddo
107600060411     c* totale incassi
107700060411     c                   seton                                        05
107800060411     c                   eval      totincs = totdiss+totpres+totfccs+totcpos
107900060411     c                             +totrais
108000060411     c                   exsr      stampa
108100021120     c                   endsr
108200021120     C**-------------------------------------------------------------
108300060321     C** decodifica il cliente codificato
108400021120     C**-------------------------------------------------------------
108500060321     C     srcli         BEGSR
108600060321     c                   clear                   cnacods
108700060321     c                   clear                   esito
108800060321     c                   callp     CHKcliente(*omit:*omit:mdiksc:*omit:*off
108900060321     c                             :*omit:*omit:*omit:esito:errc:*omit:cnacods
109000060321     c                             :*omit:*omit:*omit)
109100060321     c                   if        esito = 0
109200060321     c                   movel     acorag        desksc
109300060321     c                   else
109400060321     c                   clear                   desksc
109500060321     c                   end
109600060321     c                   endsr
109700060321     C**-------------------------------------------------------------
109800060321     C** staver
109900060321     C**-------------------------------------------------------------
110000060321     C     staver        BEGSR
110100060202     c                   clear                   totvrts
110200060411     c                   clear                   totvrcs
110300060411     c                   clear                   totvras
110400021120     c* testata versamenti
110500021120     c                   seton                                        07
110600021120     c                   exsr      stampa
110700021120     c* dettaglio versamenti
110800021122     c     kvrt          setgt     fnvrt01l
110900021120     c                   do        *hival
111000021122     c     vfil          reade     fnvrt01l
111100021120     c                   if        %eof(fnvrt01l)
111200021120     c                   leave
111300021120     c                   end
111400021120     c                   seton                                        08
111500060406     c                   move      vrtddv        dtaiso
111600060406     c                   move      dtaiso        dtaeur
111700060406     c                   move      dtaeur        eurddv
111800060406     c                   z-add     vrtass        vrtasss
111900060406     c                   z-add     vrtcon        vrtcons
112000060406     c     vrtasss       add       vrtcons       vrttots
112100060406     c                   add       vrttots       totvrts
112200060411     c                   add       vrtcons       totvrcs
112300060411     c                   add       vrtasss       totvras
112400021120     c                   exsr      stampa
112500021120     c                   enddo
112600021120     c* totale versamenti
112700021120     c                   seton                                        09
112800021120     c                   exsr      stampa
112900021115     c                   endsr
113000060406     C**-------------------------------------------------------------
113100060406     C** staret
113200060406     C**-------------------------------------------------------------
113300060406     C     staret        BEGSR
113400060406     c                   clear                   totrets
113500060406     c* testata rettifiche
113600060406     c                   seton                                        04
113700060406     c                   exsr      stampa
113800060418     c* dettaglio rettifiche
113900060406     c     kvrt          setgt     yfqri01l
114000060406     c                   do        *hival
114100060406     c     vfil          reade     yfqri01l
114200060406     c                   if        %eof(yfqri01l)
114300060406     c                   leave
114400060406     c                   end
114500060418     c                   exsr      srrett
114600060406     c                   move      qridtr        dtaiso
114700060406     c                   move      dtaiso        dtaeur
114800060406     c                   move      dtaeur        retdtas
114900060406     c                   z-add     qriiass       retasss
115000060406     c                   z-add     qriicon       retcons
115100060406     c     retasss       add       retcons       rettots
115200060406     c                   add       rettots       totrets
115300060406     c                   seton                                        38
115400060406     c                   exsr      stampa
115500060406     c                   enddo
115600060418     c* totale rettifiche
115700060406     c                   seton                                        39
115800060406     c                   exsr      stampa
115900060406     c                   endsr
116000021115     C**-------------------------------------------------------------
116100060418     C* Reperisco decodifica rettifica
116200021115     C**-------------------------------------------------------------
116300060418     C     srrett        BEGSR
116400060418     C                   Movel     'YRT'         Tbecod
116500060418     C                   Movel(p)  qricau        Tbeke1
116600060418     C                   Movel     *blanks       Tbeke2
116700060418     C     Ktbe1         Chain     Tntbe01l
116800060418     c                   if        %found(tntbe01l)
116900060418     C                   Movel     tbeuni        dyrt
117000060418     c                   else
117100060418     c                   clear                   dgei
117200060418     c                   end
117300060418     c                   movel     §yrtdes       descau
117400060418     c                   endsr
117500060418     C**-------------------------------------------------------------
117600060418     C** cassaforti
117700060418     C**-------------------------------------------------------------
117800060418     C     stacass       BEGSR
117900060327     c* testata cassaforte feritoia
118000060327     c                   seton                                        33
118100060327     c                   exsr      stampa
118200060327     c* dettaglio mandatini
118300060605     c                   clear                   savcuc
118400060605     c                   clear                   savda8
118500060605     c                   clear                   savora
118600060605     c                   clear                   impfers
118700060605     c     vfil          setll     yfcas01l
118800060327     c                   do        *hival
118900060406     c     vfil          reade     yfcas01l
119000060327     c                   if        %eof(yfcas01l)
119100060605     c                   if        impfers <> 0
119200060605     c                   seton                                        34
119300060605     c                   exsr      stampa
119400060605     c                   end
119500060327     c                   leave
119600060327     c                   end
119700060502     c                   if        casvie = 'S' or casqta < 0
119800060407     c                   iter
119900060407     c                   end
120000060605     c                   if        savcuc <> 0 and
120100060605     c                             (savcuc <> cascuc or savda8 <> casda8 or
120200060605     c                             savora <> casora)
120300060327     c                   seton                                        34
120400060327     c                   exsr      stampa
120500060605     c                   clear                   impfers
120600060605     c                   end
120700060605     c                   eval      cucfers = cascuc
120800060605     c                   eval      orafers = casora
120900060605     c                   move      casda8        dtaiso
121000060605     c                   move      dtaiso        dtaeur
121100060605     c                   move      dtaeur        dtafers
121200060605     c                   add       casicon       impfers
121300060605     c                   eval      savcuc = cascuc
121400060605     c                   eval      savda8 = casda8
121500060605     c                   eval      savora = casora
121600060327     c                   enddo
121700060327     c* totale cassaforte feritoia
121800060327     c                   seton                                        35
121900060327     c                   exsr      stampa
122000060327     c* testata cassaforte servizio
122100060327     c                   seton                                        36
122200060327     c                   exsr      stampa
122300060327     c* dettaglio contante
122400060327     c     knraz         setll     fidci01l
122500060327     c                   do        *hival
122600060327     c     knraz         reade     fidci01l
122700060327     c                   if        %eof(fidci01l)
122800060327     c                   leave
122900060327     c                   end
123000060327     c                   eval      qtadets = dciqta
123100060327     c                   eval      valdets = dcivalore
123200060327     c                   eval      impdets = dciqta * dcivalore
123300060327     c                   seton                                        10
123400060327     c                   exsr      stampa
123500060327     c                   enddo
123600060327     c* totale cassaforte feritoia
123700060327     c                   seton                                        37
123800060327     c                   exsr      stampa
123900060327     c                   endsr
124000060327     C**-------------------------------------------------------------
124100060327     C** stampa
124200060327     C**-------------------------------------------------------------
124300060327     C     stampa        BEGSR
124400021120     c* mandato in stampa
124500060406     c                   if        *in02 or *in17 or *in20 or *in23 or *in26
124600060202     c                   move      mdidmi        dtaiso
124700021125     c                   move      dtaiso        dtaeur
124800060203     c                   move      dtaeur        eurdmi
124900060202     c                   end
125000021120     c* messaggio storno
125100021120     c                   if        *in15
125200021121     c                   move      qigdta        dtaiso
125300021125     c                   move      dtaiso        dtaeur
125400021125     c                   move      dtaeur        eurdta
125500021120     c                   end
125600021120     c*
125700021121     c   99              write     testa
125800021118     c                   setoff                                       99
125900021121     c   14              write     tesinc
126000021121     c   01              write     tesman
126100021121     c   02              write     detman
126200021121     c   03              write     totman
126300060202     c   16              write     tespre
126400060202     c   17              write     detpre
126500060202     c   18              write     totpre
126600060202     c   19              write     tesfcc
126700060202     c   20              write     detfcc
126800060202     c   21              write     totfcc
126900060202     c   22              write     tescpo
127000060202     c   23              write     detcpo
127100060202     c   24              write     totcpo
127200060214     c   25              write     tesrai
127300060214     c   26              write     detrai
127400060214     c   27              write     totrai
127500021121     c   05              write     totinc
127600021121     c   07              write     tesver
127700021121     c   08              write     detver
127800021121     c   09              write     totver
127900060327     c   33              write     tesfer
128000060327     c   34              write     detfer
128100060327     c   35              write     totfer
128200060327     c   36              write     tescas
128300060327     c   10              write     detcas
128400060327     c   37              write     totcas
128500021121     c   11              write     detcasg
128600060406     c   04              write     tesret
128700060406     c   38              write     detret
128800060406     c   39              write     totret
128900021121     c   12              write     totgen
129000060321     c   29              write     quaok
129100030331     c   13              write     errore
129200021121     c   15              write     storno
129300021118     c                   movea     '000000000000'*in(01)
129400060202     c                   movea     '000000000000'*in(13)
129500060214     c                   movea     '000'         *in(25)
129600060321     c                   movea     '0'           *in(29)
129700060406     c                   movea     '0000000'     *in(33)
129800021115     c                   endsr
129900021118     C**-------------------------------------------------------------
130000021118     C** CREAZIONE MOVIMENTI CONTABILI di giroconto
130100021118     C**-------------------------------------------------------------
130200021120     C     CREGIR        BEGSR
130300021115     C* DARE
130400021115     C                   MOVE      KNMUS         MOVNMU
130500021115     C                   MOVE      KNRAZ         MOVRIF
130600021118     C                   MOVEL     vfil          MOVRIF
130700021120     C                   add       1             MOVSBN
130800021115     C                   Z-ADD     1             MOVSBR                         *PROGR.REG
130900021115     C                   MOVEL     *BLANKS       MOVFGE
131000060203     C                   MOVEL     soc           MOVSOC
131100021115     C                   MOVEL     *BLANKS       MOVFPN
131200021115     C                   MOVEL     vfil          MOVFPN
131300021115     C                   MOVEL     'D'           MOVTMO
131400021115     C                   MOVEL     'CG'          MOVCTB
131500021120     C                   MOVE      VCDUS         WCOMO6
131600021120     C                   MOVEL     vfil          WCOMO6
131700021120     C                   MOVEL     WCOMO6        movnrg
131800021120     C                   MOVE      9             movnrg
131900021118     C                   Z-ADD     AMGUDT        MOVDRG
132000021115     C                   MOVE      'COGE'        MOVSRR
132100021118     C                   MOVEL     §4OCO5        MOVCOD
132200021115     C                   Z-ADD     1             MOVDAR
132300021115     C                   Z-ADD     0             MOVAVE
132400021115     C                   MOVEL     'EUR'         MOVDIV
132500021115     C                   Z-ADD     0             MOVNDC
132600021115     C                   Z-ADD     0             MOVDDC
132700021115     C                   MOVE      *BLANKS       MOVSRD
132800021115     C                   Z-ADD     vfil          MOVNRP
132900021125     C                   move      dtagsv        dtaeur
133000021125     C                   move      dtaeur        dtaiso
133100021120     C                   move      dtaiso        MOVDRP
133200021115     C                   MOVEL     *BLANKS       MOVSRP
133300021115     C                   MOVEL     'EUR'         MOVDIP
133400021115     C                   Z-ADD     totcasv       MOVIPM
133500021115     C                   Z-ADD     totcasv       MOVIPD
133600021125     C                   Z-ADD     TOTCASV       MOVIMM
133700021125     C                   Z-ADD     TOTCASV       MOVIMD
133800021115     C                   Z-ADD     0             MOVDSC                         *
133900021115     C                   MOVEL     *BLANKS       MOVDES
134000021115     C                   MOVE      *ZEROS        MOVKCC
134100021115     C                   MOVE      *ZEROS        MOVKSC
134200021115     C                   MOVE      *ZEROS        MOVCCP
134300021115     C                   MOVE      *ZEROS        MOVSCP
134400021115     C                   MOVE      §4OCC1        MOVkcc
134500021115     C                   MOVE      §4OSC1        MOVksc
134600021122     C                   MOVE      §4OCC1        MOVccp
134700021122     C                   MOVE      §4OSC1        MOVscp
134800021115     C                   MOVEL     *BLANKS       MOVCDC
134900021115     C                   Z-ADD     0             MOVDCA
135000021115     C                   Z-ADD     MOVDRG        MOVDOP
135100021115     C                   Z-ADD     1             MOVCMB
135200021115     C                   WRITE     FNMOV000
135300021122     C* AVERE
135400021122     C                   Z-ADD     2             MOVSBR                         *PROGR.REG
135500021122     C                   Z-ADD     0             MOVDAR
135600021122     C                   Z-ADD     1             MOVAVE
135700021122     C                   move      amgudt        MOVDRP
135800021122     C                   WRITE     FNMOV000
135900021115     C                   ENDSR
136000991118     C*----------------------------------------------------*
136100991118     C* ESEGUO OVRDBF SU FNMOV
136200991118     C*----------------------------------------------------*
136300991118     C     OVRMOV        BEGSR
136400991118     C*
136500020926     C                   MOVE      '046) '       VAR               8
136600021118     C                   MOVEL     vfil          VAR
136700020925     C                   Z-ADD     49            LUNG             15 5
136800991118     C** CONTROLLO SE ESISTE IL MEMBRO X FNMOV00T
136900060321     C                   SETOFF                                       98
137000991118     C                   MOVE      VAR           CMDD(1)
137100991118     C                   MOVEL     *BLANKS       COMMAN
137200030411     C                   MOVEl(p)  CMDD(1)       COMMAN           80
137300060321     C                   CALL      'QCMDEXC'                            98
137400991118     C                   PARM                    COMMAN
137500991118     C                   PARM                    LUNG
137600991118     C** ESEGUE ADDPFM DEL MEMBRO NUOVO
137700060321     C   98              DO
137800991118     C                   MOVE      VAR           CMDD(2)
137900991118     C                   MOVEL     *BLANKS       COMMAN
138000030411     C                   MOVEl(p)  CMDD(2)       COMMAN
138100991118     C                   CALL      'QCMDEXC'
138200991118     C                   PARM                    COMMAN
138300991118     C                   PARM                    LUNG
138400991118     C                   END
138500991118     C*+ ESEGUE IN OGNI CASO OVRDBF DEL MEMBRO DESIDERATO
138600991118     C                   MOVE      VAR           CMDD(3)
138700991118     C                   MOVEL     *BLANKS       COMMAN
138800030411     C                   MOVEl(p)  CMDD(3)       COMMAN
138900991118     C                   CALL      'QCMDEXC'
139000991118     C                   PARM                    COMMAN
139100991118     C                   PARM                    LUNG
139200991118     C* GUARDO SE è ALLOCATO
139300991118     C                   MOVEL     VAR           VAR
139400991118     C                   MOVE      ')'           VAR
139500991118     C                   MOVE      VAR           CMDD(4)
139600030411     C                   MOVEl(p)  CMDD(4)       COMMAN
139700020925     C                   Z-ADD     49            LUNCMD           15 5
139800991118     C                   CALL      'QCMDEXC'                            91
139900991118     C                   PARM                    COMMAN
140000991118     C                   PARM                    LUNCMD
140100991118     C*
140200021121     C  N91              OPEN      FNMOV00T                             91
140300030318     c                   if        *in91
140400030411     C                   MOVEL     'NO'          KPJBU
140500030318     C                   SETON                                        28
140600030318     C                   MOVEL     ERR(4)        $MSG
140700030411     c                   else
140800030318     c* controllo che il membro sia vuoto
140900030318     c                   exsr      tstobj
141000030318     c                   if        nrr <> 0
141100030318     C                   SETON                                        28
141200030318     C                   MOVEL     ERR(5)        $MSG
141300030411     c                   end
141400030318     c                   end
141500991118     C*
141600991118     C                   ENDSR
141700991118     C*----------------------------------------------------*
141800991118     C* ESEGUO Dlcobj  FNMOV
141900991118     C*----------------------------------------------------*
142000991118     C     DLCMOV        BEGSR
142100991118     C*
142200991118     C* GUARDO SE è ALLOCATO
142300991118     C*
142400021122     C  N91              CLOSE     FNMOV00T                             91
142500991118     C                   MOVE      VAR           CMDD(5)
142600030411     C                   MOVEl(p)  CMDD(5)       COMMAN
142700020925     C                   Z-ADD     49            LUNCMD           15 5
142800991118     C                   CALL      'QCMDEXC'                            91
142900991118     C                   PARM                    COMMAN
143000991118     C                   PARM                    LUNCMD
143100991118     C*
143200991118     C                   ENDSR
143300021122     C*-------------------------------------------------------
143400021122     ** Sottometto trasmissioni
143500021122     **-------------------------------------------------------
143600021122     C     SBMTRA        BEGSR
143700021122     C*
143800021122     C                   MOVEL     '046'         CMZDST                          DEST.:SED
143900021122     C                   MOVEL     'FNMOV00T'    CMZFLD                          DA: >>>
144000060529     C                   EVAL      CmzMbD = 'M' + %EDITC(vfil:'X') + '046'
144100021122     C                   MOVEL     O50PLA        CMZLBD
144200021122     C                   MOVEL     'FNMOV00R'    CMZFLA                          A: >>>
144300021122     C                   MOVEL     CmzMbD        CMZMBA
144400021122     C                   MOVEL     O50ALA        CMZLBA
144500021122     C                   MOVEL     'GAITRA'      CMZLBA
144600021122     C                   MOVE      O50ALA        WQUATR            4
144700021122     C     WQUATR        IFEQ      '101 '
144800021122     C                   MOVE      '201 '        CMZLBA
144900021122     C                   ENDIF
145000021122     C                   MOVE      '0000'        CMZORI                          ORA
145100021122     C                   MOVE      '0000000'     CMZDTI                          DATA
145200021122     C                   MOVE      'N'           CMZURG                          NOTTE
145300021122     C                   MOVE      'A'           CMZADD                          IN ADD
145400021122     C                   MOVE      'S'           CMZCLR                          CLEAR
145500021122     C                   MOVE      'S'           CMZFTR                          DA TRANS.
145600021122     C                   MOVE      *BLANK        CMZERR                          1= ERR.
145700021122     C                   MOVE      'Y029'        CMZAZN                          AZIONE
145800021122     C                   MOVEL     O50ASI        CMZSYF                          SIST.INF.
145900021122     C                   MOVEL     'GAITRA'      CMZSYF
146000021122     C                   MOVE      O50ASI        WQUATR
146100021122     C     WQUATR        IFEQ      '101 '
146200021122     C                   MOVE      '201 '        CMZSYF
146300021122     C                   ENDIF
146400021122     C                   MOVE      nrr           CMZNRR                          N.REC.
146500021122     C                   MOVE      *BLANKS       CMZPRR                          FILE PRE-
146600021122     C*
146700021122     C                   CLEAR                   W236
146800021122     C                   CALL      'TIBS53C'
146900021122     C                   PARM                    O50LCL
147000021122     C                   PARM                    CMZDST
147100021122     C                   PARM                    CMZFLD
147200021122     C                   PARM                    CMZMBD
147300021122     C                   PARM                    CMZLBD
147400021122     C                   PARM                    CMZFLA
147500021122     C                   PARM                    CMZMBA
147600021122     C                   PARM                    CMZLBA
147700021122     C                   PARM                    CMZORI
147800021122     C                   PARM                    CMZDTI
147900021122     C                   PARM                    CMZURG
148000021122     C                   PARM                    CMZADD
148100021122     C                   PARM                    CMZCLR
148200021122     C                   PARM                    CMZFTR
148300021122     C                   PARM                    CMZERR
148400021122     C                   PARM                    CMZAZN
148500021122     C                   PARM                    CMZSYF
148600021122     C                   PARM                    CMZNRR
148700021122     C                   PARM                    CMZPRR
148800021122     C                   PARM                    W236            236
148900021122     C*
149000021122     C                   ENDSR
149100021122      *-----------------------------------------------------***********
149200021122      * TEST NR RECORD FILE DA TRASMETTER                   *  TSTOBJ *
149300021122      *-----------------------------------------------------***********
149400021122     C     TSTOBJ        BEGSR
149500021122     C* Richiamo TIBS55R per conoscere librerie/S.Informativi
149600021122     C* da/a cui spedire
149700021122     C                   CLEAR                   TIBS55ds
149800021122     C                   MOVEL     'L'           I50TLA
149900060529     C                   MOVEL     vfil          I50PPO
150000021122     C                   MOVEL     '046'         I50APO
150100021122     C                   CALL      'TIBS55R'
150200021122     C                   PARM                    TIBS55ds
150300060529     C* MEMBRO = M + vfil + 046
150400021122     C* GUARDO SE IL MEMBRO è VUOTO - FNMOV00T
150500021122     C                   MOVEL     o50pla        LIBR
150600021122     C                   MOVEL(p)  'FNMOV00T'    FILE
150700060529     C                   EVAL      mbr = 'M' + %EDITC(vfil:'X') + '046'
150800021122     C                   Z-ADD     0             NRR
150900021122     C                   CALL      'TRUL51C'
151000021122     C                   PARM                    LIBR             10
151100021122     C                   PARM                    FILE             10
151200021122     C                   PARM                    MBR              10
151300021122     C                   PARM                    NRR              10 0
151400021122     C                   ENDsr
151500991102      *-----------------------------------------------------***********
151600991102      * CONTROLLO DISTINTE CHIUSE NON CONTABILIZZATE        *  CNTDIS *
151700991102      *-----------------------------------------------------***********
151800991102     C     CNTDIS        BEGSR
151900021211      * OVRPRTF
152000021211     C     WOPNST        IFNE      'S'
152100021211     C                   MOVEL     'S'           WOPNST            1
152200120725     C                   MOVEA     CMDP          CMDPX            60
152300120725     C                   Z-ADD     60            LUNCMD           15 5
152400021211     C                   CALL      'QCMDEXC'                            92
152500021211     C                   PARM                    CMDPX
152600021211     C                   PARM                    LUNCMD
152700021211     C                   OPEN      QSYSPRT
152800021211     C                   END
152900991102     C* loop lettura distinte chiuse senza mandato d'introito
153000060529     C                   EVAL      ArbIPP = vfil
153100020925     C                   CLEAR                   ArbNMI
153200021211     C                   CLEAR                   UNO
153300021211     C     K02Arb71      SETLL     FnArb71L
153400021211     C                   DO        *HIVAL
153500021211     C     K02Arb71      READE     FnArb71L
153600021211     C                   if        %EOF(fnarb71l)
153700021211     C                   LEAVE
153800021211     C                   END
153900021211     C* ESCLUDO LE DISTINTE 999999
154000021211     C                   if        ARBNDC = *ALL'9'
154100021211     C                   ITER
154200021211     C                   END
154300021211     C                   IF        UNO = 0
154400991214     C                   SETON                                        28
154500991102     C                   MOVEL     ERR(2)        $MSG
154600021211     C* Stampo testata
154700021211     C                   EXCEPT    TESARB
154800021211     C                   Z-ADD     1             UNO               1 0
154900021211     C                   END
155000021211     C* Stampo DETTAGLIO
155100030318     c                   if        arbddc <> 0
155200030318     C                   MOVE      arbddc        DtaISO
155300030318     C                   MOVE      DTAISO        DtaEUR
155400021211     C                   MOVE      DTAEUR        GMADDC            8 0
155500030318     c                   else
155600030318     c                   clear                   gmaddc
155700030318     c                   end
155800120531     C* Decodifica Utente in stampa
155900120531     c                   clear                   nomUTE
156000120531     c                   z-add     arbIFP        arbfiliale        3 0
156100120531     c     kuteute       chain     azute02l
156200120531     c                   if        %Found(azute02l)
156300120531     c                   movel     filUTE        nomUTE
156400120531     c                   end
156500120816     c                   if        arbndc <> exarbndc
156600021211     C                   EXCEPT    DETARB
156700120816     c                   move      arbndc        exarbndc
156800120816     c                   endif
156900021211     C                   ENDDO
157000021211     C*
157100021211     C                   IF        UNO = 1
157200021211     C                   EXCEPT    FINEST
157300021211     C                   END
157400021119     C* controllo che ci sia una sola quadratura giornaliera
157500021120     c                   clear                   annqig
157600060202     c     kqig          chain     yfqig01l
157700060202     c                   if        %found(yfqig01l)
157800021119     C                   SETON                                        28
157900021119     C                   MOVEL     ERR(3)        $MSG
158000021119     c                   end
158100060424     c                   z-add     0             page
158200991102     C                   ENDSR
158300991102     C*-----------------------------------------------------------*
158400030318     C*  msg a cli* in sede di errata quadratura
158500991029     C*-----------------------------------------------------------*
158600030318     C     srmsg         BEGSR
158700090424     c                   move      GMAUDt        anno              2 0
158800090424     c                   movel     GMAUDt        com08             6
158900090424     c                   move      anno          com08
159000100820     C                   eval      emogg  = 'FORZATA quadratura giornaliera -
159100030318     C                             incassi e versamenti'
159200100820     C                   eval      emmsg  = 'Punto operativo '
159300060529     c                             + %editc(vfil:'X')
159400030318     c                             + ' in data '
159500030320     c                             + %subst(com08: 1: 2) + '/'
159600030320     c                             + %subst(com08: 3: 2) + '/'
159700060427     c                             + %subst(com08: 5: 2)
159800060328     c*?Se passato p.o.
159900060327     C                   if        *in40
160000100820     C                   EVAL      emmsg  = %trimr(emmsg) +
160100060328     C                             '. Totale cassaforte <> da totale generale'
160200060327     C                   END
160300060327     C                   if        *in41
160400100820     C                   EVAL      emmsg = %trimr(emmsg) +
160500060328     C                             '. Errore contante.'
160600060327     C                   END
160700060327     C                   if        *in42
160800100820     C                   EVAL      emmsg  = %trimr(emmsg) +
160900060328     C                             '. Errore assegni.'
161000060327     C                   END
161100060327     C                   if        *in43
161200100820     C                   EVAL      emmsg  = %trimr(emmsg) +
161300060414     C                             '. Inc. d''attribuire aperti da oltre 1gg.'
161400060327     C                   END
161500030318     C* UTENTE  A CUI INVIARE IL MESSAGGIO
161600100820     C*mm                MOVEL(P)  'CLI*'        MSGUTI
161700030318     C* FILIALE A CUI INVIARE IL MESSAGGIO
161800100820     C*mm                Z-ADD     046           MSGFIL
161900030318     C*
162000100820     C*                  CALL      'TNSYM4C'
162100100820     C*mm                PARM                    MSGTES           55
162200100820     C*mm                PARM                    MSGDET          198
162300100820     C*mm                PARM                    MSGFIL            3 0
162400100820     C*mm                PARM                    MSGUTI           10
162500130701     c                   eval      emlto = 'GestioneClienti@brt.it'
162600130701     c*                  eval      emlto = 'ufficio.pagamenti@bartolini.it'
162700100820     c                   eval      emlcc = *blank
162800100820     C                   CALL      'TIS701C1'
162900100820     C                   PARM                    emlto           253
163000100820     C                   PARM                    emlcc           253
163100100820     C                   PARM                    emogg            44
163200100820     C                   PARM                    emmsg          5000
163300100820     C                   PARM                    emesito           1
163400030318     C                   ENDSR
163500030318     C*-----------------------------------------------------------*
163600030318     C*  OPERAZIONI INIZIALI
163700030318     C*-----------------------------------------------------------*
163800030318     C     *INZSR        BEGSR
163900991029     C*
164000000000     C     *ENTRY        PLIST
164100000000     C                   PARM                    KPJBA
164200060517     C*---------- RICERCA DITTA :
164300060517     C                   MOVEL     'SOC001'      TIPXSC
164400060517     C                   MOVEL     'SOC001'      TIPXSC
164500060517     C                   MOVEL     *BLANK        SOCXSC
164600060517     C                   EXSR      REPSOC
164700060517     C     RTNXSC        IFNE      '1'
164800060517     C                   MOVEL     XSOCDS        SOC001
164900060517     C                   MOVEL     xscrgs        RSUT             20
165000060517     c                   else
165100060517     c                   seton                                        lr
165200060517     c                   return
165300060517     c                   end
165400900516      *
165500000000     C                   Z-ADD     1             CODUT
165600900517     C                   CALL      'X§PARUT'
165700900517     C                   PARM                    UT§DSE
165800021118     C*
165900021118     c     *dtaara       define    §azute        azuteds
166000021118     c     *dtaara       define    §datiute      ddatiute
166100021118     C                   in(E)     *dtaara
166200021118     C                   IF        %Error  or  RSUT = *blanks
166300021118     C                   call      'TIBS34R'
166400021118     C                   parm                    Tibs34Ds
166500021118     C                   in        *dtaara
166600021118     c                   ENDIF
166700060327     c                   movel     'CG'          Ctg
166800060327     c                   movel     '201'         soc
166900060529     C                   EVAL      VFil = dutpou
167000060529     c* sede
167100060529     c                   if        dutlpo = 'S' or dutlpo = ' '
167200060529     c                   seton                                        50
167300060529     c                   exsr      srwin04
167400060529     c                   end
167500040921     c* Descrizione p.o.
167600040921     C     vfil          CHAIN     AZORG01L                           21
167700040921     C  N21              MOVEL     ORGDES        DFil
167800060203     C* REPERISCO TABELLA Y4O
167900060203     C                   EXSR      SRY4O
168000021125     c* apro FNMOV00T
168100021125     c                   exsr      ovrmov
168200021118     C*--- RICERCA CAPOCONTO cassa------------------------------------*
168300021115     C                   DO        50            X                 2 0
168400021115     C                   MOVE      TCU(X)        TCUDS
168500021115     C     F56           CABNE     'CG'          END1
168600021115     C     F4            IFEQ      '4'
168700021115     C                   Z-ADD     KCU(X)        KCAS              4 0
168800021115     C                   END
168900021115     C     END1          TAG
169000021115     C                   END
169100090410     C                   TIME                    WHHUDT           14 0
169200991102     C                   TIME                    TIME              6 0
169300090410     C                   MOVE      WHHUDT        G08DAT
169400090410     C                   MOVE      *ZEROS        G08INV
169500090410     C                   MOVEL     *BLANK        G08ERR
169600090410     C                   CALL      'XSRDA8'
169700090410     C                   PARM                    WLBDA8
169800090410     C                   Z-ADD     G08INV        AMGUDt            8 0
169900090410     C                   Z-ADD     G08DAT        GMAUDt            8 0
170000031003     c* aggiungo un mese alla data di oggi
170100031003     c                   move      amgudt        dtaiso
170200031003     c                   adddur    1:*m          dtaiso
170300031003     c                   move      dtaiso        dtalimite
170400060426     C                   Z-ADD     0             D52dfi
170500061221     C                   Z-ADD     3             D52GIO
170600061221     c                   if        amgudt > 20061224 and
170700061221     c                             amgudt < 20070101
170800061221     C                   Z-ADD     5             D52GIO
170900061221     c                   end
171000060426     C                   MOVE      amgudt        D52DIN
171100060426     C                   MOVE      '-'           D52SGN
171200060426     C                   CALL      'TRUL52R'
171300060426     C                   PARM                    TRUL52DS
171400060426     C                   MOVE      D52DFI        oggimeno3         8 0
171500060426     C                   Z-ADD     0             D52dfi
171600060426     C                   Z-ADD     1             D52GIO
171700060426     C                   MOVE      amgudt        D52DIN
171800060426     C                   MOVE      '-'           D52SGN
171900060426     C                   CALL      'TRUL52R'
172000060426     C                   PARM                    TRUL52DS
172100060426     C                   MOVE      D52DFI        oggimeno1         8 0
172200060203     C* Reperisco tabella limite abbuoni
172300060203     C                   Movel     'GEI'         Tbecod
172400060203     C                   Movel     *blanks       Tbeke1
172500060203     C                   Movel     'EUR'         Tbeke1
172600060203     C                   Movel     *blanks       Tbeke2
172700060203     C     Ktbe1         Chain     Tntbe01l
172800060203     c                   if        %found(tntbe01l)
172900060203     C                   Movel     tbeuni        dgei
173000060203     c                   else
173100060203     c                   clear                   dgei
173200060203     c                   end
173300070731     C* Reperisco tabella DGD x controllo partite aperte
173400070731     C                   Movel     'DGD'         Tbecod
173500070731     C                   Movel     *blanks       Tbeke1
173600070731     C                   Movel(p)  '1'           Tbeke1
173700070731     C                   Movel     *blanks       Tbeke2
173800070731     C     Ktbe1         Chain     Tntbe01l
173900070731     c                   if        %found(tntbe01l)
174000070731     C                   Movel     tbeuni        ddgd
174100070731     c                   else
174200070731     c                   clear                   ddgd
174300070731     c                   end
174400060327     c* serve per emettere la finestra del contante la 1° volta
174500060327     c                   eval      flgcon = *on
174600900515      *---------------------------------------------------------------*
174700900515      * CHIAVI                                                        *
174800900515      *---------------------------------------------------------------*
174900021119     C     Kqig          KLIST
175000021120     C                   KFLD                    annqig
175100021120     C                   KFLD                    vfil
175200021119     C                   KFLD                    amgudt
175300021120     C     Kqig1         KLIST
175400021120     C                   KFLD                    annqig
175500021120     C                   KFLD                    vfil
175600021118     C     KTBE1         KLIST
175700021118     C                   KFLD                    tbecod
175800021118     C                   KFLD                    tbeke1
175900021118     C                   KFLD                    tbeke2
176000021120     C     Kvrt          KLIST
176100021121     C                   KFLD                    vfil
176200021122     C                   KFLD                    qigdta
176300021121     C     K02Arb71      KLIST
176400021121     C                   KFLD                    ArbIPP
176500021121     C                   KFLD                    ArbNMI
176600060203     C     K04Ppa03      KLIST
176700060203     C                   KFLD                    soc               3
176800060203     c                   KFLD                    CTG               2
176900060203     C                   KFLD                    KCC
177000060203     C                   KFLD                    KSC
177100060203     C     K08Pas03      KLIST
177200060203     C                   KFLD                    soc
177300060203     c                   KFLD                    CTG
177400060203     C                   KFLD                    KCC
177500060203     C                   KFLD                    KSC
177600060203     C                   KFLD                    PpaDtPar
177700060203     C                   KFLD                    PpaNrPar
177800060203     C                   KFLD                    PpaSerPar
177900120531     C     kuteute       KLIST
178000120531     C                   KFLD                    arbfiliale        3 0
178100120531     C                   KFLD                    arbCUC
178200991102     C*
178300991102     C* Controllo se ci sono distinte chiuse ancora da contabilizzare
178400060529     C  n50              EXSR      CNTDIS
178500991102     C*
178600991102     C                   ENDSR
178700060517     C*----------------------------------------------------*
178800060517     C* Reperimento dati società
178900060517     C*----------------------------------------------------*
179000060517     C     REPSOC        BEGSR
179100060517     C*
179200060517     C                   CALLB     'XSOC'
179300060517     C                   PARM                    TIPXSC            6
179400060517     C                   PARM                    SOCXSC            3
179500060517     C                   PARM                    CDSXSC            9 0
179600060517     C                   PARM                    MODXSC            3
179700060517     C                   PARM      *blanks       RTNXSC            1
179800060517     C                   PARM                    XSOCDS
179900060517     C                   PARM                    KPJBA
180000060517     C*
180100060517     C                   ENDSR
180200021115     c**********************************************************************
180300021115     C     SRY4O         BEGSR
180400021115     C*  CARICO I CONTI DA GENERARE  X FILIALE
180500021115     C                   MOVEL     'Y4O'         tbecod
180600060203     C                   MOVEl(p)  soc           tbeke1
180700021118     C                   MOVEL(p)  vfil          tbeke2
180800021118     C     ktbe1         CHAIN     Tntbe01l
180900021118     c                   if        %found(tntbe01l)
181000021118     C                   MOVEL     TBeUNI        Dy4O
181100021118     c                   else
181200021118     c                   clear                   dy4o
181300021118     c                   end
181400021115     C                   ENDSR
181500060202     C*-------------------------------------------------------
181600060202     ** GESTIONE CONTANTI
181700060202     **-------------------------------------------------------
181800060202     C     srtagli       BEGSR
181900060322     c                   clear                   cnc0wa00s
182000060322     c                   eval      £DIVISA ='EUR'
182100060322     c                   eval      kpjbu = cnc0wa00s
182200060322     c                   call      'CNC0WA00R'
182300060322     c                   parm                    kpjba
182400060322     c                   eval      cnc0wa00s = kpjbu
182500060322     c                   if        £RTNCMD = F6 and
182600060328     c                             £RTNCODE ='DONE'
182700060322     c                   eval      casconv = £IMPIMMESSO
182800060322     c                   end
182900060322     C                   ENDSR
183000060202      *
183100021211     OQSYSPRT   E            TESARB           02
183200021211     O                       RSUT                20
183300030318     O                                         + 10 'QUADRATURA GIORNALIERA INC'
183400030318     O                                         +  0 'ASSI E VERSAMENTI'
183500090410     O                       GMAUDt            + 13 '  /  /    '
183600021211     O                       TIME              +  1 '  :  :  '
183700021211     O                                         + 10 'CNC0W1R'
183800021211     O                       PAGE          Z    132
183900021211     O                                          128 'PAG.'
184000021211     O          E            TESARB         1 04
184100021211     O                                         + 30 'ELENCO DISTINTE AUTOTRAS'
184200021211     O                                         +  0 'PORTATORE CHIUSE E NON C'
184300021211     O                                         +  0 'ONTABILIZZATE'
184400021211     O          E            DETARB         1
184500021211     O                                         +  1 'OPERATORE'
184600021211     O                       ARBCUC        Z   +  1
184700120531     O                       NOMUTE            +  1
184800021211     O                                         +  1 'AUTOTRASP.'
184900021211     O                       ARBPDC        Z   +  1
185000021211     O                                         +  1 'NR.DISTINTA'
185100021211     O                       ARBNDC        Z   +  1
185200021211     O                                         +  1 'DEL'
185300021211     O                       GMADDC        8   +  1
185400021211     O          E            FINEST      2
185500021211     O                                         + 40 '** - F I N E    S T A M'
185600021211     O                                         +  1 'P A - **'
185700900606      *----------------------------------------------------------------
185800021118** ERR
185900060411La cass.di servizio ha un importo > di quello ammesso. Fare mandatino
186000021115ATT.NE : Distinte chiuse non contabilizzate
186100021119ATT.NE : Quadradura giornaliera già confermata
186200030318Non è possibile eseguire la quadratura. Attendere qualche minuto
186300031023ATT.NE : la precedente chiusura è terminata in modo anomalo
186400030318Ora si può premere F6 per confermare
186500060508ATT.NE : l'importo della DIFFERENZA non può essere negativo
186600060324ATT.NE : Ci sono assegni che non sono stati resi versabili
186700060327** CMDD
186800020925CHKOBJ OBJ(FNMOV00T) OBJTYPE(*FILE) MBR(M000000)
186900020925ADDPFM FILE(FNMOV00T)               MBR(M000000)
187000020925OVRDBF FILE(FNMOV00T)               MBR(M000000)
187100020925ALCOBJ OBJ((FNMOV00T *FILE *EXCL        M000000))
187200020925DLCOBJ OBJ((FNMOV00T *FILE *EXCL        M000000))
187300021211**  CMDP
187400120725OVRPRTF FILE(QSYSPRT) USRDTA(CARICO_MAN) SAVE(*YES)

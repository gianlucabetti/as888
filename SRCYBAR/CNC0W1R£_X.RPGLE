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
031900140122      ****
032000060529     c                   end
032100140122     c*
032200060529     c                   do        60            cc                3 0
032300060529     c                   if        l72(cc) = 0 or cc = 60
032400060529     c                   eval      l72(cc)=vfil
032500060529     c                   end
032600140122      ****
032700060529     C*§ SOTTOMETTO STAMPA VARIAZ. A C/ASSEGNI E P.ASSEGNATI
032800060529     c                   movel(p)  l72(cc)       kpjbu
032900060529     C                   MOVEL     'LR92'        KCOAZ
033000060529     C                   MOVEL     'KSSPECL'     KQEBI
033100060529     C                   CALL      'BCH10'
033200060529     C                   PARM                    KPJBA
033300140122      ****
033400060529     C*§ SOTTOMETTO STAMPA VARIAZ. A pre-pagati
033500060529     c                   movel(p)  l72(cc)       kpjbu
033600120628     c                   eval      KJOST = 'H'
033700060529     C                   MOVEL     'LR96'        KCOAZ
033800060529     C                   MOVEL     'KSSPECL'     KQEBI
033900060529     C                   CALL      'BCH10'
034000060529     C                   PARM                    KPJBA
034100060529     C*§ SOTTOMETTO STAMPA Assegnati/c.assegni ancora da incassare
034200060529      * ricarico la £6 se veniva cambiato il P.O aveva i dati errati
034300060529     c                   clear                   trul06ds
034400060529     C                   MOVEL     '£6'          D06COD
034500060529     C                   MOVEL     l72(cc)       D06KEY
034600060529     C                   MOVEL     TRul06ds      KPJBU
034700060529     C                   CALL      'TRUL06R'
034800060529     C                   PARM                    KPJBA
034900060529     C                   MOVEL     KPJBU         TRul06ds
035000060529     c*
035100060529     c                   clear                   param
035200060529     c                   for       i = 1 to 29
035300060529     c                   eval      lip(i)=%editc(lin(i): 'X')
035400060529     c                   endfor
035500060529     c                   eval      parnfv = '00000'
035600060529     c                   eval      pardat = 99991231
035700060529     c                   eval      parinc = 'SI'
035800060529     c                   eval      pardfa = AMGUDT
035900060529     c                   eval      parsnp = 'N'
036000060529     c                   eval      parres = 'NO'
036100060529     c                   eval      kpjbu = param
036200060529     C                   MOVEL     'LR52'        KCOAZ
036300060529     C                   MOVEL     'KSSPECL'     KQEBI
036400120628     c                   eval      KJOST = 'H'
036500060529     C                   CALL      'BCH10'
036600060529     C                   PARM                    KPJBA
036700060529     C*§ SOTTOMETTO STAMPA NOTE ACCREDITO OPERATORE 999 DEL GG PRECED.
036800060529     C                   Z-ADD     0             D52DFI                         *
036900060529     C                   Z-ADD     1             D52GIO                         *
037000060529     C                   MOVE      AMGUDT        D52DIN                         *
037100060529     C                   MOVE      '-'           D52SGN                         *
037200060529     C                   CALL      'TRUL52R'
037300060529     C                   PARM                    TRUL52ds
037400060529     C                   MOVE      D52DFI        DAT1GG            8 0          *
037500060529     C                   MOVE      l72(cc)       PFLE
037600060529     C                   MOVE      *ALL'9'       PCUC
037700060529     C                   MOVE      *ALL'9'       PNMI
037800060529     C                   MOVE      DAT1GG        PDMI
037900060529     C                   MOVEL     PARNAC        KPJBU
038000060529     C                   MOVEL     'LR93'        KCOAZ
038100060529     C                   MOVEL     'KSSPECL'     KQEBI
038200120628     c                   eval      KJOST = 'H'
038300060529     C                   CALL      'BCH10'
038400060529     C                   PARM                    KPJBA
038500060529     C*§ SOTTOMETTO STAMPA ELENCO SPEDIZIONI RIPRISTINATE DOPO CONTEGGI
038600060529     c                   movel(p)  l72(cc)       kpjbu
038700060529     C                   MOVEL     'LR94'        KCOAZ
038800060529     C                   MOVEL     'KSSPECL'     KQEBI
038900120628     c                   eval      KJOST = 'H'
039000060529     C                   CALL      'BCH10'
039100060529     C                   PARM                    KPJBA
039200060529     C*§ SOTTOMETTO STAMPA prepagati non incassati e non confermati
039300060529     c                   clear                   dsls01
039400060529     C                   eval      ds1fgs = l72(cc)
039500060529     C                   eval      ds1lnp = l72(cc)
039600060529     C                   MOVEL(p)  dsls01        KPJBU
039700060529     c                   move      'S'           kpjbu
039800060529     C                   MOVEL     'LSE8'        KCOAZ
039900060529     C                   MOVEL     'KSSPECL'     KQEBI
040000120628     c                   eval      KJOST = 'H'
040100060529     C                   CALL      'BCH10'
040200060529     C                   PARM                    KPJBA
040300060529     c* se l'ultimo po è il dutpou esco xchè vuol dire che ho già elaborato
040400060529     c* anche tutti gli eventuali po esteri
040500060529     c                   if        l72(cc)=vfil
040600060529     c                   leave
040700060529     c                   end
040800060529     c                   enddo
040900060529     C                   ENDSR
041000060529     C**-------------------------------------------------------------
041100060529     C** inizializza dspf
041200060529     C**-------------------------------------------------------------
041300060529     C     inz01         BEGSR
041400021118     C                   MOVE      DUTCOU        VCDUS
041500060201     c                   z-add     0             DISPOSV
041600060201     c                   z-add     0             DISCONV
041700060201     c                   z-add     0             DISASSV
041800060201     c                   z-add     0             PREPOSV
041900060201     c                   z-add     0             PRECONV
042000060201     c                   z-add     0             PREASSV
042100060201     c                   z-add     0             FCCPOSV
042200060201     c                   z-add     0             FCCCONV
042300060201     c                   z-add     0             FCCASSV
042400060201     c                   z-add     0             CPOPOSV
042500060201     c                   z-add     0             CPOCONV
042600060201     c                   z-add     0             CPOASSV
042700060202     c                   z-add     0             raiPOSV
042800060202     c                   z-add     0             raiCONV
042900060202     c                   z-add     0             raiASSV
043000021115     c                   z-add     0             totincv
043100060201     c                   z-add     0             VERCONV
043200060201     c                   z-add     0             VERASSV
043300060201     c                   z-add     0             TOTVERV
043400021115     c                   z-add     0             casfv
043500060201     c                   z-add     0             CASCONV
043600060201     c                   z-add     0             CASASSV
043700060201     c                   z-add     0             CASTOTV
043800021115     c                   z-add     0             totcasv
043900060201     c                   z-add     0             CGPCONV
044000060201     c                   z-add     0             CGPassV
044100060201     c                   z-add     0             CGPtotV
044200060201     c                   z-add     0             RETCONV
044300060201     c                   z-add     0             RETASSV
044400060201     c                   z-add     0             RETTOTV
044500021115     c                   z-add     0             totgenv
044600021122     C* se non trovo il record vuol dire che è la prima volta e quindi
044700021122     c* rendo modificabile la data e l'importo della cassaforte del gg
044800021122     c* precedente
044900021122     c                   clear                   annqig
045000060201     c     kqig          setll     yfqig01l
045100060201     c     kqig1         readpe    yfqig01l
045200060201     c                   if        not %eof(yfqig01l)
045300021122     c                   seton                                        31
045400060201     c                   z-add     QIGICON       CGPCONV
045500060201     c                   z-add     QIGIASS       CGPassV
045600060202     c                   eval      cgptotv = qigicon+qigiass
045700021122     c                   move      qigdta        dtaiso
045800021125     c                   move      dtaiso        dtaeur
045900021125     c                   move      dtaeur        dtagpv
046000021122     c                   else
046100060201     c                   z-add     0             CGPCONV
046200060201     c                   z-add     0             CGPassV
046300060201     c                   z-add     0             CGPtotV
046400021122     c                   z-add     0             dtagpv
046500021122     c                   setoff                                       31
046600021122     c                   end
046700021115     c                   endsr
046800021115     C**-------------------------------------------------------------
046900021115     C** carica i dati a video
047000021115     C**-------------------------------------------------------------
047100021115     C     carica        BEGSR
047200060321     c* leggo tutti i record di yfmdi del giorno per po
047300060201     c                   z-add     0             DISPOSV
047400060201     c                   z-add     0             DISCONV
047500060201     c                   z-add     0             DISASSV
047600060201     c                   z-add     0             PREPOSV
047700060201     c                   z-add     0             PRECONV
047800060201     c                   z-add     0             PREASSV
047900060201     c                   z-add     0             FCCPOSV
048000060201     c                   z-add     0             FCCCONV
048100060201     c                   z-add     0             FCCASSV
048200060201     c                   z-add     0             CPOPOSV
048300060201     c                   z-add     0             CPOCONV
048400060201     c                   z-add     0             CPOASSV
048500060202     c                   z-add     0             raiPOSV
048600060202     c                   z-add     0             raiCONV
048700060202     c                   z-add     0             raiASSV
048800080513     c                   z-add     0             verconv
048900080513     c                   z-add     0             verassv
049000060321     c                   z-add     0             casfv
049100021125     c                   z-add     0             totincv
049200060406     c                   z-add     0             RETCONV
049300060406     c                   z-add     0             RETASSV
049400060406     c                   z-add     0             RETTOTV
049500060321     c* mandati d'introito
049600060321     c     qigdta        setgt     yfmdi01l
049700060321     c                   do        *hival
049800060321     c                   read      yfmdi01l
049900060321     c                   if        %eof(yfmdi01l)
050000060321     c                   leave
050100060321     c                   end
050200060529     c                   if        mdifle <> vfil
050300060321     c                   iter
050400060321     c                   end
050500060201     c* chiusura operatore
050600060201     c                   select
050700060201     c                   when      mditmi = 'O'
050800060321     c                   add       MDIICON       DISconV
050900060321     c                   add       MDIIASS       DISassV
051000060321     c                   add       MDIIPOS       DISposV
051100060405     c* pre-pagati
051200060201     c                   when      mditmi = 'P'
051300060321     c                   add       MDIICON       PREconV
051400060321     c                   add       MDIIASS       PREassV
051500060321     c                   add       MDIIPOS       PREposV
051600060405     c* fatture codificati
051700060201     c                   when      mditmi = 'F'
051800060321     c                   add       MDIICON       FCCconV
051900060321     c                   add       MDIIASS       FCCassV
052000060321     c                   add       MDIIPOS       FCCposV
052100060405     c* omessi parziali
052200060223     c                   when      mditmi = 'Z'
052300060321     c                   add       MDIICON       CPOconV
052400060321     c                   add       MDIIASS       CPOassV
052500060321     c                   add       MDIIPOS       CPOposV
052600060405     c* recupero assegni insoluti
052700060420     c                   when      mditmi = 'G'
052800060321     c                   add       MDIICON       RAIconV
052900060321     c                   add       MDIIASS       RAIassV
053000060321     c                   add       MDIIPOS       RAIposV
053100060202     c                   endsl
053200021115     c                   enddo
053300060214     c                   eval      totincv = disconv+disassv+preconv+preassv
053400060214     c                             +fccconv+fccassv+cpoconv+cpoassv+raiconv+
053500060214     c                             raiassv
053600021120     c* leggo tutti i record di FNVRT del giorno per po
053700021125     c                   z-add     0             totverv
053800021122     c     kvrt          setgt     fnvrt01l
053900021120     c                   do        *hival
054000021122     c     vfil          reade     fnvrt01l
054100021120     c                   if        %eof(fnvrt01l)
054200021120     c                   leave
054300021120     c                   end
054400060202     c                   add       vrtcon        verconv
054500060202     c                   add       vrtass        verassv
054600021120     c                   enddo
054700060202     c                   eval      totverv=verconv+verassv
054800060406     c* cassaforte feritoia
054900060605     c     vfil          setll     yfcas01l
055000060406     c                   do        *hival
055100060406     c     vfil          reade     yfcas01l
055200060406     c                   if        %eof(yfcas01l)
055300060406     c                   leave
055400060406     c                   end
055500060502     c                   if        casvie = 'S' or casqta < 0
055600060407     c                   iter
055700060407     c                   end
055800060406     c                   add       casICON       casfv
055900060406     c                   enddo
056000060406     c* rettifiche agli incassi
056100060406     c     kvrt          setgt     yfqri01l
056200060406     c                   do        *hival
056300060406     c     vfil          reade     yfqri01l
056400060406     c                   if        %eof(yfqri01l)
056500060406     c                   leave
056600060406     c                   end
056700060406     c                   add       qriICON       retconv
056800060406     c                   add       qriIass       retassv
056900060406     c                   enddo
057000060406     c                   eval      rettotv = retconv + retassv
057100060202     c* importo assegni cassaforte di servizio
057200060202     c                   clear                   okass             1 0
057300060202     c                   clear                   casassv
057400060202     c     vfil          setll     cnass01l
057500060202     c                   do        *hival
057600060202     c     vfil          reade     cnass01l
057700060202     c                   if        %eof(cnass01l)
057800060202     c                   leave
057900060202     c                   end
058000060202     c* segnalo che non sono stati presi in carico tutti i mandati perchè
058100060202     c* ci sono assegni ancora non resi versabili
058200060202     c                   if        assfve <> 'V'
058300060202     c                   eval      okass = 1
058400060202     c                   else
058500131129     c                   if        asstpi <> 'M'
058600060202     c                   add       asscas        casassv
058700060202     c                   end
058800131129     c                   end
058900060202     c                   enddo
059000021115     c                   endsr
059100021115     C**-------------------------------------------------------------
059200021122     C** controlli
059300021115     C**-------------------------------------------------------------
059400021115     C     ctr01         BEGSR
059500021120     c* se è la prima volta controllo che ci sia la data della cassaforte
059600021120     c* del gg precedente
059700021120     c                   if        dtagpv = 0
059800021120     C                   SETON                                        30
059900021120     c                   else
060000021120     c     *eur          test(d)                 dtagpv                 30
060100021120     c                   if        *in30
060200021120     c     *dmy          test(d)                 dtagpv                 30
060300021120     c                   if        not *in30
060400021120     c     *dmy          move      dtagpv        dtaeur
060500021120     c                   move      dtaeur        dtagpv
060600021120     c                   end
060700021120     c                   end
060800021120     c                   end
060900021115     c                   endsr
061000021122     C**-------------------------------------------------------------
061100021122     C** controlli e calcoli
061200021122     C**-------------------------------------------------------------
061300021122     C     ctr02         BEGSR
061400031023     c                   setoff                                       902899
061500060327     c                   setoff                                       404142
061600060414     c                   setoff                                       4345
061700021122     c                   clear                   $msg
061800060329     c                   do
061900021211     c* ritorno a controllare che non ci siano distinte senza chiusura op.
062000060419     C                   exsr      cntdis
062100021211     c                   if        not *in28
062200021122     c* totale incassato
062300060214     c                   eval      totincv = disconv+disassv+preconv+preassv
062400060214     c                             +fccconv+fccassv+cpoconv+cpoassv+raiconv+
062500060214     c                             raiassv
062600021122     c* totale cassaforte
062700021122     c                   z-add     casfv         totcasv
062800060202     c     casassv       add       casconv       castotv
062900060202     c                   add       castotv       totcasv
063000060329     c* cassaforte di servizio contante non deve essere > 1500 eur
063100060411     c                   if        casconv > §GETCS
063200031023     C                   SETON                                        99
063300060329     C                   MOVEL     ERR(1)        $MSG
063400060329     c                   iter
063500031023     c                   end
063600060329     c* se tutti gli assegni non sono versabili
063700060406     c                   if        okass=1
063800060406     C                   SETON                                        99
063900060406     C                   MOVEL     ERR(8)        $MSG
064000060329     c                   iter
064100060329     c                   end
064200060414     c* controllo se ci sono partite aperte sugli incassi d'attribuire
064300070731     c                   if        amgudt < §dgdtada or
064400070731     c                             amgudt > §dgdtaa
064500060502     c                   exsr      incatt
064600070731     c                   end
064700060414     c   99              iter
064800060329     c* totale generale
064900060329     c     cgpassv       add       cgpconv       cgptotv
065000060329     c                   z-add     cgptotv       totgenv
065100060329     c                   add       totincv       totgenv
065200060329     c                   sub       totverv       totgenv
065300060508     c                   sub       totcasv       totgenv
065400060508     c                   add       rettotv       totgenv
065500060511     C* LO RENDO POSITIVO
065600060511     C     totgenv       IFLT      *ZEROS
065700060511     C     totgenv       MULT      -1            §ABB
065800060511     C                   ELSE
065900060511     C                   Z-ADD     totgenv       §ABB
066000060511     C                   END
066100060511     C* CONTROLLO IL LIMITE SUPERIORE DELLA differenza
066200060511     c                   if        totgenv < 0 and §abb > §getqgt
066300060511     C                   SETON                                        99
066400060511     C                   MOVEL     ERR(7)        $MSG
066500060511     c                   iter
066600060511     c                   end
066700060511     c                   if        totgenv <> 0
066800060329     C                   SETON                                        9040
066900060329     c                   end
067000060518     c* per la partenza non controlla congruenza contante e assegni perchè
067100060518     c* la chiusura del gg prima viene messa tutta nel contante
067200060518     c                   if        AMGUDT <> 20060524
067300060327     c* congruenza contante
067400060411     c                   eval      comconinc =
067500060411     c                             ((disconv+preconv+fccconv+cpoconv+raiconv
067600060526     c                             +cgpconv)-(verconv) + retconv)
067700060411     c                   eval      comconver = (casfv+ casconv)
067800060411     C     comconinc     SUB       comconver     comABB
067900060411     C* LO RENDO POSITIVO
068000060411     C     comabb        IFLT      *ZEROS
068100060411     C     comabb        MULT      -1            §ABB
068200060411     C                   ELSE
068300060411     C                   Z-ADD     comabb        §ABB
068400060411     C                   END
068500060411     C* CONTROLLO IL LIMITE SUPERIORE DELL'ABBUOBO OPERATORE
068600060411     C     §ABB          ifgt      §GETQG
068700060411     C                   SETON                                        9041
068800060327     c                   end
068900060327     c* congruenza assegni
069000060411     c                   eval      comconinc =
069100060411     c                             ((disassv+preassv+fccassv+cpoassv+raiassv
069200060526     c                             +cgpassv)-(verassv)+ retassv)
069300060411     C     comconinc     SUB       casassv       comABB
069400060411     C* LO RENDO POSITIVO
069500060411     C     comabb        IFLT      *ZEROS
069600060411     C     comabb        MULT      -1            §ABB
069700060411     C                   ELSE
069800060411     C                   Z-ADD     comabb        §ABB
069900060411     C                   END
070000060411     C* CONTROLLO IL LIMITE SUPERIORE DELL'ABBUOBO OPERATORE
070100060411     C     §ABB          ifgt      §GETQG
070200060327     C                   SETON                                        9042
070300031023     c                   end
070400060329     c                   end
070500060518     c                   end
070600060329     c                   enddo
070700060329     c                   endsr
070800060203     C**----------------------------------
070900060203     C** CALCOLO SALDO INCASSI DA ATTRIBUIRE
071000060203     C**----------------------------------
071100060203     C     INCATT        BEGSR
071200060203     C                   SETOFF                                       1318
071300060414     C                   MOVE      *ZEROS        SALDPRF          15 2
071400060414     C                   MOVE      *ZEROS        SALDPRB          15 2
071500060203     C                   MOVE      '000000'      KCC               6            CAPOCONTO
071600060203     C                   MOVE      '00000000'    KSC               8            SOTTOCONTO
071700060203     C                   MOVE      §4OCC1        KCC                            CAPOCONTO
071800060203     C                   MOVE      §4OSC1        KSC                            SOTTOCONTO
071900060203      **
072000060203     C     K04ppa03      SETLL     ndppa03l
072100060203     C**
072200060203     C                   do        *hival
072300060203     C**
072400060203     C     K04ppa03      READE     ndppa03l
072500060203     c                   if        %eof(ndppa03l)
072600060203     C                   leave
072700060203     c                   end
072800060203     C**
072900060203     C     K08Pas03      SETLL     NdPas03l
073000060203do03 c                   do        *hival
073100060203     C     K08Pas03      READE     NdPas03l
073200060203      *
073300060203     c                   if        %eof(ndpas03l)
073400060203     C                   leave
073500060203     c                   end
073600060203     C                   move      pasdtpar      WDtpar            8 0
073700060214      * controllo se ho partite aperte negli incassi d'attrribuire di tre gg
073800060414      * indietro bloccante
073900060214do03 c                   if        wdtpar <= oggimeno3
074000060203if01 C     pasDAre       IFEQ      1
074100060414     C                   ADD       pasimportd    SALDPRB
074200060203el01 C                   ELSE
074300060414     C                   SUB       pasimportd    SALDPRB                        SALDO PART
074400060203en01 C                   ENDif
074500060203en01 C                   ENDif
074600060414      * controllo se ho partite aperte negli incassi d'attrribuire indietro
074700060414      * di 3 gg forzabile
074800060414do03 c                   if        wdtpar > oggimeno3 and wdtpar < oggimeno1
074900060414if01 C     pasDAre       IFEQ      1
075000060414     C                   ADD       pasimportd    SALDPRf
075100060414el01 C                   ELSE
075200060414     C                   SUB       pasimportd    SALDPRf                        SALDO PART
075300060414en01 C                   ENDif
075400060414en01 C                   ENDif
075500060203     C                   enddo
075600060203     C*
075700060414     C     saldprb       iflt      0
075800060414     C                   eval      saldprb= saldprb* -1
075900060203     C                   end
076000060414     C     saldprb       IFGt      §GEpin
076100060414     C                   SETON                                        9945
076200060203     c                   leave
076300060203     C                   End
076400060414     C*
076500060519     C*    saldprf       iflt      0
076600060519     C*                  eval      saldprf= saldprf* -1
076700060519     C*                  end
076800060519     C*    saldprf       IFGt      §GEpin
076900060519     C*                  SETON                                        9043
077000060519     c*                  leave
077100060519     C*                  End
077200060203     C                   enddo
077300060203     C                   ENDSR
077400021115     C**-------------------------------------------------------------
077500021120     C** srdta - data cassaforte al gg successivo x giroconto
077600021115     C**-------------------------------------------------------------
077700021120     C     srdta         BEGSR
077800021120     c                   do        *hival
077900021120     C                   EXFMT     CN0W102
078000030318     C   kl              leave
078100021120     c* controllo la data x giroconto
078200030514     c                   clear                   dtaeur
078300021120     c                   if        dtagsv = 0
078400021120     C                   SETON                                        32
078500021120     c                   else
078600021125     c     *eur          test(d)                 dtagsv                 32
078700021120     c                   if        *in32
078800021120     c     *dmy          test(d)                 dtagsv                 32
078900030514     c                   if        not *in32
079000030514     c     *dmy          move      dtagsv        dtaeur
079100030514     c                   end
079200030514     c                   else
079300030514     c                   move      dtagsv        dtaeur
079400021120     c                   end
079500030514     c                   end
079600030514     c*
079700030514     c                   if        not *in32
079800030514     c                   move      dtaeur        dtagsv
079900030514     c                   move      dtaeur        dtaiso
080000030514     c                   move      dtaiso        comdta            8 0
080100031003     c* controllo che la data sia maggiore di oggi
080200030514     c                   if        comdta <= amgudt
080300030514     c                   seton                                        32
080400030514     c                   end
080500031003     c* controllo che la data non sia maggiore di un mese da oggi
080600031003     c                   if        comdta > dtalimite
080700031003     c                   seton                                        32
080800031003     c                   end
080900030514     c                   end
081000030318     c                   if        not *in32 and *inkf
081100030318     c                   leave
081200030318     c                   end
081300021120     c                   enddo
081400021120     c                   endsr
081500060327     C**-------------------------------------------------------------
081600060327     C** srcomme - commento obbligatorio per f8 forzatura
081700060327     C**-------------------------------------------------------------
081800060327     C     srcomme       BEGSR
081900060427     c                   do        *hival
082000060427     c                   reset                   ana726ds
082100060427     c                   eval      opz726 = '01'
082200060427     c                   eval      ky1726 = 'CG'
082300060427     c                   eval      ky2726 = §4OCC1 + ' ' + §4OSC1
082400090424     c                   move      GMAUDt        anno              2 0
082500090424     c                   movel     GMAUDt        com06             6
082600090424     c                   move      anno          com06
082700060427     c                   eval      ky3726 = com06
082800060427     c                   eval      ky4726 = '000000' + %EDITC(vfil:'X') +
082900060427     c                             '      '
083000060427     c                   eval      kpjbu = ana726ds
083100060427     c                   call      'ANA726R'
083200060427     c                   parm                    kpjba
083300060427     c                   eval      ana726ds = kpjbu
083400060427     c                   if        opr726 <> '1'
083500060327     C                   SETON                                        44
083600060327     c                   else
083700060327     C                   SETOff                                       44
083800060427     c                   leave
083900060327     c                   end
084000060327     c                   enddo
084100060327     c                   endsr
084200060529     C**-------------------------------------------------------------
084300060529     C** srwin04 - richiesta per sede po da vedere
084400060529     C**-------------------------------------------------------------
084500060529     C     srwin04       BEGSR
084600060529     c                   do        *hival
084700060529     C                   EXFMT     CN0W104
084800060529     c   kc              leave
084900060529     c* controllo po
085000060529     c                   clear                   despov
085100060529     c                   if        pov = 0
085200060529     C                   iter
085300060529     c                   else
085400060529     c     pov           chain     azorg01l
085500060529     c                   if        %found(azorg01l)
085600060529     c                   movel     orgdes        despov
085700060529     c                   eval      vfil = pov
085800060529     c                   leave
085900060529     c                   else
086000060529     c                   movel     *all'?'       despov
086100060529     c                   iter
086200060529     c                   end
086300060529     c                   end
086400060529     c                   enddo
086500060529     c                   endsr
086600021120     C**-------------------------------------------------------------
086700021120     C** aggiorna
086800021120     C**-------------------------------------------------------------
086900021120     C     aggio         BEGSR
087000060404     c* leggo tutti i record di YFMDI del giorno per po
087100060404     c     qigdta        setgt     yfmdi01l
087200060404     c                   do        *hival
087300060404     c                   read      yfmdi01l
087400060404     c                   if        %eof(yfmdi01l)
087500060404     c                   leave
087600060404     c                   end
087700060404     c* solo il mio po
087800060529     c                   if        mdifle <> vfil
087900060404     c                   iter
088000060404     c                   end
088100060404     c* aggiorna i flag di presa incarico e versamente
088200060404     C                   MOVEL     'S'           mdiVIE
088300060404     c                   update    yfmdi000
088400060404     C                   ENDdo
088500021122     c* giroconto della cassaforte al gg successivo negli inc. attribuire
088600060202     c                   if        totcasv <> 0
088700021115     c                   exsr      cregir
088800021120     c                   end
088900021118     c* STAMPO
089000021120     c                   seton                                        99
089100060411     c* cassaforte gg precedente
089200060411     c                   seton                                        11
089300060411     c                   exsr      stampa
089400060411     c* rettifiche
089500060411     c                   exsr      staret
089600021120     c* mandati
089700021120     c                   exsr      stadis
089800021120     c* versamenti
089900021120     c                   exsr      staver
090000021118     c* cassaforte
090100060324     c                   exsr      stacass
090200021118     c* totale generale
090300021118     c                   seton                                        12
090400021118     c                   exsr      stampa
090500021115     c* se il totale è diverso da cassaforte lo segnalo anche in stampa
090600060511     c                   if        totgenv <> 0
090700021118     c                   seton                                        13
090800021115     c                   exsr      stampa
090900030331     c                   else
091000060321     c                   seton                                        29
091100030331     c                   exsr      stampa
091200021115     c                   end
091300021122     c* se è stata stornata la quadratura del giorno lo segnalo in stampa
091400021125     c                   movel     'A'           annqig
091500060202     c     kqig          chain     yfqig01l
091600060202     c                   if        %found(yfqig01l)
091700021120     c                   seton                                        15
091800021120     c                   exsr      stampa
091900021120     c* controllo anche il gg precedente perchè la quadratura la può aver
092000021120     c* rimandata ad oggi
092100021120     c                   else
092200030320     c* cerco l'ultimo gg non annullato
092300030320     c                   clear                   savdta
092400030320     c                   clear                   annqig
092500060202     c     kqig          setgt     yfqig01l
092600060202     c     kqig1         readpe    yfqig01l
092700060202     c                   if        not %eof(yfqig01l)
092800030320     c                   move      qigdta        savdta
092900030320     c                   end
093000030320     c* cerco l'ultimo gg annullato
093100030320     c                   clear                   qigdta
093200030320     c                   movel     'A'           annqig
093300060202     c     kqig          setgt     yfqig01l
093400060202     c     kqig1         readpe    yfqig01l
093500030320     c                   if        savdta < qigdta
093600021120     c                   seton                                        15
093700021120     c                   exsr      stampa
093800021120     c                   end
093900021120     c                   end
094000021125     c* memorizzo l'importo della cassaforte per la prossima quadratura
094100021125     c                   clear                   qigann
094200030320     c                   move      amgudt        qigdta
094300021125     c                   move      vfil          qigfle
094400021125     c                   move      dutcou        qigcuc
094500021125     c                   movel     'EUR'         qigvca
094600060202     c                   eval      qigicon = casfv + casconv
094700060202     c                   z-add     casassv       qigiass
094800060202     c                   write     yfqig000
094900021120     c                   endsr
095000021120     C**-------------------------------------------------------------
095100021120     C** stadis
095200021120     C**-------------------------------------------------------------
095300021120     C     stadis        BEGSR
095400060202     c                   clear                   totdiss
095500060202     c                   clear                   totpres
095600060202     c                   clear                   totfccs
095700060202     c                   clear                   totcpos
095800060214     c                   clear                   totrais
095900060411     c                   clear                   totcons
096000060411     c                   clear                   totasss
096100060411     c* testata incassi
096200060411     c                   seton                                        14
096300060411     c                   exsr      stampa
096400060214     c                   do        5             xx                1 0
096500021120     c* testata mandati
096600060203     c                   select
096700060202     c                   when      xx = 1
096800021120     c                   seton                                        01
096900060202     c                   eval      tmi = 'O'
097000060202     c                   when      xx = 2
097100060202     c                   seton                                        16
097200060202     c                   eval      tmi = 'P'
097300060202     c                   when      xx = 3
097400060202     c                   seton                                        19
097500060202     c                   eval      tmi = 'F'
097600060202     c                   when      xx = 4
097700060202     c                   seton                                        22
097800060223     c                   eval      tmi = 'Z'
097900060214     c                   when      xx = 5
098000060214     c                   seton                                        25
098100060420     c                   eval      tmi = 'G'
098200060202     c                   endsl
098300021120     c                   exsr      stampa
098400021120     c* dettaglio mandati
098500060202     c     qigdta        setgt     yfmdi01l
098600021120     c                   do        *hival
098700060202     c                   read      yfmdi01l
098800060202     c                   if        %eof(yfmdi01l)
098900021120     c                   leave
099000021120     c                   end
099100060529     c                   if        mdifle <> vfil
099200021122     c                   iter
099300021122     c                   end
099400060202     c                   if        mditmi <> tmi
099500060202     c                   iter
099600060202     c                   end
099700060202     c                   if        mdiicon<>0 or mdiiass<>0 or mdiipos<>0
099800060202     c                   select
099900060202     c                   when      xx = 1
100000060202     c                   seton                                        02
100100060406     c                   z-add     mdiipos       disposs
100200060406     c                   z-add     mdiicon       discons
100300060406     c                   z-add     mdiiass       disasss
100400060406     c                   eval      distots = discons+disasss
100500060406     c                   add       distots       totdiss
100600060406     c                   add       disposs       distots
100700060411     c                   add       discons       totcons
100800060411     c                   add       disasss       totasss
100900060202     c                   when      xx = 2
101000060202     c                   seton                                        17
101100060406     c                   z-add     mdiipos       preposs
101200060406     c                   z-add     mdiicon       precons
101300060406     c                   z-add     mdiiass       preasss
101400060406     c                   eval      pretots = precons+preasss
101500060406     c                   add       pretots       totpres
101600060406     c                   add       preposs       pretots
101700060411     c                   add       precons       totcons
101800060411     c                   add       preasss       totasss
101900060202     c                   when      xx = 3
102000060202     c                   seton                                        20
102100060406     c                   z-add     mdiipos       fccposs
102200060406     c                   z-add     mdiicon       fcccons
102300060406     c                   z-add     mdiiass       fccasss
102400060406     c                   eval      fcctots = fcccons+fccasss
102500060406     c                   add       fcctots       totfccs
102600060406     c                   add       fccposs       fcctots
102700060411     c                   add       fcccons       totcons
102800060411     c                   add       fccasss       totasss
102900060321     c* cliente
103000060321     c                   exsr      srcli
103100060202     c                   when      xx = 4
103200060202     c                   seton                                        23
103300060406     c                   z-add     mdiipos       cpoposs
103400060406     c                   z-add     mdiicon       cpocons
103500060406     c                   z-add     mdiiass       cpoasss
103600060406     c                   eval      cpotots = cpocons+cpoasss
103700060406     c                   add       cpotots       totcpos
103800060406     c                   add       cpoposs       cpotots
103900060411     c                   add       cpocons       totcons
104000060411     c                   add       cpoasss       totasss
104100060214     c                   when      xx = 5
104200060214     c                   seton                                        26
104300060406     c                   z-add     mdiipos       raiposs
104400060406     c                   z-add     mdiicon       raicons
104500060406     c                   z-add     mdiiass       raiasss
104600060406     c                   eval      raitots = raicons+raiasss
104700060406     c                   add       raitots       totrais
104800060406     c                   add       raiposs       raitots
104900060411     c                   add       raicons       totcons
105000060411     c                   add       raiasss       totasss
105100060202     c                   endsl
105200021120     c                   exsr      stampa
105300021120     c                   end
105400021120     c                   enddo
105500021120     c* totale mandati
105600060202     c                   select
105700060202     c                   when      xx = 1
105800060202     c                   seton                                        03
105900060202     c                   when      xx = 2
106000060202     c                   seton                                        18
106100060202     c                   when      xx = 3
106200060202     c                   seton                                        21
106300060202     c                   when      xx = 4
106400060202     c                   seton                                        24
106500060214     c                   when      xx = 5
106600060214     c                   seton                                        27
106700060202     c                   endsl
106800021120     c                   exsr      stampa
106900060202     c                   enddo
107000060411     c* totale incassi
107100060411     c                   seton                                        05
107200060411     c                   eval      totincs = totdiss+totpres+totfccs+totcpos
107300060411     c                             +totrais
107400060411     c                   exsr      stampa
107500021120     c                   endsr
107600021120     C**-------------------------------------------------------------
107700060321     C** decodifica il cliente codificato
107800021120     C**-------------------------------------------------------------
107900060321     C     srcli         BEGSR
108000060321     c                   clear                   cnacods
108100060321     c                   clear                   esito
108200060321     c                   callp     CHKcliente(*omit:*omit:mdiksc:*omit:*off
108300060321     c                             :*omit:*omit:*omit:esito:errc:*omit:cnacods
108400060321     c                             :*omit:*omit:*omit)
108500060321     c                   if        esito = 0
108600060321     c                   movel     acorag        desksc
108700060321     c                   else
108800060321     c                   clear                   desksc
108900060321     c                   end
109000060321     c                   endsr
109100060321     C**-------------------------------------------------------------
109200060321     C** staver
109300060321     C**-------------------------------------------------------------
109400060321     C     staver        BEGSR
109500060202     c                   clear                   totvrts
109600060411     c                   clear                   totvrcs
109700060411     c                   clear                   totvras
109800021120     c* testata versamenti
109900021120     c                   seton                                        07
110000021120     c                   exsr      stampa
110100021120     c* dettaglio versamenti
110200021122     c     kvrt          setgt     fnvrt01l
110300021120     c                   do        *hival
110400021122     c     vfil          reade     fnvrt01l
110500021120     c                   if        %eof(fnvrt01l)
110600021120     c                   leave
110700021120     c                   end
110800021120     c                   seton                                        08
110900060406     c                   move      vrtddv        dtaiso
111000060406     c                   move      dtaiso        dtaeur
111100060406     c                   move      dtaeur        eurddv
111200060406     c                   z-add     vrtass        vrtasss
111300060406     c                   z-add     vrtcon        vrtcons
111400060406     c     vrtasss       add       vrtcons       vrttots
111500060406     c                   add       vrttots       totvrts
111600060411     c                   add       vrtcons       totvrcs
111700060411     c                   add       vrtasss       totvras
111800021120     c                   exsr      stampa
111900021120     c                   enddo
112000021120     c* totale versamenti
112100021120     c                   seton                                        09
112200021120     c                   exsr      stampa
112300021115     c                   endsr
112400060406     C**-------------------------------------------------------------
112500060406     C** staret
112600060406     C**-------------------------------------------------------------
112700060406     C     staret        BEGSR
112800060406     c                   clear                   totrets
112900060406     c* testata rettifiche
113000060406     c                   seton                                        04
113100060406     c                   exsr      stampa
113200060418     c* dettaglio rettifiche
113300060406     c     kvrt          setgt     yfqri01l
113400060406     c                   do        *hival
113500060406     c     vfil          reade     yfqri01l
113600060406     c                   if        %eof(yfqri01l)
113700060406     c                   leave
113800060406     c                   end
113900060418     c                   exsr      srrett
114000060406     c                   move      qridtr        dtaiso
114100060406     c                   move      dtaiso        dtaeur
114200060406     c                   move      dtaeur        retdtas
114300060406     c                   z-add     qriiass       retasss
114400060406     c                   z-add     qriicon       retcons
114500060406     c     retasss       add       retcons       rettots
114600060406     c                   add       rettots       totrets
114700060406     c                   seton                                        38
114800060406     c                   exsr      stampa
114900060406     c                   enddo
115000060418     c* totale rettifiche
115100060406     c                   seton                                        39
115200060406     c                   exsr      stampa
115300060406     c                   endsr
115400021115     C**-------------------------------------------------------------
115500060418     C* Reperisco decodifica rettifica
115600021115     C**-------------------------------------------------------------
115700060418     C     srrett        BEGSR
115800060418     C                   Movel     'YRT'         Tbecod
115900060418     C                   Movel(p)  qricau        Tbeke1
116000060418     C                   Movel     *blanks       Tbeke2
116100060418     C     Ktbe1         Chain     Tntbe01l
116200060418     c                   if        %found(tntbe01l)
116300060418     C                   Movel     tbeuni        dyrt
116400060418     c                   else
116500060418     c                   clear                   dgei
116600060418     c                   end
116700060418     c                   movel     §yrtdes       descau
116800060418     c                   endsr
116900060418     C**-------------------------------------------------------------
117000060418     C** cassaforti
117100060418     C**-------------------------------------------------------------
117200060418     C     stacass       BEGSR
117300060327     c* testata cassaforte feritoia
117400060327     c                   seton                                        33
117500060327     c                   exsr      stampa
117600060327     c* dettaglio mandatini
117700060605     c                   clear                   savcuc
117800060605     c                   clear                   savda8
117900060605     c                   clear                   savora
118000060605     c                   clear                   impfers
118100060605     c     vfil          setll     yfcas01l
118200060327     c                   do        *hival
118300060406     c     vfil          reade     yfcas01l
118400060327     c                   if        %eof(yfcas01l)
118500060605     c                   if        impfers <> 0
118600060605     c                   seton                                        34
118700060605     c                   exsr      stampa
118800060605     c                   end
118900060327     c                   leave
119000060327     c                   end
119100060502     c                   if        casvie = 'S' or casqta < 0
119200060407     c                   iter
119300060407     c                   end
119400060605     c                   if        savcuc <> 0 and
119500060605     c                             (savcuc <> cascuc or savda8 <> casda8 or
119600060605     c                             savora <> casora)
119700060327     c                   seton                                        34
119800060327     c                   exsr      stampa
119900060605     c                   clear                   impfers
120000060605     c                   end
120100060605     c                   eval      cucfers = cascuc
120200060605     c                   eval      orafers = casora
120300060605     c                   move      casda8        dtaiso
120400060605     c                   move      dtaiso        dtaeur
120500060605     c                   move      dtaeur        dtafers
120600060605     c                   add       casicon       impfers
120700060605     c                   eval      savcuc = cascuc
120800060605     c                   eval      savda8 = casda8
120900060605     c                   eval      savora = casora
121000060327     c                   enddo
121100060327     c* totale cassaforte feritoia
121200060327     c                   seton                                        35
121300060327     c                   exsr      stampa
121400060327     c* testata cassaforte servizio
121500060327     c                   seton                                        36
121600060327     c                   exsr      stampa
121700060327     c* dettaglio contante
121800060327     c     knraz         setll     fidci01l
121900060327     c                   do        *hival
122000060327     c     knraz         reade     fidci01l
122100060327     c                   if        %eof(fidci01l)
122200060327     c                   leave
122300060327     c                   end
122400060327     c                   eval      qtadets = dciqta
122500060327     c                   eval      valdets = dcivalore
122600060327     c                   eval      impdets = dciqta * dcivalore
122700060327     c                   seton                                        10
122800060327     c                   exsr      stampa
122900060327     c                   enddo
123000060327     c* totale cassaforte feritoia
123100060327     c                   seton                                        37
123200060327     c                   exsr      stampa
123300060327     c                   endsr
123400060327     C**-------------------------------------------------------------
123500060327     C** stampa
123600060327     C**-------------------------------------------------------------
123700060327     C     stampa        BEGSR
123800021120     c* mandato in stampa
123900060406     c                   if        *in02 or *in17 or *in20 or *in23 or *in26
124000060202     c                   move      mdidmi        dtaiso
124100021125     c                   move      dtaiso        dtaeur
124200060203     c                   move      dtaeur        eurdmi
124300060202     c                   end
124400021120     c* messaggio storno
124500021120     c                   if        *in15
124600021121     c                   move      qigdta        dtaiso
124700021125     c                   move      dtaiso        dtaeur
124800021125     c                   move      dtaeur        eurdta
124900021120     c                   end
125000021120     c*
125100021121     c   99              write     testa
125200021118     c                   setoff                                       99
125300021121     c   14              write     tesinc
125400021121     c   01              write     tesman
125500021121     c   02              write     detman
125600021121     c   03              write     totman
125700060202     c   16              write     tespre
125800060202     c   17              write     detpre
125900060202     c   18              write     totpre
126000060202     c   19              write     tesfcc
126100060202     c   20              write     detfcc
126200060202     c   21              write     totfcc
126300060202     c   22              write     tescpo
126400060202     c   23              write     detcpo
126500060202     c   24              write     totcpo
126600060214     c   25              write     tesrai
126700060214     c   26              write     detrai
126800060214     c   27              write     totrai
126900021121     c   05              write     totinc
127000021121     c   07              write     tesver
127100021121     c   08              write     detver
127200021121     c   09              write     totver
127300060327     c   33              write     tesfer
127400060327     c   34              write     detfer
127500060327     c   35              write     totfer
127600060327     c   36              write     tescas
127700060327     c   10              write     detcas
127800060327     c   37              write     totcas
127900021121     c   11              write     detcasg
128000060406     c   04              write     tesret
128100060406     c   38              write     detret
128200060406     c   39              write     totret
128300021121     c   12              write     totgen
128400060321     c   29              write     quaok
128500030331     c   13              write     errore
128600021121     c   15              write     storno
128700021118     c                   movea     '000000000000'*in(01)
128800060202     c                   movea     '000000000000'*in(13)
128900060214     c                   movea     '000'         *in(25)
129000060321     c                   movea     '0'           *in(29)
129100060406     c                   movea     '0000000'     *in(33)
129200021115     c                   endsr
129300021118     C**-------------------------------------------------------------
129400021118     C** CREAZIONE MOVIMENTI CONTABILI di giroconto
129500021118     C**-------------------------------------------------------------
129600021120     C     CREGIR        BEGSR
129700021115     C* DARE
129800021115     C                   MOVE      KNMUS         MOVNMU
129900021115     C                   MOVE      KNRAZ         MOVRIF
130000021118     C                   MOVEL     vfil          MOVRIF
130100021120     C                   add       1             MOVSBN
130200021115     C                   Z-ADD     1             MOVSBR                         *PROGR.REG
130300021115     C                   MOVEL     *BLANKS       MOVFGE
130400060203     C                   MOVEL     soc           MOVSOC
130500021115     C                   MOVEL     *BLANKS       MOVFPN
130600021115     C                   MOVEL     vfil          MOVFPN
130700021115     C                   MOVEL     'D'           MOVTMO
130800021115     C                   MOVEL     'CG'          MOVCTB
130900021120     C                   MOVE      VCDUS         WCOMO6
131000021120     C                   MOVEL     vfil          WCOMO6
131100021120     C                   MOVEL     WCOMO6        movnrg
131200021120     C                   MOVE      9             movnrg
131300021118     C                   Z-ADD     AMGUDT        MOVDRG
131400021115     C                   MOVE      'COGE'        MOVSRR
131500021118     C                   MOVEL     §4OCO5        MOVCOD
131600021115     C                   Z-ADD     1             MOVDAR
131700021115     C                   Z-ADD     0             MOVAVE
131800021115     C                   MOVEL     'EUR'         MOVDIV
131900021115     C                   Z-ADD     0             MOVNDC
132000021115     C                   Z-ADD     0             MOVDDC
132100021115     C                   MOVE      *BLANKS       MOVSRD
132200021115     C                   Z-ADD     vfil          MOVNRP
132300021125     C                   move      dtagsv        dtaeur
132400021125     C                   move      dtaeur        dtaiso
132500021120     C                   move      dtaiso        MOVDRP
132600021115     C                   MOVEL     *BLANKS       MOVSRP
132700021115     C                   MOVEL     'EUR'         MOVDIP
132800021115     C                   Z-ADD     totcasv       MOVIPM
132900021115     C                   Z-ADD     totcasv       MOVIPD
133000021125     C                   Z-ADD     TOTCASV       MOVIMM
133100021125     C                   Z-ADD     TOTCASV       MOVIMD
133200021115     C                   Z-ADD     0             MOVDSC                         *
133300021115     C                   MOVEL     *BLANKS       MOVDES
133400021115     C                   MOVE      *ZEROS        MOVKCC
133500021115     C                   MOVE      *ZEROS        MOVKSC
133600021115     C                   MOVE      *ZEROS        MOVCCP
133700021115     C                   MOVE      *ZEROS        MOVSCP
133800021115     C                   MOVE      §4OCC1        MOVkcc
133900021115     C                   MOVE      §4OSC1        MOVksc
134000021122     C                   MOVE      §4OCC1        MOVccp
134100021122     C                   MOVE      §4OSC1        MOVscp
134200021115     C                   MOVEL     *BLANKS       MOVCDC
134300021115     C                   Z-ADD     0             MOVDCA
134400021115     C                   Z-ADD     MOVDRG        MOVDOP
134500021115     C                   Z-ADD     1             MOVCMB
134600021115     C                   WRITE     FNMOV000
134700021122     C* AVERE
134800021122     C                   Z-ADD     2             MOVSBR                         *PROGR.REG
134900021122     C                   Z-ADD     0             MOVDAR
135000021122     C                   Z-ADD     1             MOVAVE
135100021122     C                   move      amgudt        MOVDRP
135200021122     C                   WRITE     FNMOV000
135300021115     C                   ENDSR
135400991118     C*----------------------------------------------------*
135500991118     C* ESEGUO OVRDBF SU FNMOV
135600991118     C*----------------------------------------------------*
135700991118     C     OVRMOV        BEGSR
135800991118     C*
135900020926     C                   MOVE      '046) '       VAR               8
136000021118     C                   MOVEL     vfil          VAR
136100020925     C                   Z-ADD     49            LUNG             15 5
136200991118     C** CONTROLLO SE ESISTE IL MEMBRO X FNMOV00T
136300060321     C                   SETOFF                                       98
136400991118     C                   MOVE      VAR           CMDD(1)
136500991118     C                   MOVEL     *BLANKS       COMMAN
136600030411     C                   MOVEl(p)  CMDD(1)       COMMAN           80
136700060321     C                   CALL      'QCMDEXC'                            98
136800991118     C                   PARM                    COMMAN
136900991118     C                   PARM                    LUNG
137000991118     C** ESEGUE ADDPFM DEL MEMBRO NUOVO
137100060321     C   98              DO
137200991118     C                   MOVE      VAR           CMDD(2)
137300991118     C                   MOVEL     *BLANKS       COMMAN
137400030411     C                   MOVEl(p)  CMDD(2)       COMMAN
137500991118     C                   CALL      'QCMDEXC'
137600991118     C                   PARM                    COMMAN
137700991118     C                   PARM                    LUNG
137800991118     C                   END
137900991118     C*+ ESEGUE IN OGNI CASO OVRDBF DEL MEMBRO DESIDERATO
138000991118     C                   MOVE      VAR           CMDD(3)
138100991118     C                   MOVEL     *BLANKS       COMMAN
138200030411     C                   MOVEl(p)  CMDD(3)       COMMAN
138300991118     C                   CALL      'QCMDEXC'
138400991118     C                   PARM                    COMMAN
138500991118     C                   PARM                    LUNG
138600991118     C* GUARDO SE è ALLOCATO
138700991118     C                   MOVEL     VAR           VAR
138800991118     C                   MOVE      ')'           VAR
138900991118     C                   MOVE      VAR           CMDD(4)
139000030411     C                   MOVEl(p)  CMDD(4)       COMMAN
139100020925     C                   Z-ADD     49            LUNCMD           15 5
139200991118     C                   CALL      'QCMDEXC'                            91
139300991118     C                   PARM                    COMMAN
139400991118     C                   PARM                    LUNCMD
139500991118     C*
139600021121     C  N91              OPEN      FNMOV00T                             91
139700030318     c                   if        *in91
139800030411     C                   MOVEL     'NO'          KPJBU
139900030318     C                   SETON                                        28
140000030318     C                   MOVEL     ERR(4)        $MSG
140100030411     c                   else
140200030318     c* controllo che il membro sia vuoto
140300030318     c                   exsr      tstobj
140400030318     c                   if        nrr <> 0
140500030318     C                   SETON                                        28
140600030318     C                   MOVEL     ERR(5)        $MSG
140700030411     c                   end
140800030318     c                   end
140900991118     C*
141000991118     C                   ENDSR
141100991118     C*----------------------------------------------------*
141200991118     C* ESEGUO Dlcobj  FNMOV
141300991118     C*----------------------------------------------------*
141400991118     C     DLCMOV        BEGSR
141500991118     C*
141600991118     C* GUARDO SE è ALLOCATO
141700991118     C*
141800021122     C  N91              CLOSE     FNMOV00T                             91
141900991118     C                   MOVE      VAR           CMDD(5)
142000030411     C                   MOVEl(p)  CMDD(5)       COMMAN
142100020925     C                   Z-ADD     49            LUNCMD           15 5
142200991118     C                   CALL      'QCMDEXC'                            91
142300991118     C                   PARM                    COMMAN
142400991118     C                   PARM                    LUNCMD
142500991118     C*
142600991118     C                   ENDSR
142700021122     C*-------------------------------------------------------
142800021122     ** Sottometto trasmissioni
142900021122     **-------------------------------------------------------
143000021122     C     SBMTRA        BEGSR
143100021122     C*
143200021122     C                   MOVEL     '046'         CMZDST                          DEST.:SED
143300021122     C                   MOVEL     'FNMOV00T'    CMZFLD                          DA: >>>
143400060529     C                   EVAL      CmzMbD = 'M' + %EDITC(vfil:'X') + '046'
143500021122     C                   MOVEL     O50PLA        CMZLBD
143600021122     C                   MOVEL     'FNMOV00R'    CMZFLA                          A: >>>
143700021122     C                   MOVEL     CmzMbD        CMZMBA
143800021122     C                   MOVEL     O50ALA        CMZLBA
143900021122     C                   MOVEL     'GAITRA'      CMZLBA
144000021122     C                   MOVE      O50ALA        WQUATR            4
144100021122     C     WQUATR        IFEQ      '101 '
144200021122     C                   MOVE      '201 '        CMZLBA
144300021122     C                   ENDIF
144400021122     C                   MOVE      '0000'        CMZORI                          ORA
144500021122     C                   MOVE      '0000000'     CMZDTI                          DATA
144600021122     C                   MOVE      'N'           CMZURG                          NOTTE
144700021122     C                   MOVE      'A'           CMZADD                          IN ADD
144800021122     C                   MOVE      'S'           CMZCLR                          CLEAR
144900021122     C                   MOVE      'S'           CMZFTR                          DA TRANS.
145000021122     C                   MOVE      *BLANK        CMZERR                          1= ERR.
145100021122     C                   MOVE      'Y029'        CMZAZN                          AZIONE
145200021122     C                   MOVEL     O50ASI        CMZSYF                          SIST.INF.
145300021122     C                   MOVEL     'GAITRA'      CMZSYF
145400021122     C                   MOVE      O50ASI        WQUATR
145500021122     C     WQUATR        IFEQ      '101 '
145600021122     C                   MOVE      '201 '        CMZSYF
145700021122     C                   ENDIF
145800021122     C                   MOVE      nrr           CMZNRR                          N.REC.
145900021122     C                   MOVE      *BLANKS       CMZPRR                          FILE PRE-
146000021122     C*
146100021122     C                   CLEAR                   W236
146200021122     C                   CALL      'TIBS53C'
146300021122     C                   PARM                    O50LCL
146400021122     C                   PARM                    CMZDST
146500021122     C                   PARM                    CMZFLD
146600021122     C                   PARM                    CMZMBD
146700021122     C                   PARM                    CMZLBD
146800021122     C                   PARM                    CMZFLA
146900021122     C                   PARM                    CMZMBA
147000021122     C                   PARM                    CMZLBA
147100021122     C                   PARM                    CMZORI
147200021122     C                   PARM                    CMZDTI
147300021122     C                   PARM                    CMZURG
147400021122     C                   PARM                    CMZADD
147500021122     C                   PARM                    CMZCLR
147600021122     C                   PARM                    CMZFTR
147700021122     C                   PARM                    CMZERR
147800021122     C                   PARM                    CMZAZN
147900021122     C                   PARM                    CMZSYF
148000021122     C                   PARM                    CMZNRR
148100021122     C                   PARM                    CMZPRR
148200021122     C                   PARM                    W236            236
148300021122     C*
148400021122     C                   ENDSR
148500021122      *-----------------------------------------------------***********
148600021122      * TEST NR RECORD FILE DA TRASMETTER                   *  TSTOBJ *
148700021122      *-----------------------------------------------------***********
148800021122     C     TSTOBJ        BEGSR
148900021122     C* Richiamo TIBS55R per conoscere librerie/S.Informativi
149000021122     C* da/a cui spedire
149100021122     C                   CLEAR                   TIBS55ds
149200021122     C                   MOVEL     'L'           I50TLA
149300060529     C                   MOVEL     vfil          I50PPO
149400021122     C                   MOVEL     '046'         I50APO
149500021122     C                   CALL      'TIBS55R'
149600021122     C                   PARM                    TIBS55ds
149700060529     C* MEMBRO = M + vfil + 046
149800021122     C* GUARDO SE IL MEMBRO è VUOTO - FNMOV00T
149900021122     C                   MOVEL     o50pla        LIBR
150000021122     C                   MOVEL(p)  'FNMOV00T'    FILE
150100060529     C                   EVAL      mbr = 'M' + %EDITC(vfil:'X') + '046'
150200021122     C                   Z-ADD     0             NRR
150300021122     C                   CALL      'TRUL51C'
150400021122     C                   PARM                    LIBR             10
150500021122     C                   PARM                    FILE             10
150600021122     C                   PARM                    MBR              10
150700021122     C                   PARM                    NRR              10 0
150800021122     C                   ENDsr
150900991102      *-----------------------------------------------------***********
151000991102      * CONTROLLO DISTINTE CHIUSE NON CONTABILIZZATE        *  CNTDIS *
151100991102      *-----------------------------------------------------***********
151200991102     C     CNTDIS        BEGSR
151300021211      * OVRPRTF
151400021211     C     WOPNST        IFNE      'S'
151500021211     C                   MOVEL     'S'           WOPNST            1
151600120725     C                   MOVEA     CMDP          CMDPX            60
151700120725     C                   Z-ADD     60            LUNCMD           15 5
151800021211     C                   CALL      'QCMDEXC'                            92
151900021211     C                   PARM                    CMDPX
152000021211     C                   PARM                    LUNCMD
152100021211     C                   OPEN      QSYSPRT
152200021211     C                   END
152300991102     C* loop lettura distinte chiuse senza mandato d'introito
152400060529     C                   EVAL      ArbIPP = vfil
152500020925     C                   CLEAR                   ArbNMI
152600021211     C                   CLEAR                   UNO
152700021211     C     K02Arb71      SETLL     FnArb71L
152800021211     C                   DO        *HIVAL
152900021211     C     K02Arb71      READE     FnArb71L
153000021211     C                   if        %EOF(fnarb71l)
153100021211     C                   LEAVE
153200021211     C                   END
153300021211     C* ESCLUDO LE DISTINTE 999999
153400021211     C                   if        ARBNDC = *ALL'9'
153500021211     C                   ITER
153600021211     C                   END
153700021211     C                   IF        UNO = 0
153800991214     C                   SETON                                        28
153900991102     C                   MOVEL     ERR(2)        $MSG
154000021211     C* Stampo testata
154100021211     C                   EXCEPT    TESARB
154200021211     C                   Z-ADD     1             UNO               1 0
154300021211     C                   END
154400021211     C* Stampo DETTAGLIO
154500030318     c                   if        arbddc <> 0
154600030318     C                   MOVE      arbddc        DtaISO
154700030318     C                   MOVE      DTAISO        DtaEUR
154800021211     C                   MOVE      DTAEUR        GMADDC            8 0
154900030318     c                   else
155000030318     c                   clear                   gmaddc
155100030318     c                   end
155200120531     C* Decodifica Utente in stampa
155300120531     c                   clear                   nomUTE
155400120531     c                   z-add     arbIFP        arbfiliale        3 0
155500120531     c     kuteute       chain     azute02l
155600120531     c                   if        %Found(azute02l)
155700120531     c                   movel     filUTE        nomUTE
155800120531     c                   end
155900120816     c                   if        arbndc <> exarbndc
156000021211     C                   EXCEPT    DETARB
156100120816     c                   move      arbndc        exarbndc
156200120816     c                   endif
156300021211     C                   ENDDO
156400021211     C*
156500021211     C                   IF        UNO = 1
156600021211     C                   EXCEPT    FINEST
156700021211     C                   END
156800021119     C* controllo che ci sia una sola quadratura giornaliera
156900021120     c                   clear                   annqig
157000060202     c     kqig          chain     yfqig01l
157100060202     c                   if        %found(yfqig01l)
157200021119     C                   SETON                                        28
157300021119     C                   MOVEL     ERR(3)        $MSG
157400021119     c                   end
157500060424     c                   z-add     0             page
157600991102     C                   ENDSR
157700991102     C*-----------------------------------------------------------*
157800030318     C*  msg a cli* in sede di errata quadratura
157900991029     C*-----------------------------------------------------------*
158000030318     C     srmsg         BEGSR
158100090424     c                   move      GMAUDt        anno              2 0
158200090424     c                   movel     GMAUDt        com08             6
158300090424     c                   move      anno          com08
158400100820     C                   eval      emogg  = 'FORZATA quadratura giornaliera -
158500030318     C                             incassi e versamenti'
158600100820     C                   eval      emmsg  = 'Punto operativo '
158700060529     c                             + %editc(vfil:'X')
158800030318     c                             + ' in data '
158900030320     c                             + %subst(com08: 1: 2) + '/'
159000030320     c                             + %subst(com08: 3: 2) + '/'
159100060427     c                             + %subst(com08: 5: 2)
159200060328     c*?Se passato p.o.
159300060327     C                   if        *in40
159400100820     C                   EVAL      emmsg  = %trimr(emmsg) +
159500060328     C                             '. Totale cassaforte <> da totale generale'
159600060327     C                   END
159700060327     C                   if        *in41
159800100820     C                   EVAL      emmsg = %trimr(emmsg) +
159900060328     C                             '. Errore contante.'
160000060327     C                   END
160100060327     C                   if        *in42
160200100820     C                   EVAL      emmsg  = %trimr(emmsg) +
160300060328     C                             '. Errore assegni.'
160400060327     C                   END
160500060327     C                   if        *in43
160600100820     C                   EVAL      emmsg  = %trimr(emmsg) +
160700060414     C                             '. Inc. d''attribuire aperti da oltre 1gg.'
160800060327     C                   END
160900030318     C* UTENTE  A CUI INVIARE IL MESSAGGIO
161000100820     C*mm                MOVEL(P)  'CLI*'        MSGUTI
161100030318     C* FILIALE A CUI INVIARE IL MESSAGGIO
161200100820     C*mm                Z-ADD     046           MSGFIL
161300030318     C*
161400100820     C*                  CALL      'TNSYM4C'
161500100820     C*mm                PARM                    MSGTES           55
161600100820     C*mm                PARM                    MSGDET          198
161700100820     C*mm                PARM                    MSGFIL            3 0
161800100820     C*mm                PARM                    MSGUTI           10
161900130701     c                   eval      emlto = 'GestioneClienti@brt.it'
162000130701     c*                  eval      emlto = 'ufficio.pagamenti@bartolini.it'
162100100820     c                   eval      emlcc = *blank
162200100820     C                   CALL      'TIS701C1'
162300100820     C                   PARM                    emlto           253
162400100820     C                   PARM                    emlcc           253
162500100820     C                   PARM                    emogg            44
162600100820     C                   PARM                    emmsg          5000
162700100820     C                   PARM                    emesito           1
162800030318     C                   ENDSR
162900030318     C*-----------------------------------------------------------*
163000030318     C*  OPERAZIONI INIZIALI
163100030318     C*-----------------------------------------------------------*
163200030318     C     *INZSR        BEGSR
163300991029     C*
163400000000     C     *ENTRY        PLIST
163500000000     C                   PARM                    KPJBA
163600060517     C*---------- RICERCA DITTA :
163700060517     C                   MOVEL     'SOC001'      TIPXSC
163800060517     C                   MOVEL     'SOC001'      TIPXSC
163900060517     C                   MOVEL     *BLANK        SOCXSC
164000060517     C                   EXSR      REPSOC
164100060517     C     RTNXSC        IFNE      '1'
164200060517     C                   MOVEL     XSOCDS        SOC001
164300060517     C                   MOVEL     xscrgs        RSUT             20
164400060517     c                   else
164500060517     c                   seton                                        lr
164600060517     c                   return
164700060517     c                   end
164800900516      *
164900000000     C                   Z-ADD     1             CODUT
165000900517     C                   CALL      'X§PARUT'
165100900517     C                   PARM                    UT§DSE
165200021118     C*
165300021118     c     *dtaara       define    §azute        azuteds
165400021118     c     *dtaara       define    §datiute      ddatiute
165500021118     C                   in(E)     *dtaara
165600021118     C                   IF        %Error  or  RSUT = *blanks
165700021118     C                   call      'TIBS34R'
165800021118     C                   parm                    Tibs34Ds
165900021118     C                   in        *dtaara
166000021118     c                   ENDIF
166100060327     c                   movel     'CG'          Ctg
166200060327     c                   movel     '201'         soc
166300060529     C                   EVAL      VFil = dutpou
166400060529     c* sede
166500060529     c                   if        dutlpo = 'S' or dutlpo = ' '
166600060529     c                   seton                                        50
166700060529     c                   exsr      srwin04
166800060529     c                   end
166900040921     c* Descrizione p.o.
167000040921     C     vfil          CHAIN     AZORG01L                           21
167100040921     C  N21              MOVEL     ORGDES        DFil
167200060203     C* REPERISCO TABELLA Y4O
167300060203     C                   EXSR      SRY4O
167400021125     c* apro FNMOV00T
167500021125     c                   exsr      ovrmov
167600021118     C*--- RICERCA CAPOCONTO cassa------------------------------------*
167700021115     C                   DO        50            X                 2 0
167800021115     C                   MOVE      TCU(X)        TCUDS
167900021115     C     F56           CABNE     'CG'          END1
168000021115     C     F4            IFEQ      '4'
168100021115     C                   Z-ADD     KCU(X)        KCAS              4 0
168200021115     C                   END
168300021115     C     END1          TAG
168400021115     C                   END
168500090410     C                   TIME                    WHHUDT           14 0
168600991102     C                   TIME                    TIME              6 0
168700090410     C                   MOVE      WHHUDT        G08DAT
168800090410     C                   MOVE      *ZEROS        G08INV
168900090410     C                   MOVEL     *BLANK        G08ERR
169000090410     C                   CALL      'XSRDA8'
169100090410     C                   PARM                    WLBDA8
169200090410     C                   Z-ADD     G08INV        AMGUDt            8 0
169300090410     C                   Z-ADD     G08DAT        GMAUDt            8 0
169400031003     c* aggiungo un mese alla data di oggi
169500031003     c                   move      amgudt        dtaiso
169600031003     c                   adddur    1:*m          dtaiso
169700031003     c                   move      dtaiso        dtalimite
169800060426     C                   Z-ADD     0             D52dfi
169900061221     C                   Z-ADD     3             D52GIO
170000061221     c                   if        amgudt > 20061224 and
170100061221     c                             amgudt < 20070101
170200061221     C                   Z-ADD     5             D52GIO
170300061221     c                   end
170400060426     C                   MOVE      amgudt        D52DIN
170500060426     C                   MOVE      '-'           D52SGN
170600060426     C                   CALL      'TRUL52R'
170700060426     C                   PARM                    TRUL52DS
170800060426     C                   MOVE      D52DFI        oggimeno3         8 0
170900060426     C                   Z-ADD     0             D52dfi
171000060426     C                   Z-ADD     1             D52GIO
171100060426     C                   MOVE      amgudt        D52DIN
171200060426     C                   MOVE      '-'           D52SGN
171300060426     C                   CALL      'TRUL52R'
171400060426     C                   PARM                    TRUL52DS
171500060426     C                   MOVE      D52DFI        oggimeno1         8 0
171600060203     C* Reperisco tabella limite abbuoni
171700060203     C                   Movel     'GEI'         Tbecod
171800060203     C                   Movel     *blanks       Tbeke1
171900060203     C                   Movel     'EUR'         Tbeke1
172000060203     C                   Movel     *blanks       Tbeke2
172100060203     C     Ktbe1         Chain     Tntbe01l
172200060203     c                   if        %found(tntbe01l)
172300060203     C                   Movel     tbeuni        dgei
172400060203     c                   else
172500060203     c                   clear                   dgei
172600060203     c                   end
172700070731     C* Reperisco tabella DGD x controllo partite aperte
172800070731     C                   Movel     'DGD'         Tbecod
172900070731     C                   Movel     *blanks       Tbeke1
173000070731     C                   Movel(p)  '1'           Tbeke1
173100070731     C                   Movel     *blanks       Tbeke2
173200070731     C     Ktbe1         Chain     Tntbe01l
173300070731     c                   if        %found(tntbe01l)
173400070731     C                   Movel     tbeuni        ddgd
173500070731     c                   else
173600070731     c                   clear                   ddgd
173700070731     c                   end
173800060327     c* serve per emettere la finestra del contante la 1° volta
173900060327     c                   eval      flgcon = *on
174000900515      *---------------------------------------------------------------*
174100900515      * CHIAVI                                                        *
174200900515      *---------------------------------------------------------------*
174300021119     C     Kqig          KLIST
174400021120     C                   KFLD                    annqig
174500021120     C                   KFLD                    vfil
174600021119     C                   KFLD                    amgudt
174700021120     C     Kqig1         KLIST
174800021120     C                   KFLD                    annqig
174900021120     C                   KFLD                    vfil
175000021118     C     KTBE1         KLIST
175100021118     C                   KFLD                    tbecod
175200021118     C                   KFLD                    tbeke1
175300021118     C                   KFLD                    tbeke2
175400021120     C     Kvrt          KLIST
175500021121     C                   KFLD                    vfil
175600021122     C                   KFLD                    qigdta
175700021121     C     K02Arb71      KLIST
175800021121     C                   KFLD                    ArbIPP
175900021121     C                   KFLD                    ArbNMI
176000060203     C     K04Ppa03      KLIST
176100060203     C                   KFLD                    soc               3
176200060203     c                   KFLD                    CTG               2
176300060203     C                   KFLD                    KCC
176400060203     C                   KFLD                    KSC
176500060203     C     K08Pas03      KLIST
176600060203     C                   KFLD                    soc
176700060203     c                   KFLD                    CTG
176800060203     C                   KFLD                    KCC
176900060203     C                   KFLD                    KSC
177000060203     C                   KFLD                    PpaDtPar
177100060203     C                   KFLD                    PpaNrPar
177200060203     C                   KFLD                    PpaSerPar
177300120531     C     kuteute       KLIST
177400120531     C                   KFLD                    arbfiliale        3 0
177500120531     C                   KFLD                    arbCUC
177600991102     C*
177700991102     C* Controllo se ci sono distinte chiuse ancora da contabilizzare
177800060529     C  n50              EXSR      CNTDIS
177900991102     C*
178000991102     C                   ENDSR
178100060517     C*----------------------------------------------------*
178200060517     C* Reperimento dati società
178300060517     C*----------------------------------------------------*
178400060517     C     REPSOC        BEGSR
178500060517     C*
178600060517     C                   CALLB     'XSOC'
178700060517     C                   PARM                    TIPXSC            6
178800060517     C                   PARM                    SOCXSC            3
178900060517     C                   PARM                    CDSXSC            9 0
179000060517     C                   PARM                    MODXSC            3
179100060517     C                   PARM      *blanks       RTNXSC            1
179200060517     C                   PARM                    XSOCDS
179300060517     C                   PARM                    KPJBA
179400060517     C*
179500060517     C                   ENDSR
179600021115     c**********************************************************************
179700021115     C     SRY4O         BEGSR
179800021115     C*  CARICO I CONTI DA GENERARE  X FILIALE
179900021115     C                   MOVEL     'Y4O'         tbecod
180000060203     C                   MOVEl(p)  soc           tbeke1
180100021118     C                   MOVEL(p)  vfil          tbeke2
180200021118     C     ktbe1         CHAIN     Tntbe01l
180300021118     c                   if        %found(tntbe01l)
180400021118     C                   MOVEL     TBeUNI        Dy4O
180500021118     c                   else
180600021118     c                   clear                   dy4o
180700021118     c                   end
180800021115     C                   ENDSR
180900060202     C*-------------------------------------------------------
181000060202     ** GESTIONE CONTANTI
181100060202     **-------------------------------------------------------
181200060202     C     srtagli       BEGSR
181300060322     c                   clear                   cnc0wa00s
181400060322     c                   eval      £DIVISA ='EUR'
181500060322     c                   eval      kpjbu = cnc0wa00s
181600060322     c                   call      'CNC0WA00R'
181700060322     c                   parm                    kpjba
181800060322     c                   eval      cnc0wa00s = kpjbu
181900060322     c                   if        £RTNCMD = F6 and
182000060328     c                             £RTNCODE ='DONE'
182100060322     c                   eval      casconv = £IMPIMMESSO
182200060322     c                   end
182300060322     C                   ENDSR
182400060202      *
182500021211     OQSYSPRT   E            TESARB           02
182600021211     O                       RSUT                20
182700030318     O                                         + 10 'QUADRATURA GIORNALIERA INC'
182800030318     O                                         +  0 'ASSI E VERSAMENTI'
182900090410     O                       GMAUDt            + 13 '  /  /    '
183000021211     O                       TIME              +  1 '  :  :  '
183100021211     O                                         + 10 'CNC0W1R'
183200021211     O                       PAGE          Z    132
183300021211     O                                          128 'PAG.'
183400021211     O          E            TESARB         1 04
183500021211     O                                         + 30 'ELENCO DISTINTE AUTOTRAS'
183600021211     O                                         +  0 'PORTATORE CHIUSE E NON C'
183700021211     O                                         +  0 'ONTABILIZZATE'
183800021211     O          E            DETARB         1
183900021211     O                                         +  1 'OPERATORE'
184000021211     O                       ARBCUC        Z   +  1
184100120531     O                       NOMUTE            +  1
184200021211     O                                         +  1 'AUTOTRASP.'
184300021211     O                       ARBPDC        Z   +  1
184400021211     O                                         +  1 'NR.DISTINTA'
184500021211     O                       ARBNDC        Z   +  1
184600021211     O                                         +  1 'DEL'
184700021211     O                       GMADDC        8   +  1
184800021211     O          E            FINEST      2
184900021211     O                                         + 40 '** - F I N E    S T A M'
185000021211     O                                         +  1 'P A - **'
185100900606      *----------------------------------------------------------------
185200021118** ERR
185300060411La cass.di servizio ha un importo > di quello ammesso. Fare mandatino
185400021115ATT.NE : Distinte chiuse non contabilizzate
185500021119ATT.NE : Quadradura giornaliera già confermata
185600030318Non è possibile eseguire la quadratura. Attendere qualche minuto
185700031023ATT.NE : la precedente chiusura è terminata in modo anomalo
185800030318Ora si può premere F6 per confermare
185900060508ATT.NE : l'importo della DIFFERENZA non può essere negativo
186000060324ATT.NE : Ci sono assegni che non sono stati resi versabili
186100060327** CMDD
186200020925CHKOBJ OBJ(FNMOV00T) OBJTYPE(*FILE) MBR(M000000)
186300020925ADDPFM FILE(FNMOV00T)               MBR(M000000)
186400020925OVRDBF FILE(FNMOV00T)               MBR(M000000)
186500020925ALCOBJ OBJ((FNMOV00T *FILE *EXCL        M000000))
186600020925DLCOBJ OBJ((FNMOV00T *FILE *EXCL        M000000))
186700021211**  CMDP
186800120725OVRPRTF FILE(QSYSPRT) USRDTA(CARICO_MAN) SAVE(*YES)

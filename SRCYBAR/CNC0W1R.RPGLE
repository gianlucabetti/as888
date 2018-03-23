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
007500161122     D OG143         E DS
007600900514     D KPJBA         E DS
007700060426     D Trul52ds      E DS
007800021121     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
007900021121     D  TCU                  398    697
008000021121     D                                     DIM(50)                              TAB FLAG C
008100021121     D  KCU                  698    847P 0
008200021121     D                                     DIM(50)                              TAB CAPO C
008300021121     D                                     PACKEVEN
008400060406     D TRul06ds      E DS
008500060406     D  LIN                    1     90  0 dim(30)
008600140122      ****
008700140122     D  L72            s              3s 0 DESCEND
008800140122     d                                     DIM(60)
008900140122      ****
009000060411     D dsls01        E DS                  inz
009100060517     D soc001        E DS                  EXTNAME(xsoc001ds)
009200060517     D xsocds          DS          1000
009300021115     D TCUDS           DS
009400021115     D  F4                     4      4
009500021121     D  F56                    5      6
009600090410     D WLBDA8          DS
009700090410     D  G08DAT                 1      8  0
009800090410     D  G08INV                 9     16  0
009900090410     D  G08ERR                17     17
010000090410     D  G08TGI                18     22  0
010100060406     D PARAM           DS
010200060406     D  LIP                    1     87
010300060406     D                                     DIM(29)                              LINEE ARRI
010400060406     D  ZNC                   88    107
010500060406     D                                     DIM(10)                              ZONE VIDEO
010600060406     D  PARDAT               108    115  0
010700060406     D  PARABI               116    117
010800060406     D  PARLNP               118    120  0
010900060406     D  PARNFV               121    125
011000060406     D  PARGES               126    128  0
011100060406     D  PAROUT               129    129
011200060406     D  PARINC               130    131
011300060406     D  PARIMA               132    133
011400060406     D* DATA F. ARRIVI DI ABBINAMENTO IN AAAAMMGG
011500060406     D  PARDFA               134    141  0
011600060406     D* NUMERO FIL GESTIONE FOGLIO IMA
011700060406     D  PARFIA               142    146  0
011800060406     D  PARFGS               147    149  0
011900060406     D* SI/NO/SOLO POSTE   S/N/P
012000060406     D  PARSNP               150    150
012100060406     D* TUTTE, SOLO SI, SOLO NO ASSEGNATO
012200060406     D  PARASS               151    152
012300060406     D* TUTTE, SOLO SI, SOLO NO RESI
012400060406     D  PARRES               153    154
012500060406     D* Tutte, solo SI, solo NO - Bolle Bloccate
012600060406     D  PARblo               155    156
012700060406     d* Funzione richiesta: *blank = stampa, "WRKF " = creaz. file di lavoro
012800060406     d  PARfr                252    256
012900060411     D PARNAC          DS
013000060411     D  PFLE                   1      3
013100060411     D  PCUC                   4      6
013200060411     D  PNMI                   7     13
013300060411     D  PDMI                  14     19
013400941124     D                SDS
013500941124     D  §PGM                   1     10
013600120531     D NomUTE          S                   like(uteUTE)
013700021118     D wcomo6          S              6  0                                      DS3A TIPO
013800021120     D annqig          S                   like(qigann)                         DS3A TIPO
013900021125     D savdta          S                   like(amgudt)                         DS3A TIPO
014000031003     D dtalimite       S                   like(amgudt)                         DS3A TIPO
014100060321     D esito           S             10I 0
014200021121     D dtaiso          S               d                                        DS3A TIPO
014300021121     D dtaeur          S               d   datfmt(*eur)                         DS3A TIPO
014400030318     D f8              S              1    inz('0')                             DS3A TIPO
014500060321     D flgprima        S              1    inz('0')                             DS3A TIPO
014600060202     D WrkLstRrnS4     S              5I 0
014700060202     D tmi             S                   like(mditmi)                         DS3A TIPO
014800060321     D chkCliente      PR            10I 0
014900060321     D  prmSoc                             LIKE(diz$soc)
015000060321     D                                     OPTIONS(*OMIT)
015100060321     D  prmKcc                             LIKE(diz$Kcc)
015200060321     D                                     OPTIONS(*OMIT)
015300060321     D  prmCsc                             LIKE(diz$csc)
015400060321     D                                     OPTIONS(*OMIT)
015500060321     D  prmKsc                             LIKE(pjzKsc)
015600060321     D                                     OPTIONS(*OMIT)
015700060321     D  prmRicerca                    1A   const
015800060321     D  prmRag                             LIKE(dizAcoRag)
015900060321     D                                     OPTIONS(*OMIT)
016000060321     D  prmFlr                       90A
016100060321     D                                     OPTIONS(*OMIT)
016200060321     D  prmKpjba                           LIKE(kpjba)
016300060321     D                                     OPTIONS(*OMIT)
016400060321     D  prmEsito                     10I 0
016500060321     D  prmErr                        1A   DIM(10)
016600060321     D  prmMsg                       80A   DIM(10)
016700060321     D                                     OPTIONS(*OMIT)
016800060321     D  prmCnaco00f                        LIKE(cnacods)
016900060321     D                                     OPTIONS(*OMIT)
017000060321     D  prmCnind00f                        LIKE(cnindds)
017100060321     D                                     OPTIONS(*OMIT)
017200060321     D  prmCnclp00f                        LIKE(cnclpds)
017300060321     D                                     OPTIONS(*OMIT)
017400060321     D  prmFncls00f                        LIKE(fnclsds)
017500060321     D                                     OPTIONS(*OMIT)
017600060322     D F6              C                   X'36'
017700060327     Dflgcon           s              1
017800060421     Di                s              2  0
017900060411     Dcomconinc        s                   like(disconv)
018000060411     Dcomconver        s                   like(verconv)
018100060411     Dcomabb           s                   like(§GETQG)
018200060411     D§abb             s                   like(§GETQG)
018300060605     DSAVCUc           S                   like(casCUC)
018400060605     DSAVda8           S                   like(casda8)
018500060605     DSAVora           S                   like(casora)
018600120816     D exarbndc        S                   like(arbndc)                         DS3A TIPO
018700150417     d                                     INZ(*HIVAL)
018800991029?     *--------------------------------------------------------------*
018900991029      * CICLO PRINCIPALE
019000991029?     *--------------------------------------------------------------*
019100021115     c                   exsr      inz01
019200021122     c* se non ci sono errori e non è la 1° volta carico i dati a video
019300021122     c                   if        not *in28 and *in31
019400021122     c                   exsr      carica
019500021122     c                   end
019600021115     c                   do        *hival
019700060327     c                   if        *in90
019800060327     c                   setoff                                       90
019900060327     C                   write     CN0W101
020000060327     c                   seton                                        90
020100060327     c                   end
020200060327     C                   EXFMT     CN0W101
020300021125     C   kc              leave
020400060329     c* distinte non contabilizzate
020500021115     c                   if        *in28
020600021115     c                   iter
020700021115     c                   end
020800021120     c* stampa dettaglio distinte e versamenti
020900021120     c                   if        *inkj
021000021120     c                   seton                                        99
021100021120     c                   exsr      stadis
021200021120     c                   exsr      staver
021300030411     c                   close     cnc0w1p
021400030411     c                   open      cnc0w1p
021500021120     c                   end
021600060202     c* gestione sfl tagli contante
021700060327     c                   if        flgcon = *on
021800060327     c                   eval      *inki = *on
021900060327     c                   eval      flgcon= *off
022000060327     c                   end
022100060321     c                   if        *inki
022200060202     c                   exsr      srtagli
022300060202     c                   end
022400021122     c* se 1° volta controlla data cassaf. gg precedente
022500021122     c                   if        not *in31
022600021115     c                   exsr      ctr01
022700021120     c   30              iter
022800021125     c                   move      dtagpv        dtaeur
022900021125     c                   move      dtaeur        dtaiso
023000021122     c                   move      dtaiso        qigdta
023100021122     c* carica
023200021122     c                   exsr      carica
023300021122     c                   end
023400021122     c* controlli e calcoli
023500021122     c                   exsr      ctr02
023600031023     c                   if        *in28 or *in99
023700031023     c                   iter
023800031023     c                   end
023900021120     c* errore forzabile
024000030318     c                   if        *in90
024100030318     c                   if        not *inkh
024200030318     c* non è stato premuto ancora f8 di forzatura
024300030318     c                   if        f8 = *off
024400030318     c                   iter
024500030318     c                   end
024600030318     c* se esiste errore di quadratura mi deve prima forzare e poi convalida
024700030318     c                   else
024800060327     c                   setoff                                       414243
024900060414     c                   setoff                                       4045
025000030318     c                   movel     *on           f8
025100030318     C                   MOVEL     ERR(6)        $MSG
025200030318     c                   iter
025300030318     c                   end
025400030318     c                   else
025500030318     c                   movel     *off          f8
025600030318     c                   end
025700060327     c* richiedo il commento obbligatorio se premuto f8
025800060327     c                   if        f8 = *on
025900060327     c                   exsr      srcomme
026000060327     c                   end
026100021115     c* aggiorna
026200030318     c                   if        *inkf
026300021120     c* chiedo la data per il giroconto della cassaforte al gg successivo
026400060202     c                   if        totcasv <> 0
026500021120     c                   exsr      srdta
026600021120     c   kl              iter
026700021120     c                   end
026800030423     c* invio msg a CLI* in sede che c'è stata un'errata quadratura
026900030423     c                   if        f8 = *on
027000030423     c                   exsr      srmsg
027100030423     c                   end
027200030423     c* aggiorna
027300021115     c                   exsr      aggio
027400030318     c                   commit
027500021115     c                   leave
027600021115     c                   end
027700991102     C                   ENDDO
027800030403     c* invia se non c'è errore
027900030320     c                   if        not *in28
028000021122      *  CONTROLLO SE HO RECORD DA TRASMETTERE
028100021122     C                   EXSR      TSTOBJ
028200021122     c* sottomette trasmissione a sede del fnmov00t
028300021122     c                   if        nrr <> 0
028400021122     C                   EXSR      SBMTRA                                       *
028500021122     c                   end
028600030320     c                   end
028700030403     C                   EXSR      DLCMOV
028800060529     c* sottomette le stampe di controllo
028900060411     c                   if        *inkf
029000060529     c                   exsr      srsbm
029100060411     c                   end
029200060404     C*
029300910521     C                   SETON                                        LR
029400021115     C**-------------------------------------------------------------
029500060529     C** sottomette stampe
029600021115     C**-------------------------------------------------------------
029700060529     C     srsbm         BEGSR
029800140127      ****
029900060529     c* reperisco i po esteri gestiti dal po contabile
030000140122      ****
030100060529     c                   if        vfil = orgfc0
030200140122      ****
030300140122      ****  Sostituito il TRUL72R con il TRUL06R chiamato in modalità "£J"
030400140122      ****
030500140122     c                   clear                   L72
030600140122     c                   clear                   trul06ds
030700140122     C                   MOVEL     '£J'          D06COD
030800140122     C                   MOVEL     orgfc0        D06KEY
030900140122     C                   MOVEL     TRul06ds      KPJBU
031000140122     C                   CALL      'TRUL06R'
031100140122     C                   PARM                    KPJBA
031200140122     C                   MOVEL     KPJBU         TRul06ds
031300140122      ****
031400140122      ***  carico la schiera che utilizzava precedentemente con il TRUL72R
031500140122      **  con la schiera ricavata dal TRUL06R £J
031600140122      ***  in questo modo non viene modificato nulla sulle righe successive
031700140122     c                   do        30            cc
031800140122     c                   eval      l72(cc) = lin(cc)
031900140122     c                   end
032000161017      ****
032100161017     c                   else
032200161017      ****
032300161017      ****  se secondo Livello deve mettere se stesso e basta
032400161017     c                   eval      l72(1)=vfil
032500060529     c                   end
032600140122     c*
032700060529     c                   do        60            cc                3 0
032800161017     c*
032900161017     c*   deve uscire al primo vuoto
033000060529     c                   if        l72(cc) = 0 or cc = 60
033100161017     c                   leave
033200060529     c                   end
033300161122      ****
033400161122      ***  -------------------------------------------------------
033500161122      **   per le filiale ESTERE le stampe devono essere CONGELATE
033600161122      ***  -------------------------------------------------------
033700161122      **
033800161122     c                   clear                   estero            1
033900161122     c     l72(cc)       chain     azorg01l
034000161122     c                   if        %found(azorg01l)
034100161122     c                   eval      og143 = orgde3
034200161122     c                   if        §ogNTW = 'DPD'  or
034300161122     c                             §ogNTW = 'EEX'  or
034400161122     c                             §ogNTW = 'FED'
034500161122     c                   move      'H'           estero
034600161122     c                   end
034700161122     c                   end
034800161122      **   congelo se estero
034900161122     c                   movel     estero        kJost
035000140122      ****
035100060529     C*§ SOTTOMETTO STAMPA VARIAZ. A C/ASSEGNI E P.ASSEGNATI
035200060529     c                   movel(p)  l72(cc)       kpjbu
035300060529     C                   MOVEL     'LR92'        KCOAZ
035400060529     C                   MOVEL     'KSSPECL'     KQEBI
035500060529     C                   CALL      'BCH10'
035600060529     C                   PARM                    KPJBA
035700140122      ****
035800060529     C*§ SOTTOMETTO STAMPA VARIAZ. A pre-pagati
035900060529     c                   movel(p)  l72(cc)       kpjbu
036000151019     c*                  eval      KJOST = 'H'
036100060529     C                   MOVEL     'LR96'        KCOAZ
036200060529     C                   MOVEL     'KSSPECL'     KQEBI
036300060529     C                   CALL      'BCH10'
036400060529     C                   PARM                    KPJBA
036500060529     C*§ SOTTOMETTO STAMPA Assegnati/c.assegni ancora da incassare
036600060529      * ricarico la £6 se veniva cambiato il P.O aveva i dati errati
036700060529     c                   clear                   trul06ds
036800060529     C                   MOVEL     '£6'          D06COD
036900060529     C                   MOVEL     l72(cc)       D06KEY
037000060529     C                   MOVEL     TRul06ds      KPJBU
037100060529     C                   CALL      'TRUL06R'
037200060529     C                   PARM                    KPJBA
037300060529     C                   MOVEL     KPJBU         TRul06ds
037400060529     c*
037500060529     c                   clear                   param
037600060529     c                   for       i = 1 to 29
037700060529     c                   eval      lip(i)=%editc(lin(i): 'X')
037800060529     c                   endfor
037900060529     c                   eval      parnfv = '00000'
038000060529     c                   eval      pardat = 99991231
038100060529     c                   eval      parinc = 'SI'
038200060529     c                   eval      pardfa = AMGUDT
038300060529     c                   eval      parsnp = 'N'
038400060529     c                   eval      parres = 'NO'
038500060529     c                   eval      kpjbu = param
038600060529     C                   MOVEL     'LR52'        KCOAZ
038700060529     C                   MOVEL     'KSSPECL'     KQEBI
038800151019     c*                  eval      KJOST = 'H'
038900060529     C                   CALL      'BCH10'
039000060529     C                   PARM                    KPJBA
039100060529     C*§ SOTTOMETTO STAMPA NOTE ACCREDITO OPERATORE 999 DEL GG PRECED.
039200060529     C                   Z-ADD     0             D52DFI                         *
039300060529     C                   Z-ADD     1             D52GIO                         *
039400060529     C                   MOVE      AMGUDT        D52DIN                         *
039500060529     C                   MOVE      '-'           D52SGN                         *
039600060529     C                   CALL      'TRUL52R'
039700060529     C                   PARM                    TRUL52ds
039800060529     C                   MOVE      D52DFI        DAT1GG            8 0          *
039900060529     C                   MOVE      l72(cc)       PFLE
040000060529     C                   MOVE      *ALL'9'       PCUC
040100060529     C                   MOVE      *ALL'9'       PNMI
040200060529     C                   MOVE      DAT1GG        PDMI
040300060529     C                   MOVEL     PARNAC        KPJBU
040400060529     C                   MOVEL     'LR93'        KCOAZ
040500060529     C                   MOVEL     'KSSPECL'     KQEBI
040600120628     c                   eval      KJOST = 'H'
040700060529     C                   CALL      'BCH10'
040800060529     C                   PARM                    KPJBA
040900060529     C*§ SOTTOMETTO STAMPA ELENCO SPEDIZIONI RIPRISTINATE DOPO CONTEGGI
041000060529     c                   movel(p)  l72(cc)       kpjbu
041100060529     C                   MOVEL     'LR94'        KCOAZ
041200060529     C                   MOVEL     'KSSPECL'     KQEBI
041300120628     c                   eval      KJOST = 'H'
041400060529     C                   CALL      'BCH10'
041500060529     C                   PARM                    KPJBA
041600060529     C*§ SOTTOMETTO STAMPA prepagati non incassati e non confermati
041700151019     c* n.b il parametro di congelamento spool rimane perchè nel pgm
041800151019     c* viene generata la stampa dei NON ICASSATI che deve essere congelata
041900151019     c* Nell'azione ci sarà quindi solo il relativo prtf
042000060529     c                   clear                   dsls01
042100060529     C                   eval      ds1fgs = l72(cc)
042200060529     C                   eval      ds1lnp = l72(cc)
042300060529     C                   MOVEL(p)  dsls01        KPJBU
042400060529     c                   move      'S'           kpjbu
042500060529     C                   MOVEL     'LSE8'        KCOAZ
042600060529     C                   MOVEL     'KSSPECL'     KQEBI
042700120628     c                   eval      KJOST = 'H'
042800060529     C                   CALL      'BCH10'
042900060529     C                   PARM                    KPJBA
043000160915      *
043100060529     c                   enddo
043200160915      *
043300060529     C                   ENDSR
043400060529     C**-------------------------------------------------------------
043500060529     C** inizializza dspf
043600060529     C**-------------------------------------------------------------
043700060529     C     inz01         BEGSR
043800021118     C                   MOVE      DUTCOU        VCDUS
043900060201     c                   z-add     0             DISPOSV
044000060201     c                   z-add     0             DISCONV
044100060201     c                   z-add     0             DISASSV
044200060201     c                   z-add     0             PREPOSV
044300060201     c                   z-add     0             PRECONV
044400060201     c                   z-add     0             PREASSV
044500060201     c                   z-add     0             FCCPOSV
044600060201     c                   z-add     0             FCCCONV
044700060201     c                   z-add     0             FCCASSV
044800060201     c                   z-add     0             CPOPOSV
044900060201     c                   z-add     0             CPOCONV
045000060201     c                   z-add     0             CPOASSV
045100060202     c                   z-add     0             raiPOSV
045200060202     c                   z-add     0             raiCONV
045300060202     c                   z-add     0             raiASSV
045400021115     c                   z-add     0             totincv
045500060201     c                   z-add     0             VERCONV
045600060201     c                   z-add     0             VERASSV
045700060201     c                   z-add     0             TOTVERV
045800021115     c                   z-add     0             casfv
045900060201     c                   z-add     0             CASCONV
046000060201     c                   z-add     0             CASASSV
046100060201     c                   z-add     0             CASTOTV
046200021115     c                   z-add     0             totcasv
046300060201     c                   z-add     0             CGPCONV
046400060201     c                   z-add     0             CGPassV
046500060201     c                   z-add     0             CGPtotV
046600060201     c                   z-add     0             RETCONV
046700060201     c                   z-add     0             RETASSV
046800060201     c                   z-add     0             RETTOTV
046900021115     c                   z-add     0             totgenv
047000021122     C* se non trovo il record vuol dire che è la prima volta e quindi
047100021122     c* rendo modificabile la data e l'importo della cassaforte del gg
047200021122     c* precedente
047300021122     c                   clear                   annqig
047400060201     c     kqig          setll     yfqig01l
047500060201     c     kqig1         readpe    yfqig01l
047600060201     c                   if        not %eof(yfqig01l)
047700021122     c                   seton                                        31
047800060201     c                   z-add     QIGICON       CGPCONV
047900060201     c                   z-add     QIGIASS       CGPassV
048000060202     c                   eval      cgptotv = qigicon+qigiass
048100021122     c                   move      qigdta        dtaiso
048200021125     c                   move      dtaiso        dtaeur
048300021125     c                   move      dtaeur        dtagpv
048400021122     c                   else
048500060201     c                   z-add     0             CGPCONV
048600060201     c                   z-add     0             CGPassV
048700060201     c                   z-add     0             CGPtotV
048800021122     c                   z-add     0             dtagpv
048900021122     c                   setoff                                       31
049000021122     c                   end
049100021115     c                   endsr
049200021115     C**-------------------------------------------------------------
049300021115     C** carica i dati a video
049400021115     C**-------------------------------------------------------------
049500021115     C     carica        BEGSR
049600060321     c* leggo tutti i record di yfmdi del giorno per po
049700060201     c                   z-add     0             DISPOSV
049800060201     c                   z-add     0             DISCONV
049900060201     c                   z-add     0             DISASSV
050000060201     c                   z-add     0             PREPOSV
050100060201     c                   z-add     0             PRECONV
050200060201     c                   z-add     0             PREASSV
050300060201     c                   z-add     0             FCCPOSV
050400060201     c                   z-add     0             FCCCONV
050500060201     c                   z-add     0             FCCASSV
050600060201     c                   z-add     0             CPOPOSV
050700060201     c                   z-add     0             CPOCONV
050800060201     c                   z-add     0             CPOASSV
050900060202     c                   z-add     0             raiPOSV
051000060202     c                   z-add     0             raiCONV
051100060202     c                   z-add     0             raiASSV
051200080513     c                   z-add     0             verconv
051300080513     c                   z-add     0             verassv
051400060321     c                   z-add     0             casfv
051500021125     c                   z-add     0             totincv
051600060406     c                   z-add     0             RETCONV
051700060406     c                   z-add     0             RETASSV
051800060406     c                   z-add     0             RETTOTV
051900060321     c* mandati d'introito
052000060321     c     qigdta        setgt     yfmdi01l
052100060321     c                   do        *hival
052200060321     c                   read      yfmdi01l
052300060321     c                   if        %eof(yfmdi01l)
052400060321     c                   leave
052500060321     c                   end
052600060529     c                   if        mdifle <> vfil
052700060321     c                   iter
052800060321     c                   end
052900060201     c* chiusura operatore
053000060201     c                   select
053100060201     c                   when      mditmi = 'O'
053200060321     c                   add       MDIICON       DISconV
053300060321     c                   add       MDIIASS       DISassV
053400060321     c                   add       MDIIPOS       DISposV
053500060405     c* pre-pagati
053600060201     c                   when      mditmi = 'P'
053700060321     c                   add       MDIICON       PREconV
053800060321     c                   add       MDIIASS       PREassV
053900060321     c                   add       MDIIPOS       PREposV
054000060405     c* fatture codificati
054100060201     c                   when      mditmi = 'F'
054200060321     c                   add       MDIICON       FCCconV
054300060321     c                   add       MDIIASS       FCCassV
054400060321     c                   add       MDIIPOS       FCCposV
054500060405     c* omessi parziali
054600060223     c                   when      mditmi = 'Z'
054700060321     c                   add       MDIICON       CPOconV
054800060321     c                   add       MDIIASS       CPOassV
054900060321     c                   add       MDIIPOS       CPOposV
055000060405     c* recupero assegni insoluti
055100060420     c                   when      mditmi = 'G'
055200060321     c                   add       MDIICON       RAIconV
055300060321     c                   add       MDIIASS       RAIassV
055400060321     c                   add       MDIIPOS       RAIposV
055500060202     c                   endsl
055600021115     c                   enddo
055700060214     c                   eval      totincv = disconv+disassv+preconv+preassv
055800060214     c                             +fccconv+fccassv+cpoconv+cpoassv+raiconv+
055900060214     c                             raiassv
056000021120     c* leggo tutti i record di FNVRT del giorno per po
056100021125     c                   z-add     0             totverv
056200021122     c     kvrt          setgt     fnvrt01l
056300021120     c                   do        *hival
056400021122     c     vfil          reade     fnvrt01l
056500021120     c                   if        %eof(fnvrt01l)
056600021120     c                   leave
056700021120     c                   end
056800060202     c                   add       vrtcon        verconv
056900060202     c                   add       vrtass        verassv
057000021120     c                   enddo
057100060202     c                   eval      totverv=verconv+verassv
057200060406     c* cassaforte feritoia
057300060605     c     vfil          setll     yfcas01l
057400060406     c                   do        *hival
057500060406     c     vfil          reade     yfcas01l
057600060406     c                   if        %eof(yfcas01l)
057700060406     c                   leave
057800060406     c                   end
057900060502     c                   if        casvie = 'S' or casqta < 0
058000060407     c                   iter
058100060407     c                   end
058200060406     c                   add       casICON       casfv
058300060406     c                   enddo
058400060406     c* rettifiche agli incassi
058500060406     c     kvrt          setgt     yfqri01l
058600060406     c                   do        *hival
058700060406     c     vfil          reade     yfqri01l
058800060406     c                   if        %eof(yfqri01l)
058900060406     c                   leave
059000060406     c                   end
059100060406     c                   add       qriICON       retconv
059200060406     c                   add       qriIass       retassv
059300060406     c                   enddo
059400060406     c                   eval      rettotv = retconv + retassv
059500060202     c* importo assegni cassaforte di servizio
059600060202     c                   clear                   okass             1 0
059700060202     c                   clear                   casassv
059800060202     c     vfil          setll     cnass01l
059900060202     c                   do        *hival
060000060202     c     vfil          reade     cnass01l
060100060202     c                   if        %eof(cnass01l)
060200060202     c                   leave
060300060202     c                   end
060400060202     c* segnalo che non sono stati presi in carico tutti i mandati perchè
060500060202     c* ci sono assegni ancora non resi versabili
060600060202     c                   if        assfve <> 'V'
060700060202     c                   eval      okass = 1
060800060202     c                   else
060900131129     c                   if        asstpi <> 'M'
061000060202     c                   add       asscas        casassv
061100060202     c                   end
061200131129     c                   end
061300060202     c                   enddo
061400021115     c                   endsr
061500021115     C**-------------------------------------------------------------
061600021122     C** controlli
061700021115     C**-------------------------------------------------------------
061800021115     C     ctr01         BEGSR
061900021120     c* se è la prima volta controllo che ci sia la data della cassaforte
062000021120     c* del gg precedente
062100021120     c                   if        dtagpv = 0
062200021120     C                   SETON                                        30
062300021120     c                   else
062400021120     c     *eur          test(d)                 dtagpv                 30
062500021120     c                   if        *in30
062600021120     c     *dmy          test(d)                 dtagpv                 30
062700021120     c                   if        not *in30
062800021120     c     *dmy          move      dtagpv        dtaeur
062900021120     c                   move      dtaeur        dtagpv
063000021120     c                   end
063100021120     c                   end
063200021120     c                   end
063300021115     c                   endsr
063400021122     C**-------------------------------------------------------------
063500021122     C** controlli e calcoli
063600021122     C**-------------------------------------------------------------
063700021122     C     ctr02         BEGSR
063800031023     c                   setoff                                       902899
063900060327     c                   setoff                                       404142
064000060414     c                   setoff                                       4345
064100021122     c                   clear                   $msg
064200060329     c                   do
064300021211     c* ritorno a controllare che non ci siano distinte senza chiusura op.
064400060419     C                   exsr      cntdis
064500021211     c                   if        not *in28
064600021122     c* totale incassato
064700060214     c                   eval      totincv = disconv+disassv+preconv+preassv
064800060214     c                             +fccconv+fccassv+cpoconv+cpoassv+raiconv+
064900060214     c                             raiassv
065000021122     c* totale cassaforte
065100021122     c                   z-add     casfv         totcasv
065200060202     c     casassv       add       casconv       castotv
065300060202     c                   add       castotv       totcasv
065400060329     c* cassaforte di servizio contante non deve essere > 1500 eur
065500060411     c                   if        casconv > §GETCS
065600031023     C                   SETON                                        99
065700060329     C                   MOVEL     ERR(1)        $MSG
065800060329     c                   iter
065900031023     c                   end
066000060329     c* se tutti gli assegni non sono versabili
066100060406     c                   if        okass=1
066200060406     C                   SETON                                        99
066300060406     C                   MOVEL     ERR(8)        $MSG
066400060329     c                   iter
066500060329     c                   end
066600060414     c* controllo se ci sono partite aperte sugli incassi d'attribuire
066700070731     c                   if        amgudt < §dgdtada or
066800070731     c                             amgudt > §dgdtaa
066900060502     c                   exsr      incatt
067000070731     c                   end
067100060414     c   99              iter
067200060329     c* totale generale
067300060329     c     cgpassv       add       cgpconv       cgptotv
067400060329     c                   z-add     cgptotv       totgenv
067500060329     c                   add       totincv       totgenv
067600060329     c                   sub       totverv       totgenv
067700060508     c                   sub       totcasv       totgenv
067800060508     c                   add       rettotv       totgenv
067900060511     C* LO RENDO POSITIVO
068000060511     C     totgenv       IFLT      *ZEROS
068100060511     C     totgenv       MULT      -1            §ABB
068200060511     C                   ELSE
068300060511     C                   Z-ADD     totgenv       §ABB
068400060511     C                   END
068500060511     C* CONTROLLO IL LIMITE SUPERIORE DELLA differenza
068600060511     c                   if        totgenv < 0 and §abb > §getqgt
068700060511     C                   SETON                                        99
068800060511     C                   MOVEL     ERR(7)        $MSG
068900060511     c                   iter
069000060511     c                   end
069100060511     c                   if        totgenv <> 0
069200060329     C                   SETON                                        9040
069300060329     c                   end
069400060518     c* per la partenza non controlla congruenza contante e assegni perchè
069500060518     c* la chiusura del gg prima viene messa tutta nel contante
069600060518     c                   if        AMGUDT <> 20060524
069700060327     c* congruenza contante
069800060411     c                   eval      comconinc =
069900060411     c                             ((disconv+preconv+fccconv+cpoconv+raiconv
070000060526     c                             +cgpconv)-(verconv) + retconv)
070100060411     c                   eval      comconver = (casfv+ casconv)
070200060411     C     comconinc     SUB       comconver     comABB
070300060411     C* LO RENDO POSITIVO
070400060411     C     comabb        IFLT      *ZEROS
070500060411     C     comabb        MULT      -1            §ABB
070600060411     C                   ELSE
070700060411     C                   Z-ADD     comabb        §ABB
070800060411     C                   END
070900060411     C* CONTROLLO IL LIMITE SUPERIORE DELL'ABBUOBO OPERATORE
071000060411     C     §ABB          ifgt      §GETQG
071100060411     C                   SETON                                        9041
071200060327     c                   end
071300060327     c* congruenza assegni
071400060411     c                   eval      comconinc =
071500060411     c                             ((disassv+preassv+fccassv+cpoassv+raiassv
071600060526     c                             +cgpassv)-(verassv)+ retassv)
071700060411     C     comconinc     SUB       casassv       comABB
071800060411     C* LO RENDO POSITIVO
071900060411     C     comabb        IFLT      *ZEROS
072000060411     C     comabb        MULT      -1            §ABB
072100060411     C                   ELSE
072200060411     C                   Z-ADD     comabb        §ABB
072300060411     C                   END
072400060411     C* CONTROLLO IL LIMITE SUPERIORE DELL'ABBUOBO OPERATORE
072500060411     C     §ABB          ifgt      §GETQG
072600060327     C                   SETON                                        9042
072700031023     c                   end
072800060329     c                   end
072900060518     c                   end
073000060329     c                   enddo
073100060329     c                   endsr
073200060203     C**----------------------------------
073300060203     C** CALCOLO SALDO INCASSI DA ATTRIBUIRE
073400060203     C**----------------------------------
073500060203     C     INCATT        BEGSR
073600060203     C                   SETOFF                                       1318
073700060414     C                   MOVE      *ZEROS        SALDPRF          15 2
073800060414     C                   MOVE      *ZEROS        SALDPRB          15 2
073900060203     C                   MOVE      '000000'      KCC               6            CAPOCONTO
074000060203     C                   MOVE      '00000000'    KSC               8            SOTTOCONTO
074100060203     C                   MOVE      §4OCC1        KCC                            CAPOCONTO
074200060203     C                   MOVE      §4OSC1        KSC                            SOTTOCONTO
074300060203      **
074400060203     C     K04ppa03      SETLL     ndppa03l
074500060203     C**
074600060203     C                   do        *hival
074700060203     C**
074800060203     C     K04ppa03      READE     ndppa03l
074900060203     c                   if        %eof(ndppa03l)
075000060203     C                   leave
075100060203     c                   end
075200060203     C**
075300060203     C     K08Pas03      SETLL     NdPas03l
075400060203do03 c                   do        *hival
075500060203     C     K08Pas03      READE     NdPas03l
075600060203      *
075700060203     c                   if        %eof(ndpas03l)
075800060203     C                   leave
075900060203     c                   end
076000060203     C                   move      pasdtpar      WDtpar            8 0
076100060214      * controllo se ho partite aperte negli incassi d'attrribuire di tre gg
076200060414      * indietro bloccante
076300060214do03 c                   if        wdtpar <= oggimeno3
076400060203if01 C     pasDAre       IFEQ      1
076500060414     C                   ADD       pasimportd    SALDPRB
076600060203el01 C                   ELSE
076700060414     C                   SUB       pasimportd    SALDPRB                        SALDO PART
076800060203en01 C                   ENDif
076900060203en01 C                   ENDif
077000060414      * controllo se ho partite aperte negli incassi d'attrribuire indietro
077100060414      * di 3 gg forzabile
077200060414do03 c                   if        wdtpar > oggimeno3 and wdtpar < oggimeno1
077300060414if01 C     pasDAre       IFEQ      1
077400060414     C                   ADD       pasimportd    SALDPRf
077500060414el01 C                   ELSE
077600060414     C                   SUB       pasimportd    SALDPRf                        SALDO PART
077700060414en01 C                   ENDif
077800060414en01 C                   ENDif
077900060203     C                   enddo
078000060203     C*
078100060414     C     saldprb       iflt      0
078200060414     C                   eval      saldprb= saldprb* -1
078300060203     C                   end
078400060414     C     saldprb       IFGt      §GEpin
078500060414     C                   SETON                                        9945
078600060203     c                   leave
078700060203     C                   End
078800060414     C*
078900060519     C*    saldprf       iflt      0
079000060519     C*                  eval      saldprf= saldprf* -1
079100060519     C*                  end
079200060519     C*    saldprf       IFGt      §GEpin
079300060519     C*                  SETON                                        9043
079400060519     c*                  leave
079500060519     C*                  End
079600060203     C                   enddo
079700060203     C                   ENDSR
079800021115     C**-------------------------------------------------------------
079900021120     C** srdta - data cassaforte al gg successivo x giroconto
080000021115     C**-------------------------------------------------------------
080100021120     C     srdta         BEGSR
080200021120     c                   do        *hival
080300021120     C                   EXFMT     CN0W102
080400030318     C   kl              leave
080500021120     c* controllo la data x giroconto
080600030514     c                   clear                   dtaeur
080700021120     c                   if        dtagsv = 0
080800021120     C                   SETON                                        32
080900021120     c                   else
081000021125     c     *eur          test(d)                 dtagsv                 32
081100021120     c                   if        *in32
081200021120     c     *dmy          test(d)                 dtagsv                 32
081300030514     c                   if        not *in32
081400030514     c     *dmy          move      dtagsv        dtaeur
081500030514     c                   end
081600030514     c                   else
081700030514     c                   move      dtagsv        dtaeur
081800021120     c                   end
081900030514     c                   end
082000030514     c*
082100030514     c                   if        not *in32
082200030514     c                   move      dtaeur        dtagsv
082300030514     c                   move      dtaeur        dtaiso
082400030514     c                   move      dtaiso        comdta            8 0
082500031003     c* controllo che la data sia maggiore di oggi
082600030514     c                   if        comdta <= amgudt
082700030514     c                   seton                                        32
082800030514     c                   end
082900031003     c* controllo che la data non sia maggiore di un mese da oggi
083000031003     c                   if        comdta > dtalimite
083100031003     c                   seton                                        32
083200031003     c                   end
083300030514     c                   end
083400030318     c                   if        not *in32 and *inkf
083500030318     c                   leave
083600030318     c                   end
083700021120     c                   enddo
083800021120     c                   endsr
083900060327     C**-------------------------------------------------------------
084000060327     C** srcomme - commento obbligatorio per f8 forzatura
084100060327     C**-------------------------------------------------------------
084200060327     C     srcomme       BEGSR
084300060427     c                   do        *hival
084400060427     c                   reset                   ana726ds
084500060427     c                   eval      opz726 = '01'
084600060427     c                   eval      ky1726 = 'CG'
084700060427     c                   eval      ky2726 = §4OCC1 + ' ' + §4OSC1
084800090424     c                   move      GMAUDt        anno              2 0
084900090424     c                   movel     GMAUDt        com06             6
085000090424     c                   move      anno          com06
085100060427     c                   eval      ky3726 = com06
085200060427     c                   eval      ky4726 = '000000' + %EDITC(vfil:'X') +
085300060427     c                             '      '
085400060427     c                   eval      kpjbu = ana726ds
085500060427     c                   call      'ANA726R'
085600060427     c                   parm                    kpjba
085700060427     c                   eval      ana726ds = kpjbu
085800060427     c                   if        opr726 <> '1'
085900060327     C                   SETON                                        44
086000060327     c                   else
086100060327     C                   SETOff                                       44
086200060427     c                   leave
086300060327     c                   end
086400060327     c                   enddo
086500060327     c                   endsr
086600060529     C**-------------------------------------------------------------
086700060529     C** srwin04 - richiesta per sede po da vedere
086800060529     C**-------------------------------------------------------------
086900060529     C     srwin04       BEGSR
087000060529     c                   do        *hival
087100060529     C                   EXFMT     CN0W104
087200060529     c   kc              leave
087300060529     c* controllo po
087400060529     c                   clear                   despov
087500060529     c                   if        pov = 0
087600060529     C                   iter
087700060529     c                   else
087800060529     c     pov           chain     azorg01l
087900060529     c                   if        %found(azorg01l)
088000060529     c                   movel     orgdes        despov
088100060529     c                   eval      vfil = pov
088200060529     c                   leave
088300060529     c                   else
088400060529     c                   movel     *all'?'       despov
088500060529     c                   iter
088600060529     c                   end
088700060529     c                   end
088800060529     c                   enddo
088900060529     c                   endsr
089000021120     C**-------------------------------------------------------------
089100021120     C** aggiorna
089200021120     C**-------------------------------------------------------------
089300021120     C     aggio         BEGSR
089400060404     c* leggo tutti i record di YFMDI del giorno per po
089500060404     c     qigdta        setgt     yfmdi01l
089600060404     c                   do        *hival
089700060404     c                   read      yfmdi01l
089800060404     c                   if        %eof(yfmdi01l)
089900060404     c                   leave
090000060404     c                   end
090100060404     c* solo il mio po
090200060529     c                   if        mdifle <> vfil
090300060404     c                   iter
090400060404     c                   end
090500060404     c* aggiorna i flag di presa incarico e versamente
090600060404     C                   MOVEL     'S'           mdiVIE
090700060404     c                   update    yfmdi000
090800060404     C                   ENDdo
090900021122     c* giroconto della cassaforte al gg successivo negli inc. attribuire
091000060202     c                   if        totcasv <> 0
091100021115     c                   exsr      cregir
091200021120     c                   end
091300021118     c* STAMPO
091400021120     c                   seton                                        99
091500060411     c* cassaforte gg precedente
091600060411     c                   seton                                        11
091700060411     c                   exsr      stampa
091800060411     c* rettifiche
091900060411     c                   exsr      staret
092000021120     c* mandati
092100021120     c                   exsr      stadis
092200021120     c* versamenti
092300021120     c                   exsr      staver
092400021118     c* cassaforte
092500060324     c                   exsr      stacass
092600021118     c* totale generale
092700021118     c                   seton                                        12
092800021118     c                   exsr      stampa
092900021115     c* se il totale è diverso da cassaforte lo segnalo anche in stampa
093000060511     c                   if        totgenv <> 0
093100021118     c                   seton                                        13
093200021115     c                   exsr      stampa
093300030331     c                   else
093400060321     c                   seton                                        29
093500030331     c                   exsr      stampa
093600021115     c                   end
093700021122     c* se è stata stornata la quadratura del giorno lo segnalo in stampa
093800021125     c                   movel     'A'           annqig
093900060202     c     kqig          chain     yfqig01l
094000060202     c                   if        %found(yfqig01l)
094100021120     c                   seton                                        15
094200021120     c                   exsr      stampa
094300021120     c* controllo anche il gg precedente perchè la quadratura la può aver
094400021120     c* rimandata ad oggi
094500021120     c                   else
094600030320     c* cerco l'ultimo gg non annullato
094700030320     c                   clear                   savdta
094800030320     c                   clear                   annqig
094900060202     c     kqig          setgt     yfqig01l
095000060202     c     kqig1         readpe    yfqig01l
095100060202     c                   if        not %eof(yfqig01l)
095200030320     c                   move      qigdta        savdta
095300030320     c                   end
095400030320     c* cerco l'ultimo gg annullato
095500030320     c                   clear                   qigdta
095600030320     c                   movel     'A'           annqig
095700060202     c     kqig          setgt     yfqig01l
095800060202     c     kqig1         readpe    yfqig01l
095900030320     c                   if        savdta < qigdta
096000021120     c                   seton                                        15
096100021120     c                   exsr      stampa
096200021120     c                   end
096300021120     c                   end
096400021125     c* memorizzo l'importo della cassaforte per la prossima quadratura
096500021125     c                   clear                   qigann
096600030320     c                   move      amgudt        qigdta
096700021125     c                   move      vfil          qigfle
096800021125     c                   move      dutcou        qigcuc
096900021125     c                   movel     'EUR'         qigvca
097000060202     c                   eval      qigicon = casfv + casconv
097100060202     c                   z-add     casassv       qigiass
097200060202     c                   write     yfqig000
097300021120     c                   endsr
097400021120     C**-------------------------------------------------------------
097500021120     C** stadis
097600021120     C**-------------------------------------------------------------
097700021120     C     stadis        BEGSR
097800060202     c                   clear                   totdiss
097900060202     c                   clear                   totpres
098000060202     c                   clear                   totfccs
098100060202     c                   clear                   totcpos
098200060214     c                   clear                   totrais
098300060411     c                   clear                   totcons
098400060411     c                   clear                   totasss
098500060411     c* testata incassi
098600060411     c                   seton                                        14
098700060411     c                   exsr      stampa
098800060214     c                   do        5             xx                1 0
098900021120     c* testata mandati
099000060203     c                   select
099100060202     c                   when      xx = 1
099200021120     c                   seton                                        01
099300060202     c                   eval      tmi = 'O'
099400060202     c                   when      xx = 2
099500060202     c                   seton                                        16
099600060202     c                   eval      tmi = 'P'
099700060202     c                   when      xx = 3
099800060202     c                   seton                                        19
099900060202     c                   eval      tmi = 'F'
100000060202     c                   when      xx = 4
100100060202     c                   seton                                        22
100200060223     c                   eval      tmi = 'Z'
100300060214     c                   when      xx = 5
100400060214     c                   seton                                        25
100500060420     c                   eval      tmi = 'G'
100600060202     c                   endsl
100700021120     c                   exsr      stampa
100800021120     c* dettaglio mandati
100900060202     c     qigdta        setgt     yfmdi01l
101000021120     c                   do        *hival
101100060202     c                   read      yfmdi01l
101200060202     c                   if        %eof(yfmdi01l)
101300021120     c                   leave
101400021120     c                   end
101500060529     c                   if        mdifle <> vfil
101600021122     c                   iter
101700021122     c                   end
101800060202     c                   if        mditmi <> tmi
101900060202     c                   iter
102000060202     c                   end
102100060202     c                   if        mdiicon<>0 or mdiiass<>0 or mdiipos<>0
102200060202     c                   select
102300060202     c                   when      xx = 1
102400060202     c                   seton                                        02
102500060406     c                   z-add     mdiipos       disposs
102600060406     c                   z-add     mdiicon       discons
102700060406     c                   z-add     mdiiass       disasss
102800060406     c                   eval      distots = discons+disasss
102900060406     c                   add       distots       totdiss
103000060406     c                   add       disposs       distots
103100060411     c                   add       discons       totcons
103200060411     c                   add       disasss       totasss
103300060202     c                   when      xx = 2
103400060202     c                   seton                                        17
103500060406     c                   z-add     mdiipos       preposs
103600060406     c                   z-add     mdiicon       precons
103700060406     c                   z-add     mdiiass       preasss
103800060406     c                   eval      pretots = precons+preasss
103900060406     c                   add       pretots       totpres
104000060406     c                   add       preposs       pretots
104100060411     c                   add       precons       totcons
104200060411     c                   add       preasss       totasss
104300060202     c                   when      xx = 3
104400060202     c                   seton                                        20
104500060406     c                   z-add     mdiipos       fccposs
104600060406     c                   z-add     mdiicon       fcccons
104700060406     c                   z-add     mdiiass       fccasss
104800060406     c                   eval      fcctots = fcccons+fccasss
104900060406     c                   add       fcctots       totfccs
105000060406     c                   add       fccposs       fcctots
105100060411     c                   add       fcccons       totcons
105200060411     c                   add       fccasss       totasss
105300060321     c* cliente
105400060321     c                   exsr      srcli
105500060202     c                   when      xx = 4
105600060202     c                   seton                                        23
105700060406     c                   z-add     mdiipos       cpoposs
105800060406     c                   z-add     mdiicon       cpocons
105900060406     c                   z-add     mdiiass       cpoasss
106000060406     c                   eval      cpotots = cpocons+cpoasss
106100060406     c                   add       cpotots       totcpos
106200060406     c                   add       cpoposs       cpotots
106300060411     c                   add       cpocons       totcons
106400060411     c                   add       cpoasss       totasss
106500060214     c                   when      xx = 5
106600060214     c                   seton                                        26
106700060406     c                   z-add     mdiipos       raiposs
106800060406     c                   z-add     mdiicon       raicons
106900060406     c                   z-add     mdiiass       raiasss
107000060406     c                   eval      raitots = raicons+raiasss
107100060406     c                   add       raitots       totrais
107200060406     c                   add       raiposs       raitots
107300060411     c                   add       raicons       totcons
107400060411     c                   add       raiasss       totasss
107500060202     c                   endsl
107600021120     c                   exsr      stampa
107700021120     c                   end
107800021120     c                   enddo
107900021120     c* totale mandati
108000060202     c                   select
108100060202     c                   when      xx = 1
108200060202     c                   seton                                        03
108300060202     c                   when      xx = 2
108400060202     c                   seton                                        18
108500060202     c                   when      xx = 3
108600060202     c                   seton                                        21
108700060202     c                   when      xx = 4
108800060202     c                   seton                                        24
108900060214     c                   when      xx = 5
109000060214     c                   seton                                        27
109100060202     c                   endsl
109200021120     c                   exsr      stampa
109300060202     c                   enddo
109400060411     c* totale incassi
109500060411     c                   seton                                        05
109600060411     c                   eval      totincs = totdiss+totpres+totfccs+totcpos
109700060411     c                             +totrais
109800060411     c                   exsr      stampa
109900021120     c                   endsr
110000021120     C**-------------------------------------------------------------
110100060321     C** decodifica il cliente codificato
110200021120     C**-------------------------------------------------------------
110300060321     C     srcli         BEGSR
110400060321     c                   clear                   cnacods
110500060321     c                   clear                   esito
110600060321     c                   callp     CHKcliente(*omit:*omit:mdiksc:*omit:*off
110700060321     c                             :*omit:*omit:*omit:esito:errc:*omit:cnacods
110800060321     c                             :*omit:*omit:*omit)
110900060321     c                   if        esito = 0
111000060321     c                   movel     acorag        desksc
111100060321     c                   else
111200060321     c                   clear                   desksc
111300060321     c                   end
111400060321     c                   endsr
111500060321     C**-------------------------------------------------------------
111600060321     C** staver
111700060321     C**-------------------------------------------------------------
111800060321     C     staver        BEGSR
111900060202     c                   clear                   totvrts
112000060411     c                   clear                   totvrcs
112100060411     c                   clear                   totvras
112200021120     c* testata versamenti
112300021120     c                   seton                                        07
112400021120     c                   exsr      stampa
112500021120     c* dettaglio versamenti
112600021122     c     kvrt          setgt     fnvrt01l
112700021120     c                   do        *hival
112800021122     c     vfil          reade     fnvrt01l
112900021120     c                   if        %eof(fnvrt01l)
113000021120     c                   leave
113100021120     c                   end
113200021120     c                   seton                                        08
113300060406     c                   move      vrtddv        dtaiso
113400060406     c                   move      dtaiso        dtaeur
113500060406     c                   move      dtaeur        eurddv
113600060406     c                   z-add     vrtass        vrtasss
113700060406     c                   z-add     vrtcon        vrtcons
113800060406     c     vrtasss       add       vrtcons       vrttots
113900060406     c                   add       vrttots       totvrts
114000060411     c                   add       vrtcons       totvrcs
114100060411     c                   add       vrtasss       totvras
114200021120     c                   exsr      stampa
114300021120     c                   enddo
114400021120     c* totale versamenti
114500021120     c                   seton                                        09
114600021120     c                   exsr      stampa
114700021115     c                   endsr
114800060406     C**-------------------------------------------------------------
114900060406     C** staret
115000060406     C**-------------------------------------------------------------
115100060406     C     staret        BEGSR
115200060406     c                   clear                   totrets
115300060406     c* testata rettifiche
115400060406     c                   seton                                        04
115500060406     c                   exsr      stampa
115600060418     c* dettaglio rettifiche
115700060406     c     kvrt          setgt     yfqri01l
115800060406     c                   do        *hival
115900060406     c     vfil          reade     yfqri01l
116000060406     c                   if        %eof(yfqri01l)
116100060406     c                   leave
116200060406     c                   end
116300060418     c                   exsr      srrett
116400060406     c                   move      qridtr        dtaiso
116500060406     c                   move      dtaiso        dtaeur
116600060406     c                   move      dtaeur        retdtas
116700060406     c                   z-add     qriiass       retasss
116800060406     c                   z-add     qriicon       retcons
116900060406     c     retasss       add       retcons       rettots
117000060406     c                   add       rettots       totrets
117100060406     c                   seton                                        38
117200060406     c                   exsr      stampa
117300060406     c                   enddo
117400060418     c* totale rettifiche
117500060406     c                   seton                                        39
117600060406     c                   exsr      stampa
117700060406     c                   endsr
117800021115     C**-------------------------------------------------------------
117900060418     C* Reperisco decodifica rettifica
118000021115     C**-------------------------------------------------------------
118100060418     C     srrett        BEGSR
118200060418     C                   Movel     'YRT'         Tbecod
118300060418     C                   Movel(p)  qricau        Tbeke1
118400060418     C                   Movel     *blanks       Tbeke2
118500060418     C     Ktbe1         Chain     Tntbe01l
118600060418     c                   if        %found(tntbe01l)
118700060418     C                   Movel     tbeuni        dyrt
118800060418     c                   else
118900060418     c                   clear                   dgei
119000060418     c                   end
119100060418     c                   movel     §yrtdes       descau
119200060418     c                   endsr
119300060418     C**-------------------------------------------------------------
119400060418     C** cassaforti
119500060418     C**-------------------------------------------------------------
119600060418     C     stacass       BEGSR
119700060327     c* testata cassaforte feritoia
119800060327     c                   seton                                        33
119900060327     c                   exsr      stampa
120000060327     c* dettaglio mandatini
120100060605     c                   clear                   savcuc
120200060605     c                   clear                   savda8
120300060605     c                   clear                   savora
120400060605     c                   clear                   impfers
120500060605     c     vfil          setll     yfcas01l
120600060327     c                   do        *hival
120700060406     c     vfil          reade     yfcas01l
120800060327     c                   if        %eof(yfcas01l)
120900060605     c                   if        impfers <> 0
121000060605     c                   seton                                        34
121100060605     c                   exsr      stampa
121200060605     c                   end
121300060327     c                   leave
121400060327     c                   end
121500060502     c                   if        casvie = 'S' or casqta < 0
121600060407     c                   iter
121700060407     c                   end
121800060605     c                   if        savcuc <> 0 and
121900060605     c                             (savcuc <> cascuc or savda8 <> casda8 or
122000060605     c                             savora <> casora)
122100060327     c                   seton                                        34
122200060327     c                   exsr      stampa
122300060605     c                   clear                   impfers
122400060605     c                   end
122500060605     c                   eval      cucfers = cascuc
122600060605     c                   eval      orafers = casora
122700060605     c                   move      casda8        dtaiso
122800060605     c                   move      dtaiso        dtaeur
122900060605     c                   move      dtaeur        dtafers
123000060605     c                   add       casicon       impfers
123100060605     c                   eval      savcuc = cascuc
123200060605     c                   eval      savda8 = casda8
123300060605     c                   eval      savora = casora
123400060327     c                   enddo
123500060327     c* totale cassaforte feritoia
123600060327     c                   seton                                        35
123700060327     c                   exsr      stampa
123800060327     c* testata cassaforte servizio
123900060327     c                   seton                                        36
124000060327     c                   exsr      stampa
124100060327     c* dettaglio contante
124200060327     c     knraz         setll     fidci01l
124300060327     c                   do        *hival
124400060327     c     knraz         reade     fidci01l
124500060327     c                   if        %eof(fidci01l)
124600060327     c                   leave
124700060327     c                   end
124800060327     c                   eval      qtadets = dciqta
124900060327     c                   eval      valdets = dcivalore
125000060327     c                   eval      impdets = dciqta * dcivalore
125100060327     c                   seton                                        10
125200060327     c                   exsr      stampa
125300060327     c                   enddo
125400060327     c* totale cassaforte feritoia
125500060327     c                   seton                                        37
125600060327     c                   exsr      stampa
125700060327     c                   endsr
125800060327     C**-------------------------------------------------------------
125900060327     C** stampa
126000060327     C**-------------------------------------------------------------
126100060327     C     stampa        BEGSR
126200021120     c* mandato in stampa
126300060406     c                   if        *in02 or *in17 or *in20 or *in23 or *in26
126400060202     c                   move      mdidmi        dtaiso
126500021125     c                   move      dtaiso        dtaeur
126600060203     c                   move      dtaeur        eurdmi
126700060202     c                   end
126800021120     c* messaggio storno
126900021120     c                   if        *in15
127000021121     c                   move      qigdta        dtaiso
127100021125     c                   move      dtaiso        dtaeur
127200021125     c                   move      dtaeur        eurdta
127300021120     c                   end
127400021120     c*
127500021121     c   99              write     testa
127600021118     c                   setoff                                       99
127700021121     c   14              write     tesinc
127800021121     c   01              write     tesman
127900021121     c   02              write     detman
128000021121     c   03              write     totman
128100060202     c   16              write     tespre
128200060202     c   17              write     detpre
128300060202     c   18              write     totpre
128400060202     c   19              write     tesfcc
128500060202     c   20              write     detfcc
128600060202     c   21              write     totfcc
128700060202     c   22              write     tescpo
128800060202     c   23              write     detcpo
128900060202     c   24              write     totcpo
129000060214     c   25              write     tesrai
129100060214     c   26              write     detrai
129200060214     c   27              write     totrai
129300021121     c   05              write     totinc
129400021121     c   07              write     tesver
129500021121     c   08              write     detver
129600021121     c   09              write     totver
129700060327     c   33              write     tesfer
129800060327     c   34              write     detfer
129900060327     c   35              write     totfer
130000060327     c   36              write     tescas
130100060327     c   10              write     detcas
130200060327     c   37              write     totcas
130300021121     c   11              write     detcasg
130400060406     c   04              write     tesret
130500060406     c   38              write     detret
130600060406     c   39              write     totret
130700021121     c   12              write     totgen
130800060321     c   29              write     quaok
130900030331     c   13              write     errore
131000021121     c   15              write     storno
131100021118     c                   movea     '000000000000'*in(01)
131200060202     c                   movea     '000000000000'*in(13)
131300060214     c                   movea     '000'         *in(25)
131400060321     c                   movea     '0'           *in(29)
131500060406     c                   movea     '0000000'     *in(33)
131600021115     c                   endsr
131700021118     C**-------------------------------------------------------------
131800021118     C** CREAZIONE MOVIMENTI CONTABILI di giroconto
131900021118     C**-------------------------------------------------------------
132000021120     C     CREGIR        BEGSR
132100021115     C* DARE
132200021115     C                   MOVE      KNMUS         MOVNMU
132300021115     C                   MOVE      KNRAZ         MOVRIF
132400021118     C                   MOVEL     vfil          MOVRIF
132500021120     C                   add       1             MOVSBN
132600021115     C                   Z-ADD     1             MOVSBR                         *PROGR.REG
132700021115     C                   MOVEL     *BLANKS       MOVFGE
132800060203     C                   MOVEL     soc           MOVSOC
132900021115     C                   MOVEL     *BLANKS       MOVFPN
133000021115     C                   MOVEL     vfil          MOVFPN
133100021115     C                   MOVEL     'D'           MOVTMO
133200021115     C                   MOVEL     'CG'          MOVCTB
133300021120     C                   MOVE      VCDUS         WCOMO6
133400021120     C                   MOVEL     vfil          WCOMO6
133500021120     C                   MOVEL     WCOMO6        movnrg
133600021120     C                   MOVE      9             movnrg
133700021118     C                   Z-ADD     AMGUDT        MOVDRG
133800021115     C                   MOVE      'COGE'        MOVSRR
133900021118     C                   MOVEL     §4OCO5        MOVCOD
134000021115     C                   Z-ADD     1             MOVDAR
134100021115     C                   Z-ADD     0             MOVAVE
134200021115     C                   MOVEL     'EUR'         MOVDIV
134300021115     C                   Z-ADD     0             MOVNDC
134400021115     C                   Z-ADD     0             MOVDDC
134500021115     C                   MOVE      *BLANKS       MOVSRD
134600021115     C                   Z-ADD     vfil          MOVNRP
134700021125     C                   move      dtagsv        dtaeur
134800021125     C                   move      dtaeur        dtaiso
134900021120     C                   move      dtaiso        MOVDRP
135000021115     C                   MOVEL     *BLANKS       MOVSRP
135100021115     C                   MOVEL     'EUR'         MOVDIP
135200021115     C                   Z-ADD     totcasv       MOVIPM
135300021115     C                   Z-ADD     totcasv       MOVIPD
135400021125     C                   Z-ADD     TOTCASV       MOVIMM
135500021125     C                   Z-ADD     TOTCASV       MOVIMD
135600021115     C                   Z-ADD     0             MOVDSC                         *
135700021115     C                   MOVEL     *BLANKS       MOVDES
135800021115     C                   MOVE      *ZEROS        MOVKCC
135900021115     C                   MOVE      *ZEROS        MOVKSC
136000021115     C                   MOVE      *ZEROS        MOVCCP
136100021115     C                   MOVE      *ZEROS        MOVSCP
136200021115     C                   MOVE      §4OCC1        MOVkcc
136300021115     C                   MOVE      §4OSC1        MOVksc
136400021122     C                   MOVE      §4OCC1        MOVccp
136500021122     C                   MOVE      §4OSC1        MOVscp
136600021115     C                   MOVEL     *BLANKS       MOVCDC
136700021115     C                   Z-ADD     0             MOVDCA
136800021115     C                   Z-ADD     MOVDRG        MOVDOP
136900021115     C                   Z-ADD     1             MOVCMB
137000021115     C                   WRITE     FNMOV000
137100021122     C* AVERE
137200021122     C                   Z-ADD     2             MOVSBR                         *PROGR.REG
137300021122     C                   Z-ADD     0             MOVDAR
137400021122     C                   Z-ADD     1             MOVAVE
137500021122     C                   move      amgudt        MOVDRP
137600021122     C                   WRITE     FNMOV000
137700021115     C                   ENDSR
137800991118     C*----------------------------------------------------*
137900991118     C* ESEGUO OVRDBF SU FNMOV
138000991118     C*----------------------------------------------------*
138100991118     C     OVRMOV        BEGSR
138200991118     C*
138300020926     C                   MOVE      '046) '       VAR               8
138400021118     C                   MOVEL     vfil          VAR
138500020925     C                   Z-ADD     49            LUNG             15 5
138600991118     C** CONTROLLO SE ESISTE IL MEMBRO X FNMOV00T
138700060321     C                   SETOFF                                       98
138800991118     C                   MOVE      VAR           CMDD(1)
138900991118     C                   MOVEL     *BLANKS       COMMAN
139000030411     C                   MOVEl(p)  CMDD(1)       COMMAN           80
139100060321     C                   CALL      'QCMDEXC'                            98
139200991118     C                   PARM                    COMMAN
139300991118     C                   PARM                    LUNG
139400991118     C** ESEGUE ADDPFM DEL MEMBRO NUOVO
139500060321     C   98              DO
139600991118     C                   MOVE      VAR           CMDD(2)
139700991118     C                   MOVEL     *BLANKS       COMMAN
139800030411     C                   MOVEl(p)  CMDD(2)       COMMAN
139900991118     C                   CALL      'QCMDEXC'
140000991118     C                   PARM                    COMMAN
140100991118     C                   PARM                    LUNG
140200991118     C                   END
140300991118     C*+ ESEGUE IN OGNI CASO OVRDBF DEL MEMBRO DESIDERATO
140400991118     C                   MOVE      VAR           CMDD(3)
140500991118     C                   MOVEL     *BLANKS       COMMAN
140600030411     C                   MOVEl(p)  CMDD(3)       COMMAN
140700991118     C                   CALL      'QCMDEXC'
140800991118     C                   PARM                    COMMAN
140900991118     C                   PARM                    LUNG
141000991118     C* GUARDO SE è ALLOCATO
141100991118     C                   MOVEL     VAR           VAR
141200991118     C                   MOVE      ')'           VAR
141300991118     C                   MOVE      VAR           CMDD(4)
141400030411     C                   MOVEl(p)  CMDD(4)       COMMAN
141500020925     C                   Z-ADD     49            LUNCMD           15 5
141600991118     C                   CALL      'QCMDEXC'                            91
141700991118     C                   PARM                    COMMAN
141800991118     C                   PARM                    LUNCMD
141900991118     C*
142000021121     C  N91              OPEN      FNMOV00T                             91
142100030318     c                   if        *in91
142200030411     C                   MOVEL     'NO'          KPJBU
142300030318     C                   SETON                                        28
142400030318     C                   MOVEL     ERR(4)        $MSG
142500030411     c                   else
142600030318     c* controllo che il membro sia vuoto
142700030318     c                   exsr      tstobj
142800030318     c                   if        nrr <> 0
142900030318     C                   SETON                                        28
143000030318     C                   MOVEL     ERR(5)        $MSG
143100030411     c                   end
143200030318     c                   end
143300991118     C*
143400991118     C                   ENDSR
143500991118     C*----------------------------------------------------*
143600991118     C* ESEGUO Dlcobj  FNMOV
143700991118     C*----------------------------------------------------*
143800991118     C     DLCMOV        BEGSR
143900991118     C*
144000991118     C* GUARDO SE è ALLOCATO
144100991118     C*
144200021122     C  N91              CLOSE     FNMOV00T                             91
144300991118     C                   MOVE      VAR           CMDD(5)
144400030411     C                   MOVEl(p)  CMDD(5)       COMMAN
144500020925     C                   Z-ADD     49            LUNCMD           15 5
144600991118     C                   CALL      'QCMDEXC'                            91
144700991118     C                   PARM                    COMMAN
144800991118     C                   PARM                    LUNCMD
144900991118     C*
145000991118     C                   ENDSR
145100021122     C*-------------------------------------------------------
145200021122     ** Sottometto trasmissioni
145300021122     **-------------------------------------------------------
145400021122     C     SBMTRA        BEGSR
145500021122     C*
145600021122     C                   MOVEL     '046'         CMZDST                          DEST.:SED
145700021122     C                   MOVEL     'FNMOV00T'    CMZFLD                          DA: >>>
145800060529     C                   EVAL      CmzMbD = 'M' + %EDITC(vfil:'X') + '046'
145900021122     C                   MOVEL     O50PLA        CMZLBD
146000021122     C                   MOVEL     'FNMOV00R'    CMZFLA                          A: >>>
146100021122     C                   MOVEL     CmzMbD        CMZMBA
146200021122     C                   MOVEL     O50ALA        CMZLBA
146300021122     C                   MOVEL     'GAITRA'      CMZLBA
146400021122     C                   MOVE      O50ALA        WQUATR            4
146500021122     C     WQUATR        IFEQ      '101 '
146600021122     C                   MOVE      '201 '        CMZLBA
146700021122     C                   ENDIF
146800021122     C                   MOVE      '0000'        CMZORI                          ORA
146900021122     C                   MOVE      '0000000'     CMZDTI                          DATA
147000021122     C                   MOVE      'N'           CMZURG                          NOTTE
147100021122     C                   MOVE      'A'           CMZADD                          IN ADD
147200021122     C                   MOVE      'S'           CMZCLR                          CLEAR
147300021122     C                   MOVE      'S'           CMZFTR                          DA TRANS.
147400021122     C                   MOVE      *BLANK        CMZERR                          1= ERR.
147500021122     C                   MOVE      'Y029'        CMZAZN                          AZIONE
147600021122     C                   MOVEL     O50ASI        CMZSYF                          SIST.INF.
147700021122     C                   MOVEL     'GAITRA'      CMZSYF
147800021122     C                   MOVE      O50ASI        WQUATR
147900021122     C     WQUATR        IFEQ      '101 '
148000021122     C                   MOVE      '201 '        CMZSYF
148100021122     C                   ENDIF
148200021122     C                   MOVE      nrr           CMZNRR                          N.REC.
148300021122     C                   MOVE      *BLANKS       CMZPRR                          FILE PRE-
148400021122     C*
148500021122     C                   CLEAR                   W236
148600021122     C                   CALL      'TIBS53C'
148700021122     C                   PARM                    O50LCL
148800021122     C                   PARM                    CMZDST
148900021122     C                   PARM                    CMZFLD
149000021122     C                   PARM                    CMZMBD
149100021122     C                   PARM                    CMZLBD
149200021122     C                   PARM                    CMZFLA
149300021122     C                   PARM                    CMZMBA
149400021122     C                   PARM                    CMZLBA
149500021122     C                   PARM                    CMZORI
149600021122     C                   PARM                    CMZDTI
149700021122     C                   PARM                    CMZURG
149800021122     C                   PARM                    CMZADD
149900021122     C                   PARM                    CMZCLR
150000021122     C                   PARM                    CMZFTR
150100021122     C                   PARM                    CMZERR
150200021122     C                   PARM                    CMZAZN
150300021122     C                   PARM                    CMZSYF
150400021122     C                   PARM                    CMZNRR
150500021122     C                   PARM                    CMZPRR
150600021122     C                   PARM                    W236            236
150700021122     C*
150800021122     C                   ENDSR
150900021122      *-----------------------------------------------------***********
151000021122      * TEST NR RECORD FILE DA TRASMETTER                   *  TSTOBJ *
151100021122      *-----------------------------------------------------***********
151200021122     C     TSTOBJ        BEGSR
151300021122     C* Richiamo TIBS55R per conoscere librerie/S.Informativi
151400021122     C* da/a cui spedire
151500021122     C                   CLEAR                   TIBS55ds
151600021122     C                   MOVEL     'L'           I50TLA
151700060529     C                   MOVEL     vfil          I50PPO
151800021122     C                   MOVEL     '046'         I50APO
151900021122     C                   CALL      'TIBS55R'
152000021122     C                   PARM                    TIBS55ds
152100060529     C* MEMBRO = M + vfil + 046
152200021122     C* GUARDO SE IL MEMBRO è VUOTO - FNMOV00T
152300021122     C                   MOVEL     o50pla        LIBR
152400021122     C                   MOVEL(p)  'FNMOV00T'    FILE
152500060529     C                   EVAL      mbr = 'M' + %EDITC(vfil:'X') + '046'
152600021122     C                   Z-ADD     0             NRR
152700021122     C                   CALL      'TRUL51C'
152800021122     C                   PARM                    LIBR             10
152900021122     C                   PARM                    FILE             10
153000021122     C                   PARM                    MBR              10
153100021122     C                   PARM                    NRR              10 0
153200021122     C                   ENDsr
153300991102      *-----------------------------------------------------***********
153400991102      * CONTROLLO DISTINTE CHIUSE NON CONTABILIZZATE        *  CNTDIS *
153500991102      *-----------------------------------------------------***********
153600991102     C     CNTDIS        BEGSR
153700021211      * OVRPRTF
153800021211     C     WOPNST        IFNE      'S'
153900021211     C                   MOVEL     'S'           WOPNST            1
154000120725     C                   MOVEA     CMDP          CMDPX            60
154100120725     C                   Z-ADD     60            LUNCMD           15 5
154200021211     C                   CALL      'QCMDEXC'                            92
154300021211     C                   PARM                    CMDPX
154400021211     C                   PARM                    LUNCMD
154500021211     C                   OPEN      QSYSPRT
154600021211     C                   END
154700991102     C* loop lettura distinte chiuse senza mandato d'introito
154800060529     C                   EVAL      ArbIPP = vfil
154900020925     C                   CLEAR                   ArbNMI
155000021211     C                   CLEAR                   UNO
155100021211     C     K02Arb71      SETLL     FnArb71L
155200021211     C                   DO        *HIVAL
155300021211     C     K02Arb71      READE     FnArb71L
155400021211     C                   if        %EOF(fnarb71l)
155500021211     C                   LEAVE
155600021211     C                   END
155700021211     C* ESCLUDO LE DISTINTE 999999
155800021211     C                   if        ARBNDC = *ALL'9'
155900021211     C                   ITER
156000021211     C                   END
156100021211     C                   IF        UNO = 0
156200991214     C                   SETON                                        28
156300991102     C                   MOVEL     ERR(2)        $MSG
156400021211     C* Stampo testata
156500021211     C                   EXCEPT    TESARB
156600021211     C                   Z-ADD     1             UNO               1 0
156700021211     C                   END
156800021211     C* Stampo DETTAGLIO
156900030318     c                   if        arbddc <> 0
157000030318     C                   MOVE      arbddc        DtaISO
157100030318     C                   MOVE      DTAISO        DtaEUR
157200021211     C                   MOVE      DTAEUR        GMADDC            8 0
157300030318     c                   else
157400030318     c                   clear                   gmaddc
157500030318     c                   end
157600120531     C* Decodifica Utente in stampa
157700120531     c                   clear                   nomUTE
157800120531     c                   z-add     arbIFP        arbfiliale        3 0
157900120531     c     kuteute       chain     azute02l
158000120531     c                   if        %Found(azute02l)
158100120531     c                   movel     filUTE        nomUTE
158200120531     c                   end
158300120816     c                   if        arbndc <> exarbndc
158400021211     C                   EXCEPT    DETARB
158500120816     c                   move      arbndc        exarbndc
158600120816     c                   endif
158700021211     C                   ENDDO
158800021211     C*
158900021211     C                   IF        UNO = 1
159000021211     C                   EXCEPT    FINEST
159100021211     C                   END
159200021119     C* controllo che ci sia una sola quadratura giornaliera
159300021120     c                   clear                   annqig
159400060202     c     kqig          chain     yfqig01l
159500060202     c                   if        %found(yfqig01l)
159600021119     C                   SETON                                        28
159700021119     C                   MOVEL     ERR(3)        $MSG
159800021119     c                   end
159900060424     c                   z-add     0             page
160000991102     C                   ENDSR
160100991102     C*-----------------------------------------------------------*
160200030318     C*  msg a cli* in sede di errata quadratura
160300991029     C*-----------------------------------------------------------*
160400030318     C     srmsg         BEGSR
160500090424     c                   move      GMAUDt        anno              2 0
160600090424     c                   movel     GMAUDt        com08             6
160700090424     c                   move      anno          com08
160800100820     C                   eval      emogg  = 'FORZATA quadratura giornaliera -
160900030318     C                             incassi e versamenti'
161000100820     C                   eval      emmsg  = 'Punto operativo '
161100060529     c                             + %editc(vfil:'X')
161200030318     c                             + ' in data '
161300030320     c                             + %subst(com08: 1: 2) + '/'
161400030320     c                             + %subst(com08: 3: 2) + '/'
161500060427     c                             + %subst(com08: 5: 2)
161600060328     c*?Se passato p.o.
161700060327     C                   if        *in40
161800100820     C                   EVAL      emmsg  = %trimr(emmsg) +
161900060328     C                             '. Totale cassaforte <> da totale generale'
162000060327     C                   END
162100060327     C                   if        *in41
162200100820     C                   EVAL      emmsg = %trimr(emmsg) +
162300060328     C                             '. Errore contante.'
162400060327     C                   END
162500060327     C                   if        *in42
162600100820     C                   EVAL      emmsg  = %trimr(emmsg) +
162700060328     C                             '. Errore assegni.'
162800060327     C                   END
162900060327     C                   if        *in43
163000100820     C                   EVAL      emmsg  = %trimr(emmsg) +
163100060414     C                             '. Inc. d''attribuire aperti da oltre 1gg.'
163200060327     C                   END
163300030318     C* UTENTE  A CUI INVIARE IL MESSAGGIO
163400100820     C*mm                MOVEL(P)  'CLI*'        MSGUTI
163500030318     C* FILIALE A CUI INVIARE IL MESSAGGIO
163600100820     C*mm                Z-ADD     046           MSGFIL
163700030318     C*
163800100820     C*                  CALL      'TNSYM4C'
163900100820     C*mm                PARM                    MSGTES           55
164000100820     C*mm                PARM                    MSGDET          198
164100100820     C*mm                PARM                    MSGFIL            3 0
164200100820     C*mm                PARM                    MSGUTI           10
164300130701     c                   eval      emlto = 'GestioneClienti@brt.it'
164400130701     c*                  eval      emlto = 'ufficio.pagamenti@bartolini.it'
164500100820     c                   eval      emlcc = *blank
164600100820     C                   CALL      'TIS701C1'
164700100820     C                   PARM                    emlto           253
164800100820     C                   PARM                    emlcc           253
164900100820     C                   PARM                    emogg            44
165000100820     C                   PARM                    emmsg          5000
165100100820     C                   PARM                    emesito           1
165200030318     C                   ENDSR
165300030318     C*-----------------------------------------------------------*
165400030318     C*  OPERAZIONI INIZIALI
165500030318     C*-----------------------------------------------------------*
165600030318     C     *INZSR        BEGSR
165700991029     C*
165800000000     C     *ENTRY        PLIST
165900000000     C                   PARM                    KPJBA
166000060517     C*---------- RICERCA DITTA :
166100060517     C                   MOVEL     'SOC001'      TIPXSC
166200060517     C                   MOVEL     'SOC001'      TIPXSC
166300060517     C                   MOVEL     *BLANK        SOCXSC
166400060517     C                   EXSR      REPSOC
166500060517     C     RTNXSC        IFNE      '1'
166600060517     C                   MOVEL     XSOCDS        SOC001
166700060517     C                   MOVEL     xscrgs        RSUT             20
166800060517     c                   else
166900060517     c                   seton                                        lr
167000060517     c                   return
167100060517     c                   end
167200900516      *
167300000000     C                   Z-ADD     1             CODUT
167400900517     C                   CALL      'X§PARUT'
167500900517     C                   PARM                    UT§DSE
167600021118     C*
167700021118     c     *dtaara       define    §azute        azuteds
167800021118     c     *dtaara       define    §datiute      ddatiute
167900021118     C                   in(E)     *dtaara
168000021118     C                   IF        %Error  or  RSUT = *blanks
168100021118     C                   call      'TIBS34R'
168200021118     C                   parm                    Tibs34Ds
168300021118     C                   in        *dtaara
168400021118     c                   ENDIF
168500060327     c                   movel     'CG'          Ctg
168600060327     c                   movel     '201'         soc
168700060529     C                   EVAL      VFil = dutpou
168800060529     c* sede
168900060529     c                   if        dutlpo = 'S' or dutlpo = ' '
169000060529     c                   seton                                        50
169100060529     c                   exsr      srwin04
169200060529     c                   end
169300040921     c* Descrizione p.o.
169400040921     C     vfil          CHAIN     AZORG01L                           21
169500040921     C  N21              MOVEL     ORGDES        DFil
169600060203     C* REPERISCO TABELLA Y4O
169700060203     C                   EXSR      SRY4O
169800021125     c* apro FNMOV00T
169900021125     c                   exsr      ovrmov
170000021118     C*--- RICERCA CAPOCONTO cassa------------------------------------*
170100021115     C                   DO        50            X                 2 0
170200021115     C                   MOVE      TCU(X)        TCUDS
170300021115     C     F56           CABNE     'CG'          END1
170400021115     C     F4            IFEQ      '4'
170500021115     C                   Z-ADD     KCU(X)        KCAS              4 0
170600021115     C                   END
170700021115     C     END1          TAG
170800021115     C                   END
170900090410     C                   TIME                    WHHUDT           14 0
171000991102     C                   TIME                    TIME              6 0
171100090410     C                   MOVE      WHHUDT        G08DAT
171200090410     C                   MOVE      *ZEROS        G08INV
171300090410     C                   MOVEL     *BLANK        G08ERR
171400090410     C                   CALL      'XSRDA8'
171500090410     C                   PARM                    WLBDA8
171600090410     C                   Z-ADD     G08INV        AMGUDt            8 0
171700090410     C                   Z-ADD     G08DAT        GMAUDt            8 0
171800031003     c* aggiungo un mese alla data di oggi
171900031003     c                   move      amgudt        dtaiso
172000031003     c                   adddur    1:*m          dtaiso
172100031003     c                   move      dtaiso        dtalimite
172200060426     C                   Z-ADD     0             D52dfi
172300061221     C                   Z-ADD     3             D52GIO
172400061221     c                   if        amgudt > 20061224 and
172500061221     c                             amgudt < 20070101
172600061221     C                   Z-ADD     5             D52GIO
172700061221     c                   end
172800060426     C                   MOVE      amgudt        D52DIN
172900060426     C                   MOVE      '-'           D52SGN
173000060426     C                   CALL      'TRUL52R'
173100060426     C                   PARM                    TRUL52DS
173200060426     C                   MOVE      D52DFI        oggimeno3         8 0
173300060426     C                   Z-ADD     0             D52dfi
173400060426     C                   Z-ADD     1             D52GIO
173500060426     C                   MOVE      amgudt        D52DIN
173600060426     C                   MOVE      '-'           D52SGN
173700060426     C                   CALL      'TRUL52R'
173800060426     C                   PARM                    TRUL52DS
173900060426     C                   MOVE      D52DFI        oggimeno1         8 0
174000060203     C* Reperisco tabella limite abbuoni
174100060203     C                   Movel     'GEI'         Tbecod
174200060203     C                   Movel     *blanks       Tbeke1
174300060203     C                   Movel     'EUR'         Tbeke1
174400060203     C                   Movel     *blanks       Tbeke2
174500060203     C     Ktbe1         Chain     Tntbe01l
174600060203     c                   if        %found(tntbe01l)
174700060203     C                   Movel     tbeuni        dgei
174800060203     c                   else
174900060203     c                   clear                   dgei
175000060203     c                   end
175100070731     C* Reperisco tabella DGD x controllo partite aperte
175200070731     C                   Movel     'DGD'         Tbecod
175300070731     C                   Movel     *blanks       Tbeke1
175400070731     C                   Movel(p)  '1'           Tbeke1
175500070731     C                   Movel     *blanks       Tbeke2
175600070731     C     Ktbe1         Chain     Tntbe01l
175700070731     c                   if        %found(tntbe01l)
175800070731     C                   Movel     tbeuni        ddgd
175900070731     c                   else
176000070731     c                   clear                   ddgd
176100070731     c                   end
176200060327     c* serve per emettere la finestra del contante la 1° volta
176300060327     c                   eval      flgcon = *on
176400900515      *---------------------------------------------------------------*
176500900515      * CHIAVI                                                        *
176600900515      *---------------------------------------------------------------*
176700021119     C     Kqig          KLIST
176800021120     C                   KFLD                    annqig
176900021120     C                   KFLD                    vfil
177000021119     C                   KFLD                    amgudt
177100021120     C     Kqig1         KLIST
177200021120     C                   KFLD                    annqig
177300021120     C                   KFLD                    vfil
177400021118     C     KTBE1         KLIST
177500021118     C                   KFLD                    tbecod
177600021118     C                   KFLD                    tbeke1
177700021118     C                   KFLD                    tbeke2
177800021120     C     Kvrt          KLIST
177900021121     C                   KFLD                    vfil
178000021122     C                   KFLD                    qigdta
178100021121     C     K02Arb71      KLIST
178200021121     C                   KFLD                    ArbIPP
178300021121     C                   KFLD                    ArbNMI
178400060203     C     K04Ppa03      KLIST
178500060203     C                   KFLD                    soc               3
178600060203     c                   KFLD                    CTG               2
178700060203     C                   KFLD                    KCC
178800060203     C                   KFLD                    KSC
178900060203     C     K08Pas03      KLIST
179000060203     C                   KFLD                    soc
179100060203     c                   KFLD                    CTG
179200060203     C                   KFLD                    KCC
179300060203     C                   KFLD                    KSC
179400060203     C                   KFLD                    PpaDtPar
179500060203     C                   KFLD                    PpaNrPar
179600060203     C                   KFLD                    PpaSerPar
179700120531     C     kuteute       KLIST
179800120531     C                   KFLD                    arbfiliale        3 0
179900120531     C                   KFLD                    arbCUC
180000991102     C*
180100991102     C* Controllo se ci sono distinte chiuse ancora da contabilizzare
180200060529     C  n50              EXSR      CNTDIS
180300991102     C*
180400991102     C                   ENDSR
180500060517     C*----------------------------------------------------*
180600060517     C* Reperimento dati società
180700060517     C*----------------------------------------------------*
180800060517     C     REPSOC        BEGSR
180900060517     C*
181000060517     C                   CALLB     'XSOC'
181100060517     C                   PARM                    TIPXSC            6
181200060517     C                   PARM                    SOCXSC            3
181300060517     C                   PARM                    CDSXSC            9 0
181400060517     C                   PARM                    MODXSC            3
181500060517     C                   PARM      *blanks       RTNXSC            1
181600060517     C                   PARM                    XSOCDS
181700060517     C                   PARM                    KPJBA
181800060517     C*
181900060517     C                   ENDSR
182000021115     c**********************************************************************
182100021115     C     SRY4O         BEGSR
182200021115     C*  CARICO I CONTI DA GENERARE  X FILIALE
182300021115     C                   MOVEL     'Y4O'         tbecod
182400060203     C                   MOVEl(p)  soc           tbeke1
182500021118     C                   MOVEL(p)  vfil          tbeke2
182600021118     C     ktbe1         CHAIN     Tntbe01l
182700021118     c                   if        %found(tntbe01l)
182800021118     C                   MOVEL     TBeUNI        Dy4O
182900021118     c                   else
183000021118     c                   clear                   dy4o
183100021118     c                   end
183200021115     C                   ENDSR
183300060202     C*-------------------------------------------------------
183400060202     ** GESTIONE CONTANTI
183500060202     **-------------------------------------------------------
183600060202     C     srtagli       BEGSR
183700060322     c                   clear                   cnc0wa00s
183800060322     c                   eval      £DIVISA ='EUR'
183900060322     c                   eval      kpjbu = cnc0wa00s
184000060322     c                   call      'CNC0WA00R'
184100060322     c                   parm                    kpjba
184200060322     c                   eval      cnc0wa00s = kpjbu
184300060322     c                   if        £RTNCMD = F6 and
184400060328     c                             £RTNCODE ='DONE'
184500060322     c                   eval      casconv = £IMPIMMESSO
184600060322     c                   end
184700060322     C                   ENDSR
184800060202      *
184900021211     OQSYSPRT   E            TESARB           02
185000021211     O                       RSUT                20
185100030318     O                                         + 10 'QUADRATURA GIORNALIERA INC'
185200030318     O                                         +  0 'ASSI E VERSAMENTI'
185300090410     O                       GMAUDt            + 13 '  /  /    '
185400021211     O                       TIME              +  1 '  :  :  '
185500021211     O                                         + 10 'CNC0W1R'
185600021211     O                       PAGE          Z    132
185700021211     O                                          128 'PAG.'
185800021211     O          E            TESARB         1 04
185900021211     O                                         + 30 'ELENCO DISTINTE AUTOTRAS'
186000021211     O                                         +  0 'PORTATORE CHIUSE E NON C'
186100021211     O                                         +  0 'ONTABILIZZATE'
186200021211     O          E            DETARB         1
186300021211     O                                         +  1 'OPERATORE'
186400021211     O                       ARBCUC        Z   +  1
186500120531     O                       NOMUTE            +  1
186600021211     O                                         +  1 'AUTOTRASP.'
186700021211     O                       ARBPDC        Z   +  1
186800021211     O                                         +  1 'NR.DISTINTA'
186900021211     O                       ARBNDC        Z   +  1
187000021211     O                                         +  1 'DEL'
187100021211     O                       GMADDC        8   +  1
187200021211     O          E            FINEST      2
187300021211     O                                         + 40 '** - F I N E    S T A M'
187400021211     O                                         +  1 'P A - **'
187500900606      *----------------------------------------------------------------
187600021118** ERR
187700060411La cass.di servizio ha un importo > di quello ammesso. Fare mandatino
187800021115ATT.NE : Distinte chiuse non contabilizzate
187900021119ATT.NE : Quadradura giornaliera già confermata
188000030318Non è possibile eseguire la quadratura. Attendere qualche minuto
188100031023ATT.NE : la precedente chiusura è terminata in modo anomalo
188200030318Ora si può premere F6 per confermare
188300060508ATT.NE : l'importo della DIFFERENZA non può essere negativo
188400060324ATT.NE : Ci sono assegni che non sono stati resi versabili
188500060327** CMDD
188600020925CHKOBJ OBJ(FNMOV00T) OBJTYPE(*FILE) MBR(M000000)
188700020925ADDPFM FILE(FNMOV00T)               MBR(M000000)
188800020925OVRDBF FILE(FNMOV00T)               MBR(M000000)
188900020925ALCOBJ OBJ((FNMOV00T *FILE *EXCL        M000000))
189000020925DLCOBJ OBJ((FNMOV00T *FILE *EXCL        M000000))
189100021211**  CMDP
189200120725OVRPRTF FILE(QSYSPRT) USRDTA(CARICO_MAN) SAVE(*YES)

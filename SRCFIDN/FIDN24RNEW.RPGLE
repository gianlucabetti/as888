000100980506      *------------------------------------------------------------------------*
000200981027      * GESTIONE COLLI RITROVATI                                               *
000300980506      *------------------------------------------------------------------------*
000400980506     H DECEDIT('0,') DATEDIT(*DMY.)
000500981201      ****************************************************************
000600981201      *  FASI C.A. FISSATE A PROGRAMMA
000700981201      ****************************************************************
000800981201      *    900 = Chiusura CA         (WCHCA)
000900981201      ****************************************************************
001000981201      *  CAUSALE CHIUSURA  FISSATA A PROGRAMMA
001100981201      *_______________________________________________________________
001200981201      *     03 = Pareggio            (WCCH)
001300981201      ***************************************************************
001400981201      *
001500981027     Ffidn24D   CF   E             WORKSTN  sfile(fi24s01:nrr1)
001600981027     f                                      sfile(fi24s02:nrr2)
001700981022     Ffndfa01L  IF   E           k DISK
001800981028     Ffndct01l  uf   E           k DISK
001900981201     Ffndct02l  if   E           k DISK    rename(fndct000:fndct2)
002000981027     Ffndcd03l  if   E           k DISK
002100981028     Ffndcd02l  uf   E           k DISK    rename(fndcd000:fndcd2)
002200981201     Ffndcd01l  uf   E           k DISK    rename(fndcd000:fndcd1)
002300981029     Ffndcs01l  if A E           k DISK
002400981201     Ffndcf01l  O  A E           k DISK
002500040802     Ffndck01l  if   E           k DISK
002600170427     Ffnblp01l  iF   E           k DISK    prefix(arb:3)
002700991115     Ffiar601l  iF   E           k DISK
002800990202     Ffnarb01l  iF   E           k DISK
002900981027     Fazorg01l  if   E           k DISK
003000981022     Ftabel00f  if   E           k DISK
003100981022      ****************************************************************
003200981022      *  RIEPILOGO INDICATORI
003300981022      ***************************************************************
003400981029      * 05   - Sproteggo P.O. IN GESTIONE
003500981028      * 06   - non abilito il tasto funzionale F18
003600981218      * 11   - non viene gestita la chiusura ma solo la fase di analisi colli
003700981027      * 20/21- Gestione 1° subfile
003800981022      * 28   - ERRORE GENERICO
003900981022      * 29   - RIEMISSIONE FORMATO VIDEO
004000981028      * 30/32- COMODO
004100981029      * 39/44- Errori nella seconda videata
004200981027      * 50/51- Gestione 2° subfile
004300990122      * 81   - Campo Reverse Immage
004400981218      * 83   - Posizionamento Cursore Errore
004500981030      * 90   - COMODO
004600981030      * 91   - Lettura subfile
004700981022      *--------------------------------------------
004800981022     D                SDS
004900981022     D  VTCPGM                 1     10
005000981022      *
005100981028     D fidn00ds      E DS
005200981028     D fidn10ds      E DS
005300981029     D fnlv50ds      E DS
005400981028     D TRUL06DS      E DS
005500981028     D  LIN                    1     90  0    DIM(30)                           P.O. COMODO
005600981022      * Ds passaggio parametri a pgm ricerca tabelle
005700981028     D TIBS02DS      E DS
005800981022
005900981022      *--------------------------------------------
006000981022     D KPJBA         E DS
006100981022     D CNCR80        E DS
006200981027     D DTAD          E DS
006300981230     D DDCT01        E DS
006400070403     d og148         e ds
006500070403     d dslr02        e ds
006600170427      * - Parametri x Controllo profilo utenti
006700170427     d TIBS34DS      e ds                  inz
006800170427      * - Ds di riferimento al file esterno AZUTE00F
006900170427     d AZUTEds       e ds                  extname(AZUTE00F)
007000170427      * - Ds per dati organigramma
007100170427     d DDatiUte      e ds
007200170427      *
007300981022      *--------------------------------------------
007400040802     D chisono         s              1
007500981022     D dateu           s              8  0
007600981029     D oramin          S              4  0
007700981022     D W0140           S             14  0
007800981022     D WDTGIO          S              8  0
007900981022     D WORA            S              6  0
008000981201     D prima           S              1
008100981028     D Wf03            S              1
008200981029     D W007a           S              7                                         * campo alfanumerico
008300981029     D W002a           S              2                                         * campo alfanumerico
008400981029     D X               S              1  0                                      * numeratore
008500981029     D wafls           s              3
008600981029     D wanrs           s              2
008700981029     D wAnsc           S              7
008800981029     D wadfv           s              4
008900981029     D wagio           s              2
009000981029     D wames           s              2
009100981029     D wafgs           s              3
009200981029     D wanps           s              2
009300981201     D Wchca           S                   like(dctfca)
009400981201     d                                     inz(900)
009500981201     D Wcch            S                   like(dctcch)
009600981201     d                                     inz('03')
009700981028     D WTPB            S                   like(I00TPB)
009800981028     D Wdch            S                   like(dcddch)
009900981028     D Wnco            S                   like(dctncn)
010000981028     D Wrknco          S                   like(dctncn)
010100981028     D WDatac          S              8  0
010200990309     D W1hpor          S                   like(v1hpor)
010300981028     D kaas            s                   like(dctaas)
010400981028     D klnp            s                   like(dctlnp)
010500981028     D knrs            s                   like(dctnrs)
010600981028     D knsp            s                   like(dctnsp)
010700981027     D kaac            s                   like(dcdaac)
010800981027     D kfil            s                   like(dcdfil)
010900981027     D knca            s                   like(dcdnca)
011000981022     D §cod            S                   LIKE(TBLcod)
011100981022     D §key            S                   LIKE(TBLkey)
011200991116     D trcar6          S                   LIKE(ar6trc)
011300981201     D kfar            S                   LIKE(DCDfar)
011400981027     d                                     INZ('S')
011500981029     d prgnot          S                   LIKE(dcspno)                         *progressivo note
011600981027     D nrr1            S              4  0
011700981027     D nrr2            S              4  0
011800981028     D DATA_eur        S               D   DATFMT(*eur)
011900070403     d wfilgeo         s              1    inz(*off)
012000981022      *
012100981028     d dsnumca         ds
012200981028     d  aac                    1      4  0
012300981028     d  fil                    5      7  0
012400981028     d  nca                    8     14  0
012500981022     D WLBDAT          DS                  INZ
012600981022     D  G02DAT                 1      8  0
012700981022     D  G02INV                 9     16  0
012800981022     D  G02ERR                17     17
012900981022     D  G02TGI                18     22  0
013000981022     D VIDIVA          DS
013100981022     D  w2civa                 1     14
013200981022     D  w2cive                15     16
013300981027      *  titolo videata (lunghezza massima 34)
013400981029     D TIT             C                   CONST('***  GESTIONE COLLI RITROVATI-
013500981029     D                                       ***')
013600981027      *  Descrizione opzioni (lunghezza massima 78)
013700981027     D DOPZ            C                   CONST('2=Manutenzione   4=Note in St-
013800981029     D                                     ampa   5=Visual. C.A.   8=Note C.A. -
013900981029     D                                      G=Gestione')
014000981027      *  titolo videata (lunghezza massima 34)
014100981027     D Kannullata      C                   CONST('   ANNULLATA                 -
014200981027     D                                         ')
014300981028     D Kanalizz        C                   CONST('   ANALIZZATA                -
014400981028     D                                         ')
014500981028      *   S C H I E R E
014600990225     D MSG             S             78    DIM(11) CTDATA PERRCD(1)             MSG VIDEO
014700981028     D L6              S              3  0 DIM(30)                              P.O.GESTITI
014800981027      *------------------------------------------------------------------------*
014900981022      *
015000981022      * ---------------------------------------------------------
015100981022      *
015200981030     c                   if        v1cfgs > 0
015300981030     c* emetto la richiesta del P.O. gestione    --------------------------------------------!
015400981030     c*--------------------------------------------------------------------------------------!
015500981218     c*                  exsr      SUB_FIRST                                                 !
015600981030     c*--------------------------------------------------------------------------------------!
015700981022      * EMISSIONE PRIMA VIDEATA
015800981029    1C                   dow       *inkc = *off
015900981027     c* caricamento dati subfile
016000981028     c                   exsr      SUB_CASF1
016100981027     C*
016200981027      * Se non ho caricato record emetto videata con segnalazione di errore
016300981028    2c                   if        nrr1 = 0
016400981029    3c                   dow       *inkc = *off
016500981027     c                   exfmt     fi24d01
016600981029      * f18=Cambio P.O. in gestione
016700981029     c   ks              eval      *in05 = *on
016800981029     c   ks              iter
016900981029
017000981029      * 05 on --> controllo P.O. in gestione
017100981029     c     *in05         ifeq      *on
017200981029     C                   exsr      SUB_CFGS
017300981029     c   28              iter
017400981029     c                   eval      *in05 = *off
017500981029     c                   leave
017600981029     c                   end
017700981029
017800981028    3c                   enddo
017900981028   x2c                   else                                                   * ci sono CA
018000981027      *
018100981028     c                   exsr      SUB_GESF1
018200981022      * F3=FINE
018300981028    3C                   if        *inkc = *on
018400981027     c                   leave
018500981028    3c                   endif                                                  * INKC
018600981027      *
018700981028    2c                   endif                                                  * NRR1 > 0
018800981027      *
018900981022    1c                   enddo                                                  *fine ciclo KC *on
019000981030      *
019100981030    2c                   endif                                                  * non sono in sede
019200981022      *
019300981028     c                   clear                   fidn10ds
019400981022     c                   movel     'C'           i10tla
019500981028     c                   movel     fidn10ds      kpjbu
019600981022     c                   call      'FIDN10R'
019700981022     c                   parm                    kpjba
019800981029     c*
019900981029     c                   clear                   tibs02ds
020000981029     c                   movel     'C'           t02tla
020100981029     c                   call      'TIBS02R'
020200981029     c                   parm                    kpjba
020300981029     c                   parm                    tibs02ds
020400981022      *
020500981022     C                   EVAL      *INLR = *ON
020600981027      *****************************************************************
020700981027      *   ROUTINE SCRITTURA SUBFILE TESTATE CA AVENTI COLLI RITROVATI
020800981027      *****************************************************************
020900981028     C     SUB_CASF1     BEGSR
021000981027     c* pulisco subfile
021100981027     c                   setoff                                           21
021200981027     c                   write     fi24c01
021300981028     c                   z-add     *zeros        nrr1
021400981027     c                   seton                                            21
021500981029     c*
021600981029     c                   clear                   kaac
021700981029     c                   clear                   kfil
021800981029     c                   clear                   knca
021900981027     c*
022000981027     c* leggo dal dettaglio colli se esistono dei records che hanno il
022100981027     c* flag di collo da analizzare uguale ad 'S'
022200981027     c*
022300981028     c     kfar          setll     fndcd03l
022400981028     c     kfar          reade     fndcd03l                               30
022500981027    1c                   dow       *in30 = *off
022600170427
022700170427     c     kdct          chain(n)  fndct01l
022800170427     c                   if        %found(fndct01l)
022900170427     c                   movel     dctflo        ddct01
023000170427     c                   else
023100170427     c                   clear                   ddct01
023200170427     c                   endif
023300000301     c* i colli devono essere ritrovati da P.O. che è in gestione o dalla sua £6
023400170427     c**                 if        dcdfgs <> v1cfgs
023500000301     c* verifico la sua £6
023600170427     c**   dcdfgs        lookup    l6                                     45
023700170427     c**n45              goto      leggi
023800170427     c**                 endif
023900000301     c*
024000170427     C     DCdfgs        LOOKUP    L6                                     45
024100170427    4c                   if        *in45 and simfel>0
024200170427     c                   setoff                                       32
024300170427     c     DCTlna        lookup    L6                                     32
024400170427    5c                   if        *in32 and d50val_a='N'
024500170427     c                   setoff                                           32
024600170427    5c                   endif
024700170427     c
024800170427     c  n32w§DCTlnpc     lookup    L6                                     32
024900170427    5c                   if        *in32 and d50val_p='N'
025000170427     c                   setoff                                       32
025100170427    5c                   endif
025200170427    5c                   if        not *in32
025300170427     c                   setoff                                       45
025400170427    5c                   endif
025500170427    4c                   endif
025600170427
025700170427     c  n45              goto      leggi
025800170427     c
025900000301    2c*****************+ if        dcdfgs = v1cfgs
026000981027     c* carico una sola c.a. anche se ritrovati + colli per la stessa
026100981028    3c                   if        knca <> dcdnca or
026200981027     c                             kaac <> dcdaac or
026300981027     c                             kfil <> dcdfil
026400981027     c*
026500981027     c                   eval      knca = dcdnca
026600981027     c                   eval      kaac = dcdaac
026700981027     c                   eval      kfil = dcdfil
026800981027     c*
026900981230     c*
027000981230     c                   movel     §dctport      v1hpor
027100981230     c*
027200981201     c* se CA chiusa non do la possibilità di chiudere di nuovo    accendo l'11
027300981201     c                   if        dctdch > 0
027400981201     c                   eval      v1hi11 = 'S'
027500990225     c                   eval      v1hchi = 'S'
027600981201     c                   else
027700981201     c                   eval      v1hi11 = ' '
027800990225     c                   eval      v1hchi = ' '
027900981201     c                   endif
028000981218      * Cerco in L6 il P.O. gestione fase
028100981218     c                   eval      v1hges   = 'N'
028200990225     C                   if        DCTgfc > *zeros
028300981218     C     DCTgfc        LOOKUP    L6                                     31
028400981218     c   31              eval      v1hges   = 'S'
028500981218     C                   endif
028600990122     c*
028700990122     c                   movel     dctgfc        v1hgfc
028800981218     c* controllo se CA in gestione dal P.O.
028900981201     c*
029000040802      * verifico se sono in arrivo o in partenza
029100040802      *
029200040802     c                   clear                   chisono
029300040802     C     DCTlna        LOOKUP    L6                                     31
029400040802     c   31              eval      chisono  = 'A'
029500040802     c
029600981027     c                   z-add     DCTaac        V1Haac
029700981030     c                   z-add     DCTfca        V1Hfca
029800981027     c                   z-add     DCTaas        V1Haas
029900981027     c                   z-add     DCTlna        V1Hlna
030000981028     c                   z-add     DCTncn        V1Hncn
030100981027     c                   z-add     DCTfil        V1Cfil
030200981027     c                   z-add     DCTnca        V1Cnca
030300981027     c                   z-add     DCTaac        V1Cdca
030400981027     c                   movel     DCTmgc        V1Cdca
030500981027     c     *mdy          move      V1Cdca        DATA_eur
030600981027     c     *dmy          move      DATA_eur      V1Cdca
030700981027     c                   move      DCTaas        V1caas2
030800981027     c                   z-add     DCTlnp        V1Clnp
030900981027     c                   z-add     DCTnrs        V1Cnrs
031000981027     c                   z-add     DCTnsp        V1Cnsp
031100981027     c                   z-add     Dctfca        V1Cfca
031200040802      * se sono l'arrivo valorizzo con il numero spedizione esistente su FNDCK se esiste
031300040802     c                   if        chisono =  'A'
031400040802     c     kdct          chain     fndck01l
031500040802     c                   if        %found(fndck01l)
031600040802     c                   move      DCKaas        V1caas2
031700040802     c                   z-add     DCKaas        V1Haas
031800040802     c                   z-add     DCKlnp        V1Clnp
031900040802     c                   z-add     DCKnrs        V1Cnrs
032000040802     c                   z-add     DCKnsp        V1Cnsp
032100040802     c                   endif
032200040802     c                   endif
032300981027      *
032400981028    4c                   IF        DCTatb <> *blanks
032500981027     c                   movel(P)  Kannullata    V1Dfca
032600981028   x4c                   ELSE
032700981027     c                   clear                   v1dfca
032800981027     c     v1cfca        chain     fndfa01l                           31
032900981027     c  n31              movel     DFAdes        V1Dfca
033000981028    4c                   ENDIF
033100981027      *
033200981027     c                   movel     dcttad        V1Ctad
033300981027      * descrizione
033400981027     C                   MOVEL     'C'           t02mod
033500981027     C                   MOVEL     knsif         t02sif
033600981027     C                   MOVEL     'TAD'         t02cod
033700981027     C                   MOVEL     v1ctad        t02ke1
033800981027      *
033900981027     C                   CALL      'TIBS02R'
034000981027     C                   PARM                    KPJBA
034100981027     C                   PARM                    TIBS02DS
034200981027      *
034300981028    4C                   IF        t02err = *BLANKS
034400981027     C                   movel     T02uni        DTAD
034500981027     C                   movel     §TADdesc      v1dtad
034600981028   x4c                   else
034700981027     c                   clear                   v1dtad
034800981028    4c                   endif
034900981027      *
035000981027      *
035100981027     c                   z-add     DCTnev        V1Cnev
035200981027     c                   z-add     DCTaae        V1Caae
035300981028      * ricerca se bolla partenza o arrivi
035400981028     c                   EXSR      SUB_TPB
035500981028      *
035600990528     c* se tpb non è a blank scrivo il record altrimenti vuol dire che non si trova
035700990528     c* in archivio
035800990528     c                   if        wtpb <> *blanks
035900990528     c*
036000990528     c                   add       1             nrr1
036100981028     c                   move      wtpb          v1htpb
036200981027      *
036300981027     c                   write     fi24s01
036400990528      *
036500990528     c                   endif
036600981027      *
036700981028    3c                   endif
036800981027      *
036900000301    2c**********         endif
037000000301     c     leggi         tag
037100981028      *
037200981028     c     kfar          reade     fndcd03l                               30
037300981027      *
037400981027    1c                   enddo
037500981027     c*
037600981027     c                   endsr
037700981027      *****************************************************************
037800981027      *   ROUTINE GESTIONE PRIMO SUBFILE
037900981027      *****************************************************************
038000981028     C     SUB_GESF1     BEGSR
038100981027     c*
038200981029     c                   seton                                        2021
038300981028     c                   z-add     1             nrr1
038400981027     c* emetto il subfile
038500981028    1c                   dow       not *inkc
038600981029     c                   write     fi24z01
038700981027     c                   exfmt     fi24c01
038800981218     c                   setoff                                       9028
038900981029      * f18=Cambio P.O. in gestione
039000981029     c   ks              eval      *in05 = *on
039100981029     c   ks              iter
039200981029
039300981029      * 05 on --> controllo P.O. in gestione
039400981029     c     *in05         ifeq      *on
039500981029     C                   exsr      SUB_CFGS
039600981029     c   28              iter
039700981029     c                   eval      *in05 = *off
039800981029     c                   leave
039900981029     c                   end
040000981027     c* KC = fine lavoro
040100981028    2c                   if        not *inkc
040200981030     c                   setoff                                       9091
040300981028     c* LETTURA SUBFILE
040400981030    3c                   dow       *in91 = *off
040500981218     c                   setoff                                       818328
040600981030     c                   readc     fi24s01                                91
040700981028     c* valorizzo dsnunca
040800981028     c                   eval      aac = v1haac
040900981028     c                   eval      fil = v1cfil
041000981028     c                   eval      nca = v1cnca
041100981028     c*
041200981028     c                   select
041300981028     c* fine file oppure è stato dato un F3 in qualche pgm chiamato
041400981030     c                   WHEN      *in91 or *in90
041500981028     c*
041600981028     c* OPZIONE  "2" Manutenzione C.A.
041700981218
041800990225     c                   WHEN      v1csce = '2'  and  v1hges = 'N'              non gestibile
041900981218     c                   movel     MSG(10)       V1Cmsg
042000981218     c                   seton                                        288390
042100990225
042200990225     c                   WHEN      v1csce = '2'  and  v1hchi = 'S'              non gestibile
042300990225     c                   movel     MSG(11)       V1Cmsg
042400990225     c                   seton                                        288390
042500981218
042600981218     c                   WHEN      v1csce = '2'  and  v1hges = 'S'
042700981028     c                   exsr      SUB_DS00                                     * car ds fidn00ds
042800981028     c                   movel     'M'           I00mod
042900981028     c                   movel     fidn00ds      kpjbu
043000981028     c                   CALL      'FIDN03R'
043100981028     c                   parm                    kpjba
043200981028     c                   movel     kpjbu         fidn00ds
043300981028     c                   eval      *in90 = (o00f03 = 'S')
043400981028     c* OPZIONE  "4" Note per stampa
043500981028     c                   WHEN      V1csce = '4'
043600981028     c                   exsr      SUB_STNOTE
043700981028     c                   eval      *in90 = (WF03 = 'S')
043800981028     c* OPZIONE  "5" Visualizza   C.A.
043900981028     c                   WHEN      v1csce = '5'
044000981030     c                   exsr      SUB_DS00                                     * car ds fidn00ds
044100981028     c                   movel     'I'           I00mod
044200981028     c                   movel     fidn00ds      kpjbu
044300981030     c                   CALL      'FIDN01R'
044400981028     c                   parm                    kpjba
044500981028     c                   movel     kpjbu         fidn00ds
044600981028     c                   eval      *in90 = (o00f03 = 'S')
044700981028     c* OPZIONE  "8" Note C.A.
044800981028     c                   WHEN      V1Csce = '8'
044900981028     c                   exsr      SUB_NOTE
045000981028     c                   eval      *in90 = (WF03 = 'S')
045100981028     c* OPZIONE  "G" Gestione Ritrovamento Colli
045200990122     c                   WHEN      V1Csce = 'G' and v1dfca <> kanalizz
045300981028     c                   exsr      SUB_GERC
045400981028    4c                   if        *inkf = *on                                  * ca analizzata
045500990122     c                   seton                                        8190      * reverse immage
045600981028     c                   movel     kanalizz      v1dfca
045700981030     c                   clear                   v1cfca
045800990225     c                   movel     'S'           v1hchi
045900981028    4c                   endif
046000981028     c*
046100981028     c                   ENDSL
046200981028     c* aggiorno il record subfile
046300981028     c                   clear                   v1csce
046400981028     c*
046500981030     c  n91              update    fi24s01
046600981028     c*
046700981028    3c  n90              enddo                                                  * lettura subfile
046800981028     c*
046900981028    2c                   endif                                                  * not *inkc
047000981028     c*
047100981028    1c                   enddo                                                  * ciclo
047200981028     c*
047300981028     c                   endsr
047400981028      **********************************************************************
047500981028      * GESTIONE RITROVAMENTO COLLO         2° VIDEATA
047600981028      **********************************************************************
047700981028     C     SUB_GERC      BEGSR
047800981028      *
047900981028     c* routine di caricamento seconda videata
048000981028     c                   exsr      SUB_CAF2
048100981028     c*
048200981028     c* routine di gestione seconda videata
048300981028     c                   exsr      SUB_GEF2
048400981028     c*
048500981028     c                   endsr
048600981028      **********************************************************************
048700981028      * CARICAMENTO VIDEATA GESTIONE COLLI RITROVATI     2° videata
048800981028      **********************************************************************
048900981028     C     SUB_CAF2      BEGSR
049000981028      *
049100981028      * DATI CA
049200981028     c                   eval      v2cfil = v1cfil                              * P.O. apertura CA
049300981028     c                   eval      v2cnca = v1cnca                              * Numero CA
049400981029     c                   movel     v1cdca        v2cdca                         * Data CA
049500981030     c                   move      v1haac        v2cdca
049600981028     c                   eval      v2ctad = v1ctad                              * tipo anomalia
049700981028     c                   movel     v1dtad        v2dtad                         * tipo anomalia des
049800981028     c                   eval      v2clnp = v1clnp                              * linea partenza
049900981028     c                   eval      v2cnrs = v1cnrs                              * numero serie
050000981028     c                   eval      v2cnsp = v1cnsp                              * numero spedizione
050100981028     c                   eval      v2cncn = v1hncn                              * num.colli dannegg.
050200990122     c                   eval      v2cfca = v1hfca                              * fase CA
050300990122     c                   movel     v1dfca        v2dfca                         * decodifica fase CA
050400990122     c                   eval      v2cgfc = v1hgfc                              * P.O. gestione CA
050500990122     c* decodifica p.o. gestione fase ca
050600990122     c     v2cgfc        chain     azorg                              30
050700990122     C  N30              if        orgde5 <> *BLANKS
050800990122     C                   movel     orgde5        v2dgfc
050900990122     C                   else
051000990122     c                   movel     orgdes        v2dgfc
051100990122     c                   endif
051200981028     c*
051300981028     c                   z-add     V1Haas        Kaas
051400981028     c                   z-add     V1Clnp        Klnp
051500981028     c                   z-add     V1Cnrs        Knrs
051600981028     c                   z-add     V1Cnsp        Knsp
051700981028     c* se è partenza vado in blp
051800981028     c                   if        v1htpb  = 'P'
051900981028     c     kspe          chain     fnblp000                           30
052000981028     c* se è arrivo vado in arb
052100981028     c                   else
052200981201     c     kspe          chain(n)  fnarb000                           30
052300981028     c                   endif
052400981028     c*
052500981029     c                   movel     arbaas        v2cdsp
052600981029     c                   move      arbmgs        v2cdsp
052700981029     c     *ymd          move      V2Cdsp        DATA_eur
052800981029     c     *eur          move      DATA_eur      v2cdsp                         * data spedizione
052900981028     c*
053000990528     c                   eval      v2clna=v1hlna                                * linea arrivo
053100981028     c* decodifica
053200981028     c     v2clna        chain     azorg                              30
053300981028     C  N30              if        orgde5 <> *BLANKS
053400981028     C                   movel     orgde5        v2dlna
053500981028     C                   else
053600981028     c                   movel     orgdes        v2dlna
053700981028     c                   endif
053800981230     c                   movel     arbnas        VTCnas
053900981201      * decodifico tipo bolla
054000981230     C                   if        v1hpor = 'F'
054100981230     C                   movel(P)  'FRANCO'      VTDcbo
054200981230     C                   else
054300981230     C                   movel(P)  'ASSEGNATO'   VTDcbo
054400981230     C                   endif
054500990309     c*
054600990309     c                   SELECT
054700990309     c*
054800990309    1c                   when      arbcca <> '9'
054900990309     c                   eval      W1hpor  = v1hpor
055000990309     c*
055100990309    2c                   when      v1hpor = 'F'
055200990309     c                   eval      W1hpor  = 'A'
055300990309     c*
055400990309    2c                   when      v1hpor   = 'A'
055500990309     c                   eval      W1hpor  = 'F'
055600990309     c*
055700990309    2c                   endsl
055800981201      *
055900981201      *  Se assegnato in partenza verifico il codice destinatario
056000991115     c*!*                clear                   DSBL4A
056100991115     c                   clear                   ar6ksc
056200990309     c                   IF        v1htpb  = 'P' and  w1hpor  = 'A'
056300991115     c*!*  Kbl4          CHAIN     FNBL4000                           30
056400991115     c*!*N30              movel     BL4NOT        DSBL4A
056500991116     c                   eval      trcar6 = '1'
056600991116     c     KAR6          CHAIN     FIAR6000                           30
056700981201     c                   ENDIF
056800981201      *
056900990309     c                   IF        w1hpor = 'A'
057000981028     c                   move      arbccm        VTCksm
057100991115     c*!*                if        §B4ksf <> *blanks
057200991115     c*!*                movel     §B4ksf        VTCksd
057300991115     c                   if        AR6ksc <> *zeros
057400991115     c                   movel     AR6ksc        VTCksd
057500981201     c                   else
057600981201     c                   movel     arbksc        VTCksd
057700981201     c                   endif
057800981028     c                   ELSE
057900981028     c                   move      arbksc        VTCksm
058000981028     c                   move      *zeros        VTCksd
058100981028     c                   ENDIF
058200981028
058300981028      * DATI BOLLA
058400981028     c* Mittente Destinatario
058500981028     c                   movel     arbrsm        VTCrsm
058600981028     c                   move      arbcam        VTCcam
058700981028     c                   movel     arblom        VTClom
058800981028     c                   movel     arbprm        VTCprm
058900981028     c                   movel     arbnzm        VTCnam
059000981028     c                   movel     arbrsd        VTCrsd
059100981028     c                   move      arbcad        VTCcad
059200981028     c                   movel     arblod        VTClod
059300981028     c                   movel     arbprd        VTCprd
059400981028     c                   movel     arbnzd        VTCnad
059500981028     c* colli peso volume
059600981028     c                   z-add     arbncl        VTCncl
059700981028     c                   z-add     arbpkf        VTCpkf
059800981028     c                   movel     arbfvf        VTCfvf
059900981028     c                   z-add     arbvlf        VTCvlf
060000981028      * DATI CHIUSURA COLLI / CA
060100981028     c                   clear                   v2ccca
060200981028     c                   clear                   v2ccco
060300981028     c                   clear                   v2ccch
060400981028     c                   clear                   v2cdch
060500981201      * se CA chiusa accendo l'11 proteggo i xcampi di chiusura in quanto la CA è già chiusa
060600981201      * ma do la possibilità di dare come analizzati i segnacolli ancora da ANALIZZARE
060700981201     C                   EVAL      *in11 = (v1hi11 = 'S')
060800981218      * se CA non in gestione da me proteggo i campi di chiusura
060900981218      * ma do la possibilità di dare come analizzati i segnacolli ancora da ANALIZZARE
061000981028      * DATI COLLI RITROVATI
061100981218     C  n11              EVAL      *in11 = (v1hges = 'N')
061200981218      *
061300981028     c                   exsr      SUB_CASF2
061400981028      *
061500981028     c                   endsr
061600981028      *****************************************************************
061700981028      *   ROUTINE SCRITTURA SUBFILE COLLI RITROVATI
061800981028      *****************************************************************
061900981028     C     SUB_CASF2     BEGSR
062000981028     c* pulisco subfile
062100981028     c                   setoff                                           51
062200981028     c                   write     fi24c02
062300981028     c                   z-add     *zeros        nrr2
062400981028     c                   seton                                            51
062500981028     c* preparo la chiave
062600981028     c                   z-add     V1haac        Kaac
062700981028     c                   z-add     V2Cfil        Kfil
062800981028     c                   z-add     V2Cnca        Knca
062900981028     c*
063000981028     c* leggo dettaglio colli della C.A. in gestione
063100981028     c* deve avere flag da analizzare uguale ad 'S'
063200981028     c* deve essere stato ritrovato dal P.O. di gestione o dalla sua £6
063300981028     c     kdcd3         setll     fndcd03l
063400981028     c     kdcd3         reade     fndcd03l                               31
063500981028     c*
063600981028    1c                   dow       *in31 = *off
063700981028     c* controllo il P.O. che ha ritrovato il collo
063800000301    2c************       if        dcdfgs = v1cfgs
063900000301     c                   if        dcdfgs <> v1cfgs
064000000301     c* verifico la sua £6
064100000301     c     dcdfgs        lookup    l6                                     45
064200000301     c  n45              goto      leggi2
064300000301     c                   endif
064400000301     c*
064500981028     c*
064600981028     c                   clear                   v2ssel
064700981028     c                   eval      v2sfls = dcdfls
064800981028     c                   eval      v2snsc = dcdnsc
064900981028     c                   eval      v2sfgs = dcdfgs
065000050314      * campi utili per la chiave
065100050314     c                   eval      v2hlna = dcdlna
065200050314     c                   eval      v2hnrs = dcdnrs
065300050314
065400981028     c     *ymd          move      dcddfv        DATA_eur
065500981029     c     *eur          move      DATA_eur      V2sdfv
065600981028     c                   eval      v2snps = dcdnps
065700981028     c*
065800981028     c                   add       1             nrr2
065900981028      *
066000981028     c                   write     fi24s02
066100981028      *
066200000301    2c***************    endif
066300981028      *
066400000301     c     leggi2        tag
066500981028     c     kdcd3         reade     fndcd03l                               31
066600981028      *
066700981028    1c                   enddo
066800981028     c*
066900981028     c                   endsr
067000981028      *****************************************************************
067100981028      *   ROUTINE GESTIONE VIDEATA COOLI RITROVATI
067200981028      *****************************************************************
067300981028     C     SUB_GEF2      BEGSR
067400981028     c*
067500981028    1c                   if        nrr2 > 0
067600981028     c                   seton                                        50
067700981028     c                   z-add     1             nrr1
067800981028    1c                   endif
067900981028     c                   clear                   prima
068000981028     c                   clear                   wnco
068100981028     c*
068200981028    1c                   do        *hival
068300981029     c*
068400981029     c                   write     fi24z02
068500981028     c* emissione
068600981028     c                   exfmt     fi24c02
068700981104     c                   setoff                                       40  42
068800981028     c                   setoff                                       434428
068900981028     c* controllo le funzioni
069000981028     c                   select
069100981028     c* F12 Videata precedente
069200981028     c                   WHEN      *INKL
069300981028     c                   leave
069400981028     c* F18 Gestione Note CA
069500981028     c                   WHEN      *INKS
069600981028     c                   exsr      SUB_NOTE
069700981028     c*
069800981028     c                   endsl
069900981028     c* controllo del formato
070000981028     c                   exsr      SUB_CTR02
070100981028     c* se non ci sono errori e  CMD 6 confermo i dati
070200981028    2c                   if        *in28 = *off and *inkf = *on
070300981028     C                   exsr      SUB_CONFE
070400981028     c                   leave
070500981028    2c                   endif
070600981028    1c                   enddo
070700981028     c*
070800981028     c                   ENDSR
070900981028      **********************************************************************
071000981028      * CONTROLLO SECONDA VIDEATA
071100981028      **********************************************************************
071200981028     C     SUB_CTR02     BEGSR
071300981028      *
071400981028     c* chiusura C.A.
071500981028    1c                   if        v2ccca = 'SI'
071600981028     c* chiusura colli
071700981028     c                             and v2ccco = 'SI'
071800981028     c* errore
071900981028     c                   movel     msg(1)        v2cmsg
072000981028     c                   seton                                        4028
072100981028     c                   goto      ectr2
072200981028    1c                   endif
072300981028      *
072400981028     c* chiusura colli
072500981028    1c                   if        v2ccco = 'SI'
072600981028     c* controllo se è stata inserita la causale chiusura
072700981028    2c                   if        v2ccch = *blank
072800981028     c* errore
072900981028     c                   movel     msg(2)        v2cmsg
073000981028     c                   seton                                        4228
073100981028     c                   goto      ectr2
073200981028     c*
073300981028   x2c                   else
073400981028     c* controllo se è stata fatta una richiesta di ricerca in tabella
073500981028     c     '?'           scan      v2ccch                                 32
073600981028      *
073700981028    3C                   IF        *IN32 = *ON
073800981028     c                   clear                   tibs02ds
073900981028     c                   movel     'R'           t02mod
074000981028     c                   movel     knsif         t02sif
074100981028     C                   movel     'CCH'         t02cod
074200981028      *
074300981028     C                   CALL      'TIBS02R'
074400981028     c                   parm                    KPJBA
074500981028     c                   parm                    tibs02ds
074600981028      *
074700981028    4c                   if        t02err =  *blanks
074800981028     C                   MOVEL     t02ke1        v2ccch
074900981030     c                   else
075000981030     c                   movel     t02msg        v2cmsg
075100981030     c                   seton                                        4228
075200981030     c                   goto      ectr2
075300981028    4c                   endif
075400981028      *
075500990302   x3C                   Endif
075600981028      *   Controlli di validità'
075700981028     c                   clear                   tibs02ds
075800981028     C                   MOVEL     'C'           t02mod
075900981028     C                   MOVEL     knsif         t02sif
076000981028     C                   MOVEL     'CCH'         t02cod
076100981028     C                   MOVEL(P)  v2ccch        t02ke1
076200981028      *
076300981028     C                   CALL      'TIBS02R'
076400981028     C                   PARM                    KPJBA
076500981028     C                   PARM                    tibs02ds
076600981028      *
076700981028    4C                   if        t02err <> *BLANKS
076800981028     c                   movel     msg(3)        v2cmsg
076900981028     c                   seton                                        4228
077000981028     c                   goto      ectr2
077100981028    4C                   endif
077200981028    3C                   endif
077300981028     c*
077400981028     c* controllo la data di chiusura colli
077500981028    2C                   if        v2cdch <> 0
077600981028     C                   move      v2cdch        g02dat
077700981028     C                   eval      g02err = *blanks
077800981028     C                   call      'XSRDA8'
077900981028     C                   parm                    wlbdat
078000981028    3C                   if        g02err = '1'
078100981028     C                   seton                                        4328
078200981028     C                   movel     msg(4)        v2cmsg
078300981028     c                   goto      ectr2
078400981028   x3C                   else
078500981028     C                   z-add     g02dat        V2cdch
078600981028     C                   move      g02inv        wdch
078700981028    3C                   endif
078800981028   x2C                   else
078900981028     C                   seton                                        2843
079000981028     C                   movel     msg(5)        v2cmsg
079100981028     c                   goto      ectr2
079200981028    2C                   ENDIF
079300981028      * controllo che la data fase sia maggiore della data apertura C.A.
079400981030     c                   move      V2Cdca        g02dat
079500981030     c                   clear                   g02err
079600981030     c                   call      'XSRDA8'
079700981030     c                   parm                    wlbdat
079800981030     c                   movel     g02inv        wdatac
079900981030     c*
080000981028    2c                   if        *in28 = *off and
080100981028     c                             wdatac > wdch
080200981028     C                   seton                                        2843
080300981028     C                   movel     msg(6)        v2cmsg
080400981028     c                   goto      ectr2
080500981028    2C                   Endif
080600981028      * controllo che la data fase sia minore o uguale ad oggi
080700981028    2c                   if        *in28 = *off and
080800981028     c                             dateu < wdch
080900981028     C                   seton                                        2843
081000981028     C                   movel     msg(7)        v2cmsg
081100981028     c                   goto      ectr2
081200981028    2C                   Endif
081300981028     c* controllo che sia stato selezionato almeno un collo ritrovato
081400981028     c                   exsr      SUB_COLLI
081500981028     c* controllo il numero dei colli selezionati sia > 0
081600981028    2c                   if        wnco = 0
081700981028     c* errore
081800981028     c                   movel     msg(8)        v2cmsg
081900981104     c                   seton                                        4428
082000981104     c     1             chain     fi24s02                            91
082100981104     c  n91              update    fi24s02
082200981028     c                   goto      ectr2
082300981028    2c                   endif
082400981028     c* controllo che il totale numero colli selezionati non sia = al TOT colli mancanti della
082500981028     c* CA .. perchè a questo punto conviene chiudere la CA
082600981104     c* il messaggio di errore è forzabile (E' meglio di no xchè quando richiamo FIDN03 non riesco
082700981104     c* a confermare la CA con colli a zero e non ho neppure la possibilità di tornare indietro)
082800981104     c* 4/11/98
082900981104    2c*********          if        prima = ' '  and                             *********
083000981104     c*       *                    wnco = v2cncn                                *       *
083100981104     c*       *          eval      prima = '1'                                  *       *
083200981104     c*********                                                                 *********
083300981104     c                   if        wnco = v2cncn
083400981028     c                   movel     msg(9)        v2cmsg
083500981104     c                   seton                                        4428
083600981028     c                   goto      ectr2
083700981028    2c                   endif
083800981028     c*
083900981028    1C                   endif
084000981028     c*
084100981028     c     ectr2         ENDSR
084200981028      *****************************************************************
084300981028      *   ROUTINE LETTURA COLLI SELEZIONATI PER CHIUSURA COLLI
084400981028      *****************************************************************
084500981028     C     SUB_COLLI     BEGSR
084600981028     c*
084700981028     c                   clear                   wrknco
084800981028     c                   z-add     1             nrr2
084900981028     c*
085000981028     c                   do        *hival
085100981028     c     nrr2          chain     fi24s02                            33
085200981028     c                   if        not *in33
085300981028     c* controllo se è stato selezionato
085400981028     c                   if        v2ssel = 'X'
085500981028     c                   add       1             wrknco
085600981028     c                   endif
085700981028     c                   add       1             nrr2
085800981028     c                   endif
085900981028     c  n33              enddo
086000981028     c*
086100981028     c* se il numero di colli calcolato è differente dal numero colli calcolato nella precedente
086200981028     c* lettura valorizzo il campo e pulisco "PRIMA" per controllo
086300981028     c                   if        wnco <> wrknco
086400981028     c                   eval      wnco = wrknco
086500981028     c                   eval      prima= ' '
086600981028     c                   endif
086700981028     c*
086800981028     c                   endsr
086900981028      **********************************************************************
087000981028      * CONFERMA CHIUSURA
087100981028      **********************************************************************
087200981028     C     SUB_CONFE     BEGSR
087300981028      * CHIUSURA
087400981028     C*           chiusura CA
087500981028     c                   if        v2ccca = 'SI'
087600981201     c* eseguo aggiornamenti nei file CA per la chiusura
087700981201     c                   EXSR      SUB_CHCA
087800990414     c                   else
087900990414     c* altrimenti se chiusura singolo collo oppure tolgo il flag del singolo collo
088000981028     c*           controllo e aggiorno il dettaglio colli
088100981028     c                   z-add     1             nrr2
088200981028     c*
088300981028     c                   do        *hival
088400981028     c     nrr2          chain     fi24s02                            33
088500981028     c                   if        not *in33
088600981028     c* aggancio il file dettaglio colli
088700981028     c     kdcd2         chain     fndcd02l                           34
088800981028     c* controllo se : è stato dato CMD 6 metto causale + data chiusura nel dettaglio collo
088900981028     c                   if        v2ssel = 'X'
089000990302     c* metto data chiusura e cauasale di chiusura solo  se valorizzati
089100990302     c                   if        wdch > 0
089200981028     c                   eval      dcddch = wdch
089300990302     c*
089400981028     c                   eval      dcdcch = v2ccch
089500981028     c                   endif
089600990525     c*
089700990524     c                   endif
089800990525     c* se ho in gestione la CA  sflaggo il record
089900990525     c*
090000990525     c                   if        not *in11
090100990525     c                   clear                   dcdftr
090200990525     c                   endif
090300990525     c*
090400981028     c                   clear                   dcdfar
090500981028     c* aggiorno il file dettaglio colli
090600981028     c  n34              update    fndcd2
090700981028     c*
090800981028     c                   add       1             nrr2
090900981028     c                   endif
091000981028     c  n33              enddo
091100990414     c*
091200990414     c                   endif
091300981028     c*
091400981028     c* se chiusura dei singoli colli richiamo il PGM di manutenzione  per sistemare
091500981028     c* il numero totale dei colli danneggiati
091600981028     c                   if        v2ccco = 'SI'
091700981028     c                   exsr      SUB_DS00
091800981028     c                   movel     'C'           I00mod
091900981028     c                   movel     fidn00ds      kpjbu
092000981028     c                   CALL      'FIDN03R'
092100981028     c                   parm                    kpjba
092200981028     c                   movel     kpjbu         fidn00ds
092300981028     c                   endif
092400981028     c
092500981028     c* aggiorno il flag di trasmissione della testata  solo nel casi in cui tolgo il flag di
092600981028     c* collo da analizzare senza chiudere niente
092700990524     c                   if        v2ccca = '  ' and v2ccco = ' ' and
092800990524     c                             not *in11
092900990524     c     kdct          chain     fndct01l                           34
093000990524     c                   clear                   dctft1
093100990524     c                   clear                   dctft2
093200990524     c  n34              update    fndct000
093300990524     c                   endif
093400981028     c*
093500981029     c* confermo le note scritte da questo PGM
093600990107     C*                  CLEAR                   fidn10ds
093700990107     C*                  MOVEL     'C'           I10FLM
093800990107     c*                  movel     'C'           i10tpd
093900990107     c*                  movel     'N'           i10trc
094000990107     c*                  movel(P)  dsnumca       i10nkt
094100990107     c*                  movel     v1cfca        i10nks
094200990107     c*                  movel(P)  fidn10ds      kpjbu
094300990107     C*                  CALL      'FIDN10R'                            99
094400990107     C*                  PARM                    kpjba
094500981029     c*
094600981029     c* cerco l'ultimo progressivo note della fase in corso
094700981029     c* scrittura nelle note dei dati relativi alla CA da mantenere
094800981029     c                   movel     v1cfca        dcsnks
094900981029     c                   eval      dcstrc='N'
095000981029     c                   eval      dcspno=0
095100981029     c*
095200981029     c* scrittura note preparazione dei campi strandard
095300981029     c                   eval      dcsatb=' '
095400981029     c                   eval      dcstpd='C'
095500981029     c                   eval      dcsnkt=dsnumca
095600981029     c                   eval      dcsstd=' '
095700981029     c                   eval      dcsdim=dateu
095800981029     c                   eval      dcshim=oramin
095900981029     c*
096000981029     c* cerco l'ultimo progressivo riga relative alla fase attuale CA se esiste
096100981029     c     keydcs        setgt     fndcs01l
096200981029     c     keydcsp       readpe    fndcs01l                               93
096300981029     c*
096400981029     c                   move      knmus         dcspru
096500981029     c                   movel     simfel        dcspos
096600981029     c                   clear                   dcsstn
096700981029     c                   clear                   dcsft1
096800981029     c                   clear                   dcsdt1
096900981029     c* scrivo 2 righe di note per ogni singolo collo ritrovato
097000981029     c                   z-add     1             nrr2
097100981029     c*
097200981029     c                   do        *hival
097300981029     c     nrr2          chain     fi24s02                            33
097400981029     c                   if        not *in33
097500981029     c* Preparo la prima riga di note
097600981029     c                   add       1             dcspno
097700981029     c                   clear                   dcsnot
097800981029     c* tolgo gli 0 non significativi dal numero segnacollo
097900981029     c                   movel     v2snsc        w007a
098000981029     c     '0'           check     w007a         x
098100981029     c                   eval      wAnsc = %subst(w007a:X)
098200981029     c                   movel     v2sfls        wafls
098300050314     c                   movel     v2hnrs        wanrs
098400981029     c                   movel     v2sdfv        wadfv
098500981029     c                   movel     wadfv         wagio
098600981029     c                   move      wadfv         wames
098700981029     c                   movel     v2sfgs        wafgs
098800981029     c                   movel     v2snps        wanps
098900981029     c                   eval      dcsnot = 'Collo '  + wafls + ' ' + wanrs +
099000981029     c                                      ' ' + wansc + ' ritrovato dal'
099100981029     c*
099200981029     c                   write     fndcs000
099300981029     c* scrivo la seconda riga
099400981029     c                   add       1             dcspno
099500981029     c                   clear                   dcsnot
099600981029     c*
099700981030     c                   eval      dcsnot = 'P.O. '  + wafgs + ' il giorno ' +
099800981029     c                                      wagio + '/' + wames + ' pistola ' +
099900981029     c                                      wanps
100000981029     c*
100100981029     c                   write     fndcs000
100200981029     c*
100300981029     c                   add       1             nrr2
100400981029     c                   endif
100500981029     c  n33              enddo
100600981029     c*
100700981028     c                   endsr
100800981201      *****************************************************************
100900981201      * AGGIORNO ARCHIVI   CHIUSURA C.A.
101000981201      *****************************************************************
101100981201     C     SUB_CHCA      BEGSR
101200981201      *
101300981201      * Imposto campi per trasmissione dati
101400981201     c     kdct          chain     fndct01l                           31
101500981201     c                   z-add     dateu         DCTdch
101600981201     c                   z-add     wchca         DCtfca
101700981201     c                   move      wcch          DCTcch
101800981222     c                   clear                   DCTft1
101900981222     c                   clear                   DCTft2
102000981201      *
102100981201      * Aggiorno testata C.A.
102200981201     c                   EXCEPT    UPDdct
102300981201      *
102400981201      * Aggiorno l dettaglio C.A.
102500981201     c                   EXSR      UPDDCD
102600981201      *
102700981201      * Scrivo fasi C.A.
102800981201     c                   EXSR      WRTDCF
102900070403
103000070403      * filiale gestione con procedura telefonate GEO attiva e LNA della spedizione
103100070403      * in £6 e bolla non in distinta e non consegnata propongo lo sblocco della spedizione
103200070403     c                   if        wfilgeo = *on and chisono = 'A' and
103300070403     c                             arbndc = *zeros and arbdcm = *zeros
103400070403     c                   clear                   dslr02
103500070403     c                   eval      dr2aas = dctaas
103600070403     c                   eval      dr2lnp = dctlnp
103700070403     c                   eval      dr2nrs = dctnrs
103800070403     c                   eval      dr2nsp = dctnsp
103900070403     c                   eval      dr2fgs = v1cfgs
104000070403     c                   eval      dr2ric = '1'
104100070403     c                   eval      kpjbu = dslr02
104200070403     c                   call      'FNLR39R'
104300070403     c                   parm                    kpjba
104400070403     c                   eval      dslr02 = kpjbu
104500070403     c                   endif
104600981201      *
104700981201     c                   ENDSR
104800981201      *****************************************************************
104900981201      * AGGIORNO FNDCD Dettaglio C.A.
105000981201      *****************************************************************
105100981201     C     UPDDCD        BEGSR
105200981201     c*
105300981201     c     kdct          setll     fndcd01l
105400981201     c     kdct          reade     fndcd01l                               31
105500981201     c*
105600981201      * Imposto campi per trasmissione dati e pulisco i dati della chiusura C A
105700981201     c                   dow       *in31 = *off
105800981201     c                   eval      DCddch = dctdch
105900981201     c                   eval      DCdcch = dctcch
106000981222     c                   eval      DCdftr = dctft1
106100990414     c* pulizia del flag di collo da analizzare
106200990414     c                   clear                   dcdfar
106300981201     c                   update    fndcd1
106400981201     c*
106500981201     c     kdct          reade     fndcd01l                               31
106600981201     c*
106700981201     c                   enddo
106800981201     c*
106900981201     c                   endsr
107000981201      *****************************************************************
107100981201      * AGGIORNO FNDCF FASI C.A.
107200981201      *****************************************************************
107300981201     C     WRTDCF        BEGSR
107400981201      *
107500981201     C                   z-add     DCTaaC        DCFaac
107600981201     C                   z-add     DCTfil        DCFfil
107700981201     C                   z-add     DCTnca        DCFnca
107800981201     c                   z-add     dctfca        DCFfca
107900981201     c                   z-add     dateu         DCFdfc
108000990414     c                   movel     wora          DCFhfc
108100981201     c                   z-add     V1Cfgs        DCFfev
108200981201     c                   movel     dctptr        DCFptr
108300981201     c                   movel     knmus         DCFpru
108400981201      *
108500981201     c                   WRITE     FNDCF000
108600981201      *
108700981201     c                   ENDSR
108800981028      **********************************************************************
108900981028      * VISUALIZZO NOTE PER STAMPA
109000981028      **********************************************************************
109100981028     C     SUB_stnote    BEGSR
109200981028      *
109300981028     c*
109400981028     c*
109500981028     C                   clear                   FIDN10DS
109600981028     c     *dmy          move      V1Cdca        DATA_eur
109700981028     c     *eur          move      DATA_eur      i10dta
109800981028     c                   movel     'D'           i10trc
109900981028     c                   movel     'T'           i10std
110000981028     c                   movel     'C'           i10tpd
110100981028      *
110200981028     c                   movel     'V'           i10flm
110300981028     c                   movel     dsnumca       i10nkt
110400981028      *
110500981028     c                   movel(P)  fidn10ds      kpjbu
110600981028     C                   CALL      'FIDN10R'
110700981028     C                   PARM                    kpjba
110800981028      *
110900981028     c                   movel     *blanks       WF03
111000981028     C                   movel     KPJBU         fidn10ds
111100981028     c                   if        o10f03    <> *BLANKS
111200981028     C                   movel     'S'           WF03
111300981028     c                   endif
111400981028      *
111500981028     C                   ENDSR
111600981022      *****************************************************************
111700981022      *   ROUTINE SCRITTURA NOTE
111800981022      *****************************************************************
111900981022     C     SUB_NOTE      BEGSR
112000981022     c* valorizzo la DS di gestione note/descrizioni e chiamo il PGM FIDN10R
112100981028     C                   CLEAR                   fidn10ds
112200981030     c     *dmy          move      V1Cdca        DATA_eur
112300981030     c     *eur          move      DATA_eur      i10dta
112400981022     c                   movel     'N'           i10trc
112500981022     c                   movel     'C'           i10tpd
112600981027     c                   movel     v1cfca        i10nks
112700981022     c                   movel     dsnumca       i10nkt
112800981028     c* se sono in gestione note dal subfile le scrivo subito nel file fndcs
112900981028     c                   if        v1csce <> 'G'
113000981028     c                   movel     'V'           i10flm
113100981028     c                   else
113200981028     c* se sono in gestione videata collo ritrovati scrivo al CMD 6
113300981030     c* se lascio 'M' in modo tale che le note vengano confermate quando viene confermata la
113400981030     c* gestione del ritrovamento colli rischio di perdere le note in quanto diversi programmi
113500981030     c* chiamati richiamano a loro volta il PGM fidn10.
113600981030     c* metto la 'V' di manutenzione note potendo così confermare tutte le volte che entro
113700981030     c*                  movel     'M'           i10flm
113800981030     c                   movel     'V'           i10flm
113900981028     c                   endif
114000981028     c*
114100981028     c                   movel(P)  fidn10ds      kpjbu
114200981022     C                   CALL      'FIDN10R'                            99
114300981022     C                   PARM                    kpjba
114400981022     c
114500981022     c                   endsr
114600981028      *****************************************************************
114700981028      *   CARICAMENTO DS FIDN00DS
114800981028      *****************************************************************
114900981028     C     SUB_DS00      BEGSR
115000981028     c*
115100981028     c                   clear                   fidn00ds
115200981028     c*
115300981028     c                   z-add     V1hfca        I00fca
115400981028     c                   z-add     V1Cfgs        I00fgs
115500981028     c                   z-add     V1Haac        I00aac
115600981028     c                   z-add     V1Cfil        I00fil
115700981028     c                   z-add     V1Cnca        I00nca
115800981028     c                   z-add     V1Haas        I00aas
115900981028     c                   z-add     V1Clnp        I00lnp
116000981028     c                   z-add     V1Cnrs        I00nrs
116100981028     c                   z-add     V1Cnsp        I00nsp
116200981028     c                   z-add     V1Hlna        I00lna
116300981028     c                   movel     V1Ctad        I00tad
116400981028     c                   movel     v1htpb        I00tpb
116500981028      *
116600981028     C                   endsr
116700981028      **********************************************************************
116800981028      * IMPOSTO TIPO BOLLA  P/A
116900981028      **********************************************************************
117000981028     C     SUB_TPB       BEGSR
117100981028      *
117200981028     c                   movel     *blanks       wtpb
117300981028      *
117400981028     c                   z-add     V1Haas        Kaas
117500981028     c                   z-add     V1Clnp        Klnp
117600981028     c                   z-add     V1Cnrs        Knrs
117700981028     c                   z-add     V1Cnsp        Knsp
117800981028     c     kspe          chain     fnblp000                           30
117900981028      *
118000981028     c                   if        *IN30 = *OFF
118100981028     c                   movel     'P'           wtpb
118200981028     C                   else
118300981028      *
118400981028     c     kspe          chain     fnarb000                           30
118500981028     c  N30              movel     'A'           wtpb
118600981028     c                   endif
118700981028      *
118800981028     C                   ENDSR
118900981030      **********************************************************************
119000981030      * EMISSIONE VIDEATA RICHIESTA P.O. GESTIONE
119100981030      **********************************************************************
119200981030     C     SUB_FIRST     BEGSR
119300981030      *
119400981030     c                   do        *hival
119500981030     c*
119600981030     c                   exfmt     fi24t01
119700981030     c                   setoff                                       3928
119800981030     c* KC = fine
119900981030     c                   if        not *inkc
120000981030     c* ks = cambio p.o. gestione
120100981030     c   ks              eval      *in05 = *on
120200981030     c   ks              iter
120300981030     c*
120400981030      * 05 on --> controllo P.O. in gestione
120500981030     c     *in05         ifeq      *on
120600981030     C                   exsr      SUB_CFGS
120700981030     c   28              iter
120800981030     c                   eval      *in05 = *off
120900981030     c                   leave
121000981030     c                   end
121100981030     c* se è stato dato solo enter vuol dire che deve proseguire
121200981030     c                   leave
121300981201     c                   else
121400981201     c                   leave
121500981030     c                   end
121600981030    3c                   enddo
121700981030     c                   endsr
121800981028      **********************************************************************
121900981028      * CONTROLLO IL P.O. IN IN GESTIONE
122000981028      **********************************************************************
122100981029     C     SUB_CFGS      BEGSR
122200981028      *
122300981029     c                   setoff                                       3928
122400981029     c                   clear                   v1dfgs
122500981029     C                   clear                   FNLV50DS
122600981029     C                   MOVEL     KNMUS         D50PRU
122700981029     C                   MOVEL     V1cfgs        D50FGS
122800170427     c                   eval      d50tcn='E'
122900981029     C                   CALL      'FNLV50R'
123000981029     C                   PARM                    FNLV50DS
123100981028      *
123200981029     C                   if        D50ERR <> *blanks
123300981029     C                   MOVEL     D50MSG        V1cMSG
123400981029     C                   SETON                                        3928
123500981029     C                   else
123600070403     c                   clear                   og148
123700981029     c     v1cfgs        CHAIN     azorg                              31
123800981029     c  n31              MOVEL     orgdes        v1dfgs
123900070403     c  n31              eval      og148 = orgde8
124000070403      * controllo se Filiale gestione è abilitata a telefonate GEO
124100070403     c                   eval      wfilgeo = (§oggeot = 'S')
124200981029     C                   endif
124300981028      *
124400981029     C  n28              EXSR      CARL6
124500981029     c                   endsr
124600981028      **************************************************************************
124700981028      * CARICO SCHIERA L6 DA TABELLA £6
124800981028      **************************************************************************
124900981028     C     carl6         BEGSR
125000981028      *
125100981028     C                   CLEAR                   L6
125200981028     C                   CLEAR                   TRUL06DS
125300981028     C                   MOVE      '£6'          D06COD
125400981028     C                   MOVEL     v1cfgs        D06key
125500981028     C                   MOVEL(P)  TRUL06DS      KPJBU
125600981028     C                   CALL      'TRUL06R'
125700981028     C                   PARM                    KPJBA
125800981028     C                   MOVEL     KPJBU         TRUL06DS
125900981028     C                   MOVEA     LIN           L6
126000981028      *
126100981028     c                   endsr
126200981022      *****************************************************************
126300981022      *   ROUTINE INIZIALE
126400981022      *****************************************************************
126500981022     C     *INZSR        BEGSR
126600981022      *
126700981022     C     *ENTRY        PLIST
126800981022     C                   PARM                    KPJBA
126900981022      *
127000170427     C                   Z-ADD     1             CODUT             1 0
127100170427      *
127200170427     c     *dtaara       define    §azute        azuteds
127300170427     c     *dtaara       define    §datiute      ddatiute
127400170427      *
127500170427     c                   in(E)     *dtaara
127600170427     c                   IF        %ERROR or RSUT = *blanks
127700170427     c                   clear                   Tibs34Ds
127800170427     c                   call      'TIBS34R'
127900170427     c                   parm                    Tibs34Ds
128000170427     c                   in        *dtaara
128100170427     c                   ENDIF
128200170427      *
128300981029      * titolo videata e opzioni
128400981029     c                   movel     tit           vtctit
128500981029     c                   movel     dopz          v1dse1
128600981022      *
128700981027     C                   TIME                    W0140
128800981022     C                   MOVE      W0140         WDTGIO
128900981022     C                   MOVEL     W0140         WORA
129000981029     C                   MOVEL     Wora          oramin
129100981022      * UDATE IN AAAAMMGG
129200981022     C                   Z-ADD     WDTGIO        G02DAT
129300981022     C                   MOVEL     *BLANK        G02ERR
129400981022     C                   CALL      'XSRDA8'
129500981022     C                   PARM                    WLBDAT
129600981022     C                   MOVEL     G02INV        DATEU
129700981028
129800981028      * IMPOSTO IL P.O. IN GESTIONE
129900170427     c     dutlpo        ifeq      '2'
130000170427     c     dutlpo        oreq      *blanks
130100170427     c                   movel     dutpou        v1cfgs
130200020502     C                   eval      *in06 = *on
130300020502     c                   else
130400020502     c                   movel     simfel        v1cfgs
130500020502     c                   endif
130600981029     c* decofico se non sono la sede
130700981029     c                   if        v1cfgs > 0
130800170427
130900170427     c**                 clear                   og148
131000170427     C**   V1CFGS        CHAIN     AZORG                              31
131100170427     C**N31              MOVEL     orgDES        V1DFGS
131200170427     c**n31              eval      og148 = orgde8
131300070403      * controllo se Filiale gestione è abilitata a telefonate GEO
131400170427     c**                 eval      wfilgeo = (§oggeot = 'S')
131500170427     C**                 exsr      CARL6
131600170427
131700170427     c                   exsr      sub_cfgs
131800981029    2C                   ENDIF
131900981027      *
132000981027     C* ACCESSO   TABEL
132100981027     C     KTAB          KLIST
132200981027     C                   KFLD                    CODUT
132300981027     C                   KFLD                    §COD
132400981027     C                   KFLD                    §KEY
132500981027      * accesso fndcd03l
132600981027     c     Kdcd3         KLIST
132700981027     C                   KFLD                    kfar
132800981028     C                   KFLD                    kaac
132900981028     C                   KFLD                    kfil
133000981028     C                   KFLD                    knca
133100981028      * accesso fndcd02l
133200981028     c     Kdcd2         KLIST
133300981028     c                   kfld                    v2sfls
133400050314     c                   kfld                    v2hlna
133500050314     c                   kfld                    v2hnrs
133600981028     c                   kfld                    v2snsc
133700981028     C                   KFLD                    kaac
133800981028     C                   KFLD                    kfil
133900981028     C                   KFLD                    knca
134000981027      * accesso fndct01l
134100981027     C     Kdct          klist
134200981027     C                   KFLD                    kaac
134300981027     C                   KFLD                    kfil
134400981027     C                   KFLD                    knca
134500981027      * accesso archivio bolle
134600991116     c     kspe          klist
134700991116     c                   kfld                    kaas
134800991116     c                   kfld                    klnp
134900991116     c                   kfld                    knrs
135000991116     c                   kfld                    knsp
135100991116     c*
135200991116     c     kar6          klist
135300981028     c                   kfld                    kaas
135400981028     c                   kfld                    klnp
135500981028     c                   kfld                    knrs
135600991116     c                   kfld                    knsp
135700991116     c                   kfld                    trcar6
135800981029     c*
135900981029     c     keydcs        KLIST
136000981029     c                   KFLD                    dcstpd
136100981029     c                   KFLD                    dcsnkt
136200981029     c                   KFLD                    dcsstd
136300981029     c                   KFLD                    dcsdim
136400981029     c                   KFLD                    dcshim
136500981029     c                   KFLD                    dcsnks
136600981029     c                   KFLD                    dcstrc
136700981029     c                   KFLD                    prgnot
136800981029     c                   eval      prgnot= 999
136900981029     c* di fndcs01l - parziale
137000981029     c*
137100981029     c     keydcsp       KLIST
137200981029     c                   KFLD                    dcstpd
137300981029     c                   KFLD                    dcsnkt
137400981029     c                   KFLD                    dcsstd
137500981029     c                   KFLD                    dcsdim
137600981029     c                   KFLD                    dcshim
137700981029     c                   KFLD                    dcsnks
137800981029     c                   KFLD                    dcstrc
137900981022      *
138000981022     C                   ENDSR
138100981201      *---------------------------------------------------------------------------------------------
138200981201     oFNDCT000  E            UPDDCT
138300981201     O                       DCTdch
138400981201     O                       DCTcch
138500981201     O                       DCTfca
138600981222     O                       DCTft1
138700981222     O                       DCTft2
138800981022      *****************************************************************
138900981028**
139000981028Non si possono selezionare entrambe le chiusure                                1
139100981028Inserire la causale chiusura                                                   2
139200981028Causale Chiusura inesistente                                                   3
139300981028Data Chiusura errata                                                           4
139400981028Inserire Data Chiusura                                                         5
139500981028Data Chiusura deve essere maggiore della data di apertura                      6
139600981028Data Chiusura non deve essere maggiore della data del giorno                   7
139700981028Selezionare i segnacolli da chiudere                                           8
139800981104Il tot. segnacolli Selez. è = al tot. colli mancanti CA (Chiudera la C.A.)     9
139900170427C.A. non manutenzionabile: in gestione ad atra filiale                        10
140000990225C.A. non manutenzionabile perchè chiusa                                       11

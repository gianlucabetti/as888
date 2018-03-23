000100980506      *------------------------------------------------------------------------*
000200981027      * GESTIONE COLLI RITROVATI                                               *
000300980506      *------------------------------------------------------------------------*
000400980803      ****************************************************************
000500980506     H DECEDIT('0,') DATEDIT(*DMY.)
000600980506      *--------------------------------------------
000700981201      ****************************************************************
000800981201      *  FASI C.A. FISSATE A PROGRAMMA
000900981201      ****************************************************************
001000981201      *    900 = Chiusura CA         (WCHCA)
001100981201      ****************************************************************
001200981201      *  CAUSALE CHIUSURA  FISSATA A PROGRAMMA
001300981201      *_______________________________________________________________
001400981201      *     03 = Pareggio            (WCCH)
001500981201      ***************************************************************
001600981201      *
001700981027     Ffidn24D   CF   E             WORKSTN  sfile(fi24s01:nrr1)
001800981027     f                                      sfile(fi24s02:nrr2)
001900981022     Ffndfa01L  IF   E           k DISK
002000981028     Ffndct01l  uf   E           k DISK
002100981201     Ffndct02l  if   E           k DISK    rename(fndct000:fndct2)
002200981027     Ffndcd03l  if   E           k DISK
002300981028     Ffndcd02l  uf   E           k DISK    rename(fndcd000:fndcd2)
002400981201     Ffndcd01l  uf   E           k DISK    rename(fndcd000:fndcd1)
002500981029     Ffndcs01l  if A E           k DISK
002600981201     Ffndcf01l  O  A E           k DISK
002700040802     Ffndck01l  if   E           k DISK
002800170428     Ffnblp01l  iF   E           k DISK    prefix(arb:3)
002900991115     Ffiar601l  iF   E           k DISK
003000990202     Ffnarb01l  iF   E           k DISK
003100981027     Fazorg01l  if   E           k DISK
003200981022     Ftabel00f  if   E           k DISK
003300981022      ****************************************************************
003400981022      *  RIEPILOGO INDICATORI
003500981022      ***************************************************************
003600981029      * 05   - Sproteggo P.O. IN GESTIONE
003700981028      * 06   - non abilito il tasto funzionale F18
003800981218      * 11   - non viene gestita la chiusura ma solo la fase di analisi colli
003900981027      * 20/21- Gestione 1° subfile
004000981022      * 28   - ERRORE GENERICO
004100981022      * 29   - RIEMISSIONE FORMATO VIDEO
004200981028      * 30/32- COMODO
004300981029      * 39/44- Errori nella seconda videata
004400981027      * 50/51- Gestione 2° subfile
004500990122      * 81   - Campo Reverse Immage
004600981218      * 83   - Posizionamento Cursore Errore
004700981030      * 90   - COMODO
004800981030      * 91   - Lettura subfile
004900981022      *--------------------------------------------
005000981022     D                SDS
005100981022     D  VTCPGM                 1     10
005200981022      *
005300981028     D fidn00ds      E DS
005400981028     D fidn10ds      E DS
005500981029     D fnlv50ds      E DS
005600991115     D*!* dsbl4a        E DS
005700981028     D TRUL06DS      E DS
005800981028     D  LIN                    1     90  0    DIM(30)                           P.O. COMODO
005900981022      * Ds passaggio parametri a pgm ricerca tabelle
006000981028     D TIBS02DS      E DS
006100981022
006200981022      *--------------------------------------------
006300981022     D KPJBA         E DS
006400981027     D DTAD          E DS
006500981230     D DDCT01        E DS
006600070403     d og148         e ds
006700070403     d dslr02        e ds
006800170428      * - Parametri x Controllo profilo utenti
006900170428     d TIBS34DS      e ds                  inz
007000170428      * - Ds di riferimento al file esterno AZUTE00F
007100170428     d AZUTEds       e ds                  extname(AZUTE00F)
007200170428      * - Ds per dati organigramma
007300170428     d DDatiUte      e ds
007400170428      *
007500981022      *--------------------------------------------
007600040802     D chisono         s              1
007700981022     D dateu           s              8  0
007800981029     D oramin          S              4  0
007900981022     D W0140           S             14  0
008000981022     D WDTGIO          S              8  0
008100981022     D WORA            S              6  0
008200981201     D prima           S              1
008300981028     D Wf03            S              1
008400981029     D W007a           S              7                                         * campo alfanumerico
008500981029     D W002a           S              2                                         * campo alfanumerico
008600981029     D X               S              1  0                                      * numeratore
008700981029     D wafls           s              3
008800981029     D wanrs           s              2
008900981029     D wAnsc           S              7
009000981029     D wadfv           s              4
009100981029     D wagio           s              2
009200981029     D wames           s              2
009300981029     D wafgs           s              3
009400981029     D wanps           s              2
009500170428     D w§dctlnpc       S              3  0
009600981201     D Wchca           S                   like(dctfca)
009700981201     d                                     inz(900)
009800981201     D Wcch            S                   like(dctcch)
009900981201     d                                     inz('03')
010000981028     D WTPB            S                   like(I00TPB)
010100981028     D Wdch            S                   like(dcddch)
010200981028     D Wnco            S                   like(dctncn)
010300981028     D Wrknco          S                   like(dctncn)
010400981028     D WDatac          S              8  0
010500990309     D W1hpor          S                   like(v1hpor)
010600981028     D kaas            s                   like(dctaas)
010700981028     D klnp            s                   like(dctlnp)
010800981028     D knrs            s                   like(dctnrs)
010900981028     D knsp            s                   like(dctnsp)
011000981027     D kaac            s                   like(dcdaac)
011100981027     D kfil            s                   like(dcdfil)
011200981027     D knca            s                   like(dcdnca)
011300981022     D §cod            S                   LIKE(TBLcod)
011400981022     D §key            S                   LIKE(TBLkey)
011500991116     D trcar6          S                   LIKE(ar6trc)
011600981201     D kfar            S                   LIKE(DCDfar)
011700981027     d                                     INZ('S')
011800981029     d prgnot          S                   LIKE(dcspno)                         *progressivo note
011900981027     D nrr1            S              4  0
012000981027     D nrr2            S              4  0
012100981028     D DATA_eur        S               D   DATFMT(*eur)
012200070403     d wfilgeo         s              1    inz(*off)
012300981022      *
012400981028     d dsnumca         ds
012500981028     d  aac                    1      4  0
012600981028     d  fil                    5      7  0
012700981028     d  nca                    8     14  0
012800981022     D WLBDAT          DS                  INZ
012900981022     D  G02DAT                 1      8  0
013000981022     D  G02INV                 9     16  0
013100981022     D  G02ERR                17     17
013200981022     D  G02TGI                18     22  0
013300981022     D VIDIVA          DS
013400981022     D  w2civa                 1     14
013500981022     D  w2cive                15     16
013600981027      *  titolo videata (lunghezza massima 34)
013700981029     D TIT             C                   CONST('***  GESTIONE COLLI RITROVATI-
013800981029     D                                       ***')
013900981027      *  Descrizione opzioni (lunghezza massima 78)
014000981027     D DOPZ            C                   CONST('2=Manutenzione   4=Note in St-
014100981029     D                                     ampa   5=Visual. C.A.   8=Note C.A. -
014200981029     D                                      G=Gestione')
014300981027      *  titolo videata (lunghezza massima 34)
014400981027     D Kannullata      C                   CONST('   ANNULLATA                 -
014500981027     D                                         ')
014600981028     D Kanalizz        C                   CONST('   ANALIZZATA                -
014700981028     D                                         ')
014800981028      *   S C H I E R E
014900990225     D MSG             S             78    DIM(11) CTDATA PERRCD(1)             MSG VIDEO
015000981028     D L6              S              3  0 DIM(30)                              P.O.GESTITI
015100981022      *
015200981022      * ---------------------------------------------------------
015300981022      *
015400981030     c                   if        v1cfgs > 0
015500981030     c* emetto la richiesta del P.O. gestione    --------------------------------------------!
015600981030     c*--------------------------------------------------------------------------------------!
015700981218     c*                  exsr      SUB_FIRST                                                 !
015800981030     c*--------------------------------------------------------------------------------------!
015900981022      * EMISSIONE PRIMA VIDEATA
016000981029    1C                   dow       *inkc = *off
016100981027     c* caricamento dati subfile
016200981028     c                   exsr      SUB_CASF1
016300981027     C*
016400981027      * Se non ho caricato record emetto videata con segnalazione di errore
016500981028    2c                   if        nrr1 = 0
016600981029    3c                   dow       *inkc = *off
016700981027     c                   exfmt     fi24d01
016800981029      * f18=Cambio P.O. in gestione
016900981029     c   ks              eval      *in05 = *on
017000981029     c   ks              iter
017100981029
017200981029      * 05 on --> controllo P.O. in gestione
017300981029     c     *in05         ifeq      *on
017400981029     C                   exsr      SUB_CFGS
017500981029     c   28              iter
017600981029     c                   eval      *in05 = *off
017700981029     c                   leave
017800981029     c                   end
017900981029
018000981028    3c                   enddo
018100981028   x2c                   else                                                   * ci sono CA
018200981027      *
018300981028     c                   exsr      SUB_GESF1
018400981022      * F3=FINE
018500981028    3C                   if        *inkc = *on
018600981027     c                   leave
018700981028    3c                   endif                                                  * INKC
018800981027      *
018900981028    2c                   endif                                                  * NRR1 > 0
019000981027      *
019100981022    1c                   enddo                                                  *fine ciclo KC *on
019200981030      *
019300981030    2c                   endif                                                  * non sono in sede
019400981022      *
019500981028     c                   clear                   fidn10ds
019600981022     c                   movel     'C'           i10tla
019700981028     c                   movel     fidn10ds      kpjbu
019800981022     c                   call      'FIDN10R'
019900981022     c                   parm                    kpjba
020000981029     c*
020100981029     c                   clear                   tibs02ds
020200981029     c                   movel     'C'           t02tla
020300981029     c                   call      'TIBS02R'
020400981029     c                   parm                    kpjba
020500981029     c                   parm                    tibs02ds
020600981022      *
020700981022     C                   EVAL      *INLR = *ON
020800981027      *****************************************************************
020900981027      *   ROUTINE SCRITTURA SUBFILE TESTATE CA AVENTI COLLI RITROVATI
021000981027      *****************************************************************
021100981028     C     SUB_CASF1     BEGSR
021200981027     c* pulisco subfile
021300981027     c                   setoff                                           21
021400981027     c                   write     fi24c01
021500981028     c                   z-add     *zeros        nrr1
021600981027     c                   seton                                            21
021700981029     c*
021800981029     c                   clear                   kaac
021900981029     c                   clear                   kfil
022000981029     c                   clear                   knca
022100981027     c*
022200981027     c* leggo dal dettaglio colli se esistono dei records che hanno il
022300981027     c* flag di collo da analizzare uguale ad 'S'
022400981027     c*
022500981028     c     kfar          setll     fndcd03l
022600981028     c     kfar          reade     fndcd03l                               30
022700981027    1c                   dow       *in30 = *off
022800000301     c* i colli devono essere ritrovati da P.O. che è in gestione o dalla sua £6
022900000301     c                   if        dcdfgs <> v1cfgs
023000000301     c* verifico la sua £6
023100170428     c* qui va vbene controllare semplicemente la filiale così come è
023200170428     c*  perchè è quella che ha ritrovato il collo e non importa se F18 lo abbia
023300170428     c*  fatto il suo term partenza o term arrivo
023400000301     c     dcdfgs        lookup    l6                                     45
023500000301     c  n45              goto      leggi
023600000301     c                   endif
023700000301     c*
023800000301    2c*****************+ if        dcdfgs = v1cfgs
023900981027     c* carico una sola c.a. anche se ritrovati + colli per la stessa
024000981028    3c                   if        knca <> dcdnca or
024100981027     c                             kaac <> dcdaac or
024200981027     c                             kfil <> dcdfil
024300981027     c*
024400981027     c                   eval      knca = dcdnca
024500981027     c                   eval      kaac = dcdaac
024600981027     c                   eval      kfil = dcdfil
024700981027     c*
024800981030     c     kdct          chain(n)  fndct01l                           31
024900990225     c*
025000981230     c                   movel     dctflo        ddct01
025100170428     c                   move      §dctlnpc      W§dctlnpc
025200981230     c*
025300981230     c                   movel     §dctport      v1hpor
025400981230     c*
025500981201     c* se CA chiusa non do la possibilità di chiudere di nuovo    accendo l'11
025600981201     c                   if        dctdch > 0
025700981201     c                   eval      v1hi11 = 'S'
025800990225     c                   eval      v1hchi = 'S'
025900981201     c                   else
026000981201     c                   eval      v1hi11 = ' '
026100990225     c                   eval      v1hchi = ' '
026200981201     c                   endif
026300981218      * Cerco in L6 il P.O. gestione fase
026400981218     c                   eval      v1hges   = 'N'
026500170428    3C                   if        DCTgfc > *zeros
026600981218     C     DCTgfc        LOOKUP    L6                                     31
026700170428    4c                   if        *in31 and simfel>0
026800170428     c                   setoff                                       32
026900170428     c     DCTlna        lookup    L6                                     32
027000170428    5c                   if        *in32 and d50val_a='N'
027100170428     c                   setoff                                           32
027200170428    5c                   endif
027300170428     c
027400170428     c  n32w§DCTlnpc     lookup    L6                                     32
027500170428    5c                   if        *in32 and d50val_p='N'
027600170428     c                   setoff                                       32
027700170428    5c                   endif
027800170428    5c                   if        not *in32
027900170428     c                   setoff                                       31
028000170428    5c                   endif
028100170428    4C                   endif
028200170428
028300981218     c   31              eval      v1hges   = 'S'
028400170428    3C                   endif
028500990122     c*
028600990122     c                   movel     dctgfc        v1hgfc
028700981218     c* controllo se CA in gestione dal P.O.
028800981201     c*
028900040802      * verifico se sono in arrivo o in partenza
029000040802      *
029100040802     c                   clear                   chisono
029200040802     C     DCTlna        LOOKUP    L6                                     31
029300170428     c                   if        *in31 and d50val_a='S'
029400170428     c                   eval      chisono  = 'A'
029500170428     c                   endif
029600040802     c
029700981027     c                   z-add     DCTaac        V1Haac
029800981030     c                   z-add     DCTfca        V1Hfca
029900981027     c                   z-add     DCTaas        V1Haas
030000981027     c                   z-add     DCTlna        V1Hlna
030100981028     c                   z-add     DCTncn        V1Hncn
030200981027     c                   z-add     DCTfil        V1Cfil
030300981027     c                   z-add     DCTnca        V1Cnca
030400981027     c                   z-add     DCTaac        V1Cdca
030500981027     c                   movel     DCTmgc        V1Cdca
030600981027     c     *mdy          move      V1Cdca        DATA_eur
030700981027     c     *dmy          move      DATA_eur      V1Cdca
030800981027     c                   move      DCTaas        V1caas2
030900981027     c                   z-add     DCTlnp        V1Clnp
031000981027     c                   z-add     DCTnrs        V1Cnrs
031100981027     c                   z-add     DCTnsp        V1Cnsp
031200981027     c                   z-add     Dctfca        V1Cfca
031300040802      * se sono l'arrivo valorizzo con il numero spedizione esistente su FNDCK se esiste
031400040802     c                   if        chisono =  'A'
031500040802     c     kdct          chain     fndck01l
031600040802     c                   if        %found(fndck01l)
031700040802     c                   move      DCKaas        V1caas2
031800040802     c                   z-add     DCKaas        V1Haas
031900040802     c                   z-add     DCKlnp        V1Clnp
032000040802     c                   z-add     DCKnrs        V1Cnrs
032100040802     c                   z-add     DCKnsp        V1Cnsp
032200040802     c                   endif
032300040802     c                   endif
032400981027      *
032500981028    4c                   IF        DCTatb <> *blanks
032600981027     c                   movel(P)  Kannullata    V1Dfca
032700981028   x4c                   ELSE
032800981027     c                   clear                   v1dfca
032900981027     c     v1cfca        chain     fndfa01l                           31
033000981027     c  n31              movel     DFAdes        V1Dfca
033100981028    4c                   ENDIF
033200981027      *
033300981027     c                   movel     dcttad        V1Ctad
033400981027      * descrizione
033500981027     C                   MOVEL     'C'           t02mod
033600981027     C                   MOVEL     knsif         t02sif
033700981027     C                   MOVEL     'TAD'         t02cod
033800981027     C                   MOVEL     v1ctad        t02ke1
033900981027      *
034000981027     C                   CALL      'TIBS02R'
034100981027     C                   PARM                    KPJBA
034200981027     C                   PARM                    TIBS02DS
034300981027      *
034400981028    4C                   IF        t02err = *BLANKS
034500981027     C                   movel     T02uni        DTAD
034600981027     C                   movel     §TADdesc      v1dtad
034700981028   x4c                   else
034800981027     c                   clear                   v1dtad
034900981028    4c                   endif
035000981027      *
035100981027      *
035200981027     c                   z-add     DCTnev        V1Cnev
035300981027     c                   z-add     DCTaae        V1Caae
035400981028      * ricerca se bolla partenza o arrivi
035500981028     c                   EXSR      SUB_TPB
035600981028      *
035700990528     c* se tpb non è a blank scrivo il record altrimenti vuol dire che non si trova
035800990528     c* in archivio
035900990528     c                   if        wtpb <> *blanks
036000990528     c*
036100990528     c                   add       1             nrr1
036200981028     c                   move      wtpb          v1htpb
036300981027      *
036400981027     c                   write     fi24s01
036500990528      *
036600990528     c                   endif
036700981027      *
036800981028    3c                   endif
036900981027      *
037000000301    2c**********         endif
037100000301     c     leggi         tag
037200981028      *
037300981028     c     kfar          reade     fndcd03l                               30
037400981027      *
037500981027    1c                   enddo
037600981027     c*
037700981027     c                   endsr
037800981027      *****************************************************************
037900981027      *   ROUTINE GESTIONE PRIMO SUBFILE
038000981027      *****************************************************************
038100981028     C     SUB_GESF1     BEGSR
038200981027     c*
038300981029     c                   seton                                        2021
038400981028     c                   z-add     1             nrr1
038500981027     c* emetto il subfile
038600981028    1c                   dow       not *inkc
038700981029     c                   write     fi24z01
038800981027     c                   exfmt     fi24c01
038900981218     c                   setoff                                       9028
039000981029      * f18=Cambio P.O. in gestione
039100981029     c   ks              eval      *in05 = *on
039200981029     c   ks              iter
039300981029
039400981029      * 05 on --> controllo P.O. in gestione
039500981029     c     *in05         ifeq      *on
039600981029     C                   exsr      SUB_CFGS
039700981029     c   28              iter
039800981029     c                   eval      *in05 = *off
039900981029     c                   leave
040000981029     c                   end
040100981027     c* KC = fine lavoro
040200981028    2c                   if        not *inkc
040300981030     c                   setoff                                       9091
040400981028     c* LETTURA SUBFILE
040500981030    3c                   dow       *in91 = *off
040600981218     c                   setoff                                       818328
040700981030     c                   readc     fi24s01                                91
040800981028     c* valorizzo dsnunca
040900981028     c                   eval      aac = v1haac
041000981028     c                   eval      fil = v1cfil
041100981028     c                   eval      nca = v1cnca
041200981028     c*
041300981028     c                   select
041400981028     c* fine file oppure è stato dato un F3 in qualche pgm chiamato
041500981030     c                   WHEN      *in91 or *in90
041600981028     c*
041700981028     c* OPZIONE  "2" Manutenzione C.A.
041800981218
041900990225     c                   WHEN      v1csce = '2'  and  v1hges = 'N'              non gestibile
042000981218     c                   movel     MSG(10)       V1Cmsg
042100981218     c                   seton                                        288390
042200990225
042300990225     c                   WHEN      v1csce = '2'  and  v1hchi = 'S'              non gestibile
042400990225     c                   movel     MSG(11)       V1Cmsg
042500990225     c                   seton                                        288390
042600981218
042700981218     c                   WHEN      v1csce = '2'  and  v1hges = 'S'
042800981028     c                   exsr      SUB_DS00                                     * car ds fidn00ds
042900981028     c                   movel     'M'           I00mod
043000981028     c                   movel     fidn00ds      kpjbu
043100981028     c                   CALL      'FIDN03R'
043200981028     c                   parm                    kpjba
043300981028     c                   movel     kpjbu         fidn00ds
043400981028     c                   eval      *in90 = (o00f03 = 'S')
043500981028     c* OPZIONE  "4" Note per stampa
043600981028     c                   WHEN      V1csce = '4'
043700981028     c                   exsr      SUB_STNOTE
043800981028     c                   eval      *in90 = (WF03 = 'S')
043900981028     c* OPZIONE  "5" Visualizza   C.A.
044000981028     c                   WHEN      v1csce = '5'
044100981030     c                   exsr      SUB_DS00                                     * car ds fidn00ds
044200981028     c                   movel     'I'           I00mod
044300981028     c                   movel     fidn00ds      kpjbu
044400981030     c                   CALL      'FIDN01R'
044500981028     c                   parm                    kpjba
044600981028     c                   movel     kpjbu         fidn00ds
044700981028     c                   eval      *in90 = (o00f03 = 'S')
044800981028     c* OPZIONE  "8" Note C.A.
044900981028     c                   WHEN      V1Csce = '8'
045000981028     c                   exsr      SUB_NOTE
045100981028     c                   eval      *in90 = (WF03 = 'S')
045200981028     c* OPZIONE  "G" Gestione Ritrovamento Colli
045300990122     c                   WHEN      V1Csce = 'G' and v1dfca <> kanalizz
045400981028     c                   exsr      SUB_GERC
045500981028    4c                   if        *inkf = *on                                  * ca analizzata
045600990122     c                   seton                                        8190      * reverse immage
045700981028     c                   movel     kanalizz      v1dfca
045800981030     c                   clear                   v1cfca
045900990225     c                   movel     'S'           v1hchi
046000981028    4c                   endif
046100981028     c*
046200981028     c                   ENDSL
046300981028     c* aggiorno il record subfile
046400981028     c                   clear                   v1csce
046500981028     c*
046600981030     c  n91              update    fi24s01
046700981028     c*
046800981028    3c  n90              enddo                                                  * lettura subfile
046900981028     c*
047000981028    2c                   endif                                                  * not *inkc
047100981028     c*
047200981028    1c                   enddo                                                  * ciclo
047300981028     c*
047400981028     c                   endsr
047500981028      **********************************************************************
047600981028      * GESTIONE RITROVAMENTO COLLO         2° VIDEATA
047700981028      **********************************************************************
047800981028     C     SUB_GERC      BEGSR
047900981028      *
048000981028     c* routine di caricamento seconda videata
048100981028     c                   exsr      SUB_CAF2
048200981028     c*
048300981028     c* routine di gestione seconda videata
048400981028     c                   exsr      SUB_GEF2
048500981028     c*
048600981028     c                   endsr
048700981028      **********************************************************************
048800981028      * CARICAMENTO VIDEATA GESTIONE COLLI RITROVATI     2° videata
048900981028      **********************************************************************
049000981028     C     SUB_CAF2      BEGSR
049100981028      *
049200981028      * DATI CA
049300981028     c                   eval      v2cfil = v1cfil                              * P.O. apertura CA
049400981028     c                   eval      v2cnca = v1cnca                              * Numero CA
049500981029     c                   movel     v1cdca        v2cdca                         * Data CA
049600981030     c                   move      v1haac        v2cdca
049700981028     c                   eval      v2ctad = v1ctad                              * tipo anomalia
049800981028     c                   movel     v1dtad        v2dtad                         * tipo anomalia des
049900981028     c                   eval      v2clnp = v1clnp                              * linea partenza
050000981028     c                   eval      v2cnrs = v1cnrs                              * numero serie
050100981028     c                   eval      v2cnsp = v1cnsp                              * numero spedizione
050200981028     c                   eval      v2cncn = v1hncn                              * num.colli dannegg.
050300990122     c                   eval      v2cfca = v1hfca                              * fase CA
050400990122     c                   movel     v1dfca        v2dfca                         * decodifica fase CA
050500990122     c                   eval      v2cgfc = v1hgfc                              * P.O. gestione CA
050600990122     c* decodifica p.o. gestione fase ca
050700990122     c     v2cgfc        chain     azorg                              30
050800990122     C  N30              if        orgde5 <> *BLANKS
050900990122     C                   movel     orgde5        v2dgfc
051000990122     C                   else
051100990122     c                   movel     orgdes        v2dgfc
051200990122     c                   endif
051300981028     c*
051400981028     c                   z-add     V1Haas        Kaas
051500981028     c                   z-add     V1Clnp        Klnp
051600981028     c                   z-add     V1Cnrs        Knrs
051700981028     c                   z-add     V1Cnsp        Knsp
051800981028     c* se è partenza vado in blp
051900981028     c                   if        v1htpb  = 'P'
052000981028     c     kspe          chain     fnblp000                           30
052100981028     c* se è arrivo vado in arb
052200981028     c                   else
052300981201     c     kspe          chain(n)  fnarb000                           30
052400981028     c                   endif
052500981028     c*
052600981029     c                   movel     arbaas        v2cdsp
052700981029     c                   move      arbmgs        v2cdsp
052800981029     c     *ymd          move      V2Cdsp        DATA_eur
052900981029     c     *eur          move      DATA_eur      v2cdsp                         * data spedizione
053000981028     c*
053100990528     c                   eval      v2clna=v1hlna                                * linea arrivo
053200981028     c* decodifica
053300981028     c     v2clna        chain     azorg                              30
053400981028     C  N30              if        orgde5 <> *BLANKS
053500981028     C                   movel     orgde5        v2dlna
053600981028     C                   else
053700981028     c                   movel     orgdes        v2dlna
053800981028     c                   endif
053900981230     c                   movel     arbnas        VTCnas
054000981201      * decodifico tipo bolla
054100981230     C                   if        v1hpor = 'F'
054200981230     C                   movel(P)  'FRANCO'      VTDcbo
054300981230     C                   else
054400981230     C                   movel(P)  'ASSEGNATO'   VTDcbo
054500981230     C                   endif
054600990309     c*
054700990309     c                   SELECT
054800990309     c*
054900990309    1c                   when      arbcca <> '9'
055000990309     c                   eval      W1hpor  = v1hpor
055100990309     c*
055200990309    2c                   when      v1hpor = 'F'
055300990309     c                   eval      W1hpor  = 'A'
055400990309     c*
055500990309    2c                   when      v1hpor   = 'A'
055600990309     c                   eval      W1hpor  = 'F'
055700990309     c*
055800990309    2c                   endsl
055900981201      *
056000981201      *  Se assegnato in partenza verifico il codice destinatario
056100991115     c*!*                clear                   DSBL4A
056200991115     c                   clear                   ar6ksc
056300990309     c                   IF        v1htpb  = 'P' and  w1hpor  = 'A'
056400991115     c*!*  Kbl4          CHAIN     FNBL4000                           30
056500991115     c*!*N30              movel     BL4NOT        DSBL4A
056600991116     c                   eval      trcar6 = '1'
056700991116     c     KAR6          CHAIN     FIAR6000                           30
056800981201     c                   ENDIF
056900981201      *
057000990309     c                   IF        w1hpor = 'A'
057100981028     c                   move      arbccm        VTCksm
057200991115     c*!*                if        §B4ksf <> *blanks
057300991115     c*!*                movel     §B4ksf        VTCksd
057400991115     c                   if        AR6ksc <> *zeros
057500991115     c                   movel     AR6ksc        VTCksd
057600981201     c                   else
057700981201     c                   movel     arbksc        VTCksd
057800981201     c                   endif
057900981028     c                   ELSE
058000981028     c                   move      arbksc        VTCksm
058100981028     c                   move      *zeros        VTCksd
058200981028     c                   ENDIF
058300981028
058400981028      * DATI BOLLA
058500981028     c* Mittente Destinatario
058600981028     c                   movel     arbrsm        VTCrsm
058700981028     c                   move      arbcam        VTCcam
058800981028     c                   movel     arblom        VTClom
058900981028     c                   movel     arbprm        VTCprm
059000981028     c                   movel     arbnzm        VTCnam
059100981028     c                   movel     arbrsd        VTCrsd
059200981028     c                   move      arbcad        VTCcad
059300981028     c                   movel     arblod        VTClod
059400981028     c                   movel     arbprd        VTCprd
059500981028     c                   movel     arbnzd        VTCnad
059600981028     c* colli peso volume
059700981028     c                   z-add     arbncl        VTCncl
059800981028     c                   z-add     arbpkf        VTCpkf
059900981028     c                   movel     arbfvf        VTCfvf
060000981028     c                   z-add     arbvlf        VTCvlf
060100981028      * DATI CHIUSURA COLLI / CA
060200981028     c                   clear                   v2ccca
060300981028     c                   clear                   v2ccco
060400981028     c                   clear                   v2ccch
060500981028     c                   clear                   v2cdch
060600981201      * se CA chiusa accendo l'11 proteggo i xcampi di chiusura in quanto la CA è già chiusa
060700981201      * ma do la possibilità di dare come analizzati i segnacolli ancora da ANALIZZARE
060800981201     C                   EVAL      *in11 = (v1hi11 = 'S')
060900981218      * se CA non in gestione da me proteggo i campi di chiusura
061000981218      * ma do la possibilità di dare come analizzati i segnacolli ancora da ANALIZZARE
061100981028      * DATI COLLI RITROVATI
061200981218     C  n11              EVAL      *in11 = (v1hges = 'N')
061300981218      *
061400981028     c                   exsr      SUB_CASF2
061500981028      *
061600981028     c                   endsr
061700981028      *****************************************************************
061800981028      *   ROUTINE SCRITTURA SUBFILE COLLI RITROVATI
061900981028      *****************************************************************
062000981028     C     SUB_CASF2     BEGSR
062100981028     c* pulisco subfile
062200981028     c                   setoff                                           51
062300981028     c                   write     fi24c02
062400981028     c                   z-add     *zeros        nrr2
062500981028     c                   seton                                            51
062600981028     c* preparo la chiave
062700981028     c                   z-add     V1haac        Kaac
062800981028     c                   z-add     V2Cfil        Kfil
062900981028     c                   z-add     V2Cnca        Knca
063000981028     c*
063100981028     c* leggo dettaglio colli della C.A. in gestione
063200981028     c* deve avere flag da analizzare uguale ad 'S'
063300981028     c* deve essere stato ritrovato dal P.O. di gestione o dalla sua £6
063400981028     c     kdcd3         setll     fndcd03l
063500981028     c     kdcd3         reade     fndcd03l                               31
063600981028     c*
063700981028    1c                   dow       *in31 = *off
063800981028     c* controllo il P.O. che ha ritrovato il collo
063900000301    2c************       if        dcdfgs = v1cfgs
064000000301     c                   if        dcdfgs <> v1cfgs
064100000301     c* verifico la sua £6
064200000301     c     dcdfgs        lookup    l6                                     45
064300000301     c  n45              goto      leggi2
064400000301     c                   endif
064500000301     c*
064600981028     c*
064700981028     c                   clear                   v2ssel
064800981028     c                   eval      v2sfls = dcdfls
064900981028     c                   eval      v2snsc = dcdnsc
065000981028     c                   eval      v2sfgs = dcdfgs
065100050314      * campi utili per la chiave
065200050314     c                   eval      v2hlna = dcdlna
065300050314     c                   eval      v2hnrs = dcdnrs
065400050314
065500981028     c     *ymd          move      dcddfv        DATA_eur
065600981029     c     *eur          move      DATA_eur      V2sdfv
065700981028     c                   eval      v2snps = dcdnps
065800981028     c*
065900981028     c                   add       1             nrr2
066000981028      *
066100981028     c                   write     fi24s02
066200981028      *
066300000301    2c***************    endif
066400981028      *
066500000301     c     leggi2        tag
066600981028     c     kdcd3         reade     fndcd03l                               31
066700981028      *
066800981028    1c                   enddo
066900981028     c*
067000981028     c                   endsr
067100981028      *****************************************************************
067200981028      *   ROUTINE GESTIONE VIDEATA COOLI RITROVATI
067300981028      *****************************************************************
067400981028     C     SUB_GEF2      BEGSR
067500981028     c*
067600981028    1c                   if        nrr2 > 0
067700981028     c                   seton                                        50
067800981028     c                   z-add     1             nrr1
067900981028    1c                   endif
068000981028     c                   clear                   prima
068100981028     c                   clear                   wnco
068200981028     c*
068300981028    1c                   do        *hival
068400981029     c*
068500981029     c                   write     fi24z02
068600981028     c* emissione
068700981028     c                   exfmt     fi24c02
068800981104     c                   setoff                                       40  42
068900981028     c                   setoff                                       434428
069000981028     c* controllo le funzioni
069100981028     c                   select
069200981028     c* F12 Videata precedente
069300981028     c                   WHEN      *INKL
069400981028     c                   leave
069500981028     c* F18 Gestione Note CA
069600981028     c                   WHEN      *INKS
069700981028     c                   exsr      SUB_NOTE
069800981028     c*
069900981028     c                   endsl
070000981028     c* controllo del formato
070100981028     c                   exsr      SUB_CTR02
070200981028     c* se non ci sono errori e  CMD 6 confermo i dati
070300981028    2c                   if        *in28 = *off and *inkf = *on
070400981028     C                   exsr      SUB_CONFE
070500981028     c                   leave
070600981028    2c                   endif
070700981028    1c                   enddo
070800981028     c*
070900981028     c                   ENDSR
071000981028      **********************************************************************
071100981028      * CONTROLLO SECONDA VIDEATA
071200981028      **********************************************************************
071300981028     C     SUB_CTR02     BEGSR
071400981028      *
071500981028     c* chiusura C.A.
071600981028    1c                   if        v2ccca = 'SI'
071700981028     c* chiusura colli
071800981028     c                             and v2ccco = 'SI'
071900981028     c* errore
072000981028     c                   movel     msg(1)        v2cmsg
072100981028     c                   seton                                        4028
072200981028     c                   goto      ectr2
072300981028    1c                   endif
072400981028      *
072500981028     c* chiusura colli
072600981028    1c                   if        v2ccco = 'SI'
072700981028     c* controllo se è stata inserita la causale chiusura
072800981028    2c                   if        v2ccch = *blank
072900981028     c* errore
073000981028     c                   movel     msg(2)        v2cmsg
073100981028     c                   seton                                        4228
073200981028     c                   goto      ectr2
073300981028     c*
073400981028   x2c                   else
073500981028     c* controllo se è stata fatta una richiesta di ricerca in tabella
073600981028     c     '?'           scan      v2ccch                                 32
073700981028      *
073800981028    3C                   IF        *IN32 = *ON
073900981028     c                   clear                   tibs02ds
074000981028     c                   movel     'R'           t02mod
074100981028     c                   movel     knsif         t02sif
074200981028     C                   movel     'CCH'         t02cod
074300981028      *
074400981028     C                   CALL      'TIBS02R'
074500981028     c                   parm                    KPJBA
074600981028     c                   parm                    tibs02ds
074700981028      *
074800981028    4c                   if        t02err =  *blanks
074900981028     C                   MOVEL     t02ke1        v2ccch
075000981030     c                   else
075100981030     c                   movel     t02msg        v2cmsg
075200981030     c                   seton                                        4228
075300981030     c                   goto      ectr2
075400981028    4c                   endif
075500981028      *
075600990302   x3C                   Endif
075700981028      *   Controlli di validità'
075800981028     c                   clear                   tibs02ds
075900981028     C                   MOVEL     'C'           t02mod
076000981028     C                   MOVEL     knsif         t02sif
076100981028     C                   MOVEL     'CCH'         t02cod
076200981028     C                   MOVEL(P)  v2ccch        t02ke1
076300981028      *
076400981028     C                   CALL      'TIBS02R'
076500981028     C                   PARM                    KPJBA
076600981028     C                   PARM                    tibs02ds
076700981028      *
076800981028    4C                   if        t02err <> *BLANKS
076900981028     c                   movel     msg(3)        v2cmsg
077000981028     c                   seton                                        4228
077100981028     c                   goto      ectr2
077200981028    4C                   endif
077300981028    3C                   endif
077400981028     c*
077500981028     c* controllo la data di chiusura colli
077600981028    2C                   if        v2cdch <> 0
077700981028     C                   move      v2cdch        g02dat
077800981028     C                   eval      g02err = *blanks
077900981028     C                   call      'XSRDA8'
078000981028     C                   parm                    wlbdat
078100981028    3C                   if        g02err = '1'
078200981028     C                   seton                                        4328
078300981028     C                   movel     msg(4)        v2cmsg
078400981028     c                   goto      ectr2
078500981028   x3C                   else
078600981028     C                   z-add     g02dat        V2cdch
078700981028     C                   move      g02inv        wdch
078800981028    3C                   endif
078900981028   x2C                   else
079000981028     C                   seton                                        2843
079100981028     C                   movel     msg(5)        v2cmsg
079200981028     c                   goto      ectr2
079300981028    2C                   ENDIF
079400981028      * controllo che la data fase sia maggiore della data apertura C.A.
079500981030     c                   move      V2Cdca        g02dat
079600981030     c                   clear                   g02err
079700981030     c                   call      'XSRDA8'
079800981030     c                   parm                    wlbdat
079900981030     c                   movel     g02inv        wdatac
080000981030     c*
080100981028    2c                   if        *in28 = *off and
080200981028     c                             wdatac > wdch
080300981028     C                   seton                                        2843
080400981028     C                   movel     msg(6)        v2cmsg
080500981028     c                   goto      ectr2
080600981028    2C                   Endif
080700981028      * controllo che la data fase sia minore o uguale ad oggi
080800981028    2c                   if        *in28 = *off and
080900981028     c                             dateu < wdch
081000981028     C                   seton                                        2843
081100981028     C                   movel     msg(7)        v2cmsg
081200981028     c                   goto      ectr2
081300981028    2C                   Endif
081400981028     c* controllo che sia stato selezionato almeno un collo ritrovato
081500981028     c                   exsr      SUB_COLLI
081600981028     c* controllo il numero dei colli selezionati sia > 0
081700981028    2c                   if        wnco = 0
081800981028     c* errore
081900981028     c                   movel     msg(8)        v2cmsg
082000981104     c                   seton                                        4428
082100981104     c     1             chain     fi24s02                            91
082200981104     c  n91              update    fi24s02
082300981028     c                   goto      ectr2
082400981028    2c                   endif
082500981028     c* controllo che il totale numero colli selezionati non sia = al TOT colli mancanti della
082600981028     c* CA .. perchè a questo punto conviene chiudere la CA
082700981104     c* il messaggio di errore è forzabile (E' meglio di no xchè quando richiamo FIDN03 non riesco
082800981104     c* a confermare la CA con colli a zero e non ho neppure la possibilità di tornare indietro)
082900981104     c* 4/11/98
083000981104    2c*********          if        prima = ' '  and                             *********
083100981104     c*       *                    wnco = v2cncn                                *       *
083200981104     c*       *          eval      prima = '1'                                  *       *
083300981104     c*********                                                                 *********
083400981104     c                   if        wnco = v2cncn
083500981028     c                   movel     msg(9)        v2cmsg
083600981104     c                   seton                                        4428
083700981028     c                   goto      ectr2
083800981028    2c                   endif
083900981028     c*
084000981028    1C                   endif
084100981028     c*
084200981028     c     ectr2         ENDSR
084300981028      *****************************************************************
084400981028      *   ROUTINE LETTURA COLLI SELEZIONATI PER CHIUSURA COLLI
084500981028      *****************************************************************
084600981028     C     SUB_COLLI     BEGSR
084700981028     c*
084800981028     c                   clear                   wrknco
084900981028     c                   z-add     1             nrr2
085000981028     c*
085100981028     c                   do        *hival
085200981028     c     nrr2          chain     fi24s02                            33
085300981028     c                   if        not *in33
085400981028     c* controllo se è stato selezionato
085500981028     c                   if        v2ssel = 'X'
085600981028     c                   add       1             wrknco
085700981028     c                   endif
085800981028     c                   add       1             nrr2
085900981028     c                   endif
086000981028     c  n33              enddo
086100981028     c*
086200981028     c* se il numero di colli calcolato è differente dal numero colli calcolato nella precedente
086300981028     c* lettura valorizzo il campo e pulisco "PRIMA" per controllo
086400981028     c                   if        wnco <> wrknco
086500981028     c                   eval      wnco = wrknco
086600981028     c                   eval      prima= ' '
086700981028     c                   endif
086800981028     c*
086900981028     c                   endsr
087000981028      **********************************************************************
087100981028      * CONFERMA CHIUSURA
087200981028      **********************************************************************
087300981028     C     SUB_CONFE     BEGSR
087400981028      * CHIUSURA
087500981028     C*           chiusura CA
087600981028     c                   if        v2ccca = 'SI'
087700981201     c* eseguo aggiornamenti nei file CA per la chiusura
087800981201     c                   EXSR      SUB_CHCA
087900990414     c                   else
088000990414     c* altrimenti se chiusura singolo collo oppure tolgo il flag del singolo collo
088100981028     c*           controllo e aggiorno il dettaglio colli
088200981028     c                   z-add     1             nrr2
088300981028     c*
088400981028     c                   do        *hival
088500981028     c     nrr2          chain     fi24s02                            33
088600981028     c                   if        not *in33
088700981028     c* aggancio il file dettaglio colli
088800981028     c     kdcd2         chain     fndcd02l                           34
088900981028     c* controllo se : è stato dato CMD 6 metto causale + data chiusura nel dettaglio collo
089000981028     c                   if        v2ssel = 'X'
089100990302     c* metto data chiusura e cauasale di chiusura solo  se valorizzati
089200990302     c                   if        wdch > 0
089300981028     c                   eval      dcddch = wdch
089400990302     c*
089500981028     c                   eval      dcdcch = v2ccch
089600981028     c                   endif
089700990525     c*
089800990524     c                   endif
089900990525     c* se ho in gestione la CA  sflaggo il record
090000990525     c*
090100990525     c                   if        not *in11
090200990525     c                   clear                   dcdftr
090300990525     c                   endif
090400990525     c*
090500981028     c                   clear                   dcdfar
090600981028     c* aggiorno il file dettaglio colli
090700981028     c  n34              update    fndcd2
090800981028     c*
090900981028     c                   add       1             nrr2
091000981028     c                   endif
091100981028     c  n33              enddo
091200990414     c*
091300990414     c                   endif
091400981028     c*
091500981028     c* se chiusura dei singoli colli richiamo il PGM di manutenzione  per sistemare
091600981028     c* il numero totale dei colli danneggiati
091700981028     c                   if        v2ccco = 'SI'
091800981028     c                   exsr      SUB_DS00
091900981028     c                   movel     'C'           I00mod
092000981028     c                   movel     fidn00ds      kpjbu
092100981028     c                   CALL      'FIDN03R'
092200981028     c                   parm                    kpjba
092300981028     c                   movel     kpjbu         fidn00ds
092400981028     c                   endif
092500981028     c
092600981028     c* aggiorno il flag di trasmissione della testata  solo nel casi in cui tolgo il flag di
092700981028     c* collo da analizzare senza chiudere niente
092800990524     c                   if        v2ccca = '  ' and v2ccco = ' ' and
092900990524     c                             not *in11
093000990524     c     kdct          chain     fndct01l                           34
093100990524     c                   clear                   dctft1
093200990524     c                   clear                   dctft2
093300990524     c  n34              update    fndct000
093400990524     c                   endif
093500981029     c*
093600981029     c* cerco l'ultimo progressivo note della fase in corso
093700981029     c* scrittura nelle note dei dati relativi alla CA da mantenere
093800981029     c                   movel     v1cfca        dcsnks
093900981029     c                   eval      dcstrc='N'
094000981029     c                   eval      dcspno=0
094100981029     c*
094200981029     c* scrittura note preparazione dei campi strandard
094300981029     c                   eval      dcsatb=' '
094400981029     c                   eval      dcstpd='C'
094500981029     c                   eval      dcsnkt=dsnumca
094600981029     c                   eval      dcsstd=' '
094700981029     c                   eval      dcsdim=dateu
094800981029     c                   eval      dcshim=oramin
094900981029     c*
095000981029     c* cerco l'ultimo progressivo riga relative alla fase attuale CA se esiste
095100981029     c     keydcs        setgt     fndcs01l
095200981029     c     keydcsp       readpe    fndcs01l                               93
095300981029     c*
095400981029     c                   move      knmus         dcspru
095500981029     c                   movel     simfel        dcspos
095600981029     c                   clear                   dcsstn
095700981029     c                   clear                   dcsft1
095800981029     c                   clear                   dcsdt1
095900981029     c* scrivo 2 righe di note per ogni singolo collo ritrovato
096000981029     c                   z-add     1             nrr2
096100981029     c*
096200981029     c                   do        *hival
096300981029     c     nrr2          chain     fi24s02                            33
096400981029     c                   if        not *in33
096500981029     c* Preparo la prima riga di note
096600981029     c                   add       1             dcspno
096700981029     c                   clear                   dcsnot
096800981029     c* tolgo gli 0 non significativi dal numero segnacollo
096900981029     c                   movel     v2snsc        w007a
097000981029     c     '0'           check     w007a         x
097100981029     c                   eval      wAnsc = %subst(w007a:X)
097200981029     c                   movel     v2sfls        wafls
097300050314     c                   movel     v2hnrs        wanrs
097400981029     c                   movel     v2sdfv        wadfv
097500981029     c                   movel     wadfv         wagio
097600981029     c                   move      wadfv         wames
097700981029     c                   movel     v2sfgs        wafgs
097800981029     c                   movel     v2snps        wanps
097900981029     c                   eval      dcsnot = 'Collo '  + wafls + ' ' + wanrs +
098000981029     c                                      ' ' + wansc + ' ritrovato dal'
098100981029     c*
098200981029     c                   write     fndcs000
098300981029     c* scrivo la seconda riga
098400981029     c                   add       1             dcspno
098500981029     c                   clear                   dcsnot
098600981029     c*
098700981030     c                   eval      dcsnot = 'P.O. '  + wafgs + ' il giorno ' +
098800981029     c                                      wagio + '/' + wames + ' pistola ' +
098900981029     c                                      wanps
099000981029     c*
099100981029     c                   write     fndcs000
099200981029     c*
099300981029     c                   add       1             nrr2
099400981029     c                   endif
099500981029     c  n33              enddo
099600981029     c*
099700981028     c                   endsr
099800981201      *****************************************************************
099900981201      * AGGIORNO ARCHIVI   CHIUSURA C.A.
100000981201      *****************************************************************
100100981201     C     SUB_CHCA      BEGSR
100200981201      *
100300981201      * Imposto campi per trasmissione dati
100400981201     c     kdct          chain     fndct01l                           31
100500981201     c                   z-add     dateu         DCTdch
100600981201     c                   z-add     wchca         DCtfca
100700981201     c                   move      wcch          DCTcch
100800981222     c                   clear                   DCTft1
100900981222     c                   clear                   DCTft2
101000981201      *
101100981201      * Aggiorno testata C.A.
101200981201     c                   EXCEPT    UPDdct
101300981201      *
101400981201      * Aggiorno l dettaglio C.A.
101500981201     c                   EXSR      UPDDCD
101600981201      *
101700981201      * Scrivo fasi C.A.
101800981201     c                   EXSR      WRTDCF
101900070403
102000070403      * filiale gestione con procedura telefonate GEO attiva e LNA della spedizione
102100070403      * in £6 e bolla non in distinta e non consegnata propongo lo sblocco della spedizione
102200070403     c                   if        wfilgeo = *on and chisono = 'A' and
102300070403     c                             arbndc = *zeros and arbdcm = *zeros
102400070403     c                   clear                   dslr02
102500070403     c                   eval      dr2aas = dctaas
102600070403     c                   eval      dr2lnp = dctlnp
102700070403     c                   eval      dr2nrs = dctnrs
102800070403     c                   eval      dr2nsp = dctnsp
102900070403     c                   eval      dr2fgs = v1cfgs
103000070403     c                   eval      dr2ric = '1'
103100070403     c                   eval      kpjbu = dslr02
103200070403     c                   call      'FNLR39R'
103300070403     c                   parm                    kpjba
103400070403     c                   eval      dslr02 = kpjbu
103500070403     c                   endif
103600981201      *
103700981201     c                   ENDSR
103800981201      *****************************************************************
103900981201      * AGGIORNO FNDCD Dettaglio C.A.
104000981201      *****************************************************************
104100981201     C     UPDDCD        BEGSR
104200981201     c*
104300981201     c     kdct          setll     fndcd01l
104400981201     c     kdct          reade     fndcd01l                               31
104500981201     c*
104600981201      * Imposto campi per trasmissione dati e pulisco i dati della chiusura C A
104700981201     c                   dow       *in31 = *off
104800981201     c                   eval      DCddch = dctdch
104900981201     c                   eval      DCdcch = dctcch
105000981222     c                   eval      DCdftr = dctft1
105100990414     c* pulizia del flag di collo da analizzare
105200990414     c                   clear                   dcdfar
105300981201     c                   update    fndcd1
105400981201     c*
105500981201     c     kdct          reade     fndcd01l                               31
105600981201     c*
105700981201     c                   enddo
105800981201     c*
105900981201     c                   endsr
106000981201      *****************************************************************
106100981201      * AGGIORNO FNDCF FASI C.A.
106200981201      *****************************************************************
106300981201     C     WRTDCF        BEGSR
106400981201      *
106500981201     C                   z-add     DCTaaC        DCFaac
106600981201     C                   z-add     DCTfil        DCFfil
106700981201     C                   z-add     DCTnca        DCFnca
106800981201     c                   z-add     dctfca        DCFfca
106900981201     c                   z-add     dateu         DCFdfc
107000990414     c                   movel     wora          DCFhfc
107100981201     c                   z-add     V1Cfgs        DCFfev
107200981201     c                   movel     dctptr        DCFptr
107300981201     c                   movel     knmus         DCFpru
107400981201      *
107500981201     c                   WRITE     FNDCF000
107600981201      *
107700981201     c                   ENDSR
107800981028      **********************************************************************
107900981028      * VISUALIZZO NOTE PER STAMPA
108000981028      **********************************************************************
108100981028     C     SUB_stnote    BEGSR
108200981028      *
108300981028     c*
108400981028     c*
108500981028     C                   clear                   FIDN10DS
108600981028     c     *dmy          move      V1Cdca        DATA_eur
108700981028     c     *eur          move      DATA_eur      i10dta
108800981028     c                   movel     'D'           i10trc
108900981028     c                   movel     'T'           i10std
109000981028     c                   movel     'C'           i10tpd
109100981028      *
109200981028     c                   movel     'V'           i10flm
109300981028     c                   movel     dsnumca       i10nkt
109400981028      *
109500981028     c                   movel(P)  fidn10ds      kpjbu
109600981028     C                   CALL      'FIDN10R'
109700981028     C                   PARM                    kpjba
109800981028      *
109900981028     c                   movel     *blanks       WF03
110000981028     C                   movel     KPJBU         fidn10ds
110100981028     c                   if        o10f03    <> *BLANKS
110200981028     C                   movel     'S'           WF03
110300981028     c                   endif
110400981028      *
110500981028     C                   ENDSR
110600981022      *****************************************************************
110700981022      *   ROUTINE SCRITTURA NOTE
110800981022      *****************************************************************
110900981022     C     SUB_NOTE      BEGSR
111000981022     c* valorizzo la DS di gestione note/descrizioni e chiamo il PGM FIDN10R
111100981028     C                   CLEAR                   fidn10ds
111200981030     c     *dmy          move      V1Cdca        DATA_eur
111300981030     c     *eur          move      DATA_eur      i10dta
111400981022     c                   movel     'N'           i10trc
111500981022     c                   movel     'C'           i10tpd
111600981027     c                   movel     v1cfca        i10nks
111700981022     c                   movel     dsnumca       i10nkt
111800981028     c* se sono in gestione note dal subfile le scrivo subito nel file fndcs
111900981028     c                   if        v1csce <> 'G'
112000981028     c                   movel     'V'           i10flm
112100981028     c                   else
112200981028     c* se sono in gestione videata collo ritrovati scrivo al CMD 6
112300981030     c* se lascio 'M' in modo tale che le note vengano confermate quando viene confermata la
112400981030     c* gestione del ritrovamento colli rischio di perdere le note in quanto diversi programmi
112500981030     c* chiamati richiamano a loro volta il PGM fidn10.
112600981030     c* metto la 'V' di manutenzione note potendo così confermare tutte le volte che entro
112700981030     c*                  movel     'M'           i10flm
112800981030     c                   movel     'V'           i10flm
112900981028     c                   endif
113000981028     c*
113100981028     c                   movel(P)  fidn10ds      kpjbu
113200981022     C                   CALL      'FIDN10R'                            99
113300981022     C                   PARM                    kpjba
113400981022     c
113500981022     c                   endsr
113600981028      *****************************************************************
113700981028      *   CARICAMENTO DS FIDN00DS
113800981028      *****************************************************************
113900981028     C     SUB_DS00      BEGSR
114000981028     c*
114100981028     c                   clear                   fidn00ds
114200981028     c*
114300981028     c                   z-add     V1hfca        I00fca
114400981028     c                   z-add     V1Cfgs        I00fgs
114500981028     c                   z-add     V1Haac        I00aac
114600981028     c                   z-add     V1Cfil        I00fil
114700981028     c                   z-add     V1Cnca        I00nca
114800981028     c                   z-add     V1Haas        I00aas
114900981028     c                   z-add     V1Clnp        I00lnp
115000981028     c                   z-add     V1Cnrs        I00nrs
115100981028     c                   z-add     V1Cnsp        I00nsp
115200981028     c                   z-add     V1Hlna        I00lna
115300981028     c                   movel     V1Ctad        I00tad
115400981028     c                   movel     v1htpb        I00tpb
115500981028      *
115600981028     C                   endsr
115700981028      **********************************************************************
115800981028      * IMPOSTO TIPO BOLLA  P/A
115900981028      **********************************************************************
116000981028     C     SUB_TPB       BEGSR
116100981028      *
116200981028     c                   movel     *blanks       wtpb
116300981028      *
116400981028     c                   z-add     V1Haas        Kaas
116500981028     c                   z-add     V1Clnp        Klnp
116600981028     c                   z-add     V1Cnrs        Knrs
116700981028     c                   z-add     V1Cnsp        Knsp
116800981028     c     kspe          chain     fnblp000                           30
116900981028      *
117000981028     c                   if        *IN30 = *OFF
117100981028     c                   movel     'P'           wtpb
117200981028     C                   else
117300981028      *
117400981028     c     kspe          chain     fnarb000                           30
117500981028     c  N30              movel     'A'           wtpb
117600981028     c                   endif
117700981028      *
117800981028     C                   ENDSR
117900981030      **********************************************************************
118000981030      * EMISSIONE VIDEATA RICHIESTA P.O. GESTIONE
118100981030      **********************************************************************
118200981030     C     SUB_FIRST     BEGSR
118300981030      *
118400981030     c                   do        *hival
118500981030     c*
118600981030     c                   exfmt     fi24t01
118700981030     c                   setoff                                       3928
118800981030     c* KC = fine
118900981030     c                   if        not *inkc
119000981030     c* ks = cambio p.o. gestione
119100981030     c   ks              eval      *in05 = *on
119200981030     c   ks              iter
119300981030     c*
119400981030      * 05 on --> controllo P.O. in gestione
119500981030     c     *in05         ifeq      *on
119600981030     C                   exsr      SUB_CFGS
119700981030     c   28              iter
119800981030     c                   eval      *in05 = *off
119900981030     c                   leave
120000981030     c                   end
120100981030     c* se è stato dato solo enter vuol dire che deve proseguire
120200981030     c                   leave
120300981201     c                   else
120400981201     c                   leave
120500981030     c                   end
120600981030    3c                   enddo
120700981030     c                   endsr
120800981028      **********************************************************************
120900981028      * CONTROLLO IL P.O. IN IN GESTIONE
121000981028      **********************************************************************
121100981029     C     SUB_CFGS      BEGSR
121200981028      *
121300981029     c                   setoff                                       3928
121400981029     c                   clear                   v1dfgs
121500981029     C                   clear                   FNLV50DS
121600981029     C                   MOVEL     KNMUS         D50PRU
121700981029     C                   MOVEL     V1cfgs        D50FGS
121800170428     c                   eval      d50tcn='E'
121900981029     C                   CALL      'FNLV50R'
122000981029     C                   PARM                    FNLV50DS
122100981028      *
122200981029     C                   if        D50ERR <> *blanks
122300981029     C                   MOVEL     D50MSG        V1cMSG
122400981029     C                   SETON                                        3928
122500981029     C                   else
122600070403     c                   clear                   og148
122700981029     c     v1cfgs        CHAIN     azorg                              31
122800981029     c  n31              MOVEL     orgdes        v1dfgs
122900070403     c  n31              eval      og148 = orgde8
123000070403      * controllo se Filiale gestione è abilitata a telefonate GEO
123100070403     c                   eval      wfilgeo = (§oggeot = 'S')
123200981029     C                   endif
123300981028      *
123400981029     C  n28              EXSR      CARL6
123500981029     c                   endsr
123600981028      **************************************************************************
123700981028      * CARICO SCHIERA L6 DA TABELLA £6
123800981028      **************************************************************************
123900981028     C     carl6         BEGSR
124000981028      *
124100981028     C                   CLEAR                   L6
124200981028     C                   CLEAR                   TRUL06DS
124300981028     C                   MOVE      '£6'          D06COD
124400981028     C                   MOVEL     v1cfgs        D06key
124500981028     C                   MOVEL(P)  TRUL06DS      KPJBU
124600981028     C                   CALL      'TRUL06R'
124700981028     C                   PARM                    KPJBA
124800981028     C                   MOVEL     KPJBU         TRUL06DS
124900981028     C                   MOVEA     LIN           L6
125000981028      *
125100981028     c                   endsr
125200981022      *****************************************************************
125300981022      *   ROUTINE INIZIALE
125400981022      *****************************************************************
125500981022     C     *INZSR        BEGSR
125600981022      *
125700981022     C     *ENTRY        PLIST
125800981022     C                   PARM                    KPJBA
125900981022      *
126000170428     C                   Z-ADD     1             CODUT             1 0
126100170428      *
126200170428     c     *dtaara       define    §azute        azuteds
126300170428     c     *dtaara       define    §datiute      ddatiute
126400170428      *
126500170428     c                   in(E)     *dtaara
126600170428     c                   IF        %ERROR or RSUT = *blanks
126700170428     c                   clear                   Tibs34Ds
126800170428     c                   call      'TIBS34R'
126900170428     c                   parm                    Tibs34Ds
127000170428     c                   in        *dtaara
127100170428     c                   ENDIF
127200170428      *
127300981029      * titolo videata e opzioni
127400981029     c                   movel     tit           vtctit
127500981029     c                   movel     dopz          v1dse1
127600981022      *
127700981027     C                   TIME                    W0140
127800981022     C                   MOVE      W0140         WDTGIO
127900981022     C                   MOVEL     W0140         WORA
128000981029     C                   MOVEL     Wora          oramin
128100981022      * UDATE IN AAAAMMGG
128200981022     C                   Z-ADD     WDTGIO        G02DAT
128300981022     C                   MOVEL     *BLANK        G02ERR
128400981022     C                   CALL      'XSRDA8'
128500981022     C                   PARM                    WLBDAT
128600981022     C                   MOVEL     G02INV        DATEU
128700981028
128800981028      * IMPOSTO IL P.O. IN GESTIONE
128900170428     c     dutlpo        ifeq      '2'
129000170428     c     dutlpo        oreq      *blanks
129100170428     c                   movel     dutpou        v1cfgs
129200020502     C                   eval      *in06 = *on
129300020502     c                   else
129400020502     c                   movel     simfel        v1cfgs
129500020502     c                   endif
129600170428
129700170428     c* decodifico se non sono la sede
129800981029     c                   if        v1cfgs > 0
129900170428     c**                 clear                   og148
130000170428     C**   V1CFGS        CHAIN     AZORG                              31
130100170428     C**N31              MOVEL     orgDES        V1DFGS
130200170428     c**n31              eval      og148 = orgde8
130300070403      * controllo se Filiale gestione è abilitata a telefonate GEO
130400170428     c**                 eval      wfilgeo = (§oggeot = 'S')
130500170428     C**                 exsr      CARL6
130600170428
130700170428     c                   exsr      sub_cfgs
130800981029    2C                   ENDIF
130900981027      *
131000981027     C* ACCESSO   TABEL
131100981027     C     KTAB          KLIST
131200981027     C                   KFLD                    CODUT
131300981027     C                   KFLD                    §COD
131400981027     C                   KFLD                    §KEY
131500981027      * accesso fndcd03l
131600981027     c     Kdcd3         KLIST
131700981027     C                   KFLD                    kfar
131800981028     C                   KFLD                    kaac
131900981028     C                   KFLD                    kfil
132000981028     C                   KFLD                    knca
132100981028      * accesso fndcd02l
132200981028     c     Kdcd2         KLIST
132300981028     c                   kfld                    v2sfls
132400050314     c                   kfld                    v2hlna
132500050314     c                   kfld                    v2hnrs
132600981028     c                   kfld                    v2snsc
132700981028     C                   KFLD                    kaac
132800981028     C                   KFLD                    kfil
132900981028     C                   KFLD                    knca
133000981027      * accesso fndct01l
133100981027     C     Kdct          klist
133200981027     C                   KFLD                    kaac
133300981027     C                   KFLD                    kfil
133400981027     C                   KFLD                    knca
133500981027      * accesso archivio bolle
133600991116     c     kspe          klist
133700991116     c                   kfld                    kaas
133800991116     c                   kfld                    klnp
133900991116     c                   kfld                    knrs
134000991116     c                   kfld                    knsp
134100991116     c*
134200991116     c     kar6          klist
134300981028     c                   kfld                    kaas
134400981028     c                   kfld                    klnp
134500981028     c                   kfld                    knrs
134600991116     c                   kfld                    knsp
134700991116     c                   kfld                    trcar6
134800981029     c*
134900981029     c     keydcs        KLIST
135000981029     c                   KFLD                    dcstpd
135100981029     c                   KFLD                    dcsnkt
135200981029     c                   KFLD                    dcsstd
135300981029     c                   KFLD                    dcsdim
135400981029     c                   KFLD                    dcshim
135500981029     c                   KFLD                    dcsnks
135600981029     c                   KFLD                    dcstrc
135700981029     c                   KFLD                    prgnot
135800981029     c                   eval      prgnot= 999
135900981029     c* di fndcs01l - parziale
136000981029     c*
136100981029     c     keydcsp       KLIST
136200981029     c                   KFLD                    dcstpd
136300981029     c                   KFLD                    dcsnkt
136400981029     c                   KFLD                    dcsstd
136500981029     c                   KFLD                    dcsdim
136600981029     c                   KFLD                    dcshim
136700981029     c                   KFLD                    dcsnks
136800981029     c                   KFLD                    dcstrc
136900981022      *
137000981022     C                   ENDSR
137100981201      *---------------------------------------------------------------------------------------------
137200981201     oFNDCT000  E            UPDDCT
137300981201     O                       DCTdch
137400981201     O                       DCTcch
137500981201     O                       DCTfca
137600981222     O                       DCTft1
137700981222     O                       DCTft2
137800981022      *****************************************************************
137900981028**
138000981028Non si possono selezionare entrambe le chiusure                                1
138100981028Inserire la causale chiusura                                                   2
138200981028Causale Chiusura inesistente                                                   3
138300981028Data Chiusura errata                                                           4
138400981028Inserire Data Chiusura                                                         5
138500981028Data Chiusura deve essere maggiore della data di apertura                      6
138600981028Data Chiusura non deve essere maggiore della data del giorno                   7
138700981028Selezionare i segnacolli da chiudere                                           8
138800981104Il tot. segnacolli Selez. è = al tot. colli mancanti CA (Chiudera la C.A.)     9
138900170428C.A. non manutenzionabile: in gestione ad altra filiale                       10
139000990225C.A. non manutenzionabile perchè chiusa                                       11

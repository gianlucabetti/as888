000100990212     H DECEDIT('0,') DATEDIT(*DMY.)
000200981203      *----------------------------------------------------*
000300990325      *  ELENCO C.A. DA ARCHIVIARE
000400990212      *----------------------------------------------------*
000500990325     FFIDN45D   CF   E             WORKSTN
000600981203     FAZORG01L  IF   E           K DISK
000700981203
000800990325     D WAMGDHD         s                   like(V1cdhd)
000900990325     D WAMGDHA         s                   like(V1cdhd)
001000990325     D Wultdta         s                   like(V1cdhd)
001100990215     D WFIL            s                   like(V1cFIL)
001200990423     D WDATA           S               D   DATFMT(*iso)
001300020604     d sav_43ds        s                   like(fidn43ds)
001400020604
001500020604     d xx              s              2  0
001600981203
001700020604     D MSG             S             78    DIM(8) CTDATA PERRCD(1)              MSG VIDEO
001800020604     d skdata          s              8  0 dim(30)
001900981203
002000981203     D WLBDAT          DS                  INZ
002100981203     D  G02DAT                 1      8  0
002200981203     D  G02INV                 9     16  0
002300981203     D  G02ERR                17     17
002400981203     D  G02TGI                18     22  0
002500020604
002600020604     d                 ds                  inz
002700020604     d wdatamax                1      8  0
002800020604     d  wdataaa                1      4
002900020604     d  wdatamm                5      6
003000020604     d  wdatagg                7      8
003100981203
003200990325     D CNCR80        E DS
003300990212     D FIDN43DS      E DS
003400990325     D FNLV50DS      E DS
003500990212     D TIBS02DS      E DS
003600990315     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003700990325     D DDCB          E DS
003800981203
003900020604     d trul06ds      e ds
004000020604     d  lin                    1     90  0 dim(30)
004100990325      *
004200981203     D KPJBA         E DS
004300020502     D* rem                    1      3
004400020502     D* remfil                 4      6
004500990315     D
004600990315     D W0140           S             14  0
004700990315     D Wdtgio          S              8  0
004800990315     D dateu           S              8  0
004900990315     D
005000981203
005100981203      *------------------------------------------------------------------------*
005200990215     c* INDICATORI
005300990215      *------------------------------------------------------------------------*
005400990325      *  01   - on - sono in sede    off - sono in filiale
005500990325      *  05   - 2 livello
005600990325      *  06   - dalla data protetto in caso di aggiornamento e stampa
005700990422      *  10   - sproteggo le richieste della ristampa
005800990325      *  21/23- errori posizionamento cursore sui campi
005900990325      *  28   - emetto campo messaggio
006000990325      *  30/31- comodo
006100990325      *  50   - proteggo e non visualizzo le date da richiedere
006200990215      *------------------------------------------------------------------------*
006300981203     c*
006400990325     c                   do        *hival
006500990325     c*
006600990325     c                   exfmt     dn45d01
006700990212     c**
006800990325     c                   setoff                                       289021
006900990423     c                   setoff                                       222324
007000981203     c                   clear                   v1cmsg
007100981203     c* F3 = FINE
007200990325     c   KC              leave
007300990212     c*
007400990326     c* F12 = Precedente
007500990326     c                   if        *inkl
007600990423     c                   setoff                                       5010
007700990326     c*
007800990326     c                   iter
007900990326     c                   endif
008000990326     c*
008100990212      * Controlli
008200990212     c                   exsr      CTRD01
008300990325     c   90              iter
008400990212     c*
008500990212     c* Nessuna selezione
008600990212     c
008700990212     c   KF              exsr      conferma
008800990325     c   KF              leave
008900990212     c**
009000990325     c                   enddo
009100990212     c
009200981203      *
009300990212     c     fine          tag
009400990325      *
009500990325     c                   clear                   tibs02ds
009600990325     C                   MOVEL     'C'           t02tla
009700990325      *
009800990325     C                   CALL      'TIBS02R'
009900990325     C                   PARM                    KPJBA
010000990325     C                   PARM                    TIBS02DS
010100990315      *
010200981203     c                   eval      *inlr = *on
010300981203      *--------------------------------------------------------------------*
010400990212      * CONTROLLI VIDEO 1
010500981203      *--------------------------------------------------------------------*
010600981203     C     Ctrd01        BEGSR
010700990212     c
010800990325     c** controllo i campi per 50 spento
010900990325     c                   if        not *in50
011000981203
011100990212     c* P.O. Apertura C.A.
011200990212     c                   clear                   v1dfil
011300990423     c                   if        v1cfil>0
011400990212     c                   eval      wfil=v1cfil
011500990212     c                   exsr      CTRPO
011600020604     c                   else
011700020604     c                   seton                                        289021
011800020604     c                   eval      v1cmsg = msg(7)
011900990325     c                   endif
012000990325     c*
012100990325     c  n28              seton                                        5090
012200990325     c*
012300990325     c                   else
012400990325     c*
012500990219     c
012600990325     c** controllo i campi per 50 acceso
012700990325     c
012800990218     c* Data chiusura C.A.
012900990325     c                   if        v1cdhd = 0 and v1cdha > 0
013000990325     C                   seton                                        222890
013100990325     c                   movel     MSG(2)        v1cmsg
013200990325     c                   goto      erro
013300990218     C                   ENDIF
013400990218     c
013500990218     c                   clear                   wamgdhd
013600990218     c                   clear                   wamgdha
013700990325     c                   if        v1cdhd > 0
013800990218     C                   move      V1cdhd        G02DAT
013900990218     C                   eval      g02err = *blanks
014000990218     C                   call      'XSRDA8'
014100990218     C                   parm                    WLBDAT
014200990218      *
014300990218     C                   if        g02err = '1'
014400990325     C                   seton                                        222890
014500990325     C                   movel     msg(2)        v1cmsg
014600990325     c                   goto      erro
014700990218     c                   else
014800990218     c                   eval      wamgdhd=g02inv
014900990218     c                   eval      v1cdhd =g02dat
015000990218     c                   endif
015100990218     c                   endif
015200990218     c
015300990325     c                   if        v1cdha > 0
015400990218     C                   move      V1cdha        G02DAT
015500990218     C                   eval      g02err = *blanks
015600990218     C                   call      'XSRDA8'
015700990218     C                   parm                    WLBDAT
015800990218      *  Errore
015900990218     C                   if        g02err = '1'
016000990325     C                   seton                                        232890
016100990325     C                   movel     msg(2)        v1cmsg
016200990325     c                   goto      erro
016300990218     c                   else
016400990218     c                   eval      wamgdha=g02inv
016500990218     c                   eval      v1cdha =g02dat
016600990423     c                   endif
016700990423     c                   endif
016800990218     c
016900990325     c                   if        wamgdha < wamgdhd
017000990423     C                   seton                                        282390
017100990325     C                   movel     msg(3)        v1cmsg
017200990325     c                   goto      erro
017300990218     c                   endif
017400990422     c* in caso di ristampa la data finale non deve essere maggiore dell'ultima data richiesta
017500990422     c*
017600990422     c                   if        *in10 and  wamgdha > wultdta
017700990422     C                   seton                                        282290
017800990422     C                   movel     msg(5)        v1cmsg
017900990422     c                   goto      erro
018000990422     c                   endif
018100990423     c* in caso di ristampa si deve selezionare almeno una delle due stampe
018200990423     c*
018300990423     c                   if        *in10 and v1crca = '  ' and v1crbo = '  '
018400990423     C                   seton                                        282490
018500990423     C                   movel     msg(6)        v1cmsg
018600990423     c                   goto      erro
018700990423     c                   endif
018800990422     c*
018900990422     c* controllo la data  "al"  che non deve essere maggiore di oggi
019000990326     c                   if        wamgdha > dateu
019100990326     C                   seton                                        282290
019200990326     C                   movel     msg(4)        v1cmsg
019300990326     c                   goto      erro
019400990326     c                   endif
019500020604
019600020604      * la data "al" non deve essere minore della data massima trovata in tabella
019700020604     c                   if        wamgdha < wdatamax
019800020604     c                   seton                                        282290
019900020604     c                   movel     msg(8)        v1cmsg
020000020604     c                   eval      %subst(v1cmsg:35:2) = wdatagg
020100020604     c                   eval      %subst(v1cmsg:37:1) = '/'
020200020604     c                   eval      %subst(v1cmsg:38:2) = wdatamm
020300020604     c                   eval      %subst(v1cmsg:40:1) = '/'
020400020604     c                   eval      %subst(v1cmsg:41:4) = wdataaa
020500020604     c                   goto      erro
020600020604     c                   endif
020700990325     c*
020800990325     c                   endif
020900990212     c
021000990325     c     Erro          ENDSR
021100990325      *------------------------------------------------------------------------*
021200990325      * RECUPERO ULTIMA DATA DI CHIUSURA PER FILIALE
021300990325      *------------------------------------------------------------------------*
021400990325     C     REC_DTA       BEGSR
021500990325     c*
021600990401     c                   eval      *in06 = *off
021700990422     c                   eval      *in10 = *off
021800990325     c                   clear                   wultdta
021900990325     c                   clear                   tibs02ds
022000990325     C                   MOVEL     'C'           t02mod
022100990325     C                   MOVEL     knsif         t02sif
022200990325     C                   MOVEL     'DCB'         t02cod
022300990325     c                   movel     v1cfil        t02ke1
022400990325      *
022500990325     C                   CALL      'TIBS02R'
022600990325     C                   PARM                    KPJBA
022700990325     C                   PARM                    TIBS02DS
022800990325      *
022900990325     C                   IF        T02err = *BLANKS
023000990325     C                   movel     T02uni        DDCB
023100990325     c                   movel     §dcbudta      wultdta
023200990325     c
023300990325     c                   else
023400990402     C                   eval      wultdta = 19990101
023500990325     C                   endif
023600020604
023700020604      * carico le date di elaborazione delle filiali gestite
023800020604     c                   z-add     *zeros        wdatamax
023900020604     c                   do        30            xx
024000020604     c                   if        lin(xx) = *zeros
024100020604     c                   leave
024200020604     c                   endif
024300020604     c                   clear                   tibs02ds
024400020604     c                   eval      t02mod = 'C'
024500020604     c                   eval      t02sif = knsif
024600020604     c                   eval      t02cod = 'DCB'
024700020604     c                   movel     lin(xx)       t02ke1
024800020604     c                   call      'TIBS02R'
024900020604     c                   parm                    kpjba
025000020604     c                   parm                    tibs02ds
025100020604     c                   if        t02err = *blanks
025200020604     c                   movel     t02uni        ddcb
025300020604     c                   movel     §dcbudta      skdata(xx)
025400020604     c                   else
025500020604     c                   eval      skdata(xx) = 19990101
025600020604     c                   endif
025700020604     c                   if        skdata(xx) > wdatamax
025800020604     c                   eval      wdatamax = skdata(xx)
025900020604     c                   endif
026000020604     c                   enddo
026100990325     c*
026200990325     c* giro la data
026300990325     C                   clear                   WLBDAT
026400990325     C                   z-add     wultdta       G02inv
026500990325     C                   movel     '3'           G02err
026600990325     C                   CALL      'XSRDA8'
026700990325     C                   PARM                    WLBDAT
026800990325     c* se richiesta la ristampa imposto la data trovata come finale altrimenti come iniziale
026900990325     c                   if        v1cese = 'S'
027000990325     c                   z-add     g02dat        v1cdha
027100990423     c                   clear                   v1cdhd
027200990422     c                   eval      *in10 = *on
027300990325     c                   else
027400990325     c                   z-add     g02dat        v1cdhd
027500990503     c     *eur          move      v1cdhd        wdata
027600990503     c                   adddur    1:*d          wdata
027700990503     c     *eur          move      wdata         v1cdhd
027800990423     c*
027900990423     c* calcolo la data finale  aggiungo un mese
028000990423     C                   clear                   WLBDAT
028100990423     c     *iso          move      wultdta       wdata
028200100603     c* Se si tratta dell'ultimo giorno del mese, asso al successivo ult mese
028300100603     c                   adddur    1:*d          wdata
028400100603     c     *iso          move      wdata         g02inv
028500100603     c                   move      g02inv        wgg               2 0
028600100603     c
028700100603     c                   if        wgg=1
028800100603     c                   adddur    1:*m          wdata
028900100603     c                   subdur    1:*d          wdata
029000100603     c                   else
029100100603     c
029200100603     c     *iso          move      wultdta       wdata
029300100603     c                   adddur    1:*m          wdata
029400100603     c                   endif
029500100603     c
029600990423     c     *iso          move      wdata         g02inv
029700990423     C                   movel     '3'           G02err
029800990423     C                   CALL      'XSRDA8'
029900990423     C                   PARM                    WLBDAT
030000990423     c                   z-add     g02dat        v1cdha
030100990401     c* e proteggo il campo
030200990401     c                   eval      *in06 = *on
030300990325     c                   endif
030400990325     C
030500990325     c                   endsr
030600990212      *------------------------------------------------------------------------*
030700990212      * CPNTROLLO P.O.
030800990212      *------------------------------------------------------------------------*
030900990212     C     CTRPO         BEGSR
031000990325     c*
031100990325     c                   clear                   v1dfil
031200990325     C                   clear                   FNLV50DS
031300990325     C                   MOVEL     KNMUS         D50PRU
031400990325     C                   MOVEL     Wfil          D50FGS
031500990325     C                   CALL      'FNLV50R'
031600990325     C                   PARM                    FNLV50DS
031700990325      *
031800020606     C                   if        D50ERR <> *blanks
031900990325     C                   MOVEL     D50MSG        V1cMSG
032000990325     C                   SETON                                        219028
032100990325     C                   else
032200990325      *
032300990325     c* controllo se appartiene alla £1
032400990212     c     wfil          chain     azorg01l                           30
032500990212     c     *in30         ifeq      *on
032600990212     c     orgfva        orne      ' '
032700990212     c     orgfag        orne      'A'
032800990212     c     orgfag        andne     'F'
032900990325     c                   eval      v1cmsg=msg(1)
033000990325     c                   seton                                        289021
033100990212     c                   endif
033200990325     c  n90              eval      v1dfil = orgdes
033300990325      *
033400990325     c                   endif
033500020604      * recupero le filiali gestite dal p.o. immesso a video
033600020604     c                   if        not *in90
033700020604     c                   clear                   trul06ds
033800020604     c                   eval      d06cod = '£6'
033900020605     c                   eval      d06esc = 'S'
034000020604     c                   movel     wfil          d06key
034100020604     c                   movel(p)  trul06ds      kpjbu
034200020604     c                   call      'TRUL06R'
034300020604     c                   parm                    kpjba
034400020604     c                   movel     kpjbu         trul06ds
034500020604     c                   endif
034600990325      *
034700990325     c* per filiale trovata recupero la data ultima stampa
034800990325     c  n90              exsr      rec_dta
034900990325     c*
035000990212     c                   ENDSR
035100990215      *------------------------------------------------------------------------*
035200990215      * SOTTOMENTTO ELABORAZIONE
035300990215      *------------------------------------------------------------------------*
035400990212     c     CONFERMA      BEGSR
035500990325      *
035600990212     c                   clear                   fidn43ds
035700990325     c                   eval      i43ord = '1'
035800990325     c                   eval      i43ftt = 'C'
035900990420     c                   eval      i43usi = 'S'
036000990212     c                   eval      i43fil = v1cfil
036100990218     c                   eval      i43dhd = v1cdhd
036200990218     c                   eval      i43dha = v1cdha
036300990218     c                   eval      i43amgdhd = wamgdhd
036400990218     c                   eval      i43amgdha = wamgdha
036500990315     c
036600990212      *
036700990212     c                   movel     fidn43ds      kpjbu
036800990212     c
036900990422     c* richiesta ristampa ma non delle CA  non chiamo il pgm
037000990422     c                   if        *in10 and v1crca = '  '
037100990422     c                   else
037200101112     c                   eval      kcoaz='DN47'
037300990212     c                   call      'BCH10'
037400990212     c                   parm                    kpjba
037500020604      * richiamo il pgm fidn44r per ogni p.o. gestito
037600020604     c                   exsr      sr_lancia
037700990422     c                   endif
037800990325      *
037900990325      * se si tratta di aggiornamento con stampa richiamo il programma di chiusura
038000990325      * bolle in CA
038100990325      *
038200990422     c                   if        v1cese = 'A' or (*in10 and v1crbo = 'SI')
038300990325     c                   movel     fidn43ds      kpjbu
038400990325      *
038500990325     c                   eval      kcoaz='DN46'
038600020605     c                   call      'BCH10'
038700990325     c                   parm                    kpjba
038800990325     c                   endif
038900990325      *
039000990212     c                   endsr
039100020604      *------------------------------------------------------------------------*
039200020604      * Lancio del FIDN44R per ogni elemento di LIN
039300020604      *------------------------------------------------------------------------*
039400020604     c     sr_lancia     begsr
039500020604
039600020604      * salvo la ds per il lancio del pgm fidn46
039700020604     c                   eval      sav_43ds = fidn43ds
039800020604
039900020604      * leggo la schiera dei p.o. gestiti
040000020604     c                   do        30            xx
040100020604
040200020604     c                   if        lin(xx) = *zeros
040300020604     c                   leave
040400020604     c                   endif
040500020604
040600020604      * imposto i dati nella ds esterna
040700020604     c                   clear                   fidn43ds
040800020604     c                   eval      i43ord = '1'
040900020604     c                   eval      i43ftt = 'C'
041000020604     c                   eval      i43usi = 'S'
041100020604     c                   eval      i43fil = lin(xx)
041200020604
041300020605      * giro data trovata in tabella se non è ristampa
041400020605     c                   if        not *in10
041500020604     c                   clear                   wlbdat
041600020604     c                   z-add     skdata(xx)    g02inv
041700020604     c                   movel     '3'           g02err
041800020604     c                   call      'XSRDA8'
041900020604     c                   parm                    wlbdat
042000020604     c                   z-add     g02dat        i43dhd
042100020605     c                   eval      i43amgdhd = skdata(xx)
042200020605     c                   endif
042300020605      * se è ristampa passo la data impostata a video
042400020605     c                   if        *in10
042500020605     c                   eval      i43dhd = v1cdhd
042600020605     c                   eval      i43amgdhd = wamgdhd
042700020605     c                   endif
042800020604     c
042900020604     c                   eval      i43dha = v1cdha
043000020604     c                   eval      i43amgdha = wamgdha
043100020604
043200020604     c                   movel     fidn43ds      kpjbu
043300110202     c*                  eval      kcoaz='DN44'
043301110202     c                   eval      kcoaz='DN47'
043400020604     c                   call      'BCH10'
043500020604     c                   parm                    kpjba
043600020604
043700020604     c                   enddo
043800020604
043900020604      * reimposto la ds per il lancio del pgm fidn46
044000020604     c                   eval      fidn43ds = sav_43ds
044100020604
044200020604     c                   endsr
044300990325      *------------------------------------------------------------------------*
044400990325      * ROUTINE INIZIALE
044500990325      *------------------------------------------------------------------------*
044600990325     C     *INZSR        BEGSR
044700990325      *
044800990325     C     *ENTRY        PLIST
044900990325     C                   PARM                    KPJBA
045000990325      *
045100990325     C                   Z-ADD     1             CODUT
045200990325     C                   CALL      'X§PARUT'
045300990325     C                   PARM                    UT§DSE
045400990325     C                   MOVEL     RAGUT         RSUT
045500990325     C                   MOVEL     REC80         CNCR80
045600990325     c
045700990325
045800990325      * reperisco data e ora
045900990325     C                   TIME                    W0140
046000990325      * UDATE IN GGMMAAAA
046100990325     C                   MOVE      W0140         WDTGIO
046200990325      * UDATE IN AAAAMMGG
046300990325     C                   Z-ADD     WDTGIO        G02DAT
046400990325     C                   MOVEL     *BLANK        G02ERR
046500990325     C                   CALL      'XSRDA8'
046600990325     C                   PARM                    WLBDAT
046700990325     C                   MOVEL     G02INV        DATEU
046800990325      *
046900990325      * IMPOSTO I PRIMI DATI A VIDEO
047000990325     c                   eval      v1cese='A'
047100990325     c
047200990325      * IMPOSTO IL P.O. IN GESTIONE
047300020502     C                   IF        simtpp = '2' or simtpp = *blanks
047400020502     c                   movel     simpou        v1cfil
047500990325     C                   eval      *in05 = *on
047600990325     C                   ELSE
047700990325     C                   movel     simfel        V1Cfil
047800990325     C                   ENDIF
047900990325      * DECODIFICO
048000990325     c     v1cfil        chain     azorg01l                           30
048100990325     c                   if        not *in30
048200990325     c                   eval      v1dfil = orgdes
048300990325     c                   endif
048400990325      *
048500990325     C                   ENDSR
048600981203      *---------------------------------------------------------------------------------------------
048700981203** MSG  Lungh. 78                                                            *
048800990325Punto operativo inesistente                                                    1
048900990325Data errata                                                                    2
049000990325Data AL   MINORE  data DAL                                                     3
049100990326Data AL non può essere maggiore della data odierna                             4
049200990422Data AL non può essere maggiore dell'ultima data di elaborazione               5
049300990422Immettere almeno una scelta                                                    6
049400020604Immettere il punto operativo                                                   7
049500020604Data AL non può essere minore del xxxxxxxxxx                                   8

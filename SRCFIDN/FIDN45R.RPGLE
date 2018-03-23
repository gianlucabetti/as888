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
004300990315     D
004400990315     D W0140           S             14  0
004500990315     D Wdtgio          S              8  0
004600990315     D dateu           S              8  0
004700990315     D
004800981203
004900981203      *------------------------------------------------------------------------*
005000990215     c* INDICATORI
005100990215      *------------------------------------------------------------------------*
005200990325      *  01   - on - sono in sede    off - sono in filiale
005300990325      *  05   - 2 livello
005400990325      *  06   - dalla data protetto in caso di aggiornamento e stampa
005500990422      *  10   - sproteggo le richieste della ristampa
005600990325      *  21/23- errori posizionamento cursore sui campi
005700990325      *  28   - emetto campo messaggio
005800990325      *  30/31- comodo
005900990325      *  50   - proteggo e non visualizzo le date da richiedere
006000990215      *------------------------------------------------------------------------*
006100981203     c*
006200990325     c                   do        *hival
006300990325     c*
006400990325     c                   exfmt     dn45d01
006500990212     c**
006600990325     c                   setoff                                       289021
006700990423     c                   setoff                                       222324
006800981203     c                   clear                   v1cmsg
006900981203     c* F3 = FINE
007000990325     c   KC              leave
007100990212     c*
007200990326     c* F12 = Precedente
007300990326     c                   if        *inkl
007400990423     c                   setoff                                       5010
007500990326     c*
007600990326     c                   iter
007700990326     c                   endif
007800990326     c*
007900990212      * Controlli
008000990212     c                   exsr      CTRD01
008100990325     c   90              iter
008200990212     c*
008300990212     c* Nessuna selezione
008400990212     c
008500990212     c   KF              exsr      conferma
008600990325     c   KF              leave
008700990212     c**
008800990325     c                   enddo
008900990212     c
009000981203      *
009100990212     c     fine          tag
009200990325      *
009300990325     c                   clear                   tibs02ds
009400990325     C                   MOVEL     'C'           t02tla
009500990325      *
009600990325     C                   CALL      'TIBS02R'
009700990325     C                   PARM                    KPJBA
009800990325     C                   PARM                    TIBS02DS
009900990315      *
010000981203     c                   eval      *inlr = *on
010100981203      *--------------------------------------------------------------------*
010200990212      * CONTROLLI VIDEO 1
010300981203      *--------------------------------------------------------------------*
010400981203     C     Ctrd01        BEGSR
010500990212     c
010600990325     c** controllo i campi per 50 spento
010700990325     c                   if        not *in50
010800981203
010900990212     c* P.O. Apertura C.A.
011000990212     c                   clear                   v1dfil
011100990423     c                   if        v1cfil>0
011200990212     c                   eval      wfil=v1cfil
011300990212     c                   exsr      CTRPO
011400020604     c                   else
011500020604     c                   seton                                        289021
011600020604     c                   eval      v1cmsg = msg(7)
011700990325     c                   endif
011800990325     c*
011900990325     c  n28              seton                                        5090
012000990325     c*
012100990325     c                   else
012200990325     c*
012300990219     c
012400990325     c** controllo i campi per 50 acceso
012500990325     c
012600990218     c* Data chiusura C.A.
012700990325     c                   if        v1cdhd = 0 and v1cdha > 0
012800990325     C                   seton                                        222890
012900990325     c                   movel     MSG(2)        v1cmsg
013000990325     c                   goto      erro
013100990218     C                   ENDIF
013200990218     c
013300990218     c                   clear                   wamgdhd
013400990218     c                   clear                   wamgdha
013500990325     c                   if        v1cdhd > 0
013600990218     C                   move      V1cdhd        G02DAT
013700990218     C                   eval      g02err = *blanks
013800990218     C                   call      'XSRDA8'
013900990218     C                   parm                    WLBDAT
014000990218      *
014100990218     C                   if        g02err = '1'
014200990325     C                   seton                                        222890
014300990325     C                   movel     msg(2)        v1cmsg
014400990325     c                   goto      erro
014500990218     c                   else
014600990218     c                   eval      wamgdhd=g02inv
014700990218     c                   eval      v1cdhd =g02dat
014800990218     c                   endif
014900990218     c                   endif
015000990218     c
015100990325     c                   if        v1cdha > 0
015200990218     C                   move      V1cdha        G02DAT
015300990218     C                   eval      g02err = *blanks
015400990218     C                   call      'XSRDA8'
015500990218     C                   parm                    WLBDAT
015600990218      *  Errore
015700990218     C                   if        g02err = '1'
015800990325     C                   seton                                        232890
015900990325     C                   movel     msg(2)        v1cmsg
016000990325     c                   goto      erro
016100990218     c                   else
016200990218     c                   eval      wamgdha=g02inv
016300990218     c                   eval      v1cdha =g02dat
016400990423     c                   endif
016500990423     c                   endif
016600990218     c
016700990325     c                   if        wamgdha < wamgdhd
016800990423     C                   seton                                        282390
016900990325     C                   movel     msg(3)        v1cmsg
017000990325     c                   goto      erro
017100990218     c                   endif
017200990422     c* in caso di ristampa la data finale non deve essere maggiore dell'ultima data richiesta
017300990422     c*
017400990422     c                   if        *in10 and  wamgdha > wultdta
017500990422     C                   seton                                        282290
017600990422     C                   movel     msg(5)        v1cmsg
017700990422     c                   goto      erro
017800990422     c                   endif
017900990423     c* in caso di ristampa si deve selezionare almeno una delle due stampe
018000990423     c*
018100990423     c                   if        *in10 and v1crca = '  ' and v1crbo = '  '
018200990423     C                   seton                                        282490
018300990423     C                   movel     msg(6)        v1cmsg
018400990423     c                   goto      erro
018500990423     c                   endif
018600990422     c*
018700990422     c* controllo la data  "al"  che non deve essere maggiore di oggi
018800990326     c                   if        wamgdha > dateu
018900990326     C                   seton                                        282290
019000990326     C                   movel     msg(4)        v1cmsg
019100990326     c                   goto      erro
019200990326     c                   endif
019300020604
019400020604      * la data "al" non deve essere minore della data massima trovata in tabella
019500020604     c                   if        wamgdha < wdatamax
019600020604     c                   seton                                        282290
019700020604     c                   movel     msg(8)        v1cmsg
019800020604     c                   eval      %subst(v1cmsg:35:2) = wdatagg
019900020604     c                   eval      %subst(v1cmsg:37:1) = '/'
020000020604     c                   eval      %subst(v1cmsg:38:2) = wdatamm
020100020604     c                   eval      %subst(v1cmsg:40:1) = '/'
020200020604     c                   eval      %subst(v1cmsg:41:4) = wdataaa
020300020604     c                   goto      erro
020400020604     c                   endif
020500990325     c*
020600990325     c                   endif
020700990212     c
020800990325     c     Erro          ENDSR
020900990325      *------------------------------------------------------------------------*
021000990325      * RECUPERO ULTIMA DATA DI CHIUSURA PER FILIALE
021100990325      *------------------------------------------------------------------------*
021200990325     C     REC_DTA       BEGSR
021300990325     c*
021400990401     c                   eval      *in06 = *off
021500990422     c                   eval      *in10 = *off
021600990325     c                   clear                   wultdta
021700990325     c                   clear                   tibs02ds
021800990325     C                   MOVEL     'C'           t02mod
021900990325     C                   MOVEL     knsif         t02sif
022000990325     C                   MOVEL     'DCB'         t02cod
022100990325     c                   movel     v1cfil        t02ke1
022200990325      *
022300990325     C                   CALL      'TIBS02R'
022400990325     C                   PARM                    KPJBA
022500990325     C                   PARM                    TIBS02DS
022600990325      *
022700990325     C                   IF        T02err = *BLANKS
022800990325     C                   movel     T02uni        DDCB
022900990325     c                   movel     §dcbudta      wultdta
023000990325     c
023100990325     c                   else
023200990402     C                   eval      wultdta = 19990101
023300990325     C                   endif
023400020604
023500020604      * carico le date di elaborazione delle filiali gestite
023600020604     c                   z-add     *zeros        wdatamax
023700020604     c                   do        30            xx
023800020604     c                   if        lin(xx) = *zeros
023900020604     c                   leave
024000020604     c                   endif
024100020604     c                   clear                   tibs02ds
024200020604     c                   eval      t02mod = 'C'
024300020604     c                   eval      t02sif = knsif
024400020604     c                   eval      t02cod = 'DCB'
024500020604     c                   movel     lin(xx)       t02ke1
024600020604     c                   call      'TIBS02R'
024700020604     c                   parm                    kpjba
024800020604     c                   parm                    tibs02ds
024900020604     c                   if        t02err = *blanks
025000020604     c                   movel     t02uni        ddcb
025100020604     c                   movel     §dcbudta      skdata(xx)
025200020604     c                   else
025300020604     c                   eval      skdata(xx) = 19990101
025400020604     c                   endif
025500020604     c                   if        skdata(xx) > wdatamax
025600020604     c                   eval      wdatamax = skdata(xx)
025700020604     c                   endif
025800020604     c                   enddo
025900990325     c*
026000990325     c* giro la data
026100990325     C                   clear                   WLBDAT
026200990325     C                   z-add     wultdta       G02inv
026300990325     C                   movel     '3'           G02err
026400990325     C                   CALL      'XSRDA8'
026500990325     C                   PARM                    WLBDAT
026600990325     c* se richiesta la ristampa imposto la data trovata come finale altrimenti come iniziale
026700990325     c                   if        v1cese = 'S'
026800990325     c                   z-add     g02dat        v1cdha
026900990423     c                   clear                   v1cdhd
027000990422     c                   eval      *in10 = *on
027100990325     c                   else
027200990325     c                   z-add     g02dat        v1cdhd
027300990503     c     *eur          move      v1cdhd        wdata
027400990503     c                   adddur    1:*d          wdata
027500990503     c     *eur          move      wdata         v1cdhd
027600990423     c*
027700990423     c* calcolo la data finale  aggiungo un mese
027800990423     C                   clear                   WLBDAT
027900990423     c     *iso          move      wultdta       wdata
028000100603     c* Se si tratta dell'ultimo giorno del mese, asso al successivo ult mese
028100100603     c                   adddur    1:*d          wdata
028200100603     c     *iso          move      wdata         g02inv
028300100603     c                   move      g02inv        wgg               2 0
028400100603     c
028500100603     c                   if        wgg=1
028600100603     c                   adddur    1:*m          wdata
028700100603     c                   subdur    1:*d          wdata
028800100603     c                   else
028900100603     c
029000100603     c     *iso          move      wultdta       wdata
029100100603     c                   adddur    1:*m          wdata
029200100603     c                   endif
029300100603     c
029400990423     c     *iso          move      wdata         g02inv
029500990423     C                   movel     '3'           G02err
029600990423     C                   CALL      'XSRDA8'
029700990423     C                   PARM                    WLBDAT
029800990423     c                   z-add     g02dat        v1cdha
029900990401     c* e proteggo il campo
030000990401     c                   eval      *in06 = *on
030100990325     c                   endif
030200990325     C
030300990325     c                   endsr
030400990212      *------------------------------------------------------------------------*
030500990212      * CPNTROLLO P.O.
030600990212      *------------------------------------------------------------------------*
030700990212     C     CTRPO         BEGSR
030800990325     c*
030900990325     c                   clear                   v1dfil
031000990325     C                   clear                   FNLV50DS
031100990325     C                   MOVEL     KNMUS         D50PRU
031200990325     C                   MOVEL     Wfil          D50FGS
031300170504     C                   EVAL      D50TCN='A'
031400990325     C                   CALL      'FNLV50R'
031500990325     C                   PARM                    FNLV50DS
031600990325      *
031700020606     C                   if        D50ERR <> *blanks
031800990325     C                   MOVEL     D50MSG        V1cMSG
031900990325     C                   SETON                                        219028
032000990325     C                   else
032100990325      *
032200170504     c* controllo
032300990212     c     wfil          chain     azorg01l                           30
032400990212     c     *in30         ifeq      *on
032500990212     c     orgfva        orne      ' '
032600990212     c     orgfag        orne      'A'
032700990212     c     orgfag        andne     'F'
032800990325     c                   eval      v1cmsg=msg(1)
032900990325     c                   seton                                        289021
033000990212     c                   endif
033100990325     c  n90              eval      v1dfil = orgdes
033200990325      *
033300990325     c                   endif
033400020604      * recupero le filiali gestite dal p.o. immesso a video
033500020604     c                   if        not *in90
033600020604     c                   clear                   trul06ds
033700020604     c                   eval      d06cod = '£6'
033800020605     c                   eval      d06esc = 'S'
033900020604     c                   movel     wfil          d06key
034000020604     c                   movel(p)  trul06ds      kpjbu
034100020604     c                   call      'TRUL06R'
034200020604     c                   parm                    kpjba
034300020604     c                   movel     kpjbu         trul06ds
034400020604     c                   endif
034500990325      *
034600990325     c* per filiale trovata recupero la data ultima stampa
034700990325     c  n90              exsr      rec_dta
034800990325     c*
034900990212     c                   ENDSR
035000990215      *------------------------------------------------------------------------*
035100990215      * SOTTOMENTTO ELABORAZIONE
035200990215      *------------------------------------------------------------------------*
035300990212     c     CONFERMA      BEGSR
035400990325      *
035500990212     c                   clear                   fidn43ds
035600990325     c                   eval      i43ord = '1'
035700990325     c                   eval      i43ftt = 'C'
035800990420     c                   eval      i43usi = 'S'
035900990212     c                   eval      i43fil = v1cfil
036000990218     c                   eval      i43dhd = v1cdhd
036100990218     c                   eval      i43dha = v1cdha
036200990218     c                   eval      i43amgdhd = wamgdhd
036300990218     c                   eval      i43amgdha = wamgdha
036400990315     c
036500990212      *
036600990212     c                   movel     fidn43ds      kpjbu
036700990212     c
036800990422     c* richiesta ristampa ma non delle CA  non chiamo il pgm
036900990422     c                   if        *in10 and v1crca = '  '
037000990422     c                   else
037100101112     c                   eval      kcoaz='DN47'
037200990212     c                   call      'BCH10'
037300990212     c                   parm                    kpjba
037400170504      * richiamo il pgm fidn47r per ogni p.o. gestito
037500020604     c                   exsr      sr_lancia
037600990422     c                   endif
037700990325      *
037800990325      * se si tratta di aggiornamento con stampa richiamo il programma di chiusura
037900990325      * bolle in CA
038000990325      *
038100990422     c                   if        v1cese = 'A' or (*in10 and v1crbo = 'SI')
038200990325     c                   movel     fidn43ds      kpjbu
038300990325      *
038400990325     c                   eval      kcoaz='DN46'
038500020605     c                   call      'BCH10'
038600990325     c                   parm                    kpjba
038700990325     c                   endif
038800990325      *
038900990212     c                   endsr
039000020604      *------------------------------------------------------------------------*
039100020604      * Lancio del FIDN44R per ogni elemento di LIN
039200020604      *------------------------------------------------------------------------*
039300020604     c     sr_lancia     begsr
039400020604
039500020604      * salvo la ds per il lancio del pgm fidn46
039600020604     c                   eval      sav_43ds = fidn43ds
039700020604
039800020604      * leggo la schiera dei p.o. gestiti
039900020604     c                   do        30            xx
040000020604
040100020604     c                   if        lin(xx) = *zeros
040200020604     c                   leave
040300020604     c                   endif
040400020604
040500020604      * imposto i dati nella ds esterna
040600020604     c                   clear                   fidn43ds
040700020604     c                   eval      i43ord = '1'
040800020604     c                   eval      i43ftt = 'C'
040900020604     c                   eval      i43usi = 'S'
041000020604     c                   eval      i43fil = lin(xx)
041100020604
041200020605      * giro data trovata in tabella se non è ristampa
041300020605     c                   if        not *in10
041400020604     c                   clear                   wlbdat
041500020604     c                   z-add     skdata(xx)    g02inv
041600020604     c                   movel     '3'           g02err
041700020604     c                   call      'XSRDA8'
041800020604     c                   parm                    wlbdat
041900020604     c                   z-add     g02dat        i43dhd
042000020605     c                   eval      i43amgdhd = skdata(xx)
042100020605     c                   endif
042200020605      * se è ristampa passo la data impostata a video
042300020605     c                   if        *in10
042400020605     c                   eval      i43dhd = v1cdhd
042500020605     c                   eval      i43amgdhd = wamgdhd
042600020605     c                   endif
042700020604     c
042800020604     c                   eval      i43dha = v1cdha
042900020604     c                   eval      i43amgdha = wamgdha
043000020604
043100020604     c                   movel     fidn43ds      kpjbu
043200110202     c*                  eval      kcoaz='DN44'
043300110202     c                   eval      kcoaz='DN47'
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

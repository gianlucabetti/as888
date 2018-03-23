000100040119      *===============================================================*
000200150904      * Tnsf22r    * Tassazione bolle con peso desunto
000300040119      *===============================================================*
000400040119
000500150908     h decedit('0,') datedit(*ymd/)
000600040119     h option(*nodebugio)
000700040119
000800040119      *---------------------------------------------------------------*
000900040119
001000150713     Fsipes01l  if   e           k disk
001100150904     Ftntbe01l  if   e           k disk
001200150925     FAzorg01l  if   e           k disk
001300040119      *
001400050526     d kpjba         e ds
001500150904      ** DS  VARIABILI Per calcolo peso desunto
001600150904     d dvpd          e ds
001700150902      ** DS  TNSF20R
001800150902     d dtas          e ds
001900060926     d Dtas01        e ds
002000050607      ** DS Calcolo tassazione  - Varie
002100050607     d Dtasv         e ds
002200150908     d Dtaspes       e ds
002300150925      ** DS campo ORGDE3 di AZORG00F
002400150925     d OG143         e ds
002500150907
002600150907      * campo di comodo
002700150904     d                 ds
002800150908     d Mese                    3      4  0
002900150908     d Anno                    5      8  0
003000150908     d dataamg                 1      8  0
003100150904
003200150902      ** campi di salvataggio  originale di titas prima della tassazione
003300150907     d W_Dtas          s                   like(dtas)
003400150907     d W_Dtasv         s                   like(dtasv)
003500040331      *
003600150902      ** campi di salvataggio tassazione con peso originale di titas
003700150902     d O_Dtas          s                   like(dtas)
003800150907     d O_Dtasv         s                   like(dtasv)
003900040331      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
004000050526      *
004100040331      * - Campi di comodo
004200150713     d Flg_pes         s              1                                         flag di scarto x PES
004300150713     d Colli_anal      s              5  2                                      % colli analizzati
004400150713     d Scost_peso      s              7  2                                      % scostamento peso
004500150713     D tipo_tar        s              1  0
004600150911     D R_taspkf        s                   like(taspkf)                         Peso desunto
004700150911     D O_taspkf        s                   like(taspkf)                         Peso di titas
004800150911     D O_tasimv        s                   like(tasimv)                         imv pkf di titas
004900150908     D Dtastat         s               d   datfmt(*iso)
005000150904     D dataiso         s               d   datfmt(*iso)
005100150904     D dataeur         s               d   datfmt(*eur)
005200150904     D xx              s              3  0
005300150929     D yy              s              3  0
005400150928     D kk              s              4  0
005500150904     D ktbecod         s                   like(tbecod) inz('VPD')
005600150925     D k_orgfil        s                   like(orgfil)
005700150904      *  - Flags booleani
005800150904     d $EoF            s               n   inz(*off)
005900150904     d $Fine           s               n   inz(*off)
006000060914      *
006100060914      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
006200150904      * date decorrenza varibili calcolo peso desunto  e variabili calcolo
006300150929     D SDtasped        S              8s 0 DIM(50)                              Data spedizione
006400150929     D ODtasped        S              8s 0 DIM(50) ascend                       Data spedizione
006500150904     D SVpd            S                   DIM(50) like(Dvpd)                   variab.peso desunto
006600150904
006700150928      * filiali estere per esclusione clienti esteri da calcolo peso desunto
006800150928     D FilEst          S              3  0 DIM(400)                             Filiali ESTERE
006900150910
007000150910     c     *entry        plist
007100150910     c                   parm                    kpjba
007200150910     c                   parm                    dtas
007300150910     c                   parm                    dtasv
007400150910     c                   parm                    dtaspes
007500150910
007600150910      * salvataggio dei parametri passati da utilizzare nelle seguenti tassazioni
007700150910     c                   movel     dtas          W_dtas
007800150910     c                   movel     dtasv         W_dtasv
007900150925
008000150925      * verifico la data di tassazione della ds DTASPES
008100150925      * se non valorizzato imposto udate
008200150925     c                   if        taspdtt = 0
008300150925     c                   eval      taspdtt = *date
008400150925     c                   endif
008500150904      * Se Tipo lancio 'C' --> solo chiusura file
008600150908     c                   if        tasptla <> 'C'
008700120321
008800150904      * eseguo la tassazione
008900150904     c                   exsr      Sr_Sf20
009000150902      * verifico se la spedizione che devo tassare ha le caratteristiche per applicare
009100150904      * calcolare il peso desunto aumentandolo della perentuale di inaffidabilità
009200150904      * del peso comunicato dal cliente
009300150904
009400150904     c                   movel     tasctr        tipo_tar
009500150925      * verifico il network del cliente
009600150925     c                   movel     tasksc        K_orgfil
009700150902
009800150904     c                   Select
009900150904
010000150907      * NON ELABORO  :
010100150907      * se tipo bolla non è F1
010200150904     c                   When      tastbl <>  'F1' or
010300150904      * se bolla con almeno un collo rilevato da VDL
010400150904     c                             tasncp <>  0    or
010500150904      * se bolla con peso bollettato diverso dal peso fatturato
010600150904     c                             tasppkb <>  taspkf  or
010700150904      * se tariffa  non è a peso
010800150907     c                             (tipo_tar <> 0 AND tipo_tar <> 3) or
010900150907      * se tariffa cliente ha tipo applicazione volume "MAI VDL"
011000150922     c                             tastap = 'M'  or
011100150925      * se tariffa cliente ha tipo applicazione peso desunto Uguale a NO
011200150925     c                             tasepd =  'N' or
011300150928      * se cliente estero
011400150928     c                             %lookup(K_orgfil:FilEst)>0   or
011401151028      * se spedizione  < di 1
011402151028     c                             taspkf <= 1                  or
011500150907      * se è già un manca tariffa
011600150907     c                             taserr <> ' '
011700150904      *
011800150904
011900150904     c                   Other
012000150904     c                   exsr      Sr_Elabora
012100150904
012200150904     c                   Endsl
012300150904
012400150904     C                   Endif
012500120323
012600150904      * FINE
012700150904     c                   if        TAStla <> *blanks
012800150904     c                   eval      *inlr = *on
012900150909     c                   movel     'C'           tastla
013000150909     C                   CALL      'TNSF20R'
013100150909     C                   PARM                    KPJBA
013200150909     C                   PARM                    DTAS
013300150909     C                   PARM                    DTASV
013400150904     c                   else
013500150904     c                   eval      *inrt = *on
013600150904     c                   endif
013700150904
013800120321      *
013900120321      **-------------------------------------------------------------**
014000120321      ** Elabora ogni singola bolla                                  **
014100120321      **-------------------------------------------------------------**
014200120321     c     sr_Elabora    BEGSR
014300150907      *
014400150907      * mi salvo le ds della tassazione con peso originale bolla
014500150907     c                   eval      O_dtas = dtas
014600150907     c                   eval      O_dtasv = dtasv
014700150907     c                   eval      O_tasimv = tasimv
014800150911     c                   eval      O_taspkf = taspkf
014900150908      *
015000150925      * cerco le variabili peso in vigore rispetto alla data di tassazione passata nella DTASPES
015100150904      *
015200150904     c                   z-add     1             xx
015300150929     c                   eval      xx=%lookuple(taspdtt:Odtasped)
015400150904     c                   if        xx > 0
015500150929     c                   eval      yy= %lookup(Odtasped(xx):Sdtasped)
015600150929     c                   if        yy > 0
015700150929     c                   eval      dvpd  =  Svpd(yy)
015800150907     c                   mult      -1            §vpdscoda1
015900150907     c                   mult      -1            §vpdscoa1
016000150907     c                   mult      -1            §vpdscoda2
016100150907     c                   mult      -1            §vpdscoa2
016200150904     c                   endif
016300150929     c                   endif
016400150907      * non trovate variabili in tabella legate alla data spedizione esco dal programma
016500150929     c                   if        xx = 0 or yy = 0
016600150908     c                   eval      tasperr = '1'
016700150908     c                   eval      taspmsg = 'Non trovata schiera VPD'
016800150907     c                   exsr      Sr_Fine
016900150904     c                   leavesr
017000150904     c                   endif
017100150713      * aggancio SIPES
017200150713     c                   exsr      SR_ctrpesVDL
017300150907      * se non ci sono errori ricalcolo con il peso desunto
017400150907     c                   if        Flg_pes = ' '
017500150907     c                   eval      R_taspkf =
017600150907     c                              (taspkf+((taspkf*Scost_peso*-1)/100))
017700150907      * se il peso ricalcolato è diverso da quello originale ricalcolo
017800151029     c                   if        R_taspkf  > taspkf
017900150907     c                   eval      dtas = W_dtas
018000150907     c                   eval      dtasv = W_dtasv
018100150907     c                   eval      taspkf = R_taspkf
018200150907      * ritasso
018300150907     c                   exsr      sr_sf20
018400150911      * dopo la ritassazione con il peso desunto ripristino i valori del TASPKF con
018500150911      * quello originale di Titas ed imposto nel campo peso cml al netto della tara
018600150911      * il peso desunto da memorizzare su fiar5 o stampare in fattura
018700150911     c                   eval      taspkcn = taspkf
018800150911     c                   eval      tasspc = 'D'
018900150911     c                   eval      taspkf = O_taspkf
019000150907      *
019100150907     c                   if        taserr <> ' '
019200150908     c                   eval      tasperr = '1'
019300150911     c                   eval      taspmsg = 'Manca tariffa con nuovo Peso'  +
019400150911     c                             %editc(taspkcn:'4')
019500150908     c                   exsr      Sr_Fine
019600150908     c                   leavesr
019700150908     c                   endif
019800150908      * verifico l'imponibile  se maggiorato applico il nuovo peso altrimenti mantengo il
019900151028      * vecchio  e  la (Bocchi 28/10/15)
019901151028      * differenza tra imponibile del peso desunto - imponibile no peso desunto è > di 0,25
020000151028     c                   if        tasimv <= O_tasimv or
020001151028     c                             ((tasimv-O_tasimv) <= 0,25)
020100150908     c                   eval      dtas = O_dtas
020200150908     c                   eval      dtasv = O_dtasv
020300150909     c                   eval      taspdes = 'N'
020400150908     c                   else
020500150909     c                   eval      taspdes = 'S'
020600150908     c                   endif
020700150908
020800150908     c                   Endif
020900150908      * Non trovata statistica o statistica non giusta per i parametri
021000150908     c                   Else
021100150908     c                   eval      tasperr = '1'
021200150908     c                   endif
021300150908      *
021400150908     c                   exsr      Sr_fine
021500120314      *
021600120323     c                   endsr
021700150904      **-------------------------------------------------------------**
021800150904      ** Controllo statistica pesi VDL                               **
021900150904      **-------------------------------------------------------------**
022000150904     c     sr_ctrpesVDL  BEGSR
022100150904      *
022200150904     c                   eval      flg_pes = 'N'                                No file pes
022300150904     c
022400151022      * imposto anno e mese ricerca statistica se non trovato si
022600150904      * cerca indietro fino ad un massimo di mesi stabilito nella tabellla delle variabili calcolo
022700150904      * peso desunto
022800150929     c                   move      tasdsp        dtastat
022900150907     c                   do        §vpdmesi
023000150904     c                   move      Dtastat       dataeur
023100150908     c                   move      dataeur       dataamg
023700150904      * aggancio la statistica
023800150904     c     Kpes          chain     Sipes01l
023900150904     c                   if        %found(Sipes01l)
024000150908     c                   eval      flg_pes = ' '                                Si file pes
024100150904     c                   leave
024200150904     c                   endif
024300150929     c                   subdur    1:*M          Dtastat
024400150904     c                   enddo
024500150904     c
024600150904      * se tutto bene
024700150904     c                   If        not %found(Sipes01l)
024800150904     c                   eval      flg_pes = 'N'                                No file pes
024900150904     c                   clear                   Colli_anal
025000150904     c                   clear                   Scost_peso
025100150904     c                   clear                   pesnsp
025200150904     c                   leavesr
025300150904     c                   endif
025400150909
025500150909      * controllo se la statistica pesi è valida per il calcolo del peso desunto
025600150909
025700150909      * calcolo % colli analizzato
025800150909     c                   eval      Colli_Anal = (pesncv*100)/pestcl
025900150909      * calcolo % scostamento pesi
026000150909     c                   eval      Scost_Peso = ((pespsb-pespsc)*100)/pespsb
026100150909
026200150909     c                   select
026300150904      * verifico numero spedizioni rilevate il  mese precedente
026400150909     c                   when      Pesnsp < §vpdnsped
026500150904     c                   eval      flg_pes = 'S'                                poche spedizioni
026600150930      * verifico se analizzate più del 50% dei colli bollettati
026700150930     c                   When      Colli_anal <= §vpdcolli                      pochi colli analizz.
026800150904     c                   eval      flg_pes = 'C'                                pochi colli
026900150907      * verifico se scostamento peso è nei range impostati in tabella in base al peso della
027000150907      * spedizione
027100150907
027200150907      * 1° range di pesi
027300150909     c                   When      taspkf <= §vpdpkal1 and taspkf >= §vpdpkda1
027400150907     c                             and (Scost_Peso < §vpdscoda1 or              Scostamento pesi
027500150907     c                             Scost_peso > §vpdscoa1)
027600150907     c                   eval      flg_pes = '1'                                scostamento 1° range
027700150907      * 2° range di pesi
027800150909     c                   when      taspkf <= §vpdpkal2 and taspkf >= §vpdpkda2
027900150907     c                             and (Scost_Peso < §vpdscoda2 or              Scostamento pesi
028000150907     c                             Scost_peso > §vpdscoa2)
028100150907     c                   eval      flg_pes = '2'                                scostamento 2°range
028200150909     c                   endsl
028300150904      *
028400150904      *
028500150904     c                   endsr
028600150907      *
028700050526      **-------------------------------------------------------------**
028800150904      ** Richiamo pgm tassaz.  tnsf20r                               **
028900050526      **-------------------------------------------------------------**
029000050526     c     sr_sf20       BEGSR
029100050526      *
029200150904     c                   call      'TNSF20R'
029300150908     c                   parm                    Kpjba
029400150904     c                   parm                    dtas
029500150904     c                   parm                    dtasv
029600150904
029700050607     c     e_sf20        endsr
029800150908      *
029900150908      **-------------------------------------------------------------**
030000150908      ** Fine programma memorizzazione dati                          **
030100150908      **-------------------------------------------------------------**
030200150908     c     sr_Fine       BEGSR
030300150908      * valorizzo i dati nella ds di comodo
030400150908     c                   if        flg_pes = 'N'
030500150908     c                   eval      taspmsg = 'Non trovato record FNPES'
030600150909     c                   clear                   TASPPKD
030700150908     c                   clear                   TASPANNO
030800150908     c                   clear                   TASPMESE
030900150908     c                   clear                   TASPCOLLI
031000150908     c                   clear                   TASPSCOST
031100150908     c                   clear                   TASPNSP
031200150909     c                   clear                   TASPIMV
031300150908     c                   endif
031400150908      *
031500150908     c                   if        flg_pes = 'S'
031600150908     c                   eval      taspmsg = 'Poche spedizioni rilevate'
031700150909     c                   clear                   TASPPKD
031800150908     c                   movel     pesann        TASPANNO
031900150908     c                   movel     pesmes        TASPMESE
032000150908     c                   movel     Colli_anal    TASPCOLLI
032100150908     c                   movel     Scost_peso    TASPSCOST
032200150908     c                   movel     pesnsp        TASPNSP
032300150909     c                   clear                   TASPIMV
032400150908     c                   endif
032500150908      *
032600150908     c                   if        flg_pes = '1'
032700150908     c                   eval      taspmsg = 'Range % scostamento 1 non valido'
032800150909     c                   clear                   TASPPKD
032900150908     c                   movel     pesann        TASPANNO
033000150908     c                   movel     pesmes        TASPMESE
033100150908     c                   movel     Colli_anal    TASPCOLLI
033200150908     c                   movel     Scost_peso    TASPSCOST
033300150908     c                   movel     pesnsp        TASPNSP
033400150909     c                   clear                   TASPIMV
033500150908     c                   endif
033600150908      *
033700150908     c                   if        flg_pes = '2'
033800150908     c                   eval      taspmsg = 'Range % scostamento 2 non valido'
033900150909     c                   clear                   TASPPKD
034000150908     c                   movel     pesann        TASPANNO
034100150908     c                   movel     pesmes        TASPMESE
034200150908     c                   movel     Colli_anal    TASPCOLLI
034300150908     c                   movel     Scost_peso    TASPSCOST
034400150908     c                   movel     pesnsp        TASPNSP
034500150909     c                   clear                   TASPIMV
034600150908     c                   endif
034700150908      *
034800150908     c                   If        flg_pes = ' '
034900150909     c                   movel     R_taspkf      TASPPKD
035000150908     c                   movel     pesann        TASPANNO
035100150908     c                   movel     pesmes        TASPMESE
035200150908     c                   movel     Colli_anal    TASPCOLLI
035300150908     c                   movel     Scost_peso    TASPSCOST
035400150908     c                   movel     pesnsp        TASPNSP
035500150909     c                   movel     O_tasimv      TASPIMV
035600150908     c                   endif
035700150908      *
035800150908     c                   Endsr
035900120323      *
036000050609      *************************************************************************
036100050609      * Operazioni iniziali
036200050609      *************************************************************************
036300050609     C     *inzsr        BEGSR
036400050609     c*
036500150713      * Chiave file statistica pesi VDL
036600150713     c     Kpes          Klist
036700150902     c                   kfld                    Tasksc
036800150713     c                   kfld                    Anno
036900150713     c                   kfld                    Mese
037000150902     c                   kfld                    Tasctr
037100150904
037200150914     c                   movel     *all'9'       sdtasped
037300150902      *
037400150902      * recupero tabella valori variabili da utilizzare nell'analisi dei dati della statistica PESI
037500150908     c     Ktbecod       setll     tntbe01l
037600150904     c     ktbecod       reade     tntbe01l
037700150904     c                   dow       not %eof(tntbe01l)
037800150904     c                   if        xx < 50
037900150904     c                   add       1             xx
038000150908     c                   eval      SDtasped(xx)= %dech(%subst(tbeKe1:1:8):8:0)
038100150908     c                   eval      SVpd(xx) = tbeuni
038200150904     c                   endif
038300150904     c     ktbecod       reade     tntbe01l
038400150904     c                   enddo
038500150929
038600150929     c                   movea     Sdtasped      Odtasped
038700150929
038800150929     c                   sorta     Odtasped
038900150928
039000150928      * Carico le filiali Estere per escludere clienti esteri dal calcolo peso desunto
039100150928     c                   clear                   kk
039200150928     c     *loval        setll     AZORG01L
039300150928     c                   read      AZORG01L
039400150928    1c                   DOW       not %eof(azorg01l)
039500150928
039600150928      * Filiale o Agenzia
039700150928    2c                   If        (orgfag = 'F' or orgfag = 'A') and
039800150928      * Annullato
039900150928    3c                             orgfva =  ' '
040000150928
040100150928    4c                   movel     ORGde3        OG143
040200150928      * Filiale estero
040300150928    4c                   If        §OGNtw = 'EEX' OR  §OGNtw = 'EUP' OR
040400150928     c                             §OGNtw = 'DPD' OR  §OGNtw = 'FED'
040500150928     c                   Add       1             KK
040600150928     c                   Movel     ORGFil        FilEst(KK)
040700150928    4c                   Endif
040800150928
040900150928    3c                   Endif
041000150928      *
041100150928     c                   Read      Azorg01l
041200150928    1c                   Enddo
041300060915
041400050609     c                   endsr

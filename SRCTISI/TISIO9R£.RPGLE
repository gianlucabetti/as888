000100040917     H DECEDIT('0,') DATEDIT(*yMd.)
000200131219      * TISIO9R *----------------------------------------------------*
000300130830      *         - Reperimento servizi orari  Cappario
000400940915      *--------------------------------------------------------------*
000500140514     Fwaopa01l  iF   E           K DISK    usropn
000600140108     Fazcos03l  iF   E           K DISK
000700140108     Fazcos04l  iF   E           K DISK    rename(azcos000:azcos004)
000800140514     Ftabel00f  IF   E           K DISK    usropn
000900151209     Ftntbe01l  IF   E           K DISK    usropn
001000020805      *
001100020805      * DEFINIZIONE SCHIERE
001200960302     D*
001300161026     D MSG             S             78    DIM(07) CTDATA PERRCD(1)
001400060315     d
001500040917     D KPJBA         E DS
001600130830     D ds15          E DS
001700140108     D ds1X          E DS
001800060922     d
001900130830     d Datasys         s               d   datfmt(*iso) inz(*sys)
002000151209     d kcod            s                   like(tbecod)
002100151209     d kke1            s                   like(tbeke1)
002200151209     d ktbkut          s                   like(tblkut)
002300130830     d ktbcod          s                   like(tblcod)
002400130830     d ktbkey          s                   like(tblkey)
002500130830     d kloc            s                   like(cosloc)
002600130830     d klna            s                   like(coslna)
002700130830     d lna999          s                   like(coslna) inz(999)
002800130830     d kcap            s                   like(coscap)
002900130830     d knar            s                   like(cosnar)
003000140108     d ktser           s                   like(costser)
003100140108     d ksoglia         s                   like(cosserv)
003200161026     d savloc          s                   like(cosloc)
003300130802     d
003400140108     d tiSIO9ds      e ds
003500140514     d
003600140514     d* Altri dati :   località normalizzata
003700140514     d tisio9ds2       ds
003800140514     D  osio9loc               1     35
003900140514     D  osio9norm             36     36
004000150728     c* ore teoriche trazine cliente e ore teoriche consegna cliente
004100150728     D  osio9trazC            37     39
004200150728     D  osio9consC            40     42
004300150728     D  osio9TR_TC            37     42
004400150728     D  osio9libero           43    100
004500130830     d
004600130830     d tisi95ds      e ds
004700140211     d fnlv55ds      e ds
004800150728
004900151209     d Dataiso         s               d   datfmt(*iso)
005000150728     D digits          c                   '0123456789'
005100060926     d
005200050103     C**********************************************************************
005300040917     C     *ENTRY        PLIST
005400040917     C                   PARM                    KPJBA
005500140107     C                   PARM                    tiSIO9ds
005600140514     C                   PARM                    tiSIO9ds2
005700130830     c
005800130830     c                   clear                   do0msg
005900130830     c                   clear                   do0err
006000131010     c                   clear                   do0rif
006100131010     c                   clear                   do0flo
006200131010     c                   clear                   do0ttc
006300131010     c                   clear                   do0serv
006400131010     c                   clear                   DO0OSTIS
006500131010     c                   clear                   DO0OSTFS
006600131010     c                   clear                   DO0OMIIS
006700131010     c                   clear                   DO0OMxfS
006800131010     c                   clear                   DO0OLRSC
006900131010     c                   clear                   DO0OLRNC
007000131010     c                   clear                   DO0OLRNK
007100140108     c                   clear                   DO0LIA
007200140108     c                   clear                   DO0fcsgl
007300130830     c*
007400130830     c* Controllo dati di input passati
007500060315     c                   exsr      CARINIZ
007600131011     c
007700140108     c* se non ho il tipo servizio giusto impostato non posso nemmeno reperire i dati STD
007800131011     c                   if        DO0err<>' ' and  do0tser<>'C'
007900131011     c                             and do0tser<>'R'
008000131011     c                   goto      fine
008100131011     c                   endif
008200140211     c
008300140211     c* Verifico presenza £6
008400140211     c                   clear                   fnlv55ds
008500140211     C                   EVAL      D55TPT='6'
008600140211     c                   eval      d55lin=do0lna
008700140214     c                   if        do0dri>0
008800140211     c                   eval      d55drf=do0dri
008900140214     c                   else
009000140214     c                   movel     datasys       d55drf
009100140214     c                   endif
009200140214     c
009300140211     c                   call      'FNLV55R'
009400140211     c                   parm                    fnlv55ds
009500131010     c
009600131219     c*  In tutti gli altri casi di errore --> prendo i dati dal record  STD
009700131011     c*
009800131011     c                   if        DO0err<>' '
009900140108     c                   eval      knar=*blanks
010000140108     c                   eval      kcap   =*blanks
010100140108     c                   eval      kloc   =*blanks
010200140108     c                   eval      ksoglia=*blanks
010300140108     c
010400140108     c                   if        do0soglia='P '
010500140108     c                   eval      ksoglia='P '
010600140108     c                   endif
010700140108     c
010800140108     c                   eval      klna=999
010900140108     c                   eval      ktser=do0tser
011000140108     c     kcos          chain     azcos03l
011100131011     c                   exsr      carOUT
011200131011     c                   eval      do0rif='9'
011300131011     c                   goto      fine
011400131011     c                   endif
011500140108
011600140108     c* Verifico se la filiale ha la soglia
011700140108     c
011800140108     c                   eval      knar=*blanks
011900140211     c***                eval      klna=do0lna
012000140211     c                   eval      klna=d55tfa
012100140108     c                   eval      ktser=do0tser
012200140108     c                   eval      ksoglia='P '
012300140108     c                   eval      kcap   =*blanks
012400140108     c                   eval      kloc   =*blanks
012500140108     c     kcos4         chain     azcos04l
012600140108     c                   if        %found(azcos04l)
012700140108     c                   eval      Do0fcsgl='S'
012800140108     c                   endif
012900130830     c*
013000130830     c* Imposto la key
013100131011     c                   eval      knar=*blanks
013200130830     c                   eval      kcap=do0cap
013300140211     c****               eval      klna=do0lna
013400140211     c                   eval      klna=d55tfa
013500130830     c                   eval      ktser=do0tser
013600130830     c                   eval      kloc=do0loc
013700130830     c
013800150505     c* se passata la località la normalizzo se non è già normalizzata
013900130830     c                   if        kloc<>*blanks
014000150505     c                   if        %parms<3 or osio9loc=*blanks
014100130830     c                   exsr      norm_loc
014200150505     c                   else
014300150505     c                   eval      Kloc=osio9loc
014400130830     c                   endif
014500150505     c                   endif
014600161026     c
014700161026     c                   eval      savloc=kloc
014800140108     c*
014900140108     c                   select
015000140108     c                   when      do0soglia='P '
015100140108     c                   eval      ksoglia='P '
015200140414     c
015300140414     c                   if        do0fcsgl='S'
015400140414     c                   eval      DO0FCSGL='P'
015500140414     c                   endif
015600140108     c
015700140108     c                   when      do0soglia='T '
015800140108     c                   eval      ksoglia='  '
015900140108     c
016000140108     c                   when      do0fcsgl<>'S'
016100140108     c                   eval      ksoglia='  '
016200140108
016300140108     c                   other
016400140108     c                   exsr      Cersoglia
016500140414
016600140414     c                   if        ksoglia='P '
016700140414     c                   eval      DO0FCSGL='P'
016800140414     c                   endif
016900140108     c                   endsl
017000140108     c
017100130830     c* Reperisco gli orari servizio
017200130830     c
017300140108     c     kcos          chain     azcos03l
017400130830
017500140108    1c                   dow       klna<>lna999  and not%found(azcos03l)
017600130830     c                   exsr      Impokey
017700130830
017800140108     c     kcos          chain     azcos03l
017900130830    1c                   enddo
018000130830
018100140108     c                   if        not%found(azcos03l)
018200130830     c                   eval      do0msg=msg(05)
018300140226     c                   eval      do0err='E'
018400130830     c                   goto      fine
018500130830     c                   endif
018600130830     c*
018700131010     c* Passo i dati di output
018800131011     c                   exsr      CarOUT
018900131011     c* compreso il Livello dati trovato
019000130830     c                   select
019100130830     c                   when      kloc<>*blanks
019200130830     c                   eval      do0rif='L'
019300130830     c                   when      kcap<>*blanks
019400130830     c                   eval      do0rif='C'
019500130830     c                   when      klna<>lna999
019600130830     c                   eval      do0rif='F'
019700130830     c                   other
019800130830     c                   eval      do0rif='9'
019900130830     c                   endsl
020000161026     c*
020100161026     c* Nel caso di RITIRO, devo prendere anche i dati consegne STD, per impostare
020200161026     c*  minimo e massimo
020300161026     c                   if        do0tser= 'R'
020400161026     c                   eval      knar=*blanks
020500161026     c                   eval      kcap=do0cap
020600161026     c                   eval      klna=d55tfa
020700161026     c                   eval      ktser='C'
020800161026     c                   eval      kloc=savloc
020900161026     c                   clear                   ksoglia
021000161026     c     kcos          chain     azcos03l
021100161026
021200161026    1c                   dow       klna<>lna999  and not%found(azcos03l)
021300161026     c                   exsr      Impokey
021400161026
021500161026     c     kcos          chain     azcos03l
021600161026    1c                   enddo
021700161026     c                   if        not%found(azcos03l)
021800161026     c                   eval      do0msg=msg(07)
021900161026     c                   eval      do0err='E'
022000161026     c                   else
022100161026     c                   eval      do0omiis=cosostis
022200161026     c                   eval      do0omxfs=cosostfs
022300161026     c                   endif
022400161026     c
022500161026     c                   endif
022600130830     c
022700130830     c     fine          tag
022800060922     c
022900000000     C                   SETON                                        LR
023000130830     C**************************************************************************
023100130830     c* impostazine della chiave per la ricerca su AZCOS a scalare
023200130830     C**************************************************************************
023300130830     c     Impokey       BEGSR
023400130830     c                   select
023500130830     c                   when      kloc<>*blanks
023600130830     c                   clear                   kloc
023700130830     c                   when      kcap<>*blanks
023800130830     c                   clear                   kcap
023900130830     c                   other
024000130830     c                   eval      klna=lna999
024100130830     c                   endsl
024200130830     c                   ENDSR
024300131011     C**************************************************************************
024400131011     c* Impostazione DS campi di output
024500131011     C**************************************************************************
024600131011     c     carOUT        BEGSR
024700131011     c                   eval      do0ostis=cosostis
024800131011     c                   eval      do0ostfs=cosostfs
024900131011     c                   eval      do0omiis=cosomiis
025000131011     c                   eval      do0omxfs=cosomxfs
025100131011     c                   eval      do0olrsc=cosolrsc
025200131011     c                   eval      do0olrnc=cosolrnc
025300131011     c                   eval      do0olrnk=cosolrnk
025400131011     c                   eval      do0ttc  =costtc
025500131011     c                   eval      do0serv =cosserv
025600131011     c                   eval      do0flo  =cosflo
025700140108     c                   eval      do0trzs =costrzs
025800131011     c                   ENDSR
025900050214     C**************************************************************************
026000060315     C     CARiniz       BEGSR
026100130830     c
026200130830     C     KEYTAB        KLIST
026300130830     C                   KFLD                    KTBKUT                         *CODICE UTENTE
026400130830     C                   KFLD                    KTBCOD                         *TABELLA
026500130830     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
026600151209     C     Ktbe          KLIST
026700151209     C                   KFLD                    Kcod                           *CODICE UTENTE
026800151209     C                   KFLD                    Kke1
026900130830     C     KCOS          KLIST
027000130830     C                   KFLD                    Do0VER
027100130830     C                   KFLD                    knar
027200130830     C                   KFLD                    kcap
027300130830     C                   KFLD                    kloc
027400130830     C                   KFLD                    klna
027500130830     C                   KFLD                    ktser
027600140108     C                   KFLD                    ksoglia
027700140108     C     KCOS4         KLIST
027800140108     C                   KFLD                    Do0VER
027900140108     C                   KFLD                    klna
028000140108     C                   KFLD                    knar
028100140108     C                   KFLD                    kcap
028200140108     C                   KFLD                    kloc
028300140108     C                   KFLD                    ktser
028400140108     C                   KFLD                    ksoglia
028500140108     c*
028600140108     C     Kopa          KLIST
028700140108     C                   KFLD                    Do0tfp
028800140108     C                   KFLD                    do0tfa
028900060331     c
029000131011     c* Se la versione non è stata passata uso la data di riferimento per calcolarla
029100130830    1c                   if        do0ver=0
029200130830     c
029300130830    2c                   if        do0dri=0
029400130830     c                   movel     datasys       do0dri
029500130830    2c                   endif
029600130830     c
029700130830     C                   Z-ADD     Do0DRI        P96DRI            8 0
029800130830     C                   Z-ADD     *ZEROS        P96VER            5 0
029900130830     C                   CALL      'TISI96R'
030000130830     C                   PARM                    P96DRI
030100130830     C                   PARM                    P96VER
030200130830     c
030300130830     c                   eval      do0ver=p96ver
030400130830    1c                   endif
030500131011
030600131011     c* Se non passato tipo servizio --> errore
030700131011     c                   if        do0tser<>'R' and do0tser<>'C'
030800131011     c                   eval      do0msg=msg(02)
030900131011     c                   eval      do0err='E'
031000131011     c                   leavesr
031100131011     c                   endif
031200140108     c*
031300140108     c* se il campo soglia non è P=POM o T=TUTTO il giorno errore e lo abblanco
031400140108     c                   if        do0soglia<>*blanks and do0soglia<>'P ' and
031500140108     c                             do0soglia<>'T '
031600140108     c                   eval      do0msg=msg(06)
031700140108     c                   eval      do0err='E'
031800140108     c                   leavesr
031900140108     c                   endif
032000130830     c
032100130830     c                   if        do0lna>0  and do0lna<>999
032200130830     c
032300140514     c***  do0lna        chain     azorg01l
032400130830     c
032500140514     c***                if        not %found(azorg01l)
032600140514     c***                          or orgfag<>'F' and orgfag<>'A'
032700140514     c***                eval      do0msg=msg(01)
032800140514     c***                eval      do0err='E'
032900140514     c***                leavesr
033000140514     c***                endif
033100130924     c
033200130924     c* Se la linea passata non è partita con la nuova procedura emetto errore
033300140414     c***                movel     do0lna        w003a             3
033400140414     c***                eval      Indx=%lookup(w003a:fil_ab)
033500140414     c***
033600140414     c***                if        indx=0 and fil_ab(1)<>'999' and do0tric<>'S'
033700140414     c***                eval      do0msg=msg(05)
033800140414     c***                eval      do0err='A'
033900140414     c***                endif
034000130924     c
034100130924     c                   endif
034200130830     c
034300130830     c* se non ho passato la linea di arrivo imposto 999 se cap e loc=*blanks
034400130830     c*  altrimenti errore
034500130830     c                   if        do0lna=0 and do0cap=*blanks and do0loc=*blanks
034600130830     c                   eval      do0lna=999
034700130830     c                   endif
034800130830     c* Se non passata linea di arrivo --> errore
034900130830     c                   if        do0lna=0
035000130830     c                   eval      do0msg=msg(01)
035100131011     c                   eval      do0err='E'
035200130830     c                   leavesr
035300130830     c                   endif
035400130830     c
035500131011     c* Se passata località e non CAP --> errore
035600130830     c                   if        do0loc<>*blanks
035700130830     c                             and do0cap=*blanks
035800130830     c                   eval      do0msg=msg(04)
035900131011     c                   eval      do0err='E'
036000130830     c                   endif
036100130830     c
036200130830     c* Ammessa soltanto nazione ITALIA
036300140514     c                   if        do0nar<>*blanks
036400140514     c                   open      tabel00f
036500130830     C                   MOVEL     1             KTBKUT
036600130830     C                   MOVEL     '15'          KTBCOD
036700130830     C                   MOVEL(P)  do0nar        KTBKEY
036800130830     C     KEYTAB        CHAIN     TABEL00F                           99
036900130830     c                   clear                   ds15
037000130830IF  1C     *IN99         IFEQ      *OFF
037100130830     C     TBLFLG        ANDEQ     *BLANKS
037200130830     C                   MOVEL     TBLUNI        DS15
037300130830     c                   endif
037400130830     c                   if        §15ita<>'I'
037500130830     c                   eval      do0msg=msg(03)
037600131011     c                   eval      do0err='A'
037700130830     c                   endif
037800140514     c                   endif
037900130830     c
038000130830     C                   ENDSR
038100130830     C**************************************************************************
038200130830     c* Normalizzo la località
038300130830     C**************************************************************************
038400130830     C     Norm_loc      BEGSR
038500130830     c                   clear                   tisi95ds
038600140516     C                   MOVEL     '3'           I95TCN
038700130830     c                   eval      i95loc=do0loc
038800130830     c                   eval      i95cap=do0cap
038900130830     c                   eval      i95nar=do0nar
039000130830     c                   eval      i95dat=do0dri
039100140516     C                   call      'TISI95R'
039200140516     C                   parm                    tisi95ds
039300140516     C*
039400130830     c
039500130830     C                   IF        O95ERR= ' ' AND o95loc<>*blanks
039600130830     c                   eval      kloc=o95loc
039700130830     c                   else
039800130830     c                   eval      kloc=do0loc
039900130830     c                   endif
040000140514     c
040100140514     c* Passo in output la località normalizzata
040200140514     c                   if        %parms=3
040300140514     c                   eval      osio9loc  =kloc
040400140514     c* Se normalizzata memorizzo
040500140514     C                   if        o95lid = '3' or o95lid = '4'
040600140514     c                   eval      osio9NORM='S'
040700140514     c                   endif
040800140514     c                   endif
040900130830     c
041000130830     C                   ENDSR
041100140108     C**************************************************************************
041200140108     c* Cerco il record giust per la soglia se la filiale lo prevede
041300140108     C**************************************************************************
041400140414     c     CerSoglia     BEGSR
041500150728     c                   clear                   wtraz             3 0
041600150728     c                   clear                   wcons             3 0
041700151209     c                   clear                   wdoppiag          1
041800151209     c                   clear                   wdti              8 0
041900151209     c                   eval      Wdti=do0dti
042000151209     c
042100151209     c* Verifico se la linea di arrivo è una eccezione
042200151209     c                   open(e)   tntbe01l
042300151209     c                   eval      kcod='FLO'
042400151209     c                   eval      kke1=%editc(do0lna:'X')
042500151211     c     ktbe          chain     tntbe01l
042600151209    1c                   if        %found(tntbe01l) and tbeatb=' '
042700151209     c                   eval      wdoppiag='S'
042800151209
042900151209     c* Se c'è la data e non l'orario vado a reperire l'orario
043000151209    2c                   if        do0dti>0 and do0hti=9999
043100151209     c                   open(e)   waopa01l
043200151209     c     kopa          chain     waopa01l
043300151209    3c                   if        %found(waopa01l)
043400151209     c                   eval      do0hti=opahha
043500151209    3c                   endif
043600151209    2c                   endif
043700151209     c
043800151209     c* se è una eccezione, verifico se orario oltre seconda soglia --> aggiungo 1 giorno
043900151209     c* alla data di arrivo
044000160226    2c                   if        do0hti>0 and do0hti<9999
044100151209     c                   exsr      Primasgl
044200151209    3c                   if        do0hti> §1xhho
044300151209     c                   eval      ktbkey=%trimr(ktbkey)+'1'
044400151209     C     KEYTAB        CHAIN     TABEL00F
044500151209    4c                   if        %found(tabel00f)
044600151209     c                   eval      ds1x=tbluni
044700151209    4c                   endif
044800151209
044900151209     c* 4) se oltre seconda soglia aggiungo 1 giorno alla data
045000160226    4c                   if        do0hti>§1xhho
045100151209     c                   Move      do0dti        Dataiso
045200151209     c                   Adddur    1:*d          Dataiso
045300151209     c                   Move      Dataiso       wdti
045400151209    4c                   endif
045500151209    3c                   endif
045600151209    2c                   endif
045700151209
045800151209    1c                   endif
045900140514     c
046000140108     c* 1) Se bolla in distinta e data arrivo trazione < data distinta --> tempi "  "
046100151209     c*                                                                --> tempi"2GG'
046200151209    1c****               if        do0ddc>0 and do0dti>0 and do0dti<do0ddc
046300151209    1c                   if        do0ddc>0 and do0dti>0 and wdti<do0ddc
046400151209     c                   if        wdoppiag=' '
046500140108     c                   eval      ksoglia='  '
046600151209     c                   else
046700151209     c                   eval      ksoglia='P '
046800151209     c                   endif
046900151209     c
047000140108     c                   leavesr
047100140108    1c                   endif
047200140526
047300140526     c* 1a) Se bolla NON in distinta e data arrivo trazione < data del giorno--> tempi "  "
047400140526     c                   movel     datasys       wdrf              8 0
047500151209    1c****               if        do0ddc=0 and do0dti>0 and do0dti<wdrf
047600151209    1c                   if        do0ddc=0 and do0dti>0 and wdti  <wdrf
047700151209     c                   if        wdoppiag=' '
047800140526     c                   eval      ksoglia='  '
047900151209     c                   else
048000151209     c                   eval      ksoglia='P '
048100151209     c                   endif
048200151209     c
048300140526     c                   leavesr
048400140526    1c                   endif
048500140108
048600140108     c* 2) Se consegna richiesta DOPO il o IL > consegna prevista --> tempi "  "
048700140109     c                   if        do0dcr>0  and do0tcr<>'P'  and do0dei>0
048800140109     c                   if        do0tcr=' ' and do0dcr>do0dei
048900151209     c                   if        wdoppiag=' '
049000140109     c                   eval      ksoglia='  '
049100151209     c                   else
049200151209     c                   eval      ksoglia='P '
049300151209     c                   endif
049400151209     c
049500140109     c                   leavesr
049600140108     c                   endif
049700140109     c                   if        do0tcr='D' and do0dcr>=do0dei
049800151209     c                   if        wdoppiag=' '
049900140109     c                   eval      ksoglia='  '
050000151209     c                   else
050100151209     c                   eval      ksoglia='P '
050200151209     c                   endif
050300151209     c
050400140109     c                   leavesr
050500140109     c                   endif
050600140109     c                   endif
050700150721     c
050800150721     c* 3) Se la data prevista consegna interna è > della data arrivo ultimo collo --> tempi "  "
050900151209    1c*****              if        do0dei>0 and do0dti>0 and do0dti<do0dei
051000151209    1c                   if        do0dei>0 and do0dti>0 and wdti  <do0dei
051100151209     c                   if        wdoppiag=' '
051200150721     c                   eval      ksoglia='  '
051300151209     c                   else
051400151209     c                   eval      ksoglia='P '
051500151209     c                   endif
051600151209     c
051700150721     c                   leavesr
051800150721    1c                   endif
051900150728     c* 4) In mancanza della data arrivo ultimo collo se i tempi TRZ+CONS sono > 48ore--> tempi "  "
052000150728     c                   if        %parms>=3 and osio9TR_TC >*zeros and do0dti=0
052100150728     c
052200150728    2c                   if        %check(digits:osio9trazC)=0
052300150728     c                   eval      wtraz=%int(osio9trazC)
052400150728    3c                   if        %check(digits:osio9consC)=0
052500150728     c                   eval      wcons=%int(osio9consC)
052600150728     c
052700150728     C     wtraz         DIV       24            WDIV              3 0
052800150728     C                   MVR                     WRES              3 0
052900150728     c
053000150728    4c                   if        wres>0 and wcons>12
053100151209     c                   if        wdoppiag=' '
053200150723     c                   eval      ksoglia='  '
053300151209     c                   else
053400151209     c                   eval      ksoglia='P '
053500151209     c                   endif
053600151209     c
053700150723     c                   leavesr
053800150728    4c                   endif
053900150728    4c                   if        wres=0 and wcons>24
054000151209     c                   if        wdoppiag=' '
054100150728     c                   eval      ksoglia='  '
054200151209     c                   else
054300151209     c                   eval      ksoglia='P '
054400151209     c                   endif
054500151209     c
054600150728     c                   leavesr
054700150728    4c                   endif
054800150728    c
054900150728    3c                   endif
055000150728    2c                   endif
055100150728    1c                   endif
055200140108     c*
055300140108     c* Se orario arrivo trazione non passato lo cerco dal file degli arrivi standard
055400140428     c* Orario = 9999 serve quando si ha la data di arrivo ma non l'ora. Ad esempio
055500140428     c*  per le bolle di sede TITAS
055600140428    1c                   if        (do0dti=0 and do0hti=0) or do0hti=9999
055700140514     c                   open      waopa01l
055800140110     c     kopa          chain     waopa01l
055900140110    2c                   if        %found(waopa01l)
056000140108     c                   eval      do0hti=opahha
056100140108    2c                   endif
056200140108    1c                   endif
056300140108     c
056400151209     c                   exsr      Primasgl
056500140108
056600140108     c* 3) Se l'orario è <= della prima soglia --> tempi "  "
056700140108    1c                   if        do0hti<=§1xhho
056800140108     c                   eval      ksoglia='  '
056900140108     c                   leavesr
057000140108   x1c                   else
057100140108     c
057200140108     c* Se l'orario è > della prima soglia verifico che sia entro la seconda
057300140108     c                   eval      ktbkey=%trimr(ktbkey)+'1'
057400140108     C     KEYTAB        CHAIN     TABEL00F
057500140108    2c                   if        %found(tabel00f)
057600140108     c                   eval      ds1x=tbluni
057700140108    2c                   endif
057800140108
057900151209     c* 4) se NON E' entro seconda seglia --> tempi "  "
058000140108    2c                   if        do0hti>§1xhho
058100140108     c                   eval      ksoglia='  '
058200140108     c                   leavesr
058300140108    2c                   endif
058400140108    1c                   endif
058500140108     c
058600140108     c* 5) ORARIO TRA LE DUE SOGLIE  --> tempi "P "
058700140108     c
058800140108     c                   eval      ksoglia='P '
058900140108     c                   leavesr
059000140108     c
059100140108     C                   ENDSR
059200151209     C**************************************************************************
059300151209     c     Primasgl      BEGSR
059400151209     c* Cerco la soglia della filiale
059500151209     c                   if        not %open(tabel00f)
059600151209     c                   open      tabel00f
059700151209     c                   endif
059800151209     c
059900151209     c                   clear                   ds1x
060000151209     C                   MOVEL     1             KTBKUT
060100151209     C                   MOVEL     '1X'          KTBCOD
060200151209     c                   eval      ktbkey=%editc(d55tfa:'X')+'OSA'
060300151209     C     KEYTAB        CHAIN     TABEL00F
060400151209    1c                   if        not %found(tabel00f)
060500151209     c                   eval      ktbkey='   OSA'
060600151209     C     KEYTAB        CHAIN     TABEL00F
060700151209    1c                   endif
060800151209     c
060900151209    1c                   if        %found(tabel00f)
061000151209     c                   eval      ds1x=tbluni
061100151209    1c                   endif
061200151209     c                   ENDSR
061300130924
061400020805      **************************************************************************
061500960302** SCHIERA MSG - MESSAGGI DI ERRORE
061600130830Linea passata errata                                                           01
061700130920Tipo servizio passato errato: richiedere R-ritiro o C-consegna                 02
061800130830Orari servizi presenti soltanto per nazione ITALIA                             03
061900130830Insieme alla località passare anche il CAP                                     04
062000130920Non sono presenti gli orari servizi                                            05
062100140108Soglia passara errata: richiedere P=pomeriggio T=tuttoILgiorno o nulla         06
062200161026Non sono presenti gli orari servizi CONSEGNE relativi a quelli di Ritiro       07

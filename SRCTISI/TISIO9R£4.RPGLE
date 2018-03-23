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
001300130830     D MSG             S             78    DIM(06) CTDATA PERRCD(1)
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
003200130802     d
003300140108     d tiSIO9ds      e ds
003400140514     d
003500140514     d* Altri dati :   località normalizzata
003600140514     d tisio9ds2       ds
003700140514     D  osio9loc               1     35
003800140514     D  osio9norm             36     36
003900150728     c* ore teoriche trazine cliente e ore teoriche consegna cliente
004000150728     D  osio9trazC            37     39
004100150728     D  osio9consC            40     42
004200150728     D  osio9TR_TC            37     42
004300150728     D  osio9libero           43    100
004400130830     d
004500130830     d tisi95ds      e ds
004600140211     d fnlv55ds      e ds
004700150728
004800151209     d Dataiso         s               d   datfmt(*iso)
004900150728     D digits          c                   '0123456789'
005000060926     d
005100050103     C**********************************************************************
005200040917     C     *ENTRY        PLIST
005300040917     C                   PARM                    KPJBA
005400140107     C                   PARM                    tiSIO9ds
005500140514     C                   PARM                    tiSIO9ds2
005600130830     c
005700130830     c                   clear                   do0msg
005800130830     c                   clear                   do0err
005900131010     c                   clear                   do0rif
006000131010     c                   clear                   do0flo
006100131010     c                   clear                   do0ttc
006200131010     c                   clear                   do0serv
006300131010     c                   clear                   DO0OSTIS
006400131010     c                   clear                   DO0OSTFS
006500131010     c                   clear                   DO0OMIIS
006600131010     c                   clear                   DO0OMxfS
006700131010     c                   clear                   DO0OLRSC
006800131010     c                   clear                   DO0OLRNC
006900131010     c                   clear                   DO0OLRNK
007000140108     c                   clear                   DO0LIA
007100140108     c                   clear                   DO0fcsgl
007200130830     c*
007300130830     c* Controllo dati di input passati
007400060315     c                   exsr      CARINIZ
007500131011     c
007600140108     c* se non ho il tipo servizio giusto impostato non posso nemmeno reperire i dati STD
007700131011     c                   if        DO0err<>' ' and  do0tser<>'C'
007800131011     c                             and do0tser<>'R'
007900131011     c                   goto      fine
008000131011     c                   endif
008100140211     c
008200140211     c* Verifico presenza £6
008300140211     c                   clear                   fnlv55ds
008400140211     C                   EVAL      D55TPT='6'
008500140211     c                   eval      d55lin=do0lna
008600140214     c                   if        do0dri>0
008700140211     c                   eval      d55drf=do0dri
008800140214     c                   else
008900140214     c                   movel     datasys       d55drf
009000140214     c                   endif
009100140214     c
009200140211     c                   call      'FNLV55R'
009300140211     c                   parm                    fnlv55ds
009400131010     c
009500131219     c*  In tutti gli altri casi di errore --> prendo i dati dal record  STD
009600131011     c*
009700131011     c                   if        DO0err<>' '
009800140108     c                   eval      knar=*blanks
009900140108     c                   eval      kcap   =*blanks
010000140108     c                   eval      kloc   =*blanks
010100140108     c                   eval      ksoglia=*blanks
010200140108     c
010300140108     c                   if        do0soglia='P '
010400140108     c                   eval      ksoglia='P '
010500140108     c                   endif
010600140108     c
010700140108     c                   eval      klna=999
010800140108     c                   eval      ktser=do0tser
010900140108     c     kcos          chain     azcos03l
011000131011     c                   exsr      carOUT
011100131011     c                   eval      do0rif='9'
011200131011     c                   goto      fine
011300131011     c                   endif
011400140108
011500140108     c* Verifico se la filiale ha la soglia
011600140108     c
011700140108     c                   eval      knar=*blanks
011800140211     c***                eval      klna=do0lna
011900140211     c                   eval      klna=d55tfa
012000140108     c                   eval      ktser=do0tser
012100140108     c                   eval      ksoglia='P '
012200140108     c                   eval      kcap   =*blanks
012300140108     c                   eval      kloc   =*blanks
012400140108     c     kcos4         chain     azcos04l
012500140108     c                   if        %found(azcos04l)
012600140108     c                   eval      Do0fcsgl='S'
012700140108     c                   endif
012800130830     c*
012900130830     c* Imposto la key
013000131011     c                   eval      knar=*blanks
013100130830     c                   eval      kcap=do0cap
013200140211     c****               eval      klna=do0lna
013300140211     c                   eval      klna=d55tfa
013400130830     c                   eval      ktser=do0tser
013500130830     c                   eval      kloc=do0loc
013600130830     c
013700150505     c* se passata la località la normalizzo se non è già normalizzata
013800130830     c                   if        kloc<>*blanks
013900150505     c                   if        %parms<3 or osio9loc=*blanks
014000130830     c                   exsr      norm_loc
014100150505     c                   else
014200150505     c                   eval      Kloc=osio9loc
014300130830     c                   endif
014400150505     c                   endif
014500140108     c*
014600140108     c                   select
014700140108     c                   when      do0soglia='P '
014800140108     c                   eval      ksoglia='P '
014900140414     c
015000140414     c                   if        do0fcsgl='S'
015100140414     c                   eval      DO0FCSGL='P'
015200140414     c                   endif
015300140108     c
015400140108     c                   when      do0soglia='T '
015500140108     c                   eval      ksoglia='  '
015600140108     c
015700140108     c                   when      do0fcsgl<>'S'
015800140108     c                   eval      ksoglia='  '
015900140108
016000140108     c                   other
016100140108     c                   exsr      Cersoglia
016200140414
016300140414     c                   if        ksoglia='P '
016400140414     c                   eval      DO0FCSGL='P'
016500140414     c                   endif
016600140108     c                   endsl
016700140108     c
016800130830     c* Reperisco gli orari servizio
016900130830     c
017000140108     c     kcos          chain     azcos03l
017100130830
017200140108    1c                   dow       klna<>lna999  and not%found(azcos03l)
017300130830     c                   exsr      Impokey
017400130830
017500140108     c     kcos          chain     azcos03l
017600130830    1c                   enddo
017700130830
017800140108     c                   if        not%found(azcos03l)
017900130830     c                   eval      do0msg=msg(05)
018000140226     c                   eval      do0err='E'
018100130830     c                   goto      fine
018200130830     c                   endif
018300130830     c*
018400131010     c* Passo i dati di output
018500131011     c                   exsr      CarOUT
018600131011     c* compreso il Livello dati trovato
018700130830     c                   select
018800130830     c                   when      kloc<>*blanks
018900130830     c                   eval      do0rif='L'
019000130830     c                   when      kcap<>*blanks
019100130830     c                   eval      do0rif='C'
019200130830     c                   when      klna<>lna999
019300130830     c                   eval      do0rif='F'
019400130830     c                   other
019500130830     c                   eval      do0rif='9'
019600130830     c                   endsl
019700130830     c
019800130830     c     fine          tag
019900060922     c
020000000000     C                   SETON                                        LR
020100130830     C**************************************************************************
020200130830     c* impostazine della chiave per la ricerca su AZCOS a scalare
020300130830     C**************************************************************************
020400130830     c     Impokey       BEGSR
020500130830     c                   select
020600130830     c                   when      kloc<>*blanks
020700130830     c                   clear                   kloc
020800130830     c                   when      kcap<>*blanks
020900130830     c                   clear                   kcap
021000130830     c                   other
021100130830     c                   eval      klna=lna999
021200130830     c                   endsl
021300130830     c                   ENDSR
021400131011     C**************************************************************************
021500131011     c* Impostazione DS campi di output
021600131011     C**************************************************************************
021700131011     c     carOUT        BEGSR
021800131011     c                   eval      do0ostis=cosostis
021900131011     c                   eval      do0ostfs=cosostfs
022000131011     c                   eval      do0omiis=cosomiis
022100131011     c                   eval      do0omxfs=cosomxfs
022200131011     c                   eval      do0olrsc=cosolrsc
022300131011     c                   eval      do0olrnc=cosolrnc
022400131011     c                   eval      do0olrnk=cosolrnk
022500131011     c                   eval      do0ttc  =costtc
022600131011     c                   eval      do0serv =cosserv
022700131011     c                   eval      do0flo  =cosflo
022800140108     c                   eval      do0trzs =costrzs
022900131011     c                   ENDSR
023000050214     C**************************************************************************
023100060315     C     CARiniz       BEGSR
023200130830     c
023300130830     C     KEYTAB        KLIST
023400130830     C                   KFLD                    KTBKUT                         *CODICE UTENTE
023500130830     C                   KFLD                    KTBCOD                         *TABELLA
023600130830     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
023700151209     C     Ktbe          KLIST
023800151209     C                   KFLD                    Kcod                           *CODICE UTENTE
023900151209     C                   KFLD                    Kke1
024000130830     C     KCOS          KLIST
024100130830     C                   KFLD                    Do0VER
024200130830     C                   KFLD                    knar
024300130830     C                   KFLD                    kcap
024400130830     C                   KFLD                    kloc
024500130830     C                   KFLD                    klna
024600130830     C                   KFLD                    ktser
024700140108     C                   KFLD                    ksoglia
024800140108     C     KCOS4         KLIST
024900140108     C                   KFLD                    Do0VER
025000140108     C                   KFLD                    klna
025100140108     C                   KFLD                    knar
025200140108     C                   KFLD                    kcap
025300140108     C                   KFLD                    kloc
025400140108     C                   KFLD                    ktser
025500140108     C                   KFLD                    ksoglia
025600140108     c*
025700140108     C     Kopa          KLIST
025800140108     C                   KFLD                    Do0tfp
025900140108     C                   KFLD                    do0tfa
026000060331     c
026100131011     c* Se la versione non è stata passata uso la data di riferimento per calcolarla
026200130830    1c                   if        do0ver=0
026300130830     c
026400130830    2c                   if        do0dri=0
026500130830     c                   movel     datasys       do0dri
026600130830    2c                   endif
026700130830     c
026800130830     C                   Z-ADD     Do0DRI        P96DRI            8 0
026900130830     C                   Z-ADD     *ZEROS        P96VER            5 0
027000130830     C                   CALL      'TISI96R'
027100130830     C                   PARM                    P96DRI
027200130830     C                   PARM                    P96VER
027300130830     c
027400130830     c                   eval      do0ver=p96ver
027500130830    1c                   endif
027600131011
027700131011     c* Se non passato tipo servizio --> errore
027800131011     c                   if        do0tser<>'R' and do0tser<>'C'
027900131011     c                   eval      do0msg=msg(02)
028000131011     c                   eval      do0err='E'
028100131011     c                   leavesr
028200131011     c                   endif
028300140108     c*
028400140108     c* se il campo soglia non è P=POM o T=TUTTO il giorno errore e lo abblanco
028500140108     c                   if        do0soglia<>*blanks and do0soglia<>'P ' and
028600140108     c                             do0soglia<>'T '
028700140108     c                   eval      do0msg=msg(06)
028800140108     c                   eval      do0err='E'
028900140108     c                   leavesr
029000140108     c                   endif
029100130830     c
029200130830     c                   if        do0lna>0  and do0lna<>999
029300130830     c
029400140514     c***  do0lna        chain     azorg01l
029500130830     c
029600140514     c***                if        not %found(azorg01l)
029700140514     c***                          or orgfag<>'F' and orgfag<>'A'
029800140514     c***                eval      do0msg=msg(01)
029900140514     c***                eval      do0err='E'
030000140514     c***                leavesr
030100140514     c***                endif
030200130924     c
030300130924     c* Se la linea passata non è partita con la nuova procedura emetto errore
030400140414     c***                movel     do0lna        w003a             3
030500140414     c***                eval      Indx=%lookup(w003a:fil_ab)
030600140414     c***
030700140414     c***                if        indx=0 and fil_ab(1)<>'999' and do0tric<>'S'
030800140414     c***                eval      do0msg=msg(05)
030900140414     c***                eval      do0err='A'
031000140414     c***                endif
031100130924     c
031200130924     c                   endif
031300130830     c
031400130830     c* se non ho passato la linea di arrivo imposto 999 se cap e loc=*blanks
031500130830     c*  altrimenti errore
031600130830     c                   if        do0lna=0 and do0cap=*blanks and do0loc=*blanks
031700130830     c                   eval      do0lna=999
031800130830     c                   endif
031900130830     c* Se non passata linea di arrivo --> errore
032000130830     c                   if        do0lna=0
032100130830     c                   eval      do0msg=msg(01)
032200131011     c                   eval      do0err='E'
032300130830     c                   leavesr
032400130830     c                   endif
032500130830     c
032600131011     c* Se passata località e non CAP --> errore
032700130830     c                   if        do0loc<>*blanks
032800130830     c                             and do0cap=*blanks
032900130830     c                   eval      do0msg=msg(04)
033000131011     c                   eval      do0err='E'
033100130830     c                   endif
033200130830     c
033300130830     c* Ammessa soltanto nazione ITALIA
033400140514     c                   if        do0nar<>*blanks
033500140514     c                   open      tabel00f
033600130830     C                   MOVEL     1             KTBKUT
033700130830     C                   MOVEL     '15'          KTBCOD
033800130830     C                   MOVEL(P)  do0nar        KTBKEY
033900130830     C     KEYTAB        CHAIN     TABEL00F                           99
034000130830     c                   clear                   ds15
034100130830IF  1C     *IN99         IFEQ      *OFF
034200130830     C     TBLFLG        ANDEQ     *BLANKS
034300130830     C                   MOVEL     TBLUNI        DS15
034400130830     c                   endif
034500130830     c                   if        §15ita<>'I'
034600130830     c                   eval      do0msg=msg(03)
034700131011     c                   eval      do0err='A'
034800130830     c                   endif
034900140514     c                   endif
035000130830     c
035100130830     C                   ENDSR
035200130830     C**************************************************************************
035300130830     c* Normalizzo la località
035400130830     C**************************************************************************
035500130830     C     Norm_loc      BEGSR
035600130830     c                   clear                   tisi95ds
035700140516     C                   MOVEL     '3'           I95TCN
035800130830     c                   eval      i95loc=do0loc
035900130830     c                   eval      i95cap=do0cap
036000130830     c                   eval      i95nar=do0nar
036100130830     c                   eval      i95dat=do0dri
036200140516     C                   call      'TISI95R'
036300140516     C                   parm                    tisi95ds
036400140516     C*
036500130830     c
036600130830     C                   IF        O95ERR= ' ' AND o95loc<>*blanks
036700130830     c                   eval      kloc=o95loc
036800130830     c                   else
036900130830     c                   eval      kloc=do0loc
037000130830     c                   endif
037100140514     c
037200140514     c* Passo in output la località normalizzata
037300140514     c                   if        %parms=3
037400140514     c                   eval      osio9loc  =kloc
037500140514     c* Se normalizzata memorizzo
037600140514     C                   if        o95lid = '3' or o95lid = '4'
037700140514     c                   eval      osio9NORM='S'
037800140514     c                   endif
037900140514     c                   endif
038000130830     c
038100130830     C                   ENDSR
038200140108     C**************************************************************************
038300140108     c* Cerco il record giust per la soglia se la filiale lo prevede
038400140108     C**************************************************************************
038500140414     c     CerSoglia     BEGSR
038600150728     c                   clear                   wtraz             3 0
038700150728     c                   clear                   wcons             3 0
038800151209     c                   clear                   wdoppiag          1
038900151209     c                   clear                   wdti              8 0
039000151209     c                   eval      Wdti=do0dti
039100151209     c
039200151209     c* Verifico se la linea di arrivo è una eccezione
039300151209     c                   open(e)   tntbe01l
039400151209     c                   eval      kcod='FLO'
039500151209     c                   eval      kke1=%editc(do0lna:'X')
039600151211     c     ktbe          chain     tntbe01l
039700151209    1c                   if        %found(tntbe01l) and tbeatb=' '
039800151209     c                   eval      wdoppiag='S'
039900151209
040000151209     c* Se c'è la data e non l'orario vado a reperire l'orario
040100151209    2c                   if        do0dti>0 and do0hti=9999
040200151209     c                   open(e)   waopa01l
040300151209     c     kopa          chain     waopa01l
040400151209    3c                   if        %found(waopa01l)
040500151209     c                   eval      do0hti=opahha
040600151209    3c                   endif
040700151209    2c                   endif
040800151209     c
040900151209     c* se è una eccezione, verifico se orario oltre seconda soglia --> aggiungo 1 giorno
041000151209     c* alla data di arrivo
041100160226    2c                   if        do0hti>0 and do0hti<9999
041200151209     c                   exsr      Primasgl
041300151209    3c                   if        do0hti> §1xhho
041400151209     c                   eval      ktbkey=%trimr(ktbkey)+'1'
041500151209     C     KEYTAB        CHAIN     TABEL00F
041600151209    4c                   if        %found(tabel00f)
041700151209     c                   eval      ds1x=tbluni
041800151209    4c                   endif
041900151209
042000151209     c* 4) se oltre seconda soglia aggiungo 1 giorno alla data
042100160226    4c                   if        do0hti>§1xhho
042200151209     c                   Move      do0dti        Dataiso
042300151209     c                   Adddur    1:*d          Dataiso
042400151209     c                   Move      Dataiso       wdti
042500151209    4c                   endif
042600151209    3c                   endif
042700151209    2c                   endif
042800151209
042900151209    1c                   endif
043000140514     c
043100140108     c* 1) Se bolla in distinta e data arrivo trazione < data distinta --> tempi "  "
043200151209     c*                                                                --> tempi"2GG'
043300151209    1c****               if        do0ddc>0 and do0dti>0 and do0dti<do0ddc
043400151209    1c                   if        do0ddc>0 and do0dti>0 and wdti<do0ddc
043500151209     c                   if        wdoppiag=' '
043600140108     c                   eval      ksoglia='  '
043700151209     c                   else
043800151209     c                   eval      ksoglia='P '
043900151209     c                   endif
044000151209     c
044100140108     c                   leavesr
044200140108    1c                   endif
044300140526
044400140526     c* 1a) Se bolla NON in distinta e data arrivo trazione < data del giorno--> tempi "  "
044500140526     c                   movel     datasys       wdrf              8 0
044600151209    1c****               if        do0ddc=0 and do0dti>0 and do0dti<wdrf
044700151209    1c                   if        do0ddc=0 and do0dti>0 and wdti  <wdrf
044800151209     c                   if        wdoppiag=' '
044900140526     c                   eval      ksoglia='  '
045000151209     c                   else
045100151209     c                   eval      ksoglia='P '
045200151209     c                   endif
045300151209     c
045400140526     c                   leavesr
045500140526    1c                   endif
045600140108
045700140108     c* 2) Se consegna richiesta DOPO il o IL > consegna prevista --> tempi "  "
045800140109     c                   if        do0dcr>0  and do0tcr<>'P'  and do0dei>0
045900140109     c                   if        do0tcr=' ' and do0dcr>do0dei
046000151209     c                   if        wdoppiag=' '
046100140109     c                   eval      ksoglia='  '
046200151209     c                   else
046300151209     c                   eval      ksoglia='P '
046400151209     c                   endif
046500151209     c
046600140109     c                   leavesr
046700140108     c                   endif
046800140109     c                   if        do0tcr='D' and do0dcr>=do0dei
046900151209     c                   if        wdoppiag=' '
047000140109     c                   eval      ksoglia='  '
047100151209     c                   else
047200151209     c                   eval      ksoglia='P '
047300151209     c                   endif
047400151209     c
047500140109     c                   leavesr
047600140109     c                   endif
047700140109     c                   endif
047800150721     c
047900150721     c* 3) Se la data prevista consegna interna è > della data arrivo ultimo collo --> tempi "  "
048000151209    1c*****              if        do0dei>0 and do0dti>0 and do0dti<do0dei
048100151209    1c                   if        do0dei>0 and do0dti>0 and wdti  <do0dei
048200151209     c                   if        wdoppiag=' '
048300150721     c                   eval      ksoglia='  '
048400151209     c                   else
048500151209     c                   eval      ksoglia='P '
048600151209     c                   endif
048700151209     c
048800150721     c                   leavesr
048900150721    1c                   endif
049000150728     c* 4) In mancanza della data arrivo ultimo collo se i tempi TRZ+CONS sono > 48ore--> tempi "  "
049100150728     c                   if        %parms>=3 and osio9TR_TC >*zeros and do0dti=0
049200150728     c
049300150728    2c                   if        %check(digits:osio9trazC)=0
049400150728     c                   eval      wtraz=%int(osio9trazC)
049500150728    3c                   if        %check(digits:osio9consC)=0
049600150728     c                   eval      wcons=%int(osio9consC)
049700150728     c
049800150728     C     wtraz         DIV       24            WDIV              3 0
049900150728     C                   MVR                     WRES              3 0
050000150728     c
050100150728    4c                   if        wres>0 and wcons>12
050200151209     c                   if        wdoppiag=' '
050300150723     c                   eval      ksoglia='  '
050400151209     c                   else
050500151209     c                   eval      ksoglia='P '
050600151209     c                   endif
050700151209     c
050800150723     c                   leavesr
050900150728    4c                   endif
051000150728    4c                   if        wres=0 and wcons>24
051100151209     c                   if        wdoppiag=' '
051200150728     c                   eval      ksoglia='  '
051300151209     c                   else
051400151209     c                   eval      ksoglia='P '
051500151209     c                   endif
051600151209     c
051700150728     c                   leavesr
051800150728    4c                   endif
051900150728    c
052000150728    3c                   endif
052100150728    2c                   endif
052200150728    1c                   endif
052300140108     c*
052400140108     c* Se orario arrivo trazione non passato lo cerco dal file degli arrivi standard
052500140428     c* Orario = 9999 serve quando si ha la data di arrivo ma non l'ora. Ad esempio
052600140428     c*  per le bolle di sede TITAS
052700140428    1c                   if        (do0dti=0 and do0hti=0) or do0hti=9999
052800140514     c                   open      waopa01l
052900140110     c     kopa          chain     waopa01l
053000140110    2c                   if        %found(waopa01l)
053100140108     c                   eval      do0hti=opahha
053200140108    2c                   endif
053300140108    1c                   endif
053400140108     c
053500151209     c                   exsr      Primasgl
053600140108
053700140108     c* 3) Se l'orario è <= della prima soglia --> tempi "  "
053800140108    1c                   if        do0hti<=§1xhho
053900140108     c                   eval      ksoglia='  '
054000140108     c                   leavesr
054100140108   x1c                   else
054200140108     c
054300140108     c* Se l'orario è > della prima soglia verifico che sia entro la seconda
054400140108     c                   eval      ktbkey=%trimr(ktbkey)+'1'
054500140108     C     KEYTAB        CHAIN     TABEL00F
054600140108    2c                   if        %found(tabel00f)
054700140108     c                   eval      ds1x=tbluni
054800140108    2c                   endif
054900140108
055000151209     c* 4) se NON E' entro seconda seglia --> tempi "  "
055100140108    2c                   if        do0hti>§1xhho
055200140108     c                   eval      ksoglia='  '
055300140108     c                   leavesr
055400140108    2c                   endif
055500140108    1c                   endif
055600140108     c
055700140108     c* 5) ORARIO TRA LE DUE SOGLIE  --> tempi "P "
055800140108     c
055900140108     c                   eval      ksoglia='P '
056000140108     c                   leavesr
056100140108     c
056200140108     C                   ENDSR
056300151209     C**************************************************************************
056400151209     c     Primasgl      BEGSR
056500151209     c* Cerco la soglia della filiale
056600151209     c                   if        not %open(tabel00f)
056700151209     c                   open      tabel00f
056800151209     c                   endif
056900151209     c
057000151209     c                   clear                   ds1x
057100151209     C                   MOVEL     1             KTBKUT
057200151209     C                   MOVEL     '1X'          KTBCOD
057300151209     c                   eval      ktbkey=%editc(d55tfa:'X')+'OSA'
057400151209     C     KEYTAB        CHAIN     TABEL00F
057500151209    1c                   if        not %found(tabel00f)
057600151209     c                   eval      ktbkey='   OSA'
057700151209     C     KEYTAB        CHAIN     TABEL00F
057800151209    1c                   endif
057900151209     c
058000151209    1c                   if        %found(tabel00f)
058100151209     c                   eval      ds1x=tbluni
058200151209    1c                   endif
058300151209     c                   ENDSR
058400130924
058500020805      **************************************************************************
058600960302** SCHIERA MSG - MESSAGGI DI ERRORE
058700130830Linea passata errata                                                           01
058800130920Tipo servizio passato errato: richiedere R-ritiro o C-consegna                 02
058900130830Orari servizi presenti soltanto per nazione ITALIA                             03
059000130830Insieme alla località passare anche il CAP                                     04
059100130920Non sono presenti gli orari servizi                                            05
059200140108Soglia passara errata: richiedere P=pomeriggio T=tuttoILgiorno o nulla         06

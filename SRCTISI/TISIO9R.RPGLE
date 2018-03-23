000100040917     H DECEDIT('0,') DATEDIT(*yMd.)
000200131219      * TISIO9R *----------------------------------------------------*
000300130830      *         - Reperimento servizi orari  Cappario
000400940915      *--------------------------------------------------------------*
000500140514     Fwaopa01l  iF   E           K DISK    usropn
000600170925     Fazcpc03l  iF   E           K DISK
000700170925     Fazcos03l  iF   E           K DISK
000800140108     Fazcos04l  iF   E           K DISK    rename(azcos000:azcos004)
000900140514     Ftabel00f  IF   E           K DISK    usropn
001000151209     Ftntbe01l  IF   E           K DISK    usropn
001100020805      *
001200020805      * DEFINIZIONE SCHIERE
001300960302     D*
001400161026     D MSG             S             78    DIM(07) CTDATA PERRCD(1)
001500060315     d
001600040917     D KPJBA         E DS
001700130830     D ds15          E DS
001800140108     D ds1X          E DS
001900060922     d
002000130830     d Datasys         s               d   datfmt(*iso) inz(*sys)
002100151209     d kcod            s                   like(tbecod)
002200151209     d kke1            s                   like(tbeke1)
002300151209     d ktbkut          s                   like(tblkut)
002400130830     d ktbcod          s                   like(tblcod)
002500130830     d ktbkey          s                   like(tblkey)
002600130830     d kloc            s                   like(cosloc)
002700130830     d klna            s                   like(coslna)
002800130830     d lna999          s                   like(coslna) inz(999)
002900130830     d kcap            s                   like(coscap)
003000130830     d knar            s                   like(cosnar)
003100140108     d ktser           s                   like(costser)
003200140108     d ksoglia         s                   like(cosserv)
003300161026     d savloc          s                   like(cosloc)
003400130802     d
003500140108     d tiSIO9ds      e ds
003600140514     d
003700140514     d* Altri dati :   località normalizzata
003800140514     d tisio9ds2       ds
003900140514     D  osio9loc               1     35
004000140514     D  osio9norm             36     36
004100150728     c* ore teoriche trazine cliente e ore teoriche consegna cliente
004200150728     D  osio9trazC            37     39
004300150728     D  osio9consC            40     42
004400150728     D  osio9TR_TC            37     42
004500150728     D  osio9libero           43    100
004600130830     d
004700130830     d tisi95ds      e ds
004800140211     d fnlv55ds      e ds
004900150728
005000151209     d Dataiso         s               d   datfmt(*iso)
005100150728     D digits          c                   '0123456789'
005200060926     d
005300050103     C**********************************************************************
005400040917     C     *ENTRY        PLIST
005500040917     C                   PARM                    KPJBA
005600140107     C                   PARM                    tiSIO9ds
005700140514     C                   PARM                    tiSIO9ds2
005800130830     c
005900130830     c                   clear                   do0msg
006000130830     c                   clear                   do0err
006100131010     c                   clear                   do0rif
006200131010     c                   clear                   do0flo
006300131010     c                   clear                   do0ttc
006400131010     c                   clear                   do0serv
006500131010     c                   clear                   DO0OSTIS
006600131010     c                   clear                   DO0OSTFS
006700131010     c                   clear                   DO0OMIIS
006800131010     c                   clear                   DO0OMxfS
006900131010     c                   clear                   DO0OLRSC
007000131010     c                   clear                   DO0OLRNC
007100131010     c                   clear                   DO0OLRNK
007200140108     c                   clear                   DO0LIA
007300140108     c                   clear                   DO0fcsgl
007400130830     c*
007500130830     c* Controllo dati di input passati
007600060315     c                   exsr      CARINIZ
007700131011     c
007800140108     c* se non ho il tipo servizio giusto impostato non posso nemmeno reperire i dati STD
007900131011     c                   if        DO0err<>' ' and  do0tser<>'C'
008000131011     c                             and do0tser<>'R'
008100131011     c                   goto      fine
008200131011     c                   endif
008300170925
008400170925     c* Verifico se la filiale è presente sul cappario: se non c'è verifico £6
008500170925     c*  per cercare gli orari della sua "gestrice"
008600170925     c                   clear                   fnlv55ds
008700170925     c                   clear                   knar
008800170925     c                   eval      klna=do0lna
008900170925     c     kcpc          setll     azcpc03l
009000170925     c                   if        %equal(azcpc03l)
009100170925     c                   eval      d55tfa=do0lna
009200170925     c                   else
009300140211     c
009400140211     c* Verifico presenza £6
009500140211     C                   EVAL      D55TPT='6'
009600140211     c                   eval      d55lin=do0lna
009700140214     c                   if        do0dri>0
009800140211     c                   eval      d55drf=do0dri
009900140214     c                   else
010000140214     c                   movel     datasys       d55drf
010100140214     c                   endif
010200170925     c                   eval      d55tla='L'
010300140214     c
010400140211     c                   call      'FNLV55R'
010500140211     c                   parm                    fnlv55ds
010600170925     c                   endif
010700131010     c
010800131219     c*  In tutti gli altri casi di errore --> prendo i dati dal record  STD
010900131011     c*
011000131011     c                   if        DO0err<>' '
011100140108     c                   eval      knar=*blanks
011200140108     c                   eval      kcap   =*blanks
011300140108     c                   eval      kloc   =*blanks
011400140108     c                   eval      ksoglia=*blanks
011500140108     c
011600140108     c                   if        do0soglia='P '
011700140108     c                   eval      ksoglia='P '
011800140108     c                   endif
011900140108     c
012000140108     c                   eval      klna=999
012100140108     c                   eval      ktser=do0tser
012200140108     c     kcos          chain     azcos03l
012300131011     c                   exsr      carOUT
012400131011     c                   eval      do0rif='9'
012500131011     c                   goto      fine
012600131011     c                   endif
012700140108
012800140108     c* Verifico se la filiale ha la soglia
012900140108     c
013000140108     c                   eval      knar=*blanks
013100140211     c***                eval      klna=do0lna
013200140211     c                   eval      klna=d55tfa
013300140108     c                   eval      ktser=do0tser
013400140108     c                   eval      ksoglia='P '
013500140108     c                   eval      kcap   =*blanks
013600140108     c                   eval      kloc   =*blanks
013700140108     c     kcos4         chain     azcos04l
013800140108     c                   if        %found(azcos04l)
013900140108     c                   eval      Do0fcsgl='S'
014000140108     c                   endif
014100130830     c*
014200130830     c* Imposto la key
014300131011     c                   eval      knar=*blanks
014400130830     c                   eval      kcap=do0cap
014500140211     c****               eval      klna=do0lna
014600140211     c                   eval      klna=d55tfa
014700130830     c                   eval      ktser=do0tser
014800130830     c                   eval      kloc=do0loc
014900130830     c
015000150505     c* se passata la località la normalizzo se non è già normalizzata
015100130830     c                   if        kloc<>*blanks
015200150505     c                   if        %parms<3 or osio9loc=*blanks
015300130830     c                   exsr      norm_loc
015400150505     c                   else
015500150505     c                   eval      Kloc=osio9loc
015600130830     c                   endif
015700150505     c                   endif
015800161026     c
015900161026     c                   eval      savloc=kloc
016000140108     c*
016100140108     c                   select
016200140108     c                   when      do0soglia='P '
016300140108     c                   eval      ksoglia='P '
016400140414     c
016500140414     c                   if        do0fcsgl='S'
016600140414     c                   eval      DO0FCSGL='P'
016700140414     c                   endif
016800140108     c
016900140108     c                   when      do0soglia='T '
017000140108     c                   eval      ksoglia='  '
017100140108     c
017200140108     c                   when      do0fcsgl<>'S'
017300140108     c                   eval      ksoglia='  '
017400140108
017500140108     c                   other
017600140108     c                   exsr      Cersoglia
017700140414
017800140414     c                   if        ksoglia='P '
017900140414     c                   eval      DO0FCSGL='P'
018000140414     c                   endif
018100140108     c                   endsl
018200140108     c
018300130830     c* Reperisco gli orari servizio
018400130830     c
018500140108     c     kcos          chain     azcos03l
018600130830
018700140108    1c                   dow       klna<>lna999  and not%found(azcos03l)
018800130830     c                   exsr      Impokey
018900130830
019000140108     c     kcos          chain     azcos03l
019100130830    1c                   enddo
019200130830
019300140108     c                   if        not%found(azcos03l)
019400130830     c                   eval      do0msg=msg(05)
019500140226     c                   eval      do0err='E'
019600130830     c                   goto      fine
019700130830     c                   endif
019800130830     c*
019900131010     c* Passo i dati di output
020000131011     c                   exsr      CarOUT
020100131011     c* compreso il Livello dati trovato
020200130830     c                   select
020300130830     c                   when      kloc<>*blanks
020400130830     c                   eval      do0rif='L'
020500130830     c                   when      kcap<>*blanks
020600130830     c                   eval      do0rif='C'
020700130830     c                   when      klna<>lna999
020800130830     c                   eval      do0rif='F'
020900130830     c                   other
021000130830     c                   eval      do0rif='9'
021100130830     c                   endsl
021200161026     c*
021300161026     c* Nel caso di RITIRO, devo prendere anche i dati consegne STD, per impostare
021400161026     c*  minimo e massimo
021500161026     c                   if        do0tser= 'R'
021600161026     c                   eval      knar=*blanks
021700161026     c                   eval      kcap=do0cap
021800161026     c                   eval      klna=d55tfa
021900161026     c                   eval      ktser='C'
022000161026     c                   eval      kloc=savloc
022100161026     c                   clear                   ksoglia
022200161026     c     kcos          chain     azcos03l
022300161026
022400161026    1c                   dow       klna<>lna999  and not%found(azcos03l)
022500161026     c                   exsr      Impokey
022600161026
022700161026     c     kcos          chain     azcos03l
022800161026    1c                   enddo
022900161026     c                   if        not%found(azcos03l)
023000161026     c                   eval      do0msg=msg(07)
023100161026     c                   eval      do0err='E'
023200161026     c                   else
023300161026     c                   eval      do0omiis=cosostis
023400161026     c                   eval      do0omxfs=cosostfs
023500161026     c                   endif
023600161026     c
023700161026     c                   endif
023800130830     c
023900130830     c     fine          tag
024000060922     c
024100000000     C                   SETON                                        LR
024200130830     C**************************************************************************
024300130830     c* impostazine della chiave per la ricerca su AZCOS a scalare
024400130830     C**************************************************************************
024500130830     c     Impokey       BEGSR
024600130830     c                   select
024700130830     c                   when      kloc<>*blanks
024800130830     c                   clear                   kloc
024900130830     c                   when      kcap<>*blanks
025000130830     c                   clear                   kcap
025100130830     c                   other
025200130830     c                   eval      klna=lna999
025300130830     c                   endsl
025400130830     c                   ENDSR
025500131011     C**************************************************************************
025600131011     c* Impostazione DS campi di output
025700131011     C**************************************************************************
025800131011     c     carOUT        BEGSR
025900131011     c                   eval      do0ostis=cosostis
026000131011     c                   eval      do0ostfs=cosostfs
026100131011     c                   eval      do0omiis=cosomiis
026200131011     c                   eval      do0omxfs=cosomxfs
026300131011     c                   eval      do0olrsc=cosolrsc
026400131011     c                   eval      do0olrnc=cosolrnc
026500131011     c                   eval      do0olrnk=cosolrnk
026600131011     c                   eval      do0ttc  =costtc
026700131011     c                   eval      do0serv =cosserv
026800131011     c                   eval      do0flo  =cosflo
026900140108     c                   eval      do0trzs =costrzs
027000131011     c                   ENDSR
027100050214     C**************************************************************************
027200060315     C     CARiniz       BEGSR
027300130830     c
027400130830     C     KEYTAB        KLIST
027500130830     C                   KFLD                    KTBKUT                         *CODICE UTENTE
027600130830     C                   KFLD                    KTBCOD                         *TABELLA
027700130830     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
027800151209     C     Ktbe          KLIST
027900151209     C                   KFLD                    Kcod                           *CODICE UTENTE
028000151209     C                   KFLD                    Kke1
028100130830     C     KCOS          KLIST
028200130830     C                   KFLD                    Do0VER
028300130830     C                   KFLD                    knar
028400130830     C                   KFLD                    kcap
028500130830     C                   KFLD                    kloc
028600130830     C                   KFLD                    klna
028700130830     C                   KFLD                    ktser
028800140108     C                   KFLD                    ksoglia
028900140108     C     KCOS4         KLIST
029000140108     C                   KFLD                    Do0VER
029100140108     C                   KFLD                    klna
029200140108     C                   KFLD                    knar
029300140108     C                   KFLD                    kcap
029400140108     C                   KFLD                    kloc
029500140108     C                   KFLD                    ktser
029600140108     C                   KFLD                    ksoglia
029700170925     C     KCpc          KLIST
029800170925     C                   KFLD                    Do0VER
029900170925     C                   KFLD                    knar
030000170925     C                   KFLD                    klna
030100140108     c*
030200140108     C     Kopa          KLIST
030300140108     C                   KFLD                    Do0tfp
030400140108     C                   KFLD                    do0tfa
030500060331     c
030600131011     c* Se la versione non è stata passata uso la data di riferimento per calcolarla
030700130830    1c                   if        do0ver=0
030800130830     c
030900130830    2c                   if        do0dri=0
031000130830     c                   movel     datasys       do0dri
031100130830    2c                   endif
031200130830     c
031300130830     C                   Z-ADD     Do0DRI        P96DRI            8 0
031400130830     C                   Z-ADD     *ZEROS        P96VER            5 0
031500130830     C                   CALL      'TISI96R'
031600130830     C                   PARM                    P96DRI
031700130830     C                   PARM                    P96VER
031800130830     c
031900130830     c                   eval      do0ver=p96ver
032000130830    1c                   endif
032100131011
032200131011     c* Se non passato tipo servizio --> errore
032300131011     c                   if        do0tser<>'R' and do0tser<>'C'
032400131011     c                   eval      do0msg=msg(02)
032500131011     c                   eval      do0err='E'
032600131011     c                   leavesr
032700131011     c                   endif
032800140108     c*
032900140108     c* se il campo soglia non è P=POM o T=TUTTO il giorno errore e lo abblanco
033000140108     c                   if        do0soglia<>*blanks and do0soglia<>'P ' and
033100140108     c                             do0soglia<>'T '
033200140108     c                   eval      do0msg=msg(06)
033300140108     c                   eval      do0err='E'
033400140108     c                   leavesr
033500140108     c                   endif
033600130830     c
033700130830     c                   if        do0lna>0  and do0lna<>999
033800130830     c
033900140514     c***  do0lna        chain     azorg01l
034000130830     c
034100140514     c***                if        not %found(azorg01l)
034200140514     c***                          or orgfag<>'F' and orgfag<>'A'
034300140514     c***                eval      do0msg=msg(01)
034400140514     c***                eval      do0err='E'
034500140514     c***                leavesr
034600140514     c***                endif
034700130924     c
034800130924     c* Se la linea passata non è partita con la nuova procedura emetto errore
034900140414     c***                movel     do0lna        w003a             3
035000140414     c***                eval      Indx=%lookup(w003a:fil_ab)
035100140414     c***
035200140414     c***                if        indx=0 and fil_ab(1)<>'999' and do0tric<>'S'
035300140414     c***                eval      do0msg=msg(05)
035400140414     c***                eval      do0err='A'
035500140414     c***                endif
035600130924     c
035700130924     c                   endif
035800130830     c
035900130830     c* se non ho passato la linea di arrivo imposto 999 se cap e loc=*blanks
036000130830     c*  altrimenti errore
036100130830     c                   if        do0lna=0 and do0cap=*blanks and do0loc=*blanks
036200130830     c                   eval      do0lna=999
036300130830     c                   endif
036400130830     c* Se non passata linea di arrivo --> errore
036500130830     c                   if        do0lna=0
036600130830     c                   eval      do0msg=msg(01)
036700131011     c                   eval      do0err='E'
036800130830     c                   leavesr
036900130830     c                   endif
037000130830     c
037100131011     c* Se passata località e non CAP --> errore
037200130830     c                   if        do0loc<>*blanks
037300130830     c                             and do0cap=*blanks
037400130830     c                   eval      do0msg=msg(04)
037500131011     c                   eval      do0err='E'
037600130830     c                   endif
037700130830     c
037800130830     c* Ammessa soltanto nazione ITALIA
037900140514     c                   if        do0nar<>*blanks
038000140514     c                   open      tabel00f
038100130830     C                   MOVEL     1             KTBKUT
038200130830     C                   MOVEL     '15'          KTBCOD
038300130830     C                   MOVEL(P)  do0nar        KTBKEY
038400130830     C     KEYTAB        CHAIN     TABEL00F                           99
038500130830     c                   clear                   ds15
038600130830IF  1C     *IN99         IFEQ      *OFF
038700130830     C     TBLFLG        ANDEQ     *BLANKS
038800130830     C                   MOVEL     TBLUNI        DS15
038900130830     c                   endif
039000130830     c                   if        §15ita<>'I'
039100130830     c                   eval      do0msg=msg(03)
039200131011     c                   eval      do0err='A'
039300130830     c                   endif
039400140514     c                   endif
039500130830     c
039600130830     C                   ENDSR
039700130830     C**************************************************************************
039800130830     c* Normalizzo la località
039900130830     C**************************************************************************
040000130830     C     Norm_loc      BEGSR
040100130830     c                   clear                   tisi95ds
040200140516     C                   MOVEL     '3'           I95TCN
040300130830     c                   eval      i95loc=do0loc
040400130830     c                   eval      i95cap=do0cap
040500130830     c                   eval      i95nar=do0nar
040600130830     c                   eval      i95dat=do0dri
040700140516     C                   call      'TISI95R'
040800140516     C                   parm                    tisi95ds
040900140516     C*
041000130830     c
041100130830     C                   IF        O95ERR= ' ' AND o95loc<>*blanks
041200130830     c                   eval      kloc=o95loc
041300130830     c                   else
041400130830     c                   eval      kloc=do0loc
041500130830     c                   endif
041600140514     c
041700140514     c* Passo in output la località normalizzata
041800140514     c                   if        %parms=3
041900140514     c                   eval      osio9loc  =kloc
042000140514     c* Se normalizzata memorizzo
042100140514     C                   if        o95lid = '3' or o95lid = '4'
042200140514     c                   eval      osio9NORM='S'
042300140514     c                   endif
042400140514     c                   endif
042500130830     c
042600130830     C                   ENDSR
042700140108     C**************************************************************************
042800140108     c* Cerco il record giust per la soglia se la filiale lo prevede
042900140108     C**************************************************************************
043000140414     c     CerSoglia     BEGSR
043100150728     c                   clear                   wtraz             3 0
043200150728     c                   clear                   wcons             3 0
043300151209     c                   clear                   wdoppiag          1
043400151209     c                   clear                   wdti              8 0
043500151209     c                   eval      Wdti=do0dti
043600151209     c
043700151209     c* Verifico se la linea di arrivo è una eccezione
043800151209     c                   open(e)   tntbe01l
043900151209     c                   eval      kcod='FLO'
044000151209     c                   eval      kke1=%editc(do0lna:'X')
044100151211     c     ktbe          chain     tntbe01l
044200151209    1c                   if        %found(tntbe01l) and tbeatb=' '
044300151209     c                   eval      wdoppiag='S'
044400151209
044500151209     c* Se c'è la data e non l'orario vado a reperire l'orario
044600151209    2c                   if        do0dti>0 and do0hti=9999
044700151209     c                   open(e)   waopa01l
044800151209     c     kopa          chain     waopa01l
044900151209    3c                   if        %found(waopa01l)
045000151209     c                   eval      do0hti=opahha
045100151209    3c                   endif
045200151209    2c                   endif
045300151209     c
045400151209     c* se è una eccezione, verifico se orario oltre seconda soglia --> aggiungo 1 giorno
045500151209     c* alla data di arrivo
045600160226    2c                   if        do0hti>0 and do0hti<9999
045700151209     c                   exsr      Primasgl
045800151209    3c                   if        do0hti> §1xhho
045900151209     c                   eval      ktbkey=%trimr(ktbkey)+'1'
046000151209     C     KEYTAB        CHAIN     TABEL00F
046100151209    4c                   if        %found(tabel00f)
046200151209     c                   eval      ds1x=tbluni
046300151209    4c                   endif
046400151209
046500151209     c* 4) se oltre seconda soglia aggiungo 1 giorno alla data
046600160226    4c                   if        do0hti>§1xhho
046700151209     c                   Move      do0dti        Dataiso
046800151209     c                   Adddur    1:*d          Dataiso
046900151209     c                   Move      Dataiso       wdti
047000151209    4c                   endif
047100151209    3c                   endif
047200151209    2c                   endif
047300151209
047400151209    1c                   endif
047500140514     c
047600140108     c* 1) Se bolla in distinta e data arrivo trazione < data distinta --> tempi "  "
047700151209     c*                                                                --> tempi"2GG'
047800151209    1c****               if        do0ddc>0 and do0dti>0 and do0dti<do0ddc
047900151209    1c                   if        do0ddc>0 and do0dti>0 and wdti<do0ddc
048000151209     c                   if        wdoppiag=' '
048100140108     c                   eval      ksoglia='  '
048200151209     c                   else
048300151209     c                   eval      ksoglia='P '
048400151209     c                   endif
048500151209     c
048600140108     c                   leavesr
048700140108    1c                   endif
048800140526
048900140526     c* 1a) Se bolla NON in distinta e data arrivo trazione < data del giorno--> tempi "  "
049000140526     c                   movel     datasys       wdrf              8 0
049100151209    1c****               if        do0ddc=0 and do0dti>0 and do0dti<wdrf
049200151209    1c                   if        do0ddc=0 and do0dti>0 and wdti  <wdrf
049300151209     c                   if        wdoppiag=' '
049400140526     c                   eval      ksoglia='  '
049500151209     c                   else
049600151209     c                   eval      ksoglia='P '
049700151209     c                   endif
049800151209     c
049900140526     c                   leavesr
050000140526    1c                   endif
050100140108
050200140108     c* 2) Se consegna richiesta DOPO il o IL > consegna prevista --> tempi "  "
050300140109     c                   if        do0dcr>0  and do0tcr<>'P'  and do0dei>0
050400140109     c                   if        do0tcr=' ' and do0dcr>do0dei
050500151209     c                   if        wdoppiag=' '
050600140109     c                   eval      ksoglia='  '
050700151209     c                   else
050800151209     c                   eval      ksoglia='P '
050900151209     c                   endif
051000151209     c
051100140109     c                   leavesr
051200140108     c                   endif
051300140109     c                   if        do0tcr='D' and do0dcr>=do0dei
051400151209     c                   if        wdoppiag=' '
051500140109     c                   eval      ksoglia='  '
051600151209     c                   else
051700151209     c                   eval      ksoglia='P '
051800151209     c                   endif
051900151209     c
052000140109     c                   leavesr
052100140109     c                   endif
052200140109     c                   endif
052300150721     c
052400150721     c* 3) Se la data prevista consegna interna è > della data arrivo ultimo collo --> tempi "  "
052500151209    1c*****              if        do0dei>0 and do0dti>0 and do0dti<do0dei
052600151209    1c                   if        do0dei>0 and do0dti>0 and wdti  <do0dei
052700151209     c                   if        wdoppiag=' '
052800150721     c                   eval      ksoglia='  '
052900151209     c                   else
053000151209     c                   eval      ksoglia='P '
053100151209     c                   endif
053200151209     c
053300150721     c                   leavesr
053400150721    1c                   endif
053500150728     c* 4) In mancanza della data arrivo ultimo collo se i tempi TRZ+CONS sono > 48ore--> tempi "  "
053600150728     c                   if        %parms>=3 and osio9TR_TC >*zeros and do0dti=0
053700150728     c
053800150728    2c                   if        %check(digits:osio9trazC)=0
053900150728     c                   eval      wtraz=%int(osio9trazC)
054000150728    3c                   if        %check(digits:osio9consC)=0
054100150728     c                   eval      wcons=%int(osio9consC)
054200150728     c
054300150728     C     wtraz         DIV       24            WDIV              3 0
054400150728     C                   MVR                     WRES              3 0
054500150728     c
054600150728    4c                   if        wres>0 and wcons>12
054700151209     c                   if        wdoppiag=' '
054800150723     c                   eval      ksoglia='  '
054900151209     c                   else
055000151209     c                   eval      ksoglia='P '
055100151209     c                   endif
055200151209     c
055300150723     c                   leavesr
055400150728    4c                   endif
055500150728    4c                   if        wres=0 and wcons>24
055600151209     c                   if        wdoppiag=' '
055700150728     c                   eval      ksoglia='  '
055800151209     c                   else
055900151209     c                   eval      ksoglia='P '
056000151209     c                   endif
056100151209     c
056200150728     c                   leavesr
056300150728    4c                   endif
056400150728    c
056500150728    3c                   endif
056600150728    2c                   endif
056700150728    1c                   endif
056800140108     c*
056900140108     c* Se orario arrivo trazione non passato lo cerco dal file degli arrivi standard
057000140428     c* Orario = 9999 serve quando si ha la data di arrivo ma non l'ora. Ad esempio
057100140428     c*  per le bolle di sede TITAS
057200140428    1c                   if        (do0dti=0 and do0hti=0) or do0hti=9999
057300140514     c                   open      waopa01l
057400140110     c     kopa          chain     waopa01l
057500140110    2c                   if        %found(waopa01l)
057600140108     c                   eval      do0hti=opahha
057700140108    2c                   endif
057800140108    1c                   endif
057900140108     c
058000151209     c                   exsr      Primasgl
058100140108
058200140108     c* 3) Se l'orario è <= della prima soglia --> tempi "  "
058300140108    1c                   if        do0hti<=§1xhho
058400140108     c                   eval      ksoglia='  '
058500140108     c                   leavesr
058600140108   x1c                   else
058700140108     c
058800140108     c* Se l'orario è > della prima soglia verifico che sia entro la seconda
058900140108     c                   eval      ktbkey=%trimr(ktbkey)+'1'
059000140108     C     KEYTAB        CHAIN     TABEL00F
059100140108    2c                   if        %found(tabel00f)
059200140108     c                   eval      ds1x=tbluni
059300140108    2c                   endif
059400140108
059500151209     c* 4) se NON E' entro seconda seglia --> tempi "  "
059600140108    2c                   if        do0hti>§1xhho
059700140108     c                   eval      ksoglia='  '
059800140108     c                   leavesr
059900140108    2c                   endif
060000140108    1c                   endif
060100140108     c
060200140108     c* 5) ORARIO TRA LE DUE SOGLIE  --> tempi "P "
060300140108     c
060400140108     c                   eval      ksoglia='P '
060500140108     c                   leavesr
060600140108     c
060700140108     C                   ENDSR
060800151209     C**************************************************************************
060900151209     c     Primasgl      BEGSR
061000151209     c* Cerco la soglia della filiale
061100151209     c                   if        not %open(tabel00f)
061200151209     c                   open      tabel00f
061300151209     c                   endif
061400151209     c
061500151209     c                   clear                   ds1x
061600151209     C                   MOVEL     1             KTBKUT
061700151209     C                   MOVEL     '1X'          KTBCOD
061800151209     c                   eval      ktbkey=%editc(d55tfa:'X')+'OSA'
061900151209     C     KEYTAB        CHAIN     TABEL00F
062000151209    1c                   if        not %found(tabel00f)
062100151209     c                   eval      ktbkey='   OSA'
062200151209     C     KEYTAB        CHAIN     TABEL00F
062300151209    1c                   endif
062400151209     c
062500151209    1c                   if        %found(tabel00f)
062600151209     c                   eval      ds1x=tbluni
062700151209    1c                   endif
062800151209     c                   ENDSR
062900130924
063000020805      **************************************************************************
063100960302** SCHIERA MSG - MESSAGGI DI ERRORE
063200130830Linea passata errata                                                           01
063300130920Tipo servizio passato errato: richiedere R-ritiro o C-consegna                 02
063400130830Orari servizi presenti soltanto per nazione ITALIA                             03
063500130830Insieme alla località passare anche il CAP                                     04
063600130920Non sono presenti gli orari servizi                                            05
063700140108Soglia passara errata: richiedere P=pomeriggio T=tuttoILgiorno o nulla         06
063800161026Non sono presenti gli orari servizi CONSEGNE relativi a quelli di Ritiro       07

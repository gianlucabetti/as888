000100040917     H DECEDIT('0,') DATEDIT(*yMd.)
000200131219      * TISIO9R *----------------------------------------------------*
000300130830      *         - Reperimento servizi orari  Cappario
000400940915      *--------------------------------------------------------------*
000500140514     Fwaopa01l  iF   E           K DISK    usropn
000600140108     Fazcos03l  iF   E           K DISK
000700140108     Fazcos04l  iF   E           K DISK    rename(azcos000:azcos004)
000800140514     Ftabel00f  IF   E           K DISK    usropn
000900020805      *
001000020805      * DEFINIZIONE SCHIERE
001100960302     D*
001200130830     D MSG             S             78    DIM(06) CTDATA PERRCD(1)
001300060315     d
001400040917     D KPJBA         E DS
001500130830     D ds15          E DS
001600140108     D ds1X          E DS
001700060922     d
001800130830     d Datasys         s               d   datfmt(*iso) inz(*sys)
001900130830     d ktbkut          s                   like(tblkut)
002000130830     d ktbcod          s                   like(tblcod)
002100130830     d ktbkey          s                   like(tblkey)
002200130830     d kloc            s                   like(cosloc)
002300130830     d klna            s                   like(coslna)
002400130830     d lna999          s                   like(coslna) inz(999)
002500130830     d kcap            s                   like(coscap)
002600130830     d knar            s                   like(cosnar)
002700140108     d ktser           s                   like(costser)
002800140108     d ksoglia         s                   like(cosserv)
002900130802     d
003000140108     d tiSIO9ds      e ds
003100140514     d
003200140514     d* Altri dati :   località normalizzata
003300140514     d tisio9ds2       ds
003400140514     D  osio9loc               1     35
003500140514     D  osio9norm             36     36
003600150728     c* ore teoriche trazine cliente e ore teoriche consegna cliente
003700150728     D  osio9trazC            37     39
003800150728     D  osio9consC            40     42
003900150728     D  osio9TR_TC            37     42
004000150728     D  osio9libero           43    100
004100130830     d
004200130830     d tisi95ds      e ds
004300140211     d fnlv55ds      e ds
004400150728
004500150728     D digits          c                   '0123456789'
004600060926     d
004700050103     C**********************************************************************
004800040917     C     *ENTRY        PLIST
004900040917     C                   PARM                    KPJBA
005000140107     C                   PARM                    tiSIO9ds
005100140514     C                   PARM                    tiSIO9ds2
005200130830     c
005300130830     c                   clear                   do0msg
005400130830     c                   clear                   do0err
005500131010     c                   clear                   do0rif
005600131010     c                   clear                   do0flo
005700131010     c                   clear                   do0ttc
005800131010     c                   clear                   do0serv
005900131010     c                   clear                   DO0OSTIS
006000131010     c                   clear                   DO0OSTFS
006100131010     c                   clear                   DO0OMIIS
006200131010     c                   clear                   DO0OMxfS
006300131010     c                   clear                   DO0OLRSC
006400131010     c                   clear                   DO0OLRNC
006500131010     c                   clear                   DO0OLRNK
006600140108     c                   clear                   DO0LIA
006700140108     c                   clear                   DO0fcsgl
006800130830     c*
006900130830     c* Controllo dati di input passati
007000060315     c                   exsr      CARINIZ
007100131011     c
007200140108     c* se non ho il tipo servizio giusto impostato non posso nemmeno reperire i dati STD
007300131011     c                   if        DO0err<>' ' and  do0tser<>'C'
007400131011     c                             and do0tser<>'R'
007500131011     c                   goto      fine
007600131011     c                   endif
007700140211     c
007800140211     c* Verifico presenza £6
007900140211     c                   clear                   fnlv55ds
008000140211     C                   EVAL      D55TPT='6'
008100140211     c                   eval      d55lin=do0lna
008200140214     c                   if        do0dri>0
008300140211     c                   eval      d55drf=do0dri
008400140214     c                   else
008500140214     c                   movel     datasys       d55drf
008600140214     c                   endif
008700140214     c
008800140211     c                   call      'FNLV55R'
008900140211     c                   parm                    fnlv55ds
009000131010     c
009100131219     c*  In tutti gli altri casi di errore --> prendo i dati dal record  STD
009200131011     c*
009300131011     c                   if        DO0err<>' '
009400140108     c                   eval      knar=*blanks
009500140108     c                   eval      kcap   =*blanks
009600140108     c                   eval      kloc   =*blanks
009700140108     c                   eval      ksoglia=*blanks
009800140108     c
009900140108     c                   if        do0soglia='P '
010000140108     c                   eval      ksoglia='P '
010100140108     c                   endif
010200140108     c
010300140108     c                   eval      klna=999
010400140108     c                   eval      ktser=do0tser
010500140108     c     kcos          chain     azcos03l
010600131011     c                   exsr      carOUT
010700131011     c                   eval      do0rif='9'
010800131011     c                   goto      fine
010900131011     c                   endif
011000140108
011100140108     c* Verifico se la filiale ha la soglia
011200140108     c
011300140108     c                   eval      knar=*blanks
011400140211     c***                eval      klna=do0lna
011500140211     c                   eval      klna=d55tfa
011600140108     c                   eval      ktser=do0tser
011700140108     c                   eval      ksoglia='P '
011800140108     c                   eval      kcap   =*blanks
011900140108     c                   eval      kloc   =*blanks
012000140108     c     kcos4         chain     azcos04l
012100140108     c                   if        %found(azcos04l)
012200140108     c                   eval      Do0fcsgl='S'
012300140108     c                   endif
012400130830     c*
012500130830     c* Imposto la key
012600131011     c                   eval      knar=*blanks
012700130830     c                   eval      kcap=do0cap
012800140211     c****               eval      klna=do0lna
012900140211     c                   eval      klna=d55tfa
013000130830     c                   eval      ktser=do0tser
013100130830     c                   eval      kloc=do0loc
013200130830     c
013300150505     c* se passata la località la normalizzo se non è già normalizzata
013400130830     c                   if        kloc<>*blanks
013500150505     c                   if        %parms<3 or osio9loc=*blanks
013600130830     c                   exsr      norm_loc
013700150505     c                   else
013800150505     c                   eval      Kloc=osio9loc
013900130830     c                   endif
014000150505     c                   endif
014100140108     c*
014200140108     c                   select
014300140108     c                   when      do0soglia='P '
014400140108     c                   eval      ksoglia='P '
014500140414     c
014600140414     c                   if        do0fcsgl='S'
014700140414     c                   eval      DO0FCSGL='P'
014800140414     c                   endif
014900140108     c
015000140108     c                   when      do0soglia='T '
015100140108     c                   eval      ksoglia='  '
015200140108     c
015300140108     c                   when      do0fcsgl<>'S'
015400140108     c                   eval      ksoglia='  '
015500140108
015600140108     c                   other
015700140108     c                   exsr      Cersoglia
015800140414
015900140414     c                   if        ksoglia='P '
016000140414     c                   eval      DO0FCSGL='P'
016100140414     c                   endif
016200140108     c                   endsl
016300140108     c
016400130830     c* Reperisco gli orari servizio
016500130830     c
016600140108     c     kcos          chain     azcos03l
016700130830
016800140108    1c                   dow       klna<>lna999  and not%found(azcos03l)
016900130830     c                   exsr      Impokey
017000130830
017100140108     c     kcos          chain     azcos03l
017200130830    1c                   enddo
017300130830
017400140108     c                   if        not%found(azcos03l)
017500130830     c                   eval      do0msg=msg(05)
017600140226     c                   eval      do0err='E'
017700130830     c                   goto      fine
017800130830     c                   endif
017900130830     c*
018000131010     c* Passo i dati di output
018100131011     c                   exsr      CarOUT
018200131011     c* compreso il Livello dati trovato
018300130830     c                   select
018400130830     c                   when      kloc<>*blanks
018500130830     c                   eval      do0rif='L'
018600130830     c                   when      kcap<>*blanks
018700130830     c                   eval      do0rif='C'
018800130830     c                   when      klna<>lna999
018900130830     c                   eval      do0rif='F'
019000130830     c                   other
019100130830     c                   eval      do0rif='9'
019200130830     c                   endsl
019300130830     c
019400130830     c     fine          tag
019500060922     c
019600000000     C                   SETON                                        LR
019700130830     C**************************************************************************
019800130830     c* impostazine della chiave per la ricerca su AZCOS a scalare
019900130830     C**************************************************************************
020000130830     c     Impokey       BEGSR
020100130830     c                   select
020200130830     c                   when      kloc<>*blanks
020300130830     c                   clear                   kloc
020400130830     c                   when      kcap<>*blanks
020500130830     c                   clear                   kcap
020600130830     c                   other
020700130830     c                   eval      klna=lna999
020800130830     c                   endsl
020900130830     c                   ENDSR
021000131011     C**************************************************************************
021100131011     c* Impostazione DS campi di output
021200131011     C**************************************************************************
021300131011     c     carOUT        BEGSR
021400131011     c                   eval      do0ostis=cosostis
021500131011     c                   eval      do0ostfs=cosostfs
021600131011     c                   eval      do0omiis=cosomiis
021700131011     c                   eval      do0omxfs=cosomxfs
021800131011     c                   eval      do0olrsc=cosolrsc
021900131011     c                   eval      do0olrnc=cosolrnc
022000131011     c                   eval      do0olrnk=cosolrnk
022100131011     c                   eval      do0ttc  =costtc
022200131011     c                   eval      do0serv =cosserv
022300131011     c                   eval      do0flo  =cosflo
022400140108     c                   eval      do0trzs =costrzs
022500131011     c                   ENDSR
022600050214     C**************************************************************************
022700060315     C     CARiniz       BEGSR
022800130830     c
022900130830     C     KEYTAB        KLIST
023000130830     C                   KFLD                    KTBKUT                         *CODICE UTENTE
023100130830     C                   KFLD                    KTBCOD                         *TABELLA
023200130830     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
023300130830     C     KCOS          KLIST
023400130830     C                   KFLD                    Do0VER
023500130830     C                   KFLD                    knar
023600130830     C                   KFLD                    kcap
023700130830     C                   KFLD                    kloc
023800130830     C                   KFLD                    klna
023900130830     C                   KFLD                    ktser
024000140108     C                   KFLD                    ksoglia
024100140108     C     KCOS4         KLIST
024200140108     C                   KFLD                    Do0VER
024300140108     C                   KFLD                    klna
024400140108     C                   KFLD                    knar
024500140108     C                   KFLD                    kcap
024600140108     C                   KFLD                    kloc
024700140108     C                   KFLD                    ktser
024800140108     C                   KFLD                    ksoglia
024900140108     c*
025000140108     C     Kopa          KLIST
025100140108     C                   KFLD                    Do0tfp
025200140108     C                   KFLD                    do0tfa
025300060331     c
025400131011     c* Se la versione non è stata passata uso la data di riferimento per calcolarla
025500130830    1c                   if        do0ver=0
025600130830     c
025700130830    2c                   if        do0dri=0
025800130830     c                   movel     datasys       do0dri
025900130830    2c                   endif
026000130830     c
026100130830     C                   Z-ADD     Do0DRI        P96DRI            8 0
026200130830     C                   Z-ADD     *ZEROS        P96VER            5 0
026300130830     C                   CALL      'TISI96R'
026400130830     C                   PARM                    P96DRI
026500130830     C                   PARM                    P96VER
026600130830     c
026700130830     c                   eval      do0ver=p96ver
026800130830    1c                   endif
026900131011
027000131011     c* Se non passato tipo servizio --> errore
027100131011     c                   if        do0tser<>'R' and do0tser<>'C'
027200131011     c                   eval      do0msg=msg(02)
027300131011     c                   eval      do0err='E'
027400131011     c                   leavesr
027500131011     c                   endif
027600140108     c*
027700140108     c* se il campo soglia non è P=POM o T=TUTTO il giorno errore e lo abblanco
027800140108     c                   if        do0soglia<>*blanks and do0soglia<>'P ' and
027900140108     c                             do0soglia<>'T '
028000140108     c                   eval      do0msg=msg(06)
028100140108     c                   eval      do0err='E'
028200140108     c                   leavesr
028300140108     c                   endif
028400130830     c
028500130830     c                   if        do0lna>0  and do0lna<>999
028600130830     c
028700140514     c***  do0lna        chain     azorg01l
028800130830     c
028900140514     c***                if        not %found(azorg01l)
029000140514     c***                          or orgfag<>'F' and orgfag<>'A'
029100140514     c***                eval      do0msg=msg(01)
029200140514     c***                eval      do0err='E'
029300140514     c***                leavesr
029400140514     c***                endif
029500130924     c
029600130924     c* Se la linea passata non è partita con la nuova procedura emetto errore
029700140414     c***                movel     do0lna        w003a             3
029800140414     c***                eval      Indx=%lookup(w003a:fil_ab)
029900140414     c***
030000140414     c***                if        indx=0 and fil_ab(1)<>'999' and do0tric<>'S'
030100140414     c***                eval      do0msg=msg(05)
030200140414     c***                eval      do0err='A'
030300140414     c***                endif
030400130924     c
030500130924     c                   endif
030600130830     c
030700130830     c* se non ho passato la linea di arrivo imposto 999 se cap e loc=*blanks
030800130830     c*  altrimenti errore
030900130830     c                   if        do0lna=0 and do0cap=*blanks and do0loc=*blanks
031000130830     c                   eval      do0lna=999
031100130830     c                   endif
031200130830     c* Se non passata linea di arrivo --> errore
031300130830     c                   if        do0lna=0
031400130830     c                   eval      do0msg=msg(01)
031500131011     c                   eval      do0err='E'
031600130830     c                   leavesr
031700130830     c                   endif
031800130830     c
031900131011     c* Se passata località e non CAP --> errore
032000130830     c                   if        do0loc<>*blanks
032100130830     c                             and do0cap=*blanks
032200130830     c                   eval      do0msg=msg(04)
032300131011     c                   eval      do0err='E'
032400130830     c                   endif
032500130830     c
032600130830     c* Ammessa soltanto nazione ITALIA
032700140514     c                   if        do0nar<>*blanks
032800140514     c                   open      tabel00f
032900130830     C                   MOVEL     1             KTBKUT
033000130830     C                   MOVEL     '15'          KTBCOD
033100130830     C                   MOVEL(P)  do0nar        KTBKEY
033200130830     C     KEYTAB        CHAIN     TABEL00F                           99
033300130830     c                   clear                   ds15
033400130830IF  1C     *IN99         IFEQ      *OFF
033500130830     C     TBLFLG        ANDEQ     *BLANKS
033600130830     C                   MOVEL     TBLUNI        DS15
033700130830     c                   endif
033800130830     c                   if        §15ita<>'I'
033900130830     c                   eval      do0msg=msg(03)
034000131011     c                   eval      do0err='A'
034100130830     c                   endif
034200140514     c                   endif
034300130830     c
034400130830     C                   ENDSR
034500130830     C**************************************************************************
034600130830     c* Normalizzo la località
034700130830     C**************************************************************************
034800130830     C     Norm_loc      BEGSR
034900130830     c                   clear                   tisi95ds
035000140516     C                   MOVEL     '3'           I95TCN
035100130830     c                   eval      i95loc=do0loc
035200130830     c                   eval      i95cap=do0cap
035300130830     c                   eval      i95nar=do0nar
035400130830     c                   eval      i95dat=do0dri
035500140516     C                   call      'TISI95R'
035600140516     C                   parm                    tisi95ds
035700140516     C*
035800130830     c
035900130830     C                   IF        O95ERR= ' ' AND o95loc<>*blanks
036000130830     c                   eval      kloc=o95loc
036100130830     c                   else
036200130830     c                   eval      kloc=do0loc
036300130830     c                   endif
036400140514     c
036500140514     c* Passo in output la località normalizzata
036600140514     c                   if        %parms=3
036700140514     c                   eval      osio9loc  =kloc
036800140514     c* Se normalizzata memorizzo
036900140514     C                   if        o95lid = '3' or o95lid = '4'
037000140514     c                   eval      osio9NORM='S'
037100140514     c                   endif
037200140514     c                   endif
037300130830     c
037400130830     C                   ENDSR
037500140108     C**************************************************************************
037600140108     c* Cerco il record giust per la soglia se la filiale lo prevede
037700140108     C**************************************************************************
037800140414     c     CerSoglia     BEGSR
037900150728     c                   clear                   wtraz             3 0
038000150728     c                   clear                   wcons             3 0
038100140514     c
038200140108     c* 1) Se bolla in distinta e data arrivo trazione < data distinta --> tempi "  "
038300140526    1c                   if        do0ddc>0 and do0dti>0 and do0dti<do0ddc
038400140108     c                   eval      ksoglia='  '
038500140108     c                   leavesr
038600140108    1c                   endif
038700140526
038800140526     c* 1a) Se bolla NON in distinta e data arrivo trazione < data del giorno--> tempi "  "
038900140526     c                   movel     datasys       wdrf              8 0
039000140526    1c                   if        do0ddc=0 and do0dti>0 and do0dti<wdrf
039100140526     c                   eval      ksoglia='  '
039200140526     c                   leavesr
039300140526    1c                   endif
039400140108
039500140108     c* 2) Se consegna richiesta DOPO il o IL > consegna prevista --> tempi "  "
039600140109     c                   if        do0dcr>0  and do0tcr<>'P'  and do0dei>0
039700140109     c                   if        do0tcr=' ' and do0dcr>do0dei
039800140109     c                   eval      ksoglia='  '
039900140109     c                   leavesr
040000140108     c                   endif
040100140109     c                   if        do0tcr='D' and do0dcr>=do0dei
040200140109     c                   eval      ksoglia='  '
040300140109     c                   leavesr
040400140109     c                   endif
040500140109     c                   endif
040600150721     c
040700150721     c* 3) Se la data prevista consegna interna è > della data arrivo ultimo collo --> tempi "  "
040800150721    1c                   if        do0dei>0 and do0dti>0 and do0dti<do0dei
040900150721     c                   eval      ksoglia='  '
041000150721     c                   leavesr
041100150721    1c                   endif
041200150728     c* 4) In mancanza della data arrivo ultimo collo se i tempi TRZ+CONS sono > 48ore--> tempi "  "
041300150728     c                   if        %parms>=3 and osio9TR_TC >*zeros and do0dti=0
041400150728     c
041500150728    2c                   if        %check(digits:osio9trazC)=0
041600150728     c                   eval      wtraz=%int(osio9trazC)
041700150728    3c                   if        %check(digits:osio9consC)=0
041800150728     c                   eval      wcons=%int(osio9consC)
041900150728     c
042000150728     C     wtraz         DIV       24            WDIV              3 0
042100150728     C                   MVR                     WRES              3 0
042200150728     c
042300150728    4c                   if        wres>0 and wcons>12
042400150723     c                   eval      ksoglia='  '
042500150723     c                   leavesr
042600150728    4c                   endif
042700150728    4c                   if        wres=0 and wcons>24
042800150728     c                   eval      ksoglia='  '
042900150728     c                   leavesr
043000150728    4c                   endif
043100150728    c
043200150728    3c                   endif
043300150728    2c                   endif
043400150728    1c                   endif
043500140108     c*
043600140108     c* Se orario arrivo trazione non passato lo cerco dal file degli arrivi standard
043700140428     c* Orario = 9999 serve quando si ha la data di arrivo ma non l'ora. Ad esempio
043800140428     c*  per le bolle di sede TITAS
043900140428    1c                   if        (do0dti=0 and do0hti=0) or do0hti=9999
044000140514     c                   open      waopa01l
044100140110     c     kopa          chain     waopa01l
044200140110    2c                   if        %found(waopa01l)
044300140108     c                   eval      do0hti=opahha
044400140108    2c                   endif
044500140108    1c                   endif
044600140108     c
044700140108     c* Cerco la soglia della filiale
044800140514     c                   if        not %open(tabel00f)
044900140514     c                   open      tabel00f
045000140514     c                   endif
045100140514     c
045200140108     c                   clear                   ds1x
045300140108     C                   MOVEL     1             KTBKUT
045400140108     C                   MOVEL     '1X'          KTBCOD
045500140211     c*****              eval      ktbkey=%editc(do0lna:'X')+'OSA'
045600140211     c                   eval      ktbkey=%editc(d55tfa:'X')+'OSA'
045700140108     C     KEYTAB        CHAIN     TABEL00F
045800140108    1c                   if        not %found(tabel00f)
045900140108     c                   eval      ktbkey='   OSA'
046000140108     C     KEYTAB        CHAIN     TABEL00F
046100140108    1c                   endif
046200140108     c
046300140108    1c                   if        %found(tabel00f)
046400140108     c                   eval      ds1x=tbluni
046500140108    1c                   endif
046600140108
046700140108     c* 3) Se l'orario è <= della prima soglia --> tempi "  "
046800140108    1c                   if        do0hti<=§1xhho
046900140108     c                   eval      ksoglia='  '
047000140108     c                   leavesr
047100140108   x1c                   else
047200140108     c
047300140108     c* Se l'orario è > della prima soglia verifico che sia entro la seconda
047400140108     c                   eval      ktbkey=%trimr(ktbkey)+'1'
047500140108     C     KEYTAB        CHAIN     TABEL00F
047600140108    2c                   if        %found(tabel00f)
047700140108     c                   eval      ds1x=tbluni
047800140108    2c                   endif
047900140108
048000140108     c* 4) se NON E' entro seconda seglia --A tempi "  "
048100140108    2c                   if        do0hti>§1xhho
048200140108     c                   eval      ksoglia='  '
048300140108     c                   leavesr
048400140108    2c                   endif
048500140108    1c                   endif
048600140108     c
048700140108     c* 5) ORARIO TRA LE DUE SOGLIE  --> tempi "P "
048800140108     c
048900140108     c                   eval      ksoglia='P '
049000140108     c                   leavesr
049100140108     c
049200140108     C                   ENDSR
049300130924
049400020805      **************************************************************************
049500960302** SCHIERA MSG - MESSAGGI DI ERRORE
049600130830Linea passata errata                                                           01
049700130920Tipo servizio passato errato: richiedere R-ritiro o C-consegna                 02
049800130830Orari servizi presenti soltanto per nazione ITALIA                             03
049900130830Insieme alla località passare anche il CAP                                     04
050000130920Non sono presenti gli orari servizi                                            05
050100140108Soglia passara errata: richiedere P=pomeriggio T=tuttoILgiorno o nulla         06

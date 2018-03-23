000100040917     H DECEDIT('0,') DATEDIT(*yMd.)
000200131219      * TISIO9R *----------------------------------------------------*
000300130830      *         - Reperimento servizi orari  Cappario
000400940915      *--------------------------------------------------------------*
000500140514     Fwaopa01l  iF   E           K DISK    usropn
000600140108     Fazcos03l  iF   E           K DISK
000700140108     Fazcos04l  iF   E           K DISK    rename(azcos000:azcos004)
000800140514     F**azorg01l  iF   E           K DISK
000900140514     Ftabel00f  IF   E           K DISK    usropn
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
002000130830     d ktbkut          s                   like(tblkut)
002100130830     d ktbcod          s                   like(tblcod)
002200130830     d ktbkey          s                   like(tblkey)
002300130830     d kloc            s                   like(cosloc)
002400130830     d klna            s                   like(coslna)
002500130830     d lna999          s                   like(coslna) inz(999)
002600130830     d kcap            s                   like(coscap)
002700130830     d knar            s                   like(cosnar)
002800140108     d ktser           s                   like(costser)
002900140108     d ksoglia         s                   like(cosserv)
003000130802     d
003100140108     d tiSIO9ds      e ds
003200140514     d
003300140514     d* Altri dati :   località normalizzata
003400140514     d tisio9ds2       ds
003500140514     D  osio9loc               1     35
003600140514     D  osio9norm             36     36
003700140514     D  osio9libero           37    100
003800130830     d
003900130830     d tisi95ds      e ds
004000140211     d fnlv55ds      e ds
004100060926     d
004200050103     C**********************************************************************
004300040917     C     *ENTRY        PLIST
004400040917     C                   PARM                    KPJBA
004500140107     C                   PARM                    tiSIO9ds
004600140514     C                   PARM                    tiSIO9ds2
004700130830     c
004800130830     c                   clear                   do0msg
004900130830     c                   clear                   do0err
005000131010     c                   clear                   do0rif
005100131010     c                   clear                   do0flo
005200131010     c                   clear                   do0ttc
005300131010     c                   clear                   do0serv
005400131010     c                   clear                   DO0OSTIS
005500131010     c                   clear                   DO0OSTFS
005600131010     c                   clear                   DO0OMIIS
005700131010     c                   clear                   DO0OMxfS
005800131010     c                   clear                   DO0OLRSC
005900131010     c                   clear                   DO0OLRNC
006000131010     c                   clear                   DO0OLRNK
006100140108     c                   clear                   DO0LIA
006200140108     c                   clear                   DO0fcsgl
006300130830     c*
006400130830     c* Controllo dati di input passati
006500060315     c                   exsr      CARINIZ
006600131011     c
006700140108     c* se non ho il tipo servizio giusto impostato non posso nemmeno reperire i dati STD
006800131011     c                   if        DO0err<>' ' and  do0tser<>'C'
006900131011     c                             and do0tser<>'R'
007000131011     c                   goto      fine
007100131011     c                   endif
007200140211     c
007300140211     c* Verifico presenza £6
007400140211     c                   clear                   fnlv55ds
007500140211     C                   EVAL      D55TPT='6'
007600140211     c                   eval      d55lin=do0lna
007700140214     c                   if        do0dri>0
007800140211     c                   eval      d55drf=do0dri
007900140214     c                   else
008000140214     c                   movel     datasys       d55drf
008100140214     c                   endif
008200140214     c
008300140211     c                   call      'FNLV55R'
008400140211     c                   parm                    fnlv55ds
008500131010     c
008600131219     c*  In tutti gli altri casi di errore --> prendo i dati dal record  STD
008700131011     c*
008800131011     c                   if        DO0err<>' '
008900140108     c                   eval      knar=*blanks
009000140108     c                   eval      kcap   =*blanks
009100140108     c                   eval      kloc   =*blanks
009200140108     c                   eval      ksoglia=*blanks
009300140108     c
009400140108     c                   if        do0soglia='P '
009500140108     c                   eval      ksoglia='P '
009600140108     c                   endif
009700140108     c
009800140108     c                   eval      klna=999
009900140108     c                   eval      ktser=do0tser
010000140108     c     kcos          chain     azcos03l
010100131011     c                   exsr      carOUT
010200131011     c                   eval      do0rif='9'
010300131011     c                   goto      fine
010400131011     c                   endif
010500140108
010600140108     c* Verifico se la filiale ha la soglia
010700140108     c
010800140108     c                   eval      knar=*blanks
010900140211     c***                eval      klna=do0lna
011000140211     c                   eval      klna=d55tfa
011100140108     c                   eval      ktser=do0tser
011200140108     c                   eval      ksoglia='P '
011300140108     c                   eval      kcap   =*blanks
011400140108     c                   eval      kloc   =*blanks
011500140108     c     kcos4         chain     azcos04l
011600140108     c                   if        %found(azcos04l)
011700140108     c                   eval      Do0fcsgl='S'
011800140108     c                   endif
011900130830     c*
012000130830     c* Imposto la key
012100131011     c                   eval      knar=*blanks
012200130830     c                   eval      kcap=do0cap
012300140211     c****               eval      klna=do0lna
012400140211     c                   eval      klna=d55tfa
012500130830     c                   eval      ktser=do0tser
012600130830     c                   eval      kloc=do0loc
012700130830     c
012800150505     c* se passata la località la normalizzo se non è già normalizzata
012900130830     c                   if        kloc<>*blanks
013000150505     c                   if        %parms<3 or osio9loc=*blanks
013100130830     c                   exsr      norm_loc
013200150505     c                   else
013300150505     c                   eval      Kloc=osio9loc
013400130830     c                   endif
013500150505     c                   endif
013600140108     c*
013700140108     c                   select
013800140108     c                   when      do0soglia='P '
013900140108     c                   eval      ksoglia='P '
014000140414     c
014100140414     c                   if        do0fcsgl='S'
014200140414     c                   eval      DO0FCSGL='P'
014300140414     c                   endif
014400140108     c
014500140108     c                   when      do0soglia='T '
014600140108     c                   eval      ksoglia='  '
014700140108     c
014800140108     c                   when      do0fcsgl<>'S'
014900140108     c                   eval      ksoglia='  '
015000140108
015100140108     c                   other
015200140108     c                   exsr      Cersoglia
015300140414
015400140414     c                   if        ksoglia='P '
015500140414     c                   eval      DO0FCSGL='P'
015600140414     c                   endif
015700140108     c                   endsl
015800140108     c
015900130830     c* Reperisco gli orari servizio
016000130830     c
016100140108     c     kcos          chain     azcos03l
016200130830
016300140108    1c                   dow       klna<>lna999  and not%found(azcos03l)
016400130830     c                   exsr      Impokey
016500130830
016600140108     c     kcos          chain     azcos03l
016700130830    1c                   enddo
016800130830
016900140108     c                   if        not%found(azcos03l)
017000130830     c                   eval      do0msg=msg(05)
017100140226     c                   eval      do0err='E'
017200130830     c                   goto      fine
017300130830     c                   endif
017400130830     c*
017500131010     c* Passo i dati di output
017600131011     c                   exsr      CarOUT
017700131011     c* compreso il Livello dati trovato
017800130830     c                   select
017900130830     c                   when      kloc<>*blanks
018000130830     c                   eval      do0rif='L'
018100130830     c                   when      kcap<>*blanks
018200130830     c                   eval      do0rif='C'
018300130830     c                   when      klna<>lna999
018400130830     c                   eval      do0rif='F'
018500130830     c                   other
018600130830     c                   eval      do0rif='9'
018700130830     c                   endsl
018800130830     c
018900130830     c     fine          tag
019000060922     c
019100000000     C                   SETON                                        LR
019200130830     C**************************************************************************
019300130830     c* impostazine della chiave per la ricerca su AZCOS a scalare
019400130830     C**************************************************************************
019500130830     c     Impokey       BEGSR
019600130830     c                   select
019700130830     c                   when      kloc<>*blanks
019800130830     c                   clear                   kloc
019900130830     c                   when      kcap<>*blanks
020000130830     c                   clear                   kcap
020100130830     c                   other
020200130830     c                   eval      klna=lna999
020300130830     c                   endsl
020400130830     c                   ENDSR
020500131011     C**************************************************************************
020600131011     c* Impostazione DS campi di output
020700131011     C**************************************************************************
020800131011     c     carOUT        BEGSR
020900131011     c                   eval      do0ostis=cosostis
021000131011     c                   eval      do0ostfs=cosostfs
021100131011     c                   eval      do0omiis=cosomiis
021200131011     c                   eval      do0omxfs=cosomxfs
021300131011     c                   eval      do0olrsc=cosolrsc
021400131011     c                   eval      do0olrnc=cosolrnc
021500131011     c                   eval      do0olrnk=cosolrnk
021600131011     c                   eval      do0ttc  =costtc
021700131011     c                   eval      do0serv =cosserv
021800131011     c                   eval      do0flo  =cosflo
021900140108     c                   eval      do0trzs =costrzs
022000131011     c                   ENDSR
022100050214     C**************************************************************************
022200060315     C     CARiniz       BEGSR
022300130830     c
022400130830     C     KEYTAB        KLIST
022500130830     C                   KFLD                    KTBKUT                         *CODICE UTENTE
022600130830     C                   KFLD                    KTBCOD                         *TABELLA
022700130830     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
022800130830     C     KCOS          KLIST
022900130830     C                   KFLD                    Do0VER
023000130830     C                   KFLD                    knar
023100130830     C                   KFLD                    kcap
023200130830     C                   KFLD                    kloc
023300130830     C                   KFLD                    klna
023400130830     C                   KFLD                    ktser
023500140108     C                   KFLD                    ksoglia
023600140108     C     KCOS4         KLIST
023700140108     C                   KFLD                    Do0VER
023800140108     C                   KFLD                    klna
023900140108     C                   KFLD                    knar
024000140108     C                   KFLD                    kcap
024100140108     C                   KFLD                    kloc
024200140108     C                   KFLD                    ktser
024300140108     C                   KFLD                    ksoglia
024400140108     c*
024500140108     C     Kopa          KLIST
024600140108     C                   KFLD                    Do0tfp
024700140108     C                   KFLD                    do0tfa
024800060331     c
024900131011     c* Se la versione non è stata passata uso la data di riferimento per calcolarla
025000130830    1c                   if        do0ver=0
025100130830     c
025200130830    2c                   if        do0dri=0
025300130830     c                   movel     datasys       do0dri
025400130830    2c                   endif
025500130830     c
025600130830     C                   Z-ADD     Do0DRI        P96DRI            8 0
025700130830     C                   Z-ADD     *ZEROS        P96VER            5 0
025800130830     C                   CALL      'TISI96R'
025900130830     C                   PARM                    P96DRI
026000130830     C                   PARM                    P96VER
026100130830     c
026200130830     c                   eval      do0ver=p96ver
026300130830    1c                   endif
026400131011
026500131011     c* Se non passato tipo servizio --> errore
026600131011     c                   if        do0tser<>'R' and do0tser<>'C'
026700131011     c                   eval      do0msg=msg(02)
026800131011     c                   eval      do0err='E'
026900131011     c                   leavesr
027000131011     c                   endif
027100140108     c*
027200140108     c* se il campo soglia non è P=POM o T=TUTTO il giorno errore e lo abblanco
027300140108     c                   if        do0soglia<>*blanks and do0soglia<>'P ' and
027400140108     c                             do0soglia<>'T '
027500140108     c                   eval      do0msg=msg(06)
027600140108     c                   eval      do0err='E'
027700140108     c                   leavesr
027800140108     c                   endif
027900130830     c
028000130830     c                   if        do0lna>0  and do0lna<>999
028100130830     c
028200140514     c***  do0lna        chain     azorg01l
028300130830     c
028400140514     c***                if        not %found(azorg01l)
028500140514     c***                          or orgfag<>'F' and orgfag<>'A'
028600140514     c***                eval      do0msg=msg(01)
028700140514     c***                eval      do0err='E'
028800140514     c***                leavesr
028900140514     c***                endif
029000130924     c
029100130924     c* Se la linea passata non è partita con la nuova procedura emetto errore
029200140414     c***                movel     do0lna        w003a             3
029300140414     c***                eval      Indx=%lookup(w003a:fil_ab)
029400140414     c***
029500140414     c***                if        indx=0 and fil_ab(1)<>'999' and do0tric<>'S'
029600140414     c***                eval      do0msg=msg(05)
029700140414     c***                eval      do0err='A'
029800140414     c***                endif
029900130924     c
030000130924     c                   endif
030100130830     c
030200130830     c* se non ho passato la linea di arrivo imposto 999 se cap e loc=*blanks
030300130830     c*  altrimenti errore
030400130830     c                   if        do0lna=0 and do0cap=*blanks and do0loc=*blanks
030500130830     c                   eval      do0lna=999
030600130830     c                   endif
030700130830     c* Se non passata linea di arrivo --> errore
030800130830     c                   if        do0lna=0
030900130830     c                   eval      do0msg=msg(01)
031000131011     c                   eval      do0err='E'
031100130830     c                   leavesr
031200130830     c                   endif
031300130830     c
031400131011     c* Se passata località e non CAP --> errore
031500130830     c                   if        do0loc<>*blanks
031600130830     c                             and do0cap=*blanks
031700130830     c                   eval      do0msg=msg(04)
031800131011     c                   eval      do0err='E'
031900130830     c                   endif
032000130830     c
032100130830     c* Ammessa soltanto nazione ITALIA
032200140514     c                   if        do0nar<>*blanks
032300140514     c                   open      tabel00f
032400130830     C                   MOVEL     1             KTBKUT
032500130830     C                   MOVEL     '15'          KTBCOD
032600130830     C                   MOVEL(P)  do0nar        KTBKEY
032700130830     C     KEYTAB        CHAIN     TABEL00F                           99
032800130830     c                   clear                   ds15
032900130830IF  1C     *IN99         IFEQ      *OFF
033000130830     C     TBLFLG        ANDEQ     *BLANKS
033100130830     C                   MOVEL     TBLUNI        DS15
033200130830     c                   endif
033300130830     c                   if        §15ita<>'I'
033400130830     c                   eval      do0msg=msg(03)
033500131011     c                   eval      do0err='A'
033600130830     c                   endif
033700140514     c                   endif
033800130830     c
033900130830     C                   ENDSR
034000130830     C**************************************************************************
034100130830     c* Normalizzo la località
034200130830     C**************************************************************************
034300130830     C     Norm_loc      BEGSR
034400130830     c                   clear                   tisi95ds
034500140516     C                   MOVEL     '3'           I95TCN
034600130830     c                   eval      i95loc=do0loc
034700130830     c                   eval      i95cap=do0cap
034800130830     c                   eval      i95nar=do0nar
034900130830     c                   eval      i95dat=do0dri
035000140516     C                   call      'TISI95R'
035100140516     C                   parm                    tisi95ds
035200140516     C*
035300130830     c
035400130830     C                   IF        O95ERR= ' ' AND o95loc<>*blanks
035500130830     c                   eval      kloc=o95loc
035600130830     c                   else
035700130830     c                   eval      kloc=do0loc
035800130830     c                   endif
035900140514     c
036000140514     c* Passo in output la località normalizzata
036100140514     c                   if        %parms=3
036200140514     c                   eval      osio9loc  =kloc
036300140514     c* Se normalizzata memorizzo
036400140514     C                   if        o95lid = '3' or o95lid = '4'
036500140514     c                   eval      osio9NORM='S'
036600140514     c                   endif
036700140514     c                   endif
036800130830     c
036900130830     C                   ENDSR
037000140108     C**************************************************************************
037100140108     c* Cerco il record giust per la soglia se la filiale lo prevede
037200140108     C**************************************************************************
037300140414     c     CerSoglia     BEGSR
037400140514     c
037500140108     c* 1) Se bolla in distinta e data arrivo trazione < data distinta --> tempi "  "
037600140526    1c                   if        do0ddc>0 and do0dti>0 and do0dti<do0ddc
037700140108     c                   eval      ksoglia='  '
037800140108     c                   leavesr
037900140108    1c                   endif
038000140526
038100140526     c* 1a) Se bolla NON in distinta e data arrivo trazione < data del giorno--> tempi "  "
038200140526     c                   movel     datasys       wdrf              8 0
038300140526    1c                   if        do0ddc=0 and do0dti>0 and do0dti<wdrf
038400140526     c                   eval      ksoglia='  '
038500140526     c                   leavesr
038600140526    1c                   endif
038700140108
038800140108     c* 2) Se consegna richiesta DOPO il o IL > consegna prevista --> tempi "  "
038900140109     c                   if        do0dcr>0  and do0tcr<>'P'  and do0dei>0
039000140109     c                   if        do0tcr=' ' and do0dcr>do0dei
039100140109     c                   eval      ksoglia='  '
039200140109     c                   leavesr
039300140108     c                   endif
039400140109     c                   if        do0tcr='D' and do0dcr>=do0dei
039500140109     c                   eval      ksoglia='  '
039600140109     c                   leavesr
039700140109     c                   endif
039800140109     c                   endif
039900140108     c*
040000140108     c* Se orario arrivo trazione non passato lo cerco dal file degli arrivi standard
040100140428     c* Orario = 9999 serve quando si ha la data di arrivo ma non l'ora. Ad esempio
040200140428     c*  per le bolle di sede TITAS
040300140428    1c                   if        (do0dti=0 and do0hti=0) or do0hti=9999
040400140514     c                   open      waopa01l
040500140110     c     kopa          chain     waopa01l
040600140110    2c                   if        %found(waopa01l)
040700140108     c                   eval      do0hti=opahha
040800140108    2c                   endif
040900140108    1c                   endif
041000140108     c
041100140108     c* Cerco la soglia della filiale
041200140514     c                   if        not %open(tabel00f)
041300140514     c                   open      tabel00f
041400140514     c                   endif
041500140514     c
041600140108     c                   clear                   ds1x
041700140108     C                   MOVEL     1             KTBKUT
041800140108     C                   MOVEL     '1X'          KTBCOD
041900140211     c*****              eval      ktbkey=%editc(do0lna:'X')+'OSA'
042000140211     c                   eval      ktbkey=%editc(d55tfa:'X')+'OSA'
042100140108     C     KEYTAB        CHAIN     TABEL00F
042200140108    1c                   if        not %found(tabel00f)
042300140108     c                   eval      ktbkey='   OSA'
042400140108     C     KEYTAB        CHAIN     TABEL00F
042500140108    1c                   endif
042600140108     c
042700140108    1c                   if        %found(tabel00f)
042800140108     c                   eval      ds1x=tbluni
042900140108    1c                   endif
043000140108
043100140108     c* 3) Se l'orario è <= della prima soglia --> tempi "  "
043200140108    1c                   if        do0hti<=§1xhho
043300140108     c                   eval      ksoglia='  '
043400140108     c                   leavesr
043500140108   x1c                   else
043600140108     c
043700140108     c* Se l'orario è > della prima soglia verifico che sia entro la seconda
043800140108     c                   eval      ktbkey=%trimr(ktbkey)+'1'
043900140108     C     KEYTAB        CHAIN     TABEL00F
044000140108    2c                   if        %found(tabel00f)
044100140108     c                   eval      ds1x=tbluni
044200140108    2c                   endif
044300140108
044400140108     c* 4) se NON E' entro seconda seglia --A tempi "  "
044500140108    2c                   if        do0hti>§1xhho
044600140108     c                   eval      ksoglia='  '
044700140108     c                   leavesr
044800140108    2c                   endif
044900140108    1c                   endif
045000140108     c
045100140108     c* 5) ORARIO TRA LE DUE SOGLIE  --> tempi "P "
045200140108     c
045300140108     c                   eval      ksoglia='P '
045400140108     c                   leavesr
045500140108     c
045600140108     C                   ENDSR
045700130924
045800020805      **************************************************************************
045900960302** SCHIERA MSG - MESSAGGI DI ERRORE
046000130830Linea passata errata                                                           01
046100130920Tipo servizio passato errato: richiedere R-ritiro o C-consegna                 02
046200130830Orari servizi presenti soltanto per nazione ITALIA                             03
046300130830Insieme alla località passare anche il CAP                                     04
046400130920Non sono presenti gli orari servizi                                            05
046500140108Soglia passara errata: richiedere P=pomeriggio T=tuttoILgiorno o nulla         06

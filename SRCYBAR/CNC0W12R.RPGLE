000100060307     H DFTACTGRP(*NO) ACTGRP('QILE') BNDDIR('PJXBND':'TRULCIF')
000200060302     H DECEDIT('0,') DATEDIT(*DMY.)
000300060224      * -------------------------------------------------------------*
000400060512      * Rettifiche x errato incasso dopo chius.operat. fatto da sede *
000500960702     F*--------------------------------------------------------------*
000600060224     FFNARB01L  UF   E           K DISK    commit
000700060224     Fcnass01L  uF a E           K DISK    commit
000800120126     Ftncsm03L  uF a E           K DISK    commit
000900120126     Fcnass02L  uF a E           K DISK    commit rename(cnass000:cnass002)
001000060412     Ffiar901L  uF a E           K DISK    commit
001100120229     Ffiar501l  iF   E           K DISK
001200060414     Ffiari01L  uF   E           K DISK    commit
001300060505     Ffncsb01L  uF   E           K DISK    commit
001400060505     Fyfqri00f  o    E           K DISK    commit
001500060224     FFnlbl01L  iF   E           K DISK
001600120126     FFnlbl02L  iF   E           K DISK           rename(fnlbl000:fnlbl002)
001700060516     Ftabel00f  iF   E           K DISK
001800030730      *---------------------------------------------------------------
001900030730     d* ds esterne
002000030730      *---------------------------------------------------------------
002100030730     D KPJBA         E DS
002200060301     D fnarbdssav    e ds                  extname(fnarb00F) prefix(s)
002300060301     D fnarbds       e ds                  extname(fnarb00F)
002400060301     D fnarbmds      e ds                  extname(fnarb00F) prefix(m)
002500060301     D azuteds       e ds                  extname(AZUTE00F)
002600060309     D fiar6ds       e ds                  extname(fiar600F)
002700060309     D fiar9ds       e ds                  extname(fiar900F)
002800060505     D yfqrids       e ds                  extname(yfqri00F)
002900060516     D ds1a          e ds                  inz
003000120229     D dar5gen       e ds                  inz
003100060505     D tabelds       e ds                  extname(tabel00F)
003200060307     D chkBollaIncassata...
003300060307     D                 PR            10I 0
003400060307     D  aas                                LIKE(arbaas)
003500060307     D  lnp                                LIKE(arblnp)
003600060307     D  nrs                                LIKE(arbnrs)
003700060307     D  nsp                                LIKE(arbnsp)
003800060307     D  cbo                                LIKE(arbcbo)
003900060307     D                                     OPTIONS(*OMIT)
004000060307     D  datiCbo                      89A
004100060310     D                                     OPTIONS(*OMIT)
004200060519     D  tpd                           1A
004300060307     D  esito                        10I 0
004400060307     D  err                           1A   DIM(10)
004500060307     D  msg                          80A   DIM(10)
004600060307     D                                     OPTIONS(*OMIT)
004700060310     D  datiFattura               32767A
004800060307     D                                     OPTIONS(*OMIT:*VARSIZE)
004900060307     D                                     VARYING
005000060310     D  datiContrassegno...
005100060307     D                            32767A
005200060307     D                                     OPTIONS(*OMIT:*VARSIZE)
005300060307     D                                     VARYING
005400060307     D prmEsito        S             10I 0
005500060307     D esito           S             10I 0
005600060307     D err             S              1    DIM(10)                              SKI CHIAVI
005700060310     D msg             S             80    DIM(10)                              SKI CHIAVI
005800060310     Ddatifat          s          32767A
005900060310     D                                     VARYING
006000060310     Ddatica           s          32767A
006100060310     D                                     VARYING
006200060505     d param           ds
006300111111     D pkaas                          4s 0                                      SKI TAB. 3
006400111111     D pklnp                          3s 0                                      SKI TAB. 3
006500111111     D pknrs                          2s 0                                      SKI TAB. 3
006600111111     D pknsp                          7s 0                                      SKI TAB. 3
006700060519     D parca                          1                                         SKI TAB. 3
006800060519     D paresito                       1                                         SKI TAB. 3
006900060511     D sifda                         10
007000060511     D sifa                          10
007100060505     D WLBDAT          DS
007200060505     D  G02DAT                 1      8  0
007300060505     D  G02INV                 9     16  0
007400060505     D  G02ERR                17     17
007500060505     D  G02TGI                18     22  0
007600111111     D kaas            s                   LIKE(pkaas)
007700111111     D klnp            s                   LIKE(pklnp)
007800111111     D knrs            s                   LIKE(pknrs)
007900111111     D knsp            s                   LIKE(pknsp)
008000911016      *---------------------------------------------------------------*
008100911016      * FLUSSO PRINCIPALE                                             *
008200911016      *---------------------------------------------------------------*
008300030730     c* controlli
008400060510     c                   clear                   paresito
008500111111     C                   Z-ADD     pkaas         KAAS
008600111111     C                   Z-ADD     pklnp         KLNP
008700111111     C                   Z-ADD     pknrs         KNRS
008800111111     C                   Z-ADD     pknsp         KNSP
008900030730     c                   exsr      srctr
009000060511     c                   if        *in90
009100060511     c                   eval      paresito = '1'
009200060511     c                   else
009300030730     c*aggiorna
009400030730     c                   exsr      sraggio
009500060511     c                   end
009600060511     c                   eval      kpjbu = param
009700941107     C*
009800030730     C                   SETON                                        LR
009900030730     c**********************************************************************
010000030730     C     srctr         BEGSR
010100030730     c**********************************************************************
010200030620     C                   SETOFF                                       90
010300060227     C     Kspe          CHAIN(n)  FNARB01L
010400060307     c                   if        not %found(fnarb01l)
010500060504     c                   seton                                        90
010600060224     c                   else
010700060301     c                   movel     fnarbds       fnarbdssav
010800060412     c                   clear                   fiar6ds
010900060412     c                   clear                   fiar9ds
011000060412     c                   clear                   datifat
011100060412     c                   clear                   datica
011200060505     C*
011300060228     c                   do
011400060302     C* Controllo se esiste bolla mamma
011500060310     c                   clear                   fnarbmds
011600060228     C     KSPE          CHAIN     FNLBL01L
011700060228     c* per cambio di porto il contrassegno è sulla mamma
011800060228     C                   IF        %found(fnlbl01l) and lbllan = lbllap
011900111111     C                   Z-ADD     LBLAAP        KAAS
012000111111     C                   Z-ADD     LBLLPP        KLNP
012100111111     C                   Z-ADD     LBLNRP        KNRS
012200111111     C                   Z-ADD     LBLNSP        KNSP
012300060301     C     KSPE          CHAIN(n)  FNARB01L
012400060224     C* Memorizzo il legame esistente per i successivi accessi su FNARB
012500060228     C                   IF        %found(fnarb01l) and arbcca <> '2' and
012600060228     C                             arbcca <> '6'
012700060301     C                   movel     fnarbds       fnarbmds
012800060224     C                   END
012900060301     C                   movel     fnarbdssav    fnarbds
013000060224     C                   END
013100060505     c                   clear                   tbluni
013200060309     c* controllo bolla
013300060307     c                   callp     CHKBOLLAincassata(
013400060307     c                             arbaas:arblnp:arbnrs:arbnsp
013500060519     c                             :arbcbo:tbluni:parca:esito:err:msg
013600060310     c                             :datifat:datica)
013700060310     c* se errore oppure contrassegno sulla mamma
013800060321     c                   if        esito <>0 and marbaas <> 0
013900060309     c                   clear                   esito
014000060309     c                   callp     CHKBOLLAincassata(
014100060309     c                             marbaas:marblnp:marbnrs:marbnsp
014200060519     c                             :marbcbo:tbluni:parca:esito:err:msg
014300060310     c                             :*omit:datica)
014400060309     c                   end
014500060310     C*
0146000603101    c                   if        esito = 0
014700060310     c                   movel     datifat       fiar6ds
014800060310     c                   movel     datica        fiar9ds
014900060505     c* contrassegno non incassati
015000060505     c                   if        (arbicc = ' ' or arbicc = 'R')
015100060530     c                             and parca='C'
015200060504     c                   seton                                        90
015300060309     c                   leave
015400060309     C                   END
015500120126     c* assegnato non incassati
015600060519     c                   if        (arbica = ' ' or arbica = 'R')
015700060530     c                             and parca='A'
015800060519     c                   seton                                        90
015900060519     c                   leave
016000060519     C                   END
016100060310x1   c                   else
016200060310     c                   seton                                        90
0163000603101    c                   end
016400060412     c*
016500060412     C                   ENDdo
0166000605051    c                   end
016700060224     C*
016800060224     C                   ENDSR
016900060412     C*-----------------------------------------------------*
017000060412     C* sraggio - aggiorno FNARB, anomalie ed eventi        *
017100060412     C*-----------------------------------------------------*
017200060412     c     sraggio       begsr
017300060413     c* ERRATO INCASSO
017400111104     C     Kspe          CHAIN(n)  FNARB01L
017500060508     c                   if        %found(fnarb01l)
017600111104     c* verifico se presenti più assegni o più spedizioni per un assegno
017700060511     c                   z-add     arbipp        assfle
017800111104     c     kass1         setll     cnass01l
017900111104     c                   do        *hival
018000111104     c     kass1         reade     cnass01l
018100111104     c                   if        %eof(cnass01l)
018200111104     c                   leave
018300111104     c                   end
018400120126     c* spedizione con multiassegni
018500120126     c     ' '           scan      assnra                                 88
018600120127     c                   if        not *in88 and assabi = 0
018700111104     c                   exsr      srasssped
018800120126     c                   leave
018900111104     c                   end
019000120126     C                   delete    cnass000
019100111104     c                   enddo
019200111104     c                   end
019300111104     c* FNARB
019400111104     C     Kspe          CHAIN     FNARB01L
019500111104     c                   if        %found(fnarb01l)
019600111104     c                   if        arbica<>' ' and parca='A'
019700111104     c                   eval      arbica = 'R'
019800111104     c                   clear                   arbmif
019900111104     c                   end
0200001111043    c                   if        arbicc<>' ' and parca='C'
020100111104     c                   eval      arbicc='R'
0202001111043    c                   end
020300111104     C                   UPDATE    FNARB000
020400060414     c* FIARI
020500060414     c     kspe          setll     fiari01l
020600060414     c                   do        *hival
020700060414     c     kspe          reade     fiari01l
020800060414     c                   if        %eof(fiari01l)
020900060414     c                   leave
021000060414     c                   end
021100060519     c                   if        aritip<>'C' and parca='C'
021200060505     c                   iter
021300060505     c                   end
021400060519     c                   if        aritip<>'A' and parca='A'
021500060519     c                   iter
021600060519     c                   end
021700111109     c
021800060414     c                   if        ARIIPP = 0
021900060414     C                   delete    fiari000
022000060414     c                   else
022100060414     c                   clear                   ariimp
022200060414     C                   update    fiari000
022300060414     c                   end
022400060414     c                   enddo
023400060504     c* FIAR9
023500060414     c     kspe          chain     fiar901l
0236000604142    c                   if        %found(fiar901l)
023700060516     c                   exsr      srtic
023701120322     c                   exsr      srticbol
023702120322     c                   eval      ar9tic = savtici
023900060414     c                   update    fiar9000
0240000604142    c                   end
024100120126     c* nei campi chiave c'è impostato l'eventuale mamma o la spedizione
024200120126     c* senza legami. se c'è la mamma devo
024300120126     c* mettere il ritorno all'incasso anche sulla figlia
0244001201262    c                   if        marbaas <> 0
024500120126     c     karb          chain     fnarb01l
0246001201263    c                   if        arbicc <> ' ' and arbicc <> 'R'
024700120126     c                   eval      arbicc = 'R'
0248001201263    c                   end
024900120126     C                   UPDATE    FNARB000
0250001201261    c                   end
025100060504     c* fncsb
025200060505     c     kspe          chain     fncsb01l
0253000605052    c                   if        %found(fncsb01l)
025400060505     c                   delete    fncsb000
0255000605052    c                   end
025600060504     c* rettifiche
025700060518     c                   if        §1afmb <> 'M'
025800071119     c                   z-add     arbifp        QRIFLE
025900060505     c                   eval      QRICUC = 0
026000060505     c                   eval      QRICAU = 'ERI'
026100060505     c                   eval      QRIDTR = amgudt
026200060505     c                   eval      QRIORA = time
026300060505     C     §1AFCA        IFNE      'S'
026400060505     C     §1AFCA        andne     'P'
026500060505     c                   eval      QRIIASS = ar9cas * -1
026600060508     c                   clear                   qriicon
026700060505     c                   else
026800060516     c                   eval      QRIicon = ar9cas * -1
026900060508     c                   clear                   qriiass
027000060505     c                   end
027100060505     c                   write     yfqri000
027200060518     c                   end
027300060505     c*
027400030730     c                   commit
027500060508     c                   end
027600030730     c*
027700030730     c                   endsr
027800111104     C*-----------------------------------------------------*
027900111104     C* aggiorno le spedizioni come ritorno all'incasso c/ass
028000111104     C* o cancello i più assegni per la stessa spedizione
028100111104     C*-----------------------------------------------------*
028200111104     C     srasssped     BEGSR
028300120126     c* spedizione multiassegni
028400120126     c     kass2         setll     cnass02l
028500111104     c                   do        *hival
028600120126     c     kass2         reade     cnass02l
028700120126     c                   if        %eof(cnass02l)
028800111104     c                   leave
028900111104     c                   end
029000120126     c                   if        assabi <> 0
029100120126     c                   iter
029200120126     c                   end
029300120126     c                   delete    cnass002
029400120126     c* multiassegni
029500120126     c     assnra        setll     tncsm03l
029600120126     c                   do        *hival
029700120126     c     assnra        reade     tncsm03l
029800120126     c                   if        %eof(tncsm03l)
029900120126     c                   leave
030000120126     c                   end
030100120126     c                   delete    tncsm000
030200120126     c                   enddo
030300111104     c* FNARB
030400111104     C     Kspe2         CHAIN     FNARB01L
0305001111042    c                   if        %found(fnarb01l)
0306001201263    c                   if        arbicc <> ' ' and arbicc <> 'R'
030700120126     c                             and parca = 'C'
030800111104     c                   eval      arbicc = 'R'
0309001201263    c                   end
031000111104     C                   UPDATE    FNARB000
0311001111042    c                   end
031200111111     c* FNARB eventuale figlia
031300120126     C     Karb          CHAIN     FNLBL02L
031400120126     c* per cambio di porto il contrassegno è sulla mamma
031500120126     C                   IF        %found(fnlbl02l) and lbllan = lbllap
031600120126     C     Kspef2        CHAIN     FNARB01L
0317001111112    c                   if        %found(fnarb01l)
0318001111113    c                   if        arbicc <> ' '
031900111111     c                   eval      arbicc = 'R'
0320001111113    c                   end
032100111111     C                   UPDATE    FNARB000
0322001111112    c                   end
0323001111112    c                   end
033300111104     c* FIAR9
033400120126     c     kspe2         chain     fiar901l
0335001111042    c                   if        %found(fiar901l)
033600111104     c                   exsr      srtic
033601120322     c                   exsr      srticbol
033700120229     c                   eval      ar9tic = savtici
033800111104     c                   update    fiar9000
0339001111042    c                   end
034000111104     c* fncsb
034100111104     c     kspe2         chain     fncsb01l
0342001111042    c                   if        %found(fncsb01l)
034300111104     c                   delete    fncsb000
0344001111042    c                   end
034500111104     c* rettifiche
034600111104     c                   if        §1afmb <> 'M'
034700111104     c                   z-add     arbifp        QRIFLE
034800111104     c                   eval      QRICUC = 0
034900111104     c                   eval      QRICAU = 'ERI'
035000111104     c                   eval      QRIDTR = amgudt
035100111104     c                   eval      QRIORA = time
035200111104     C     §1AFCA        IFNE      'S'
035300111104     C     §1AFCA        andne     'P'
035400111104     c                   eval      QRIIASS = ar9cas * -1
035500111104     c                   clear                   qriicon
035600111104     c                   else
035700111104     c                   eval      QRIicon = ar9cas * -1
035800111104     c                   clear                   qriiass
035900111104     c                   end
036000111104     c                   write     yfqri000
036100111104     c                   end
036200111104     c                   enddo
036300111104     c                   endsr
036301120322     C*-----------------------------------------------------*
036302120322     c* reperisce il tipo incasso bollettato altrimenti pulisce ...
036303120322     C*-----------------------------------------------------*
036304120322     C     srticbol      BEGSR
036305120322     c                   clear                   savtici           2
036306120322     c                   if        ar9aas > 2009
036307120322     c     kar5          chain     fiar501l
036308120322     c                   if        %found(fiar501l)
036309120322     c                   movel     ar5uni        dar5gen
036310120322     c                   movel     §AR5TICI      savtici
036311120322     c                   end
036312120322     c                   end
036313120322     c                   endsr
036400060516     C*-----------------------------------------------------*
036500060516     C* srtic   - tipo incasso                              *
036600060516     C*-----------------------------------------------------*
036700060516     c     srtic         begsr
036800060516     C*  tipo incasso
036900060516     C                   MOVEL     '1A'          tblCOD
037000060516     C                   MOVEl(p)  ar9tic        tblkey
037100060516     c     ktab1         chain     tabel00f
037200060516     c                   if        %found(tabel00f)
037300060516     c                   movel     tbluni        ds1a
037400060516     c                   else
037500060516     C                   clear                   ds1a
037600060516     c                   end
037700060516     C*
037800060516     C                   ENDSR
037900940322     C*-----------------------------------------------------*
038000941107     C*    Operazioni iniziali
038100941107     C*-----------------------------------------------------*
038200940322     C     *INZSR        BEGSR
038300950907     C*
038400940322     C     *ENTRY        PLIST
038500940322     C                   PARM                    KPJBA
038600060505     c                   movel     kpjbu         param
038700060505     c*
038800060505     C                   TIME                    TIME              6 0
038900060505     C                   Z-ADD     UDATE         G02DAT
039000060505     C                   MOVE      *ZEROS        G02INV
039100060505     C                   MOVEL     *BLANK        G02ERR
039200060505     C                   CALL      'XSRDA8'
039300060505     C                   PARM                    WLBDAT
039400060505     C                   Z-ADD     G02INV        AMGUDT            8 0
039500020905     c*
039600940322      *---------------------------------------------------------------*
039700940322      * CHIAVI                                                        *
039800940322      *---------------------------------------------------------------*
039900060516     C     KTAB1         KLIST
040000060516     C                   KFLD                    tblKUT
040100060516     C                   KFLD                    tblCOD
040200060516     C                   KFLD                    tblkey
040300060516     C                   Z-ADD     1             tblKUT
040400060307     C     KASS1         KLIST
040500060511     C                   KFLD                    assfle
040600060505     C                   KFLD                    klnp
040700060505     C                   KFLD                    kaas
040800060505     C                   KFLD                    knrs
040900060505     C                   KFLD                    knsp
041000120126     C     KASS2         KLIST
041100120126     C                   KFLD                    assfle
041200120126     C                   KFLD                    assnra
041300120126     C     Karb          KLIST
041400120126     C                   KFLD                    arbAAS
041500120126     C                   KFLD                    arbLNP
041600120126     C                   KFLD                    arbNRS
041700120126     C                   KFLD                    arbNSP
041800120126     C     Kspe          KLIST
041900120126     C                   KFLD                    KAAS
042000120126     C                   KFLD                    KLNP
042100120126     C                   KFLD                    KNRS
042200120126     C                   KFLD                    KNSP
042300120126     C     Kspef2        KLIST
042400120126     C                   KFLD                    LBLAAn
042500120126     C                   KFLD                    LBLLPn
042600120126     C                   KFLD                    LBLNRn
042700120126     C                   KFLD                    LBLNSn
042800111104     C     Kspe2         KLIST
042900120126     C                   KFLD                    assAAS
043000120126     C                   KFLD                    assLNP
043100120126     C                   KFLD                    assNRS
043200120126     C                   KFLD                    assNSP
043900120322     C     Kar5          KLIST
044000120322     C                   KFLD                    ar9AAS
044100120322     C                   KFLD                    ar9LNP
044200120322     C                   KFLD                    ar9NRS
044300120322     C                   KFLD                    ar9NSP
044400120229     C                   KFLD                    ar5trd
044500120229     c                   eval      ar5trd = 'GEN'
044600940322     C*
044700940322     C                   ENDSR

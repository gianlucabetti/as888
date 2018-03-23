000100060307     H DFTACTGRP(*NO) ACTGRP('QILE') BNDDIR('PJXBND':'TRULCIF')
000200060302     H DECEDIT('0,') DATEDIT(*DMY.)
000300060224      * -------------------------------------------------------------*
000400060512      * Rettifiche x errato incasso dopo chius.operat. fatto da sede *
000500960702     F*--------------------------------------------------------------*
000700060224     FFNARB01L  UF   E           K DISK    commit
000900060224     Fcnass01L  uF a E           K DISK    commit
000901060412     Ffiar901L  uF a E           K DISK    commit
000902060414     Ffiari01L  uF   E           K DISK    commit
000903060505     Ffncsb01L  uF   E           K DISK    commit
000904060505     Fyfqri00f  o    E           K DISK    commit
001100060224     FFnlbl01L  iF   E           K DISK
001101060516     Ftabel00f  iF   E           K DISK
001500030730      *---------------------------------------------------------------
001600030730     d* ds esterne
001700030730      *---------------------------------------------------------------
001800030730     D KPJBA         E DS
002000060301     D fnarbdssav    e ds                  extname(fnarb00F) prefix(s)
002100060301     D fnarbds       e ds                  extname(fnarb00F)
002200060301     D fnarbmds      e ds                  extname(fnarb00F) prefix(m)
002300060301     D azuteds       e ds                  extname(AZUTE00F)
002400060309     D fiar6ds       e ds                  extname(fiar600F)
002500060309     D fiar9ds       e ds                  extname(fiar900F)
002501060505     D yfqrids       e ds                  extname(yfqri00F)
002502060516     D ds1a          e ds                  inz
002503060505     D tabelds       e ds                  extname(tabel00F)
004300060307     D chkBollaIncassata...
004400060307     D                 PR            10I 0
004500060307     D  aas                                LIKE(arbaas)
004600060307     D  lnp                                LIKE(arblnp)
004700060307     D  nrs                                LIKE(arbnrs)
004800060307     D  nsp                                LIKE(arbnsp)
004900060307     D  cbo                                LIKE(arbcbo)
005000060307     D                                     OPTIONS(*OMIT)
005100060307     D  datiCbo                      89A
005200060310     D                                     OPTIONS(*OMIT)
005300060519     D  tpd                           1A
005400060307     D  esito                        10I 0
005500060307     D  err                           1A   DIM(10)
005600060307     D  msg                          80A   DIM(10)
005700060307     D                                     OPTIONS(*OMIT)
005800060310     D  datiFattura               32767A
005900060307     D                                     OPTIONS(*OMIT:*VARSIZE)
006000060307     D                                     VARYING
006100060310     D  datiContrassegno...
006200060307     D                            32767A
006300060307     D                                     OPTIONS(*OMIT:*VARSIZE)
006400060307     D                                     VARYING
009200060307     D prmEsito        S             10I 0
009300060307     D esito           S             10I 0
009400060307     D err             S              1    DIM(10)                              SKI CHIAVI
009500060310     D msg             S             80    DIM(10)                              SKI CHIAVI
009900060307     D nra             S                   like(assnra)                         SKI TAB. 3
010600060310     Ddatifat          s          32767A
010700060310     D                                     VARYING
010800060310     Ddatica           s          32767A
010900060310     D                                     VARYING
010901060505     d param           ds
010902060510     D kaas                           4s 0                                      SKI TAB. 3
010903060510     D klnp                           3s 0                                      SKI TAB. 3
010904060510     D knrs                           2s 0                                      SKI TAB. 3
010905060510     D knsp                           7s 0                                      SKI TAB. 3
010906060519     D parca                          1                                         SKI TAB. 3
010907060519     D paresito                       1                                         SKI TAB. 3
010908060511     D sifda                         10
010909060511     D sifa                          10
010910060505     D WLBDAT          DS
010911060505     D  G02DAT                 1      8  0
010912060505     D  G02INV                 9     16  0
010913060505     D  G02ERR                17     17
010914060505     D  G02TGI                18     22  0
011000911016      *---------------------------------------------------------------*
011100911016      * FLUSSO PRINCIPALE                                             *
011200911016      *---------------------------------------------------------------*
012600030730     c* controlli
012601060510     c                   clear                   paresito
012700030730     c                   exsr      srctr
012701060511     c                   if        *in90
012801060511     c                   eval      paresito = '1'
012802060511     c                   else
013400030730     c*aggiorna
013600030730     c                   exsr      sraggio
013602060511     c                   end
013603060511     c                   eval      kpjbu = param
014200941107     C*
014300030730     C                   SETON                                        LR
017000030730     c**********************************************************************
017100030730     C     srctr         BEGSR
017200030730     c**********************************************************************
017300030620     C                   SETOFF                                       90
026300060227     C     Kspe          CHAIN(n)  FNARB01L
026400060307     c                   if        not %found(fnarb01l)
026500060504     c                   seton                                        90
026700060224     c                   else
026800060301     c                   movel     fnarbds       fnarbdssav
027977060412     c                   clear                   fiar6ds
027978060412     c                   clear                   fiar9ds
027979060412     c                   clear                   datifat
027980060412     c                   clear                   datica
027981060505     C*
028000060228     c                   do
030400060302     C* Controllo se esiste bolla mamma
030500060310     c                   clear                   fnarbmds
030600060228     C     KSPE          CHAIN     FNLBL01L
030700060228     c* per cambio di porto il contrassegno è sulla mamma
030800060228     C                   IF        %found(fnlbl01l) and lbllan = lbllap
030900060224     C                   Z-ADD     LBLAAP        KAAS
031000060224     C                   Z-ADD     LBLLPP        KLNP
031100060224     C                   Z-ADD     LBLNRP        KNRS
031200060224     C                   Z-ADD     LBLNSP        KNSP
031300060301     C     KSPE          CHAIN(n)  FNARB01L
031400060224     C* Memorizzo il legame esistente per i successivi accessi su FNARB
031500060228     C                   IF        %found(fnarb01l) and arbcca <> '2' and
031600060228     C                             arbcca <> '6'
031700060301     C                   movel     fnarbds       fnarbmds
031800060224     C                   END
031900060301     C                   movel     fnarbdssav    fnarbds
032000060224     C                   END
032001060505     c                   clear                   tbluni
033600060309     c* controllo bolla
033900060307     c                   callp     CHKBOLLAincassata(
034000060307     c                             arbaas:arblnp:arbnrs:arbnsp
034100060519     c                             :arbcbo:tbluni:parca:esito:err:msg
034200060310     c                             :datifat:datica)
034300060310     c* se errore oppure contrassegno sulla mamma
034400060321     c                   if        esito <>0 and marbaas <> 0
034500060309     c                   clear                   esito
034600060309     c                   callp     CHKBOLLAincassata(
034700060309     c                             marbaas:marblnp:marbnrs:marbnsp
034800060519     c                             :marbcbo:tbluni:parca:esito:err:msg
034900060310     c                             :*omit:datica)
035000060309     c                   end
035100060310     C*
0352000603101    c                   if        esito = 0
035300060310     c                   movel     datifat       fiar6ds
035400060310     c                   movel     datica        fiar9ds
037100060505     c* contrassegno non incassati
037300060505     c                   if        (arbicc = ' ' or arbicc = 'R')
037301060530     c                             and parca='C'
037400060504     c                   seton                                        90
037500060309     c                   leave
037600060309     C                   END
037601060519     c* contrassegno non incassati
037602060519     c                   if        (arbica = ' ' or arbica = 'R')
037603060530     c                             and parca='A'
037604060519     c                   seton                                        90
037605060519     c                   leave
037606060519     C                   END
039900060310x1   c                   else
040500060310     c                   seton                                        90
0408000603101    c                   end
041035060412     c*
041036060412     C                   ENDdo
0410370605051    c                   end
041100060224     C*
041200060224     C                   ENDSR
059933060412     C*-----------------------------------------------------*
059934060412     C* sraggio - aggiorno FNARB, anomalie ed eventi        *
059935060412     C*-----------------------------------------------------*
059936060412     c     sraggio       begsr
059938060413     c* ERRATO INCASSO
059939060508     C     Kspe          CHAIN     FNARB01L
059940060508     c                   if        %found(fnarb01l)
060900060530     c                   if        arbica<>' ' and parca='A'
061000060519     c                   eval      arbica = 'R'
061001060519     c                   clear                   arbmif
061100060301     c                   end
0612000605303    c                   if        arbicc<>' ' and parca='C'
061300060519     c                   eval      arbicc='R'
0614000604123    c                   end
061500030730     C                   UPDATE    FNARB000
061501060505     c*
061502060511     c                   z-add     arbipp        assfle
062001060413     c     kass1         chain     cnass01l
062002060413     c                   if        %found(cnass01l)
062004060413     C                   delete    cnass000
062005060413     c                   end
062006060414     c* FIARI
062007060414     c     kspe          setll     fiari01l
062008060414     c                   do        *hival
062009060414     c     kspe          reade     fiari01l
062010060414     c                   if        %eof(fiari01l)
062011060414     c                   leave
062012060414     c                   end
062013060519     c                   if        aritip<>'C' and parca='C'
062014060505     c                   iter
062015060505     c                   end
062016060519     c                   if        aritip<>'A' and parca='A'
062017060519     c                   iter
062018060519     c                   end
062019060414     c                   if        ARIIMP <> 0
062020060414     c                   if        ARIIPP = 0
062021060414     C                   delete    fiari000
062022060414     c                   else
062023060414     c                   clear                   ariimp
062024060414     C                   update    fiari000
062025060414     c                   end
062026060414     c                   end
062027060414     c                   enddo
062028060504     c* FIAR9
062029060414     c     kspe          chain     fiar901l
0620300604142    c                   if        not %found(fiar901l) and marbaas <> 0
062031060414     C                   Z-ADD     marbaas       KAAS
062032060414     C                   Z-ADD     marblnp       KLNP
062033060414     C                   Z-ADD     marbnrs       KNRS
062034060414     C                   Z-ADD     marbnsp       KNSP
062035060414     c     kspe          chain     fiar901l
0620360604142    c                   end
0620370604142    c                   if        %found(fiar901l)
062038060516     c                   exsr      srtic
062041060414     c                   clear                   ar9tic
062043060414     c                   update    fiar9000
0620440604142    c                   end
062046060504     c* fncsb
062047060505     c     kspe          chain     fncsb01l
0620490605052    c                   if        %found(fncsb01l)
062050060505     c                   delete    fncsb000
0620650605052    c                   end
062066060504     c* rettifiche
062067060518     c                   if        §1afmb <> 'M'
062068071119     c                   z-add     arbifp        QRIFLE
062069060505     c                   eval      QRICUC = 0
062070060505     c                   eval      QRICAU = 'ERI'
062071060505     c                   eval      QRIDTR = amgudt
062072060505     c                   eval      QRIORA = time
062073060505     C     §1AFCA        IFNE      'S'
062074060505     C     §1AFCA        andne     'P'
062075060505     c                   eval      QRIIASS = ar9cas * -1
062076060508     c                   clear                   qriicon
062077060505     c                   else
062078060516     c                   eval      QRIicon = ar9cas * -1
062079060508     c                   clear                   qriiass
062080060505     c                   end
062081060505     c                   write     yfqri000
062082060518     c                   end
062083060505     c*
066800030730     c                   commit
066801060508     c                   end
066900030730     c*
067000030730     c                   endsr
067001060516     C*-----------------------------------------------------*
067002060516     C* srtic   - tipo incasso                              *
067003060516     C*-----------------------------------------------------*
067004060516     c     srtic         begsr
067006060516     C*  tipo incasso
067008060516     C                   MOVEL     '1A'          tblCOD
067009060516     C                   MOVEl(p)  ar9tic        tblkey
067010060516     c     ktab1         chain     tabel00f
067011060516     c                   if        %found(tabel00f)
067012060516     c                   movel     tbluni        ds1a
067013060516     c                   else
067014060516     C                   clear                   ds1a
067015060516     c                   end
067016060516     C*
067017060516     C                   ENDSR
067100940322     C*-----------------------------------------------------*
067200941107     C*    Operazioni iniziali
067300941107     C*-----------------------------------------------------*
067400940322     C     *INZSR        BEGSR
067500950907     C*
067600940322     C     *ENTRY        PLIST
067700940322     C                   PARM                    KPJBA
067701060505     c                   movel     kpjbu         param
067702060505     c*
067703060505     C                   TIME                    TIME              6 0
067704060505     C                   Z-ADD     UDATE         G02DAT
067705060505     C                   MOVE      *ZEROS        G02INV
067706060505     C                   MOVEL     *BLANK        G02ERR
067707060505     C                   CALL      'XSRDA8'
067708060505     C                   PARM                    WLBDAT
067709060505     C                   Z-ADD     G02INV        AMGUDT            8 0
067800020905     c*
069500940322      *---------------------------------------------------------------*
069600940322      * CHIAVI                                                        *
069700940322      *---------------------------------------------------------------*
069701060516     C     KTAB1         KLIST
069702060516     C                   KFLD                    tblKUT
069703060516     C                   KFLD                    tblCOD
069704060516     C                   KFLD                    tblkey
069705060516     C                   Z-ADD     1             tblKUT
070600060307     C     KASS1         KLIST
070700060511     C                   KFLD                    assfle
070800060505     C                   KFLD                    klnp
070900060505     C                   KFLD                    kaas
071000060505     C                   KFLD                    knrs
071100060505     C                   KFLD                    knsp
071700060301     C     Kspe          KLIST
071800060301     C                   KFLD                    KAAS
071900060301     C                   KFLD                    KLNP
072000060301     C                   KFLD                    KNRS
072100060301     C                   KFLD                    KNSP
074700940322     C*
074800940322     C                   ENDSR

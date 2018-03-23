000100910926     H DECEDIT('0,') DATEDIT(*DMY.)
000200131203     H* FNLVA1R *----------------------------------------------------*
000300131204     H*  Verifica Variazioni reali per AUT
000301131204     H*
000302131204     H*  Dati una spedizione, data/ora variazione, causale e profilo
000303131204     H*  utente variazione,
000304131204     H*  il pgm controlla, sulla base dell'immagine precedente e della
000305131204     H*  situazione attuale, se sono variati i dati che interessano
000306131204     H*  l'autotrasportatore e restituisce l'esito al chiamante
000400000000     H*--------------------------------------------------------------*
000401131203     Ffnarb01l  iF   E           K DISK
000402131204     Ffiar601l  iF   E           K DISK
000403131204     Ffiar901l  iF   E           K DISK
000404131204     Ffiar501l  iF   E           K DISK
000405131204     Ffiar401l  iF   E           K DISK
000406131204     Ffiarbf2c  iF   E           K DISK    infds(infdsarbf)
004005121218
004006131204     d ktrc            s                   like(ar6trc)
004007131204     d ktrc4           s                   like(ar4trc)
004008131204     d ktrd            s                   like(ar5trd) inz('GEN')
004009131204
004010131204     d infdsarbf       ds
004011131204     d   recname             261    270
007000920113     D KPJBA         E DS
007001131203     d fnlva1ds      e ds
007800021014     D ddatiute      e ds
007900021014     D azuteds       e ds                  extname(AZUTE00F)
008000020604     D tibs34ds      E DS
008001131204     D darbn_ref     E DS
008002131204     D dar5gen       E DS
008003131204
008004131204     D arbds§        E DS                  EXTNAME(fnarb00f) qualified
008005131204     D arbds         E DS                  EXTNAME(fnarb00f)
008006131203     D datiArb         DS
008007131204     d   A_arbrsd                          like(arbrsd)
008008131204     d   A_arbrd2                          like(arbrd2)
008009131204     d   A_ARBIND                          like(arbind)
008010131204     d   A_ARBCAD                          like(arbcad)
008011131204     d   A_ARBLOD                          like(arblod)
008012131204     d   A_ARBPRD                          like(arbprd)
008013131204     d   A_ARBNZD                          like(arbnzd)
008014131204     d   A_ARBFIN                          like(arbfin)
008015131204     d   A_ARBTIC                          like(arbtic)
008016131204     d   A_ARBCAS                          like(arbcas)
008017131204     d   A_ARBVCA                          like(arbvca)
008018131204     d   A_ARBDIV                          like(arbdiv)
008019131204     d   A_ARBIFT                          like(arbift)
008020131204     d   A_§ARBNREF                        like(§arbNREF)
008021131204     d   A_§ARBNTELD                       like(§arbNTELD)
008022131203     D datiVar         DS
008023131204     d   V_arbrsd                          like(arbrsd)
008024131204     d   V_arbrd2                          like(arbrd2)
008025131204     d   V_ARBIND                          like(arbind)
008026131204     d   V_ARBCAD                          like(arbcad)
008027131204     d   V_ARBLOD                          like(arblod)
008028131204     d   V_ARBPRD                          like(arbprd)
008029131204     d   V_ARBNZD                          like(arbnzd)
008030131204     d   V_ARBFIN                          like(arbfin)
008032131204     d   V_ARBTIC                          like(arbtic)
008033131204     d   V_ARBCAS                          like(arbcas)
008034131204     d   V_ARBVCA                          like(arbvca)
008035131204     d   V_ARBDIV                          like(arbdiv)
008036131204     d   V_ARBIFT                          like(arbift)
008037131204     d   V_§ARBNREF                        like(§arbNREF)
008038131204     d   V_§ARBNTELD                       like(§arbNTELD)
011900941130     I*
013500920113     I/SPACE 3
013600920113     C****************************************************************
013700920113     C*  RIEPILOGO INDICATORI
013800920113     C***************************************************************
013900131203     C* XX    -
016400920113     C*****************************************************************
016500920113     C*
016501121219     C     *ENTRY        PLIST
016502121219     C                   PARM                    KPJBA
016506121219
016507131203     c                   movel     kpjbu         fnlva1ds
016508131204
016509131204     c                   clear                   OA1ESITO
016510130204
016511130114
016512131203    0c                   if        ia1tla<>'C'
016513130222
016514121220     c* Impostazione
016515131203     c                   exsr      elabora
016516131204     c                   if        datiarb<>dativar
016517131204     c                   eval      OA1ESITO='V'
016518131204     c                   endif
016519131205     c                   eval      kpjbu=fnlva1ds
016554130117    0c                   endif
016555130114      *  Chiusura PGM
016556131203     c                   if        ia1tla =' '
016557130114     c                   return
016558130114     c                   else
016559130114     c                   exsr      chiusura
035600920113     C                   SETON                                        LR
035602130114     c                   endif
035603131203     C**---------------------------------------------------------
035604131203     C**
035605131203     C**---------------------------------------------------------
035606131203     C     elabora       BEGSR
035607131203     c
035608131204     c                   clear                   datiArb
035609131204     c                   clear                   datiVar
035610131204
035611131204     c     kbol          chain     fnarb01l
035612131203     c                   if        %found (fnarb01l)
035613131204     c* prima di leggere le variazioni mi salvo record di arb
035615131204     c                   eval      arbds§=arbds
035616131203     c* con la key ricevuta leggo lo storico variazioni
035617131203     c     kvar          setll     fiarbf2c
035618131203     c     kvar          reade     fiarbf2c
035619131203     c                   dow       not %eof(fiarbf2c)
035620131203     c* elaboro solo se c'è corrispondenza fra
035621131203     c* - causale variazione bolla
035622131203     c* - profilo utente della variazione
035623131203     c                   if        ia1pru=arbpru  and ia1cvb=arbcvb
035624131204     c                   exsr      memo_Dati
035626131203     c                   endif
035627131203     c     kvar          reade     fiarbf2c
035628131203     c                   enddo
035629131203
035630131203     c                   endif
035633131203     C*
035634131203
035635131203     c                   endsr
035636131204     C**---------------------------------------------------------
035637131204     C** Memorizzazione in DS dei dati sensibili Prima e Dopo la Variazione
035638131204     C**---------------------------------------------------------
035639131204     C     memo_Dati     BEGSR
035640131204     c
035641131204     c                   select
035642131204     c* DESTINATARIO
035643131204     c                   when      ia1cvb='I0'
035644131204     c                   select
035645131204     c* Ragione Sociale, Ind, Loc ....
035646131204     c                   when      recname='FNARBD00'
035647131204     c* immagine BEFORE presa dallo storico variazioni
035648131204     c                   eval      v_arbrsd=arbrsd
035649131204     c                   eval      V_arbrd2=arbrd2
035650131204     c                   eval      V_ARBIND=ARBIND
035651131204     c                   eval      V_ARBCAD=ARBCAD
035652131204     c                   eval      V_ARBLOD=ARBLOD
035653131204     c                   eval      V_ARBPRD=ARBPRD
035654131204     c                   eval      V_ARBNZD=ARBNZD
035655131204     c                   eval      V_ARBFIN=ARBFIN
035656131204     c* immagine AFTER  presa da Bolle Arrivi
035657131204     c                   eval      A_arbrsd=arbds§.arbrsd
035658131204     c                   eval      ktrc4='D'
035659131204     c     kar4          chain     fiar401l
035660131204     c                   if        %found(fiar401l)
035661131204     c                   eval      A_arbrd2=ar4not
035662131204     c                   endif
035663131204     c                   eval      A_ARBIND=arbds§.ARBIND
035664131204     c                   eval      A_ARBCAD=arbds§.ARBCAD
035665131204     c                   eval      A_ARBLOD=arbds§.ARBLOD
035666131204     c                   eval      A_ARBPRD=arbds§.ARBPRD
035667131204     c                   eval      A_ARBNZD=arbds§.ARBNZD
035668131204     c                   eval      A_ARBFIN=arbds§.ARBFIN
035669131204     c* Referente/Telefono
035670131204     c                   when      recname='FNARBN00'
035671131204     c* immagine BEFORE presa dallo storico variazioni
035672131204     c                   if        arbtrd='REF'
035673131205     c                   exsr      mem_REF
035684131204     c                   endif
035685131204     c                   endsl
035686131205     c* NOTE - Referente e telefono
035687131205     c                   when      %subst(ia1cvb:1:1)='N'
035688131205     c                   if        arbtrd='REF'
035689131205     c                   exsr      mem_REF
035690131205     c                   endif
035691131204     c
035692131205     c* CONTRASSEGNO
035693131204     c                   when      %subst(ia1cvb:1:1)='K'
035694131204     c* immagine BEFORE presa dallo storico variazioni
035695131204     c                   eval      V_ARBTIC = ARBTIC
035696131204     c                   eval      V_ARBCAS = ARBCAS
035697131204     c                   eval      V_ARBVCA = ARBVCA
035698131204     c* immagine AFTER  presa da Bolle Arrivi  (fiar9)
035699131204     c     kbol          chain     fiar901l
035700131204     c                   if        %found(fiar901l)
035701131204     c                   eval      A_ARBTIC = AR9TIC
035702131204     c                   eval      A_ARBCAS = AR9CAS
035703131204     c                   eval      A_ARBVCA = AR9VCA
035704131204     c                   endif
035705131205     c* TASSAZIONE
035706131204     c                   when      %subst(ia1cvb:1:1)='T'
035707131204     c                             or %subst(ia1cvb:1:1)='2'
035708131204     c* immagine BEFORE presa dallo storico variazioni
035709131204     c                   eval      V_ARBDIV=ARBDIV
035710131204     c                   eval      V_ARBIFT=ARBIFT
035711131204     c* immagine AFTER  presa da Bolle Arrivi  (fiar6)
035712131204     c                   if        %subst(ia1cvb:1:1)='T'
035713131204     c                   eval      ktrc='1'
035714131204     c                   else
035715131204     c                   eval      ktrc='2'
035716131204     c                   endif
035717131204     c     kar6          chain     fiar601l
035718131204     c                   if        %found(fiAR601L)
035719131204     c                   eval      A_ARBDIV=AR6DIV
035720131204     c                   eval      A_ARBIFT=AR6IFT
035721131204     C                   ENDIF
035722131204     c                   endsl
035723131204
035724131204     c                   endsr
035725131205     C**---------------------------------------------------------
035726131205     C** Memorizza rerefente consegna e numero di telefono
035727131205     C**---------------------------------------------------------
035728131205     C     mem_REF       BEGSR
035729131205     c                   eval      darbn_ref=arbuni70
035730131205     c                   eval      V_§ARBNREF =§ARBNREF
035731131205     c                   eval      V_§ARBNTELD=§ARBNTELD
035732131205     c* immagine AFTER  presa da Bolle Arrivi  (fiar5 record  "GEN")
035733131205     c     kar5          chain     fiar501l
035734131205     c                   if        %found(fiar501l)
035735131205     c                   eval      dar5gen=ar5uni
035736131205     c                   eval      A_§ARBNREF =§AR5REF
035737131205     c                   eval      A_§ARBNTELD=§AR5TELD
035738131205     c                   endif
035739131205     c                   endsr
035740130114     C**---------------------------------------------------------
035741130114     C**
035742130114     C**---------------------------------------------------------
035743130114     C     chiusura      BEGSR
035744130114     C*
035745130114
035746130114     c                   endsr
347700941130     C**---------------------------------------------------------
347800941130     C* ROUTINE INIZIALE
347900941130     C**---------------------------------------------------------
348000941130     C     *INZSR        BEGSR
348100021014
348400021014
348500941130     C                   Z-ADD     1             CODUT             1 0
348600021014
348700021014     c     *dtaara       define    §azute        azuteds
348800021014     c     *dtaara       define    §datiute      ddatiute
348900021014     C                   in(E)     *dtaara
349000021014     C                   IF        %Error  or  RSUT = *blanks
349100021014     C                   CLEAR                   TIBS34DS
349200021014     C                   call      'TIBS34R'
349300021014     C                   parm                    Tibs34Ds
349400021014     C                   in        *dtaara
349500021014     c                   ENDIF
349600021014
349700021014     C                   Z-ADD     DUTKCI        KCI               4 0
349800021014
349801131204     c     kbol          klist
349802131204     c                   kfld                    ia1aas
349803131204     c                   kfld                    ia1lnp
349804131204     c                   kfld                    ia1nrs
349805131204     c                   kfld                    ia1nsp
349806131204     c     kar6          klist
349807131204     c                   kfld                    ia1aas
349808131204     c                   kfld                    ia1lnp
349809131204     c                   kfld                    ia1nrs
349810131204     c                   kfld                    ia1nsp
349811131204     c                   kfld                    ktrc
349812131204     c     kar4          klist
349813131204     c                   kfld                    ia1aas
349814131204     c                   kfld                    ia1lnp
349815131204     c                   kfld                    ia1nrs
349816131204     c                   kfld                    ia1nsp
349817131204     c                   kfld                    ktrc4
349818131204     c     kar5          klist
349819131204     c                   kfld                    ia1aas
349820131204     c                   kfld                    ia1lnp
349821131204     c                   kfld                    ia1nrs
349822131204     c                   kfld                    ia1nsp
349823131204     c                   kfld                    ktrd
349905131203     c     kvar          klist
349906131203     c                   kfld                    ia1aas
349907131203     c                   kfld                    ia1lnp
349908131203     c                   kfld                    ia1nrs
349909131203     c                   kfld                    ia1nsp
349910131203     c                   kfld                    ia1dtv
349911131203     c                   kfld                    ia1orv
367200071102
367300941130     C                   ENDSR

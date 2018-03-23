000200000000     H DECEDIT('0,') DATEDIT(*DMY.)
000300011127     H*--------------------------------------------------------------*
000400051121     H*             - Anagrafico attività  -                         *
000500000000     H*--------------------------------------------------------------*
000501091030     fTNTBEx1L  if   e           k disk    rename(tntbe000: tntbe£)
000502091030     f                                     prefix(£)
000702060118     Ftntbe01l  iF   E           k disk
001001060118     Ftntud00f  if a E           k DISK
001002060118     Ftntud01l  if   E           k DISK    rename(tntud000:tntud001)
001003060116     Ftntud02l  if   E           k DISK    rename(tntud000:tntud002)
001004060116     Ftntud03l  if   E           k DISK    rename(tntud000:tntud003)
001005060116     Ftntus01l  uf a E           k DISK
001100030116
001600011127      *
001601060117     d Wdata8          DS
001602060117     d  dadata                 1      8  0
001603060117     d  adata                  9     16  0
001604060117     d  GioLav                17     21  0
001700020103     D Psds           SDS
001800020103     D  PgmName          *PROC
001801060117     D tibs56ds      E DS
001802060117     d  lin                    7    756  0 dim(250)
001803060120     D DSLV55        E DS                  EXTNAME(FNLV55DS)
001900060116     D tntu08DS      E DS
001901060118     D dtut          E DS
001902091030     D dmfm          E DS
001903060116     D tntuads       E DS                  extname(tntua00f)
001904060116     D tntutds       E DS                  extname(tntut00f)
001905060116     D KPJBA         E DS
004600011127     D DTAiso          S               D   INZ
004601060116     D DTiiso          S               D   INZ
004602060116     D DTfiso          S               D   INZ
004603060120     D comfil          S                   like(tusfil)
004604160916     D comfils         S                   like(tutfil)
004605160916     D comclis         S                   like(tutcli)
004606160916     D comtems         S                   like(tuttem)
004607060120     D fil             S                   like(tudfil)
004608060120     D dta             S                   like(tuddta)
004609160916     D sqlcods         S                   like(sqlcode)
004610160916     d  si             s              1
004611060111     d trul06ds      e ds
004612060111     d  lint                   1     90  0 dim(30)
004613060116     d  po             s              3  0 dim(500)
004614060116     d  cli            s              7  0 dim(2000)
004615060116     d  xx             s              4  0
004616060116     d  comdti         s             10
004617060116     d  comdtf         s             10
004618091102     d  com08          s              8  0
004619091102     d  savfil         s                   like(fil)
004620091102     d  contagg        s              2  0
004621060116     d  TEMPO          s                   LIKE(TUSTEM)
004622091030     c* gg mensili per moltiplicatori fissi
004623091030     D ptab            S               *
004624091030     D                                     INZ(%ADDR(D§mfm001))
004625091030     D mtab            S                   LIKE(D§mfm001)
004626091030     D                                     DIM(12)
004627091030     D                                     BASED(Ptab)
007200030116
007201060118     c* inserisco i moltiplicatori fissi nel tntud00f
007202060118     c                   exsr      srmultfis
007214060111     C/EXEC SQL
007215060111     C+ DECLARE a1 CURSOR FOR
007216060111     C+ SELECT tntua00f.*, tntut00f.* FROM tntua00f join tntut00f on tuaatt=
007217060111     C+ tutatt and tuaprg = tutprg WHERE tuaatb = ' ' and tutatb = ' '
007218060118     C+ and (tuadti between :comdti and :comdtf or tuadtf between :comdti
007219060118     C+ and :comdtf or
007220060118     C+ (tuadti <= :comdti and tuadtf>= :comdtf))
007221060118     C+ and (tutdti between :comdti and :comdtf or tutdtf between :comdti
007222060118     C+ and :comdtf or
007223160916     C+ (tutdti <= :comdti and tutdtf>= :comdtf))
007224060116     C+ order by tuaatt, tuaprg, tutcli desc, tutfil desc
007225060111     C/END-EXEC
007226060111     C/EXEC SQL
007227060111     C+ OPEN a1
007228060111     C/END-EXEC
007229060111     C                   DO        *hival
007230060111     C/EXEC SQL
007231060111     C+ FETCH NEXT FROM a1 INTO :tntuads, :tntutds
007232060111     C/END-EXEC
007233060111     C                   SELECT
007234060111     c*
007235060111     C                   WHEN      SqlCod = 100
007236060111     c                   leave
007237060111     C                   WHEN      SqlCod < 0
007238060111     c                   exsr      srfine
007239060111     c                   other
007240060116     c                   movel     tuaatt        comatt            6
007241060116     c                   move      tuaprg        comatt
007242060116     c                   if        comatt <> comatts
007243060116     c                   clear                   xx
007244060116     c                   clear                   po
007245060116     c                   clear                   cli
007246060116     c                   movel     comatt        comatts           6
007247060116     c                   end
007248060116     c                   select
007249060111     c* personalizzazioni per cliente
007250060116     c                   when      tutcli <> 0
007251060111     c                   exsr      srtempocli
007252060116     c* personalizzazioni per filiale
007253060116     c                   when      tutfil <> 0
007254060116     c                   exsr      srtempofil
007255060116     c* nessuna personalizzazioni
007256060116     c                   other
007257060116     c                   exsr      srtempomult
007258060116     c                   endsl
007259060116     c*
007260060111     C                   ENDSL
007261060111      *
007262060111     C                   ENDDO
007263060111     C/EXEC SQL
007264060111     C+ CLOSE a1
007265060111     C/END-EXEC
007266060117     c* scrivo i record di totale per ogni attività
007267060118     c                   exsr      srrectot
007268060120     c* chiudo pgm
007269060120     C                   CLEAR                   DSLV55
007270060120     C                   MOVEL     'C'           D55TLA
007271060120     C                   CALL      'FNLV55R'
007272060120     C                   PARM                    DSLV55
007273060116     c                   seton                                        lr
055700011127     C******************************************************
055800060111     C     srtempocli    BEGSR
055900060111     c******************************************************
055901060116     c                   add       1             xx
055902060116     c                   eval      cli(xx) = tutcli
055905060111     c* personalizzazione per cliente
055907060116     c     ktudcd        setll     tntud02l
055908060111     c                   do        *hival
055909060116     c     ktudc         reade     tntud02l
055910060116     c                   if        %eof(tntud02l)
055911060111     c                   leave
055912060111     c                   end
055913060116     c                   if        tuddta > dtfiso
055914060111     c                   leave
055915060111     c                   end
055916060331     c                   if        tuddta < tutdti
055917060331     c                   iter
055918060116     c                   end
055919060331     c                   if        tuddta > tutdtf
055920160916     c* verifico se ho la decorrenza successiva, se la trovo la data verrà
055921160916     c* valorizzata successivamente con il reord letto dall'sql iniziale
055923160915     c                   exsr      srtut
055924160915     c                   if        si = '1'
055925060331     c                   leave
055926160915     c                   end
055927160915     c* se non esiste il tempo con decorrenza successiva cerco la cartello
055928160915     c* se la trovo valorizzo
055929160915     c                   exsr      srcar
055930160915     c                   if        si = '0'
055931160915     c                   leave
055932160915     c                   end
055934060331     c                   end
055935060116     c                   exsr      scrivi
055936060111     c                   enddo
055937060111     c                   endsr
055938060111     C******************************************************
055939060111     C     srtempofil    BEGSR
055940060111     c******************************************************
055941060116     c                   add       1             xx
055942060116     c                   eval      po(xx) = tutfil
055943060111     c* personalizzazione per p.o.
055944060116     c     ktudpd        setll     tntud03l
055945060111     c                   do        *hival
055946060116     c     ktudp         reade     tntud03l
055947060116     c                   if        %eof(tntud03l)
055948060111     c                   leave
055949060111     c                   end
055950060116     c                   if        tuddta > dtfiso
055951060111     c                   leave
055952060111     c                   end
055953060331     c                   if        tuddta < tutdti
055954060331     c                   iter
055955060116     c                   end
055956060331     c                   if        tuddta > tutdtf
055958160915     c* verifico se ho la decorrenza successiva, se la trovo verrà
055959160915     c* valòorizzata successivamente con il reord letto dall'sql iniziale
055960160915     c                   exsr      srtut
055961160915     c                   if        si = '1'
055962160915     c                   leave
055963160915     c                   end
055964160915     c* se non esiste il tempo con decorrenza successiva cerco la cartello
055965160915     c* se la trovo valorizzo
055966160915     c                   exsr      srcar
055967160916     c                   if        si = ' '
055968160915     c                   leave
055971160915     c                   end
055972060331     c                   end
055973060116     c* se ho la personalizzazione per cliente rendo la qta negativa in modo
055974060116     c* da sottrarre dal p.o.
055975060116     c                   if        tudcli <> 0
055976060116     c                   eval      tudqta = tudqta * -1
055977060116     c                   end
055978060116     c                   exsr      scrivi
055979060111     c                   enddo
055980060111     c                   endsr
055981060111     C******************************************************
055982160915     C     srtut         BEGSR
055983060111     c******************************************************
055984160915     c                   eval      sqlcods = sqlcode
055985160916     c                   clear                   si                1
055988160916     c                   eval      comclis= tutcli
055989160916     c                   eval      comfils= tutfil
055990160916     C/EXEC SQL
055991160915    CC+  SELECT '1' into :si
055992160916    CC+     FROM tntut00f WHERE tutatt = :tuaatt and tutprg=:tuaprg and
055993160916    CC+     tutfil = :comfils and tutcli = :comclis and tutdti<= :tuddta
055994160916     C+     and tutdtf >= :tuddta and tutatb = ' '
055995160915     C+  FETCH first row only
055996160916     C/END-EXEC
055997160916     c                   eval      sqlcode  = sqlcods
055998160915     c                   endsr
055999160916     C******************************************************
056000160916     C     srcar         BEGSR
056001160916     c******************************************************
056002160916     c                   eval      sqlcods = sqlcode
056003160916     c                   movel     *hival        comtems
056006160916     C/EXEC SQL
056007160916    CC+  SELECT tuttem into :comtems
056008160916    CC+     FROM tntut00f WHERE tutatt = :tuaatt and tutprg=:tuaprg and
056009160916    CC+     tutfil = 0 and tutcli = 0 and tutdti<= :tuddta and
056010160916     C+     tutdtf >= :tuddta and tutatb = ' '
056011160916     C+  FETCH first row only
056012160916     C/END-EXEC
056013160916     c                   if        comtems = *hival
056014160916     c                   clear                   si
056015160916     c                   else
056016160916     c                   eval      tuttem = comtems
056017160916     c                   eval      si= '1'
056018160916     c                   end
056019160916     c                   eval      sqlcode  = sqlcods
056020160916     c                   endsr
056021160915     C******************************************************
056022160915     C     srtempomult   BEGSR
056023160915     c******************************************************
056024060116     c* reperisco i clienti personalizzati
056025060111     c* nessuna personalizzazione
056026060116     c     ktud1         setll     tntud01l
056027060111     c                   do        *hival
056028060116     c     tuamult       reade     tntud01l
056029060116     c                   if        %eof(tntud01l)
056030060111     c                   leave
056031060111     c                   end
056032060116     c                   if        tuddta > dtfiso
056033060116     c                   leave
056034060116     c                   end
056035060331     c                   if        tuddta < tutdti
056036060331     c                   iter
056037060116     c                   end
056038060331     c                   if        tuddta > tutdtf
056039060331     c                   leave
056040060331     c                   end
056041060116     c* controllo se il p.o. è già stato elaborato perchè personalizzato
056042060116     c                   if        tudcli = 0
056043060116     c     tudfil        lookup    po                                     01
056044060116     c                   if        *in01
056045060116     c                   iter
056046060116     c                   end
056047060116     c                   else
056048060116     c* se ho la personalizzazione per cliente rendo la qta negativa in modo
056049060116     c* da sottrarla dal p.o.
056050060116     c     tudcli        lookup    cli                                    01
056051060116     c                   if        *in01
056052060116     c                   eval      tudqta = tudqta * -1
056053060116     c                   else
056054060116     c                   iter
056055060116     c                   end
056056060116     c                   end
056057060116     c*
056058060111     c                   exsr      scrivi
056059060111     c                   enddo
056060060111     c                   endsr
056061060111     C******************************************************
056062060111     C     scrivi        BEGSR
056063060111     C******************************************************
056064060111     c                   clear                   tntus000
056065060120     c* se chi fa l'attività è TP (terminal partenza) o TA (terminal arrivo)
056066060120     c* vado a reperirlo
056067060120     c                   if        tuachi <> 'PO'
056068060120     c                   exsr      srchi
056069060120     c                   else
056070060120     c                   eval      comfil = tudfil
056071060120     c                   end
056072060120     c*
056073060116     c     ktus          chain     tntus01l
056074060111     c                   eval      tusatt = tuaatt
056075060111     c                   eval      tusprg = tuaprg
056076060116     c                   eval      tusaas = t08aa
056077060116     c                   eval      tusmes = t08mm
056078060111     c                   eval      tusrep = tuarep
056079060116     c                   eval      tusdta = tuddta
056080060120     c                   eval      tusfil = comfil
056081060116     c                   eval      tempo = tudqta * tuttem
056082060116     c                   add       tempo         tustem
056083060118     c                   if        tempo <> 0
056084060116     c                   if        not %found(tntus01l)
056085060111     c                   write     tntus000
056086060116     c                   else
056087060116     c                   update    tntus000
056088060116     c                   end
056089060118     c                   end
056090060111     C                   ENDSR
056091060111     C******************************************************
056092060117     C     srchi         BEGSR
056093060111     C******************************************************
056094060120     C* TERMINAL
056095060120     C                   CLEAR                   DSLV55
056096060120     C                   MOVEL     *BLANKS       D55TLA
056097060120     C                   MOVEL     ' '           D55TPT
056098060120     C                   Z-ADD     tudfil        D55LIN
056099060120     C                   move      tuddta        D55DRF
056100060120     C                   CALL      'FNLV55R'
056101060120     C                   PARM                    DSLV55
056102060120IF  1C     D55ERR        IFNE      *BLANKS
056103060120     C                   eval      comfil = tudfil
056104060120X   1C                   ELSE
056105060120     c                   if        tuachi = 'TA'
056106060120     C                   eval      comfil = D55TFA
056107060120     c                   else
056108060120     C                   eval      comfil = D55TFP
056109060120     c                   end
056110060120E   1C                   ENDIF
056111060117     c                   endsr
056112060117     C******************************************************
056113060117     C     srmultfis     BEGSR
056114060117     C******************************************************
056115060118     c     'TUT'         setll     tntbe01l
056116060117     c                   do        *hival
056117060118     c     'TUT'         reade     tntbe01l
056118060117     c                   if        %eof(tntbe01l)
056119060117     c                   leave
056120060117     c                   end
056121060117     c* annullati o non moltiplicatori fissi
056122060117     c                   if        tbeatb <> ' '
056123060117     c                   iter
056124060117     c                   end
056125060117     c                   movel     tbeuni        dtut
056126060117     c                   if        §tutril = 'S'
056127060117     c                   iter
056128060117     c                   end
056129060118     c* cancello i moltiplicatori fissi del periodo nel caso si stia
056130060118     c* rielaborando
056131060118     c                   exsr      srdeltud
056132091030     c* aggangio tabella MFM per verificare quanti gg aggiungere
056133091030     c                   exsr      srmfm
056134091102     c* se non presenti gg salto inserimento
056135091102     c                   if        mtab(t08mm) =  0
056136091102     c                   iter
056137091102     c                   end
056138060123     c* inserisco i moltiplicatori fissi se il po ha eseguito almeno
056139060123     c* un'attività in un determinato gg (compreso nel periodo) lavorativo
056140060120     C/EXEC SQL
056141060120     C+ DECLARE B1 CURSOR FOR SELECT tudfil, tuddta FROM tntud00f WHERE
056142060120     C+ tuddta between :comdti and :comdtf GROUP BY tudfil, tuddta ORDER BY
056143060120     C+ tudfil, tuddta
056144060120     C/END-EXEC
056145060120     C/EXEC SQL
056146060120     C+ OPEN b1
056147060120     C/END-EXEC
056148060120     C                   DO        *hival
056149060120     C/EXEC SQL
056150060120     C+ FETCH NEXT FROM b1 INTO :fil, :dta
056151060120     C/END-EXEC
056152060120     C                   SELECT
056153060120     c*
056154060120     C                   WHEN      SqlCod = 100
056155060120     c                   leave
056156060120     C                   WHEN      SqlCod < 0
056157060120     c                   exsr      srfine
056158060120     c                   other
056159091102     c                   if        fil <> savfil
056160091102     c                   clear                   contagg
056161091102     c                   eval      savfil = fil
056162091102     c                   end
056163091102     c* se raggiunto limite gg da inserire nel mese x la filiale non
056164091102     c* proseguo
056165091102     c                   if        contagg >= mtab(t08mm)
056166091102     c                   iter
056167091102     c                   end
056168060118     c*
056169060120     c                   move      dta           dadata
056170060120     c                   move      dta           adata
056171060117     c                   CALL      'XSRLAV8'
056172060117     c                   PARM                    Wdata8
056173091102    4c                   if        giolav = 1
056174060118     c                   clear                   tntud000
056175060118     c                   movel     tbeke1        tudmult
056176060118     c                   move      §tutqta       tudqta
056177060120     c                   move      dta           tuddta
056178060120     c                   move      fil           tudfil
056179060118     c                   write     tntud000
056180091102     c                   add       1             contagg
056181060118    4c                   endif
056182060120     c*
056183060120     C                   ENDSL
056184060120      *
056185060120     C                   ENDDO
056186060120     C/EXEC SQL
056187060120     C+ CLOSE b1
056188060120     C/END-EXEC
056189060118     c                   enddo
056190060118     c*
056191060117     c                   endsr
056192060117     C******************************************************
056193091030     C     srmfm         BEGSR
056194060117     C******************************************************
056195091030     c                   if        tbeke1 = '900'
056196091102     c                   eval      mtab = *hival
056197091030     c                   else
056198091030     c     keytab        setll     tntbex1l
056199091030     c     keytab1       reade     tntbex1l
056200091030     c                   if        %eof(tntbex1l) or
056201091030     c                             (not %eof(tntbex1l) and £tbeatb <> ' ' )
056202091030     c                   clear                   mtab
056203091030     c                   else
056204091030     c                   movel     £tbeuni       dmfm
056205091030     c                   end
056206091030     c                   end
056207091030     c                   endsr
056208091030     C******************************************************
056209091030     C     srdeltud      BEGSR
056210091030     C******************************************************
056211060118     C/EXEC SQL
056212060118     C+ delete from tntud00f where tudmult = :tbeke1 and tuddta between
056213060118     C+ :comdti and :comdtf
056214060118     C/END-EXEC
056215060117     c                   endsr
056216060118     C******************************************************
056217060118     C     srrectot      BEGSR
056218060118     C******************************************************
056219060118     C/EXEC SQL
056220060403     C+ INSERT INTO TNTUS10F SELECT TUSATT, TUSPRG, TUSFIL,
056221060403     C+ TUSAAS, TUSMES, TUSREP, sum(TUSTEM) FROM tntus00f WHERE TUSAAS =
056222060403     C+ :t08aa and TUSMES =:t08mm GROUP BY TUSATT, TUSPRG, TUSFIL,
056223060403     C+ TUSAAS,TUSMES, TUSREP
056224060118     C/END-EXEC
056225060118     c                   endsr
056226060117     C******************************************************
056227060117     C     *INZSR        BEGSR
056228060117     C******************************************************
056229011127     C*
056230011127     C     *ENTRY        PLIST
056231051121     C                   PARM                    KPJBA
056232060111     c                   movel     kpjbu         tntu08ds
056233060111     c*
056234060116     c                   move      t08aa         comaa             4
056235060116     c                   move      t08mm         commm             2
056236060403     c                   eval      comdti = comaa + '-' + commm + '-01'
056237060403     c                   move      comdti        dtiiso
056238060403     c     dtiiso        adddur    1:*m          dtfiso
056239060403     c                   subdur    1:*d          dtfiso
056240060403     c                   move      dtfiso        comdtf
056241091102     c                   move      dtfiso        com08
056242091102     c                   movel     com08         £tbeke2
056243060111     c* mi memorizzo i terminal, p.o., contabilità e p.o. gestionali
056244060111     c* in un work file
056245060116     c*                  exsr      srpo
056246060111     c*
056247060111     C     ktudcd        KLIST
056248060111     C                   KFLD                    tuamult
056249060111     C                   KFLD                    tutcli
056250060111     C                   KFLD                    dtiiso
056251060111     C     ktudc         KLIST
056252060111     C                   KFLD                    tuamult
056253060111     C                   KFLD                    tutcli
056254060111     C     ktudpd        KLIST
056255060111     C                   KFLD                    tuamult
056256060111     C                   KFLD                    tutfil
056257060111     C                   KFLD                    dtiiso
056258060116     C     ktudp         KLIST
056259060111     C                   KFLD                    tuamult
056260060111     C                   KFLD                    tutfil
056261060111     C     ktud1         KLIST
056262060111     C                   KFLD                    tuamult
056263060111     C                   KFLD                    dtiiso
056264060116     C     ktus          KLIST
056265060116     C                   KFLD                    tuaatt
056266060116     C                   KFLD                    tuaprg
056267060116     C                   KFLD                    tuddta
056268060120     C                   KFLD                    comfil
056269060118     C                   KFLD                    tuarep
056270091030     c     Keytab        klist
056271091030     c                   kfld                    £TBEcod
056272091030     c                   kfld                    TBEke1
056273091030     c                   kfld                    £TBEke2
056274091030     c     Keytab1       klist
056275091030     c                   kfld                    £TBEcod
056276091030     c                   kfld                    TBEke1
056277091030     c                   eval      £tbecod = 'MFM'
056278060117     c* recupero i p.o.
056279060117     c                   clear                   tibs56ds
056280060118     c                   eval      i56ppo=001
056281060117     c                   eval      i56mod='POA'
056282060117     c                   call      'TIBS56R'
056283060117     c                   parm                    tibs56ds
056284060118     c* cancello la valorizzazione precedente ammesso che ci sia stata
056285060118     C/EXEC SQL
056286060118     C+ delete from tntus00f where tusaas = :t08aa and tusmes = :t08mm
056287060118     C/END-EXEC
056288060124     C/EXEC SQL
056289060124     C+ delete from tntus10f where tus1aas = :t08aa and tus1mes = :t08mm
056290060124     C/END-EXEC
059500011127     C                   ENDSR
059600060116     C**************************************************************************
059700060116     C* esco con codice d'errore
059800060116     C**************************************************************************
059900060116     C     srfine        BEGSR
060000060116     c                   seton                                        lr
060100060116     c                   return
060200060116     c*
060300060116     c                   endsr

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
004604060120     D fil             S                   like(tudfil)
004605060120     D dta             S                   like(tuddta)
004606060111     d trul06ds      e ds
004607060111     d  lint                   1     90  0 dim(30)
004608060116     d  po             s              3  0 dim(500)
004609060116     d  cli            s              7  0 dim(2000)
004610060116     d  xx             s              4  0
004611060116     d  comdti         s             10
004612060116     d  comdtf         s             10
004613091102     d  com08          s              8  0
004614091102     d  savfil         s                   like(fil)
004615091102     d  contagg        s              2  0
004616060116     d  TEMPO          s                   LIKE(TUSTEM)
004617091030     c* gg mensili per moltiplicatori fissi
004618091030     D ptab            S               *
004619091030     D                                     INZ(%ADDR(D§mfm001))
004620091030     D mtab            S                   LIKE(D§mfm001)
004621091030     D                                     DIM(12)
004622091030     D                                     BASED(Ptab)
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
007223060118     C+ (tutdti <= :comdti and tutdtf>= :comdtf))
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
055920060331     c                   leave
055921060331     c                   end
055922060116     c                   exsr      scrivi
055923060111     c                   enddo
055924060111     c                   endsr
055925060111     C******************************************************
055926060111     C     srtempofil    BEGSR
055927060111     c******************************************************
055928060116     c                   add       1             xx
055929060116     c                   eval      po(xx) = tutfil
055930060111     c* personalizzazione per p.o.
055931060116     c     ktudpd        setll     tntud03l
055932060111     c                   do        *hival
055933060116     c     ktudp         reade     tntud03l
055934060116     c                   if        %eof(tntud03l)
055935060111     c                   leave
055936060111     c                   end
055937060116     c                   if        tuddta > dtfiso
055938060111     c                   leave
055939060111     c                   end
055940060331     c                   if        tuddta < tutdti
055941060331     c                   iter
055942060116     c                   end
055943060331     c                   if        tuddta > tutdtf
055944060331     c                   leave
055945060331     c                   end
055946060116     c* se ho la personalizzazione per cliente rendo la qta negativa in modo
055947060116     c* da sottrarre dal p.o.
055948060116     c                   if        tudcli <> 0
055949060116     c                   eval      tudqta = tudqta * -1
055950060116     c                   end
055951060116     c                   exsr      scrivi
055952060111     c                   enddo
055953060111     c                   endsr
055974060111     C******************************************************
055975060111     C     srtempomult   BEGSR
055976060111     c******************************************************
055977060116     c* reperisco i clienti personalizzati
056015060111     c* nessuna personalizzazione
056017060116     c     ktud1         setll     tntud01l
056018060111     c                   do        *hival
056019060116     c     tuamult       reade     tntud01l
056020060116     c                   if        %eof(tntud01l)
056021060111     c                   leave
056022060111     c                   end
056024060116     c                   if        tuddta > dtfiso
056025060116     c                   leave
056026060116     c                   end
056027060331     c                   if        tuddta < tutdti
056028060331     c                   iter
056029060116     c                   end
056030060331     c                   if        tuddta > tutdtf
056031060331     c                   leave
056032060331     c                   end
056038060116     c* controllo se il p.o. è già stato elaborato perchè personalizzato
056039060116     c                   if        tudcli = 0
056040060116     c     tudfil        lookup    po                                     01
056041060116     c                   if        *in01
056042060116     c                   iter
056043060116     c                   end
056044060116     c                   else
056045060116     c* se ho la personalizzazione per cliente rendo la qta negativa in modo
056046060116     c* da sottrarla dal p.o.
056048060116     c     tudcli        lookup    cli                                    01
056049060116     c                   if        *in01
056050060116     c                   eval      tudqta = tudqta * -1
056051060116     c                   else
056052060116     c                   iter
056053060116     c                   end
056054060116     c                   end
056055060116     c*
056056060111     c                   exsr      scrivi
056057060111     c                   enddo
056058060111     c                   endsr
056059060111     C******************************************************
056060060111     C     scrivi        BEGSR
056061060111     C******************************************************
056062060111     c                   clear                   tntus000
056063060120     c* se chi fa l'attività è TP (terminal partenza) o TA (terminal arrivo)
056064060120     c* vado a reperirlo
056065060120     c                   if        tuachi <> 'PO'
056066060120     c                   exsr      srchi
056067060120     c                   else
056068060120     c                   eval      comfil = tudfil
056069060120     c                   end
056070060120     c*
056071060116     c     ktus          chain     tntus01l
056072060111     c                   eval      tusatt = tuaatt
056073060111     c                   eval      tusprg = tuaprg
056074060116     c                   eval      tusaas = t08aa
056075060116     c                   eval      tusmes = t08mm
056076060111     c                   eval      tusrep = tuarep
056077060116     c                   eval      tusdta = tuddta
056078060120     c                   eval      tusfil = comfil
056079060116     c                   eval      tempo = tudqta * tuttem
056080060116     c                   add       tempo         tustem
056081060118     c                   if        tempo <> 0
056082060116     c                   if        not %found(tntus01l)
056083060111     c                   write     tntus000
056084060116     c                   else
056085060116     c                   update    tntus000
056086060116     c                   end
056087060118     c                   end
056088060111     C                   ENDSR
056089060111     C******************************************************
056090060117     C     srchi         BEGSR
056091060111     C******************************************************
056092060120     C* TERMINAL
056093060120     C                   CLEAR                   DSLV55
056094060120     C                   MOVEL     *BLANKS       D55TLA
056095060120     C                   MOVEL     ' '           D55TPT
056096060120     C                   Z-ADD     tudfil        D55LIN
056097060120     C                   move      tuddta        D55DRF
056098060120     C                   CALL      'FNLV55R'
056099060120     C                   PARM                    DSLV55
056100060120IF  1C     D55ERR        IFNE      *BLANKS
056101060120     C                   eval      comfil = tudfil
056102060120X   1C                   ELSE
056103060120     c                   if        tuachi = 'TA'
056104060120     C                   eval      comfil = D55TFA
056105060120     c                   else
056106060120     C                   eval      comfil = D55TFP
056107060120     c                   end
056108060120E   1C                   ENDIF
056109060117     c                   endsr
056110060117     C******************************************************
056111060117     C     srmultfis     BEGSR
056112060117     C******************************************************
056113060118     c     'TUT'         setll     tntbe01l
056114060117     c                   do        *hival
056115060118     c     'TUT'         reade     tntbe01l
056116060117     c                   if        %eof(tntbe01l)
056117060117     c                   leave
056118060117     c                   end
056119060117     c* annullati o non moltiplicatori fissi
056120060117     c                   if        tbeatb <> ' '
056121060117     c                   iter
056122060117     c                   end
056123060117     c                   movel     tbeuni        dtut
056124060117     c                   if        §tutril = 'S'
056125060117     c                   iter
056126060117     c                   end
056127060118     c* cancello i moltiplicatori fissi del periodo nel caso si stia
056128060118     c* rielaborando
056129060118     c                   exsr      srdeltud
056130091030     c* aggangio tabella MFM per verificare quanti gg aggiungere
056132091030     c                   exsr      srmfm
056133091102     c* se non presenti gg salto inserimento
056134091102     c                   if        mtab(t08mm) =  0
056135091102     c                   iter
056136091102     c                   end
056137060123     c* inserisco i moltiplicatori fissi se il po ha eseguito almeno
056138060123     c* un'attività in un determinato gg (compreso nel periodo) lavorativo
056139060120     C/EXEC SQL
056140060120     C+ DECLARE B1 CURSOR FOR SELECT tudfil, tuddta FROM tntud00f WHERE
056141060120     C+ tuddta between :comdti and :comdtf GROUP BY tudfil, tuddta ORDER BY
056142060120     C+ tudfil, tuddta
056143060120     C/END-EXEC
056144060120     C/EXEC SQL
056145060120     C+ OPEN b1
056146060120     C/END-EXEC
056147060120     C                   DO        *hival
056148060120     C/EXEC SQL
056149060120     C+ FETCH NEXT FROM b1 INTO :fil, :dta
056150060120     C/END-EXEC
056151060120     C                   SELECT
056152060120     c*
056153060120     C                   WHEN      SqlCod = 100
056154060120     c                   leave
056155060120     C                   WHEN      SqlCod < 0
056156060120     c                   exsr      srfine
056157060120     c                   other
056158091102     c                   if        fil <> savfil
056159091102     c                   clear                   contagg
056160091102     c                   eval      savfil = fil
056161091102     c                   end
056162091102     c* se raggiunto limite gg da inserire nel mese x la filiale non
056163091102     c* proseguo
056164091102     c                   if        contagg >= mtab(t08mm)
056165091102     c                   iter
056166091102     c                   end
056167060118     c*
056168060120     c                   move      dta           dadata
056169060120     c                   move      dta           adata
056170060117     c                   CALL      'XSRLAV8'
056171060117     c                   PARM                    Wdata8
056172091102    4c                   if        giolav = 1
056173060118     c                   clear                   tntud000
056174060118     c                   movel     tbeke1        tudmult
056175060118     c                   move      §tutqta       tudqta
056176060120     c                   move      dta           tuddta
056177060120     c                   move      fil           tudfil
056178060118     c                   write     tntud000
056179091102     c                   add       1             contagg
056180060118    4c                   endif
056181060120     c*
056182060120     C                   ENDSL
056183060120      *
056184060120     C                   ENDDO
056185060120     C/EXEC SQL
056186060120     C+ CLOSE b1
056187060120     C/END-EXEC
056188060118     c                   enddo
056189060118     c*
056190060117     c                   endsr
056191060117     C******************************************************
056192091030     C     srmfm         BEGSR
056193060117     C******************************************************
056194091030     c                   if        tbeke1 = '900'
056195091102     c                   eval      mtab = *hival
056196091030     c                   else
056197091030     c     keytab        setll     tntbex1l
056198091030     c     keytab1       reade     tntbex1l
056199091030     c                   if        %eof(tntbex1l) or
056200091030     c                             (not %eof(tntbex1l) and £tbeatb <> ' ' )
056201091030     c                   clear                   mtab
056202091030     c                   else
056203091030     c                   movel     £tbeuni       dmfm
056204091030     c                   end
056205091030     c                   end
056206091030     c                   endsr
056207091030     C******************************************************
056208091030     C     srdeltud      BEGSR
056209091030     C******************************************************
056210060118     C/EXEC SQL
056211060118     C+ delete from tntud00f where tudmult = :tbeke1 and tuddta between
056212060118     C+ :comdti and :comdtf
056213060118     C/END-EXEC
056214060117     c                   endsr
056215060118     C******************************************************
056216060118     C     srrectot      BEGSR
056217060118     C******************************************************
056218060118     C/EXEC SQL
056219060403     C+ INSERT INTO TNTUS10F SELECT TUSATT, TUSPRG, TUSFIL,
056220060403     C+ TUSAAS, TUSMES, TUSREP, sum(TUSTEM) FROM tntus00f WHERE TUSAAS =
056221060403     C+ :t08aa and TUSMES =:t08mm GROUP BY TUSATT, TUSPRG, TUSFIL,
056222060403     C+ TUSAAS,TUSMES, TUSREP
056223060118     C/END-EXEC
056224060118     c                   endsr
056225060117     C******************************************************
056226060117     C     *INZSR        BEGSR
056227060117     C******************************************************
056228011127     C*
056229011127     C     *ENTRY        PLIST
056230051121     C                   PARM                    KPJBA
056231060111     c                   movel     kpjbu         tntu08ds
056232060111     c*
056233060116     c                   move      t08aa         comaa             4
056234060116     c                   move      t08mm         commm             2
056235060403     c                   eval      comdti = comaa + '-' + commm + '-01'
056236060403     c                   move      comdti        dtiiso
056237060403     c     dtiiso        adddur    1:*m          dtfiso
056238060403     c                   subdur    1:*d          dtfiso
056239060403     c                   move      dtfiso        comdtf
056240091102     c                   move      dtfiso        com08
056241091102     c                   movel     com08         £tbeke2
056242060111     c* mi memorizzo i terminal, p.o., contabilità e p.o. gestionali
056243060111     c* in un work file
056244060116     c*                  exsr      srpo
056245060111     c*
056246060111     C     ktudcd        KLIST
056247060111     C                   KFLD                    tuamult
056248060111     C                   KFLD                    tutcli
056249060111     C                   KFLD                    dtiiso
056250060111     C     ktudc         KLIST
056251060111     C                   KFLD                    tuamult
056252060111     C                   KFLD                    tutcli
056253060111     C     ktudpd        KLIST
056254060111     C                   KFLD                    tuamult
056255060111     C                   KFLD                    tutfil
056256060111     C                   KFLD                    dtiiso
056257060116     C     ktudp         KLIST
056258060111     C                   KFLD                    tuamult
056259060111     C                   KFLD                    tutfil
056260060111     C     ktud1         KLIST
056261060111     C                   KFLD                    tuamult
056262060111     C                   KFLD                    dtiiso
056263060116     C     ktus          KLIST
056264060116     C                   KFLD                    tuaatt
056265060116     C                   KFLD                    tuaprg
056266060116     C                   KFLD                    tuddta
056267060120     C                   KFLD                    comfil
056268060118     C                   KFLD                    tuarep
056269091030     c     Keytab        klist
056270091030     c                   kfld                    £TBEcod
056271091030     c                   kfld                    TBEke1
056272091030     c                   kfld                    £TBEke2
056273091030     c     Keytab1       klist
056274091030     c                   kfld                    £TBEcod
056275091030     c                   kfld                    TBEke1
056276091030     c                   eval      £tbecod = 'MFM'
056277060117     c* recupero i p.o.
056278060117     c                   clear                   tibs56ds
056279060118     c                   eval      i56ppo=001
056280060117     c                   eval      i56mod='POA'
056281060117     c                   call      'TIBS56R'
056282060117     c                   parm                    tibs56ds
056283060118     c* cancello la valorizzazione precedente ammesso che ci sia stata
056284060118     C/EXEC SQL
056285060118     C+ delete from tntus00f where tusaas = :t08aa and tusmes = :t08mm
056286060118     C/END-EXEC
056287060124     C/EXEC SQL
056288060124     C+ delete from tntus10f where tus1aas = :t08aa and tus1mes = :t08mm
056289060124     C/END-EXEC
059500011127     C                   ENDSR
059600060116     C**************************************************************************
059700060116     C* esco con codice d'errore
059800060116     C**************************************************************************
059900060116     C     srfine        BEGSR
060000060116     c                   seton                                        lr
060100060116     c                   return
060200060116     c*
060300060116     c                   endsr

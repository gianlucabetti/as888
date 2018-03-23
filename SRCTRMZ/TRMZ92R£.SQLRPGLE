000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000101070730     fmzgad01l  if   e           k disk
000102070926     fazorg01l  if   e           k disk
000103070731     ftabel00f  if   e           k disk
000104070921     fwfctra0f  o    e             disk
000105070730     fqsysprt   o    f  132        printer oflind(*inof)
000106070731     d* Parametri x Controllo profilo utenti
000107070731     d TIBS34ds      e ds
000108070731     d AZUTEds       e ds                  extname(AZUTE00F)
000109070926     D X16GGSETDS    e ds
000110070731     d DDatiUte      e ds
000111070730     D trmz91ds      e ds                  inz
000112070730     D trmz61ds      e ds                  inz
000113070730     D trmz62ds      e ds                  inz
000114070731     d dsxn          e ds
000115070730     D GFES8           DS
000116070730     D  DATADA                 1      8  0
000117070730     D  DATAA                  9     16  0
000118070730     D  GGF                   17     21  0
000119070730     d  tga            s                   like(d61tga)
000120070730     d  tgn            s                   like(d61tgn)
000121070730     d  dta            s                   like(d61dta)
000122070926     d  nominativo     s             15
000123070926     d  dtaa           s              8
000124070926     d  oraa           s              6
000125070926     d  ind            s             24
000126070926     d  loc            s             20
000127070926     d  prov           s              2
000128070926     d  div            s              3
000129070926     d  prt            s             11  3
000130070926     d  oram           s              8
000131070926     d  comora         s              6
000132070926     d  desm           s             36
000133070921     d  savdtai        s                   like(d61dta)
000134070921     d  savdtaf        s                   like(d61dta)
000135070730     d  ora            s                   like(gadora)
000136070921     d  mingad         s                   like(gaddta)
000137070921     d  maxgad         s                   like(gaddta)
000138070730     d  dtaiso         s               d
000139070730     d  dataiso        s               d
000140070731     d  dataeur        s               d   datfmt(*eur)
000141070731     d  sdtada         s               d   datfmt(*eur)
000142070731     d  sdtaa          s               d   datfmt(*eur)
000143070921     d  datai          s               d   datfmt(*eur)
000144070921     d  dataf          s               d   datfmt(*eur)
000145070730     d  com91dtada     s             10
000146070730     d  com91dtaa      s             10
000147070730     d  dtm            s             10
000812070730     d kpjba         e ds
000813070921     c                   eval      savdtai = *hival
000814070921     c                   eval      ctratpu = 'A'
000815070730     c* controllo agip
003100031028     C/EXEC SQL
003200070926     C+ DECLARE A1 CURSOR FOR SELECT NOMINATIVO, DATA_00003, ORA_V00001,
003201070926     C+ INDIR00001, LOCAL00001, PROV_PV, IMPORTO FROM agip01l WHERE
003202070926     C+ DATA_00003 between :d91dtada and :d91dtaa and FLAG00001 ='C'
003203070926     C+ ORDER BY NOMINATIVO, DATA_00003
003600031028     C/END-EXEC
003700031106
003800031106     C/EXEC SQL
003900031106     C+ OPEN A1
004000031106     C/END-EXEC
004100031028
004200070731     C                   do        *hival
004300031028
004400031028     C/EXEC SQL
004500070926     C+ FETCH NEXT FROM A1 INTO :nominativo, :dta, :oraa, :ind, :loc,
004501070926     C+ :prov, :prt
004600031028     C/END-EXEC
004700031028
004800031028     C                   select
004801070926     c* fine file
004900031028     C                   when      sqlcod = 100
004901070921     c                   move      savdtai       dataiso
004902070920     c                   move      dataiso       dataeur
004903070921     c                   move      dataeur       datai
004904070921     c                   move      savdtaf       dataiso
004905070921     c                   move      dataiso       dataf
004906070926     c* stampo periodo agip
004907070920     c                   except    ultagip
005001070731     c                   leave
005002070926     c* dettaglio
005400031028     C                   when      sqlcod = 0
005401070926     c* imposto i campi comuni
005402070926     c                   movel     nominativo    tga
005403070926     c                   eval      tgn = %subst(nominativo: 3: 3)+' ' +
005404070926     c                             %subst(nominativo: 6: 2)
005405070926     c                   eval      ctraluogo = ind+loc+prov
005406070926     c                   if        oraa <> ' '
005407070926     c                   movel     oraa          ctraora
005408070926     c                   else
005409070926     c                   clear                   ctraora
005410070926     c                   end
005411070921     c* salvo la data del primo e dell'ultimo movimento trovato in archivio
005412070921     c                   if        dta < savdtai
005413070921     c                   eval      savdtai=dta
005414070920     c                   end
005415070921     c                   if        dta > savdtaf
005416070921     c                   eval      savdtaf=dta
005417070921     c                   end
005500070926     c* controllo il rifornimento
005600070730     c                   exsr      srctr
005700031028     C                   other
005800031028     C                   exsr      sqlerror
005900031028     C                   endsl
006000031028     C*
006100031028     C                   enddo
006200031028
006300031028     C/EXEC SQL
006400031028     C+ CLOSE A1
006500031028     C/END-EXEC
006501070926     c* inizializzo campi
006503070730     c                   move      d91dtada      dtaiso
006504070730     c                   move      dtaiso        com91dtada
006505070730     c                   move      d91dtaa       dtaiso
006506070730     c                   move      dtaiso        com91dtaa
006507070921     c                   eval      savdtai = *hival
006508070921     c                   eval      ctratpu = 'F'
006509070921     c                   clear                   savdtaf
006510070921     c                   seton                                        02
006511070731     c                   setoff                                       01
006512070926     c* controllo faiservice
006513070730     C/EXEC SQL
006514070926     C+ DECLARE B1 CURSOR FOR SELECT FAITGA, FAITGN, faidtm, FAIORAM,
006515070926     C+ FAIDES, faiimp FROM mzfai00f WHERE faidtm between :com91dtada and
006516070926     C+ :com91dtaa and faitga <> ' ' and FAITMV = 'PE' ORDER BY FAITGA,
006517070926     C+ FAITGN, FAIDTM
006518070730     C/END-EXEC
006519070730
006520070730     C/EXEC SQL
006521070730     C+ OPEN b1
006522070730     C/END-EXEC
006523070730
006524070731     C                   do        *hival
006525070730
006526070730     C/EXEC SQL
006527070926     C+ FETCH NEXT FROM B1 INTO :tga, :tgn, :dtm, :oram, :desm, :prt
006528070730     C/END-EXEC
006529070730
006530070730     C                   select
006531070926     c* fine file
006532070730     C                   when      sqlcod = 100
006533070921     c                   move      savdtai       dataiso
006534070921     c                   move      dataiso       datai
006535070921     c                   move      savdtaf       dataiso
006536070921     c                   move      dataiso       dataf
006537070926     c* stampo periodo fais
006538070920     c                   except    ultfais
006539070731     C                   leave
006540070926     c* dettaglio
006541070730     C                   when      sqlcod = 0
006542070926     c* imposto i campi comuni
006543070730     c                   move      dtm           dtaiso
006544070730     c                   move      dtaiso        dta
006545070926     c                   eval      ctraluogo = desm
006546070926     c                   if        oram <> ' '
006547070926     c                   eval      comora = %subst(oram: 1: 2)+
006548070926     c                             %subst(oram: 4: 2)+
006549070926     c                             %subst(oram: 7: 2)
006550070926     c                   movel     comora        ctraora
006551070926     c                   else
006552070926     c                   clear                   ctraora
006553070926     c                   end
006554070926     c* controllo pedaggio
006555070730     c                   exsr      srctr
006556070921     c* salvo la data del primo e dell'ultimo movimento trovato in archivio
006557070921     c                   if        dta > savdtaf
006558070921     c                   eval      savdtaf=dta
006559070920     c                   end
006560070921     c                   if        dta < savdtai
006561070921     c                   eval      savdtai=dta
006562070921     c                   end
006563070730     C                   other
006564070730     C                   exsr      sqlerror
006565070730     C                   endsl
006566070730     C*
006567070730     C                   enddo
006568070730
006569070730     C/EXEC SQL
006570070730     C+ CLOSE b1
006571070730     C/END-EXEC
006572070920
006573070926     c* reperisco periodo rilevazioni presenze
006574070920     C/EXEC SQL
006575070921     C+ SELECT min(gaddta), max(GADDTA) INTO :mingad, :maxgad FROM
006576070921     C+ mzgad00f WHERE gaddta between :com91dtada and :com91dtaa
006577070920     C/END-EXEC
006578070921     c                   move      mingad        dataiso
006579070921     c                   move      dataiso       datai
006580070921     c                   move      maxgad        dataiso
006581070921     c                   move      dataiso       dataf
006582070920     c                   except    ultgad
006583070920     c                   except    fine
006584070731     c                   seton                                        lr
006600031028     C**********************************************************************
006700070730     c* scrive record MZgad00F
006800031028     C**********************************************************************
006900070730     c     srctr         begsr
006901070730     c* controllo se è un gg di festa
006902070730     C                   Z-ADD     dta           DATADA
006903070730     C                   Z-ADD     dta           DATAA
006904070920     c                   clear                   ggf
006905070730     C                   CALL      'XGGFEST'
006906070730     C                   PARM                    GFES8
007000070730     c                   if        ggf <> 0
007001070926     C* - verifico se sabato o domenica
007002070926     C                   CLEAR                   X16GGSETDS
007003070926     C     *ISO          MOVE      Dta           X16DATA
007004070926     C                   CALL      'X16GGSET'
007005070926     C                   PARM                    X16GGSETDS
007006070921     c                   seton                                        10
007007070731     c                   end
007008070730     c* reperisco l'autista
007009070730     c                   reset                   trmz61ds
007010070730     c                   eval      d61tga = tga
007011070730     c                   eval      d61tgn = tgn
007015070730     c                   move      dta           d61dta
007016070730     c                   call      'TRMZ61R'
007017070730     c                   parm                    trmz61ds
007018070730     c* ritorno E= errore bloccante A=anomalia
007020070730     c                   if        d61err <>' '
007021070730     c                   seton                                        11
007023070731     c                   else
007024070730     c* reperisco codice dipendente
007025070802     c                   if        d61cauo <> ' '
007026070730     c                   clear                   trmz62ds
007027070730     c                   move      dta           d62dta
007028070730     c                   move      d61cauo       d62cau
007029070730     c                   call      'TRMZ62R'
007030070730     c                   parm                    trmz62ds
007031070802     c                   if        d62soco <> ' '
007032070730     c* controllo assenze
007033070730     c                   clear                   ora
007034070730     c                   move      dta           dtaiso
007035070730     c     kgad          setll     mzgad01l
007036070730     c                   do        *hival
007037070730     c     kgad          reade     mzgad01l
007038070730     c                   if        %eof(mzgad01l)
007039070730     c                   leave
007040070730     c                   end
007041070730     c                   add       gadora        ora
007042070730     c                   enddo
007043070730     c                   if        ora >= 8
007044070730     c                   seton                                        12
007045070730     c                   end
007046070802     c                   else
007047070802     c                   seton                                        13
007050070730     c                   end
007051070802     c                   else
007052070802     c                   seton                                        13
007053070802     c                   end
007054070802     c                   end
007056070731     c* stampa
007057070921     c                   if        *in10 or *in11 or *in12 or *in13
007058070731     c                   exsr      stampa
007059070731     c                   end
012500031028     c                   endsr
012501070730     C**********************************************************************
012502070730     C     sqlerror      begsr
012503070730     C**********************************************************************
012504070730     C*
012505070730     C                   call      'X66CHGJOB'
012506070730     C                   seton                                        lr
012507070730     C                   ENDSR
012508070730     C**********************************************************************
012509070730     C     stampa        begsr
012510070730     C**********************************************************************
012511070730     c*
012512070730     c                   if        *inof
012513070921     C                   EXCEPT    TESTA
012514070730     c                   if        *in01
012515070730     c                   except    testaagi
012516070730     c                   else
012517070730     c                   except    testafai
012518070730     c                   end
012519070730     c                   end
012520070730     c                   setoff                                       of
012521070730     c*
012522070730     c                   move      dta           dataiso
012523070730     c                   move      dataiso       dataeur
012524070731     c                   eval      tblkey = d61cauo
012525070731     c                   clear                   dsxn
012526070731     c     ktab          chain     tabel00f
012527070731     c                   if        %found(tabel00f)
012528070731     c                   eval      dsxn = tbluni
012529070731     c                   end
012530070926     c                   clear                   ctrafil
012531070926     c                   clear                   ctraarea
012532070926     c                   clear                   ctradis
012533070926     c                   if        d61cdc <> ' '
012534070926     c                   movel     d61cdc        comfil            3 0
012535070926     c     comfil        chain     azorg01l
012536070926     c                   if        %found(azorg01l)
012537070926     c                   eval      ctrafil = comfil
012538070926     c                   movel(p)  orgfl3        ctradis
012539070926     c                   movel(p)  orgcar        ctraarea
012541070926     c                   end
012542070926     c                   end
012543070921     c* per ora non stampo ma memorizzo solo file di lavoro
012544070921     c*  10
012545070921     c*r 12              except    err01
012546070921     c   10
012547070921     cor 12              exsr      wtrwf
012548070730     c   11              except    err02
012549070921     c   13              except    err03
012550070730     c                   setoff                                       101112
012551070802     c                   setoff                                       13
012552070730     C*
012553070730     C                   ENDSR
018000020709     C**********************************************************************
018100070921     C     wtrwf         BEGSR
018200020709     C**********************************************************************
018301070921     C*
018303070921     c                   movel     tga           ctratgN
018304070921     c                   move      tgn           ctratgn
018307070926     c                   eval      ctradtu = dta
018308070921     c                   eval      ctracau = d61cauo
018309070921     c                   eval      ctrades = §xndes
018310070921     c                   eval      ctrasoc = d62soco
018311070921     c                   eval      ctramat = d62mato
018312070926     c                   if        *in10
018313070926     c                   select
018314070926     c                   when      x16giorno = 1
018315070926     c                   eval      ctratpa = 'D'
018316070926     c                   when      x16giorno = 7
018317070926     c                   eval      ctratpa = 'S'
018318070926     c                   other
018319070926     c                   eval      ctratpa = 'F'
018320070926     c                   endsl
018321070926     c                   end
018323070921     c   12              eval      ctratpa = 'A'
018324070921     c                   eval      ctradiv = 'EUR'
018325070921     c                   eval      ctraprt = prt
018326070921     c                   write     wfctra00
018327070921     C*
018328070921     C                   ENDSR
018329070921     C**********************************************************************
018330070921     C     *inzsr        BEGSR
018331070921     C**********************************************************************
018332070921     c     *entry        plist
018400070730     c                   parm                    kpjba
018401070730     c                   movel     kpjbu         trmz91ds
018402070731     c                   move      d91dtada      dtaiso
018403070731     c                   move      dtaiso        sdtada
018404070731     c                   move      d91dtaa       dtaiso
018405070731     c                   move      dtaiso        sdtaa
018406070731      * Reperisco dati job
018407070731     c                   exsr      DatiJob
018700070730     C     Kgad          KLIST                                                  *
018800070730     C                   KFLD                    d62soco                        *
018900070730     C                   KFLD                    d62mato                        *
019000070730     C                   KFLD                    dtaiso                         *
019001070731     C     Ktab          KLIST                                                  *
019002070731     C                   KFLD                    tblkut                         *
019003070731     C                   KFLD                    tblcod                         *
019004070731     C                   KFLD                    tblkey                         *
019005070731     c                   eval      tblkut = 1
019006070731     c                   eval      tblcod = 'XN'
019007070730     c                   seton                                        of01
019400020709     C                   ENDSR
019401070731      *---------------------------------------------------------------*
019402070731      * Reperimento Dati del job (Utente/Operativi)                   *
019403070731      *---------------------------------------------------------------*
019404070731     c     DatiJob       BEGSR
019405070731      *
019406070731     c     *dtaara       define    §azute        azuteds
019407070731     c     *dtaara       define    §datiute      ddatiute
019408070731      *
019409070731     c                   in(E)     *dtaara
019410070731     c                   IF        %ERROR or RSUT = *blanks
019411070731     c                   clear                   Tibs34Ds
019412070731     c                   call      'TIBS34R'
019413070731     c                   parm                    Tibs34Ds
019414070731     c                   in        *dtaara
019415070731     c                   ENDIF
019416070731      *
019417070731     c                   ENDSR
019500070921     OQSYSPRT   E            TESTA            02
019501070731     O                       RSUT                20
019600070921     O                                         + 15 'CONTROLLO UTILIZZO AUTO'
019601070921     o                                         +  1 'AZIENDALI DA'
019602070731     O                       sdtada            +  1
019603070731     o                                         +  1 'A'
019604070731     O                       sdtaa             +  1
019900070919     O                                          113 'TRMZ92R'
020000070730     O                       UDATE              127 '  /  /  '
020100070730     O                       PAGE1         Z    132
020101070921     O          e            TESTAagi    2  2
020102070921     O                                         +  0 'CONTROLLO RIFORNIMENTO CAR'
020103070921     o                                         +  0 'BURANTE:'
020111070921     O          E            TESTAfai    2  2
020113070921     O                                         +  0 'CONTROLLO PASSAGGI AUTOSTR'
020114070921     o                                         +  0 'ADALI:'
020200070730     O          e            err01       1
020402070731     o                       tga               +  0
020403070731     o                       tgn               +  1
020404070731     O                       dataeur           +  1
020405070731     o                       d61cauo           +  1
020408070731     o                       §xndes            +  1
020409070802     o                       d62soco           +  1
020410070802     o                       d62mato       z   +  1
020500070802     O               10                        +  6 'F'
020501070802     O               12                        +  1 'A'
021000070730     O          e            err02       1
021001070731     O                                         +  0 'Anagrafica automezzo non'
021002070731     O                                         +  1 'reperita'
021003070731     o                       tga               +  1
021004070731     o                       tgn               +  1
021005070921     o                                         +  1 ' in data'
021006070921     o                       dataeur           +  1
021100070802     O          e            err03       1
021200070802     O                                         +  0 'Anagrafica autista non'
021300070802     O                                         +  1 'reperita targa'
021400070802     o                       tga               +  1
021500070802     o                       tgn               +  1
021501070921     o                                         +  1 ' in data'
021502070921     o                       dataeur           +  1
021503070920     O          e            ultagip     1
021504070921     O                                         +  0 'Periodo rilevato per rifor'
021505070921     O                                         +  0 'nimenti:'
021506070921     o                       datai             +  1
021507070921     O                                         +  1 'al'
021508070921     o                       dataf             +  1
021509070920     O          e            ultfais     1
021510070921     O                                         +  0 'Periodo rilevato per pedag'
021511070921     O                                         +  0 'gi:'
021512070921     o                       datai             +  1
021513070921     O                                         +  1 'al'
021514070921     o                       dataf             +  1
021515070921     O          e            ultgad      3
021516070921     O                                         +  0 'Periodo rilevato per rilev'
021517070921     O                                         +  0 'azione presenze:'
021518070921     o                       datai             +  1
021519070921     O                                         +  1 'al'
021520070921     o                       dataf             +  1
021600070802     O          e            fine        1
021700070802     O                                         + 50 '*** FINE STAMPA ***'

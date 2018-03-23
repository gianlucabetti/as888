000100970526     H*------------------------------------------------------------------------*
000200070124     H* stampa controllo giro aut.
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500070124     Ffiapd01l  IF   E           K DISK
000600070507     Ffiapd22l  IF   E           K DISK
000700070125     Ffiapd32l  IF   E           K DISK
000800070723     Ffiapd33l  IF   E           K DISK    rename(fiapd300:fiapd330)
000801070723     Ffiapd52l  IF   E           K DISK
000802070724     Ffiapd53l  IF   E           K DISK    rename(fiapd500:fiapd530)
000803070724     Ffigrg03l  IF   E           K DISK
000900070125     FFidg14p   O    E             PRINTER OFLIND(*IN80)
001000970526     D*------------
001100020916     D* DS REPERIMENTO DATI UTENTE
001200970526     D*------------
001300020916     D TIBS34DS      E DS                                                       *Profili utente
001400020916     D DDATIUTE      E DS                                                       *Dati utente
001500020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
001600970526     D*------------
001700970526     D* DS DI PROCEDURA
001800001020     D*------------
001900070124     D fidg14ds      e ds
002000070129     D fidg09ds      e ds
002100970526     D*------------
002200970526     D* ARCHITETTURA
002300970526     D*------------
002400970526     D KPJBA         E DS
002401070724     dcomtipa          s                   like(apd3tipa)
002402070724     dcompdr           s                   like(apd3pdr)
002403070724     dcomfpp           s                   like(apd3fpp)
002500070126     dsavpdr           s                   like(apdpdr)
002501070724     dsavpdr1          s                   like(apdpdr)
002600070126     dsavcgi           s                   like(apd3cgi)
002700070126     dsavprgg          s                   like(apd3prgg)
002800070129     dsavfgs           s                   like(apd3fgsg)
002900070126     d*avsql           s                   like(apd3sql)
003000070129     ddtaiso           s               d
003100070129     ddtaeur           s               d   datfmt(*Eur)
003200970526     C*------------------------------------------------------------------------*
003300970526     C* MAIN LINES
003400970526     C*------------------------------------------------------------------------*
003401070724     c* GIRO
003500070124     c                   if        d14ordine = 'G'
003501070724     c                   select
003502070724     c* tipo pianificazione entrambi
003503070724     c                   when      d14tp = 'E'
003600070124     c                   exsr      srcgi
003601070724     c* tipo pianificazione consegne
003602070724     c                   when      d14tp = 'C'
003603070724     c                   exsr      srcgic
003604070724     c* tipo pianificazione ritiri
003605070724     c                   when      d14tp = 'R'
003606070724     c                   exsr      srcgir
003607070724     c                   endsl
003700070124     c                   else
003701070724     c* AUTISTA
003800070124     c                   exsr      srpdr
003900070124     c                   end
004000070124     c                   seton                                        lr
004100070124     C*------------------------------------------------------------------------*
004200070129     C* x aut.
004300070124     C*------------------------------------------------------------------------*
004400070124     C     srpdr         BEGSR
004500070124     c     keypdr        setll     fiapd01l
004600070124     c                   do        *hival
004700070124     c                   if        d14pdr <> 0
004800070124     c     keypdr        reade     fiapd01l
004900070124     c                   else
005000070124     c     apdtip        reade     fiapd01l
005100070124     c                   end
005200070124     c                   if        %eof(fiapd01l)
005300070124     c                   leave
005400070124     c                   end
005500070124     C* FILTRO X FLAG ANNULLAMENTO
005600070124     C                   if        apdatb <> ' '
005700070124     C                   iter                                                   *RECORD NON VALIDO
005800070124     C                   endif
005900070124     C* p.o. di gestione
006000070129     c                   movel     apdpdr        com03             3 0
006100070129     c                   if        d14fgs <>0 and
006200070129     C                             com03 > d14fgs
006300070124     C                   leave                                                  *RECORD NON VALIDO
006400070124     C                   endif
006500070723     c* aggancio giri CONSEGNE
006501070723     c                   if        d14tp = 'C' or d14tp = 'E'
006600070124     c     keypdr        setll     fiapd32l
006700070124     c                   do        *hival
006800070124     c     keypdr        reade     fiapd32l
006900070124     c                   if        %eof(fiapd32l)
007000070124     c                   leave
007100070124     c                   end
007200070124     C* FILTRO X FLAG ANNULLAMENTO
007300070124     C                   if        apd3atb <> ' '
007400070124     C                   iter                                                   *RECORD NON VALIDO
007500070124     C                   endif
007600070124     C* FILTRO X ALLA DATA
007700070124     c                   if        d14dat <> 0
007800070124     C                   if        d14DAT >= apd3dde AND
007900070124     C                             d14DAT <= apd3dscad
008000070124     C                   else
008100070124     C                   iter                                                   *RECORD NON VALIDO
008200070124     C                   endif
008300070124     C                   endif
008400070129     C* p.o. giro
008500070129     c                   if        d14fgsg<> 0  and
008600070129     C                             apd3fgsg<> d14fgsg
008700070129     C                   iter                                                   *RECORD NON VALIDO
008800070129     C                   endif
008900070202     C* FILTRO X giro
009000070202     C                   if        d14cgi <> ' ' and d14cgi <> apd3cgi
009100070202     C                   iter                                                   *RECORD NON VALIDO
009200070202     C                   endif
009300070126     c*
009400070126     c                   if        apdpdr <>savpdr
009500070126     c                   clear                   savprgg
009700070126     c                   end
009800070126     c*
009900070129     c                   if        com03 <> savfgs and savfgs <> 0
010000070129     c                   seton                                        80
010100070126     c                   end
010200070129     c                   eval      savfgs = com03
010300070129     c*
010400070201     c                   if        not *in80
010500070129     c                   if        apdpdr = savpdr and savpdr <> 0
010600070724     c                   seton                                        1013
010700070129     c                   end
010800070126     c*
010900070126     c                   if        apd3prgg= savprgg and savprgg<> 0
011000070126     c                   seton                                        11
011100070126     c                   end
011200070126     c*
011600070201     c                   end
011700070201     c                   eval      savpdr = apdpdr
011800070201     c                   eval      savprgg= apd3prgg
012000070124     c* aggancio anagrafico giro
012001070723     c                   clear                   fidg09ds
012002070723     c                   eval      d09itug = 'C'
012003070723     c                   eval      d09idat = apd3dde
012004070723     c                   eval      d09ifgs = apd3fgsg
012005070723     c                   eval      d09icgi = apd3cgi
012100070125     c                   exsr      sranacgi
012200070125     c* stampo
012300070124     c                   seton                                        01
012400070724     c                   exsr      srfldstpc
012401070724     c                   eval      comtipa=apd3tipa
012402070724     c                   eval      compdr=apd3pdr
012403070724     c                   eval      comfpp=apd3fpp
012404070724     c                   eval      prttp='C'
012405070724     c                   exsr      stampa
012500070124     c                   enddo
012501070723     c                   end
012502070723     c* aggancio giri RITIRI
012503070723     c                   if        d14tp = 'R' or d14tp = 'E'
012504070724     c                   clear                   savpdr1
012505070723     c     keypdr        setll     fiapd52l
012506070723     c                   do        *hival
012507070723     c     keypdr        reade     fiapd52l
012508070723     c                   if        %eof(fiapd52l)
012509070723     c                   leave
012510070723     c                   end
012511070723     C* FILTRO X FLAG ANNULLAMENTO
012512070723     C                   if        apd5atb <> ' '
012513070723     C                   iter                                                   *RECORD NON VALIDO
012514070723     C                   endif
012515070723     C* FILTRO X ALLA DATA
012516070723     c                   if        d14dat <> 0
012517070723     C                   if        d14DAT >= apd5dde AND
012518070723     C                             d14DAT <= apd5dscad
012519070723     C                   else
012520070723     C                   iter                                                   *RECORD NON VALIDO
012521070723     C                   endif
012522070723     C                   endif
012523070723     C* p.o. giro
012524070723     c                   if        d14fgsg<> 0  and
012525070723     C                             apd5fgsg<> d14fgsg
012526070723     C                   iter                                                   *RECORD NON VALIDO
012527070723     C                   endif
012528070723     C* FILTRO X giro
012529070723     C                   if        d14cgi <> ' ' and d14cgi <> apd5cgi
012530070723     C                   iter                                                   *RECORD NON VALIDO
012531070723     C                   endif
012532070723     c*
012533070723     c                   if        apdpdr <>savpdr
012534070723     c                   clear                   savprgg
012535070723     c                   end
012536070723     c*
012537070723     c                   if        com03 <> savfgs and savfgs <> 0
012538070723     c                   seton                                        80
012539070723     c                   end
012540070723     c                   eval      savfgs = com03
012541070723     c*
012542070723     c                   if        not *in80
012543070723     c                   if        apdpdr = savpdr and savpdr <> 0
012544070724     c                   seton                                        10
012545070723     c                   end
012546070724     c                   if        apdpdr = savpdr1 and savpdr1 <> 0
012547070724     c                   seton                                        13
012548070724     c                   end
012549070723     c*
012550070723     c                   if        apd5prgg= savprgg and savprgg<> 0
012551070723     c                   seton                                        11
012552070723     c                   end
012553070723     c*
012554070723     c                   end
012555070723     c                   eval      savpdr = apdpdr
012556070724     c                   eval      savpdr1= apdpdr
012557070723     c                   eval      savprgg= apd5prgg
012558070723     c* aggancio anagrafico giro
012559070723     c                   clear                   fidg09ds
012560070723     c                   eval      d09itug = 'R'
012561070723     c                   eval      d09idat = apd5dde
012562070723     c                   eval      d09ifgs = apd5fgsg
012563070723     c                   eval      d09icgi = apd5cgi
012564070723     c                   exsr      sranacgi
012565070723     c* stampo
012566070723     c                   seton                                        01
012567070724     c                   exsr      srfldstpr
012568070724     c                   eval      comtipa=apd5tipa
012569070724     c                   eval      compdr=apd5pdr
012570070724     c                   eval      comfpp=apd5fpp
012571070724     c                   eval      prttp='R'
012572070723     c                   exsr      stampa
012573070723     c                   enddo
012574070723     c                   end
012600070124     c                   enddo
012700070124     c*
012800070124     c                   endsr
012801070724     C*------------------------------------------------------------------------*
012802070724     C* x giro ritiri
012803070724     C*------------------------------------------------------------------------*
012804070724     C     srcgir        BEGSR
012805070724     c                   if        d14cgi <> ' '
012806070724     c     keycgi        setll     fiapd53l
012807070724     c                   else
012808070724     c                   if        d14fgsg<> 0
012809070724     c     d14fgsg       setll     fiapd53l
012810070724     c                   else
012811070724     c     *loval        setll     fiapd53l
012812070724     c                   end
012813070724     c                   end
012814070724     c                   do        *hival
012815070724     c                   if        d14cgi <> ' '
012816070724     c     keycgi        reade     fiapd53l
012817070724     c                   else
012818070724     c                   if        d14fgsg<> 0
012819070724     c     d14fgsg       reade     fiapd53l
012820070724     c                   else
012821070724     c                   read      fiapd53l
012822070724     c                   end
012823070724     c                   end
012824070724     c                   if        %eof(fiapd53l)
012825070724     c                   leave
012826070724     c                   end
012827070724     C* FILTRO X FLAG ANNULLAMENTO
012828070724     C                   if        apd5atb <> ' '
012829070724     C                   iter                                                   *RECORD NON VALIDO
012830070724     C                   endif
012831070724     C* FILTRO X aut
012832070724     C                   if        d14pdr <> 0 and d14pdr <> apd5pdr
012833070724     C                   iter                                                   *RECORD NON VALIDO
012834070724     C                   endif
012835070724     C* FILTRO X ALLA DATA
012836070724     c                   if        d14dat <> 0
012837070724     C                   if        d14DAT >= apd5dde AND
012838070724     C                             d14DAT <= apd5dscad
012839070724     C                   else
012840070724     C                   iter                                                   *RECORD NON VALIDO
012841070724     C                   endif
012842070724     C                   endif
012843070724     c* aggancio anagrafico aut
012844070724     c                   eval      apdpdr = apd5pdr
012845070724     c     keypdr        chain     fiapd01l
012846070724     c                   if        not %found(fiapd01l)
012847070724     c                             or (%found(fiapd01l) and apdatb<> ' ')
012848070724     c                   iter
012849070724     c                   end
012850070724     c*
012851070724     c                   if        apd5cgi <>savcgi
012852070724     c                   clear                   savprgg
012853070724     c*                  clear                   savsql
012854070724     c                   end
012855070724     c*
012856070724     c                   if        apd5fgsg<>savfgs and savfgs <> 0
012857070724     c                   seton                                        80
012858070724     c                   end
012859070724     c                   eval      savfgs = apd5fgsg
012860070724     c*
012861070724     c                   if        not *in80
012862070724     c                   if        apd3cgi = savcgi and savcgi <> ' '
012863070724     c                   seton                                        10
012864070724     c                   end
012865070724     c*
012866070724     c                   if        apd5prgg= savprgg and savprgg<> 0
012867070724     c                   seton                                        11
012868070724     c                   end
012869070724     c*
012870070724     c*                  if        apd5sql = savsql and savsql <> 0
012871070724     c*                  seton                                        12
012872070724     c*                  end
012873070724     c                   end
012874070724     c                   eval      savcgi = apd5cgi
012875070724     c                   eval      savprgg= apd5prgg
012876070724     c*                  eval      savsql = apd5sql
012877070724     c* aggancio anagrafico giri
012878070724     c                   clear                   fidg09ds
012879070724     c                   eval      d09itug = 'R'
012880070724     c                   eval      d09idat = apd5dde
012881070724     c                   eval      d09ifgs = apd5fgsg
012882070724     c                   eval      d09icgi = apd5cgi
012883070724     c                   exsr      sranacgi
012884070724     c* stampo
012885070724     c                   seton                                        02
012886070724     c                   exsr      srfldstpr
012887070724     c                   eval      comtipa=apd5tipa
012888070724     c                   eval      compdr=apd5pdr
012889070724     c                   eval      comfpp=apd5fpp
012890070724     c                   eval      prttp='R'
012891070724     c                   exsr      stampa
012892070724     c                   enddo
012893070724     c*
012894070724     c                   endsr
012895070724     C*------------------------------------------------------------------------*
012896070724     C* x giro consegne
012897070724     C*------------------------------------------------------------------------*
012898070724     C     srcgic        BEGSR
012899070724     c                   if        d14cgi <> ' '
012900070724     c     keycgi        setll     fiapd33l
012901070724     c                   else
012902070724     c                   if        d14fgsg<> 0
012903070724     c     d14fgsg       setll     fiapd33l
012904070724     c                   else
012905070724     c     *loval        setll     fiapd33l
012906070724     c                   end
012907070724     c                   end
012908070724     c                   do        *hival
012909070724     c                   if        d14cgi <> ' '
012910070724     c     keycgi        reade     fiapd33l
012911070724     c                   else
012912070724     c                   if        d14fgsg<> 0
012913070724     c     d14fgsg       reade     fiapd33l
012914070724     c                   else
012915070724     c                   read      fiapd33l
012916070724     c                   end
012917070724     c                   end
012918070724     c                   if        %eof(fiapd33l)
012919070724     c                   leave
012920070724     c                   end
012921070724     C* FILTRO X FLAG ANNULLAMENTO
012922070724     C                   if        apd3atb <> ' '
012923070724     C                   iter                                                   *RECORD NON VALIDO
012924070724     C                   endif
012925070724     C* FILTRO X aut
012926070724     C                   if        d14pdr <> 0 and d14pdr <> apd3pdr
012927070724     C                   iter                                                   *RECORD NON VALIDO
012928070724     C                   endif
012929070724     C* FILTRO X ALLA DATA
012930070724     c                   if        d14dat <> 0
012931070724     C                   if        d14DAT >= apd3dde AND
012932070724     C                             d14DAT <= apd3dscad
012933070724     C                   else
012934070724     C                   iter                                                   *RECORD NON VALIDO
012935070724     C                   endif
012936070724     C                   endif
012937070724     c* aggancio anagrafico aut
012938070724     c                   eval      apdpdr = apd3pdr
012939070724     c     keypdr        chain     fiapd01l
012940070724     c                   if        not %found(fiapd01l)
012941070724     c                             or (%found(fiapd01l) and apdatb<> ' ')
012942070724     c                   iter
012943070724     c                   end
012944070724     c*
012945070724     c                   if        apd3cgi <>savcgi
012946070724     c                   clear                   savprgg
012947070724     c*                  clear                   savsql
012948070724     c                   end
012949070724     c*
012950070724     c                   if        apd3fgsg<>savfgs and savfgs <> 0
012951070724     c                   seton                                        80
012952070724     c                   end
012953070724     c                   eval      savfgs = apd3fgsg
012954070724     c*
012955070724     c                   if        not *in80
012956070724     c                   if        apd3cgi = savcgi and savcgi <> ' '
012957070724     c                   seton                                        10
012958070724     c                   end
012959070724     c*
012960070724     c                   if        apd3prgg= savprgg and savprgg<> 0
012961070724     c                   seton                                        11
012962070724     c                   end
012963070724     c*
012964070724     c*                  if        apd3sql = savsql and savsql <> 0
012965070724     c*                  seton                                        12
012966070724     c*                  end
012967070724     c                   end
012968070724     c                   eval      savcgi = apd3cgi
012969070724     c                   eval      savprgg= apd3prgg
012970070724     c*                  eval      savsql = apd3sql
012971070724     c* aggancio anagrafico giri
012972070724     c                   clear                   fidg09ds
012973070724     c                   eval      d09itug = 'C'
012974070724     c                   eval      d09idat = apd3dde
012975070724     c                   eval      d09ifgs = apd3fgsg
012976070724     c                   eval      d09icgi = apd3cgi
012977070724     c                   exsr      sranacgi
012978070724     c* stampo
012979070724     c                   seton                                        02
012980070724     c                   exsr      srfldstpc
012981070724     c                   eval      comtipa=apd3tipa
012982070724     c                   eval      compdr=apd3pdr
012983070724     c                   eval      comfpp=apd3fpp
012984070724     c                   eval      prttp='C'
012985070724     c                   exsr      stampa
012986070724     c                   enddo
012987070724     c*
012988070724     c                   endsr
012989070124     C*------------------------------------------------------------------------*
013000070124     C* x giro
013100070124     C*------------------------------------------------------------------------*
013200070124     C     srcgi         BEGSR
013300070124     c                   if        d14cgi <> ' '
013400070724     c     keycgi        setll     figrg03l
013500070124     c                   else
013600070129     c                   if        d14fgsg<> 0
013700070724     c     d14fgsg       setll     figrg03l
013800070129     c                   else
013900070724     c     *loval        setll     figrg03l
014000070124     c                   end
014100070129     c                   end
014200070124     c                   do        *hival
014300070124     c                   if        d14cgi <> ' '
014400070724     c     keycgi        reade     figrg03l
014500070124     c                   else
014600070129     c                   if        d14fgsg<> 0
014700070724     c     d14fgsg       reade     figrg03l
014800070129     c                   else
014900070724     c                   read      figrg03l
015000070124     c                   end
015100070129     c                   end
015200070724     c                   if        %eof(figrg03l)
015300070124     c                   leave
015400070126     c                   end
015401070724     C* FILTRO X FLAG ANNULLAMENTO
015402070724     C                   if        grgatb <> ' '
015403070724     C                   iter                                                   *RECORD NON VALIDO
015404070724     C                   endif
015406070724     c* CONSEGNE
015407070724     c     keygrg        setll     fiapd33l
015410070724     c                   do        *hival
015412070724     c     keygrg        reade     fiapd33l
015420070724     c                   if        %eof(fiapd33l)
015421070724     c                   leave
015422070724     c                   end
015500070124     C* FILTRO X FLAG ANNULLAMENTO
015600070724     C                   if        apd3atb <> ' '
015700070124     C                   iter                                                   *RECORD NON VALIDO
015800070124     C                   endif
015900070202     C* FILTRO X aut
016000070202     C                   if        d14pdr <> 0 and d14pdr <> apd3pdr
016100070202     C                   iter                                                   *RECORD NON VALIDO
016200070202     C                   endif
016300070124     C* FILTRO X ALLA DATA
016400070124     c                   if        d14dat <> 0
016500070124     C                   if        d14DAT >= apd3dde AND
016600070124     C                             d14DAT <= apd3dscad
016700070124     C                   else
016800070124     C                   iter                                                   *RECORD NON VALIDO
016900070124     C                   endif
017000070124     C                   endif
017100070124     c* aggancio anagrafico aut
017200070124     c                   eval      apdpdr = apd3pdr
017300070124     c     keypdr        chain     fiapd01l
017400070124     c                   if        not %found(fiapd01l)
017500070124     c                             or (%found(fiapd01l) and apdatb<> ' ')
017600070124     c                   iter
017700070124     c                   end
017800070126     c*
017900070126     c                   if        apd3cgi <>savcgi
018000070126     c                   clear                   savprgg
018200070126     c                   end
018300070126     c*
018400070129     c                   if        apd3fgsg<>savfgs and savfgs <> 0
018500070129     c                   seton                                        80
018600070126     c                   end
018700070129     c                   eval      savfgs = apd3fgsg
018800070129     c*
018900070201     c                   if        not *in80
019000070129     c                   if        apd3cgi = savcgi and savcgi <> ' '
019100070129     c                   seton                                        10
019200070129     c                   end
019300070126     c*
019400070126     c                   if        apd3prgg= savprgg and savprgg<> 0
019500070126     c                   seton                                        11
019600070126     c                   end
019700070126     c*
020100070201     c                   end
020200070201     c                   eval      savcgi = apd3cgi
020300070201     c                   eval      savprgg= apd3prgg
020400070201     c*                  eval      savsql = apd3sql
020500070125     c* aggancio anagrafico giri
020501070723     c                   clear                   fidg09ds
020502070723     c                   eval      d09itug = 'C'
020503070723     c                   eval      d09idat = apd3dde
020504070723     c                   eval      d09ifgs = apd3fgsg
020505070723     c                   eval      d09icgi = apd3cgi
020600070125     c                   exsr      sranacgi
020700070125     c* stampo
020800070124     c                   seton                                        02
020801070724     c                   exsr      srfldstpc
020802070724     c                   eval      comtipa=apd3tipa
020803070724     c                   eval      compdr=apd3pdr
020804070724     c                   eval      comfpp=apd3fpp
020805070724     c                   eval      prttp='C'
020900070124     c                   exsr      stampa
021000070124     c                   enddo
021001070724     c* RITIRI
021002070724     c                   clear                   savprgg
021003070724     c     keygrg        setll     fiapd53l
021004070724     c                   do        *hival
021005070724     c     keygrg        reade     fiapd53l
021006070724     c                   if        %eof(fiapd53l)
021007070724     c                   leave
021008070724     c                   end
021009070724     C* FILTRO X FLAG ANNULLAMENTO
021010070724     C                   if        apd5atb <> ' '
021011070724     C                   iter                                                   *RECORD NON VALIDO
021012070724     C                   endif
021013070724     C* FILTRO X aut
021014070724     C                   if        d14pdr <> 0 and d14pdr <> apd5pdr
021015070724     C                   iter                                                   *RECORD NON VALIDO
021016070724     C                   endif
021017070724     C* FILTRO X ALLA DATA
021018070724     c                   if        d14dat <> 0
021019070724     C                   if        d14DAT >= apd5dde AND
021020070724     C                             d14DAT <= apd5dscad
021021070724     C                   else
021022070724     C                   iter                                                   *RECORD NON VALIDO
021023070724     C                   endif
021024070724     C                   endif
021025070724     c* aggancio anagrafico aut
021026070724     c                   eval      apdpdr = apd5pdr
021027070724     c     keypdr        chain     fiapd01l
021028070724     c                   if        not %found(fiapd01l)
021029070724     c                             or (%found(fiapd01l) and apdatb<> ' ')
021030070724     c                   iter
021031070724     c                   end
021032070724     c*
021033070724     c                   if        apd5cgi <>savcgi
021034070724     c                   clear                   savprgg
021035070724     c                   end
021036070724     c*
021037070724     c                   if        apd5fgsg<>savfgs and savfgs <> 0
021038070724     c                   seton                                        80
021039070724     c                   end
021040070724     c                   eval      savfgs = apd5fgsg
021041070724     c*
021042070724     c                   if        not *in80
021043070724     c                   if        apd5cgi = savcgi and savcgi <> ' '
021044070724     c                   seton                                        10
021045070724     c                   end
021046070724     c*
021047070724     c                   if        apd5prgg= savprgg and savprgg<> 0
021048070724     c                   seton                                        11
021049070724     c                   end
021050070724     c*
021051070724     c                   end
021052070724     c                   eval      savcgi = apd5cgi
021053070724     c                   eval      savprgg= apd5prgg
021054070724     c*                  eval      savsql = apd5sql
021055070724     c* aggancio anagrafico giri
021056070724     c                   clear                   fidg09ds
021057070829     c                   eval      d09itug = 'R'
021058070724     c                   eval      d09idat = apd5dde
021059070724     c                   eval      d09ifgs = apd5fgsg
021060070724     c                   eval      d09icgi = apd5cgi
021061070724     c                   exsr      sranacgi
021062070724     c* stampo
021063070724     c                   seton                                        02
021064070724     c                   exsr      srfldstpr
021065070724     c                   eval      comtipa=apd5tipa
021066070724     c                   eval      compdr=apd5pdr
021067070724     c                   eval      comfpp=apd5fpp
021068070724     c                   eval      prttp='R'
021069070724     c                   exsr      stampa
021070070724     c                   enddo
021071070724     c                   enddo
021100070124     c*
021200070124     c                   endsr
021300070124     C*------------------------------------------------------------------------*
021400070124     C* decodifico il giro
021500070124     C*------------------------------------------------------------------------*
021600070125     C     sranacgi      BEGSR
021700070124     c*
021900070129     c                   eval      D09IOP0 = '001'
022300070129     c                   eval      d09iain = d14ain
022500070129     c                   eval      kpjbu = fidg09ds
022600070129     c                   call      'FIDG09R'
022700070129     c                   parm                    kpjba
022800070129     c                   eval      fidg09ds = kpjbu
022900070129     c                   if        d09oerr = '1'
023000070129     c                   eval      prtdcgi = *all'?'
023100070129     c                   eval      prtain = *all'?'
023200070129     c                   else
023300070129     c                   eval      prtdcgi = d09odes
023400070129     c                   eval      prtain = d09oain
023500070129     c                   end
023600070124     c*
023700070124     c                   endsr
023701070724     C*------------------------------------------------------------------------*
023703070724     c* campi in stampa
023704070724     C*------------------------------------------------------------------------*
023705070724     C     srfldstpc     BEGSR
023708070724     c                   eval      prtfgsg= apd3fgsg
023709070724     c                   eval      prtcgi = apd3cgi
023710070724     c                   eval      prtfpp = apd3fpp
023711070724     c                   eval      prtprgg= apd3prgg
023712070724     c                   eval      prtsql = apd3sql
023713070724     c                   endsr
023714070724     C*------------------------------------------------------------------------*
023715070724     c* campi in stampa
023716070724     C*------------------------------------------------------------------------*
023717070724     C     srfldstpr     BEGSR
023720070724     c                   eval      prtfgsg= apd5fgsg
023721070724     c                   eval      prtcgi = apd5cgi
023722070724     c                   eval      prtfpp = apd5fpp
023723070724     c                   eval      prtprgg= apd5prgg
023724070724     c                   eval      prtsql = apd5sql
023725070724     c                   endsr
023800070124     C*------------------------------------------------------------------------*
023900070124     C* stampa
024000070124     C*------------------------------------------------------------------------*
024100070124     C     stampa        BEGSR
024200070507      *
024300070507     c* Aggancia estensione anagrafica x MAX.pesi volumi fermate
024400070507     c                   exsr      srapd22
024401070724     c                   eval      prtpdr = apdpdr
024402070724     c                   eval      prtdsf = apdrsc
025300070724     c                   if        ok_APD22 = 'S'
025301070724     c                   if        prttp = 'C'
025400070507     c                   eval      prtpkl = apd2pkl
025500070507     c                   eval      prtvlu = apd2vlu
025600070507     c                   eval      prtnftl= apd2nftl
025601070724     c                   else
025602070724     c                   eval      prtpkl = apd2rpkl
025603070724     c                   eval      prtvlu = apd2rvlu
025604070724     c                   eval      prtnftl= apd2rnftl
025605070724     c                   end
025700070507     c                   else
025800070507     c                   clear                   prtpkl
025900070507     c                   clear                   prtvlu
026000070507     c                   clear                   prtnftl
026100070507     c                   end
026200070124     c*
026300070124     c                   if        d14ordine = 'G'
026400070126     c   80              write     testag
026500070124     c                   else
026600070126     c   80              write     testaa
026700070124     c                   end
026800070125     c                   setoff                                       80
026900070126     c  n10              write     riga
027000070126     c   01              write     rigaa
027100070126     c   02              write     rigag
027200070724     c                   setoff                                       010213
027300070202     c                   setoff                                       101112
027400070124     c*
027500070124     c                   endsr
027600070124     C*------------------------------------------------------------------------*
027601070724     C     srapd22       BEGSR
027800070507     C*------------------------------------------------------------------------*
028000070507     c*
028100070507     c                   clear                   ok_APD22          1
028200070507      *
028300070507     C     KEYpdr2       setll     fiapd22l
028400070507     c                   do        *hival
028500070507     C     KEYpdr2       reade     fiapd22l
028600070507     c                   if        %Eof(fiapd22l)
028700070507     c                   Leave
028800070507     c                   end
028900070507      * se il record è interno ai parametri quindi OK
029000070507     C                   if        d14DAT >= apd2dde AND
029100070507     C                             d14DAT <= apd2dscad
029200070507     c                   eval      ok_APD22 = 'S'
029300070507     c                   Leave
029400070507     c                   end
029500070507      *
029600070507     c                   endDo
029700070507     c*
029800070507     c                   endsr
029900070507     C*------------------------------------------------------------------------*
030000070124     C* *INZSR - OPERAZIONI INIZIALI
030100070124     C*------------------------------------------------------------------------*
030200070124     C     *INZSR        BEGSR
030300970526     C*---
030400970526     C* RICEVIMENTO PARAMETRI
030500970526     C*---
030600970526     C     *ENTRY        PLIST
030700970612     C                   PARM                    KPJBA
030800070122     C*
030900070122     C                   IF        KPJBU <> *blanks
031000070124     C                   MOVEL     KPJBU         fidg14ds
031100070124     c                   else
031200070124     c                   clear                   fidg14ds
031300070124     c                   end
031400070126     c                   eval      nompgm = 'FIDG14R'
031500070126     C* REPERIMENTO DATI UTENTE
031600070126     C                   EXSR      REPDATIUTE
031700070124     C* controllo PARAMETRI DI INGRESSO
031800070126     c                   if        d14fgs = 0 and dutpou <> 046
031900070126     c                   eval      d14fgs = dutpou
032000070124     c                   end
032100070129     c                   if        d14fgsg= 0 and dutpou <> 046
032200070129     c                   eval      d14fgsg= dutpou
032300070129     c                   end
032400070124     c                   if        d14pdr = 0
032500070126     c                   if        dutpou <> 046
032600070126     c                   movel     dutpou        apdpdr
032700070126     c                   end
032800070124     c                   else
032900070124     c                   move      d14pdr        apdpdr
033000070124     c                   end
033100070129     c                   move      d14dat        dtaiso
033200070129     c                   move      dtaiso        dtaeur
033300070129     c                   move      dtaeur        prtdat
033400070125     C* fiapd01l
033500070129     C     KEYcgi        KLIST
033600070129     C                   KFLD                    d14fgsg
033700070129     C                   KFLD                    d14cgi
033701070724     C     KEYgrg        KLIST
033702070724     C                   KFLD                    grgfgs
033703070724     C                   KFLD                    grgcgi
033800070129     C     KEYpdr        KLIST
033900070129     C                   KFLD                    apdtip
034000070129     C                   KFLD                    apdpdr
034100070124     c                   eval      apdtip = 'A'
034200070507      *
034300070507     C     KEYpdr2       KLIST
034400070724     C                   KFLD                    comTIPA
034500070724     C                   KFLD                    comPDR
034600070724     c                   KFLD                    comFPP
034700070507     c*
034800070125     c                   seton                                        80
034900070126     C                   MOVEL     *ALL'-'       CMP132
035000070125     C                   TIME                    TIMES             6 0
035100070125     C                   TIME                    WHDAT            12 0
035200070125     C                   MOVE      WHDAT         WOGGI             6 0
035300070124     c*
035400070119     C                   ENDSR
035500070124     C*--------------------------------------------------------------------------------------------*
035600070124     C* REPERISCE I DATI UTENTE
035700070124     C*--------------------------------------------------------------------------------------------*
035800070124     C     REPDATIUTE    BEGSR
035900070124     C*
036000070124     C* INIZIALIZZA VARIABILI DI WRK
036100070124     C                   CLEAR                   TIBS34DS
036200070124     C                   CLEAR                   AZUTEDS
036300070124     C                   CLEAR                   DDATIUTE
036400070124     C*
036500070124     C     *DTAARA       DEFINE    §azute        azuteds
036600070124     C     *DTAARA       DEFINE    §datiute      ddatiute
036700070124     C                   IN(E)     *DTAARA
036800070124if  1C                   IF        %Error
036900070124     c                   EVAL      I34Tla = 'L'
037000070124     C                   CALL      'TIBS34R'
037100070124     C                   PARM                    Tibs34Ds
037200070124     C                   IN        *DTAARA
037300070124e   1C                   ENDIF
037400070124     C*
037500070124     C                   ENDSR

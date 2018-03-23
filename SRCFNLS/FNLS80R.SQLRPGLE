000100031008     h Decedit('0,') Datedit(*dmy.) Option(*nodebugio)
000200031007      *---------------------------------------------------------------*
000300031007      * Visualizzazione totali VAB x cliente
000400031007      *---------------------------------------------------------------*
000500031007     fFnls80d   cf   e             Workstn    Sfile(Ls80s01:nrr)
000600071219     fFivax01l  if   e           k Disk
000700031007
000800161118     d sav_VabFGS      s                   Like(VabFGS) Inz
000900161118     d sav_VabCcm      s                   Like(VabCcm) Inz
001000031008     d sav_VabNsr      s                   Like(VabNrs) Inz
001100071219     d stringasql      s           1100    varying
001200031008     d VabMgsAlfa      s              4
001300071219     d xx              s              3  0
001400071219     d wvad            s              5  0 inz
001500071219     d wvat            s              5  0 inz
001600091016     d wrkvad          s             10  0 inz
001700091016     d wrkvat          s             10  0 inz
001800091110     d $anomalo        s              1n
001900031008
002000031008     d sav_VabMgs      ds             4
002100031008     d  VabMese                1      2
002200031008     d  VabGiorno              3      4
002300031008
002400031007     d Fnls80ds      e ds
002500031008     d  Pog                    1     90  0 Dim(30)
002600031007
002700071219     d fivabds       e ds                  extname(fivab00f)
002800071219     d fivadds       e ds                  extname(fivad00f)
002900071219     d fivatds       e ds                  extname(fivat00f)
003000071219     d fivaxds       e ds                  extname(fivax00f)
003100071219
003200031008     d Tibs69ds      e ds
003300031008     d ds_cnaco      e ds                  Extname(Cnaco00f)
003400031008     d ds_cnind      e ds                  Extname(Cnind00f)
003500031008     d ds_cnclp      e ds                  Extname(Cnclp00f)
003600031008     d ds_fncls      e ds                  Extname(Fncls00f)
003700031007     d Tibs34ds      e ds                  Inz
003800031007     d dDatiute      e ds
003900031007     d Azuteds       e ds                  Extname(Azute00f)
004000031008     d Kpjba         e ds
004100031007
004200031009     d Testo           c                   Const('** CONFERMA BOLLE  DA CLIENTI-
004300031009     d                                      **')
004400031009     d Testo1          c                   Const('*  RIEPILOGO  *')
004500161128     d c_AZORG         c                   Const('inner join azorg00f on vabfgs-
004600161128     d                                     =orgfil and orgfva = '' '' and orgfa-
004700161128     d                                     g in (''F'' , ''A'') ')
004800031007      *---------------------------------------------------------------*
004900031007      *  RIEPILOGO INDICATORI
005000031007      *---------------------------------------------------------------*
005100031008      *  01  On   Ho letto almeno 1 rcd in FIVAB
005200031007      *  20  On   SFLEND
005300031007      *  21  Off  SFLDSP
005400031007      *  21  Off  SFLDSPCTL
005500031007      *  21  On   SFLCLR
005600031007      *---------------------------------------------------------------*
005700031007
005800071219     c/exec sql
005900071219     c+ set option dynusrprf = *owner, closqlcsr = *endmod
006000071219     c/end-exec
006100071219
006200071219      *         C A R I C A M E N T O   S T R I N G A   S Q L
006300071219
006400071219     c                   exsr      sr_stringasql
006500031007
006600031007      *         P U L I Z I A   S U B F I L E
006700031007
006800031007     c                   ExSr      Sr_PulSfl
006900031007
007000031007      *         C A R I C A M E N T O   S U B F I L E
007100031007
007200031007     c                   ExSr      Sr_CarSfl
007300031007
007400031007      *         E M I S S I O N E   S U B F I L E
007500031007
007600031007     c                   ExSr      Sr_EmiSfl
007700031007
007800031007     c                   Eval      *InLr = *On
007900071219
008000071219      *---------------------------------------------------------------*
008100071219      * CARICAMENTO STRINGA SQL
008200071219      *---------------------------------------------------------------*
008300071219     c     sr_stringasql begsr
008400071219
008500071219     c                   clear                   stringasql
008600071219
008700071219      * preparo la stringa sql
008800161118     c                   eval      stringasql =
008900161118     c                             'select fivab00f.* from '                  +
009000161118     c                             'fivab00f '
009100161121      * se sono richieste le bolle di tutti i clienti, mostro tutti quelli delle
009200161121      * filiali che l'utente può vedere
009300161121     c                   if        d80ccm = 0
009400161118     c                   eval      stringasql = %trim(stringasql) +
009500161118     c                             ' where vabfgs in ('
009600071219      * ciclo sulla sk p.o. da elaborare per i p.o. emissione
009700071219     c                   do        30            xx
009800071219     c                   if        pog(xx) = 0
009900071219     c                   leave
010000071219     c                   endif
010100071219     c                   eval      stringasql =
010200071219     c                             %trim(stringasql) +  ' '                    +
010300071219     c                             %editc(pog(xx):'1') + ','
010400071219     c                   enddo
010500071219     c                   eval      stringasql =
010600071219     c                             %trim(stringasql) + ')'
010700161121
010800161121      * se sono richieste le bolle di un solo cliente, mostro lui in tutte le
010900161121      * filiali esistenti
011000161121     c                   else
011100161128      * effettuo una innerjoin con AZORG00F per evitare di leggere rcd con
011200161128      * filiali fittizie
011300161121     c                   eval      stringasql = %trim(stringasql) +
011400161128     c                             ' ' + c_AZORG
011500161128     c                   eval      stringasql = %trim(stringasql) +
011600161121     c                             ' where vabfgs = vabfgs'
011700161121     c                   endif
011800071219
011900071219     c                   select
012000071219
012100071219      * richiesto: cliente e serie
012200071219     c                   when      d80ccm <> 0 and
012300071219     c                             d80nsr <> 99
012400071219     c                   eval      stringasql =
012500071219     c                             %trim(stringasql) + ' '                     +
012600071219     c                             'and vabccm = ' + %editc(d80ccm:'X') + ' '  +
012700071219     c                             'and vabnrs = ' + %editc(d80nsr:'X') + ' '
012800071219      * richiesto: cliente
012900071219     c                   when      d80ccm <> 0 and
013000071219     c                             d80nsr = 99
013100071219     c                   eval      stringasql =
013200071219     c                             %trim(stringasql) + ' '                     +
013300071219     c                             'and vabccm = ' + %editc(d80ccm:'X') + ' '
013400071219      * richiesto: serie
013500071219     c                   when      d80nsr <> 99
013600071219     c                   eval      stringasql =
013700071219     c                             %trim(stringasql) + ' '                     +
013800071219     c                             'and vabnrs = ' + %editc(d80nsr:'X') + ' '
013900071219
014000071219     c                   endsl
014100071219
014200071219      * richiesta la linea di arrivo
014300071219     c                   if        d80lna <> 999
014400071219     c                   eval      stringasql =
014500071219     c                             %trim(stringasql) + ' '                     +
014600071219     c                             'and vablna = ' + %editc(d80lna:'X') + ' '
014700071219     c                   endif
014800071219      * ordino il file
014900161121      * se tutti i clienti, allora l'ordinamento non deve prevedere la filiale
015000161121      * segnacollo
015100161121     c                   if        d80ccm = 0
015200071219     c                   eval      stringasql =
015300071219     c                             %trim(stringasql) + ' '                     +
015400161121     c                             'order by vabccm, vabnrs, vabmgs'
015500161121      * se un solo cliente, allora l'ordinamento prevede la filiale
015600161121      * segnacollo
015700161121     c                   else
015800161121     c                   eval      stringasql =
015900161121     c                             %trim(stringasql) + ' '                     +
016000161121     c                             'order by vabfgs, vabccm, vabnrs, vabmgs'
016100161121     c                   endif
016200071219
016300071219     c                   endsr
016400031007
016500031007      *---------------------------------------------------------------*
016600031007      * PULIZIA SUBFILE
016700031007      *---------------------------------------------------------------*
016800031007     c     Sr_PulSfl     BegSr
016900031007
017000031007     c                   Clear                   Nrr               4 0
017100031007     c                   Eval      *In21 = *On
017200161121     c                   setoff                                       22
017300161121     c* se richiesti tutti i clienti
017400161121     c                   if        d80ccm = 0
017500161121     c* mostro il sflctl senza la colonna Fil
017600161121     c                   seton                                        22
017700161121     c                   endif
017800031007     c                   Write     Ls80c01
017900031007     c                   Eval      *In20 = *On
018000031007     c                   Eval      *In21 = *Off
018100091110
018200091110     c                   eval      $anomalo = *off
018300031007
018400031007     c                   EndSr
018500031007
018600031007      *---------------------------------------------------------------*
018700031007      * CARICAMENTO SUBFILE
018800031007      *---------------------------------------------------------------*
018900031007     c     Sr_CarSfl     BegSr
019000031007
019100050207     c                   Eval      *In01 = *Off
019200031007
019300071219     c/exec sql
019400071219     c+ prepare s1 from :stringasql
019500071219     c/end-exec
019600071219
019700071219     c/exec sql
019800071219     c+ declare a1 cursor for s1
019900071219     c/end-exec
020000071219
020100071219     c/exec sql
020200071219     c+ open a1
020300071219     c/end-exec
020400071219
020500071219     c                   do        *hival
020600071219
020700071219     c/exec sql
020800071219     c+ fetch next from a1 into :fivabds
020900071219     c/end-exec
021000071219
021100071219     c                   if        sqlcod = 100 or
021200071219     c                             sqlcod < 0
021300071219     c                   leave
021400071219     c                   endif
021500071219
021600031007      * Carico i dati
021601170901     c                   if        vabatb<>'A'
021700031007     c                   ExSr      Sr_Cardati
021800170901     c                   endif
021801170901     c
021900071219     c                   enddo
022000071219
022100071219     c/exec sql
022200071219     c+ close a1
022300071219     c/end-exec
022400071219
022500071219      * Scrivo l'ultimo record
022600071219     c   01              ExSr      Sr_Ultimo
022700031007
022800031007     c                   EndSr
022900031007
023000031007      *---------------------------------------------------------------*
023100031007      * EMISSIONE SUBFILE
023200031007      *---------------------------------------------------------------*
023300031007     c     Sr_EmiSfl     BegSr
023400031007
023500031008      * Trovati Record
023600031007     c                   If        Nrr <> 0
023700031008     c                   Dow       *InKl = *Off
023800031008     c                   Eval      Rec = 1
023900031008     c                   Eval      Nrr = 1
024000091110      * se dati anomali msg
024100091110     c                   if        $anomalo
024200091110     c                   eval      *in28 = *on
024300091110     c                   eval      v1cmsg = 'Attenzione presenti clienti -
024400091110     c                             con dati anomali'
024500091110     c                   else
024600091110     c                   eval      *in28 = *off
024700091110     c                   clear                   v1cmsg
024800091110     c                   endif
024900031008     c                   Write     Ls80t01
025000031007     c                   Write     Ls80z01
025100031007     c                   Exfmt     Ls80c01
025200031008     c                   EndDo
025300031008
025400031008      * Non trovati Record
025500031008     c                   Else
025600031008     c                   Dow       *InKl = *Off
025700031008     c                   Write     Ls80t01
025800031008     c                   Write     Ls80z01
025900031008     c                   Exfmt     Ls80d01
026000031008     c                   EndDo
026100031008     c                   EndIf
026200031007
026300031007     c                   EndSr
026400031007
026500031007      *---------------------------------------------------------------*
026600031007      * CARICAMENTO DATI
026700031007      *---------------------------------------------------------------*
026800031007     c     Sr_CarDati    BegSr
026900031008
027000031008     c                   Move      VabMgs        VabMgsAlfa
027100031007
027200161121      * Rottura Filiale segnacollo se passato un solo cliente
027300161121     c                   If        d80ccm <> 0 and
027400161121     c                             VabFGS <> Sav_VabFGS and
027500161118     c                             Sav_VabFGS <> *Zeros
027600161118     c                   Eval      %subst(V1sMgs:4:2) = VabMese
027700161118     c                   Eval      %subst(V1sMgs:1:2) = VabGiorno
027800161118     c                   Eval      %subst(V1sMgs:3:1) = '/'
027900161118     c                   Eval      V1sCcm = Sav_VabCcm
028000161118     c                   Exsr      Sr_RagSoc
028100161118     c                   Eval      V1sNsr = Sav_VabNsr
028200161118     c                   Eval      V1sFGS = *blank
028300161121     c                   if        d80ccm <> 0
028400161118     c                   Eval      V1sFGS = %editc(Sav_VabFGS:'X')
028500161118     c                   endif
028600161118      * Cliente in reverse image se dati anomali
028700161118     c                   if        v1srvad = 99999 or v1srvat = 99999
028800161118     c                   eval      *in10 = *on
028900161118     c                   else
029000161118     c                   eval      *in10 = *off
029100161118     c                   endif
029200161118     c                   Add       1             Nrr
029300161118     c                   Write     Ls80s01
029400161118     c                   Clear                   V1srVab
029500161118     c                   Clear                   V1stNcl
029600161118     c                   Clear                   V1stPkb
029700161118     c                   Clear                   V1srVad
029800161118     c                   Clear                   V1srVat
029900161118     c                   Clear                   V1srVax
030000161121     c                   Eval      Sav_VabCCM = VabCCM
030100161121     c                   Eval      Sav_VabMgs = VabMgsAlfa
030200161118     c                   EndIF
030300161118
030400161118      * Rottura Cliente
030500161118     c                   If        VabCcm <> Sav_VabCcm and
030600161118     c                             Sav_VabCcm <> *Zeros
030700031008     c                   Eval      %subst(V1sMgs:4:2) = VabMese
030800031008     c                   Eval      %subst(V1sMgs:1:2) = VabGiorno
030900031008     c                   Eval      %subst(V1sMgs:3:1) = '/'
031000031008     c                   Eval      V1sCcm = Sav_VabCcm
031100031008     c                   Exsr      Sr_RagSoc
031200031008     c                   Eval      V1sNsr = Sav_VabNsr
031300161118     c                   Eval      V1sFGS = *blank
031400161121     c                   if        d80ccm <> 0
031500161118     c                   Eval      V1sFGS = %editc(Sav_VabFGS:'X')
031600161118     c                   endif
031700091110      * Cliente in reverse image se dati anomali
031800091110     c                   if        v1srvad = 99999 or v1srvat = 99999
031900091110     c                   eval      *in10 = *on
032000091110     c                   else
032100091110     c                   eval      *in10 = *off
032200091110     c                   endif
032300031008     c                   Add       1             Nrr
032400031008     c                   Write     Ls80s01
032500031008     c                   Clear                   V1srVab
032600031008     c                   Clear                   V1stNcl
032700031008     c                   Clear                   V1stPkb
032800031008     c                   Clear                   V1srVad
032900031008     c                   Clear                   V1srVat
033000040604     c                   Clear                   V1srVax
033100031008     c                   Eval      Sav_VabMgs = VabMgsAlfa
033200031008     c                   EndIF
033300031008      * Rottura Data
033400031008     c                   If        VabMgsAlfa <> Sav_VabMgs and
033500031008     c                             Sav_VabMgs <> *Blanks
033600031008     c                   Eval      %subst(V1sMgs:4:2) = VabMese
033700031008     c                   Eval      %subst(V1sMgs:1:2) = VabGiorno
033800031008     c                   Eval      %subst(V1sMgs:3:1) = '/'
033900031008     c                   Eval      V1sCcm = Sav_VabCcm
034000031008     c                   Exsr      Sr_RagSoc
034100031008     c                   Eval      V1sNsr = Sav_VabNsr
034200161118     c                   Eval      V1sFGS = *blank
034300161121     c                   if        d80ccm <> 0
034400161118     c                   Eval      V1sFGS = %editc(Sav_VabFGS:'X')
034500161118     c                   endif
034600091110      * Cliente in reverse image se dati anomali
034700091110     c                   if        v1srvad = 99999 or v1srvat = 99999
034800091110     c                   eval      *in10 = *on
034900091110     c                   else
035000091110     c                   eval      *in10 = *off
035100091110     c                   endif
035200031008     c                   Add       1             Nrr
035300031008     c                   Write     Ls80s01
035400031008     c                   Clear                   V1srVab
035500031008     c                   Clear                   V1stNcl
035600031008     c                   Clear                   V1stPkb
035700031008     c                   Clear                   V1srVad
035800031008     c                   Clear                   V1srVat
035900040604     c                   Clear                   V1srVax
036000031008     c                   EndIF
036100031007
036200031008     c                   Eval      *In01 = *On
036300031007     c                   Add       1             V1srVab
036400031007     c                   Add       VabNcl        V1stNcl
036500031008     c                   Add       VabPkb        V1stPkb
036600031008     c                   Eval      Sav_VabNsr = VabNrs
036700071219
036800071219      * conto i VAD
036900071219     c                   clear                   wvad
037000071219     c/exec sql
037100071220     c+ declare vad cursor for select count(*) from fivad01l
037200071219     c+ where vadfgs= :vabfgs and vadccm = :vabccm and
037300071219     c+       vadaas= :vabaas and vadlnp = :vablnp and
037400071219     c+       vadnrs= :vabnrs and vadnsp = :vabnsp
037500071219     c/end-exec
037600071219
037700071219     c/exec sql
037800071219     c+ open vad
037900071219     c/end-exec
038000071219
038100071219     c/exec sql
038200071219     c+ fetch next from vad into :wvad
038300071219     c/end-exec
038400071219
038500071219     c/exec sql
038600071219     c+ close vad
038700071219     c/end-exec
038800091016
038900091016      * controllo se il totale ci sta a video
039000091016     c                   eval      wrkvad = v1srvad + wvad
039100091016     c                   if        wrkvad >= 99998
039200091016     c                   eval      v1srvad = 99999
039300091110     c                   eval      $anomalo = *on
039400091016     c                   else
039500071219
039600071219     c                   eval      v1srvad = v1srvad + wvad
039700091016     c                   endif
039800031007
039900071219      * conto i VAT
040000071219     c                   clear                   wvat
040100071219     c/exec sql
040200071220     c+ declare vat cursor for select count(*) from fivat01l
040300071219     c+ where vatfgs= :vabfgs and vatccm = :vabccm and
040400071219     c+       vataas= :vabaas and vatlnp = :vablnp and
040500071219     c+       vatnrs= :vabnrs and vatnsp = :vabnsp
040600071219     c/end-exec
040700071219
040800071219     c/exec sql
040900071219     c+ open vat
041000071219     c/end-exec
041100071219
041200071219     c/exec sql
041300071219     c+ fetch next from vat into :wvat
041400071219     c/end-exec
041500071219
041600071219     c/exec sql
041700071219     c+ close vat
041800071219     c/end-exec
041900091016
042000091016      * controllo se il totale ci sta a video
042100091016     c                   eval      wrkvat = v1srvat + wvat
042200091016     c                   if        wrkvat >= 99998
042300091016     c                   eval      v1srvat = 99999
042400091110     c                   eval      $anomalo = *on
042500091016     c                   else
042600071219
042700071219     c                   eval      v1srvat = v1srvat + wvat
042800091016     c                   endif
042900071219
043000071219      * conto i VAX
043100071219     c     kvax          Setll     Fivax01l
043200071219     c                   If        %equal(Fivax01l)
043300071219     c                   Add       1             V1srVax
043400071219     c                   EndIf
043500031007
043600031008      * Salvo Data
043700031008     c                   If        VabMgsAlfa <> Sav_VabMgs
043800101020     c                   if        vabccm = 0
043900101020     c                   eval      vabccm = *all'9'
044000101020     c                   endif
044100161121     c                   Eval      Sav_VabFGS = VabFGS
044200031008     c                   Eval      Sav_VabCcm = VabCcm
044300031008     c                   Eval      Sav_VabMgs = VabMgsAlfa
044400031007     c                   EndIF
044500031008      * Salvo Cliente
044600031008     c                   If        VabCcm <> Sav_VabCcm
044700101020     c                   if        vabccm = 0
044800101020     c                   eval      vabccm = *all'9'
044900101020     c                   endif
045000161121     c                   Eval      Sav_VabFGS = VabFGS
045100031008     c                   Eval      Sav_VabCcm = VabCcm
045200031008     c                   Eval      Sav_VabMgs = VabMgsAlfa
045300031007     c                   EndIF
045400161118      * Salvo Filiale segnacollo
045500161118     c                   If        VabFGS <> Sav_VabFGS
045600161121     c                   if        vabfgs = 0
045700161121     c                   eval      vabfgs = *all'9'
045800161121     c                   endif
045900161118     c                   Eval      Sav_VabFGS = VabFGS
046000161118     c                   Eval      Sav_VabCcm = VabCcm
046100161118     c                   Eval      Sav_VabMgs = VabMgsAlfa
046200161118     c                   EndIF
046300031007
046400031007     c     EndCarDati    EndSr
046500031007
046600031008
046700031008      *---------------------------------------------------------------*
046800031008      * SCRIVO L'ULTIMO RECORD
046900031008      *---------------------------------------------------------------*
047000031008     c     Sr_Ultimo     BegSr
047100031008
047200031008     c                   Eval      %subst(V1sMgs:4:2) = VabMese
047300031008     c                   Eval      %subst(V1sMgs:1:2) = VabGiorno
047400031008     c                   Eval      %subst(V1sMgs:3:1) = '/'
047500031008     c                   Eval      V1sCcm = Sav_VabCcm
047600031008     c                   Exsr      Sr_RagSoc
047700031008     c                   Eval      V1sNsr = Sav_VabNsr
047800161118     c                   Eval      V1sFGS = *blank
047900161121     c                   if        d80ccm <> 0
048000161118     c                   Eval      V1sFGS = %editc(Sav_VabFGS:'X')
048100161118     c                   endif
048200091110      * Cliente in reverse image se dati anomali
048300091110     c                   if        v1srvad = 99999 or v1srvat = 99999
048400091110     c                   eval      *in10 = *on
048500091110     c                   else
048600091110     c                   eval      *in10 = *off
048700091110     c                   endif
048800031008     c                   Add       1             Nrr
048900031008     c                   Write     Ls80s01
049000031008     c                   Clear                   V1srVab
049100031008     c                   Clear                   V1stNcl
049200031008     c                   Clear                   V1stPkb
049300031008     c                   Clear                   V1srVad
049400031008     c                   Clear                   V1srVat
049500040604     c                   Clear                   V1srVax
049600031008
049700031008     c                   EndSr
049800031008
049900031008      *---------------------------------------------------------------*
050000031008      * RAGIONE SOCIALE CLIENTE                                       *
050100031008      *---------------------------------------------------------------*
050200031008     c     Sr_RagSoc     BegSr
050300031008
050400031008     c                   Clear                   Tibs69ds
050500031008     c                   Eval      i69Kac = V1sCcm
050600031008     c                   Call      'TIBS69R'
050700031008     c                   Parm                    Tibs69DS
050800031008     c                   Parm                    ds_Cnaco
050900031008     c                   Parm                    ds_Cnind
051000031008     c                   Parm                    ds_Cnclp
051100031008     c                   Parm                    ds_Fncls
051200031008
051300031008     c                   If        o69Err <> '1'
051400031008     c                   Movel     AcoRag        V1sRag
051500031008     c                   Else
051600031008     c                   Movel     *all'*'       V1sRag
051700031008     c                   EndIf
051800031008
051900031008     c                   EndSr
052000031008
052100031007      *---------------------------------------------------------------*
052200031007      * SUBROUTINE INIZIALE                                           *
052300031007      *---------------------------------------------------------------*
052400031007     c     *Inzsr        BegSr
052500031007
052600031007     c     *Entry        Plist
052700031008     c                   Parm                    Kpjba
052800031007     c                   Parm                    Fnls80ds
052900031007
053000031007     c     *dtaara       define    §azute        azuteds
053100031007     c     *dtaara       define    §datiute      ddatiute
053200031007     c                   in(E)     *dtaara
053300031007     c                   IF        %error  or RSUT = *blanks
053400031007     c                   CLEAR                   tibs34ds
053500031007     c                   CALL      'TIBS34R'
053600031007     c                   PARM                    tibs34ds
053700031007     c                   in        *dtaara
053800031007     c                   endif
053900031007
054000031007     c                   Eval      VidDsa = Rsut
054100031007     c                   Eval      VidTes = Testo
054200031009     c                   Eval      VidTes1 = Testo1
054300031007
054400071219     c     kvax          Klist
054500071219     c                   Kfld                    VabFgs
054600071219     c                   Kfld                    VabCcm
054700071219     c                   Kfld                    VabAas
054800071219     c                   Kfld                    VabLnp
054900071219     c                   Kfld                    VabNrs
055000071219     c                   Kfld                    VabNsp
055100071219
055200031007     c                   EndSr

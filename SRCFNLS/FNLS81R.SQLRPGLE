000100031008     h Decedit('0,') Datedit(*dmy.) Option(*nodebugio)
000200031007      *---------------------------------------------------------------*
000300031007      * Visualizzazione totali VAB x cliente
000400031007      *---------------------------------------------------------------*
000500031007     fFnls80d   cf   e             Workstn    Sfile(Ls80s01:nrr)
000600071219     fFivax01l  if   e           k Disk
000700031007
000800031008     d sav_VabCcm      s                   Like(VabCcm) Inz
000900031008     d sav_VabNsr      s                   Like(VabNrs) Inz
001000071219     d stringasql      s           1100    varying
001100031008     d VabMgsAlfa      s              4
001200071219     d xx              s              3  0
001300071219     d wvad            s              5  0 inz
001400071219     d wvat            s              5  0 inz
001500091016     d wrkvad          s             10  0 inz
001600091016     d wrkvat          s             10  0 inz
001700091110     d $anomalo        s              1n
001800031008
001900031008     d sav_VabMgs      ds             4
002000031008     d  VabMese                1      2
002100031008     d  VabGiorno              3      4
002200031008
002300031007     d Fnls80ds      e ds
002400031008     d  Pog                    1     90  0 Dim(30)
002500031007
002600071219     d fivabds       e ds                  extname(fivab00f)
002700071219     d fivadds       e ds                  extname(fivad00f)
002800071219     d fivatds       e ds                  extname(fivat00f)
002900071219     d fivaxds       e ds                  extname(fivax00f)
003000071219
003100031008     d Tibs69ds      e ds
003200031008     d ds_cnaco      e ds                  Extname(Cnaco00f)
003300031008     d ds_cnind      e ds                  Extname(Cnind00f)
003400031008     d ds_cnclp      e ds                  Extname(Cnclp00f)
003500031008     d ds_fncls      e ds                  Extname(Fncls00f)
003600031007     d Tibs34ds      e ds                  Inz
003700031007     d dDatiute      e ds
003800031007     d Azuteds       e ds                  Extname(Azute00f)
003900031008     d Kpjba         e ds
004000031007
004100031009     d Testo           c                   Const('** CONFERMA BOLLE  DA CLIENTI-
004200031009     d                                      **')
004300031009     d Testo1          c                   Const('*  RIEPILOGO  *')
004400031007
004500031007      *---------------------------------------------------------------*
004600031007      *  RIEPILOGO INDICATORI
004700031007      *---------------------------------------------------------------*
004800031008      *  01  On   Ho letto almeno 1 rcd in FIVAB
004900031007      *  20  On   SFLEND
005000031007      *  21  Off  SFLDSP
005100031007      *  21  Off  SFLDSPCTL
005200031007      *  21  On   SFLCLR
005300031007      *---------------------------------------------------------------*
005400031007
005500071219     c/exec sql
005600071219     c+ set option dynusrprf = *owner, closqlcsr = *endmod
005700071219     c/end-exec
005800071219
005900071219      *         C A R I C A M E N T O   S T R I N G A   S Q L
006000071219
006100071219     c                   exsr      sr_stringasql
006200031007
006300031007      *         P U L I Z I A   S U B F I L E
006400031007
006500031007     c                   ExSr      Sr_PulSfl
006600031007
006700031007      *         C A R I C A M E N T O   S U B F I L E
006800031007
006900031007     c                   ExSr      Sr_CarSfl
007000031007
007100031007      *         E M I S S I O N E   S U B F I L E
007200031007
007300031007     c                   ExSr      Sr_EmiSfl
007400031007
007500031007     c                   Eval      *InLr = *On
007600071219
007700071219      *---------------------------------------------------------------*
007800071219      * CARICAMENTO STRINGA SQL
007900071219      *---------------------------------------------------------------*
008000071219     c     sr_stringasql begsr
008100071219
008200071219     c                   clear                   stringasql
008300071219
008400071219      * preparo la stringa sql
008500071219     c                   eval      stringasql =
008600071220     c                             'select fivab02l.* from '                  +
008700071220     c                             'fivab02l '                                +
008800071219     c                             'where vabfgs in ('
008900071219      * ciclo sulla sk p.o. da elaborare per i p.o. emissione
009000071219     c                   do        30            xx
009100071219     c                   if        pog(xx) = 0
009200071219     c                   leave
009300071219     c                   endif
009400071219     c                   eval      stringasql =
009500071219     c                             %trim(stringasql) +  ' '                    +
009600071219     c                             %editc(pog(xx):'1') + ','
009700071219     c                   enddo
009800071219     c                   eval      stringasql =
009900071219     c                             %trim(stringasql) + ')'
010000071219
010100071219     c                   select
010200071219
010300071219      * richiesto: cliente e serie
010400071219     c                   when      d80ccm <> 0 and
010500071219     c                             d80nsr <> 99
010600071219     c                   eval      stringasql =
010700071219     c                             %trim(stringasql) + ' '                     +
010800071219     c                             'and vabccm = ' + %editc(d80ccm:'X') + ' '  +
010900071219     c                             'and vabnrs = ' + %editc(d80nsr:'X') + ' '
011000071219      * richiesto: cliente
011100071219     c                   when      d80ccm <> 0 and
011200071219     c                             d80nsr = 99
011300071219     c                   eval      stringasql =
011400071219     c                             %trim(stringasql) + ' '                     +
011500071219     c                             'and vabccm = ' + %editc(d80ccm:'X') + ' '
011600071219      * richiesto: serie
011700071219     c                   when      d80nsr <> 99
011800071219     c                   eval      stringasql =
011900071219     c                             %trim(stringasql) + ' '                     +
012000071219     c                             'and vabnrs = ' + %editc(d80nsr:'X') + ' '
012100071219
012200071219     c                   endsl
012300071219
012400071219      * richiesta la linea di arrivo
012500071219     c                   if        d80lna <> 999
012600071219     c                   eval      stringasql =
012700071219     c                             %trim(stringasql) + ' '                     +
012800071219     c                             'and vablna = ' + %editc(d80lna:'X') + ' '
012900071219     c                   endif
013000071219      * ordino il file
013100071219     c                   eval      stringasql =
013200071219     c                             %trim(stringasql) + ' '                     +
013300071219     c                             'order by vabccm, vabnrs, vabmgs'
013400071219
013500071219     c                   endsr
013600031007
013700031007      *---------------------------------------------------------------*
013800031007      * PULIZIA SUBFILE
013900031007      *---------------------------------------------------------------*
014000031007     c     Sr_PulSfl     BegSr
014100031007
014200031007     c                   Clear                   Nrr               4 0
014300031007     c                   Eval      *In21 = *On
014400031007     c                   Write     Ls80c01
014500031007     c                   Eval      *In20 = *On
014600031007     c                   Eval      *In21 = *Off
014700091110
014800091110     c                   eval      $anomalo = *off
014900031007
015000031007     c                   EndSr
015100031007
015200031007      *---------------------------------------------------------------*
015300031007      * CARICAMENTO SUBFILE
015400031007      *---------------------------------------------------------------*
015500031007     c     Sr_CarSfl     BegSr
015600031007
015700050207     c                   Eval      *In01 = *Off
015800031007
015900071219     c/exec sql
016000071219     c+ prepare s1 from :stringasql
016100071219     c/end-exec
016200071219
016300071219     c/exec sql
016400071219     c+ declare a1 cursor for s1
016500071219     c/end-exec
016600071219
016700071219     c/exec sql
016800071219     c+ open a1
016900071219     c/end-exec
017000071219
017100071219     c                   do        *hival
017200071219
017300071219     c/exec sql
017400071219     c+ fetch next from a1 into :fivabds
017500071219     c/end-exec
017600071219
017700071219     c                   if        sqlcod = 100 or
017800071219     c                             sqlcod < 0
017900071219     c                   leave
018000071219     c                   endif
018100071219
018200031007      * Carico i dati
018300031007     c                   ExSr      Sr_Cardati
018400071219
018500071219     c                   enddo
018600071219
018700071219     c/exec sql
018800071219     c+ close a1
018900071219     c/end-exec
019000071219
019100071219      * Scrivo l'ultimo record
019200071219     c   01              ExSr      Sr_Ultimo
019300031007
019400031007     c                   EndSr
019500031007
019600031007      *---------------------------------------------------------------*
019700031007      * EMISSIONE SUBFILE
019800031007      *---------------------------------------------------------------*
019900031007     c     Sr_EmiSfl     BegSr
020000031007
020100031008      * Trovati Record
020200031007     c                   If        Nrr <> 0
020300031008     c                   Dow       *InKl = *Off
020400031008     c                   Eval      Rec = 1
020500031008     c                   Eval      Nrr = 1
020600091110      * se dati anomali msg
020700091110     c                   if        $anomalo
020800091110     c                   eval      *in28 = *on
020900091110     c                   eval      v1cmsg = 'Attenzione presenti clienti -
021000091110     c                             con dati anomali'
021100091110     c                   else
021200091110     c                   eval      *in28 = *off
021300091110     c                   clear                   v1cmsg
021400091110     c                   endif
021500031008     c                   Write     Ls80t01
021600031007     c                   Write     Ls80z01
021700031007     c                   Exfmt     Ls80c01
021800031008     c                   EndDo
021900031008
022000031008      * Non trovati Record
022100031008     c                   Else
022200031008     c                   Dow       *InKl = *Off
022300031008     c                   Write     Ls80t01
022400031008     c                   Write     Ls80z01
022500031008     c                   Exfmt     Ls80d01
022600031008     c                   EndDo
022700031008     c                   EndIf
022800031007
022900031007     c                   EndSr
023000031007
023100031007      *---------------------------------------------------------------*
023200031007      * CARICAMENTO DATI
023300031007      *---------------------------------------------------------------*
023400031007     c     Sr_CarDati    BegSr
023500031008
023600031008     c                   Move      VabMgs        VabMgsAlfa
023700031007
023800031008      * Rottura Cliente
023900031008     c                   If        VabCcm <> Sav_VabCcm and
024000031008     c                             Sav_VabCcm <> *Zeros
024100031008     c                   Eval      %subst(V1sMgs:4:2) = VabMese
024200031008     c                   Eval      %subst(V1sMgs:1:2) = VabGiorno
024300031008     c                   Eval      %subst(V1sMgs:3:1) = '/'
024400031008     c                   Eval      V1sCcm = Sav_VabCcm
024500031008     c                   Exsr      Sr_RagSoc
024600031008     c                   Eval      V1sNsr = Sav_VabNsr
024700091110      * Cliente in reverse image se dati anomali
024800091110     c                   if        v1srvad = 99999 or v1srvat = 99999
024900091110     c                   eval      *in10 = *on
025000091110     c                   else
025100091110     c                   eval      *in10 = *off
025200091110     c                   endif
025300031008     c                   Add       1             Nrr
025400031008     c                   Write     Ls80s01
025500031008     c                   Clear                   V1srVab
025600031008     c                   Clear                   V1stNcl
025700031008     c                   Clear                   V1stPkb
025800031008     c                   Clear                   V1srVad
025900031008     c                   Clear                   V1srVat
026000040604     c                   Clear                   V1srVax
026100031008     c                   Eval      Sav_VabMgs = VabMgsAlfa
026200031008     c                   EndIF
026300031008      * Rottura Data
026400031008     c                   If        VabMgsAlfa <> Sav_VabMgs and
026500031008     c                             Sav_VabMgs <> *Blanks
026600031008     c                   Eval      %subst(V1sMgs:4:2) = VabMese
026700031008     c                   Eval      %subst(V1sMgs:1:2) = VabGiorno
026800031008     c                   Eval      %subst(V1sMgs:3:1) = '/'
026900031008     c                   Eval      V1sCcm = Sav_VabCcm
027000031008     c                   Exsr      Sr_RagSoc
027100031008     c                   Eval      V1sNsr = Sav_VabNsr
027200091110      * Cliente in reverse image se dati anomali
027300091110     c                   if        v1srvad = 99999 or v1srvat = 99999
027400091110     c                   eval      *in10 = *on
027500091110     c                   else
027600091110     c                   eval      *in10 = *off
027700091110     c                   endif
027800031008     c                   Add       1             Nrr
027900031008     c                   Write     Ls80s01
028000031008     c                   Clear                   V1srVab
028100031008     c                   Clear                   V1stNcl
028200031008     c                   Clear                   V1stPkb
028300031008     c                   Clear                   V1srVad
028400031008     c                   Clear                   V1srVat
028500040604     c                   Clear                   V1srVax
028600031008     c                   EndIF
028700031007
028800031008     c                   Eval      *In01 = *On
028900031007     c                   Add       1             V1srVab
029000031007     c                   Add       VabNcl        V1stNcl
029100031008     c                   Add       VabPkb        V1stPkb
029200031008     c                   Eval      Sav_VabNsr = VabNrs
029300071219
029400071219      * conto i VAD
029500071219     c                   clear                   wvad
029600071219     c/exec sql
029700071220     c+ declare vad cursor for select count(*) from fivad01l
029800071219     c+ where vadfgs= :vabfgs and vadccm = :vabccm and
029900071219     c+       vadaas= :vabaas and vadlnp = :vablnp and
030000071219     c+       vadnrs= :vabnrs and vadnsp = :vabnsp
030100071219     c/end-exec
030200071219
030300071219     c/exec sql
030400071219     c+ open vad
030500071219     c/end-exec
030600071219
030700071219     c/exec sql
030800071219     c+ fetch next from vad into :wvad
030900071219     c/end-exec
031000071219
031100071219     c/exec sql
031200071219     c+ close vad
031300071219     c/end-exec
031400091016
031500091016      * controllo se il totale ci sta a video
031600091016     c                   eval      wrkvad = v1srvad + wvad
031700091016     c                   if        wrkvad >= 99998
031800091016     c                   eval      v1srvad = 99999
031900091110     c                   eval      $anomalo = *on
032000091016     c                   else
032100071219
032200071219     c                   eval      v1srvad = v1srvad + wvad
032300091016     c                   endif
032400031007
032500071219      * conto i VAT
032600071219     c                   clear                   wvat
032700071219     c/exec sql
032800071220     c+ declare vat cursor for select count(*) from fivat01l
032900071219     c+ where vatfgs= :vabfgs and vatccm = :vabccm and
033000071219     c+       vataas= :vabaas and vatlnp = :vablnp and
033100071219     c+       vatnrs= :vabnrs and vatnsp = :vabnsp
033200071219     c/end-exec
033300071219
033400071219     c/exec sql
033500071219     c+ open vat
033600071219     c/end-exec
033700071219
033800071219     c/exec sql
033900071219     c+ fetch next from vat into :wvat
034000071219     c/end-exec
034100071219
034200071219     c/exec sql
034300071219     c+ close vat
034400071219     c/end-exec
034500091016
034600091016      * controllo se il totale ci sta a video
034700091016     c                   eval      wrkvat = v1srvat + wvat
034800091016     c                   if        wrkvat >= 99998
034900091016     c                   eval      v1srvat = 99999
035000091110     c                   eval      $anomalo = *on
035100091016     c                   else
035200071219
035300071219     c                   eval      v1srvat = v1srvat + wvat
035400091016     c                   endif
035500071219
035600071219      * conto i VAX
035700071219     c     kvax          Setll     Fivax01l
035800071219     c                   If        %equal(Fivax01l)
035900071219     c                   Add       1             V1srVax
036000071219     c                   EndIf
036100031007
036200031008      * Salvo Data
036300031008     c                   If        VabMgsAlfa <> Sav_VabMgs
036400101020     c                   if        vabccm = 0
036500101020     c                   eval      vabccm = *all'9'
036600101020     c                   endif
036700031008     c                   Eval      Sav_VabCcm = VabCcm
036800031008     c                   Eval      Sav_VabMgs = VabMgsAlfa
036900031007     c                   EndIF
037000031008      * Salvo Cliente
037100031008     c                   If        VabCcm <> Sav_VabCcm
037200101020     c                   if        vabccm = 0
037300101020     c                   eval      vabccm = *all'9'
037400101020     c                   endif
037500031008     c                   Eval      Sav_VabCcm = VabCcm
037600031008     c                   Eval      Sav_VabMgs = VabMgsAlfa
037700031007     c                   EndIF
037800031007
037900031007     c     EndCarDati    EndSr
038000031007
038100031008
038200031008      *---------------------------------------------------------------*
038300031008      * SCRIVO L'ULTIMO RECORD
038400031008      *---------------------------------------------------------------*
038500031008     c     Sr_Ultimo     BegSr
038600031008
038700031008     c                   Eval      %subst(V1sMgs:4:2) = VabMese
038800031008     c                   Eval      %subst(V1sMgs:1:2) = VabGiorno
038900031008     c                   Eval      %subst(V1sMgs:3:1) = '/'
039000031008     c                   Eval      V1sCcm = Sav_VabCcm
039100031008     c                   Exsr      Sr_RagSoc
039200031008     c                   Eval      V1sNsr = Sav_VabNsr
039300091110      * Cliente in reverse image se dati anomali
039400091110     c                   if        v1srvad = 99999 or v1srvat = 99999
039500091110     c                   eval      *in10 = *on
039600091110     c                   else
039700091110     c                   eval      *in10 = *off
039800091110     c                   endif
039900031008     c                   Add       1             Nrr
040000031008     c                   Write     Ls80s01
040100031008     c                   Clear                   V1srVab
040200031008     c                   Clear                   V1stNcl
040300031008     c                   Clear                   V1stPkb
040400031008     c                   Clear                   V1srVad
040500031008     c                   Clear                   V1srVat
040600040604     c                   Clear                   V1srVax
040700031008
040800031008     c                   EndSr
040900031008
041000031008      *---------------------------------------------------------------*
041100031008      * RAGIONE SOCIALE CLIENTE                                       *
041200031008      *---------------------------------------------------------------*
041300031008     c     Sr_RagSoc     BegSr
041400031008
041500031008     c                   Clear                   Tibs69ds
041600031008     c                   Eval      i69Kac = V1sCcm
041700031008     c                   Call      'TIBS69R'
041800031008     c                   Parm                    Tibs69DS
041900031008     c                   Parm                    ds_Cnaco
042000031008     c                   Parm                    ds_Cnind
042100031008     c                   Parm                    ds_Cnclp
042200031008     c                   Parm                    ds_Fncls
042300031008
042400031008     c                   If        o69Err <> '1'
042500031008     c                   Movel     AcoRag        V1sRag
042600031008     c                   Else
042700031008     c                   Movel     *all'*'       V1sRag
042800031008     c                   EndIf
042900031008
043000031008     c                   EndSr
043100031008
043200031007      *---------------------------------------------------------------*
043300031007      * SUBROUTINE INIZIALE                                           *
043400031007      *---------------------------------------------------------------*
043500031007     c     *Inzsr        BegSr
043600031007
043700031007     c     *Entry        Plist
043800031008     c                   Parm                    Kpjba
043900031007     c                   Parm                    Fnls80ds
044000031007
044100031007     c     *dtaara       define    §azute        azuteds
044200031007     c     *dtaara       define    §datiute      ddatiute
044300031007     c                   in(E)     *dtaara
044400031007     c                   IF        %error  or RSUT = *blanks
044500031007     c                   CLEAR                   tibs34ds
044600031007     c                   CALL      'TIBS34R'
044700031007     c                   PARM                    tibs34ds
044800031007     c                   in        *dtaara
044900031007     c                   endif
045000031007
045100031007     c                   Eval      VidDsa = Rsut
045200031007     c                   Eval      VidTes = Testo
045300031009     c                   Eval      VidTes1 = Testo1
045400031007
045500071219     c     kvax          Klist
045600071219     c                   Kfld                    VabFgs
045700071219     c                   Kfld                    VabCcm
045800071219     c                   Kfld                    VabAas
045900071219     c                   Kfld                    VabLnp
046000071219     c                   Kfld                    VabNrs
046100071219     c                   Kfld                    VabNsp
046200071219
046300031007     c                   EndSr

000100011031     H DECEDIT('0,') DATEDIT(*YMD.)
000200011031
000300011031      ****************************************************************
000400011031      *                                                              *
000500011031      *     ? VISUALIZZAZIONE VAS ?                                  *
000600011031      *                                                              *
000700011031      ****************************************************************
000800011031
000900120523     fTIORI01L  IF   E           K DISK    usropn
001000120523     f                                     extfile(wLib)
001100120523     fTIORe01L  IF   E           K DISK    usropn
001200120523     f                                     extfile(wLib)
001300011031     FCNACO00F  IF   E           K DISK
001400011031     FFNACR01L  IF   E           K DISK
001500011031     FAZORG01L  IF   E           K DISK
001600100225     FFIOR29D   CF   E             WORKSTN  sfile(fior2s2:nrr)
001700011031
001800011031      *------------------------------------------------------------------------*
001900011031
002000120523       // - Nome libreria dei files TIORI/TIORE
002100120523     d wLib            s             21    inz
002200100225
002300100225     d nrr             s              4  0 inz
002400011031
002500011031      *  titolo videata (lunghezza massima 34)
002600011106     D TIT_A           C                   CONST('*** VISUALIZZAZIONE VAS O.R.M-
002700011106     D                                     . ***')
002800011031      *  caratteri numerici
002900011031     D DIGITn          C                   CONST('1234567890')
003000011031
003100011031      *   D S   I N T E R N E / E S T E R N E
003200011031
003300011031     D WLBDAT          DS                  INZ
003400011031     D  G02DAT                 1      8  0
003500011031     D  G02INV                 9     16  0
003600011031     D  G02ERR                17     17
003700011031     D  G02TGI                18     22  0
003800011031      *
003900011031     D                 DS
004000011031     D   v1cra1                1      3  0
004100011031     D   v1cra2                4      7  0
004200011031     D   v1cra3                8     10  0
004300011102     D v1ccra                  1     10  0
004400011031      *
004500011031     D                 DS
004600011031     D   v1cor1                1      3  0
004700011031     D   v1cor2                4      7  0
004800011031     D   v1cor3                8     10  0
004900011102     D v1ccor                  1     10  0
005000011031      *
005100011031     D                 DS
005200011031     D   v1crc1                1      3  0
005300011031     D   v1crc2                4      7  0
005400011031     D   v1crc3                8     10  0
005500011102     D v1ccrc                  1     10  0
005600011031      *
005700011102     D ds_ritiro       DS
005800011031     D   VAOpor
005900011031     D   VAOzor
006000011102      *
006100011102     D fior07ds        DS                  INZ
006200011102     D  ppoe                          3  0
006300011102     D  pnor                          7  0
006400011102     D  pnsr                          2  0
006500011102     D  pnrv                          2  0
006600011031      *
006700011031     D fior29ds        DS
006800011031     D  or29err                       1
006900011106     D  or29end                       1
007000011031     D  or29prg                       7  0
007100011031     D  or29poe                       3  0
007200011031     D  or29nsr                       2  0
007300011031     D  or29nor                       7  0
007400011031     D  or29nrv                       2  0
007500011031      *
007600011031     D TIBS02DS      E DS
007700011031      *
007800011109     D DORM01        E DS                  INZ                                  flag operativi FNORM
007900011109      *
008000011031     D KPJBA         E DS
008100100225
008200100225      * - Parametri x Controllo profilo utenti
008300100225     d TIBS34ds      e ds
008400100225      * - Ds di riferimento al file esterno AZUTE00F
008500100225     d AzuteDs       e ds                  extname(AZUTE00F)
008600100225      * - Ds per dati organigramma
008700100225     d dDatiUte      e ds
008800011031      *
008900011031     D                SDS
009000011031     D  VTCPGM                 1     10
009100011031
009200011031      *------------------------------------------------------------------------*
009300011107
009400011107      ****************************************************************
009500011107      *  RIEPILOGO INDICATORI
009600011107      ****************************************************************
009700011109      * 04 - ABILITAZIONE TASTO FUZIONALE F4=Visualizzazione ORM
009800011107      * 12 - SONO IN SEDE
009900011107      * 28 - ERRORE GENERICO DSPF
010000011107      ****************************************************************
010100011031
010200011031     C                   movel     kpjbu         fior29ds
010300011031
010400011031     C                   eval      v1prg  = OR29prg
010500011109     C                   if        OR29poe > 0
010600011109     C                   move      OR29poe       v1cpoe
010700011109     C                   else
010800011109     C                   clear                   v1cpoe
010900011109     C                   endif
011000011031     C                   eval      v1nsr  = OR29nsr
011100011031     C                   eval      v1nor  = OR29nor
011200011031     C                   eval      v1nrv  = OR29nrv
011300011031
011400011031      * Carica i dati dal file TIORI
011500011031
011600011031     C     OR29prg       chain     tiori01L
011700011109IF  1C                   if        not %found(tiori01l)
011800011031     C                   movel     '1'           OR29err
011900011031     C                   goto      fine
012000011031E   1C                   endif
012100011031
012200011031      * Emissione 1° Videata
012300011031     C                   exsr      Sr_Carvid
012400011112     C                   exsr      Sr_Decodifica
012500011031
012600011109DO  1C                   dou       *inkc = *on  or  *inkl = *on
012700011031
012800011031     C                   exfmt     fior291
012900011106
013000011106      * F3=Fine
013100011109     C   kc              movel     *on           OR29end
013200011102
013300011109      * F4=Visualizzazione ORM
013400011109     C   kd              exsr      Vis_ORM
013500011109
013600011109      * F7=Altri Dati
013700011109IF  2C                   if        *inkg = *on
013800100225     C                   z-add     1             recsf
013900011109DO  3C                   dou       *inkc = *on  or  *inkl = *on
014000100225     C                   write     fior2z2
014100100225     C                   exfmt     fior2c2
014200100225     C**************     exfmt     fior292
014300011109E   3C                   enddo
014400011109     C                   eval      *inkl = *off
014500011109E   2C                   endif
014600011031
014700011031E   1C                   enddo
014800011031
014900011031     C     fine          tag
015000011031     C                   movel(p)  fior29ds      kpjbu
015100011031     C                   eval      *inrt = *on
015200011031
015300011031      **********************************************************************
015400011031      * CARICA DATI NELLA VIDEATA DAL FILE
015500011031      **********************************************************************
015600011031     C     Sr_Carvid     begsr
015700011109
015800011109      * 1.a videata:
015900011031
016000011031     C                   eval      *in28  =  *off
016100011031     C                   clear                   v1cmsg
016200011102
016300011109      * abilito il tasto F4 solo se ORM confermato
016400011107IF  1C                   if            ORInsr = 0
016500011102     C                             and ORInor = 0
016600011102     C                             and ORInrv = 0
016700011109     C                   eval      *in04  =  *off
016800011107X   1C                   else
016900011109     C                   eval      *in04  =  *on
017000011107E   1C                   endif
017100011031
017200011109      * Annullato ?
017300080722IF  2C                   if        VAOatb = 'A'
017400090402     c                   eval      v1ann =
017500090402     c                             'Annullato da ' +
017600090402     c                              %subst(oriflo:1:10) + ' il ' +
017700090402     c                              %subst(oriflo:15:2) + '/' +
017800090402     c                              %subst(oriflo:13:2) + '/' +
017900090402     c                              %subst(oriflo:11:2) + ' ' +
018000090402     c                              %subst(oriflo:17:2) + ':' +
018100090402     c                              %subst(oriflo:19:2)
018200011109X   2C                   else
018300011109     C                   clear                   v1ann
018400011109E   2C                   endif
018500011109
018600011031      * campi hidden
018700011031     C                   movel     VAOtco        v1ctco
018800011107     C                   z-add     VAOblc        v1blc
018900011107     C                   z-add     VAOatt        v1att
019000011107     C                   z-add     VAOmtc        v1mtc
019100011107     C                   z-add     VAOsto        v1sto
019200011107     C                   movel     VAOzor        v1czor
019300011031
019400011031      * campi visualizzati
019500011102IF  1C                   if        ORIidc <> *zeros
019600011031     C                   movel     ORIidc        v1cidc
019700011102X   1C                   else
019800011102     C                   clear                   v1cidc
019900011102E   1C                   endif
020000011031     C                   z-add     ORIprg        v1prg
020100011109IF  1C                   if        ORIpoe  > 0
020200011109     C                   move      ORIpoe        v1cpoe
020300011109X   1C                   else
020400011109     C                   clear                   v1cpoe
020500011109E   1C                   endif
020600011031     C                   z-add     ORInsr        v1nsr
020700011031     C                   z-add     ORInor        v1nor
020800011031     C                   z-add     ORInrv        v1nrv
020900011105      *   Dati elaborazione record
021000011105     C                   z-add     ORIder        G02inv
021100011105     C                   movel     '3'           G02err
021200011105     C                   call      'XSRDA8'
021300011105     C                   parm                    wlbdat
021400011105     C                   z-add     G02dat        v1der
021500011105     C                   movel     ORIoer        v1oer
021600011031      *   Dati ap.ORM/chiamata
021700011031     C                   z-add     VAOdao        G02inv
021800011031     C                   movel     '3'           G02err
021900011031     C                   call      'XSRDA8'
022000011031     C                   parm                    wlbdat
022100011031     C                   z-add     G02dat        v1dao
022200011031     C                   movel     VAOoao        v1oao
022300011031      *   Tipo ordine
022400011031     C                   movel     VAOtor        v1ctor
022500011031      *   Dati ordinante
022600011031     C                   z-add     VAOcor        v1ccor
022700011031     C                   movel     VAOrso        v1rso
022800011031     C                   movel     VAOino        v1ino
022900011031     C                   movel     VAOloo        v1loo
023000011031     C                   movel     VAOcao        v1cao
023100011031     C                   movel     VAOpro        v1pro
023200011031     C                   movel     VAOnao        v1nao
023300011031      *   Dati mittente
023400011031     C                   z-add     VAOcra        v1ccra
023500011031     C                   movel     VAOrsr        v1rsr
023600011031     C                   movel     VAOinr        v1inr
023700011031     C                   movel     VAOlor        v1lor
023800011031     C                   movel     VAOcar        v1car
023900011031     C                   movel     VAOprr        v1prr
024000011031     C                   movel     VAOnar        v1nar
024100011031      *   Dati Destinatario
024200011031     C                   z-add     VAOcrc        v1ccrc
024300011031     C                   movel     VAOrsc        v1rsc
024400011031     C                   movel     VAOinc        v1inc
024500011031     C                   movel     VAOcac        v1cac
024600011031     C                   movel     VAOloc        v1loc
024700011031     C                   movel     VAOprc        v1prc
024800011031     C                   movel     VAOnac        v1nac
024900011031      *   Peso/Volume/Bancali/Bilici/Autotreni/Motrici
025000011031     C                   z-add     VAOncl        v1ncl
025100011031     C                   z-add     VAOpkg        v1pkg
025200011031     C                   z-add     VAOvlm        v1vlm
025300011031     C                   z-add     VAObnc        v1bnc
025400011031      *   Sponda idraulica
025500011109IF  1C                   if        VAOspi = 'S'
025600011031     C                   eval      v1spi = 'SI'
025700011109X   1C                   else
025800011031     C                   eval      v1spi = 'NO'
025900011109E   1C                   endif
026000011031      *   Dati ritiro
026100011031     C                   z-add     VAOdar        G02inv
026200011031     C                   movel     '3'           G02err
026300011031     C                   call      'XSRDA8'
026400011031     C                   parm                    wlbdat
026500011031     C                   z-add     G02dat        v1dar
026600011031     C                   movel     VAOorr        v1orr
026700011031      *   Dati ritiro
026800011109IF  1C                   if        VAOpor  > 0
026900011109     C                   move      VAOpor        v1cpor
027000011109X   1C                   else
027100011109     C                   clear                   v1cpor
027200011109E   1C                   endif
027300011031     C                   movel     VAOrer        v1rer
027400011031     C                   movel     VAOter        v1ter
027500011031      *   Descrizione natura merce
027600011031     C                   movel     VAOnam        v1nam
027700011031      *   Riferimento alfanumerico
027800011031     C                   movel     VAOrfa        v1rfa
027900011031      *   Cliente pagante
028000011109SEL 1C                   select
028100011109W   1C                   when      VAOpag = 'M'
028200011107     C                   eval      v1pag  = 'MITTENTE    '
028300011109W   1C                   when      VAOpag = 'D'
028400011107     C                   eval      v1pag  = 'DESTINATARIO'
028500011109W   1C                   when      VAOpag = 'O'
028600011107     C                   eval      v1pag  = 'ORDINANTE   '
028700011109X   1C                   other
028800011102     C                   clear                   v1pag
028900011109E   1C                   endsl
029000011031     C                   z-add     VAOksc        v1ksc
029100011031     C                   movel     VAOctr        v1ctr
029200011031      *   DDT
029300011109SEL 1C                   select
029400011109W   1C                   when      VAOddt = 'S'
029500011031     C                   eval      v1ddt  = 'SI'
029600011109W   1C                   when      VAOddt = 'N'
029700011031     C                   eval      v1ddt  = 'NO'
029800011109E   1C                   endsl
029900011031      *   P.O. consegna
030000011109IF  1C                   if        VAOpoc  > 0
030100011109     C                   move      VAOpoc        v1cpoc
030200011109X   1C                   else
030300011109     C                   clear                   v1cpoc
030400011109E   1C                   endif
030500011031      *   Fermo deposito
030600011109IF  1C                   if        VAOffd = 'S'
030700011031     C                   eval      v1ffd  = 'SI'
030800011109X   1C                   else
030900011031     C                   eval      v1ffd  = 'NO'
031000011109E   1C                   endif
031100011031      *   Note
031200011031     C                   movel     VAOno1        v1not1
031300011031     C                   movel     VAOno2        v1not2
031400011109
031500011109      * 2.a videata:
031600100225
031700100225      * Pulisco subfile
031800100225     c                   clear                   nrr
031900100225     c                   eval      *in21 = *off
032000100225     c                   write     fior2c2
032100100225     c                   eval      *in20 = *on
032200100225     c                   eval      *in21 = *on
032300100225      * Carico il subfile
032400100225     c     OR29prg       setll     tiore01l
032500100225     c                   do        *hival
032600100225     c     OR29prg       reade     tiore01l
032700100225     c                   if        %eof(tiore01l)
032800100225     c                   leave
032900100225     c                   endif
033000100225     c                   eval      vs2trc  = vaoetrc
033100100225     c                   eval      vs2dati = vaoedati
033200100225     c                   add       1             nrr
033300100225     c                   write     fior2s2
033400100225     c                   enddo
033500100225      * se non ho trovato nessuna estnsione carico un rcd vuoto nel subfile
033600100225     c                   IF        nrr = 0
033700100225     c                   clear                   vaoetrc
033800100225     c                   clear                   vaoedati
033900100225     c                   add       1             nrr
034000100225     c                   write     fior2s2
034100100225     c                   endif
034200011109
034300100225      * Imposto i dati nel control
034400011109      *   Flag Operativi
034500011109     C                   movel     VAOflo        DORM01
034600011109      *   - FNVAB abbinato
034700011109IF  1C                   if        §ORmcb = 'S'
034800011109     C                   movel     §ORmcb        v2mcb
034900011109X   1C                   else
035000011109     C                   clear                   v2mcb
035100011109E   1C                   endif
035200011109      *   - Forzatura mittente abbinato
035300011109IF  1C                   if        §ORmfr = 'S'
035400011109     C                   movel     §ORmfr        v2mfr
035500011109X   1C                   else
035600011109     C                   clear                   v2mfr
035700011109E   1C                   endif
035800011109      *   - Ritorno VAS al "codice cliente"
035900011109IF  1C                   if        §ORmks = 'S'
036000011109     C                   movel     §ORmks        v2mks
036100011109X   1C                   else
036200011109     C                   clear                   v2mks
036300011109E   1C                   endif
036400011109      *   - Forzatura destinatario
036500011109IF  1C                   if        §ORmfd = 'S'
036600011109     C                   movel     §ORmfd        v2mfd
036700011109X   1C                   else
036800011109     C                   clear                   v2mfd
036900011109E   1C                   endif
037000091009      *   - ORM commissionato
037100050422     C                   movel     §ORcomc       v2comc
037200100222      *   - ORM con preavviso
037300100222     c                   clear                   v2pre
037400100222     c                   if        §ormpre = 'M'
037500100222     c                   eval      v2pre = 'S'
037600100222     c                   endif
037700140307      *   - ORM Ricevuta di ritiro
037800140307     C                   movel     §ORsrm        v2srm
037900011031
038000011031     C                   endsr
038100011031
038200011031      **********************************************************************
038300011031      * DECODIFICHE
038400011031      **********************************************************************
038500011031     C     Sr_Decodifica begsr
038600011031
038700011031      * Cliente
038800011102     C                   clear                   v1didc
038900011031      *   controllo numericità
039000011109IF  1C                   if        ORIidc <> *blanks
039100011102     C                             and ORIidc <> *zeros
039200011031     C     DigitN        check     ORIidc                                 28
039300011031IF  2C                   if        *in28  =  *off
039400011031      *   controllo anagrafico
039500011031     C                   move      ORIidc        ACOksc
039600011031     C     kcnaco        chain     CNACO00F
039700011031B   3C                   if        %found(CNACO00F)
039800011031     C                             and ACOflg = *blanks
039900011031     C                   movel     ACOrag        v1didc
040000011031E   3C                   endif
040100011031E   2C                   endif
040200011031E   1C                   endif
040300011031
040400011031      * Tipo ordine
040500011102     C                   clear                   v1dtor
040600011031IF  1C                   if        VAOtor <> *blanks
040700011031     C                   clear                   TIBS02DS
040800011031     C                   movel     'C'           t02mod
040900011031     C                   movel     knsif         t02sif
041000011031     C                   movel     'TOR'         t02cod
041100011031     C                   movel(p)  VAOtor        t02ke1
041200011031     C                   call      'TIBS02R'
041300011031     C                   parm                    KPJBA
041400011031     C                   parm                    TIBS02DS
041500011031     C                   movel     t02uni        v1dtor
041600011102X   1C                   else
041700011102     C                   clear                   v1dtor
041800011031E   1C                   endif
041900011031
042000011031      * Tipo comunicazione ORM
042100011031IF  1C                   if        VAOtco <> *blanks
042200011031     C                   clear                   TIBS02DS
042300011031     C                   movel     'C'           t02mod
042400011031     C                   movel     knsif         t02sif
042500011031     C                   movel     'TCO'         t02cod
042600011031     C                   movel(p)  VAOtco        t02ke1
042700011031     C                   call      'TIBS02R'
042800011031     C                   parm                    KPJBA
042900011031     C                   parm                    TIBS02DS
043000011031     C                   movel     t02uni        v1dtco
043100011102X   1C                   else
043200011102     C                   clear                   v1dtco
043300011031E   1C                   endif
043400011031
043500011031      * P.O. ritiro
043600011102     C                   clear                   v1dpor
043700011031IF  1C                   if        VAOpor <> 0
043800011031     C     VAOpor        chain     azorg01l
043900011031IF  2C                   if        %found(azorg01l)
044000011031     C                   movel     ORGdes        v1dpor
044100011031E   2C                   endif
044200011031E   1C                   endif
044300011031
044400011031      * P.O. consegna
044500011102     C                   clear                   v1dpoc
044600011031IF  1C                   if        VAOpoc <> 0
044700011031     C     VAOpoc        chain     azorg01L
044800011031IF  2C                   if        %found(azorg01l)
044900011031     C                   movel     ORGdes        v1dpoc
045000011031E   2C                   endif
045100011031E   1C                   endif
045200011031
045300011031      * Cliente ordinante (se non decodificato)
045400011031IF  1C                   if            VAOrso =  *blanks
045500011031     C                             and VAOino =  *blanks
045600011031     C                             and VAOloo =  *blanks
045700011031     C                             and VAOcao =  *blanks
045800011031     C                             and VAOpro =  *blanks
045900011031     C                             and VAOnao =  *blanks
046000011102     C                             and VAOcor <> *zeros
046100011031     C     VAOcor        chain     FNACR01L
046200011031IF  2C                   if        %found(fnacr01l)
046300011031     C                             and ACRatb = *blanks
046400011031     C                   movel     ACRrsr        v1rso
046500011031     C                   movel     ACRinr        v1ino
046600011031     C                   movel     ACRcar        v1cao
046700011031     C                   movel     ACRlor        v1loo
046800011031     C                   movel     ACRprr        v1pro
046900011031     C                   movel     ACRnar        v1nao
047000011031E   2C                   endif
047100011031E   1C                   endif
047200011031      * Cliente mittente (se non decodificato)
047300011031IF  1C                   if            VAOrsr =  *blanks
047400011031     C                             and VAOinr =  *blanks
047500011031     C                             and VAOlor =  *blanks
047600011031     C                             and VAOcar =  *blanks
047700011031     C                             and VAOprr =  *blanks
047800011031     C                             and VAOnar =  *blanks
047900011102     C                             and VAOcra <> *zeros
048000011031     C     VAOcra        chain     FNACR01L
048100011031IF  2C                   if        %found(fnacr01l)
048200011031     C                             and ACRatb = *blanks
048300011031     C                   movel     ACRrsr        v1rsr
048400011031     C                   movel     ACRinr        v1inr
048500011031     C                   movel     ACRcar        v1car
048600011031     C                   movel     ACRlor        v1lor
048700011031     C                   movel     ACRprr        v1prr
048800011031     C                   movel     ACRnar        v1nar
048900011031E   2C                   endif
049000011031E   1C                   endif
049100011031      * Cliente destinatario (se non decodificato)
049200011031IF  1C                   if            VAOrsc =  *blanks
049300011031     C                             and VAOinc =  *blanks
049400011031     C                             and VAOloc =  *blanks
049500011031     C                             and VAOcac =  *blanks
049600011031     C                             and VAOprc =  *blanks
049700011031     C                             and VAOnac =  *blanks
049800011102     C                             and VAOcrc <> *zeros
049900011031     C     VAOcrc        chain     FNACR01L
050000011031IF  2C                   if        %found(fnacr01l)
050100011031     C                             and ACRatb = *blanks
050200011031     C                   movel     ACRrsr        v1rsc
050300011031     C                   movel     ACRinr        v1inc
050400011031     C                   movel     ACRcar        v1cac
050500011031     C                   movel     ACRlor        v1loc
050600011031     C                   movel     ACRprr        v1prc
050700011031     C                   movel     ACRnar        v1nac
050800011031E   2C                   endif
050900011031E   1C                   endif
051000011031
051100011031     C                   endsr
051200011102
051300011102      *****************************************************************
051400011102      * VISUALIZZAZIONE ORM
051500011102      *****************************************************************
051600011102     C     Vis_ORM       BEGSR
051700011102
051800011102     C                   clear                   fior07ds
051900011109     C                   z-add     ORIpoe        ppoe
052000011109     C                   z-add     ORInor        pnor
052100011109     C                   z-add     ORInsr        pnsr
052200011109     C                   z-add     ORInrv        pnrv
052300011102
052400011102     C                   movel     fior07ds      kpjbu
052500011102     C                   call      'FIOR07R'
052600011102     C                   parm                    kpjba
052700011102     C                   movel     kpjbu         fior07ds
052800011102
052900011102     C                   endsr
053000011031
053100011031      *****************************************************************
053200011031      * ROUTINE INIZIALE
053300011031      *****************************************************************
053400011031     C     *INZSR        BEGSR
053500011031
053600011031     C     *ENTRY        PLIST
053700011031     C                   PARM                    KPJBA
053800011031
053900011031     C                   movel     TIT_A         VTCtit
054000011031
054100100225     C                   Z-ADD     1             CODUT             1 0
054200100225     c     *dtaara       define    §azute        AZUTEds
054300100225     c     *dtaara       define    §datiute      dDATIUTE
054400100225     c                   in(E)     *dtaara
054500100225     c                   if        %ERROR or RSUT = *blanks
054600100225     c                   clear                   Tibs34Ds
054700100225     c                   call      'TIBS34R'
054800100225     c                   parm                    Tibs34Ds
054900100225     c                   in        *dtaara
055000100225     c                   endif
055100120523
055200120523      /free
055300120523       //?Imposto libreria/file per file TIORI/TIORE
055400120523        IF  %subst(knsif : 7 : 1) = 'P';
055500120523          wLib = 'GAITRAGRPS/';
055600120523          %subst(wLib:12:8) = 'TIORI01L';
055700120523          open TIORI01L;
055800120523          %subst(wLib:12:8) = 'TIORE01L';
055900120523          open TIORE01L;
056000120523        ENDIF;
056100120523        IF  %subst(knsif : 7 : 1) <> 'P';
056200120523          wLib = 'GAITRAGRU/';
056300120523          %subst(wLib:11:8) = 'TIORI01L';
056400120523          open TIORI01L;
056500120523          %subst(wLib:11:8) = 'TIORE01L';
056600120523          open TIORE01L;
056700120523        ENDIF;
056800120523      /end-free
056900011031
057000011031      * Da sede non imposto il p.o. di gestione
057100011031IF  1C                   if        simfel = 0
057200011031     C                   eval      *in12 = *on
057300011031X   1C                   else
057400011031     C                   eval      *in12 = *off
057500011031E   1C                   endif
057600011031
057700011031     C     Kcnaco        klist
057800011031     C                   kfld                    codut
057900100225     C                   kfld                    dutkci
058000011031     C                   kfld                    ACOksc
058100011031
058200011031     C                   ENDSR

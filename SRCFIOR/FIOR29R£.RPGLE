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
014200011109E   3C                   enddo
014300011109     C                   eval      *inkl = *off
014400011109E   2C                   endif
014500011031
014600011031E   1C                   enddo
014700011031
014800011031     C     fine          tag
014900011031     C                   movel(p)  fior29ds      kpjbu
015000011031     C                   eval      *inrt = *on
015100011031
015200011031      **********************************************************************
015300011031      * CARICA DATI NELLA VIDEATA DAL FILE
015400011031      **********************************************************************
015500011031     C     Sr_Carvid     begsr
015600011109
015700011109      * 1.a videata:
015800011031
015900011031     C                   eval      *in28  =  *off
016000011031     C                   clear                   v1cmsg
016100011102
016200011109      * abilito il tasto F4 solo se ORM confermato
016300011107IF  1C                   if            ORInsr = 0
016400011102     C                             and ORInor = 0
016500011102     C                             and ORInrv = 0
016600011109     C                   eval      *in04  =  *off
016700011107X   1C                   else
016800011109     C                   eval      *in04  =  *on
016900011107E   1C                   endif
017000140708
017100140708      /free
017200140708       //?Se ORM senza serie ma con stesso numero ORM sui campi ORI e VAO
017300140708       //?vuol dire che è un ORM ancora da confermare quindi NON abilito F4
017400140708         IF  ORInsr = 0 and ORInor > 0 and
017500140708                            ORIpoe = VAOpoe and ORInsr = VAOnsr and
017600140708                            ORInor = VAOnor and ORInrv = VAOnrv;
017700140708           *in04 = *off;
017800140708         ENDIF;
017900140708      /end-free
018000011031
018100011109      * Annullato ?
018200080722IF  2C                   if        VAOatb = 'A'
018300090402     c                   eval      v1ann =
018400090402     c                             'Annullato da ' +
018500090402     c                              %subst(oriflo:1:10) + ' il ' +
018600090402     c                              %subst(oriflo:15:2) + '/' +
018700090402     c                              %subst(oriflo:13:2) + '/' +
018800090402     c                              %subst(oriflo:11:2) + ' ' +
018900090402     c                              %subst(oriflo:17:2) + ':' +
019000090402     c                              %subst(oriflo:19:2)
019100011109X   2C                   else
019200011109     C                   clear                   v1ann
019300011109E   2C                   endif
019400011109
019500011031      * campi hidden
019600011031     C                   movel     VAOtco        v1ctco
019700011107     C                   z-add     VAOblc        v1blc
019800011107     C                   z-add     VAOatt        v1att
019900011107     C                   z-add     VAOmtc        v1mtc
020000011107     C                   z-add     VAOsto        v1sto
020100011107     C                   movel     VAOzor        v1czor
020200011031
020300011031      * campi visualizzati
020400011102IF  1C                   if        ORIidc <> *zeros
020500011031     C                   movel     ORIidc        v1cidc
020600011102X   1C                   else
020700011102     C                   clear                   v1cidc
020800011102E   1C                   endif
020900011031     C                   z-add     ORIprg        v1prg
021000011109IF  1C                   if        ORIpoe  > 0
021100011109     C                   move      ORIpoe        v1cpoe
021200011109X   1C                   else
021300011109     C                   clear                   v1cpoe
021400011109E   1C                   endif
021500140708     C   04              z-add     ORInsr        v1nsr
021600140708     C   04              z-add     ORInor        v1nor
021700140708     C   04              z-add     ORInrv        v1nrv
021800011105      *   Dati elaborazione record
021900011105     C                   z-add     ORIder        G02inv
022000011105     C                   movel     '3'           G02err
022100011105     C                   call      'XSRDA8'
022200011105     C                   parm                    wlbdat
022300011105     C                   z-add     G02dat        v1der
022400011105     C                   movel     ORIoer        v1oer
022500011031      *   Dati ap.ORM/chiamata
022600011031     C                   z-add     VAOdao        G02inv
022700011031     C                   movel     '3'           G02err
022800011031     C                   call      'XSRDA8'
022900011031     C                   parm                    wlbdat
023000011031     C                   z-add     G02dat        v1dao
023100011031     C                   movel     VAOoao        v1oao
023200011031      *   Tipo ordine
023300011031     C                   movel     VAOtor        v1ctor
023400011031      *   Dati ordinante
023500011031     C                   z-add     VAOcor        v1ccor
023600011031     C                   movel     VAOrso        v1rso
023700011031     C                   movel     VAOino        v1ino
023800011031     C                   movel     VAOloo        v1loo
023900011031     C                   movel     VAOcao        v1cao
024000011031     C                   movel     VAOpro        v1pro
024100011031     C                   movel     VAOnao        v1nao
024200011031      *   Dati mittente
024300011031     C                   z-add     VAOcra        v1ccra
024400011031     C                   movel     VAOrsr        v1rsr
024500011031     C                   movel     VAOinr        v1inr
024600011031     C                   movel     VAOlor        v1lor
024700011031     C                   movel     VAOcar        v1car
024800011031     C                   movel     VAOprr        v1prr
024900011031     C                   movel     VAOnar        v1nar
025000011031      *   Dati Destinatario
025100011031     C                   z-add     VAOcrc        v1ccrc
025200011031     C                   movel     VAOrsc        v1rsc
025300011031     C                   movel     VAOinc        v1inc
025400011031     C                   movel     VAOcac        v1cac
025500011031     C                   movel     VAOloc        v1loc
025600011031     C                   movel     VAOprc        v1prc
025700011031     C                   movel     VAOnac        v1nac
025800011031      *   Peso/Volume/Bancali/Bilici/Autotreni/Motrici
025900011031     C                   z-add     VAOncl        v1ncl
026000011031     C                   z-add     VAOpkg        v1pkg
026100011031     C                   z-add     VAOvlm        v1vlm
026200011031     C                   z-add     VAObnc        v1bnc
026300011031      *   Sponda idraulica
026400011109IF  1C                   if        VAOspi = 'S'
026500011031     C                   eval      v1spi = 'SI'
026600011109X   1C                   else
026700011031     C                   eval      v1spi = 'NO'
026800011109E   1C                   endif
026900011031      *   Dati ritiro
027000011031     C                   z-add     VAOdar        G02inv
027100011031     C                   movel     '3'           G02err
027200011031     C                   call      'XSRDA8'
027300011031     C                   parm                    wlbdat
027400011031     C                   z-add     G02dat        v1dar
027500011031     C                   movel     VAOorr        v1orr
027600011031      *   Dati ritiro
027700011109IF  1C                   if        VAOpor  > 0
027800011109     C                   move      VAOpor        v1cpor
027900011109X   1C                   else
028000011109     C                   clear                   v1cpor
028100011109E   1C                   endif
028200011031     C                   movel     VAOrer        v1rer
028300011031     C                   movel     VAOter        v1ter
028400011031      *   Descrizione natura merce
028500011031     C                   movel     VAOnam        v1nam
028600011031      *   Riferimento alfanumerico
028700011031     C                   movel     VAOrfa        v1rfa
028800011031      *   Cliente pagante
028900011109SEL 1C                   select
029000011109W   1C                   when      VAOpag = 'M'
029100011107     C                   eval      v1pag  = 'MITTENTE    '
029200011109W   1C                   when      VAOpag = 'D'
029300011107     C                   eval      v1pag  = 'DESTINATARIO'
029400011109W   1C                   when      VAOpag = 'O'
029500011107     C                   eval      v1pag  = 'ORDINANTE   '
029600011109X   1C                   other
029700011102     C                   clear                   v1pag
029800011109E   1C                   endsl
029900011031     C                   z-add     VAOksc        v1ksc
030000011031     C                   movel     VAOctr        v1ctr
030100011031      *   DDT
030200011109SEL 1C                   select
030300011109W   1C                   when      VAOddt = 'S'
030400011031     C                   eval      v1ddt  = 'SI'
030500011109W   1C                   when      VAOddt = 'N'
030600011031     C                   eval      v1ddt  = 'NO'
030700011109E   1C                   endsl
030800011031      *   P.O. consegna
030900011109IF  1C                   if        VAOpoc  > 0
031000011109     C                   move      VAOpoc        v1cpoc
031100011109X   1C                   else
031200011109     C                   clear                   v1cpoc
031300011109E   1C                   endif
031400011031      *   Fermo deposito
031500011109IF  1C                   if        VAOffd = 'S'
031600011031     C                   eval      v1ffd  = 'SI'
031700011109X   1C                   else
031800011031     C                   eval      v1ffd  = 'NO'
031900011109E   1C                   endif
032000011031      *   Note
032100011031     C                   movel     VAOno1        v1not1
032200011031     C                   movel     VAOno2        v1not2
032300011109
032400011109      * 2.a videata:
032500100225
032600100225      * Pulisco subfile
032700100225     c                   clear                   nrr
032800100225     c                   eval      *in21 = *off
032900100225     c                   write     fior2c2
033000100225     c                   eval      *in20 = *on
033100100225     c                   eval      *in21 = *on
033200100225      * Carico il subfile
033300100225     c     OR29prg       setll     tiore01l
033400100225     c                   do        *hival
033500100225     c     OR29prg       reade     tiore01l
033600100225     c                   if        %eof(tiore01l)
033700100225     c                   leave
033800100225     c                   endif
033900100225     c                   eval      vs2trc  = vaoetrc
034000100225     c                   eval      vs2dati = vaoedati
034100100225     c                   add       1             nrr
034200100225     c                   write     fior2s2
034300100225     c                   enddo
034400100225      * se non ho trovato nessuna estnsione carico un rcd vuoto nel subfile
034500100225     c                   IF        nrr = 0
034600100225     c                   clear                   vaoetrc
034700100225     c                   clear                   vaoedati
034800100225     c                   add       1             nrr
034900100225     c                   write     fior2s2
035000100225     c                   endif
035100011109
035200100225      * Imposto i dati nel control
035300011109      *   Flag Operativi
035400011109     C                   movel     VAOflo        DORM01
035500011109      *   - FNVAB abbinato
035600011109IF  1C                   if        §ORmcb = 'S'
035700011109     C                   movel     §ORmcb        v2mcb
035800011109X   1C                   else
035900011109     C                   clear                   v2mcb
036000011109E   1C                   endif
036100011109      *   - Forzatura mittente abbinato
036200011109IF  1C                   if        §ORmfr = 'S'
036300011109     C                   movel     §ORmfr        v2mfr
036400011109X   1C                   else
036500011109     C                   clear                   v2mfr
036600011109E   1C                   endif
036700011109      *   - Ritorno VAS al "codice cliente"
036800011109IF  1C                   if        §ORmks = 'S'
036900011109     C                   movel     §ORmks        v2mks
037000011109X   1C                   else
037100011109     C                   clear                   v2mks
037200011109E   1C                   endif
037300011109      *   - Forzatura destinatario
037400011109IF  1C                   if        §ORmfd = 'S'
037500011109     C                   movel     §ORmfd        v2mfd
037600011109X   1C                   else
037700011109     C                   clear                   v2mfd
037800011109E   1C                   endif
037900091009      *   - ORM commissionato
038000050422     C                   movel     §ORcomc       v2comc
038100100222      *   - ORM con preavviso
038200100222     c                   clear                   v2pre
038300100222     c                   if        §ormpre = 'M'
038400100222     c                   eval      v2pre = 'S'
038500100222     c                   endif
038600140307      *   - ORM Ricevuta di ritiro
038700140307     C                   movel     §ORsrm        v2srm
038800011031
038900011031     C                   endsr
039000011031
039100011031      **********************************************************************
039200011031      * DECODIFICHE
039300011031      **********************************************************************
039400011031     C     Sr_Decodifica begsr
039500011031
039600011031      * Cliente
039700011102     C                   clear                   v1didc
039800011031      *   controllo numericità
039900011109IF  1C                   if        ORIidc <> *blanks
040000011102     C                             and ORIidc <> *zeros
040100011031     C     DigitN        check     ORIidc                                 28
040200011031IF  2C                   if        *in28  =  *off
040300011031      *   controllo anagrafico
040400011031     C                   move      ORIidc        ACOksc
040500011031     C     kcnaco        chain     CNACO00F
040600011031B   3C                   if        %found(CNACO00F)
040700011031     C                             and ACOflg = *blanks
040800011031     C                   movel     ACOrag        v1didc
040900011031E   3C                   endif
041000011031E   2C                   endif
041100011031E   1C                   endif
041200011031
041300011031      * Tipo ordine
041400011102     C                   clear                   v1dtor
041500011031IF  1C                   if        VAOtor <> *blanks
041600011031     C                   clear                   TIBS02DS
041700011031     C                   movel     'C'           t02mod
041800011031     C                   movel     knsif         t02sif
041900011031     C                   movel     'TOR'         t02cod
042000011031     C                   movel(p)  VAOtor        t02ke1
042100011031     C                   call      'TIBS02R'
042200011031     C                   parm                    KPJBA
042300011031     C                   parm                    TIBS02DS
042400011031     C                   movel     t02uni        v1dtor
042500011102X   1C                   else
042600011102     C                   clear                   v1dtor
042700011031E   1C                   endif
042800011031
042900011031      * Tipo comunicazione ORM
043000011031IF  1C                   if        VAOtco <> *blanks
043100011031     C                   clear                   TIBS02DS
043200011031     C                   movel     'C'           t02mod
043300011031     C                   movel     knsif         t02sif
043400011031     C                   movel     'TCO'         t02cod
043500011031     C                   movel(p)  VAOtco        t02ke1
043600011031     C                   call      'TIBS02R'
043700011031     C                   parm                    KPJBA
043800011031     C                   parm                    TIBS02DS
043900011031     C                   movel     t02uni        v1dtco
044000011102X   1C                   else
044100011102     C                   clear                   v1dtco
044200011031E   1C                   endif
044300011031
044400011031      * P.O. ritiro
044500011102     C                   clear                   v1dpor
044600011031IF  1C                   if        VAOpor <> 0
044700011031     C     VAOpor        chain     azorg01l
044800011031IF  2C                   if        %found(azorg01l)
044900011031     C                   movel     ORGdes        v1dpor
045000011031E   2C                   endif
045100011031E   1C                   endif
045200011031
045300011031      * P.O. consegna
045400011102     C                   clear                   v1dpoc
045500011031IF  1C                   if        VAOpoc <> 0
045600011031     C     VAOpoc        chain     azorg01L
045700011031IF  2C                   if        %found(azorg01l)
045800011031     C                   movel     ORGdes        v1dpoc
045900011031E   2C                   endif
046000011031E   1C                   endif
046100011031
046200011031      * Cliente ordinante (se non decodificato)
046300011031IF  1C                   if            VAOrso =  *blanks
046400011031     C                             and VAOino =  *blanks
046500011031     C                             and VAOloo =  *blanks
046600011031     C                             and VAOcao =  *blanks
046700011031     C                             and VAOpro =  *blanks
046800011031     C                             and VAOnao =  *blanks
046900011102     C                             and VAOcor <> *zeros
047000011031     C     VAOcor        chain     FNACR01L
047100011031IF  2C                   if        %found(fnacr01l)
047200011031     C                             and ACRatb = *blanks
047300011031     C                   movel     ACRrsr        v1rso
047400011031     C                   movel     ACRinr        v1ino
047500011031     C                   movel     ACRcar        v1cao
047600011031     C                   movel     ACRlor        v1loo
047700011031     C                   movel     ACRprr        v1pro
047800011031     C                   movel     ACRnar        v1nao
047900011031E   2C                   endif
048000011031E   1C                   endif
048100011031      * Cliente mittente (se non decodificato)
048200011031IF  1C                   if            VAOrsr =  *blanks
048300011031     C                             and VAOinr =  *blanks
048400011031     C                             and VAOlor =  *blanks
048500011031     C                             and VAOcar =  *blanks
048600011031     C                             and VAOprr =  *blanks
048700011031     C                             and VAOnar =  *blanks
048800011102     C                             and VAOcra <> *zeros
048900011031     C     VAOcra        chain     FNACR01L
049000011031IF  2C                   if        %found(fnacr01l)
049100011031     C                             and ACRatb = *blanks
049200011031     C                   movel     ACRrsr        v1rsr
049300011031     C                   movel     ACRinr        v1inr
049400011031     C                   movel     ACRcar        v1car
049500011031     C                   movel     ACRlor        v1lor
049600011031     C                   movel     ACRprr        v1prr
049700011031     C                   movel     ACRnar        v1nar
049800011031E   2C                   endif
049900011031E   1C                   endif
050000011031      * Cliente destinatario (se non decodificato)
050100011031IF  1C                   if            VAOrsc =  *blanks
050200011031     C                             and VAOinc =  *blanks
050300011031     C                             and VAOloc =  *blanks
050400011031     C                             and VAOcac =  *blanks
050500011031     C                             and VAOprc =  *blanks
050600011031     C                             and VAOnac =  *blanks
050700011102     C                             and VAOcrc <> *zeros
050800011031     C     VAOcrc        chain     FNACR01L
050900011031IF  2C                   if        %found(fnacr01l)
051000011031     C                             and ACRatb = *blanks
051100011031     C                   movel     ACRrsr        v1rsc
051200011031     C                   movel     ACRinr        v1inc
051300011031     C                   movel     ACRcar        v1cac
051400011031     C                   movel     ACRlor        v1loc
051500011031     C                   movel     ACRprr        v1prc
051600011031     C                   movel     ACRnar        v1nac
051700011031E   2C                   endif
051800011031E   1C                   endif
051900011031
052000011031     C                   endsr
052100011102
052200011102      *****************************************************************
052300011102      * VISUALIZZAZIONE ORM
052400011102      *****************************************************************
052500011102     C     Vis_ORM       BEGSR
052600011102
052700011102     C                   clear                   fior07ds
052800011109     C                   z-add     ORIpoe        ppoe
052900011109     C                   z-add     ORInor        pnor
053000011109     C                   z-add     ORInsr        pnsr
053100011109     C                   z-add     ORInrv        pnrv
053200011102
053300011102     C                   movel     fior07ds      kpjbu
053400011102     C                   call      'FIOR07R'
053500011102     C                   parm                    kpjba
053600011102     C                   movel     kpjbu         fior07ds
053700011102
053800011102     C                   endsr
053900011031
054000011031      *****************************************************************
054100011031      * ROUTINE INIZIALE
054200011031      *****************************************************************
054300011031     C     *INZSR        BEGSR
054400011031
054500011031     C     *ENTRY        PLIST
054600011031     C                   PARM                    KPJBA
054700011031
054800011031     C                   movel     TIT_A         VTCtit
054900011031
055000100225     C                   Z-ADD     1             CODUT             1 0
055100100225     c     *dtaara       define    §azute        AZUTEds
055200100225     c     *dtaara       define    §datiute      dDATIUTE
055300100225     c                   in(E)     *dtaara
055400100225     c                   if        %ERROR or RSUT = *blanks
055500100225     c                   clear                   Tibs34Ds
055600100225     c                   call      'TIBS34R'
055700100225     c                   parm                    Tibs34Ds
055800100225     c                   in        *dtaara
055900100225     c                   endif
056000120523
056100120523      /free
056200120523       //?Imposto libreria/file per file TIORI/TIORE
056300120523        IF  %subst(knsif : 7 : 1) = 'P';
056400120523          wLib = 'GAITRAGRPS/';
056500120523          %subst(wLib:12:8) = 'TIORI01L';
056600120523          open TIORI01L;
056700120523          %subst(wLib:12:8) = 'TIORE01L';
056800120523          open TIORE01L;
056900120523        ENDIF;
057000120523        IF  %subst(knsif : 7 : 1) <> 'P';
057100120523          wLib = 'GAITRAGRU/';
057200120523          %subst(wLib:11:8) = 'TIORI01L';
057300120523          open TIORI01L;
057400120523          %subst(wLib:11:8) = 'TIORE01L';
057500120523          open TIORE01L;
057600120523        ENDIF;
057700120523      /end-free
057800011031
057900011031      * Da sede non imposto il p.o. di gestione
058000011031IF  1C                   if        simfel = 0
058100011031     C                   eval      *in12 = *on
058200011031X   1C                   else
058300011031     C                   eval      *in12 = *off
058400011031E   1C                   endif
058500011031
058600011031     C     Kcnaco        klist
058700011031     C                   kfld                    codut
058800100225     C                   kfld                    dutkci
058900011031     C                   kfld                    ACOksc
059000011031
059100011031     C                   ENDSR

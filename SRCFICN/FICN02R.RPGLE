000100081219     H DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP(*CALLER)
000200120724     H BNDDIR('TIBS') EXTBININT(*YES)
000300081219     H* ficn02R *----------------------------------------------------*
000400000000     H*-----------------*                                            *
000500081218     H*               GESTIONE SUPERTESTA TARIFFE PADRONCINI         *
000600000000     H*--------------------------------------------------------------*
000700081219     Fficn02D   CF   E             WORKSTN
000800090331     Ftabel00f  iF   E           K DISK
000900111124     Ftntbe01l  iF   E           K DISK
001000090331     Fanfrn01l  iF   E           K DISK
001100090331     Fansog01l  iF   E           K DISK
001200090324     Faitrs02l  iF   E           K DISK
001300090423     Faitra03l  iF   E           K DISK
001400090324     Ffitgt01L  iF   E           K DISK
001500090108     F                                     INFDS(TAR)
001600090108     Ffitgt00f  UF a E             DISK    commit
001700090108     f                                     rename(fitgt000:fitgtf)
001800021203     Ffiapd01L  IF   E           K DISK
001900081219     Ffiftt02L  IF   E           K DISK
002000121010     Ffifst02L  IF   E           K DISK    rename(FIFTT000:FIFST000) usropn
002100121010      *---
002200090109     d ficn05ds      e ds
002300090112     d ficn06ds      e ds
002400090109     d dgpi          e ds
002500090109     d ficn02ds      e ds
002600081219     d fitgtds       e ds                  extname(fitgt00f)
002700090401     D DS5Aaut       E DS
002800090519     D Dtgtflr       E DS
002900090108     D TAR             DS
003000090108     D  tgtNRR               397    400B 0
003100081219     d*
003200921102     D WLBDAT          DS
003300940906     D  G02DAT                 1      8  0
003400940906     D  G02INV                 9     16  0
003500940906     D  G02ERR                17     17
003600940906     D  G02TGI                18     22  0
003700940504     D                 DS
003800940906     D  AA                     1      4  0
003900940906     D  MM                     5      6  0
004000940906     D  GG                     7      8  0
004100940906     D  DATA                   1      8  0
004200170308      *
004300090324     d isoddt          s               d   datfmt(*iso)
004400090324     d isodst          s               d   datfmt(*iso)
004500090324     d comodo          s               d   datfmt(*iso)
004600081219     d dataiso         s               d   datfmt(*iso)
004700081219     d dataeur         s               d   datfmt(*eur)
004800090108     d savnrr          s                   like(tgtnrr)
004900081219     D*
005000000000     D KPJBA         E DS
005100090402     D dapdflr       E DS
005200131008     D tibs42ds      E DS
005300131008     D uteautds      E DS
005400111124     D dvpocont      E DS
005500081219     D XDTF            S              2  0 DIM(12) CTDATA PERRCD(12)            GG DEI MESI
005600081222     D ddatiute      e ds
005700081222     D azuteds       e ds                  extname(AZUTE00F)
005800081222     D tibs34ds      E DS                  inz
005900170308      *
006000090113     d comtsr          s              1
006100090113     d comctr          s              3  0
006200090401     d tgtpcarc        s                   like(v5pcar)
006300170308     d wSaveIn_1       s             99    inz(*zeros)
006400170308     d wSaveIn_2       s             99    inz(*zeros)
006500170314     d wSaveInKF       s               n   inz
006600170314     d wSaveInKG       s               n   inz
006700120724ab   **
006800120724ab   ** Prototipi.
006900120724ab    **
007000120724ab    /COPY GAITRASRC/SRCCONST,TIBSSOFR
007100120724ab    /DEFINE DFTACTGRP_NO
007200120724ab    /COPY GAITRASRC/SRCPROTOPR,TIBSSOFR
007300120724ab    /UNDEFINE DFTACTGRP_NO
007400120724      *
007500120724ab   D TibsSof_esito...
007600120724ab   D                 S             10I 0 IMPORT
007700120724ab   D idSocieta...
007800120724EDPDCD                 S              3A
007900120724ab   **
008000120724EDPDCd dtVALIDITA      s               D
008100120724EDPDCd VALdatFINE      s               D
008200120724EDPDCd VALdatINIZ      s               D
008300120724      *---------------- Gestione formato video video01----------------------
008400120724ab   C*     Inizializza
008500120724ab    /FREE
008600120724ab     TibsSof_Init();
008700120724ab    /END-FREE
008800930907     C*
008900090710     c                   setoff                                       91
009000170308     c                   eval      *in08 = *off
009100170308      *
009200081219     C                   do        *hival
009300170308      * -?Emissione videata SENZA errori per visualizzare le decodifiche?
009400170308     c                   if        *in90
009500170308     c                   movea     *in           wSaveIn_2
009600170308     c                   movea     wSaveIn_1     *in
009700170308     c                   write     CN10C05
009800170308     c                   movea     wSaveIn_2     *in
009900170308     c                   endif
010000081219     C                   EXFMT     CN10C05
010100081219     C** FINE LAVORO
010200081219     C   Kl              leave
010300081219     C** conferma
010400930811     C** CONTROLLI DELLA PRIMA VIDEATA
010500090113     C  n10              EXSR      CONTRtgt
010600081219     C   90              iter
010700081219     c*  CONFERMA
010800081229     c                   if        (cn02mod = '4' and *inkf) or
010900081229     c                             *inkq
011000081229     C                   call      'FICN04R'
011100081229     C                   parm                    vidaut
011200081229     C                   parm                    cn02sml
011300081229     C                   parm                    comPRG
011400090113     C                   parm                    comtsr
011500090113     C                   parm                    comctr
011600081229     c                   endif
011700090112     c                   if        *inkf or *inkq or *inkg
011800081229     c                   exsr      aggio
011900081229     c                   leave
012000081229     c                   end
012100930811     C*
012200081219     c                   enddo
012300090424     c                   movel(p)  ficn02ds      kpjbu
012400081219     C                   SETON                                        LR
012500090402      *--------------------------------------------------
012600090402     c     srcdf         begsr
012700090402      *--------------------------------------------------
012800090402     c                   clear                   sogpartiva
012900090402     c                   clear                   sogdes
013000090402     c     kfrn          chain     anfrn01l
013100090402     c                   if        %found(anfrn01l)
013200090402     c     frnsogg       chain     ansog01l
013300090402     c                   end
013400090402     c                   endsr
013500081219      *--------------------------------------------------
013600081219     c     *inzsr        begsr
013700081219      *--------------------------------------------------
013800081219
013900081219     C     *ENTRY        PLIST
014000081219     C                   PARM                    KPJBA
014100081222     c     *dtaara       define    §azute        azuteds
014200081222     c     *dtaara       define    §datiute      ddatiute
014300081222     C                   in(E)     *dtaara
014400081222     C                   IF        %Error  or  RSUT = *blanks
014500081222     C                   call      'TIBS34R'
014600081222     C                   parm                    Tibs34Ds
014700081222     C                   in        *dtaara
014800081222     c                   ENDIF
014900081219     c                   movel     kpjbu         ficn02ds
015000081219     c                   movel     cn02div       vi3div
015100111124     c* tabbella VPO record CONT
015200111124     C                   movel     'VPO'         tbecod
015300111124     C                   MOVEL(P)  'CONT'        tbeke1
015400111124     C                   clear                   tbeke2
015500111124     C                   clear                   dvpocont
015600111124     C     KTbe          CHAIN     Tntbe01l
015700111124     c                   if        %found(tntbe01l)
015800111124     C                   movel     tbeuni        dvpocont
015900111124     c                   else
016000111124     c                   clear                   dvpocont
016100090331     c                   end
016200111124     c* reperisce gg min durata allegato
016300111124     C                   movel     '5A'          tblcod
016400111124     C                   MOVEL(P)  'AUT'         tblKEY
016500111124     c                   z-add     1             tblkut
016600111124     c                   z-add     90            giomin
016700111124     C     KTAB          CHAIN     TABEL00F
016800111124     c                   if        %found(tabel00f)
016900111124     C                   movel     tbluni        ds5Aaut
017000111124     C                   z-add     §5AGGTARA     giomin            3 0
017100111124     c                   end
017200090108     c*annullamento solo per edp
017300101209     c                   if        %subst(knmus: 1: 3) = 'EDP'
017400101209     c                   seton                                        44
017500101209     c                   end
017600100907     c* Si è voluto permettere a TUTTI di cancellare le tariffe.
017700100907     c*   NON + solo a EDP !!!!
017800101209     c*                  seton                                        44
017900020125     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
018000020125     C                   TIME                    W0120            14 0
018100020125     C                   MOVE      W0120         WDAT              8 0
018200020125     C*
018300020125     C                   Z-ADD     WDAT          G02DAT
018400020125     C                   MOVEL     *BLANK        G02ERR
018500020125     C                   CALL      'XSRDA8'
018600020125     C                   PARM                    WLBDAT
018700081222     c                   movel     G02INV        oggi              8 0
018800090108     c                   movel     oggi          dataiso
018900090108     c                   adddur    1:*y          dataiso
019000090108     c                   move      dataiso       unanno            8 0
019100111124     C     Ktbe          KLIST
019200111124     C                   KFLD                    tbecod
019300111124     C                   KFLD                    tbeke1
019400111124     C                   KFLD                    tbeke2
019500111124     C     Ktab          KLIST
019600111124     C                   KFLD                    tblkut
019700111124     C                   KFLD                    tblcod
019800111124     C                   KFLD                    tblkey
019900081219     C* ACCESSO fitgt
020000081219     C     KTGT          KLIST
020100090112     C                   KFLD                    cn02pdr
020200081219     C                   KFLD                    cn02SML
020300081219     C     KTGT1         KLIST
020400081219     C                   KFLD                    Cn02pDR
020500081219     C                   KFLD                    Cn02SML
020600081219     C                   KFLD                    COMPRG
020700021203     C* ACCESSO fiapd
020800021203     C     Kapdc         KLIST
020900081219     C                   KFLD                    apdtip
021000081219     c                   KFLD                    vidaut
021100081219     c                   eval      apdtip = 'A'
021200081219     C     Kftt          KLIST
021300081219     C                   KFLD                    vidaut
021400081219     C                   KFLD                    klvl              1
021500090324     C     Ktrs          KLIST
021600090324     C                   KFLD                    apdcsf
021700090324     C                   KFLD                    trsiva
021800090423     C     Ktrs1         KLIST
021900090423     C                   KFLD                    apdcsf
022000090423     C                   KFLD                    trsiva
022100090423     C                   KFLD                    dfc               8 0
022200090423     C     Ktra          KLIST
022300090423     C                   KFLD                    vidaut
022400090423     C                   KFLD                    dfc               8 0
022500090324     C     Kfrn          KLIST
022600091130     C                   KFLD                    frnsocieta
022700090324     C                   KFLD                    frnksc
022800081219     C*
022900081219     C     *LIKE         DEFINE    TGTPrg        COMPrg
023000081219     C     *LIKE         DEFINE    TGTDDT        COMDDT
023100081219     C     *LIKE         DEFINE    TGTDST        COMDST
023200081219     C*
023300081219     C* 19 ON  - UTILIZZARE TARIFFE     DI SIMULAZIONE
023400081219     C* 19 OFF - UTILIZZARE TARIFFE NON DI SIMULAZIONE
023500081219     C     cn02sml       IFEQ      'S'
023600081219     C                   SETON                                        19
023700121010     c                   if        dutlpo <> 'S'
023800121010     c                   open      fifst02l
023900121010     c                   end
024000081219     C                   ENDIF
024100081219     C*
024200081229     c     cn02mod       comp      '5'                                    10
024300090423     c   10              eval      vidtes = ' Interrogazione tariffe Aut Città '
024400090423     c  n10              eval      vidtes = '    Gestione tariffe Aut Città    '
024500090112     c* inserimento
024600090112     c     cn02mod       comp      '1'                                    43
024700020125     C*
024800081219     c                   movel     cn02div       vi3div
024900081219     c                   movel     cn02pdr       vidaut
025000081222     c                   movel     cn02prg       comprg
025100081219     C     kapdc         CHAIN     fiapd000
025200081219     c                   if        %found(fiapd01l)
025300090706     C                   MOVEL     APDRSf        DESPDR
025400090402     C                   MOVEL     APDflr        Dapdflr
025500090402     c                   z-add     §APDMCP       v5pkl
025600081219     c                   else
025700081219     C                   MOVEL     *all'?'       DESPDR
025800081219     c                   end
025900090402     c* reperisco la p.iva  e la ragione sociale
026000090324     c                   movel     *all'0'       frnksc
026100090324     c                   move      apdksc        frnksc
026200091130     c                   movel     apdcsf        frnsocieta
026300090402     c                   exsr      srcdf
026400090402     c                   movel     sogpartiva    trsiva
026500081219     c*
026600081229     c                   if        cn02mod = '1'
026700090108     C     Ktgt          setgt     fitgt000                           32
026800081219     c                   else
026900090108     C     Ktgt1         setgt     fitgt000                           32
027000081219     c                   end
027100081219     C*
027200090108     c                   clear                   savnrr
027300081219     c*
027400081229     c                   if        cn02mod = '1'
027500081219     c     ktgt          readpe    fitgt000
027600081219     c                   else
027700081229     c                   if        cn02mod = '5'
027800090108     c     ktgt1         readpe    fitgt000
027900081222     c                   else
028000081219     c     ktgt1         readpe    fitgt000
028100090108     c     tgtnrr        chain(e)  fitgt00f
028200090108     C* RECORD ALLOCATO
028300090108     C                   IF        %error
028400090427     c                   eval      CN02ERR = '1'
028500090427     c                   eval      CN02MSG = 'Tariffa non manutenzionabile-
028600090427     c                              perchè già in uso'
028700090427     c                   seton                                        lr
028800090427     c                   movel(p)  ficn02ds      kpjbu
028900090427     c                   return
029000090108     c                   else
029100090108     c                   eval      savnrr = tgtnrr
029200090108     C                   ENDIF
029300081219     c                   end
029400081222     c                   end
029500081219     c*
029600090423     c                   if        %eof(fitgt01l)
029700090423     c                   clear                   fitgtds
029800090423     c                   end
029900090427     c* controlli per annullamento
030000090427     c                   if        cn02mod = '4' and tgtdcn <> 0
030100090427     c                   eval      CN02ERR = '1'
030200090427     c                   eval      CN02MSG = 'Non si può annulare una -
030300090427     c                             tariffa già stampata'
030400090427     c                   seton                                        lr
030500090427     c                   movel(p)  ficn02ds      kpjbu
030600090427     c                   return
030700090427     c                   end
030800081219     C* CARICO DATI A VIDEO
030900081219     C* inserimento
031000081222     c                   if        cn02mod = '1'
031100081219     c                   if        not %eof(fitgt01l)
031200090423     c* controllo se si può aprire un nuovo progressivo solo se non
031300090423     c* annullato. se lo fosse vuol dire che i controlli li aveva già
031400090423     c* passati prima
031500090424     c                   if        tgtatb = ' ' and cn02sml = ' '
031600090423     c                   if        tgtdts = 0
031700090423     c                   eval      CN02ERR = '1'
031800090423     c                   eval      CN02MSG = 'Errore, si dev-
031900090423     c                             e prima confermare il precedente'
032000090423     c                   seton                                        lr
032100090423     c                   movel(p)  ficn02ds      kpjbu
032200090423     c                   return
032300090423     c                   else
032400090508     c* controllo esistenza del contratto solo se non cambiato fornitore
032500090508     c                   movel     *all'0'       com08             8
032600090508     c                   move      apdksc        com08
032700090508     c                   if        apdcsf = tgtsoc
032800090508     c                             and com08 = tgtcdf
032900090423     c     ktrs1         chain     aitrs02l
033000090423     c                   if        not %found(aitrs02l)
033100090423     c                   eval      CN02ERR = '1'
033200090423     c                   eval      CN02MSG = 'Errore, -
033300090423     c                             occorre avere prima un contratto'
033400090423     c                   seton                                        lr
033500090423     c                   movel(p)  ficn02ds      kpjbu
033600090423     c                   return
033700090423     c                   end
033800090508     c                   end
033900090423     c                   end
034000090423     c                   end
034100090423     c*
034200081222     c     tgtprg        add       1             vi3prg
034300081222     c                   movel     tgtdst        dataiso
034400081222     c                   adddur    1:*d          dataiso
034500081222     c                   movel     dataiso       dataeur
034600081222     c                   movel     dataeur       v5ddt
034700081219     c                   else
034800090114     c                   eval      vi3prg = 200
034900081219     c                   end
035000081222     c                   clear                   fitgtds
035100081219     c                   end
035200081219     C* modifica
035300131024     c                   setoff                                       20  15
035400090519     c                   movel     tgtflr        dtgtflr
035500081229     c                   if        cn02mod <>'1'
035600081219     c                   move      cn02prg       vi3prg
035700090521     c     §tgtris       comp      'R'                                    20
035800131024     c                   seton                                        15
035900081219     c                   end
036000081222     c* Controllo se abilitato alla manutenzione delle tariffe
036100081219     c* solo se è impostata la data di stampa
036200131024     c                   setoff                                       13
036300090521     c                   if        tgtsml = *blank
036400131008     c                   clear                   tibs42ds
036500131008     c                   movel     vidaut        i42pge
036600131008     c                   call      'TIBS42R'
036700131008     c                   parm                    tibs42ds
036800131008     c                   movel     o42uni        uteautds
036900081219      * se abilitato con *in13 abilito la manutenzione della data scadenza
037000081219      * con *in15 proteggo gli altri campi
037100081219     c                   if        §autmtc <>'S'
037200081219     c                   seton                                        13
037300081219     c                   end
037400090521     C                   else
037500090521     c* in simulazione e manutenzione proteggo comunque perchè non verrà
037600090521     c* stampato
037700090521     C                   seton                                        15
037800081219     c                   end
037900081219      *record esistente
038000081219     c                   if        Tgtddt > 0
038100081219     c                   move      tgtddt        dataiso
038200081219     c                   move      dataiso       dataeur
038300081219     c                   move      dataeur       v5ddt
038400081219     c                   else
038500081219     c                   z-add     0             v5ddt
038600081219     c                   end
038700081219     c                   if        Tgtdst > 0
038800081219     c                   move      Tgtdst        dataiso
038900081219     c                   move      dataiso       dataeur
039000081219     c                   move      dataeur       v5dst
039100081219     c                   else
039200081219     c                   z-add     31122039      v5dst
039300081219     c                   end
039400081219      *
039500081219     c                   if        Tgtduv > 0
039600081219     c                   move      Tgtduv        dataiso
039700081219     c                   move      dataiso       dataeur
039800081219     c                   move      dataeur       v5duv
039900081219     c                   else
040000081219     c                   z-add     0             v5duv
040100081219     c                   end
040200090320     c                   setoff                                       17
040300090320      * se Data Stampa <> 0 non è + possibile fare modifiche
040400090320      * alla tariffa.
040500081219     c                   if        Tgtdts > 0
040600090320     c                   seton                                        17
040700081219     c                   move      tgtdts        dataiso
040800081219     c                   move      dataiso       dataeur
040900081219     c                   move      dataeur       v5dts
041000081219     c                   else
041100081219     c                   z-add     0             v5dts
041200081219     c                   end
041300081219      *
041400090320     c                   if        Tgtdcn > 0
041500081219     c                   move      Tgtdcn        dataiso
041600081219     c                   move      dataiso       dataeur
041700081219     c                   move      dataeur       v5dcn
041800081219     c                   else
041900081219     c                   z-add     0             v5dcn
042000081219     c                   end
042100120104     c* ristampa
042200120104     c* se è una ristampa si deve riempire la "data di stampa" con
042300120104     c* la 1° data certa, e mettere nella "data di ristampa"
042400120104     c* l'ultima data certa ...
042500120104     c                   if        §tgtris = 'R'
042600120104     c                   if        §Tgtunodc <> ' '
042700120104     c                   move      §Tgtunodc     v5unodc
042800120104     c                   move      v5unodc       dataiso
042900120104     c                   move      dataiso       dataeur
043000120104     c                   move      dataeur       v5unodc
043100120104     c                   else
043200120104     c                   z-add     0             v5unodc
043300120104     c                   end
043400120104     c                   eval      v5unodc = v5dcn
043500120104     c                   if        §Tgtunodc <> ' '
043600120104     c                   move      dataeur       v5dcn
043700120104     c                   else
043800120104     c* tariffe prima di dicembre 2011 quando non esisteva ancora
043900120104     c* il campo della prima data certa
044000120104     c                   clear                   v5dcn
044100120104     c                   end
044200120104     c                   end
044300090320      *
044400090320     c                   if        Tgtdrc > 0
044500090320     c                   move      Tgtdrc        dataiso
044600090320     c                   move      dataiso       dataeur
044700090320     c                   move      dataeur       v5drc
044800090320     c                   else
044900090324     c                   z-add     0             v5drc
045000090320     c                   end
045100081219
045200081219     c                   movel     tgtdcv        v5dcv
045300090402     c                   move      tgtsoc        v5soc
045400090402     c                   move      tgtcdf        v5cdf
045500090402     c                   move      tgtcdf        frnksc
045600091130     c                   movel     tgtsoc        frnsocieta
045700090402     c                   exsr      srcdf
045800090402     c                   movel     sogdes        desfor
045900090402     c                   z-add     tgtfil        v5fil
046000090402     c                   z-add     tgtnrc        v5nrc
046100081222     c                   z-add     tgtkm         v5km
046200090109     c                   z-add     tgtpcar       v5pcar
046300111215     c*    v5pcar        comp      0                                      88
046400111215     c                   seton                                        88
046500090320     c                   z-add     tgttmpc       v5tmpc
046600090320     c                   z-add     tgttmps       v5tmps
046700090401     c                   if        tgtpkl <> 0
046800090401     c                   z-add     tgtpkl        v5pkl
046900090401     c                   end
047000170308      * -?Salvataggio iniziale indicatori (senza errori)?
047100170308     c                   movea     *in           wSaveIn_1
047200170308      *
047300090113     c                   if        not *in10
047400081229     C                   exsr      contrtgt
047500081229     c                   end
047600081219
047700081219     C                   endsr
047800081219     C*---------------------------------------------------------------*
047900081219     C     aggio         begsr
048000081219     C*---------------------------------------------------------------*
048100081222     c                   clear                   fitgtds
048200081222     c                   eval      comprg = vi3prg
048300090108     c                   if        savnrr <> 0
048400090108     c     savnrr        chain     fitgt00f
048500090108     c                   end
048600081222     c                   move      vidaut        tgtpdr
048700081222     c                   movel     cn02sml       tgtsml
048800081222     c                   movel     vi3div        tgtdiv
048900081222     c                   move      vi3prg        tgtprg
049000081222     c                   if        v5duv = 0
049100081222     c                   movel     oggi          tgtduv
049200081222     c                   else
049300081222     c                   movel     v5duv         dataeur
049400081222     c                   movel     dataeur       dataiso
049500081222     c                   movel     dataiso       tgtduv
049600081222     c                   end
049700081222     c                   movel     v5ddt         dataeur
049800081222     c                   movel     dataeur       dataiso
049900081222     c                   movel     dataiso       tgtddt
050000081222     c                   movel     v5dst         dataeur
050100081222     c                   movel     dataeur       dataiso
050200081222     c                   movel     dataiso       tgtdst
050300081222     c                   if        v5dts <> 0
050400081222     c                   movel     v5dts         dataeur
050500081222     c                   movel     dataeur       dataiso
050600081222     c                   movel     dataiso       tgtdts
050700081222     c                   end
050800081222     c                   movel     v5km          tgtkm
050900081222     c                   movel     v5dcv         tgtdcv
051000081222     c                   movel     dutute        tgtpru
051100090401     c                   z-add     v5pkl         tgtpkl
051200090108     c                   if        savnrr = 0
051300111222     c                   clear                   dtgtflr
051400111222     c                   eval      tgtflr = dtgtflr
051500090324     c* gli altri dati vengono memorizzati nel momento della stampa
051600090108     c                   write     fitgtf
051700090112     c* se richiesta duplica
051800090112     c                   if        *inkg
051900090112     c                   clear                   ficn06ds
052000090112     C                   MOVEL     VIdaut        filnew
052100090112     C                   MOVE      VIdaut        pdrnew
052200090112     c                   z-add     vi3prg        prgnew
052300090112     c                   movel     cn02sml       smlnew
052400090424     c                   movel(p)  ficn06ds      kpjbu
052500090112     c                   call      'FICN85R'
052600090112     c                   parm                    kpjba
052700090112     c                   end
052800081222     c                   else
052900090112     c                   if        cn02mod ='4' or *inkq
053000100526     c                   delete    fitgtf
053100100526     c                   else
053200090108     c                   update    fitgtf
053300081229     C* AGGIORNO I RECORD DEL FIFGT SE SONO IN MODIFICA
053400090331     c* commit
053500090331     c                   eval      comcom = *on
053600081229     C                   call      'FICN03R'
053700081229     C                   parm                    vidaut
053800081229     C                   parm                    cn02sml
053900081229     C                   parm                    comPRG
054000090331     C                   parm                    comcom            1
054100090108     c                   end
054200100526     c                   end
054300090112     c                   commit
054400081219     C                   endsr
054500081219     C*---------------------------------------------------------------*
054600081219     C     contrtgt      begsr
054700081219     C*---------------------------------------------------------------*
054800081219     C                   SETOFF                                       90
054900081219     C****  DATA DECORRENZA TARIFFA  ****
055000081219     C                   Z-ADD     V5ddt         G02DAT
055100081219     C                   MOVEL     *BLANKS       G02ERR
055200081219     C                   CALL      'XSRDA8'
055300081219     C                   PARM                    WLBDAT
055400081219     C     G02ERR        IFEQ      '1'
055500081219     C                   SETON                                        4590
055600081219     C                   leavesr
055700081219     C                   ELSE
055800081219     C                   MOVE      G02INV        COMDDT
055900081219     C                   ENDIF
056000090324     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
056100090324     C                   Z-ADD     G02DAT        V5ddt
056200170308     c                   write     CN10C05
056300090423     c* controllo data decorrenza solo se manutenzionabile
056400170308if  1c                   if        not *in15 and
056500090504     c                             cn02mod <> '4'
056600090324     c* la decorrenza non può essere maggiore del 2039
056700090108     c                   if        comddt > 20391231
056800090108     C                   SETON                                        4590
056900090108     C                   leavesr
057000090108     c                   end
057100090324     c* le supertestate devono essere inserite solo con decorrenze
057200090421     c* successive alla trascodifica
057300090421     c                   if        comddt < 20090501
057400090113     C                   SETON                                        4590
057500090113     C                   leavesr
057600090113     c                   end
057700111124     c* le supertestate devono essere inserite solo se non presente
057800111124     c* la data di blocco impostata nella tabella VPO record CONT
057900111124     c                   if        comddt >= §vpodpa
058000111124     C                   SETON                                        4590
058100111124     C                   leavesr
058200111124     c                   end
058300090323     c* ???? obbligatorio inizio mese eccetto le prime supertestate
058400090323     c* create dalla trascodifica
058500090331     c*                  move      comddt        isoddt
058600090331     c*                  extrct    isoddt:*d     inizio            2 0
058700090331     c*                  if        comddt > 20090201 and inizio <> 01
058800090331     C*                  SETON                                        9490
058900090331     C*                  leavesr
059000090331     c*                  end
059100170308      * -?La decorrenza non può essere di oltre 3 mesi successiva ad oggi?
059200170308     c                   If        %date( comDDT : *iso ) >
059300170308     c                             %date() + %months(3)
059400170308     c                   eval      *in53     = *on
059500170308     c                   eval      *in90     = *on
059600170308     c                   leavesr
059700170308     c                   EndIf
059800170314      * -?Si chiede conferma (F8) per una decorrenza di oltre 1 mese?
059900170314      *  ?successiva ad oggi (SE premuto F6 o F7)?
060000170314     c*//                If        NOT *inKH       and
060100170314     c                   If        ( *inKF    or   *inKG ) and
060200170308     c                             %date( comDDT : *iso ) >
060300170308     c                             %date() + %months(1)
060400170314     c                   eval      wSaveInKF = *inKF
060500170314     c                   eval      wSaveInKG = *inKG
060600170308     c                   eval      %subst( wSaveIn_1 : 08 : 1 ) = *on
060700170308     c                   eval      *in08     = *on
060800170308     c                   eval      *in51     = *on
060900170308     c                   eval      *in90     = *on
061000170308     c                   leavesr
061100170308     c                   EndIf
061200170314     c                   If        *inKH           and
061300170308     c                             %date( comDDT : *iso ) >
061400170308     c                             %date() + %months(1)
061500170314     c                   eval      *inKF = wSaveInKF
061600170314     c                   eval      *inKG = wSaveInKG
061700170308     c                   EndIf
061800170308      *
061900090512     c* verifico che non sia presente un contratto attivo con decorrenza
062000090512     c* successiva. Nel caso non esita si bloccherranno le valorizzaz.
062100090423     c     ktrs1         setll     aitrs02l
062200170308do  2c                   do        *hival
062300090323     c     ktrs          reade     aitrs02l
062400090423     c                   if        %eof(aitrs02l)
062500090423     c                   leave
062600090423     c                   end
062700090423     c                   if        trsann <> ' '
062800090423     c                   iter
062900090423     c                   end
063000090423     c                   if        trsdfc <> 0
063100090423     c                   leave
063200090423     c                   end
063300090423     c                   if        trsdec > comddt
063400090324     C                   SETON                                        9590
063500090323     C                   leavesr
063600090323     c                   end
063700090512     c* verifico che l'autista sia accreditato nel contratto corretto
063800090423     c     ktra          setll     aitra03l
063900170308do  3c                   do        *hival
064000090423     c     vidaut        reade     aitra03l
064100090423     c                   if        %eof(aitra03l)
064200090423     c                   leave
064300090423     c                   end
064400090423     c                   if        traann <> ' '
064500090423     c                   iter
064600090423     c                   end
064700090609     c                   if        tranrc <>trsnrc
064800090512     c                   iter
064900090512     c                   end
065000170308if  4c                   if        tradfi = 0
065100090423     c                   if        tradin > comddt
065200090423     C                   SETON                                        9590
065300090423     C                   leavesr
065400090423     c                   else
065500090423     c                   leave
065600090512     c                   end
065700170308x   4c                   else
065800090423     C                   SETON                                        9890
065900090423     C                   leavesr
066000170308e   4c                   end
066100170308e   3c                   enddo
066200170308e   2c                   enddo
066300170308e   1c                   end
066400081219     C*
066500081219     C****  DATA SCADENZA TARIFFA  ****
066600081219     C                   Z-ADD     V5dst         G02DAT
066700081219     C                   MOVEL     *BLANKS       G02ERR
066800081219     C                   CALL      'XSRDA8'
066900081219     C                   PARM                    WLBDAT
067000081219     C     G02ERR        IFEQ      '1'
067100081219     C                   SETON                                        4690
067200081219     C                   leavesr
067300081219     C                   ELSE
067400081219     C                   MOVE      G02INV        COMDST
067500081219     C                   ENDIF
067600170308     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
067700170308     C                   Z-ADD     G02DAT        V5dst
067800170308     c                   write     CN10C05
067900081222     c                   if        comdst > 20391231
068000081222     C                   SETON                                        4690
068100081222     C                   leavesr
068200081222     c                   end
068300090108     c                   if        comdst<> 20391231 and comdst > unanno
068400090324     C                   SETON                                        9390
068500090108     C                   leavesr
068600090108     c                   end
068700120724      * controllo congruenza scadenza con validità società
068800130110     c***************    if        comdst <> 20391231 and
068900130110      * avendo spostato il controllo della data validità con la data scadenza anzichè decorrenza
069000130110      * occorre eliminare il test(x solo <> da 20391231)altrimenti qualcuno potrebbe riaprire
069100130110      * una tariffa sulla società precedente riabilitandola con scadenza infinito e passerebbe
069200130110      * oltre questo controllo.
069300130110     c                   if        comdst <> 0 and v5soc <> ' '
069400120724     c                   movel     vidaut        adtpdrf           3 0
069500121218     c                   move      comdst        dtvalidita
069600120724ab   C                   exsr      TEST_societa
069700120724     c                   move      valdatfine    valdatfineW       8 0
069800121219     c*
069900121219     c                   if          idsocieta <> v5soc
070000120724ab   c                   seton                                        4290
070100121219ab   C                   end
070200121219     c*
070300121219     c                   if           *in42
070400121219     c                   leavesr
070500120724ab   C                   end
070600121219ab   C                   end
070700120724     ***
070800090331     c* la data di scadenza deve essere fine mese ????
070900090331     c*                  move      comdst        isodst
071000090331     c*    isodst        adddur    1:*d          comodo
071100090331     c*                  extrct    comodo:*d     inizio            2 0
071200090331     c*                  if        inizio <> 01
071300090331     C*                  SETON                                        9690
071400090331     C*                  leavesr
071500090331     c*                  end
071600081219     C*
071700081219     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
071800081219     C                   Z-ADD     G02DAT        V5DST
071900081219     C*
072000081219     C* LA DATA SCADENZA DEVE ESSERE MAGGIORE DELLA DATA DECORRENZA
072100081219     C     COMDST        IFLT      COMDDT
072200081219     C                   SETON                                        4790
072300081219     C                   leavesr
072400081219     C                   ENDIF
072500090323     c* la tariffa deve aver validità per almeno xx giorni
072600090323     c                   move      comddt        isoddt
072700090323     c                   move      comdst        isodst
072800090324     c     isodst        subdur    isoddt        ggvalidi:*D       4 0
072900090331     c                   if        ggvalidi < giomin
073000090324     C                   SETON                                        9790
073100090323     C                   leavesr
073200090323     c                   end
073300121010      **
073400121010      **  se SONO in SEDE e ho chiesto la simulazione
073500121010      **  deve saltare questo controllo incrociato con le VAlorizzazioni simulate
073600121010      **  poichè le valorizzazioni simulate al momento sono solo in ambiente di Filiale
073700121010     c                   IF        dutlpo <> 'S'
073800121010     c                             or cn02sml = ' '
073900121010      **
074000081219      *verifica se esistono conteggi valorizzati o confermati x la data scadenza
074100081219     c                   move      'C'           klvl
074200121010     c                   if          cn02sml = ' '
074300081219     c     kftt          setgt     fiftt02l
074400081219     c     kftt          readpe    fiftt02l                               59
074500121010     c                   else
074600121010     c     kftt          setgt     fifst02l
074700121010     c     kftt          readpe    fifst02l                               59
074800121010     c                   end
074900081219     c                   if        not *in59
075000081219     c                   if        fttddc >  comdst
075100090710     c                   seton                                        6090
075200081219     C                   leavesr
075300090710     c                   endif
075400090710     c* se esitono valorizzazioni avviso prima di annullare (solo x edp)
075500101108     c                   if        fttddc >= comddt and
075600101108     c                             fttddc <= comdst and
075700090427     c                             cn02mod = '4'
075800090710     c                   seton                                        61
075900090710     c  n91              seton                                        9091
076000090710     c  n91              leavesr
076100090427     c                   endif
076200081219     c                   endif
076300090710     c*
076400081219     c                   move      'V'           klvl
076500121010     c                   if          cn02sml = ' '
076600081219     c     kftt          setgt     fiftt02l
076700081219     c     kftt          readpe    fiftt02l                               59
076800121010     c                   else
076900121010     c     kftt          setgt     fifst02l
077000121010     c     kftt          readpe    fifst02l                               59
077100121010     c                   end
077200091112     c                   if        not *in59 and fttfla = ' '
077300081219     c                   if        fttddc >  comdst
077400090710     c                   seton                                        6090
077500081219     C                   leavesr
077600090710     c                   endif
077700090710     c* se esitono valorizzazioni avviso prima di annullare (solo x edp)
077800101108     c                   if        fttddc >= comddt and
077900101108     c                             fttddc <= comdst and
078000090427     c                             cn02mod = '4'
078100090710     c                   seton                                        61
078200090710     c  n91              seton                                        9091
078300090710     c  n91              leavesr
078400090427     c                   endif
078500081219     c                   endif
078600081219      *
078700121010     c                   END
078800121010      *
078900090114     C* LA SCADENZA DEL PROGRESSIVO che sto controllando DEVE ESSERE MINORE
079000081219     C*   DELLA DATA DECORRENZA DEL PROGRESSIVO SEGUENTE
079100081219     C     vi3prg        ADD       1             COMPRG
079200081219     C     KtGT1         SETLL     fitgt000
079300090112     C     KtGT          READE     fitgt000                               32
079400081219     C*
079500081219     C     *IN32         DOWEQ     *OFF
079600081219     C     tGTATB        IFEQ      ' '
079700090114     C                   move      comdst        dataiso
079800090114     c                   adddur    1:*d          dataiso
079900090114     C                   move      dataiso       DATA
080000090114     C     data          IFne      tGTDDT
080100081219     C                   SETON                                        4890
080200081219     C                   leavesr
080300081219     C                   ELSE
080400081219     C                   SETON                                        32
080500081219     C                   ENDIF
080600081219     C                   ENDIF
080700081219     C*
080800090112     C  N32KtGT          READE     fitgt000                               32
080900081219     C                   ENDDO
081000081219     C*
081100090114     C* LA DECORRENZA DEL PROGRESSIVO che sto controllando DEVE ESSERE
081200081219     C*   MAGGIORE DELLA DATA SCADENZA DEL PROGRESSIVO PRECEDENTE
081300090113     C                   z-add     vi3prg        COMPRG
081400081219     C     KtGT1         SETLL     fitgt000
081500090113     C     KtGT          READpe    fitgt000                               32
081600081219     C*
081700081219     C     *IN32         DOWEQ     *OFF
081800081219     C     tGTATB        IFEQ      ' '
081900081219     C     COMDDT        IFLE      tGTDST
082000081219     C                   SETON                                        4990
082100081219     C                   leavesr
082200081219     C                   ELSE
082300090114     C* LA DECORRENZA PROGRESSIVO che sto controllando DEVE ESSERE MAGGIORE
082400081219     C*   DI 1 GIORNO DELLA DATA SCADENZA PROGRESSIVO PRECEDENTE
082500090113     C                   move      tGTDST        dataiso
082600090113     c                   adddur    1:*d          dataiso
082700090113     C                   move      dataiso       DATA
082800081219     C*
082900081219     C     COMDDT        IFNE      DATA
083000081219     C                   SETON                                        4990
083100081219     C                   leavesr
083200081219     C                   ENDIF
083300081219     C*
083400081219     C                   SETON                                        32
083500081219     C                   ENDIF
083600081219     C                   ENDIF
083700081219     C*
083800090113     C  N32KtGT          READpe    fitgt000                               32
083900081219     C                   ENDDO
084000090108     C* km obbligatori
084100090417     C*    v5km          IFeq      0
084200090417     C*                  SETON                                        5090
084300090417     C*                  leavesr
084400090417     C*                  ENDIF
084500090109     C* se variato prezzo ingresso obbligo a far scadere la tariffa
084600090109     c* solo se non sono in inserimento
084700090109     c                   if        cn02mod <> '1' and comdst > oggi
084800090109     c                   clear                   ficn05ds
084900090109     c                   call      'FICN05R'
085000090109     c                   parm                    ficn05ds
085100090109     c                   eval      dgpi = o05gpi
085200090112     c                   z-add     D§GPIGPI      tgtpcarc
085300090112     C     tgtpcarc      IFne      v5pcar
085400090324     c     v5pcar        andne     0
085500111213     C*                  SETON                                        5290
085600111213     C*                  leavesr
085700090109     C                   ENDIF
085800090109     C                   ENDIF
085900111214     c*    v5pcar        comp      0                                      88
086000111214     c                   seton                                        88
086100081219     c                   endsr
086200120724ab   C**************************************************************************
086300120724ab    * Controlla se nel periodo successivo è prevista un'altra società
086400120724ab   C**************************************************************************
086500120724ab   C     TEST_SOCieta  begSR
086600120724ab    *
086700120724ab    /FREE
086800120724ab
086900120724EDPDC  idSocieta = TibsSof_GetIdSocietaByIdFiliale( adtpdrf
087000120724EDPDC                                             : dtVALIDITA
087100120724EDPDC                                             : 'O'
087200120724EDPDC                                             : valDatIniz
087300120724EDPDC                                             : valDatFine
087400120724ab                                                );
087500120724ab
087600120724EDPDC  IF idSocieta = *BLANK OR TibsSof_esito < TIBSSOF_ESITO_OK;
087700120724EDPDC    // Gestire l'errore.
087800121219           *in42 = '1';
087900120724ab     ENDIF;
088000120724ab
088100120724ab    /END-FREE
088200120724ab   C                   ENDSR
088300081219**   XDTF
088400081219312831303130313130313031

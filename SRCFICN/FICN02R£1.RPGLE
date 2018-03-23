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
004200090324     d isoddt          s               d   datfmt(*iso)
004300090324     d isodst          s               d   datfmt(*iso)
004400090324     d comodo          s               d   datfmt(*iso)
004500081219     d dataiso         s               d   datfmt(*iso)
004600081219     d dataeur         s               d   datfmt(*eur)
004700090108     d savnrr          s                   like(tgtnrr)
004800081219     D*
004900000000     D KPJBA         E DS
005000090402     D dapdflr       E DS
005100131008     D tibs42ds      E DS
005200131008     D uteautds      E DS
005300111124     D dvpocont      E DS
005400081219     D XDTF            S              2  0 DIM(12) CTDATA PERRCD(12)            GG DEI MESI
005500081222     D ddatiute      e ds
005600081222     D azuteds       e ds                  extname(AZUTE00F)
005700081222     D tibs34ds      E DS                  inz
005800090113     d comtsr          s              1
005900090113     d comctr          s              3  0
006000090401     d tgtpcarc        s                   like(v5pcar)
006100120724ab   **
006200120724ab   ** Prototipi.
006300120724ab    **
006400120724ab    /COPY GAITRASRC/SRCCONST,TIBSSOFR
006500120724ab    /DEFINE DFTACTGRP_NO
006600120724ab    /COPY GAITRASRC/SRCPROTOPR,TIBSSOFR
006700120724ab    /UNDEFINE DFTACTGRP_NO
006800120724      *
006900120724ab   D TibsSof_esito...
007000120724ab   D                 S             10I 0 IMPORT
007100120724ab   D idSocieta...
007200120724EDPDCD                 S              3A
007300120724ab   **
007400120724EDPDCd dtVALIDITA      s               D
007500120724EDPDCd VALdatFINE      s               D
007600120724EDPDCd VALdatINIZ      s               D
007700120724      *---------------- Gestione formato video video01----------------------
007800120724ab   C*     Inizializza
007900120724ab    /FREE
008000120724ab     TibsSof_Init();
008100120724ab    /END-FREE
008200930907     C*
008300090710     c                   setoff                                       91
008400081219     C                   do        *hival
008500081219     C                   EXFMT     CN10C05
008600081219     C** FINE LAVORO
008700081219     C   Kl              leave
008800081219     C** conferma
008900930811     C** CONTROLLI DELLA PRIMA VIDEATA
009000090113     C  n10              EXSR      CONTRtgt
009100081219     C   90              iter
009200081219     c*  CONFERMA
009300081229     c                   if        (cn02mod = '4' and *inkf) or
009400081229     c                             *inkq
009500081229     C                   call      'FICN04R'
009600081229     C                   parm                    vidaut
009700081229     C                   parm                    cn02sml
009800081229     C                   parm                    comPRG
009900090113     C                   parm                    comtsr
010000090113     C                   parm                    comctr
010100081229     c                   endif
010200090112     c                   if        *inkf or *inkq or *inkg
010300081229     c                   exsr      aggio
010400081229     c                   leave
010500081229     c                   end
010600930811     C*
010700081219     c                   enddo
010800090424     c                   movel(p)  ficn02ds      kpjbu
010900081219     C                   SETON                                        LR
011000090402      *--------------------------------------------------
011100090402     c     srcdf         begsr
011200090402      *--------------------------------------------------
011300090402     c                   clear                   sogpartiva
011400090402     c                   clear                   sogdes
011500090402     c     kfrn          chain     anfrn01l
011600090402     c                   if        %found(anfrn01l)
011700090402     c     frnsogg       chain     ansog01l
011800090402     c                   end
011900090402     c                   endsr
012000081219      *--------------------------------------------------
012100081219     c     *inzsr        begsr
012200081219      *--------------------------------------------------
012300081219
012400081219     C     *ENTRY        PLIST
012500081219     C                   PARM                    KPJBA
012600081222     c     *dtaara       define    §azute        azuteds
012700081222     c     *dtaara       define    §datiute      ddatiute
012800081222     C                   in(E)     *dtaara
012900081222     C                   IF        %Error  or  RSUT = *blanks
013000081222     C                   call      'TIBS34R'
013100081222     C                   parm                    Tibs34Ds
013200081222     C                   in        *dtaara
013300081222     c                   ENDIF
013400081219     c                   movel     kpjbu         ficn02ds
013500081219     c                   movel     cn02div       vi3div
013600111124     c* tabbella VPO record CONT
013700111124     C                   movel     'VPO'         tbecod
013800111124     C                   MOVEL(P)  'CONT'        tbeke1
013900111124     C                   clear                   tbeke2
014000111124     C                   clear                   dvpocont
014100111124     C     KTbe          CHAIN     Tntbe01l
014200111124     c                   if        %found(tntbe01l)
014300111124     C                   movel     tbeuni        dvpocont
014400111124     c                   else
014500111124     c                   clear                   dvpocont
014600090331     c                   end
014700111124     c* reperisce gg min durata allegato
014800111124     C                   movel     '5A'          tblcod
014900111124     C                   MOVEL(P)  'AUT'         tblKEY
015000111124     c                   z-add     1             tblkut
015100111124     c                   z-add     90            giomin
015200111124     C     KTAB          CHAIN     TABEL00F
015300111124     c                   if        %found(tabel00f)
015400111124     C                   movel     tbluni        ds5Aaut
015500111124     C                   z-add     §5AGGTARA     giomin            3 0
015600111124     c                   end
015700090108     c*annullamento solo per edp
015800101209     c                   if        %subst(knmus: 1: 3) = 'EDP'
015900101209     c                   seton                                        44
016000101209     c                   end
016100100907     c* Si è voluto permettere a TUTTI di cancellare le tariffe.
016200100907     c*   NON + solo a EDP !!!!
016300101209     c*                  seton                                        44
016400020125     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
016500020125     C                   TIME                    W0120            14 0
016600020125     C                   MOVE      W0120         WDAT              8 0
016700020125     C*
016800020125     C                   Z-ADD     WDAT          G02DAT
016900020125     C                   MOVEL     *BLANK        G02ERR
017000020125     C                   CALL      'XSRDA8'
017100020125     C                   PARM                    WLBDAT
017200081222     c                   movel     G02INV        oggi              8 0
017300090108     c                   movel     oggi          dataiso
017400090108     c                   adddur    1:*y          dataiso
017500090108     c                   move      dataiso       unanno            8 0
017600111124     C     Ktbe          KLIST
017700111124     C                   KFLD                    tbecod
017800111124     C                   KFLD                    tbeke1
017900111124     C                   KFLD                    tbeke2
018000111124     C     Ktab          KLIST
018100111124     C                   KFLD                    tblkut
018200111124     C                   KFLD                    tblcod
018300111124     C                   KFLD                    tblkey
018400081219     C* ACCESSO fitgt
018500081219     C     KTGT          KLIST
018600090112     C                   KFLD                    cn02pdr
018700081219     C                   KFLD                    cn02SML
018800081219     C     KTGT1         KLIST
018900081219     C                   KFLD                    Cn02pDR
019000081219     C                   KFLD                    Cn02SML
019100081219     C                   KFLD                    COMPRG
019200021203     C* ACCESSO fiapd
019300021203     C     Kapdc         KLIST
019400081219     C                   KFLD                    apdtip
019500081219     c                   KFLD                    vidaut
019600081219     c                   eval      apdtip = 'A'
019700081219     C     Kftt          KLIST
019800081219     C                   KFLD                    vidaut
019900081219     C                   KFLD                    klvl              1
020000090324     C     Ktrs          KLIST
020100090324     C                   KFLD                    apdcsf
020200090324     C                   KFLD                    trsiva
020300090423     C     Ktrs1         KLIST
020400090423     C                   KFLD                    apdcsf
020500090423     C                   KFLD                    trsiva
020600090423     C                   KFLD                    dfc               8 0
020700090423     C     Ktra          KLIST
020800090423     C                   KFLD                    vidaut
020900090423     C                   KFLD                    dfc               8 0
021000090324     C     Kfrn          KLIST
021100091130     C                   KFLD                    frnsocieta
021200090324     C                   KFLD                    frnksc
021300081219     C*
021400081219     C     *LIKE         DEFINE    TGTPrg        COMPrg
021500081219     C     *LIKE         DEFINE    TGTDDT        COMDDT
021600081219     C     *LIKE         DEFINE    TGTDST        COMDST
021700081219     C*
021800081219     C* 19 ON  - UTILIZZARE TARIFFE     DI SIMULAZIONE
021900081219     C* 19 OFF - UTILIZZARE TARIFFE NON DI SIMULAZIONE
022000081219     C     cn02sml       IFEQ      'S'
022100081219     C                   SETON                                        19
022200121010     c                   if        dutlpo <> 'S'
022300121010     c                   open      fifst02l
022400121010     c                   end
022500081219     C                   ENDIF
022600081219     C*
022700081229     c     cn02mod       comp      '5'                                    10
022800090423     c   10              eval      vidtes = ' Interrogazione tariffe Aut Città '
022900090423     c  n10              eval      vidtes = '    Gestione tariffe Aut Città    '
023000090112     c* inserimento
023100090112     c     cn02mod       comp      '1'                                    43
023200020125     C*
023300081219     c                   movel     cn02div       vi3div
023400081219     c                   movel     cn02pdr       vidaut
023500081222     c                   movel     cn02prg       comprg
023600081219     C     kapdc         CHAIN     fiapd000
023700081219     c                   if        %found(fiapd01l)
023800090706     C                   MOVEL     APDRSf        DESPDR
023900090402     C                   MOVEL     APDflr        Dapdflr
024000090402     c                   z-add     §APDMCP       v5pkl
024100081219     c                   else
024200081219     C                   MOVEL     *all'?'       DESPDR
024300081219     c                   end
024400090402     c* reperisco la p.iva  e la ragione sociale
024500090324     c                   movel     *all'0'       frnksc
024600090324     c                   move      apdksc        frnksc
024700091130     c                   movel     apdcsf        frnsocieta
024800090402     c                   exsr      srcdf
024900090402     c                   movel     sogpartiva    trsiva
025000081219     c*
025100081229     c                   if        cn02mod = '1'
025200090108     C     Ktgt          setgt     fitgt000                           32
025300081219     c                   else
025400090108     C     Ktgt1         setgt     fitgt000                           32
025500081219     c                   end
025600081219     C*
025700090108     c                   clear                   savnrr
025800081219     c*
025900081229     c                   if        cn02mod = '1'
026000081219     c     ktgt          readpe    fitgt000
026100081219     c                   else
026200081229     c                   if        cn02mod = '5'
026300090108     c     ktgt1         readpe    fitgt000
026400081222     c                   else
026500081219     c     ktgt1         readpe    fitgt000
026600090108     c     tgtnrr        chain(e)  fitgt00f
026700090108     C* RECORD ALLOCATO
026800090108     C                   IF        %error
026900090427     c                   eval      CN02ERR = '1'
027000090427     c                   eval      CN02MSG = 'Tariffa non manutenzionabile-
027100090427     c                              perchè già in uso'
027200090427     c                   seton                                        lr
027300090427     c                   movel(p)  ficn02ds      kpjbu
027400090427     c                   return
027500090108     c                   else
027600090108     c                   eval      savnrr = tgtnrr
027700090108     C                   ENDIF
027800081219     c                   end
027900081222     c                   end
028000081219     c*
028100090423     c                   if        %eof(fitgt01l)
028200090423     c                   clear                   fitgtds
028300090423     c                   end
028400090427     c* controlli per annullamento
028500090427     c                   if        cn02mod = '4' and tgtdcn <> 0
028600090427     c                   eval      CN02ERR = '1'
028700090427     c                   eval      CN02MSG = 'Non si può annulare una -
028800090427     c                             tariffa già stampata'
028900090427     c                   seton                                        lr
029000090427     c                   movel(p)  ficn02ds      kpjbu
029100090427     c                   return
029200090427     c                   end
029300081219     C* CARICO DATI A VIDEO
029400081219     C* inserimento
029500081222     c                   if        cn02mod = '1'
029600081219     c                   if        not %eof(fitgt01l)
029700090423     c* controllo se si può aprire un nuovo progressivo solo se non
029800090423     c* annullato. se lo fosse vuol dire che i controlli li aveva già
029900090423     c* passati prima
030000090424     c                   if        tgtatb = ' ' and cn02sml = ' '
030100090423     c                   if        tgtdts = 0
030200090423     c                   eval      CN02ERR = '1'
030300090423     c                   eval      CN02MSG = 'Errore, si dev-
030400090423     c                             e prima confermare il precedente'
030500090423     c                   seton                                        lr
030600090423     c                   movel(p)  ficn02ds      kpjbu
030700090423     c                   return
030800090423     c                   else
030900090508     c* controllo esistenza del contratto solo se non cambiato fornitore
031000090508     c                   movel     *all'0'       com08             8
031100090508     c                   move      apdksc        com08
031200090508     c                   if        apdcsf = tgtsoc
031300090508     c                             and com08 = tgtcdf
031400090423     c     ktrs1         chain     aitrs02l
031500090423     c                   if        not %found(aitrs02l)
031600090423     c                   eval      CN02ERR = '1'
031700090423     c                   eval      CN02MSG = 'Errore, -
031800090423     c                             occorre avere prima un contratto'
031900090423     c                   seton                                        lr
032000090423     c                   movel(p)  ficn02ds      kpjbu
032100090423     c                   return
032200090423     c                   end
032300090508     c                   end
032400090423     c                   end
032500090423     c                   end
032600090423     c*
032700081222     c     tgtprg        add       1             vi3prg
032800081222     c                   movel     tgtdst        dataiso
032900081222     c                   adddur    1:*d          dataiso
033000081222     c                   movel     dataiso       dataeur
033100081222     c                   movel     dataeur       v5ddt
033200081219     c                   else
033300090114     c                   eval      vi3prg = 200
033400081219     c                   end
033500081222     c                   clear                   fitgtds
033600081219     c                   end
033700081219     C* modifica
033800131024     c                   setoff                                       20  15
033900090519     c                   movel     tgtflr        dtgtflr
034000081229     c                   if        cn02mod <>'1'
034100081219     c                   move      cn02prg       vi3prg
034200090521     c     §tgtris       comp      'R'                                    20
034201131024     c                   seton                                        15
034300081219     c                   end
034400081222     c* Controllo se abilitato alla manutenzione delle tariffe
034500081219     c* solo se è impostata la data di stampa
034600131024     c                   setoff                                       13
034700090521     c                   if        tgtsml = *blank
034900131008     c                   clear                   tibs42ds
035000131008     c                   movel     vidaut        i42pge
035200131008     c                   call      'TIBS42R'
035300131008     c                   parm                    tibs42ds
035400131008     c                   movel     o42uni        uteautds
035500081219      * se abilitato con *in13 abilito la manutenzione della data scadenza
035600081219      * con *in15 proteggo gli altri campi
035800081219     c                   if        §autmtc <>'S'
035900081219     c                   seton                                        13
036000081219     c                   end
036200090521     C                   else
036300090521     c* in simulazione e manutenzione proteggo comunque perchè non verrà
036400090521     c* stampato
036500090521     C                   seton                                        15
036600081219     c                   end
036700081219      *record esistente
036800081219     c                   if        Tgtddt > 0
036900081219     c                   move      tgtddt        dataiso
037000081219     c                   move      dataiso       dataeur
037100081219     c                   move      dataeur       v5ddt
037200081219     c                   else
037300081219     c                   z-add     0             v5ddt
037400081219     c                   end
037500081219     c                   if        Tgtdst > 0
037600081219     c                   move      Tgtdst        dataiso
037700081219     c                   move      dataiso       dataeur
037800081219     c                   move      dataeur       v5dst
037900081219     c                   else
038000081219     c                   z-add     31122039      v5dst
038100081219     c                   end
038200081219      *
038300081219     c                   if        Tgtduv > 0
038400081219     c                   move      Tgtduv        dataiso
038500081219     c                   move      dataiso       dataeur
038600081219     c                   move      dataeur       v5duv
038700081219     c                   else
038800081219     c                   z-add     0             v5duv
038900081219     c                   end
039000090320     c                   setoff                                       17
039100090320      * se Data Stampa <> 0 non è + possibile fare modifiche
039200090320      * alla tariffa.
039300081219     c                   if        Tgtdts > 0
039400090320     c                   seton                                        17
039500081219     c                   move      tgtdts        dataiso
039600081219     c                   move      dataiso       dataeur
039700081219     c                   move      dataeur       v5dts
039800081219     c                   else
039900081219     c                   z-add     0             v5dts
040000081219     c                   end
040100081219      *
040200090320     c                   if        Tgtdcn > 0
040300081219     c                   move      Tgtdcn        dataiso
040400081219     c                   move      dataiso       dataeur
040500081219     c                   move      dataeur       v5dcn
040600081219     c                   else
040700081219     c                   z-add     0             v5dcn
040800081219     c                   end
040900120104     c* ristampa
041000120104     c* se è una ristampa si deve riempire la "data di stampa" con
041100120104     c* la 1° data certa, e mettere nella "data di ristampa"
041200120104     c* l'ultima data certa ...
041300120104     c                   if        §tgtris = 'R'
041400120104     c                   if        §Tgtunodc <> ' '
041500120104     c                   move      §Tgtunodc     v5unodc
041600120104     c                   move      v5unodc       dataiso
041700120104     c                   move      dataiso       dataeur
041800120104     c                   move      dataeur       v5unodc
041900120104     c                   else
042000120104     c                   z-add     0             v5unodc
042100120104     c                   end
042200120104     c                   eval      v5unodc = v5dcn
042300120104     c                   if        §Tgtunodc <> ' '
042400120104     c                   move      dataeur       v5dcn
042500120104     c                   else
042600120104     c* tariffe prima di dicembre 2011 quando non esisteva ancora
042700120104     c* il campo della prima data certa
042800120104     c                   clear                   v5dcn
042900120104     c                   end
043000120104     c                   end
043100090320      *
043200090320     c                   if        Tgtdrc > 0
043300090320     c                   move      Tgtdrc        dataiso
043400090320     c                   move      dataiso       dataeur
043500090320     c                   move      dataeur       v5drc
043600090320     c                   else
043700090324     c                   z-add     0             v5drc
043800090320     c                   end
043900081219
044000081219     c                   movel     tgtdcv        v5dcv
044100090402     c                   move      tgtsoc        v5soc
044200090402     c                   move      tgtcdf        v5cdf
044300090402     c                   move      tgtcdf        frnksc
044400091130     c                   movel     tgtsoc        frnsocieta
044500090402     c                   exsr      srcdf
044600090402     c                   movel     sogdes        desfor
044700090402     c                   z-add     tgtfil        v5fil
044800090402     c                   z-add     tgtnrc        v5nrc
044900081222     c                   z-add     tgtkm         v5km
045000090109     c                   z-add     tgtpcar       v5pcar
045100111215     c*    v5pcar        comp      0                                      88
045200111215     c                   seton                                        88
045300090320     c                   z-add     tgttmpc       v5tmpc
045400090320     c                   z-add     tgttmps       v5tmps
045500090401     c                   if        tgtpkl <> 0
045600090401     c                   z-add     tgtpkl        v5pkl
045700090401     c                   end
045800081219
045900090113     c                   if        not *in10
046000081229     C                   exsr      contrtgt
046100081229     c                   end
046200081219
046300081219     C                   endsr
046400081219     C*---------------------------------------------------------------*
046500081219     C     aggio         begsr
046600081219     C*---------------------------------------------------------------*
046700081222     c                   clear                   fitgtds
046800081222     c                   eval      comprg = vi3prg
046900090108     c                   if        savnrr <> 0
047000090108     c     savnrr        chain     fitgt00f
047100090108     c                   end
047200081222     c                   move      vidaut        tgtpdr
047300081222     c                   movel     cn02sml       tgtsml
047400081222     c                   movel     vi3div        tgtdiv
047500081222     c                   move      vi3prg        tgtprg
047600081222     c                   if        v5duv = 0
047700081222     c                   movel     oggi          tgtduv
047800081222     c                   else
047900081222     c                   movel     v5duv         dataeur
048000081222     c                   movel     dataeur       dataiso
048100081222     c                   movel     dataiso       tgtduv
048200081222     c                   end
048300081222     c                   movel     v5ddt         dataeur
048400081222     c                   movel     dataeur       dataiso
048500081222     c                   movel     dataiso       tgtddt
048600081222     c                   movel     v5dst         dataeur
048700081222     c                   movel     dataeur       dataiso
048800081222     c                   movel     dataiso       tgtdst
048900081222     c                   if        v5dts <> 0
049000081222     c                   movel     v5dts         dataeur
049100081222     c                   movel     dataeur       dataiso
049200081222     c                   movel     dataiso       tgtdts
049300081222     c                   end
049400081222     c                   movel     v5km          tgtkm
049500081222     c                   movel     v5dcv         tgtdcv
049600081222     c                   movel     dutute        tgtpru
049700090401     c                   z-add     v5pkl         tgtpkl
049800090108     c                   if        savnrr = 0
049900111222     c                   clear                   dtgtflr
050000111222     c                   eval      tgtflr = dtgtflr
050100090324     c* gli altri dati vengono memorizzati nel momento della stampa
050200090108     c                   write     fitgtf
050300090112     c* se richiesta duplica
050400090112     c                   if        *inkg
050500090112     c                   clear                   ficn06ds
050600090112     C                   MOVEL     VIdaut        filnew
050700090112     C                   MOVE      VIdaut        pdrnew
050800090112     c                   z-add     vi3prg        prgnew
050900090112     c                   movel     cn02sml       smlnew
051000090424     c                   movel(p)  ficn06ds      kpjbu
051100090112     c                   call      'FICN85R'
051200090112     c                   parm                    kpjba
051300090112     c                   end
051400081222     c                   else
051500090112     c                   if        cn02mod ='4' or *inkq
051600100526     c                   delete    fitgtf
051700100526     c                   else
051800090108     c                   update    fitgtf
051900081229     C* AGGIORNO I RECORD DEL FIFGT SE SONO IN MODIFICA
052000090331     c* commit
052100090331     c                   eval      comcom = *on
052200081229     C                   call      'FICN03R'
052300081229     C                   parm                    vidaut
052400081229     C                   parm                    cn02sml
052500081229     C                   parm                    comPRG
052600090331     C                   parm                    comcom            1
052700090108     c                   end
052800100526     c                   end
052900090112     c                   commit
053000081219     C                   endsr
053100081219     C*---------------------------------------------------------------*
053200081219     C     contrtgt      begsr
053300081219     C*---------------------------------------------------------------*
053400081219     C                   SETOFF                                       90
053500081219     C****  DATA DECORRENZA TARIFFA  ****
053600081219     C                   Z-ADD     V5ddt         G02DAT
053700081219     C                   MOVEL     *BLANKS       G02ERR
053800081219     C                   CALL      'XSRDA8'
053900081219     C                   PARM                    WLBDAT
054000081219     C     G02ERR        IFEQ      '1'
054100081219     C                   SETON                                        4590
054200081219     C                   leavesr
054300081219     C                   ELSE
054400081219     C                   MOVE      G02INV        COMDDT
054500081219     C                   ENDIF
054600090324     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
054700090324     C                   Z-ADD     G02DAT        V5ddt
054800090423     c* controllo data decorrenza solo se manutenzionabile
054900090504     c                   if        not *in15 and
055000090504     c                             cn02mod <> '4'
055100090324     c* la decorrenza non può essere maggiore del 2039
055200090108     c                   if        comddt > 20391231
055300090108     C                   SETON                                        4590
055400090108     C                   leavesr
055500090108     c                   end
055600090324     c* le supertestate devono essere inserite solo con decorrenze
055700090421     c* successive alla trascodifica
055800090421     c                   if        comddt < 20090501
055900090113     C                   SETON                                        4590
056000090113     C                   leavesr
056100090113     c                   end
056200111124     c* le supertestate devono essere inserite solo se non presente
056300111124     c* la data di blocco impostata nella tabella VPO record CONT
056400111124     c                   if        comddt >= §vpodpa
056500111124     C                   SETON                                        4590
056600111124     C                   leavesr
056700111124     c                   end
056800090323     c* ???? obbligatorio inizio mese eccetto le prime supertestate
056900090323     c* create dalla trascodifica
057000090331     c*                  move      comddt        isoddt
057100090331     c*                  extrct    isoddt:*d     inizio            2 0
057200090331     c*                  if        comddt > 20090201 and inizio <> 01
057300090331     C*                  SETON                                        9490
057400090331     C*                  leavesr
057500090331     c*                  end
057600090512     c* verifico che non sia presente un contratto attivo con decorrenza
057700090512     c* successiva. Nel caso non esita si bloccherranno le valorizzaz.
057800090423     c     ktrs1         setll     aitrs02l
057900090423     c                   do        *hival
058000090323     c     ktrs          reade     aitrs02l
058100090423     c                   if        %eof(aitrs02l)
058200090423     c                   leave
058300090423     c                   end
058400090423     c                   if        trsann <> ' '
058500090423     c                   iter
058600090423     c                   end
058700090423     c                   if        trsdfc <> 0
058800090423     c                   leave
058900090423     c                   end
059000090423     c                   if        trsdec > comddt
059100090324     C                   SETON                                        9590
059200090323     C                   leavesr
059300090323     c                   end
059400090512     c* verifico che l'autista sia accreditato nel contratto corretto
059500090423     c     ktra          setll     aitra03l
059600090423     c                   do        *hival
059700090423     c     vidaut        reade     aitra03l
059800090423     c                   if        %eof(aitra03l)
059900090423     c                   leave
060000090423     c                   end
060100090423     c                   if        traann <> ' '
060200090423     c                   iter
060300090423     c                   end
060400090609     c                   if        tranrc <>trsnrc
060500090512     c                   iter
060600090512     c                   end
060700090423     c                   if        tradfi = 0
060800090423     c                   if        tradin > comddt
060900090423     C                   SETON                                        9590
061000090423     C                   leavesr
061100090423     c                   else
061200090423     c                   leave
061300090512     c                   end
061400090423     c                   else
061500090423     C                   SETON                                        9890
061600090423     C                   leavesr
061700090423     c                   end
061800090423     c                   enddo
061900090423     c                   enddo
062000090423     c                   end
062100081219     C*
062200081219     C****  DATA SCADENZA TARIFFA  ****
062300081219     C                   Z-ADD     V5dst         G02DAT
062400081219     C                   MOVEL     *BLANKS       G02ERR
062500081219     C                   CALL      'XSRDA8'
062600081219     C                   PARM                    WLBDAT
062700081219     C     G02ERR        IFEQ      '1'
062800081219     C                   SETON                                        4690
062900081219     C                   leavesr
063000081219     C                   ELSE
063100081219     C                   MOVE      G02INV        COMDST
063200081219     C                   ENDIF
063300081222     c                   if        comdst > 20391231
063400081222     C                   SETON                                        4690
063500081222     C                   leavesr
063600081222     c                   end
063700090108     c                   if        comdst<> 20391231 and comdst > unanno
063800090324     C                   SETON                                        9390
063900090108     C                   leavesr
064000090108     c                   end
064100120724      * controllo congruenza scadenza con validità società
064200130110     c***************    if        comdst <> 20391231 and
064300130110      * avendo spostato il controllo della data validità con la data scadenza anzichè decorrenza
064400130110      * occorre eliminare il test(x solo <> da 20391231)altrimenti qualcuno potrebbe riaprire
064500130110      * una tariffa sulla società precedente riabilitandola con scadenza infinito e passerebbe
064600130110      * oltre questo controllo.
064700130110     c                   if        comdst <> 0 and v5soc <> ' '
064800120724     c                   movel     vidaut        adtpdrf           3 0
064900121218     c                   move      comdst        dtvalidita
065000120724ab   C                   exsr      TEST_societa
065100120724     c                   move      valdatfine    valdatfineW       8 0
065200121219     c*
065300121219     c                   if          idsocieta <> v5soc
065400120724ab   c                   seton                                        4290
065500121219ab   C                   end
065600121219     c*
065700121219     c                   if           *in42
065800121219     c                   leavesr
065900120724ab   C                   end
066000121219ab   C                   end
066100120724     ***
066200090331     c* la data di scadenza deve essere fine mese ????
066300090331     c*                  move      comdst        isodst
066400090331     c*    isodst        adddur    1:*d          comodo
066500090331     c*                  extrct    comodo:*d     inizio            2 0
066600090331     c*                  if        inizio <> 01
066700090331     C*                  SETON                                        9690
066800090331     C*                  leavesr
066900090331     c*                  end
067000081219     C*
067100081219     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
067200081219     C                   Z-ADD     G02DAT        V5DST
067300081219     C*
067400081219     C* LA DATA SCADENZA DEVE ESSERE MAGGIORE DELLA DATA DECORRENZA
067500081219     C     COMDST        IFLT      COMDDT
067600081219     C                   SETON                                        4790
067700081219     C                   leavesr
067800081219     C                   ENDIF
067900090323     c* la tariffa deve aver validità per almeno xx giorni
068000090323     c                   move      comddt        isoddt
068100090323     c                   move      comdst        isodst
068200090324     c     isodst        subdur    isoddt        ggvalidi:*D       4 0
068300090331     c                   if        ggvalidi < giomin
068400090324     C                   SETON                                        9790
068500090323     C                   leavesr
068600090323     c                   end
068700121010      **
068800121010      **  se SONO in SEDE e ho chiesto la simulazione
068900121010      **  deve saltare questo controllo incrociato con le VAlorizzazioni simulate
069000121010      **  poichè le valorizzazioni simulate al momento sono solo in ambiente di Filiale
069100121010     c                   IF        dutlpo <> 'S'
069200121010     c                             or cn02sml = ' '
069300121010      **
069400081219      *verifica se esistono conteggi valorizzati o confermati x la data scadenza
069500081219     c                   move      'C'           klvl
069600121010     c                   if          cn02sml = ' '
069700081219     c     kftt          setgt     fiftt02l
069800081219     c     kftt          readpe    fiftt02l                               59
069900121010     c                   else
070000121010     c     kftt          setgt     fifst02l
070100121010     c     kftt          readpe    fifst02l                               59
070200121010     c                   end
070300081219     c                   if        not *in59
070400081219     c                   if        fttddc >  comdst
070500090710     c                   seton                                        6090
070600081219     C                   leavesr
070700090710     c                   endif
070800090710     c* se esitono valorizzazioni avviso prima di annullare (solo x edp)
070900101108     c                   if        fttddc >= comddt and
071000101108     c                             fttddc <= comdst and
071100090427     c                             cn02mod = '4'
071200090710     c                   seton                                        61
071300090710     c  n91              seton                                        9091
071400090710     c  n91              leavesr
071500090427     c                   endif
071600081219     c                   endif
071700090710     c*
071800081219     c                   move      'V'           klvl
071900121010     c                   if          cn02sml = ' '
072000081219     c     kftt          setgt     fiftt02l
072100081219     c     kftt          readpe    fiftt02l                               59
072200121010     c                   else
072300121010     c     kftt          setgt     fifst02l
072400121010     c     kftt          readpe    fifst02l                               59
072500121010     c                   end
072600091112     c                   if        not *in59 and fttfla = ' '
072700081219     c                   if        fttddc >  comdst
072800090710     c                   seton                                        6090
072900081219     C                   leavesr
073000090710     c                   endif
073100090710     c* se esitono valorizzazioni avviso prima di annullare (solo x edp)
073200101108     c                   if        fttddc >= comddt and
073300101108     c                             fttddc <= comdst and
073400090427     c                             cn02mod = '4'
073500090710     c                   seton                                        61
073600090710     c  n91              seton                                        9091
073700090710     c  n91              leavesr
073800090427     c                   endif
073900081219     c                   endif
074000081219      *
074100121010     c                   END
074200121010      *
074300090114     C* LA SCADENZA DEL PROGRESSIVO che sto controllando DEVE ESSERE MINORE
074400081219     C*   DELLA DATA DECORRENZA DEL PROGRESSIVO SEGUENTE
074500081219     C     vi3prg        ADD       1             COMPRG
074600081219     C     KtGT1         SETLL     fitgt000
074700090112     C     KtGT          READE     fitgt000                               32
074800081219     C*
074900081219     C     *IN32         DOWEQ     *OFF
075000081219     C     tGTATB        IFEQ      ' '
075100090114     C                   move      comdst        dataiso
075200090114     c                   adddur    1:*d          dataiso
075300090114     C                   move      dataiso       DATA
075400090114     C     data          IFne      tGTDDT
075500081219     C                   SETON                                        4890
075600081219     C                   leavesr
075700081219     C                   ELSE
075800081219     C                   SETON                                        32
075900081219     C                   ENDIF
076000081219     C                   ENDIF
076100081219     C*
076200090112     C  N32KtGT          READE     fitgt000                               32
076300081219     C                   ENDDO
076400081219     C*
076500090114     C* LA DECORRENZA DEL PROGRESSIVO che sto controllando DEVE ESSERE
076600081219     C*   MAGGIORE DELLA DATA SCADENZA DEL PROGRESSIVO PRECEDENTE
076700090113     C                   z-add     vi3prg        COMPRG
076800081219     C     KtGT1         SETLL     fitgt000
076900090113     C     KtGT          READpe    fitgt000                               32
077000081219     C*
077100081219     C     *IN32         DOWEQ     *OFF
077200081219     C     tGTATB        IFEQ      ' '
077300081219     C     COMDDT        IFLE      tGTDST
077400081219     C                   SETON                                        4990
077500081219     C                   leavesr
077600081219     C                   ELSE
077700090114     C* LA DECORRENZA PROGRESSIVO che sto controllando DEVE ESSERE MAGGIORE
077800081219     C*   DI 1 GIORNO DELLA DATA SCADENZA PROGRESSIVO PRECEDENTE
077900090113     C                   move      tGTDST        dataiso
078000090113     c                   adddur    1:*d          dataiso
078100090113     C                   move      dataiso       DATA
078200081219     C*
078300081219     C     COMDDT        IFNE      DATA
078400081219     C                   SETON                                        4990
078500081219     C                   leavesr
078600081219     C                   ENDIF
078700081219     C*
078800081219     C                   SETON                                        32
078900081219     C                   ENDIF
079000081219     C                   ENDIF
079100081219     C*
079200090113     C  N32KtGT          READpe    fitgt000                               32
079300081219     C                   ENDDO
079400090108     C* km obbligatori
079500090417     C*    v5km          IFeq      0
079600090417     C*                  SETON                                        5090
079700090417     C*                  leavesr
079800090417     C*                  ENDIF
079900090109     C* se variato prezzo ingresso obbligo a far scadere la tariffa
080000090109     c* solo se non sono in inserimento
080100090109     c                   if        cn02mod <> '1' and comdst > oggi
080200090109     c                   clear                   ficn05ds
080300090109     c                   call      'FICN05R'
080400090109     c                   parm                    ficn05ds
080500090109     c                   eval      dgpi = o05gpi
080600090112     c                   z-add     D§GPIGPI      tgtpcarc
080700090112     C     tgtpcarc      IFne      v5pcar
080800090324     c     v5pcar        andne     0
080900111213     C*                  SETON                                        5290
081000111213     C*                  leavesr
081100090109     C                   ENDIF
081200090109     C                   ENDIF
081300111214     c*    v5pcar        comp      0                                      88
081400111214     c                   seton                                        88
081500081219     c                   endsr
081600120724ab   C**************************************************************************
081700120724ab    * Controlla se nel periodo successivo è prevista un'altra società
081800120724ab   C**************************************************************************
081900120724ab   C     TEST_SOCieta  begSR
082000120724ab    *
082100120724ab    /FREE
082200120724ab
082300120724EDPDC  idSocieta = TibsSof_GetIdSocietaByIdFiliale( adtpdrf
082400120724EDPDC                                             : dtVALIDITA
082500120724EDPDC                                             : 'O'
082600120724EDPDC                                             : valDatIniz
082700120724EDPDC                                             : valDatFine
082800120724ab                                                );
082900120724ab
083000120724EDPDC  IF idSocieta = *BLANK OR TibsSof_esito < TIBSSOF_ESITO_OK;
083100120724EDPDC    // Gestire l'errore.
083200121219           *in42 = '1';
083300120724ab     ENDIF;
083400120724ab
083500120724ab    /END-FREE
083600120724ab   C                   ENDSR
083700081219**   XDTF
083800081219312831303130313130313031

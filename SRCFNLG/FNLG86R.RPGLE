000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300050418     ftigcp26l  uf   e           k disk    commit
000400050221     FFNARB01L  IF   E           K DISK
000500071026     FFnlbl02l  IF   E           K DISK
000600071026     FFnblp01l  IF   E           K DISK
000700050221     FFiAR501L  IF   E           K DISK
000800050221     FTABEL00F  IF   E           K DISK
000900170308     FWFGCS80F  O    E             DISK    USROPN
001000170308     FWFGCS81L  IF   E           K DISK    RENAME(WFGCS080:WFGCS1) USROPN
001100170308     FWFGCS82L  IF   E           K DISK    RENAME(WFGCS080:WFGCS2) USROPN
001200170308     FWFGCS85L  IF   E           K DISK    RENAME(WFGCS080:WFGCS5) USROPN
001300180119     FWFGCS86L  IF   E           K DISK    RENAME(WFGCS080:WFGCS8) USROPN
001400070629     Ffnart01L  IF   E           K DISK
001500071026     Ffnblt01L  IF   E           K DISK
001600141215     Ffnbrv07L  IF   E           K DISK
001700161111     Ffnfvv01l  IF   E           K DISK
001800050221     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
001900050221     D FIL£6           S              3  0 DIM(30)
002000170331     D zone            S              5    DIM(200)
002100170331     d
002200050221     D §FLE            S              3  0
002300050221     D WLBDA8          DS
002400050221     D  G08DAT                 1      8  0
002500050221     D  G08INV                 9     16  0
002600050221     D  G08ERR                17     17
002700050221     D  G08TGI                18     22  0
002800050221      *
002900161111     D RECBRV        E DS                  EXTNAME(FNBRV00F)
003000161111     D WSAVB         E DS                  EXTNAME(FNBRV00F)
003100161111     d                                     prefix(S_)
003200050221     d dar5gen       e ds
003300050221     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
003400070629     d dskiere         ds
003500070629     d  sk1                    1    140  0 dim(20)
003600070629     d  sk2                  141    280  0 dim(20)
003700070629     d  sk3                  281    420  0 dim(20)
003800070629     d  sk4                  421    560  0 dim(20)
003900070629     d  sk5                  561    700  0 dim(20)
004000070629     d  skt                    1    700  0 dim(100)
004100071026     d dskieref        ds
004200071026     d  sk1f                   1    140  0 dim(20)
004300071026     d  sk2f                 141    280  0 dim(20)
004400071026     d  sk3f                 281    420  0 dim(20)
004500071026     d  sk4f                 421    560  0 dim(20)
004600071026     d  sk5f                 561    700  0 dim(20)
004700071026     d  sktf                   1    700  0 dim(100)
004800071026
004900070629
005000161007     D tigcp00ds     E DS                  extname(tigcp00f)
005100161007     D fnlgi1ds      E DS
005200170512     D TRULTASDS     E DS
005300050221     D CNCR80        E DS
005400050221     D KPJBA         E DS
005500161111     D DS7B          E DS
005600170331     D DS03          E DS
005700050221     d trul06ds      e ds
005800050221     d  lin                    1     90  0 dim(30)
005900050221     D FNLG87        E DS                  EXTNAME(FNLG87DS)
006000161111     D* REPERISCE DATI FOGLI
006100161111     D DSLV53        E DS                  EXTNAME(FNLV53DS)
006200050221      *
006300050221     D                SDS
006400050221     D  §PGM                   1     10
006500071011     D cmd1            S             66    DIM(01) CTDATA PERRCD(1)
006600071011     D cmd2            S             66    DIM(01) CTDATA PERRCD(1)
006700071011     D cmd3            S             66    DIM(01) CTDATA PERRCD(1)
006800071011     D cmd4            S             66    DIM(01) CTDATA PERRCD(1)
006900071011     D cmd5            S             66    DIM(01) CTDATA PERRCD(1)
007000071011     D cmd6            S             66    DIM(01) CTDATA PERRCD(1)
007100170222     D cmd7            S             66    DIM(01) CTDATA PERRCD(1)
007200170222     D cmd8            S             66    DIM(01) CTDATA PERRCD(1)
007300180119     D cmd9            S             66    DIM(01) CTDATA PERRCD(1)
007400180119     D cmd10           S             66    DIM(01) CTDATA PERRCD(1)
007500161111     D WLBDAT          DS
007600161111     D  G02DAT                 1      8  0
007700161111     D  G02INV                 9     16  0
007800161111     D  G02ERR                17     17
007900161111     D  G02TGI                18     22  0
008000071011     d comman          s             80
008100071011     d lenght          s             15  5 inz(80)
008200161111     d wdfv            S                   LIKE(d53dfv)
008300161111     d WSPG            S                   LIKE(FVVSPG)
008400161111     d Wnfv5           S                   LIKE(FVVnfv)
008500170331     d wok             s              1
008600070629      *----------------------------------------------------------------
008700050221     C                   DO        30            I                 2 0
008800050221     c                   if        fil£6(i) = 0
008900050221     c                   leave
009000050221     c                   endif
009100050221     C                   Z-ADD     fil£6(i)      FLE               3 0
009200050221     C*
009300050221     c   01              z-add     10            fas
009400050221     c   03              z-add     35            fas
009500050221      * lettura giacenze
009600050221      *
009700050418     c     kgcp          setll     tigcp26l
009800000607     c                   do        *hival
009900050418     c     fle           reade     tigcp26l                             90
010000050413      *allocazione
010100050413     c                   if        *in90
010200050418     c     fle           reade(n)  tigcp26l
010300050413     c                   if        %eof
010400050413     c                   leave
010500050413     c                   end
010600050413     c                   iter
010700050413     c                   endif
010800050413      *
010900000607     c                   if        %eof
011000000607     c                   leave
011100000607     c                   end
011200050221      * se richiesto materiale da portare a magazzino la fase non può essere
011300050221      * maggiore di 26
011400050418     c                   if        gcpfas > 30 and *in01
011500050221     c                   leave
011600040507     c                   end
011700050221
011800050408      *solo progressivo 0
011900050408     c                   if        gcpfrg <> 0
012000050408     c                   iter
012100050408     c                   end
012200161007     c*
012300161007     c                   clear                   fnlgi1ds
012400161007     c                   eval      ilgi1agc=gcpagc
012500161007     c                   eval      ilgi1fgc=gcpfgc
012600161007     c                   eval      ilgi1ngc=gcpngc
012700161007     c                   movel     fnlgi1ds      kpjbu
012800161007     c                   call      'FNLGI1R'
012900161007     c                   parm                    kpjba
013000161007     c                   parm                    tigcp00ds
013100161007     c                   movel     kpjbu         fnlgi1ds
013200161007
013300050411      * in uscita dal magazzino
013400050221--2)>C     GCPFAS        IFEQ      35
013500050221     C     GCPDuma       ANDEQ     0
013600050221     C     D87RIS        ANDEQ     *BLANK
013700050221     C     *IN03         ANDEQ     '1'
013800161007     c     olgi1outmg    andne     ' '
013900050221      *
014000050221     C     GCPFAS        OREQ      35
014100050221     C     GCPDuma       ANDEQ     D87DAT
014200050221     C     D87RIS        ANDEQ     'R'
014300050221     C     *IN03         ANDEQ     '1'
014400161007     c     olgi1outmg    andne     ' '
014500050221      *
014600050411      * in uscita dal magazzino x resi
014700141110---->C*mm  GCPFAS        OReq      50
014800141107---->C     GCPFAS        ORge      50
014900050411---->C     GCPdis        andeq     'R'
015000050411     C     GCPDuma       ANDEQ     0
015100050411     C     D87RIS        ANDEQ     *BLANK
015200050411     C     *IN03         ANDEQ     '1'
015300161007     c     olgi1outmg    andne     ' '
015400050221      *
015500050411---->C     GCPdis        oreq      'R'
015600050411     C     GCPDuma       ANDEQ     D87DAT
015700050411     C     D87RIS        ANDEQ     'R'
015800050411     C     *IN03         ANDEQ     '1'
015900161007     c     olgi1outmg    andne     ' '
016000161007      *
016100050411      * in uscita dal magazzino x dirottamenti
016200141110---->C*mm  GCPFAS        OReq      40
016300141107---->C     GCPFAS        ORge      40
016400050411---->C     GCPdis        andeq     'D'
016500050411     C     GCPDuma       ANDEQ     0
016600050411     C     D87RIS        ANDEQ     *BLANK
016700050411     C     *IN03         ANDEQ     '1'
016800161007     c     olgi1outmg    andne     ' '
016900050411      *
017000141110---->C*mm  GCPFAS        OReq      50
017100141110---->C*mm  GCPdis        andeq     'D'
017200141110     C*mm  GCPDuma       ANDEQ     0
017300141110     C*mm  D87RIS        ANDEQ     *BLANK
017400141110     C*mm  *IN03         ANDEQ     '1'
017500050420      *
017600050411---->C     GCPdis        oreq      'D'
017700050411     C     GCPDuma       ANDEQ     D87DAT
017800050411     C     D87RIS        ANDEQ     'R'
017900050411     C     *IN03         ANDEQ     '1'
018000161007     c     olgi1outmg    andne     ' '
018100050411
018200050411      *merce da portare in magazzino
018300050413---->C     GCPFAS        ORle      30
018400050221     C     D87RIS        ANDEQ     *BLANK
018500050221     C     *IN01         ANDEQ     '1'
018600161007     c     olgi1amg      andeq     'S'
018700161007     C**   GCPDLMa       ANDEQ     0
018800050221      *
018900050221     C     GCPFAS        ORle      70
019000050221     C     GCPDLMa       ANDEQ     D87DAT
019100050221     C     D87RIS        ANDEQ     'R'
019200050221     C     *IN01         ANDEQ     '1'
019300050221      *
019400050221     C* PRELEVO DATI BOLLA
019500141119     C     KARB          CHAIN     FNARB01L
019600170307    1c                   if        %found(fnarb01l)
019700050221     c                   clear                   dar5gen
019800050221     C     KAR5          CHAIN     FiAR501L
019900170307    2c                   if        %found(fiar501l)
020000050221     c                   movel     ar5uni        dar5gen
020100170307    2c                   end
020200141215     c                   clear                   znc               2 0
020300141216     C     Kart1         chain     Fnart01L
020400170307    2c                   if        %found(fnart01l)
020500141216     C     Kbrv          setll     Fnbrv07L
020600170307    3c                   do        *hival
020700141216     C     Kbrv          reade     Fnbrv07L
020800170307    4c                   if        %eof(fnbrv07l)
020900141215     c                   leave
021000170307    4c                   endif
021100170307    4c                   if        brvdcs = 0 or brvnpg = 8
021200141215     c                   iter
021300170307   X4c                   else
021400141215     c                   move      brvznc        znc
021500141215     c                   leave
021600170307    4c                   endif
021700170307    3c                   enddo
021800170307    2c                   endif
021900071026      * solo per merce da prelevare dal magazzino *IN03=on
022000071026      * se dirottamento o reso reperisce riferimenti figlia da stampare
022100170307    2c                   if        *in03
022200080220     c                   seton                                        31
022300170307    3c                   if        gcpdis = 'D' or gcpdis = 'R'
022400071026     C     Karb          chain     fnlbl02l                           31
022500071026     C  n31Kblp          chain     fnblp01l                           31
022600170307    3c                   endif
022700170307    2c                   endif
022800170307     c
022900170307     c* solo per merce da portare a mag giacenze posso parzializzare
023000170307     c*  per fermo deposito
023100170331     c                   eval      wok='S'
023200170331     c                   if        *in01 and *in06 and arbffd<>d87fdep
023300170331     c                   eval      wok='N'
023400170331     c                   endif
023500170331
023600170331     c* OPPURE per zone messaggeria
023700170331     c                   if        *in01 and *in07
023800170331     C                   IF        NOT *IN06  OR  WOK='N'
023900170331     c                   movel     arblna        alfzona           5
024000170331     c                   move      arbznc        alfzona
024100170331     c                   if        %lookup(alfzona:zone)=0
024200170331     c                   eval      wok='N'
024300170331     C                   ELSE
024400170331     c                   eval      wok='S'
024500170331     c                   endif
024600170331     c                   endif
024700170331     c                   endif
024800170307
024900071011      *scrittura wf con bolle da stampare successivamente
025000170331    2c*****              if        *in03 or d87fdep=' ' or arbffd=d87fdep
025100170331
025200170331    2c                   if        *in03 or (*in01 and wok='S')
025300071011     c                   exsr      scriviwf
025400170331     c
025500050221     c
025600071026     C* AGGIORNO DATA DI STAMPA
025700170307    3C     D87RIS        IFEQ      *BLANK
025800170512     c                   if        *in01
025900170512     C                   MOVE      AMGUD8        GCPDLMa
026000170512     C                   EXCEPT    AGG5
026100170512
026200170512     c* richiamo pgm per attribuire competenze
026300170512     c                   clear                   trultasds
026400170512     c                   eval      TTASAAS=arbaas
026500170512     c                   eval      TTASlnp=arblnp
026600170512     c                   eval      TTASnrs=arbnrs
026700170512     c                   eval      TTASnsp=arbnsp
026800170512     c                   movel     trulTASds     KPJBU
026900170512     c                   call      'TRULTASR'
027000170512     c                   parm                    kpjba
027100170512
027200170512     c                   endif
027300170512     c
027400071011     C   03              MOVE      AMGUD8        GCPDuma
027500071011     C   03              EXCEPT    AGG0
027600170307    4c                   if        gescomm = 'S'
027700071011     c                   commit
027800170307    4c                   endif
027900170307    3c                   endif
028000170307    2c                   endif
028100071011
028200170307    1C                   END
028300071011     C*
028400170307    0C                   END
028500071011     C*
028600071011     c                   enddo
028700071011
028800071011     c                   enddo
028900071011      * eliminazione file di work
029000170308     c                   close     wfgcs80f
029100071011      * rilegge wf e produce il tabulato
029200071011     c                   exsr      stampa
029300071011      *
029400170302     c                   if        *in03  and d87ord='Z'
029500180119     c                   close     wfgcs81l
029600170302     c                   else
029700170308     c                   close     wfgcs85l
029800170302     c                   endif
029900180119     c** 01              close     wfgcs82l
030000180119
030100180119     c                   if        *in01  and d87ord='Z'
030200180119     c                   close     wfgcs82l
030300180119     c                   else
030400180119     c                   close     wfgcs86l
030500180119     c                   endif
030600180119
030700170224     c                   if        *in03  and d87ord='Z'
030800170222     c                   Movel(p)  cmd3(1)       comman
030900170222     c                   endif
031000170222     c                   if        *in03  and d87ord='L'
031100170222     c                   Movel(p)  cmd7(1)       comman
031200170222     c                   endif
031300180119
031400180119     c***01              Movel(p)  cmd5(1)       comman
031500180119     c
031600180119     c                   if        *in01  and d87ord='Z'
031700180119     c                   Movel(p)  cmd5(1)       comman
031800180119     c                   endif
031900180119     c                   if        *in01  and d87ord='L'
032000180119     c                   Movel(p)  cmd9(1)       comman
032100180119     c                   endif
032200180119
032300071011     c                   eval      lenght = %len(%trim(comman))
032400071011     c                   Call      'QCMDEXC'                            97
032500071011     c                   Parm                    comman
032600071011     c                   Parm                    lenght
032700170302     c
032800170302     c* Cancellazione file fisico da QTEMP
032900170302     c                   Movel(p)  cmd1(1)       comman
033000170302     c                   eval      lenght = %len(%trim(comman))
033100170302     c                   Call      'QCMDEXC'                            97
033200170302     c                   Parm                    comman
033300170302     c                   Parm                    lenght
033400161007
033500161007     c                   clear                   fnlgi1ds
033600161007     c                   eval      ILGI1TLA= 'C'
033700161007     c                   movel     fnlgi1ds      kpjbu
033800161007     c                   call      'FNLGI1R'
033900161007     c                   parm                    kpjba
034000161111     c
034100161111     C                   CLEAR                   DSLV53
034200161111     C                   MOVEL     'C'           D53TLA
034300161111     C                   CALL      'FNLV53R'
034400161111     C                   PARM                    DSLV53
034500161111     C*
034600071011     c                   seton                                        lr
034700071011      *_________________________________________________________
034800071011     c     stampa        begsr
034900071011      *_________________________________________________________
035000071011      * legge la v.l. opportuna x l'ordinamento a seconda che sia
035100071011      * merce da prelevare o da portare a magazzino
035200170224     c   03              if        d87ord='Z'
035300170308     c     *loval        setll     wfgcs81l
035400170222     c                   endif
035500170222     c   03              if        d87ord='L'
035600170308     c     *loval        setll     wfgcs85l
035700170222     c                   endif
035800180119     c   01              if        d87ord='Z'
035900180119     c     *loval        setll     wfgcs82l
036000180119     c                   endif
036100180119     c   01              if        d87ord='L'
036200180119     c     *loval        setll     wfgcs86l
036300180119     c                   endif
036400170222
036500071011     c                   do        *hival
036600170222
036700170224     c   03              if        d87ord='Z'
036800170308     c                   read      wfgcs81l                               98
036900170222     c                   endif
037000170222     c   03              if        d87ord='L'
037100170308     c                   read      wfgcs85l                               98
037200170222     c                   endif
037300180119     c   01              if        d87ord='Z'
037400180119     c                   read      wfgcs82l                               98
037500180119     c                   endif
037600180119     c   01              if        d87ord='L'
037700180119     c                   read      wfgcs86l                               98
037800180119     c                   endif
037900170222
038000071011     c   98              leave
038100071011      * segnacolli non sequenziali prelevo i numeri da dettaglio colli
038200070629     c                   clear                   skt
038300071011     c                   if        gcsfns = 'S'
038400071015     C     Kart          setll     Fnart01L
038500070629     c                   z-add     0             ncl               3 0
038600071011     c                   do        *hival
038700071015     C     Kart          reade     Fnart01L
038800070629     c                   if        %eof(fnart01l) or ncl = 100
038900070629     c                   leave
039000070629     c                   endif
039100070629     c                   add       1             ncl
039200070629     c                   z-add     artnsc        skt(ncl)
039300070629     c                   enddo
039400070629     c                   endif
039500070629     c
039600071026      * bolla figlia
039700071026     c                   clear                   sktf
039800140728     c                   clear                   spuntato          8
039900140728     c                   clear                   spu               3 0
040000140728     c                   clear                   nospu             3 0
040100170306     c                   clear                   stpsta            2
040200170306
040300140718     c                   if        *in03
040400071026     C     Kblt          setll     Fnblt01L
040500071026     c                   z-add     0             ncl               3 0
040600071026     c                   do        *hival
040700071026     C     Kblt          reade     Fnblt01L
040800071026     c                   if        %eof(fnblt01l) or ncl = 100
040900140728     c                   select
041000140728     c                   when      spu >= 1 and nospu = 0
041100140728     c                   eval      spuntato = 'Totale'
041200140728     c                   when      spu >= 1 and nospu >= 1
041300140728     c                   eval      spuntato = 'Parziale'
041400140728     c                   other
041500140728     c                   clear                   spuntato
041600140728     c                   endsl
041700071026     c                   leave
041800071026     c                   endif
041900140718     c* se presente la data entrata vuol dire
042000140718     c* che è già stato etichettato il pacco con la nuova
042100140718     c* etichetta della figlia
042200140728     c                   if        bltdse <> 0
042300140728     c                   add       1             spu
042400140728     c                   else
042500140728     c                   add       1             nospu
042600140728     c                   end
042700140718     c*
042800140718     c                   if        gcsfnsf = 'S'
042900071026     c                   add       1             ncl
043000071026     c                   z-add     bltnsc        sktf(ncl)
043100071026     c                   endif
043200140718     c                   enddo
043300140718     c                   endif
043400071026     c
043500050221      *
043600071011     C   03GCsDIS        IFNE      SAVDIS
043700050221     C* PRELEVO DESCRIZIONE TIPO DISPOSIZIONE
043800050221     C                   MOVEL     '2D'          COD
043900050221     C                   MOVEL     *BLANKS       KEY
044000071011     C                   MOVEL     GCsDIS        KEY
044100050221     C     KTAB          CHAIN     TABEL                              50
044200050221     C  N50              MOVEL     TBLUNI        DESCR            20
044300050221     C   50              MOVEL     *ALL'?'       DESCR
044400071026     c                   setoff                                       05
044500071026     c                   if        gcsdis  = 'D' or gcsdis = 'R'
044600071026     c                   seton                                        05
044700071026     c                   endif
044800050221     C                   EXCEPT    TESTA
044900050221     C     D87RIS        IFEQ      'R'
045000170308     c     *in06         oreq      *on
045100170331     c     *in07         oreq      *on
045200050221     C                   EXCEPT    TESTAR
045300050221     C                   END
045400050221     C                   EXCEPT    TESTAT
045500050221     C                   SETON                                        99
045600071011     C                   MOVE      GCsDIS        SAVDIS            1
045700071011     C                   END
045800050221      *
045900050221     C  N99
046000050221     COR OF              DO
046100050221     C                   EXCEPT    TESTA
046200050221     C     D87RIS        IFEQ      'R'
046300170308     c     *in06         oreq      *on
046400170331     c     *in07         oreq      *on
046500050221     C                   EXCEPT    TESTAR
046600050221     C                   END
046700050221     C                   EXCEPT    TESTAT
046800050221     C                   END
046900050221     C  N99              SETON                                        99
047000050221      *
047100140718     c                   movel     gcsrsm        arbrsms          23
047200140718     c                   movel     gcsrsd        arbrsds          23
047300170222     c***                movel     gcszng        s30_zng          30
047400170222
047500170222     c                   clear                   s30_zng          30
047600170222     c                   if        gcszng<>*blanks
047700170222     c                   eval      s30_zng=%trim(gcszng)+'-'+gcsultsp
047800170222     c                   else
047900170222     c                   eval      s30_zng=gcsultsp
048000170222     c                   endif
048100170222     c
048200050221     C     *IN03         IFEQ      '1'
048300071011     C                   Z-ADD     GCsdeda       G08INV
048400050221     C                   MOVE      *ZEROS        G08DAT
048500050221     C                   MOVEL     '3'           G08ERR
048600050221     C                   CALL      'XSRDA8'
048700050221     C                   PARM                    WLBDA8
048800170306     C                   move      G08DAT        w0020             2 0
048900170306     C                   movel     g08dat        STPDED            6 0
049000170306     C                   move      w0020         STPDED
049100170308     c*
049200170306     c
049300070522     C                   MOVEL     '2C'          COD
049400070522     C                   MOVEL     *BLANKS       KEY
049500071011     C                   MOVEL     GCscfg        KEY
049600070522     C     KTAB          CHAIN     TABEL                              50
049700170306     C  N50              MOVEL     TBLUNI        gcscfgd          26
049800071011     C   50              MOVEL     *ALL'?'       gcscfgd
049900170306     c* Vedo se è già a mag gaicenze oppure no
050000170306     c                   if        gcsdlma=0
050100170306     c                   eval      stpsta='NO'
050200170306     c                   else
050300170306     c                   eval      stpsta='SI'
050400170306     c                   endif
050500170306     C                   END
050600170308     c
050700170308     c                   clear                   stpfdep           2
050800170308     c                   if        *in01  and gcsffd='S'
050900170308     c                   eval      stpfdep='SI'
051000170308     c                   endif
051100170306
051200050221     C                   EXCEPT    RIGA
051300070629      * stampa dettaglio dei segnacolli se non sequenziali
051400070629     c                   if        skt(1) > 0
051500070629     c                   except    segn1
051600070629     c                   endif
051700070629     c                   if        skt(21) > 0
051800070629     c                   except    segn2
051900070629     c                   endif
052000070629     c                   if        skt(41) > 0
052100070629     c                   except    segn3
052200070629     c                   endif
052300070629     c                   if        skt(61) > 0
052400070629     c                   except    segn4
052500070629     c                   endif
052600070629     c                   if        skt(81) > 0
052700070629     c                   except    segn5
052800070629     c                   endif
052900071026      * solo per merce da prelevare dal magazzino *IN03=ON
053000071026      * seconda riga dettaglio bolla figlia
053100071026      * testa se il campo spedizione è pieno per capire se è stato
053200071026      * agganciato il legame altrimenti stampa anomalia
053300071026     c   03              if        gcsnspf > 0
053400140718     c                   movel     gcsRSMf       gcsRSMfs         23
053500071026     C                   EXCEPT    RIGAf
053600071026      * stampa dettaglio dei segnacolli se non sequenziali
053700071026     c                   if        sktf(1) > 0
053800071026     c                   except    segn1f
053900071026     c                   endif
054000071026     c                   if        sktf(21) > 0
054100071026     c                   except    segn2f
054200071026     c                   endif
054300071026     c                   if        sktf(41) > 0
054400071026     c                   except    segn3f
054500071026     c                   endif
054600071026     c                   if        sktf(61) > 0
054700071026     c                   except    segn4f
054800071026     c                   endif
054900071026     c                   if        sktf(81) > 0
055000071026     c                   except    segn5f
055100071026     c                   endif
055200071026      * separazione
055300071026     C                   EXCEPT    separa
055400071026     c                   else
055500071026     c                   if        gcsdis = 'D' or gcsdis = 'R'
055600071026     C                   EXCEPT    errRIGAf
055700071026     c                   endif
055800071026     c                   endif
055900071011      *
056000071011     c                   enddo
056100071011     c                   endsr
056200071011      *_________________________________________________________
056300071011     c     scriviwf      begsr
056400071011      *_________________________________________________________
056500170308     c                   clear                   wfgcs080
056600071011     c                   z-add     arbaas        GCSaas
056700071011     c                   z-add     arblnp        GCSlnp
056800071011     c                   z-add     arbnrs        GCSnrs
056900071011     c                   z-add     arbnsp        GCSnsp
057000071011     c                   z-add     arblna        GCSlna
057100170308     c                   move      arbffd        GCSffd
057200071011     c                   move      arbrsm        GCSrsm
057300071011     c                   move      arbrsd        GCSrsd
057400141215     c                   if        arbznc <> znc and znc <> 0
057500141215     c                   z-add     znc           gcsznc
057600141215     c                   else
057700071011     c                   z-add     arbznc        GCSznc
057800141215     c                   endif
057900071011     c                   z-add     gcpfgc        GCSfgc
058000071011     c                   z-add     gcpngc        GCSngc
058100071011     c                   z-add     gcpdeda       GCSdeda
058200170306     c                   z-add     gcpdlma       GCSdlma
058300071011     c                   move      gcpdis        GCSdis
058400071011     c                   move      gcpcfg        GCScfg
058500170126     c
058600170126     c* se non è stato portato in mag giacenza, non stampo
058700170126     c*  l'ubicazine, altrimenti si
058800170307     c   03              if        gcpdlma>0
058900071011     c                   movel     §ar5zng       GCSzng
059000170126     c                   endif
059100170307     c  n03              movel     §ar5zng       GCSzng
059200161111     c* se la bolla ha la data merce a mag giacenze, significa che
059300161111     c*  probabilmente non e' stata portata in giacenza
059400161111     c*  quindi al posto della ubicazione stampo la data e ora ultima spunta
059500161111     c*
059600170126     c**                 if        gcpdlma=0
059700161111     C     Kart1         chain     Fnart01L
059800161111     c                   if        %found(fnart01l)
059900161111     c                   exsr      Ricspu
060000161111     c                   endif
060100170126     c***                endif
060200161111     c
060300071011     c                   move      §ar5bva       GCSbva
060400071026     c                   z-add     arbfls        GCSfls
060500071011     c                   z-add     arbncd        GCSncd
060600071011     c                   z-add     arbnca        GCSnca
060700071011     c                   move      arbfns        GCSfns
060800071011     c                   z-add     arbncl        GCSncl
060900071011     c                   z-add     arbpkf        GCSpkf
061000071011     c                   z-add     arbvlf        GCSvlf
061100071026      * solo per merce da prelevare da magazzino *IN03=on
061200071026      *figlia se non trova legame o blp non scrive nulla
061300071026     c   03              if        not *in31
061400071026     c                   z-add     blpaas        GCSaasf
061500071026     c                   z-add     blplnp        GCSlnpf
061600071026     c                   z-add     blpnrs        GCSnrsf
061700071026     c                   z-add     blpnsp        GCSnspf
061800071026     c                   z-add     blplna        GCSlnaf
061900071026     c                   move      blprsm        GCSrsmf
062000071026     c                   move      blprsd        GCSrsdf
062100071026     c                   z-add     blpznc        GCSzncf
062200071026     c                   z-add     blpfls        GCSflsf
062300071026     c                   z-add     blpncd        GCSncdf
062400071026     c                   z-add     blpnca        GCSncaf
062500071026     c                   move      blpfns        GCSfnsf
062600071026     c                   z-add     blpncl        GCSnclf
062700071026     c                   z-add     blppkf        GCSpkff
062800071026     c                   z-add     blpvlf        GCSvlff
062900071026     c                   endif
063000170308     c                   write     wfgcs080
063100071011     c                   endsr
063200161111     C**************************************************************************
063300161111     C* cerco spunta del collo in qualsiasi categoria
063400161111     C**************************************************************************
063500161111     C     RICSPU        BEGSR
063600161111     C                   CLEAR                   Wimm
063700161111     C                   CLEAR                   WSAVB
063800161111     C                   CLEAR                   WDFV
063900161111     C                   CLEAR                   WTROV
064000161111     C                   CLEAR                   WSPG
064100161111     C                   CLEAR                   stadsf           10
064200161111RM*  C     kbrv          CHAIN     FNBRV07l                           33
064300161111RM*  C*
064400161111RM* 1C     *IN33         DOWEQ     *OFF
064500161111     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
064600161111     c                   clear                   dslv53
064700161111     C                   z-add     brvnFV        D53NFV
064800161111     C                   MOVEL     brvNPG        D53NPG
064900161111     C                   MOVEL     brvFGS        D53FGS
065000161111     C****               MOVEL     brvfle        D53flf
065100161111     C                   CALL      'FNLV53R'
065200161111     C                   PARM                    DSLV53
065300161111     C* ERRORE
065400161111    2C     D53ERR        IFeq      ' '
065500161111     c                   movel     brvdfs        brvimm           14 0
065600161111     c                   move      brvhms        brvimm
065700161111    3c                   select
065800161111     c* Se maggiore memorizzo
065900161111     c                   when      d53dfv>wdfv
066000161111     C                   MOVEL     RECBRV        WSAVB
066100161111     C                   MOVEL     brvimm        Wimm             14 0
066200161111     C                   MOVEL     d53DFV        WDFV
066300161111     C                   MOVEL     d53spg        WSPG
066400161111     C                   MOVEL     'S'           WTROV             1
066500161111     c
066600161111     c* Se uguale tengo la data ora immissione + alta
066700161111     c                   when      d53dfv=wdfv  and brvimm>wimm
066800161111     C                   MOVEL     brvimm        Wimm             14 0
066900161111     C                   MOVEL     RECBRV        WSAVB
067000161111     C                   MOVEL     d53DFV        WDFV
067100161111     C                   MOVEL     d53spg        WSPG
067200161111     C                   MOVEL     'S'           WTROV             1
067300161111     C                   eval      wimm=brvimm
067400161111     c* Se stessa data e ora immissione, tengo cat 3 o 4
067500161111     c                   when      d53dfv=wdfv and brvimm=wimm and brvnpg<>2
067600161111     C                   MOVEL     brvimm        Wimm             14 0
067700161111     C                   MOVEL     RECBRV        WSAVB
067800161111     C                   MOVEL     d53DFV        WDFV
067900161111     C                   MOVEL     d53spg        WSPG
068000161111     C                   MOVEL     'S'           WTROV             1
068100161111     C                   eval      wimm=brvimm
068200161111    3c                   endsl
068300161111    2c                   endif
068400161111     c
068500161111RM*  C     Kartd         reade     FNBRV07l                               33
068600161111    1c                   enddo
068700161111     c
068800161111     C     WTROV         IFEQ      'S'
068900161111     C                   MOVEL     WSAVB         RECBRV
069000161111     c
069100161111     c* Memorizzo nel campo ubicazione
069200161111     C* DECODIFICO LA CATEGORIA SPUNTA
069300161111    1C     BRVNPG        IFEQ      3
069400161111     C                   MOVEL     '7B'          COD
069500161111     C                   MOVEL(P)  wspg          KEY
069600161111     C     KTAB          CHAIN     TABEL                              32
069700161111     C  N32              MOVEL     TBLUNI        ds7b
069800161111     C   32              CLEAR                   ds7b
069900161111     C                   MOVEL     §7bdesb       ST2CAT            3
070000161111   X1C                   ELSE
070100161111     C                   MOVEL     '7N'          COD
070200161111     C                   MOVEL(P)  BRVNPG        KEY
070300161111     C     KTAB          CHAIN     TABEL                              32
070400161111     C  N32              MOVEL     TBLUNI        ST2CAT
070500161111     C   32              CLEAR                   ST2CAT
070600161111    1C                   ENDIF
070700161111     C* SE CAT 4 AGGANCIO FNFVV PER AVERE DECODIFICA DEL PADRONCINO
070800161111    1C                   IF        BRVNPG=4
070900161111     C                   Z-ADD     BRVNFV        WNFV5
071000161111     C     KFVV2         CHAIN(N)  FNFVV000                           32
071100161111     C  N32              eval      stadsf=fvvdsf
071200161111    1C                   ENDIF
071300161111     C* GIRO LA DATA
071400161111     C                   MOVE      WDFV          G02INV
071500161111     C                   MOVE      *ZEROS        G02DAT
071600161111     C                   MOVEL     3             G02ERR
071700161111     C                   CALL      'XSRDA8'
071800161111     C                   PARM                    WLBDAT
071900161111     C                   MOVEL     G02DAT        ST2DFV            4 0
072000161111     c* ora
072100161111     c                   clear                   st2hms            4 0
072200161111     c                   eval      st2hms=brvhms/100
072300161111     c
072400170222     c***                eval      GCSzng=%trim(gcszng)
072500170222     c***                if        %subst(gcszng:1:1)<>' '
072600170222     c***                eval      gcszng=%trim(gcszng)+'-'
072700170222     c***                endif
072800161111     c
072900161111     c                   if        brvnpg<>4
073000170222     c                   eval      GCSultsp=
073100161111     c                             st2cat+
073200161111     c                             ' ' +%editw(st2dfv:'  /  ')+
073300161111     c                             ' '+%editw(st2hms:'  :  ')
073400161111     c                   else
073500170222     c                   eval      GCSultsp=
073600161111     c                             st2cat+
073700161111     c                             ' ' +%editw(st2dfv:'  /  ')+
073800161111     c                             ' '+stadsf
073900161111     c                   endif
074000170310     c                   else
074100170310     c                   clear                   recbrv
074200170310     c                   clear                   gcsultsp
074300170310     C                   ENDIF
074400161111     c
074500161111     c                   endsr
074600050221     C*---------------------------------------------------------------*
074700050221     C     *INZSR        BEGSR
074800050221     C*---------------------------------------------------------------*
074900050221     C     *ENTRY        PLIST
075000050221     C                   PARM                    KPJBA
075100050221     C                   MOVEL     KPJBU         FNLG87
075200050221      *
075300050322     c                   move      d87comm       gescomm           1
075400050221      *
075500050221     C                   Z-ADD     1             CODUT
075600050221     C                   CALL      'X§PARUT'
075700050221     C                   PARM                    UTEDSE
075800050221     C                   MOVEL     REC80         CNCR80
075900050221     C                   Z-ADD     0             SAVLNP            3 0
076000050221      *
076100050221     C* VERIFICO SE SI E' COLLEGATA UN SECONDO LIVELLO
076200050221     C     SimTpP        COMP      '2'                                    02
076300170221     c* la filiale da elaborare non è più dal profilo utente ma passata in DS
076400170221     C*****              Z-ADD     SIMPOU        §FLE
076500170221     c                   if        d87fgs<=*zeros
076600170221     C                   Z-ADD     SIMPOU        §FLE
076700170221     c                   else
076800170221     C                   movel     d87fgs        §FLE
076900170221     c                   endif
077000050221      *
077100050221     C                   TIME                    WHHUDT           14 0
077200050221     C                   MOVEL     WHHUDT        ORA               6 0
077300050221     C                   MOVE      WHHUDT        GMAUD8            8 0
077400050221     C* GIRO UDATE
077500050221     C                   MOVE      GMAUD8        G08DAT
077600050221     C                   MOVE      *ZEROS        G08INV
077700050221     C                   MOVEL     *BLANK        G08ERR
077800050221     C                   CALL      'XSRDA8'
077900050221     C                   PARM                    WLBDA8
078000050221     C                   Z-ADD     G08INV        AMGUD8            8 0
078100050221     C*
078200050221      *Trasformo la data dal formato AA/MM/GG al formato GG/MM/AA
078300050221     C     D87RIS        IFEQ      'R'
078400050221     C                   MOVE      D87DAT        G08INV
078500050221     C                   MOVE      *ZEROS        G08DAT
078600050221     C                   MOVEL     3             G08ERR
078700050221     C                   CALL      'XSRDA8'
078800050221     C                   PARM                    WLBDA8
078900050221     C                   Z-ADD     G08DAT        GMADRI            8 0
079000050221     C                   END
079100050221     C*
079200071011      * genero file di work in qtemp
079300071011     c                   Movel(p)  cmd2(1)       comman
079400071011     c                   eval      lenght = %len(%trim(comman))
079500071011     c                   Call      'QCMDEXC'                            97
079600071011     c                   Parm                    comman
079700071011     c                   Parm                    lenght
079800170308     c                   open      wfgcs80f
079900050221     C     D87TST        IFEQ      '1'
080000071011      * merce da portare
080100050221     C                   SETON                                        01
080200180119     c                   if        d87ord='Z'
080300071011     c                   Movel(p)  cmd6(1)       comman
080400180119     c                   else
080500180119     c                   Movel(p)  cmd10(1)      comman
080600180119     c                   endif
080700071011     c                   eval      lenght = %len(%trim(comman))
080800071011     c                   Call      'QCMDEXC'                            97
080900071011     c                   Parm                    comman
081000071011     c                   Parm                    lenght
081100180119     c                   if        d87ord='Z'
081200170308     c                   open      wfgcs82l
081300180119     c                   else
081400180119     c                   open      wfgcs86l
081500180119     c                   endif
081600180119     c
081700050221     C                   ELSE
081800071011      * merce da prelevare
081900050221     C                   SETON                                        03
082000170224     c                   if        d87ord='Z'
082100071011     c                   Movel(p)  cmd4(1)       comman
082200170222     c                   else
082300170222     c                   Movel(p)  cmd8(1)       comman
082400170222     c                   endif
082500071011     c                   eval      lenght = %len(%trim(comman))
082600071011     c                   Call      'QCMDEXC'                            97
082700071011     c                   Parm                    comman
082800071011     c                   Parm                    lenght
082900170224     c                   if        d87ord='Z'
083000170308     c                   open      wfgcs81l
083100170222     c                   else
083200170308     c                   open      wfgcs85l
083300170222     c                   endif
083400170222     c
083500050221     C                   END
083600071011     c   97              except    errore
083700170307
083800170307     C* SOLO SPED IN FERMO DEPOSITO
083900170307     c                   if        *in01 and d87fdep='S'
084000170307     c                   seton                                        06
084100170307     c                   endif
084200170331     c                   if        *in01 and d87MES ='S'
084300170331     c                   seton                                        07
084400170331     c                   endif
084500170331
084600050221      *
084700050221     C* DEFINIZIONE CHIAVI
084800050221     C     KGCp          KLIST
084900050221     C                   KFLD                    FLE
085000050221     C                   KFLD                    FAS               3 0
085100161111RM*  C     KFVV2         KLIST
085200161111RM*  C                   KFLD                    BRVNPG
085300161111RM*  C                   KFLD                    WNFV5
085400161111RM*  C                   KFLD                    BRVFGS
085500161111RM*  C     KARTD         KLIST
085600161111RM*  C                   KFLD                    ARTFLS
085700161111RM*  C                   KFLD                    ARTLNA
085800161111RM*  C                   KFLD                    ARTNRS
085900161111RM*  C                   KFLD                    ARTNSC
086000050221     C*
086100050221     C     KARB          KLIST
086200050412     C                   KFLD                    GCPAASa
086300050412     C                   KFLD                    GCPLNPa
086400050412     C                   KFLD                    GCPNRSa
086500050412     C                   KFLD                    GCPNSPa
086600141215      *
086700141215     C     Kart1         KLIST
086800141216     C                   KFLD                    ARbAAS
086900141216     C                   KFLD                    ARbLNP
087000141216     C                   KFLD                    ARbNRS
087100141216     C                   KFLD                    ARbNSP
087200141216     C     Kbrv          KLIST
087300141219     C                   KFLD                    artFLS
087400141216     C                   KFLD                    artLNA
087500141216     C                   KFLD                    artNRS
087600141216     C                   KFLD                    artNSC
087700071026     C*
087800071026     C     Karbf         KLIST
087900071026     C                   KFLD                    arbAAS
088000071026     C                   KFLD                    arbLNP
088100071026     C                   KFLD                    arbNRS
088200071026     C                   KFLD                    arbNSP
088300071026     C*
088400071026     C     Kblp          KLIST
088500071026     C                   KFLD                    LBLAAN
088600071026     C                   KFLD                    LBLLPN
088700071026     C                   KFLD                    LBLNRN
088800071026     C                   KFLD                    LBLNSN
088900071026      *
089000071015     C     Kart          KLIST
089100071015     C                   KFLD                    GCsAAS
089200071015     C                   KFLD                    GCsLNP
089300071015     C                   KFLD                    GCsNRS
089400071015     C                   KFLD                    GCsNSP
089500050221     C*
089600071026     C     Kblt          KLIST
089700071026     C                   KFLD                    gcsAASf
089800071026     C                   KFLD                    gcsLNPf
089900071026     C                   KFLD                    gcsNRSf
090000071026     C                   KFLD                    gcsNSPf
090100071026     C*
090200071026     C     KAR5          KLIST
090300071026     C                   KFLD                    GCPAASa
090400071026     C                   KFLD                    GCPLNPa
090500071026     C                   KFLD                    GCPNRSa
090600071026     C                   KFLD                    GCPNSPa
090700071026     C                   KFLD                    trec5             3
090800071026     c                   move      'GEN'         trec5
090900050221     C*
091000050221     C     KTAB          KLIST
091100050221     C                   KFLD                    CODUT
091200050221     C                   KFLD                    COD               2
091300050221     C                   KFLD                    KEY               8
091400170331     C     KTAB2         KLIST
091500170331     C                   KFLD                    CODUT
091600170331     C                   KFLD                    COD
091700050221     C*
091800071026     c                   movel     *all'_'       lin198          198
091900170331     C***
092000170331     C* CARICO EVENTUALI ALTRI FILIALI GESTITE DA QUELLA COLLEGATA
092100170331     C***
092200050221     c                   clear                   trul06ds
092300050221     c                   eval      d06cod = '£6'
092400050221     c                   movel     §fle          d06key
092500050221     c                   movel     trul06ds      kpjbu
092600050221     c                   call      'TRUL06R'
092700050221     c                   parm                    kpjba
092800050221     c                   movel     kpjbu         trul06ds
092900050221     c                   movea     lin           fil£6
093000170331     c
093100170331     C* carico ZONE MESSAGGERIE
093200170331    1c                   if        *in01 and d87MES ='S'
093300170331     c                   clear                   xx                4 0
093400170331     c                   clear                   yy                4 0
093500170331     c                   clear                   ZZ                4 0
093600170331     C                   EVAL      ZZ=1
093700170331   1aC                   DOW       FIL£6(zz)>0
093800170331     c
093900170331     c                   movel     fil£6(zz)     alffil            3
094000170331     c                   movel     fil£6(zz)     wpo
094100170331     c                   movel(P)  '03'          cod
094200170331     c                   movel(P)  fil£6(zz)     key
094300170331     c     ktab          setll     tabel
094400170331     c     ktab2         reade     tabel
094500170331    2c                   dow       not %eof(tabel00f) and  wpo=alffil
094600170331    3c                   if        tblflg=' '
094700170331     c                   movel     tblkey        wpo               3
094800170331    4c                   if        wpo=alffil
094900170331     c                   movel     tbluni        ds03
095000170331
095100170331    4c                   if        §03tpm='M'
095200170331     c                   add       1             xx
095300170331     c                   movel     tblkey        w005a             5
095400170331     c                   move      w005a         zone(xx)
095500170331    5c                   endif
095600170331    4c                   endif
095700170331    3c                   endif
095800170331ù    c     ktab2         reade     tabel
095900170331    2c                   enddo
096000170331
096100170331     c                   add       1             zz
096200170331   1ac                   enddo
096300170331    1c                   endif
096400170331    c
096500050221     C                   ENDSR
096600050221     C*****************************************************************
096700050221     Otigcp000  E            AGG0
096800050221     O                       GCPDuma
096900050221     Otigcp000  E            AGG5
097000050221     O                       GCPDLMa
097100050221     OPRTF198   E    03      TESTA          1 02
097200050221     O                       RAGUT             +  0
097300170221     O                                           25 '   '
097400050221     O                                         + 25 '* ELENCO SPEDIZIONI DA '
097500050221     O                                         +  0 'PRELEVARE DAL MAGAZZINO '
097600050221     O                                         +  0 'GIACENZE *'
097700050221     O                       §PGM               127
097800050221     O                       GMAUD8             147 '  /  /    '
097900170308     O                                          192 'PAG:'
098000170308     O                       PAGE          Z    198
098100050221     O          E    01      TESTA          1 02
098200050221     O                       RAGUT             +  0
098300170221     O                                           25 '   '
098400050221     O                                         + 25 '* ELENCO SPEDIZIONI DA '
098500050221     O                                         +  0 'PORTARE NEL MAGAZZINO '
098600050221     O                                         +  0 'GIACENZE *'
098700050221     O                       §PGM               127
098800050221     O                       GMAUD8             147 '  /  /    '
098900170308     O                                          192 'PAG:'
099000170308     O                       PAGE          Z    198
099100050221     O          E            TESTA          1
099200050221     O                       KNSIF             +  0
099300050221     O                       KNMUS             +  0
099400050221     O                       ORA                147 '  :  :  '
099500050221     O          E    03      TESTA          1
099600050221     O                                         +  0 'CON DISPOSIZIONI DI:'
099700050221     O                       DESCR             +  1
099800071026     O          E    03 05   TESTA       1
099900071026     O                                         +  0 '* Prima riga bolla O'
100000071026     O                                         +  0 'riginale '
100100071026     O                                         +  0 'Seconda riga nuova '
100200071026     O                                         +  0 'Bolla *'
100300071026     O          E    03 05   TESTA          0
100400071026     O                                         +  0 '* Prima riga bolla O'
100500071026     O                                         +  0 'riginale '
100600071026     O                                         +  0 'Seconda riga nuova '
100700071026     O                                         +  0 'Bolla *'
100800071026     O          E    03 05   TESTA          0
100900071026     O                                         +  0 '* Prima riga bolla O'
101000071026     O                                         +  0 'riginale '
101100071026     O                                         +  0 'Seconda riga nuova '
101200071026     O                                         +  0 'Bolla *'
101300050221     O          E            TESTAR      1
101400170308     O                  03                     +  0 '---------  RISTAMPA'
101500170308     O                  03                     +  1 '---------  ELENCO DEL'
101600170308     O                  03   GMADRI            +  1 '  /  /    '
101700170307     O                  06                     +  5 '- SOLO SPEDIZIONI IN'
101800170307     O                  06                     +  1 'FERMO DEPOSITO -'
101900170331     O                  07                     +  5 '- SOLO ZONE MESSAGGE'
102000170331     O                  07                     +  0 'RIA -'
102100050221     O          E            TESTAT      2
102200071026     O                                            3 'LNP'
102300161125     O                                            7 'LNA'
102400161125     O                                           13 'ANNO'
102500161125     O                                           17 'SER'
102600161125     O                                           25 'SPEDIZ.'
102700161125     O                                           35 'Mittente'
102800161125     O                                           64 'Destinatario'
102900161125     O               03                          95 'Motivo di chiusura'
103000170306     O                                          109 'FlSgn'
103100170306     O                                          125 'Segnacol.Dal-Al'
103200170306     O                                          131 'Colli'
103300170306     O                                          140 'Peso Kg'
103400170306     O                                          145 'Zona'
103500170308     O               01                         156 'F.Deposito'
103600170308     O               03                         154 'DataEsec'
103700170306     O               03                         161 'MagGIA'
103800161125     o                                          172 'Ubicazione'
103900161125     o                                          195 'Val'
104000161125     o                                          198 'Sp'
104100050221     O          E            RIGA        2
104200071026     O                       GCsLNP               3
104300161125     O                       GCsLNA               7
104400161125     O                       GCsAAS              13
104500161125     O                       GCsNRS              17
104600161125     O                       GCsNSP        Z     25
104700161125     O                       arbRSMs             50
104800170125     O                       arbRSDs             75
104900170306     O                       gcscfgd            103
105000170306     O                       gcsfls        Z    108
105100170306     O                       gcsNCD        Z    117
105200170306     O                                          118 '/'
105300170306     O                       gcsNCA        Z    125
105400170306     O                       gcsNCL        Z    131
105500170306     O                       gcspkf        2    140
105600170306     O                       gcsZNC             144
105700170308     O               01      STPfdep            153
105800170306     O               03      STPDED             154 '  /  /  '
105900170306     O               03      STPsta             159
106000170306     O                       S30_zng            192
106100170306     O                       gcsbva             194
106200070629     O          E            segn1       1
106300070629     O                       sk1(1)        z   + 30
106400070629     O                       sk1(2)        z   +  1
106500070629     O                       sk1(3)        z   +  1
106600070629     O                       sk1(4)        z   +  1
106700070629     O                       sk1(5)        z   +  1
106800070629     O                       sk1(6)        z   +  1
106900070629     O                       sk1(7)        z   +  1
107000070629     O                       sk1(8)        z   +  1
107100070629     O                       sk1(9)        z   +  1
107200070629     O                       sk1(10)       z   +  1
107300070629     O                       sk1(11)       z   +  1
107400070629     O                       sk1(12)       z   +  1
107500070629     O                       sk1(13)       z   +  1
107600070629     O                       sk1(14)       z   +  1
107700070629     O                       sk1(15)       z   +  1
107800070629     O                       sk1(16)       z   +  1
107900070629     O                       sk1(17)       z   +  1
108000070629     O                       sk1(18)       z   +  1
108100070629     O                       sk1(19)       z   +  1
108200070629     O                       sk1(20)       z   +  1
108300070629     O          E            segn2       1
108400070629     O                       sk2(1)        z   + 30
108500070629     O                       sk2(2)        z   +  1
108600070629     O                       sk2(3)        z   +  1
108700070629     O                       sk2(4)        z   +  1
108800070629     O                       sk2(5)        z   +  1
108900070629     O                       sk2(6)        z   +  1
109000070629     O                       sk2(7)        z   +  1
109100070629     O                       sk2(8)        z   +  1
109200070629     O                       sk2(9)        z   +  1
109300070629     O                       sk2(10)       z   +  1
109400070629     O                       sk2(11)       z   +  1
109500070629     O                       sk2(12)       z   +  1
109600070629     O                       sk2(13)       z   +  1
109700070629     O                       sk2(14)       z   +  1
109800070629     O                       sk2(15)       z   +  1
109900070629     O                       sk2(16)       z   +  1
110000070629     O                       sk2(17)       z   +  1
110100070629     O                       sk2(18)       z   +  1
110200070629     O                       sk2(19)       z   +  1
110300070629     O                       sk2(20)       z   +  1
110400070629     O          E            segn3       1
110500070629     O                       sk3(1)        z   + 30
110600070629     O                       sk3(2)        z   +  1
110700070629     O                       sk3(3)        z   +  1
110800070629     O                       sk3(4)        z   +  1
110900070629     O                       sk3(5)        z   +  1
111000070629     O                       sk3(6)        z   +  1
111100070629     O                       sk3(7)        z   +  1
111200070629     O                       sk3(8)        z   +  1
111300070629     O                       sk3(9)        z   +  1
111400070629     O                       sk3(10)       z   +  1
111500070629     O                       sk3(11)       z   +  1
111600070629     O                       sk3(12)       z   +  1
111700070629     O                       sk3(13)       z   +  1
111800070629     O                       sk3(14)       z   +  1
111900070629     O                       sk3(15)       z   +  1
112000070629     O                       sk3(16)       z   +  1
112100070629     O                       sk3(17)       z   +  1
112200070629     O                       sk3(18)       z   +  1
112300070629     O                       sk3(19)       z   +  1
112400070629     O                       sk3(20)       z   +  1
112500070629     O          E            segn4       1
112600070629     O                       sk4(1)        z   + 30
112700070629     O                       sk4(2)        z   +  1
112800070629     O                       sk4(3)        z   +  1
112900070629     O                       sk4(4)        z   +  1
113000070629     O                       sk4(5)        z   +  1
113100070629     O                       sk4(6)        z   +  1
113200070629     O                       sk4(7)        z   +  1
113300070629     O                       sk4(8)        z   +  1
113400070629     O                       sk4(9)        z   +  1
113500070629     O                       sk4(10)       z   +  1
113600070629     O                       sk4(11)       z   +  1
113700070629     O                       sk4(12)       z   +  1
113800070629     O                       sk4(13)       z   +  1
113900070629     O                       sk4(14)       z   +  1
114000070629     O                       sk4(15)       z   +  1
114100070629     O                       sk4(16)       z   +  1
114200070629     O                       sk4(17)       z   +  1
114300070629     O                       sk4(18)       z   +  1
114400070629     O                       sk4(19)       z   +  1
114500070629     O                       sk4(20)       z   +  1
114600070629     O          E            segn5       1
114700070629     O                       sk5(1)        z   + 30
114800070629     O                       sk5(2)        z   +  1
114900070629     O                       sk5(3)        z   +  1
115000070629     O                       sk5(4)        z   +  1
115100070629     O                       sk5(5)        z   +  1
115200070629     O                       sk5(6)        z   +  1
115300070629     O                       sk5(7)        z   +  1
115400070629     O                       sk5(8)        z   +  1
115500070629     O                       sk5(9)        z   +  1
115600070629     O                       sk5(10)       z   +  1
115700070629     O                       sk5(11)       z   +  1
115800070629     O                       sk5(12)       z   +  1
115900070629     O                       sk5(13)       z   +  1
116000070629     O                       sk5(14)       z   +  1
116100070629     O                       sk5(15)       z   +  1
116200070629     O                       sk5(16)       z   +  1
116300070629     O                       sk5(17)       z   +  1
116400070629     O                       sk5(18)       z   +  1
116500070629     O                       sk5(19)       z   +  1
116600070629     O                       sk5(20)       z   +  1
116700071026     O          E            RIGAf       1
116800071026     O                       GCsLNPf              3
116900161125     O                       GCsLNAf              7
117000161125     O                       GCsAASf             13
117100161125     O                       GCsNRSf             17
117200161125     O                       GCsNSPf       Z     25
117300161125     O                       gcsRSMfs            50
117400170125     O                       gcsRSDf             87
117500170306     O                       gcsflsf       z    108
117600170306     O                       gcsNCDf       Z    117
117700170306     O                                          118 '/'
117800170306     O                       gcsNCAf       Z    125
117900161125     O                       spuntato           198
118000071026     O          E            segn1f      1
118100071026     O                       sk1f(1)       z   + 30
118200071026     O                       sk1f(2)       z   +  1
118300071026     O                       sk1f(3)       z   +  1
118400071026     O                       sk1f(4)       z   +  1
118500071026     O                       sk1f(5)       z   +  1
118600071026     O                       sk1f(6)       z   +  1
118700071026     O                       sk1f(7)       z   +  1
118800071026     O                       sk1f(8)       z   +  1
118900071026     O                       sk1f(9)       z   +  1
119000071026     O                       sk1f(10)      z   +  1
119100071026     O                       sk1f(11)      z   +  1
119200071026     O                       sk1f(12)      z   +  1
119300071026     O                       sk1f(13)      z   +  1
119400071026     O                       sk1f(14)      z   +  1
119500071026     O                       sk1f(15)      z   +  1
119600071026     O                       sk1f(16)      z   +  1
119700071026     O                       sk1f(17)      z   +  1
119800071026     O                       sk1f(18)      z   +  1
119900071026     O                       sk1f(19)      z   +  1
120000071026     O                       sk1f(20)      z   +  1
120100071026     O          E            segn2f      1
120200071026     O                       sk2f(1)       z   + 30
120300071026     O                       sk2f(2)       z   +  1
120400071026     O                       sk2f(3)       z   +  1
120500071026     O                       sk2f(4)       z   +  1
120600071026     O                       sk2f(5)       z   +  1
120700071026     O                       sk2f(6)       z   +  1
120800071026     O                       sk2f(7)       z   +  1
120900071026     O                       sk2f(8)       z   +  1
121000071026     O                       sk2f(9)       z   +  1
121100071026     O                       sk2f(10)      z   +  1
121200071026     O                       sk2f(11)      z   +  1
121300071026     O                       sk2f(12)      z   +  1
121400071026     O                       sk2f(13)      z   +  1
121500071026     O                       sk2f(14)      z   +  1
121600071026     O                       sk2f(15)      z   +  1
121700071026     O                       sk2f(16)      z   +  1
121800071026     O                       sk2f(17)      z   +  1
121900071026     O                       sk2f(18)      z   +  1
122000071026     O                       sk2f(19)      z   +  1
122100071026     O                       sk2f(20)      z   +  1
122200071026     O          E            segn3f      1
122300071026     O                       sk3f(1)       z   + 30
122400071026     O                       sk3f(2)       z   +  1
122500071026     O                       sk3f(3)       z   +  1
122600071026     O                       sk3f(4)       z   +  1
122700071026     O                       sk3f(5)       z   +  1
122800071026     O                       sk3f(6)       z   +  1
122900071026     O                       sk3f(7)       z   +  1
123000071026     O                       sk3f(8)       z   +  1
123100071026     O                       sk3f(9)       z   +  1
123200071026     O                       sk3f(10)      z   +  1
123300071026     O                       sk3f(11)      z   +  1
123400071026     O                       sk3f(12)      z   +  1
123500071026     O                       sk3f(13)      z   +  1
123600071026     O                       sk3f(14)      z   +  1
123700071026     O                       sk3f(15)      z   +  1
123800071026     O                       sk3f(16)      z   +  1
123900071026     O                       sk3f(17)      z   +  1
124000071026     O                       sk3f(18)      z   +  1
124100071026     O                       sk3f(19)      z   +  1
124200071026     O                       sk3f(20)      z   +  1
124300071026     O          E            segn4f      1
124400071026     O                       sk4f(1)       z   + 30
124500071026     O                       sk4f(2)       z   +  1
124600071026     O                       sk4f(3)       z   +  1
124700071026     O                       sk4f(4)       z   +  1
124800071026     O                       sk4f(5)       z   +  1
124900071026     O                       sk4f(6)       z   +  1
125000071026     O                       sk4f(7)       z   +  1
125100071026     O                       sk4f(8)       z   +  1
125200071026     O                       sk4f(9)       z   +  1
125300071026     O                       sk4f(10)      z   +  1
125400071026     O                       sk4f(11)      z   +  1
125500071026     O                       sk4f(12)      z   +  1
125600071026     O                       sk4f(13)      z   +  1
125700071026     O                       sk4f(14)      z   +  1
125800071026     O                       sk4f(15)      z   +  1
125900071026     O                       sk4f(16)      z   +  1
126000071026     O                       sk4f(17)      z   +  1
126100071026     O                       sk4f(18)      z   +  1
126200071026     O                       sk4f(19)      z   +  1
126300071026     O                       sk4f(20)      z   +  1
126400071026     O          E            segn5f      1
126500071026     O                       sk5f(1)       z   + 30
126600071026     O                       sk5f(2)       z   +  1
126700071026     O                       sk5f(3)       z   +  1
126800071026     O                       sk5f(4)       z   +  1
126900071026     O                       sk5f(5)       z   +  1
127000071026     O                       sk5f(6)       z   +  1
127100071026     O                       sk5f(7)       z   +  1
127200071026     O                       sk5f(8)       z   +  1
127300071026     O                       sk5f(9)       z   +  1
127400071026     O                       sk5f(10)      z   +  1
127500071026     O                       sk5f(11)      z   +  1
127600071026     O                       sk5f(12)      z   +  1
127700071026     O                       sk5f(13)      z   +  1
127800071026     O                       sk5f(14)      z   +  1
127900071026     O                       sk5f(15)      z   +  1
128000071026     O                       sk5f(16)      z   +  1
128100071026     O                       sk5f(17)      z   +  1
128200071026     O                       sk5f(18)      z   +  1
128300071026     O                       sk5f(19)      z   +  1
128400071026     O                       sk5f(20)      z   +  1
128500071026     O          E            separa      1
128600071026     o                       lin198             198
128700071026     O          E            errRIGAf    1
128800071026     O                                         + 25 '* Attenzione non trovata '
128900071026     O                                         +  0 'spedizione figlia verifi'
129000071026     O                                         +  0 'care i dati della bolla *'
129100071011     O          E            errore         1 02
129200071011     O                       RAGUT             +  0
129300170221     O                                           25 '   '
129400071011     O                                         + 25 '* ELENCO SPEDIZIONI DA '
129500071011     O                                         +  0 'PORTARE NEL MAGAZZINO '
129600071011     O                                         +  0 'GIACENZE *'
129700071011     O                       §PGM               127
129800071011     O                       GMAUD8             147 '  /  /    '
129900071011     O                                         + 10 'PAG:'
130000071011     O                       PAGE          Z   +  1
130100071011     O          E            errore      2
130200071011     O                                         + 25 '* Attenzione errore in '
130300071011     O                                         +  0 'esecuzione programma  '
130400071011     O                                         +  0 'avvisare il CED *'
130500071011      *_________________________________________________________________________
130600071011**  CMD1
130700170308DLTF FILE(QTEMP/WFGCS80F)
130800071011**  CMD2
130900170308CRTDUPOBJ OBJ(WFGCS80F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
131000071011**  CMD3
131100170308DLTF FILE(QTEMP/WFGCS81L)
131200071011**  CMD4
131300170308CRTDUPOBJ OBJ(WFGCS81L) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
131400071011**  CMD5
131500170308DLTF FILE(QTEMP/WFGCS82L)
131600071011**  CMD6
131700170308CRTDUPOBJ OBJ(WFGCS82L) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
131800170222**  CMD7
131900170308DLTF FILE(QTEMP/WFGCS85L)
132000170222**  CMD8
132100170308CRTDUPOBJ OBJ(WFGCS85L) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
132200180119**  CMD9
132300180119DLTF FILE(QTEMP/WFGCS86L)
132400180119**  CMd10
132500180119CRTDUPOBJ OBJ(WFGCS86L) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)

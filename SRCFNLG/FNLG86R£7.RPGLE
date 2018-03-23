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
001300070629     Ffnart01L  IF   E           K DISK
001400071026     Ffnblt01L  IF   E           K DISK
001500141215     Ffnbrv07L  IF   E           K DISK
001600161111     Ffnfvv01l  IF   E           K DISK
001700050221     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
001800050221     D FIL£6           S              3  0 DIM(30)
001900170331     D zone            S              5    DIM(200)
002000170331     d
002100050221     D §FLE            S              3  0
002200050221     D WLBDA8          DS
002300050221     D  G08DAT                 1      8  0
002400050221     D  G08INV                 9     16  0
002500050221     D  G08ERR                17     17
002600050221     D  G08TGI                18     22  0
002700050221      *
002800161111     D RECBRV        E DS                  EXTNAME(FNBRV00F)
002900161111     D WSAVB         E DS                  EXTNAME(FNBRV00F)
003000161111     d                                     prefix(S_)
003100050221     d dar5gen       e ds
003200050221     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
003300070629     d dskiere         ds
003400070629     d  sk1                    1    140  0 dim(20)
003500070629     d  sk2                  141    280  0 dim(20)
003600070629     d  sk3                  281    420  0 dim(20)
003700070629     d  sk4                  421    560  0 dim(20)
003800070629     d  sk5                  561    700  0 dim(20)
003900070629     d  skt                    1    700  0 dim(100)
004000071026     d dskieref        ds
004100071026     d  sk1f                   1    140  0 dim(20)
004200071026     d  sk2f                 141    280  0 dim(20)
004300071026     d  sk3f                 281    420  0 dim(20)
004400071026     d  sk4f                 421    560  0 dim(20)
004500071026     d  sk5f                 561    700  0 dim(20)
004600071026     d  sktf                   1    700  0 dim(100)
004700071026
004800070629
004900161007     D tigcp00ds     E DS                  extname(tigcp00f)
005000161007     D fnlgi1ds      E DS
005100050221     D CNCR80        E DS
005200050221     D KPJBA         E DS
005300161111     D DS7B          E DS
005400170331     D DS03          E DS
005500050221     d trul06ds      e ds
005600050221     d  lin                    1     90  0 dim(30)
005700050221     D FNLG87        E DS                  EXTNAME(FNLG87DS)
005800161111     D* REPERISCE DATI FOGLI
005900161111     D DSLV53        E DS                  EXTNAME(FNLV53DS)
006000050221      *
006100050221     D                SDS
006200050221     D  §PGM                   1     10
006300071011     D cmd1            S             66    DIM(01) CTDATA PERRCD(1)
006400071011     D cmd2            S             66    DIM(01) CTDATA PERRCD(1)
006500071011     D cmd3            S             66    DIM(01) CTDATA PERRCD(1)
006600071011     D cmd4            S             66    DIM(01) CTDATA PERRCD(1)
006700071011     D cmd5            S             66    DIM(01) CTDATA PERRCD(1)
006800071011     D cmd6            S             66    DIM(01) CTDATA PERRCD(1)
006900170222     D cmd7            S             66    DIM(01) CTDATA PERRCD(1)
007000170222     D cmd8            S             66    DIM(01) CTDATA PERRCD(1)
007100161111     D WLBDAT          DS
007200161111     D  G02DAT                 1      8  0
007300161111     D  G02INV                 9     16  0
007400161111     D  G02ERR                17     17
007500161111     D  G02TGI                18     22  0
007600071011     d comman          s             80
007700071011     d lenght          s             15  5 inz(80)
007800161111     d wdfv            S                   LIKE(d53dfv)
007900161111     d WSPG            S                   LIKE(FVVSPG)
008000161111     d Wnfv5           S                   LIKE(FVVnfv)
008100170331     d wok             s              1
008200070629      *----------------------------------------------------------------
008300050221     C                   DO        30            I                 2 0
008400050221     c                   if        fil£6(i) = 0
008500050221     c                   leave
008600050221     c                   endif
008700050221     C                   Z-ADD     fil£6(i)      FLE               3 0
008800050221     C*
008900050221     c   01              z-add     10            fas
009000050221     c   03              z-add     35            fas
009100050221      * lettura giacenze
009200050221      *
009300050418     c     kgcp          setll     tigcp26l
009400000607     c                   do        *hival
009500050418     c     fle           reade     tigcp26l                             90
009600050413      *allocazione
009700050413     c                   if        *in90
009800050418     c     fle           reade(n)  tigcp26l
009900050413     c                   if        %eof
010000050413     c                   leave
010100050413     c                   end
010200050413     c                   iter
010300050413     c                   endif
010400050413      *
010500000607     c                   if        %eof
010600000607     c                   leave
010700000607     c                   end
010800050221      * se richiesto materiale da portare a magazzino la fase non può essere
010900050221      * maggiore di 26
011000050418     c                   if        gcpfas > 30 and *in01
011100050221     c                   leave
011200040507     c                   end
011300050221
011400050408      *solo progressivo 0
011500050408     c                   if        gcpfrg <> 0
011600050408     c                   iter
011700050408     c                   end
011800161007     c*
011900161007     c                   clear                   fnlgi1ds
012000161007     c                   eval      ilgi1agc=gcpagc
012100161007     c                   eval      ilgi1fgc=gcpfgc
012200161007     c                   eval      ilgi1ngc=gcpngc
012300161007     c                   movel     fnlgi1ds      kpjbu
012400161007     c                   call      'FNLGI1R'
012500161007     c                   parm                    kpjba
012600161007     c                   parm                    tigcp00ds
012700161007     c                   movel     kpjbu         fnlgi1ds
012800161007
012900050411      * in uscita dal magazzino
013000050221--2)>C     GCPFAS        IFEQ      35
013100050221     C     GCPDuma       ANDEQ     0
013200050221     C     D87RIS        ANDEQ     *BLANK
013300050221     C     *IN03         ANDEQ     '1'
013400161007     c     olgi1outmg    andne     ' '
013500050221      *
013600050221     C     GCPFAS        OREQ      35
013700050221     C     GCPDuma       ANDEQ     D87DAT
013800050221     C     D87RIS        ANDEQ     'R'
013900050221     C     *IN03         ANDEQ     '1'
014000161007     c     olgi1outmg    andne     ' '
014100050221      *
014200050411      * in uscita dal magazzino x resi
014300141110---->C*mm  GCPFAS        OReq      50
014400141107---->C     GCPFAS        ORge      50
014500050411---->C     GCPdis        andeq     'R'
014600050411     C     GCPDuma       ANDEQ     0
014700050411     C     D87RIS        ANDEQ     *BLANK
014800050411     C     *IN03         ANDEQ     '1'
014900161007     c     olgi1outmg    andne     ' '
015000050221      *
015100050411---->C     GCPdis        oreq      'R'
015200050411     C     GCPDuma       ANDEQ     D87DAT
015300050411     C     D87RIS        ANDEQ     'R'
015400050411     C     *IN03         ANDEQ     '1'
015500161007     c     olgi1outmg    andne     ' '
015600161007      *
015700050411      * in uscita dal magazzino x dirottamenti
015800141110---->C*mm  GCPFAS        OReq      40
015900141107---->C     GCPFAS        ORge      40
016000050411---->C     GCPdis        andeq     'D'
016100050411     C     GCPDuma       ANDEQ     0
016200050411     C     D87RIS        ANDEQ     *BLANK
016300050411     C     *IN03         ANDEQ     '1'
016400161007     c     olgi1outmg    andne     ' '
016500050411      *
016600141110---->C*mm  GCPFAS        OReq      50
016700141110---->C*mm  GCPdis        andeq     'D'
016800141110     C*mm  GCPDuma       ANDEQ     0
016900141110     C*mm  D87RIS        ANDEQ     *BLANK
017000141110     C*mm  *IN03         ANDEQ     '1'
017100050420      *
017200050411---->C     GCPdis        oreq      'D'
017300050411     C     GCPDuma       ANDEQ     D87DAT
017400050411     C     D87RIS        ANDEQ     'R'
017500050411     C     *IN03         ANDEQ     '1'
017600161007     c     olgi1outmg    andne     ' '
017700050411
017800050411      *merce da portare in magazzino
017900050413---->C     GCPFAS        ORle      30
018000050221     C     D87RIS        ANDEQ     *BLANK
018100050221     C     *IN01         ANDEQ     '1'
018200161007     c     olgi1amg      andeq     'S'
018300161007     C**   GCPDLMa       ANDEQ     0
018400050221      *
018500050221     C     GCPFAS        ORle      70
018600050221     C     GCPDLMa       ANDEQ     D87DAT
018700050221     C     D87RIS        ANDEQ     'R'
018800050221     C     *IN01         ANDEQ     '1'
018900050221      *
019000050221     C* PRELEVO DATI BOLLA
019100141119     C     KARB          CHAIN     FNARB01L
019200170307    1c                   if        %found(fnarb01l)
019300050221     c                   clear                   dar5gen
019400050221     C     KAR5          CHAIN     FiAR501L
019500170307    2c                   if        %found(fiar501l)
019600050221     c                   movel     ar5uni        dar5gen
019700170307    2c                   end
019800141215     c                   clear                   znc               2 0
019900141216     C     Kart1         chain     Fnart01L
020000170307    2c                   if        %found(fnart01l)
020100141216     C     Kbrv          setll     Fnbrv07L
020200170307    3c                   do        *hival
020300141216     C     Kbrv          reade     Fnbrv07L
020400170307    4c                   if        %eof(fnbrv07l)
020500141215     c                   leave
020600170307    4c                   endif
020700170307    4c                   if        brvdcs = 0 or brvnpg = 8
020800141215     c                   iter
020900170307   X4c                   else
021000141215     c                   move      brvznc        znc
021100141215     c                   leave
021200170307    4c                   endif
021300170307    3c                   enddo
021400170307    2c                   endif
021500071026      * solo per merce da prelevare dal magazzino *IN03=on
021600071026      * se dirottamento o reso reperisce riferimenti figlia da stampare
021700170307    2c                   if        *in03
021800080220     c                   seton                                        31
021900170307    3c                   if        gcpdis = 'D' or gcpdis = 'R'
022000071026     C     Karb          chain     fnlbl02l                           31
022100071026     C  n31Kblp          chain     fnblp01l                           31
022200170307    3c                   endif
022300170307    2c                   endif
022400170307     c
022500170307     c* solo per merce da portare a mag giacenze posso parzializzare
022600170307     c*  per fermo deposito
022700170331     c                   eval      wok='S'
022800170331     c                   if        *in01 and *in06 and arbffd<>d87fdep
022900170331     c                   eval      wok='N'
023000170331     c                   endif
023100170331
023200170331     c* OPPURE per zone messaggeria
023300170331     c                   if        *in01 and *in07
023400170331     C                   IF        NOT *IN06  OR  WOK='N'
023500170331     c                   movel     arblna        alfzona           5
023600170331     c                   move      arbznc        alfzona
023700170331     c                   if        %lookup(alfzona:zone)=0
023800170331     c                   eval      wok='N'
023900170331     C                   ELSE
024000170331     c                   eval      wok='S'
024100170331     c                   endif
024200170331     c                   endif
024300170331     c                   endif
024400170307
024500071011      *scrittura wf con bolle da stampare successivamente
024600170331    2c*****              if        *in03 or d87fdep=' ' or arbffd=d87fdep
024700170331
024800170331    2c                   if        *in03 or (*in01 and wok='S')
024900071011     c                   exsr      scriviwf
025000170331     c
025100050221     c
025200071026     C* AGGIORNO DATA DI STAMPA
025300170307    3C     D87RIS        IFEQ      *BLANK
025400071011     C   01              MOVE      AMGUD8        GCPDLMa
025500071011     C   01              EXCEPT    AGG5
025600071011     C   03              MOVE      AMGUD8        GCPDuma
025700071011     C   03              EXCEPT    AGG0
025800170307    4c                   if        gescomm = 'S'
025900071011     c                   commit
026000170307    4c                   endif
026100170307    3c                   endif
026200170307    2c                   endif
026300071011
026400170307    1C                   END
026500071011     C*
026600170307    0C                   END
026700071011     C*
026800071011     c                   enddo
026900071011
027000071011     c                   enddo
027100071011      * eliminazione file di work
027200170308     c                   close     wfgcs80f
027300071011      * rilegge wf e produce il tabulato
027400071011     c                   exsr      stampa
027500071011      *
027600170302     c                   if        *in03  and d87ord='Z'
027700170308     c   03              close     wfgcs81l
027800170302     c                   else
027900170308     c                   close     wfgcs85l
028000170302     c                   endif
028100170308     c   01              close     wfgcs82l
028200170224     c                   if        *in03  and d87ord='Z'
028300170222     c                   Movel(p)  cmd3(1)       comman
028400170222     c                   endif
028500170222     c                   if        *in03  and d87ord='L'
028600170222     c                   Movel(p)  cmd7(1)       comman
028700170222     c                   endif
028800071011     c   01              Movel(p)  cmd5(1)       comman
028900071011     c                   eval      lenght = %len(%trim(comman))
029000071011     c                   Call      'QCMDEXC'                            97
029100071011     c                   Parm                    comman
029200071011     c                   Parm                    lenght
029300170302     c
029400170302     c* Cancellazione file fisico da QTEMP
029500170302     c                   Movel(p)  cmd1(1)       comman
029600170302     c                   eval      lenght = %len(%trim(comman))
029700170302     c                   Call      'QCMDEXC'                            97
029800170302     c                   Parm                    comman
029900170302     c                   Parm                    lenght
030000161007
030100161007     c                   clear                   fnlgi1ds
030200161007     c                   eval      ILGI1TLA= 'C'
030300161007     c                   movel     fnlgi1ds      kpjbu
030400161007     c                   call      'FNLGI1R'
030500161007     c                   parm                    kpjba
030600161111     c
030700161111     C                   CLEAR                   DSLV53
030800161111     C                   MOVEL     'C'           D53TLA
030900161111     C                   CALL      'FNLV53R'
031000161111     C                   PARM                    DSLV53
031100161111     C*
031200071011     c                   seton                                        lr
031300071011      *_________________________________________________________
031400071011     c     stampa        begsr
031500071011      *_________________________________________________________
031600071011      * legge la v.l. opportuna x l'ordinamento a seconda che sia
031700071011      * merce da prelevare o da portare a magazzino
031800170224     c   03              if        d87ord='Z'
031900170308     c     *loval        setll     wfgcs81l
032000170222     c                   endif
032100170222     c   03              if        d87ord='L'
032200170308     c     *loval        setll     wfgcs85l
032300170222     c                   endif
032400170308     c   01*loval        setll     wfgcs82l
032500170222
032600071011     c                   do        *hival
032700170222
032800170224     c   03              if        d87ord='Z'
032900170308     c                   read      wfgcs81l                               98
033000170222     c                   endif
033100170222     c   03              if        d87ord='L'
033200170308     c                   read      wfgcs85l                               98
033300170222     c                   endif
033400170308     c   01              read      wfgcs82l                               98
033500170222
033600071011     c   98              leave
033700071011      * segnacolli non sequenziali prelevo i numeri da dettaglio colli
033800070629     c                   clear                   skt
033900071011     c                   if        gcsfns = 'S'
034000071015     C     Kart          setll     Fnart01L
034100070629     c                   z-add     0             ncl               3 0
034200071011     c                   do        *hival
034300071015     C     Kart          reade     Fnart01L
034400070629     c                   if        %eof(fnart01l) or ncl = 100
034500070629     c                   leave
034600070629     c                   endif
034700070629     c                   add       1             ncl
034800070629     c                   z-add     artnsc        skt(ncl)
034900070629     c                   enddo
035000070629     c                   endif
035100070629     c
035200071026      * bolla figlia
035300071026     c                   clear                   sktf
035400140728     c                   clear                   spuntato          8
035500140728     c                   clear                   spu               3 0
035600140728     c                   clear                   nospu             3 0
035700170306     c                   clear                   stpsta            2
035800170306
035900140718     c                   if        *in03
036000071026     C     Kblt          setll     Fnblt01L
036100071026     c                   z-add     0             ncl               3 0
036200071026     c                   do        *hival
036300071026     C     Kblt          reade     Fnblt01L
036400071026     c                   if        %eof(fnblt01l) or ncl = 100
036500140728     c                   select
036600140728     c                   when      spu >= 1 and nospu = 0
036700140728     c                   eval      spuntato = 'Totale'
036800140728     c                   when      spu >= 1 and nospu >= 1
036900140728     c                   eval      spuntato = 'Parziale'
037000140728     c                   other
037100140728     c                   clear                   spuntato
037200140728     c                   endsl
037300071026     c                   leave
037400071026     c                   endif
037500140718     c* se presente la data entrata vuol dire
037600140718     c* che è già stato etichettato il pacco con la nuova
037700140718     c* etichetta della figlia
037800140728     c                   if        bltdse <> 0
037900140728     c                   add       1             spu
038000140728     c                   else
038100140728     c                   add       1             nospu
038200140728     c                   end
038300140718     c*
038400140718     c                   if        gcsfnsf = 'S'
038500071026     c                   add       1             ncl
038600071026     c                   z-add     bltnsc        sktf(ncl)
038700071026     c                   endif
038800140718     c                   enddo
038900140718     c                   endif
039000071026     c
039100050221      *
039200071011     C   03GCsDIS        IFNE      SAVDIS
039300050221     C* PRELEVO DESCRIZIONE TIPO DISPOSIZIONE
039400050221     C                   MOVEL     '2D'          COD
039500050221     C                   MOVEL     *BLANKS       KEY
039600071011     C                   MOVEL     GCsDIS        KEY
039700050221     C     KTAB          CHAIN     TABEL                              50
039800050221     C  N50              MOVEL     TBLUNI        DESCR            20
039900050221     C   50              MOVEL     *ALL'?'       DESCR
040000071026     c                   setoff                                       05
040100071026     c                   if        gcsdis  = 'D' or gcsdis = 'R'
040200071026     c                   seton                                        05
040300071026     c                   endif
040400050221     C                   EXCEPT    TESTA
040500050221     C     D87RIS        IFEQ      'R'
040600170308     c     *in06         oreq      *on
040700170331     c     *in07         oreq      *on
040800050221     C                   EXCEPT    TESTAR
040900050221     C                   END
041000050221     C                   EXCEPT    TESTAT
041100050221     C                   SETON                                        99
041200071011     C                   MOVE      GCsDIS        SAVDIS            1
041300071011     C                   END
041400050221      *
041500050221     C  N99
041600050221     COR OF              DO
041700050221     C                   EXCEPT    TESTA
041800050221     C     D87RIS        IFEQ      'R'
041900170308     c     *in06         oreq      *on
042000170331     c     *in07         oreq      *on
042100050221     C                   EXCEPT    TESTAR
042200050221     C                   END
042300050221     C                   EXCEPT    TESTAT
042400050221     C                   END
042500050221     C  N99              SETON                                        99
042600050221      *
042700140718     c                   movel     gcsrsm        arbrsms          23
042800140718     c                   movel     gcsrsd        arbrsds          23
042900170222     c***                movel     gcszng        s30_zng          30
043000170222
043100170222     c                   clear                   s30_zng          30
043200170222     c                   if        gcszng<>*blanks
043300170222     c                   eval      s30_zng=%trim(gcszng)+'-'+gcsultsp
043400170222     c                   else
043500170222     c                   eval      s30_zng=gcsultsp
043600170222     c                   endif
043700170222     c
043800050221     C     *IN03         IFEQ      '1'
043900071011     C                   Z-ADD     GCsdeda       G08INV
044000050221     C                   MOVE      *ZEROS        G08DAT
044100050221     C                   MOVEL     '3'           G08ERR
044200050221     C                   CALL      'XSRDA8'
044300050221     C                   PARM                    WLBDA8
044400170306     C                   move      G08DAT        w0020             2 0
044500170306     C                   movel     g08dat        STPDED            6 0
044600170306     C                   move      w0020         STPDED
044700170308     c*
044800170306     c
044900070522     C                   MOVEL     '2C'          COD
045000070522     C                   MOVEL     *BLANKS       KEY
045100071011     C                   MOVEL     GCscfg        KEY
045200070522     C     KTAB          CHAIN     TABEL                              50
045300170306     C  N50              MOVEL     TBLUNI        gcscfgd          26
045400071011     C   50              MOVEL     *ALL'?'       gcscfgd
045500170306     c* Vedo se è già a mag gaicenze oppure no
045600170306     c                   if        gcsdlma=0
045700170306     c                   eval      stpsta='NO'
045800170306     c                   else
045900170306     c                   eval      stpsta='SI'
046000170306     c                   endif
046100170306     C                   END
046200170308     c
046300170308     c                   clear                   stpfdep           2
046400170308     c                   if        *in01  and gcsffd='S'
046500170308     c                   eval      stpfdep='SI'
046600170308     c                   endif
046700170306
046800050221     C                   EXCEPT    RIGA
046900070629      * stampa dettaglio dei segnacolli se non sequenziali
047000070629     c                   if        skt(1) > 0
047100070629     c                   except    segn1
047200070629     c                   endif
047300070629     c                   if        skt(21) > 0
047400070629     c                   except    segn2
047500070629     c                   endif
047600070629     c                   if        skt(41) > 0
047700070629     c                   except    segn3
047800070629     c                   endif
047900070629     c                   if        skt(61) > 0
048000070629     c                   except    segn4
048100070629     c                   endif
048200070629     c                   if        skt(81) > 0
048300070629     c                   except    segn5
048400070629     c                   endif
048500071026      * solo per merce da prelevare dal magazzino *IN03=ON
048600071026      * seconda riga dettaglio bolla figlia
048700071026      * testa se il campo spedizione è pieno per capire se è stato
048800071026      * agganciato il legame altrimenti stampa anomalia
048900071026     c   03              if        gcsnspf > 0
049000140718     c                   movel     gcsRSMf       gcsRSMfs         23
049100071026     C                   EXCEPT    RIGAf
049200071026      * stampa dettaglio dei segnacolli se non sequenziali
049300071026     c                   if        sktf(1) > 0
049400071026     c                   except    segn1f
049500071026     c                   endif
049600071026     c                   if        sktf(21) > 0
049700071026     c                   except    segn2f
049800071026     c                   endif
049900071026     c                   if        sktf(41) > 0
050000071026     c                   except    segn3f
050100071026     c                   endif
050200071026     c                   if        sktf(61) > 0
050300071026     c                   except    segn4f
050400071026     c                   endif
050500071026     c                   if        sktf(81) > 0
050600071026     c                   except    segn5f
050700071026     c                   endif
050800071026      * separazione
050900071026     C                   EXCEPT    separa
051000071026     c                   else
051100071026     c                   if        gcsdis = 'D' or gcsdis = 'R'
051200071026     C                   EXCEPT    errRIGAf
051300071026     c                   endif
051400071026     c                   endif
051500071011      *
051600071011     c                   enddo
051700071011     c                   endsr
051800071011      *_________________________________________________________
051900071011     c     scriviwf      begsr
052000071011      *_________________________________________________________
052100170308     c                   clear                   wfgcs080
052200071011     c                   z-add     arbaas        GCSaas
052300071011     c                   z-add     arblnp        GCSlnp
052400071011     c                   z-add     arbnrs        GCSnrs
052500071011     c                   z-add     arbnsp        GCSnsp
052600071011     c                   z-add     arblna        GCSlna
052700170308     c                   move      arbffd        GCSffd
052800071011     c                   move      arbrsm        GCSrsm
052900071011     c                   move      arbrsd        GCSrsd
053000141215     c                   if        arbznc <> znc and znc <> 0
053100141215     c                   z-add     znc           gcsznc
053200141215     c                   else
053300071011     c                   z-add     arbznc        GCSznc
053400141215     c                   endif
053500071011     c                   z-add     gcpfgc        GCSfgc
053600071011     c                   z-add     gcpngc        GCSngc
053700071011     c                   z-add     gcpdeda       GCSdeda
053800170306     c                   z-add     gcpdlma       GCSdlma
053900071011     c                   move      gcpdis        GCSdis
054000071011     c                   move      gcpcfg        GCScfg
054100170126     c
054200170126     c* se non è stato portato in mag giacenza, non stampo
054300170126     c*  l'ubicazine, altrimenti si
054400170307     c   03              if        gcpdlma>0
054500071011     c                   movel     §ar5zng       GCSzng
054600170126     c                   endif
054700170307     c  n03              movel     §ar5zng       GCSzng
054800161111     c* se la bolla ha la data merce a mag giacenze, significa che
054900161111     c*  probabilmente non e' stata portata in giacenza
055000161111     c*  quindi al posto della ubicazione stampo la data e ora ultima spunta
055100161111     c*
055200170126     c**                 if        gcpdlma=0
055300161111     C     Kart1         chain     Fnart01L
055400161111     c                   if        %found(fnart01l)
055500161111     c                   exsr      Ricspu
055600161111     c                   endif
055700170126     c***                endif
055800161111     c
055900071011     c                   move      §ar5bva       GCSbva
056000071026     c                   z-add     arbfls        GCSfls
056100071011     c                   z-add     arbncd        GCSncd
056200071011     c                   z-add     arbnca        GCSnca
056300071011     c                   move      arbfns        GCSfns
056400071011     c                   z-add     arbncl        GCSncl
056500071011     c                   z-add     arbpkf        GCSpkf
056600071011     c                   z-add     arbvlf        GCSvlf
056700071026      * solo per merce da prelevare da magazzino *IN03=on
056800071026      *figlia se non trova legame o blp non scrive nulla
056900071026     c   03              if        not *in31
057000071026     c                   z-add     blpaas        GCSaasf
057100071026     c                   z-add     blplnp        GCSlnpf
057200071026     c                   z-add     blpnrs        GCSnrsf
057300071026     c                   z-add     blpnsp        GCSnspf
057400071026     c                   z-add     blplna        GCSlnaf
057500071026     c                   move      blprsm        GCSrsmf
057600071026     c                   move      blprsd        GCSrsdf
057700071026     c                   z-add     blpznc        GCSzncf
057800071026     c                   z-add     blpfls        GCSflsf
057900071026     c                   z-add     blpncd        GCSncdf
058000071026     c                   z-add     blpnca        GCSncaf
058100071026     c                   move      blpfns        GCSfnsf
058200071026     c                   z-add     blpncl        GCSnclf
058300071026     c                   z-add     blppkf        GCSpkff
058400071026     c                   z-add     blpvlf        GCSvlff
058500071026     c                   endif
058600170308     c                   write     wfgcs080
058700071011     c                   endsr
058800161111     C**************************************************************************
058900161111     C* cerco spunta del collo in qualsiasi categoria
059000161111     C**************************************************************************
059100161111     C     RICSPU        BEGSR
059200161111     C                   CLEAR                   Wimm
059300161111     C                   CLEAR                   WSAVB
059400161111     C                   CLEAR                   WDFV
059500161111     C                   CLEAR                   WTROV
059600161111     C                   CLEAR                   WSPG
059700161111     C                   CLEAR                   stadsf           10
059800161111RM*  C     kbrv          CHAIN     FNBRV07l                           33
059900161111RM*  C*
060000161111RM* 1C     *IN33         DOWEQ     *OFF
060100161111     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
060200161111     c                   clear                   dslv53
060300161111     C                   z-add     brvnFV        D53NFV
060400161111     C                   MOVEL     brvNPG        D53NPG
060500161111     C                   MOVEL     brvFGS        D53FGS
060600161111     C****               MOVEL     brvfle        D53flf
060700161111     C                   CALL      'FNLV53R'
060800161111     C                   PARM                    DSLV53
060900161111     C* ERRORE
061000161111    2C     D53ERR        IFeq      ' '
061100161111     c                   movel     brvdfs        brvimm           14 0
061200161111     c                   move      brvhms        brvimm
061300161111    3c                   select
061400161111     c* Se maggiore memorizzo
061500161111     c                   when      d53dfv>wdfv
061600161111     C                   MOVEL     RECBRV        WSAVB
061700161111     C                   MOVEL     brvimm        Wimm             14 0
061800161111     C                   MOVEL     d53DFV        WDFV
061900161111     C                   MOVEL     d53spg        WSPG
062000161111     C                   MOVEL     'S'           WTROV             1
062100161111     c
062200161111     c* Se uguale tengo la data ora immissione + alta
062300161111     c                   when      d53dfv=wdfv  and brvimm>wimm
062400161111     C                   MOVEL     brvimm        Wimm             14 0
062500161111     C                   MOVEL     RECBRV        WSAVB
062600161111     C                   MOVEL     d53DFV        WDFV
062700161111     C                   MOVEL     d53spg        WSPG
062800161111     C                   MOVEL     'S'           WTROV             1
062900161111     C                   eval      wimm=brvimm
063000161111     c* Se stessa data e ora immissione, tengo cat 3 o 4
063100161111     c                   when      d53dfv=wdfv and brvimm=wimm and brvnpg<>2
063200161111     C                   MOVEL     brvimm        Wimm             14 0
063300161111     C                   MOVEL     RECBRV        WSAVB
063400161111     C                   MOVEL     d53DFV        WDFV
063500161111     C                   MOVEL     d53spg        WSPG
063600161111     C                   MOVEL     'S'           WTROV             1
063700161111     C                   eval      wimm=brvimm
063800161111    3c                   endsl
063900161111    2c                   endif
064000161111     c
064100161111RM*  C     Kartd         reade     FNBRV07l                               33
064200161111    1c                   enddo
064300161111     c
064400161111     C     WTROV         IFEQ      'S'
064500161111     C                   MOVEL     WSAVB         RECBRV
064600161111     c
064700161111     c* Memorizzo nel campo ubicazione
064800161111     C* DECODIFICO LA CATEGORIA SPUNTA
064900161111    1C     BRVNPG        IFEQ      3
065000161111     C                   MOVEL     '7B'          COD
065100161111     C                   MOVEL(P)  wspg          KEY
065200161111     C     KTAB          CHAIN     TABEL                              32
065300161111     C  N32              MOVEL     TBLUNI        ds7b
065400161111     C   32              CLEAR                   ds7b
065500161111     C                   MOVEL     §7bdesb       ST2CAT            3
065600161111   X1C                   ELSE
065700161111     C                   MOVEL     '7N'          COD
065800161111     C                   MOVEL(P)  BRVNPG        KEY
065900161111     C     KTAB          CHAIN     TABEL                              32
066000161111     C  N32              MOVEL     TBLUNI        ST2CAT
066100161111     C   32              CLEAR                   ST2CAT
066200161111    1C                   ENDIF
066300161111     C* SE CAT 4 AGGANCIO FNFVV PER AVERE DECODIFICA DEL PADRONCINO
066400161111    1C                   IF        BRVNPG=4
066500161111     C                   Z-ADD     BRVNFV        WNFV5
066600161111     C     KFVV2         CHAIN(N)  FNFVV000                           32
066700161111     C  N32              eval      stadsf=fvvdsf
066800161111    1C                   ENDIF
066900161111     C* GIRO LA DATA
067000161111     C                   MOVE      WDFV          G02INV
067100161111     C                   MOVE      *ZEROS        G02DAT
067200161111     C                   MOVEL     3             G02ERR
067300161111     C                   CALL      'XSRDA8'
067400161111     C                   PARM                    WLBDAT
067500161111     C                   MOVEL     G02DAT        ST2DFV            4 0
067600161111     c* ora
067700161111     c                   clear                   st2hms            4 0
067800161111     c                   eval      st2hms=brvhms/100
067900161111     c
068000170222     c***                eval      GCSzng=%trim(gcszng)
068100170222     c***                if        %subst(gcszng:1:1)<>' '
068200170222     c***                eval      gcszng=%trim(gcszng)+'-'
068300170222     c***                endif
068400161111     c
068500161111     c                   if        brvnpg<>4
068600170222     c                   eval      GCSultsp=
068700161111     c                             st2cat+
068800161111     c                             ' ' +%editw(st2dfv:'  /  ')+
068900161111     c                             ' '+%editw(st2hms:'  :  ')
069000161111     c                   else
069100170222     c                   eval      GCSultsp=
069200161111     c                             st2cat+
069300161111     c                             ' ' +%editw(st2dfv:'  /  ')+
069400161111     c                             ' '+stadsf
069500161111     c                   endif
069600170310     c                   else
069700170310     c                   clear                   recbrv
069800170310     c                   clear                   gcsultsp
069900170310     C                   ENDIF
070000161111     c
070100161111     c                   endsr
070200050221     C*---------------------------------------------------------------*
070300050221     C     *INZSR        BEGSR
070400050221     C*---------------------------------------------------------------*
070500050221     C     *ENTRY        PLIST
070600050221     C                   PARM                    KPJBA
070700050221     C                   MOVEL     KPJBU         FNLG87
070800050221      *
070900050322     c                   move      d87comm       gescomm           1
071000050221      *
071100050221     C                   Z-ADD     1             CODUT
071200050221     C                   CALL      'X§PARUT'
071300050221     C                   PARM                    UTEDSE
071400050221     C                   MOVEL     REC80         CNCR80
071500050221     C                   Z-ADD     0             SAVLNP            3 0
071600050221      *
071700050221     C* VERIFICO SE SI E' COLLEGATA UN SECONDO LIVELLO
071800050221     C     SimTpP        COMP      '2'                                    02
071900170221     c* la filiale da elaborare non è più dal profilo utente ma passata in DS
072000170221     C*****              Z-ADD     SIMPOU        §FLE
072100170221     c                   if        d87fgs<=*zeros
072200170221     C                   Z-ADD     SIMPOU        §FLE
072300170221     c                   else
072400170221     C                   movel     d87fgs        §FLE
072500170221     c                   endif
072600050221      *
072700050221     C                   TIME                    WHHUDT           14 0
072800050221     C                   MOVEL     WHHUDT        ORA               6 0
072900050221     C                   MOVE      WHHUDT        GMAUD8            8 0
073000050221     C* GIRO UDATE
073100050221     C                   MOVE      GMAUD8        G08DAT
073200050221     C                   MOVE      *ZEROS        G08INV
073300050221     C                   MOVEL     *BLANK        G08ERR
073400050221     C                   CALL      'XSRDA8'
073500050221     C                   PARM                    WLBDA8
073600050221     C                   Z-ADD     G08INV        AMGUD8            8 0
073700050221     C*
073800050221      *Trasformo la data dal formato AA/MM/GG al formato GG/MM/AA
073900050221     C     D87RIS        IFEQ      'R'
074000050221     C                   MOVE      D87DAT        G08INV
074100050221     C                   MOVE      *ZEROS        G08DAT
074200050221     C                   MOVEL     3             G08ERR
074300050221     C                   CALL      'XSRDA8'
074400050221     C                   PARM                    WLBDA8
074500050221     C                   Z-ADD     G08DAT        GMADRI            8 0
074600050221     C                   END
074700050221     C*
074800071011      * genero file di work in qtemp
074900071011     c                   Movel(p)  cmd2(1)       comman
075000071011     c                   eval      lenght = %len(%trim(comman))
075100071011     c                   Call      'QCMDEXC'                            97
075200071011     c                   Parm                    comman
075300071011     c                   Parm                    lenght
075400170308     c                   open      wfgcs80f
075500050221     C     D87TST        IFEQ      '1'
075600071011      * merce da portare
075700050221     C                   SETON                                        01
075800071011     c                   Movel(p)  cmd6(1)       comman
075900071011     c                   eval      lenght = %len(%trim(comman))
076000071011     c                   Call      'QCMDEXC'                            97
076100071011     c                   Parm                    comman
076200071011     c                   Parm                    lenght
076300170308     c                   open      wfgcs82l
076400050221     C                   ELSE
076500071011      * merce da prelevare
076600050221     C                   SETON                                        03
076700170224     c                   if        d87ord='Z'
076800071011     c                   Movel(p)  cmd4(1)       comman
076900170222     c                   else
077000170222     c                   Movel(p)  cmd8(1)       comman
077100170222     c                   endif
077200071011     c                   eval      lenght = %len(%trim(comman))
077300071011     c                   Call      'QCMDEXC'                            97
077400071011     c                   Parm                    comman
077500071011     c                   Parm                    lenght
077600170224     c                   if        d87ord='Z'
077700170308     c                   open      wfgcs81l
077800170222     c                   else
077900170308     c                   open      wfgcs85l
078000170222     c                   endif
078100170222     c
078200050221     C                   END
078300071011     c   97              except    errore
078400170307
078500170307     C* SOLO SPED IN FERMO DEPOSITO
078600170307     c                   if        *in01 and d87fdep='S'
078700170307     c                   seton                                        06
078800170307     c                   endif
078900170331     c                   if        *in01 and d87MES ='S'
079000170331     c                   seton                                        07
079100170331     c                   endif
079200170331
079300050221      *
079400050221     C* DEFINIZIONE CHIAVI
079500050221     C     KGCp          KLIST
079600050221     C                   KFLD                    FLE
079700050221     C                   KFLD                    FAS               3 0
079800161111RM*  C     KFVV2         KLIST
079900161111RM*  C                   KFLD                    BRVNPG
080000161111RM*  C                   KFLD                    WNFV5
080100161111RM*  C                   KFLD                    BRVFGS
080200161111RM*  C     KARTD         KLIST
080300161111RM*  C                   KFLD                    ARTFLS
080400161111RM*  C                   KFLD                    ARTLNA
080500161111RM*  C                   KFLD                    ARTNRS
080600161111RM*  C                   KFLD                    ARTNSC
080700050221     C*
080800050221     C     KARB          KLIST
080900050412     C                   KFLD                    GCPAASa
081000050412     C                   KFLD                    GCPLNPa
081100050412     C                   KFLD                    GCPNRSa
081200050412     C                   KFLD                    GCPNSPa
081300141215      *
081400141215     C     Kart1         KLIST
081500141216     C                   KFLD                    ARbAAS
081600141216     C                   KFLD                    ARbLNP
081700141216     C                   KFLD                    ARbNRS
081800141216     C                   KFLD                    ARbNSP
081900141216     C     Kbrv          KLIST
082000141219     C                   KFLD                    artFLS
082100141216     C                   KFLD                    artLNA
082200141216     C                   KFLD                    artNRS
082300141216     C                   KFLD                    artNSC
082400071026     C*
082500071026     C     Karbf         KLIST
082600071026     C                   KFLD                    arbAAS
082700071026     C                   KFLD                    arbLNP
082800071026     C                   KFLD                    arbNRS
082900071026     C                   KFLD                    arbNSP
083000071026     C*
083100071026     C     Kblp          KLIST
083200071026     C                   KFLD                    LBLAAN
083300071026     C                   KFLD                    LBLLPN
083400071026     C                   KFLD                    LBLNRN
083500071026     C                   KFLD                    LBLNSN
083600071026      *
083700071015     C     Kart          KLIST
083800071015     C                   KFLD                    GCsAAS
083900071015     C                   KFLD                    GCsLNP
084000071015     C                   KFLD                    GCsNRS
084100071015     C                   KFLD                    GCsNSP
084200050221     C*
084300071026     C     Kblt          KLIST
084400071026     C                   KFLD                    gcsAASf
084500071026     C                   KFLD                    gcsLNPf
084600071026     C                   KFLD                    gcsNRSf
084700071026     C                   KFLD                    gcsNSPf
084800071026     C*
084900071026     C     KAR5          KLIST
085000071026     C                   KFLD                    GCPAASa
085100071026     C                   KFLD                    GCPLNPa
085200071026     C                   KFLD                    GCPNRSa
085300071026     C                   KFLD                    GCPNSPa
085400071026     C                   KFLD                    trec5             3
085500071026     c                   move      'GEN'         trec5
085600050221     C*
085700050221     C     KTAB          KLIST
085800050221     C                   KFLD                    CODUT
085900050221     C                   KFLD                    COD               2
086000050221     C                   KFLD                    KEY               8
086100170331     C     KTAB2         KLIST
086200170331     C                   KFLD                    CODUT
086300170331     C                   KFLD                    COD
086400050221     C*
086500071026     c                   movel     *all'_'       lin198          198
086600170331     C***
086700170331     C* CARICO EVENTUALI ALTRI FILIALI GESTITE DA QUELLA COLLEGATA
086800170331     C***
086900050221     c                   clear                   trul06ds
087000050221     c                   eval      d06cod = '£6'
087100050221     c                   movel     §fle          d06key
087200050221     c                   movel     trul06ds      kpjbu
087300050221     c                   call      'TRUL06R'
087400050221     c                   parm                    kpjba
087500050221     c                   movel     kpjbu         trul06ds
087600050221     c                   movea     lin           fil£6
087700170331     c
087800170331     C* carico ZONE MESSAGGERIE
087900170331    1c                   if        *in01 and d87MES ='S'
088000170331     c                   clear                   xx                4 0
088100170331     c                   clear                   yy                4 0
088200170331     c                   clear                   ZZ                4 0
088300170331     C                   EVAL      ZZ=1
088400170331   1aC                   DOW       FIL£6(zz)>0
088500170331     c
088600170331     c                   movel     fil£6(zz)     alffil            3
088700170331     c                   movel     fil£6(zz)     wpo
088800170331     c                   movel(P)  '03'          cod
088900170331     c                   movel(P)  fil£6(zz)     key
089000170331     c     ktab          setll     tabel
089100170331     c     ktab2         reade     tabel
089200170331    2c                   dow       not %eof(tabel00f) and  wpo=alffil
089300170331    3c                   if        tblflg=' '
089400170331     c                   movel     tblkey        wpo               3
089500170331    4c                   if        wpo=alffil
089600170331     c                   movel     tbluni        ds03
089700170331
089800170331    4c                   if        §03tpm='M'
089900170331     c                   add       1             xx
090000170331     c                   movel     tblkey        w005a             5
090100170331     c                   move      w005a         zone(xx)
090200170331    5c                   endif
090300170331    4c                   endif
090400170331    3c                   endif
090500170331ù    c     ktab2         reade     tabel
090600170331    2c                   enddo
090700170331
090800170331     c                   add       1             zz
090900170331   1ac                   enddo
091000170331    1c                   endif
091100170331    c
091200050221     C                   ENDSR
091300050221     C*****************************************************************
091400050221     Otigcp000  E            AGG0
091500050221     O                       GCPDuma
091600050221     Otigcp000  E            AGG5
091700050221     O                       GCPDLMa
091800050221     OPRTF198   E    03      TESTA          1 02
091900050221     O                       RAGUT             +  0
092000170221     O                                           25 '   '
092100050221     O                                         + 25 '* ELENCO SPEDIZIONI DA '
092200050221     O                                         +  0 'PRELEVARE DAL MAGAZZINO '
092300050221     O                                         +  0 'GIACENZE *'
092400050221     O                       §PGM               127
092500050221     O                       GMAUD8             147 '  /  /    '
092600170308     O                                          192 'PAG:'
092700170308     O                       PAGE          Z    198
092800050221     O          E    01      TESTA          1 02
092900050221     O                       RAGUT             +  0
093000170221     O                                           25 '   '
093100050221     O                                         + 25 '* ELENCO SPEDIZIONI DA '
093200050221     O                                         +  0 'PORTARE NEL MAGAZZINO '
093300050221     O                                         +  0 'GIACENZE *'
093400050221     O                       §PGM               127
093500050221     O                       GMAUD8             147 '  /  /    '
093600170308     O                                          192 'PAG:'
093700170308     O                       PAGE          Z    198
093800050221     O          E            TESTA          1
093900050221     O                       KNSIF             +  0
094000050221     O                       KNMUS             +  0
094100050221     O                       ORA                147 '  :  :  '
094200050221     O          E    03      TESTA          1
094300050221     O                                         +  0 'CON DISPOSIZIONI DI:'
094400050221     O                       DESCR             +  1
094500071026     O          E    03 05   TESTA       1
094600071026     O                                         +  0 '* Prima riga bolla O'
094700071026     O                                         +  0 'riginale '
094800071026     O                                         +  0 'Seconda riga nuova '
094900071026     O                                         +  0 'Bolla *'
095000071026     O          E    03 05   TESTA          0
095100071026     O                                         +  0 '* Prima riga bolla O'
095200071026     O                                         +  0 'riginale '
095300071026     O                                         +  0 'Seconda riga nuova '
095400071026     O                                         +  0 'Bolla *'
095500071026     O          E    03 05   TESTA          0
095600071026     O                                         +  0 '* Prima riga bolla O'
095700071026     O                                         +  0 'riginale '
095800071026     O                                         +  0 'Seconda riga nuova '
095900071026     O                                         +  0 'Bolla *'
096000050221     O          E            TESTAR      1
096100170308     O                  03                     +  0 '---------  RISTAMPA'
096200170308     O                  03                     +  1 '---------  ELENCO DEL'
096300170308     O                  03   GMADRI            +  1 '  /  /    '
096400170307     O                  06                     +  5 '- SOLO SPEDIZIONI IN'
096500170307     O                  06                     +  1 'FERMO DEPOSITO -'
096600170331     O                  07                     +  5 '- SOLO ZONE MESSAGGE'
096700170331     O                  07                     +  0 'RIA -'
096800050221     O          E            TESTAT      2
096900071026     O                                            3 'LNP'
097000161125     O                                            7 'LNA'
097100161125     O                                           13 'ANNO'
097200161125     O                                           17 'SER'
097300161125     O                                           25 'SPEDIZ.'
097400161125     O                                           35 'Mittente'
097500161125     O                                           64 'Destinatario'
097600161125     O               03                          95 'Motivo di chiusura'
097700170306     O                                          109 'FlSgn'
097800170306     O                                          125 'Segnacol.Dal-Al'
097900170306     O                                          131 'Colli'
098000170306     O                                          140 'Peso Kg'
098100170306     O                                          145 'Zona'
098200170308     O               01                         156 'F.Deposito'
098300170308     O               03                         154 'DataEsec'
098400170306     O               03                         161 'MagGIA'
098500161125     o                                          172 'Ubicazione'
098600161125     o                                          195 'Val'
098700161125     o                                          198 'Sp'
098800050221     O          E            RIGA        2
098900071026     O                       GCsLNP               3
099000161125     O                       GCsLNA               7
099100161125     O                       GCsAAS              13
099200161125     O                       GCsNRS              17
099300161125     O                       GCsNSP        Z     25
099400161125     O                       arbRSMs             50
099500170125     O                       arbRSDs             75
099600170306     O                       gcscfgd            103
099700170306     O                       gcsfls        Z    108
099800170306     O                       gcsNCD        Z    117
099900170306     O                                          118 '/'
100000170306     O                       gcsNCA        Z    125
100100170306     O                       gcsNCL        Z    131
100200170306     O                       gcspkf        2    140
100300170306     O                       gcsZNC             144
100400170308     O               01      STPfdep            153
100500170306     O               03      STPDED             154 '  /  /  '
100600170306     O               03      STPsta             159
100700170306     O                       S30_zng            192
100800170306     O                       gcsbva             194
100900070629     O          E            segn1       1
101000070629     O                       sk1(1)        z   + 30
101100070629     O                       sk1(2)        z   +  1
101200070629     O                       sk1(3)        z   +  1
101300070629     O                       sk1(4)        z   +  1
101400070629     O                       sk1(5)        z   +  1
101500070629     O                       sk1(6)        z   +  1
101600070629     O                       sk1(7)        z   +  1
101700070629     O                       sk1(8)        z   +  1
101800070629     O                       sk1(9)        z   +  1
101900070629     O                       sk1(10)       z   +  1
102000070629     O                       sk1(11)       z   +  1
102100070629     O                       sk1(12)       z   +  1
102200070629     O                       sk1(13)       z   +  1
102300070629     O                       sk1(14)       z   +  1
102400070629     O                       sk1(15)       z   +  1
102500070629     O                       sk1(16)       z   +  1
102600070629     O                       sk1(17)       z   +  1
102700070629     O                       sk1(18)       z   +  1
102800070629     O                       sk1(19)       z   +  1
102900070629     O                       sk1(20)       z   +  1
103000070629     O          E            segn2       1
103100070629     O                       sk2(1)        z   + 30
103200070629     O                       sk2(2)        z   +  1
103300070629     O                       sk2(3)        z   +  1
103400070629     O                       sk2(4)        z   +  1
103500070629     O                       sk2(5)        z   +  1
103600070629     O                       sk2(6)        z   +  1
103700070629     O                       sk2(7)        z   +  1
103800070629     O                       sk2(8)        z   +  1
103900070629     O                       sk2(9)        z   +  1
104000070629     O                       sk2(10)       z   +  1
104100070629     O                       sk2(11)       z   +  1
104200070629     O                       sk2(12)       z   +  1
104300070629     O                       sk2(13)       z   +  1
104400070629     O                       sk2(14)       z   +  1
104500070629     O                       sk2(15)       z   +  1
104600070629     O                       sk2(16)       z   +  1
104700070629     O                       sk2(17)       z   +  1
104800070629     O                       sk2(18)       z   +  1
104900070629     O                       sk2(19)       z   +  1
105000070629     O                       sk2(20)       z   +  1
105100070629     O          E            segn3       1
105200070629     O                       sk3(1)        z   + 30
105300070629     O                       sk3(2)        z   +  1
105400070629     O                       sk3(3)        z   +  1
105500070629     O                       sk3(4)        z   +  1
105600070629     O                       sk3(5)        z   +  1
105700070629     O                       sk3(6)        z   +  1
105800070629     O                       sk3(7)        z   +  1
105900070629     O                       sk3(8)        z   +  1
106000070629     O                       sk3(9)        z   +  1
106100070629     O                       sk3(10)       z   +  1
106200070629     O                       sk3(11)       z   +  1
106300070629     O                       sk3(12)       z   +  1
106400070629     O                       sk3(13)       z   +  1
106500070629     O                       sk3(14)       z   +  1
106600070629     O                       sk3(15)       z   +  1
106700070629     O                       sk3(16)       z   +  1
106800070629     O                       sk3(17)       z   +  1
106900070629     O                       sk3(18)       z   +  1
107000070629     O                       sk3(19)       z   +  1
107100070629     O                       sk3(20)       z   +  1
107200070629     O          E            segn4       1
107300070629     O                       sk4(1)        z   + 30
107400070629     O                       sk4(2)        z   +  1
107500070629     O                       sk4(3)        z   +  1
107600070629     O                       sk4(4)        z   +  1
107700070629     O                       sk4(5)        z   +  1
107800070629     O                       sk4(6)        z   +  1
107900070629     O                       sk4(7)        z   +  1
108000070629     O                       sk4(8)        z   +  1
108100070629     O                       sk4(9)        z   +  1
108200070629     O                       sk4(10)       z   +  1
108300070629     O                       sk4(11)       z   +  1
108400070629     O                       sk4(12)       z   +  1
108500070629     O                       sk4(13)       z   +  1
108600070629     O                       sk4(14)       z   +  1
108700070629     O                       sk4(15)       z   +  1
108800070629     O                       sk4(16)       z   +  1
108900070629     O                       sk4(17)       z   +  1
109000070629     O                       sk4(18)       z   +  1
109100070629     O                       sk4(19)       z   +  1
109200070629     O                       sk4(20)       z   +  1
109300070629     O          E            segn5       1
109400070629     O                       sk5(1)        z   + 30
109500070629     O                       sk5(2)        z   +  1
109600070629     O                       sk5(3)        z   +  1
109700070629     O                       sk5(4)        z   +  1
109800070629     O                       sk5(5)        z   +  1
109900070629     O                       sk5(6)        z   +  1
110000070629     O                       sk5(7)        z   +  1
110100070629     O                       sk5(8)        z   +  1
110200070629     O                       sk5(9)        z   +  1
110300070629     O                       sk5(10)       z   +  1
110400070629     O                       sk5(11)       z   +  1
110500070629     O                       sk5(12)       z   +  1
110600070629     O                       sk5(13)       z   +  1
110700070629     O                       sk5(14)       z   +  1
110800070629     O                       sk5(15)       z   +  1
110900070629     O                       sk5(16)       z   +  1
111000070629     O                       sk5(17)       z   +  1
111100070629     O                       sk5(18)       z   +  1
111200070629     O                       sk5(19)       z   +  1
111300070629     O                       sk5(20)       z   +  1
111400071026     O          E            RIGAf       1
111500071026     O                       GCsLNPf              3
111600161125     O                       GCsLNAf              7
111700161125     O                       GCsAASf             13
111800161125     O                       GCsNRSf             17
111900161125     O                       GCsNSPf       Z     25
112000161125     O                       gcsRSMfs            50
112100170125     O                       gcsRSDf             87
112200170306     O                       gcsflsf       z    108
112300170306     O                       gcsNCDf       Z    117
112400170306     O                                          118 '/'
112500170306     O                       gcsNCAf       Z    125
112600161125     O                       spuntato           198
112700071026     O          E            segn1f      1
112800071026     O                       sk1f(1)       z   + 30
112900071026     O                       sk1f(2)       z   +  1
113000071026     O                       sk1f(3)       z   +  1
113100071026     O                       sk1f(4)       z   +  1
113200071026     O                       sk1f(5)       z   +  1
113300071026     O                       sk1f(6)       z   +  1
113400071026     O                       sk1f(7)       z   +  1
113500071026     O                       sk1f(8)       z   +  1
113600071026     O                       sk1f(9)       z   +  1
113700071026     O                       sk1f(10)      z   +  1
113800071026     O                       sk1f(11)      z   +  1
113900071026     O                       sk1f(12)      z   +  1
114000071026     O                       sk1f(13)      z   +  1
114100071026     O                       sk1f(14)      z   +  1
114200071026     O                       sk1f(15)      z   +  1
114300071026     O                       sk1f(16)      z   +  1
114400071026     O                       sk1f(17)      z   +  1
114500071026     O                       sk1f(18)      z   +  1
114600071026     O                       sk1f(19)      z   +  1
114700071026     O                       sk1f(20)      z   +  1
114800071026     O          E            segn2f      1
114900071026     O                       sk2f(1)       z   + 30
115000071026     O                       sk2f(2)       z   +  1
115100071026     O                       sk2f(3)       z   +  1
115200071026     O                       sk2f(4)       z   +  1
115300071026     O                       sk2f(5)       z   +  1
115400071026     O                       sk2f(6)       z   +  1
115500071026     O                       sk2f(7)       z   +  1
115600071026     O                       sk2f(8)       z   +  1
115700071026     O                       sk2f(9)       z   +  1
115800071026     O                       sk2f(10)      z   +  1
115900071026     O                       sk2f(11)      z   +  1
116000071026     O                       sk2f(12)      z   +  1
116100071026     O                       sk2f(13)      z   +  1
116200071026     O                       sk2f(14)      z   +  1
116300071026     O                       sk2f(15)      z   +  1
116400071026     O                       sk2f(16)      z   +  1
116500071026     O                       sk2f(17)      z   +  1
116600071026     O                       sk2f(18)      z   +  1
116700071026     O                       sk2f(19)      z   +  1
116800071026     O                       sk2f(20)      z   +  1
116900071026     O          E            segn3f      1
117000071026     O                       sk3f(1)       z   + 30
117100071026     O                       sk3f(2)       z   +  1
117200071026     O                       sk3f(3)       z   +  1
117300071026     O                       sk3f(4)       z   +  1
117400071026     O                       sk3f(5)       z   +  1
117500071026     O                       sk3f(6)       z   +  1
117600071026     O                       sk3f(7)       z   +  1
117700071026     O                       sk3f(8)       z   +  1
117800071026     O                       sk3f(9)       z   +  1
117900071026     O                       sk3f(10)      z   +  1
118000071026     O                       sk3f(11)      z   +  1
118100071026     O                       sk3f(12)      z   +  1
118200071026     O                       sk3f(13)      z   +  1
118300071026     O                       sk3f(14)      z   +  1
118400071026     O                       sk3f(15)      z   +  1
118500071026     O                       sk3f(16)      z   +  1
118600071026     O                       sk3f(17)      z   +  1
118700071026     O                       sk3f(18)      z   +  1
118800071026     O                       sk3f(19)      z   +  1
118900071026     O                       sk3f(20)      z   +  1
119000071026     O          E            segn4f      1
119100071026     O                       sk4f(1)       z   + 30
119200071026     O                       sk4f(2)       z   +  1
119300071026     O                       sk4f(3)       z   +  1
119400071026     O                       sk4f(4)       z   +  1
119500071026     O                       sk4f(5)       z   +  1
119600071026     O                       sk4f(6)       z   +  1
119700071026     O                       sk4f(7)       z   +  1
119800071026     O                       sk4f(8)       z   +  1
119900071026     O                       sk4f(9)       z   +  1
120000071026     O                       sk4f(10)      z   +  1
120100071026     O                       sk4f(11)      z   +  1
120200071026     O                       sk4f(12)      z   +  1
120300071026     O                       sk4f(13)      z   +  1
120400071026     O                       sk4f(14)      z   +  1
120500071026     O                       sk4f(15)      z   +  1
120600071026     O                       sk4f(16)      z   +  1
120700071026     O                       sk4f(17)      z   +  1
120800071026     O                       sk4f(18)      z   +  1
120900071026     O                       sk4f(19)      z   +  1
121000071026     O                       sk4f(20)      z   +  1
121100071026     O          E            segn5f      1
121200071026     O                       sk5f(1)       z   + 30
121300071026     O                       sk5f(2)       z   +  1
121400071026     O                       sk5f(3)       z   +  1
121500071026     O                       sk5f(4)       z   +  1
121600071026     O                       sk5f(5)       z   +  1
121700071026     O                       sk5f(6)       z   +  1
121800071026     O                       sk5f(7)       z   +  1
121900071026     O                       sk5f(8)       z   +  1
122000071026     O                       sk5f(9)       z   +  1
122100071026     O                       sk5f(10)      z   +  1
122200071026     O                       sk5f(11)      z   +  1
122300071026     O                       sk5f(12)      z   +  1
122400071026     O                       sk5f(13)      z   +  1
122500071026     O                       sk5f(14)      z   +  1
122600071026     O                       sk5f(15)      z   +  1
122700071026     O                       sk5f(16)      z   +  1
122800071026     O                       sk5f(17)      z   +  1
122900071026     O                       sk5f(18)      z   +  1
123000071026     O                       sk5f(19)      z   +  1
123100071026     O                       sk5f(20)      z   +  1
123200071026     O          E            separa      1
123300071026     o                       lin198             198
123400071026     O          E            errRIGAf    1
123500071026     O                                         + 25 '* Attenzione non trovata '
123600071026     O                                         +  0 'spedizione figlia verifi'
123700071026     O                                         +  0 'care i dati della bolla *'
123800071011     O          E            errore         1 02
123900071011     O                       RAGUT             +  0
124000170221     O                                           25 '   '
124100071011     O                                         + 25 '* ELENCO SPEDIZIONI DA '
124200071011     O                                         +  0 'PORTARE NEL MAGAZZINO '
124300071011     O                                         +  0 'GIACENZE *'
124400071011     O                       §PGM               127
124500071011     O                       GMAUD8             147 '  /  /    '
124600071011     O                                         + 10 'PAG:'
124700071011     O                       PAGE          Z   +  1
124800071011     O          E            errore      2
124900071011     O                                         + 25 '* Attenzione errore in '
125000071011     O                                         +  0 'esecuzione programma  '
125100071011     O                                         +  0 'avvisare il CED *'
125200071011      *_________________________________________________________________________
125300071011**  CMD1
125400170308DLTF FILE(QTEMP/WFGCS80F)
125500071011**  CMD2
125600170308CRTDUPOBJ OBJ(WFGCS80F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
125700071011**  CMD3
125800170308DLTF FILE(QTEMP/WFGCS81L)
125900071011**  CMD4
126000170308CRTDUPOBJ OBJ(WFGCS81L) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
126100071011**  CMD5
126200170308DLTF FILE(QTEMP/WFGCS82L)
126300071011**  CMD6
126400170308CRTDUPOBJ OBJ(WFGCS82L) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
126500170222**  CMD7
126600170308DLTF FILE(QTEMP/WFGCS85L)
126700170222**  CMD8
126800170308CRTDUPOBJ OBJ(WFGCS85L) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)

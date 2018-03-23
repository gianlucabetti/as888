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
005100170512     D TRULTASDS     E DS
005200050221     D CNCR80        E DS
005300050221     D KPJBA         E DS
005400161111     D DS7B          E DS
005500170331     D DS03          E DS
005600050221     d trul06ds      e ds
005700050221     d  lin                    1     90  0 dim(30)
005800050221     D FNLG87        E DS                  EXTNAME(FNLG87DS)
005900161111     D* REPERISCE DATI FOGLI
006000161111     D DSLV53        E DS                  EXTNAME(FNLV53DS)
006100050221      *
006200050221     D                SDS
006300050221     D  §PGM                   1     10
006400071011     D cmd1            S             66    DIM(01) CTDATA PERRCD(1)
006500071011     D cmd2            S             66    DIM(01) CTDATA PERRCD(1)
006600071011     D cmd3            S             66    DIM(01) CTDATA PERRCD(1)
006700071011     D cmd4            S             66    DIM(01) CTDATA PERRCD(1)
006800071011     D cmd5            S             66    DIM(01) CTDATA PERRCD(1)
006900071011     D cmd6            S             66    DIM(01) CTDATA PERRCD(1)
007000170222     D cmd7            S             66    DIM(01) CTDATA PERRCD(1)
007100170222     D cmd8            S             66    DIM(01) CTDATA PERRCD(1)
007200161111     D WLBDAT          DS
007300161111     D  G02DAT                 1      8  0
007400161111     D  G02INV                 9     16  0
007500161111     D  G02ERR                17     17
007600161111     D  G02TGI                18     22  0
007700071011     d comman          s             80
007800071011     d lenght          s             15  5 inz(80)
007900161111     d wdfv            S                   LIKE(d53dfv)
008000161111     d WSPG            S                   LIKE(FVVSPG)
008100161111     d Wnfv5           S                   LIKE(FVVnfv)
008200170331     d wok             s              1
008300070629      *----------------------------------------------------------------
008400050221     C                   DO        30            I                 2 0
008500050221     c                   if        fil£6(i) = 0
008600050221     c                   leave
008700050221     c                   endif
008800050221     C                   Z-ADD     fil£6(i)      FLE               3 0
008900050221     C*
009000050221     c   01              z-add     10            fas
009100050221     c   03              z-add     35            fas
009200050221      * lettura giacenze
009300050221      *
009400050418     c     kgcp          setll     tigcp26l
009500000607     c                   do        *hival
009600050418     c     fle           reade     tigcp26l                             90
009700050413      *allocazione
009800050413     c                   if        *in90
009900050418     c     fle           reade(n)  tigcp26l
010000050413     c                   if        %eof
010100050413     c                   leave
010200050413     c                   end
010300050413     c                   iter
010400050413     c                   endif
010500050413      *
010600000607     c                   if        %eof
010700000607     c                   leave
010800000607     c                   end
010900050221      * se richiesto materiale da portare a magazzino la fase non può essere
011000050221      * maggiore di 26
011100050418     c                   if        gcpfas > 30 and *in01
011200050221     c                   leave
011300040507     c                   end
011400050221
011500050408      *solo progressivo 0
011600050408     c                   if        gcpfrg <> 0
011700050408     c                   iter
011800050408     c                   end
011900161007     c*
012000161007     c                   clear                   fnlgi1ds
012100161007     c                   eval      ilgi1agc=gcpagc
012200161007     c                   eval      ilgi1fgc=gcpfgc
012300161007     c                   eval      ilgi1ngc=gcpngc
012400161007     c                   movel     fnlgi1ds      kpjbu
012500161007     c                   call      'FNLGI1R'
012600161007     c                   parm                    kpjba
012700161007     c                   parm                    tigcp00ds
012800161007     c                   movel     kpjbu         fnlgi1ds
012900161007
013000050411      * in uscita dal magazzino
013100050221--2)>C     GCPFAS        IFEQ      35
013200050221     C     GCPDuma       ANDEQ     0
013300050221     C     D87RIS        ANDEQ     *BLANK
013400050221     C     *IN03         ANDEQ     '1'
013500161007     c     olgi1outmg    andne     ' '
013600050221      *
013700050221     C     GCPFAS        OREQ      35
013800050221     C     GCPDuma       ANDEQ     D87DAT
013900050221     C     D87RIS        ANDEQ     'R'
014000050221     C     *IN03         ANDEQ     '1'
014100161007     c     olgi1outmg    andne     ' '
014200050221      *
014300050411      * in uscita dal magazzino x resi
014400141110---->C*mm  GCPFAS        OReq      50
014500141107---->C     GCPFAS        ORge      50
014600050411---->C     GCPdis        andeq     'R'
014700050411     C     GCPDuma       ANDEQ     0
014800050411     C     D87RIS        ANDEQ     *BLANK
014900050411     C     *IN03         ANDEQ     '1'
015000161007     c     olgi1outmg    andne     ' '
015100050221      *
015200050411---->C     GCPdis        oreq      'R'
015300050411     C     GCPDuma       ANDEQ     D87DAT
015400050411     C     D87RIS        ANDEQ     'R'
015500050411     C     *IN03         ANDEQ     '1'
015600161007     c     olgi1outmg    andne     ' '
015700161007      *
015800050411      * in uscita dal magazzino x dirottamenti
015900141110---->C*mm  GCPFAS        OReq      40
016000141107---->C     GCPFAS        ORge      40
016100050411---->C     GCPdis        andeq     'D'
016200050411     C     GCPDuma       ANDEQ     0
016300050411     C     D87RIS        ANDEQ     *BLANK
016400050411     C     *IN03         ANDEQ     '1'
016500161007     c     olgi1outmg    andne     ' '
016600050411      *
016700141110---->C*mm  GCPFAS        OReq      50
016800141110---->C*mm  GCPdis        andeq     'D'
016900141110     C*mm  GCPDuma       ANDEQ     0
017000141110     C*mm  D87RIS        ANDEQ     *BLANK
017100141110     C*mm  *IN03         ANDEQ     '1'
017200050420      *
017300050411---->C     GCPdis        oreq      'D'
017400050411     C     GCPDuma       ANDEQ     D87DAT
017500050411     C     D87RIS        ANDEQ     'R'
017600050411     C     *IN03         ANDEQ     '1'
017700161007     c     olgi1outmg    andne     ' '
017800050411
017900050411      *merce da portare in magazzino
018000050413---->C     GCPFAS        ORle      30
018100050221     C     D87RIS        ANDEQ     *BLANK
018200050221     C     *IN01         ANDEQ     '1'
018300161007     c     olgi1amg      andeq     'S'
018400161007     C**   GCPDLMa       ANDEQ     0
018500050221      *
018600050221     C     GCPFAS        ORle      70
018700050221     C     GCPDLMa       ANDEQ     D87DAT
018800050221     C     D87RIS        ANDEQ     'R'
018900050221     C     *IN01         ANDEQ     '1'
019000050221      *
019100050221     C* PRELEVO DATI BOLLA
019200141119     C     KARB          CHAIN     FNARB01L
019300170307    1c                   if        %found(fnarb01l)
019400050221     c                   clear                   dar5gen
019500050221     C     KAR5          CHAIN     FiAR501L
019600170307    2c                   if        %found(fiar501l)
019700050221     c                   movel     ar5uni        dar5gen
019800170307    2c                   end
019900141215     c                   clear                   znc               2 0
020000141216     C     Kart1         chain     Fnart01L
020100170307    2c                   if        %found(fnart01l)
020200141216     C     Kbrv          setll     Fnbrv07L
020300170307    3c                   do        *hival
020400141216     C     Kbrv          reade     Fnbrv07L
020500170307    4c                   if        %eof(fnbrv07l)
020600141215     c                   leave
020700170307    4c                   endif
020800170307    4c                   if        brvdcs = 0 or brvnpg = 8
020900141215     c                   iter
021000170307   X4c                   else
021100141215     c                   move      brvznc        znc
021200141215     c                   leave
021300170307    4c                   endif
021400170307    3c                   enddo
021500170307    2c                   endif
021600071026      * solo per merce da prelevare dal magazzino *IN03=on
021700071026      * se dirottamento o reso reperisce riferimenti figlia da stampare
021800170307    2c                   if        *in03
021900080220     c                   seton                                        31
022000170307    3c                   if        gcpdis = 'D' or gcpdis = 'R'
022100071026     C     Karb          chain     fnlbl02l                           31
022200071026     C  n31Kblp          chain     fnblp01l                           31
022300170307    3c                   endif
022400170307    2c                   endif
022500170307     c
022600170307     c* solo per merce da portare a mag giacenze posso parzializzare
022700170307     c*  per fermo deposito
022800170331     c                   eval      wok='S'
022900170331     c                   if        *in01 and *in06 and arbffd<>d87fdep
023000170331     c                   eval      wok='N'
023100170331     c                   endif
023200170331
023300170331     c* OPPURE per zone messaggeria
023400170331     c                   if        *in01 and *in07
023500170331     C                   IF        NOT *IN06  OR  WOK='N'
023600170331     c                   movel     arblna        alfzona           5
023700170331     c                   move      arbznc        alfzona
023800170331     c                   if        %lookup(alfzona:zone)=0
023900170331     c                   eval      wok='N'
024000170331     C                   ELSE
024100170331     c                   eval      wok='S'
024200170331     c                   endif
024300170331     c                   endif
024400170331     c                   endif
024500170307
024600071011      *scrittura wf con bolle da stampare successivamente
024700170331    2c*****              if        *in03 or d87fdep=' ' or arbffd=d87fdep
024800170331
024900170331    2c                   if        *in03 or (*in01 and wok='S')
025000071011     c                   exsr      scriviwf
025100170331     c
025200050221     c
025300071026     C* AGGIORNO DATA DI STAMPA
025400170307    3C     D87RIS        IFEQ      *BLANK
025500170512     c                   if        *in01
025600170512     C                   MOVE      AMGUD8        GCPDLMa
025700170512     C                   EXCEPT    AGG5
025800170512
025900170512     c* richiamo pgm per attribuire competenze
026000170512     c                   clear                   trultasds
026100170512     c                   eval      TTASAAS=arbaas
026200170512     c                   eval      TTASlnp=arblnp
026300170512     c                   eval      TTASnrs=arbnrs
026400170512     c                   eval      TTASnsp=arbnsp
026500170512     c                   movel     trulTASds     KPJBU
026600170512     c                   call      'TRULTASR'
026700170512     c                   parm                    kpjba
026800170512
026900170512     c                   endif
027000170512     c
027100071011     C   03              MOVE      AMGUD8        GCPDuma
027200071011     C   03              EXCEPT    AGG0
027300170307    4c                   if        gescomm = 'S'
027400071011     c                   commit
027500170307    4c                   endif
027600170307    3c                   endif
027700170307    2c                   endif
027800071011
027900170307    1C                   END
028000071011     C*
028100170307    0C                   END
028200071011     C*
028300071011     c                   enddo
028400071011
028500071011     c                   enddo
028600071011      * eliminazione file di work
028700170308     c                   close     wfgcs80f
028800071011      * rilegge wf e produce il tabulato
028900071011     c                   exsr      stampa
029000071011      *
029100170302     c                   if        *in03  and d87ord='Z'
029200170308     c   03              close     wfgcs81l
029300170302     c                   else
029400170308     c                   close     wfgcs85l
029500170302     c                   endif
029600170308     c   01              close     wfgcs82l
029700170224     c                   if        *in03  and d87ord='Z'
029800170222     c                   Movel(p)  cmd3(1)       comman
029900170222     c                   endif
030000170222     c                   if        *in03  and d87ord='L'
030100170222     c                   Movel(p)  cmd7(1)       comman
030200170222     c                   endif
030300071011     c   01              Movel(p)  cmd5(1)       comman
030400071011     c                   eval      lenght = %len(%trim(comman))
030500071011     c                   Call      'QCMDEXC'                            97
030600071011     c                   Parm                    comman
030700071011     c                   Parm                    lenght
030800170302     c
030900170302     c* Cancellazione file fisico da QTEMP
031000170302     c                   Movel(p)  cmd1(1)       comman
031100170302     c                   eval      lenght = %len(%trim(comman))
031200170302     c                   Call      'QCMDEXC'                            97
031300170302     c                   Parm                    comman
031400170302     c                   Parm                    lenght
031500161007
031600161007     c                   clear                   fnlgi1ds
031700161007     c                   eval      ILGI1TLA= 'C'
031800161007     c                   movel     fnlgi1ds      kpjbu
031900161007     c                   call      'FNLGI1R'
032000161007     c                   parm                    kpjba
032100161111     c
032200161111     C                   CLEAR                   DSLV53
032300161111     C                   MOVEL     'C'           D53TLA
032400161111     C                   CALL      'FNLV53R'
032500161111     C                   PARM                    DSLV53
032600161111     C*
032700071011     c                   seton                                        lr
032800071011      *_________________________________________________________
032900071011     c     stampa        begsr
033000071011      *_________________________________________________________
033100071011      * legge la v.l. opportuna x l'ordinamento a seconda che sia
033200071011      * merce da prelevare o da portare a magazzino
033300170224     c   03              if        d87ord='Z'
033400170308     c     *loval        setll     wfgcs81l
033500170222     c                   endif
033600170222     c   03              if        d87ord='L'
033700170308     c     *loval        setll     wfgcs85l
033800170222     c                   endif
033900170308     c   01*loval        setll     wfgcs82l
034000170222
034100071011     c                   do        *hival
034200170222
034300170224     c   03              if        d87ord='Z'
034400170308     c                   read      wfgcs81l                               98
034500170222     c                   endif
034600170222     c   03              if        d87ord='L'
034700170308     c                   read      wfgcs85l                               98
034800170222     c                   endif
034900170308     c   01              read      wfgcs82l                               98
035000170222
035100071011     c   98              leave
035200071011      * segnacolli non sequenziali prelevo i numeri da dettaglio colli
035300070629     c                   clear                   skt
035400071011     c                   if        gcsfns = 'S'
035500071015     C     Kart          setll     Fnart01L
035600070629     c                   z-add     0             ncl               3 0
035700071011     c                   do        *hival
035800071015     C     Kart          reade     Fnart01L
035900070629     c                   if        %eof(fnart01l) or ncl = 100
036000070629     c                   leave
036100070629     c                   endif
036200070629     c                   add       1             ncl
036300070629     c                   z-add     artnsc        skt(ncl)
036400070629     c                   enddo
036500070629     c                   endif
036600070629     c
036700071026      * bolla figlia
036800071026     c                   clear                   sktf
036900140728     c                   clear                   spuntato          8
037000140728     c                   clear                   spu               3 0
037100140728     c                   clear                   nospu             3 0
037200170306     c                   clear                   stpsta            2
037300170306
037400140718     c                   if        *in03
037500071026     C     Kblt          setll     Fnblt01L
037600071026     c                   z-add     0             ncl               3 0
037700071026     c                   do        *hival
037800071026     C     Kblt          reade     Fnblt01L
037900071026     c                   if        %eof(fnblt01l) or ncl = 100
038000140728     c                   select
038100140728     c                   when      spu >= 1 and nospu = 0
038200140728     c                   eval      spuntato = 'Totale'
038300140728     c                   when      spu >= 1 and nospu >= 1
038400140728     c                   eval      spuntato = 'Parziale'
038500140728     c                   other
038600140728     c                   clear                   spuntato
038700140728     c                   endsl
038800071026     c                   leave
038900071026     c                   endif
039000140718     c* se presente la data entrata vuol dire
039100140718     c* che è già stato etichettato il pacco con la nuova
039200140718     c* etichetta della figlia
039300140728     c                   if        bltdse <> 0
039400140728     c                   add       1             spu
039500140728     c                   else
039600140728     c                   add       1             nospu
039700140728     c                   end
039800140718     c*
039900140718     c                   if        gcsfnsf = 'S'
040000071026     c                   add       1             ncl
040100071026     c                   z-add     bltnsc        sktf(ncl)
040200071026     c                   endif
040300140718     c                   enddo
040400140718     c                   endif
040500071026     c
040600050221      *
040700071011     C   03GCsDIS        IFNE      SAVDIS
040800050221     C* PRELEVO DESCRIZIONE TIPO DISPOSIZIONE
040900050221     C                   MOVEL     '2D'          COD
041000050221     C                   MOVEL     *BLANKS       KEY
041100071011     C                   MOVEL     GCsDIS        KEY
041200050221     C     KTAB          CHAIN     TABEL                              50
041300050221     C  N50              MOVEL     TBLUNI        DESCR            20
041400050221     C   50              MOVEL     *ALL'?'       DESCR
041500071026     c                   setoff                                       05
041600071026     c                   if        gcsdis  = 'D' or gcsdis = 'R'
041700071026     c                   seton                                        05
041800071026     c                   endif
041900050221     C                   EXCEPT    TESTA
042000050221     C     D87RIS        IFEQ      'R'
042100170308     c     *in06         oreq      *on
042200170331     c     *in07         oreq      *on
042300050221     C                   EXCEPT    TESTAR
042400050221     C                   END
042500050221     C                   EXCEPT    TESTAT
042600050221     C                   SETON                                        99
042700071011     C                   MOVE      GCsDIS        SAVDIS            1
042800071011     C                   END
042900050221      *
043000050221     C  N99
043100050221     COR OF              DO
043200050221     C                   EXCEPT    TESTA
043300050221     C     D87RIS        IFEQ      'R'
043400170308     c     *in06         oreq      *on
043500170331     c     *in07         oreq      *on
043600050221     C                   EXCEPT    TESTAR
043700050221     C                   END
043800050221     C                   EXCEPT    TESTAT
043900050221     C                   END
044000050221     C  N99              SETON                                        99
044100050221      *
044200140718     c                   movel     gcsrsm        arbrsms          23
044300140718     c                   movel     gcsrsd        arbrsds          23
044400170222     c***                movel     gcszng        s30_zng          30
044500170222
044600170222     c                   clear                   s30_zng          30
044700170222     c                   if        gcszng<>*blanks
044800170222     c                   eval      s30_zng=%trim(gcszng)+'-'+gcsultsp
044900170222     c                   else
045000170222     c                   eval      s30_zng=gcsultsp
045100170222     c                   endif
045200170222     c
045300050221     C     *IN03         IFEQ      '1'
045400071011     C                   Z-ADD     GCsdeda       G08INV
045500050221     C                   MOVE      *ZEROS        G08DAT
045600050221     C                   MOVEL     '3'           G08ERR
045700050221     C                   CALL      'XSRDA8'
045800050221     C                   PARM                    WLBDA8
045900170306     C                   move      G08DAT        w0020             2 0
046000170306     C                   movel     g08dat        STPDED            6 0
046100170306     C                   move      w0020         STPDED
046200170308     c*
046300170306     c
046400070522     C                   MOVEL     '2C'          COD
046500070522     C                   MOVEL     *BLANKS       KEY
046600071011     C                   MOVEL     GCscfg        KEY
046700070522     C     KTAB          CHAIN     TABEL                              50
046800170306     C  N50              MOVEL     TBLUNI        gcscfgd          26
046900071011     C   50              MOVEL     *ALL'?'       gcscfgd
047000170306     c* Vedo se è già a mag gaicenze oppure no
047100170306     c                   if        gcsdlma=0
047200170306     c                   eval      stpsta='NO'
047300170306     c                   else
047400170306     c                   eval      stpsta='SI'
047500170306     c                   endif
047600170306     C                   END
047700170308     c
047800170308     c                   clear                   stpfdep           2
047900170308     c                   if        *in01  and gcsffd='S'
048000170308     c                   eval      stpfdep='SI'
048100170308     c                   endif
048200170306
048300050221     C                   EXCEPT    RIGA
048400070629      * stampa dettaglio dei segnacolli se non sequenziali
048500070629     c                   if        skt(1) > 0
048600070629     c                   except    segn1
048700070629     c                   endif
048800070629     c                   if        skt(21) > 0
048900070629     c                   except    segn2
049000070629     c                   endif
049100070629     c                   if        skt(41) > 0
049200070629     c                   except    segn3
049300070629     c                   endif
049400070629     c                   if        skt(61) > 0
049500070629     c                   except    segn4
049600070629     c                   endif
049700070629     c                   if        skt(81) > 0
049800070629     c                   except    segn5
049900070629     c                   endif
050000071026      * solo per merce da prelevare dal magazzino *IN03=ON
050100071026      * seconda riga dettaglio bolla figlia
050200071026      * testa se il campo spedizione è pieno per capire se è stato
050300071026      * agganciato il legame altrimenti stampa anomalia
050400071026     c   03              if        gcsnspf > 0
050500140718     c                   movel     gcsRSMf       gcsRSMfs         23
050600071026     C                   EXCEPT    RIGAf
050700071026      * stampa dettaglio dei segnacolli se non sequenziali
050800071026     c                   if        sktf(1) > 0
050900071026     c                   except    segn1f
051000071026     c                   endif
051100071026     c                   if        sktf(21) > 0
051200071026     c                   except    segn2f
051300071026     c                   endif
051400071026     c                   if        sktf(41) > 0
051500071026     c                   except    segn3f
051600071026     c                   endif
051700071026     c                   if        sktf(61) > 0
051800071026     c                   except    segn4f
051900071026     c                   endif
052000071026     c                   if        sktf(81) > 0
052100071026     c                   except    segn5f
052200071026     c                   endif
052300071026      * separazione
052400071026     C                   EXCEPT    separa
052500071026     c                   else
052600071026     c                   if        gcsdis = 'D' or gcsdis = 'R'
052700071026     C                   EXCEPT    errRIGAf
052800071026     c                   endif
052900071026     c                   endif
053000071011      *
053100071011     c                   enddo
053200071011     c                   endsr
053300071011      *_________________________________________________________
053400071011     c     scriviwf      begsr
053500071011      *_________________________________________________________
053600170308     c                   clear                   wfgcs080
053700071011     c                   z-add     arbaas        GCSaas
053800071011     c                   z-add     arblnp        GCSlnp
053900071011     c                   z-add     arbnrs        GCSnrs
054000071011     c                   z-add     arbnsp        GCSnsp
054100071011     c                   z-add     arblna        GCSlna
054200170308     c                   move      arbffd        GCSffd
054300071011     c                   move      arbrsm        GCSrsm
054400071011     c                   move      arbrsd        GCSrsd
054500141215     c                   if        arbznc <> znc and znc <> 0
054600141215     c                   z-add     znc           gcsznc
054700141215     c                   else
054800071011     c                   z-add     arbznc        GCSznc
054900141215     c                   endif
055000071011     c                   z-add     gcpfgc        GCSfgc
055100071011     c                   z-add     gcpngc        GCSngc
055200071011     c                   z-add     gcpdeda       GCSdeda
055300170306     c                   z-add     gcpdlma       GCSdlma
055400071011     c                   move      gcpdis        GCSdis
055500071011     c                   move      gcpcfg        GCScfg
055600170126     c
055700170126     c* se non è stato portato in mag giacenza, non stampo
055800170126     c*  l'ubicazine, altrimenti si
055900170307     c   03              if        gcpdlma>0
056000071011     c                   movel     §ar5zng       GCSzng
056100170126     c                   endif
056200170307     c  n03              movel     §ar5zng       GCSzng
056300161111     c* se la bolla ha la data merce a mag giacenze, significa che
056400161111     c*  probabilmente non e' stata portata in giacenza
056500161111     c*  quindi al posto della ubicazione stampo la data e ora ultima spunta
056600161111     c*
056700170126     c**                 if        gcpdlma=0
056800161111     C     Kart1         chain     Fnart01L
056900161111     c                   if        %found(fnart01l)
057000161111     c                   exsr      Ricspu
057100161111     c                   endif
057200170126     c***                endif
057300161111     c
057400071011     c                   move      §ar5bva       GCSbva
057500071026     c                   z-add     arbfls        GCSfls
057600071011     c                   z-add     arbncd        GCSncd
057700071011     c                   z-add     arbnca        GCSnca
057800071011     c                   move      arbfns        GCSfns
057900071011     c                   z-add     arbncl        GCSncl
058000071011     c                   z-add     arbpkf        GCSpkf
058100071011     c                   z-add     arbvlf        GCSvlf
058200071026      * solo per merce da prelevare da magazzino *IN03=on
058300071026      *figlia se non trova legame o blp non scrive nulla
058400071026     c   03              if        not *in31
058500071026     c                   z-add     blpaas        GCSaasf
058600071026     c                   z-add     blplnp        GCSlnpf
058700071026     c                   z-add     blpnrs        GCSnrsf
058800071026     c                   z-add     blpnsp        GCSnspf
058900071026     c                   z-add     blplna        GCSlnaf
059000071026     c                   move      blprsm        GCSrsmf
059100071026     c                   move      blprsd        GCSrsdf
059200071026     c                   z-add     blpznc        GCSzncf
059300071026     c                   z-add     blpfls        GCSflsf
059400071026     c                   z-add     blpncd        GCSncdf
059500071026     c                   z-add     blpnca        GCSncaf
059600071026     c                   move      blpfns        GCSfnsf
059700071026     c                   z-add     blpncl        GCSnclf
059800071026     c                   z-add     blppkf        GCSpkff
059900071026     c                   z-add     blpvlf        GCSvlff
060000071026     c                   endif
060100170308     c                   write     wfgcs080
060200071011     c                   endsr
060300161111     C**************************************************************************
060400161111     C* cerco spunta del collo in qualsiasi categoria
060500161111     C**************************************************************************
060600161111     C     RICSPU        BEGSR
060700161111     C                   CLEAR                   Wimm
060800161111     C                   CLEAR                   WSAVB
060900161111     C                   CLEAR                   WDFV
061000161111     C                   CLEAR                   WTROV
061100161111     C                   CLEAR                   WSPG
061200161111     C                   CLEAR                   stadsf           10
061300161111RM*  C     kbrv          CHAIN     FNBRV07l                           33
061400161111RM*  C*
061500161111RM* 1C     *IN33         DOWEQ     *OFF
061600161111     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
061700161111     c                   clear                   dslv53
061800161111     C                   z-add     brvnFV        D53NFV
061900161111     C                   MOVEL     brvNPG        D53NPG
062000161111     C                   MOVEL     brvFGS        D53FGS
062100161111     C****               MOVEL     brvfle        D53flf
062200161111     C                   CALL      'FNLV53R'
062300161111     C                   PARM                    DSLV53
062400161111     C* ERRORE
062500161111    2C     D53ERR        IFeq      ' '
062600161111     c                   movel     brvdfs        brvimm           14 0
062700161111     c                   move      brvhms        brvimm
062800161111    3c                   select
062900161111     c* Se maggiore memorizzo
063000161111     c                   when      d53dfv>wdfv
063100161111     C                   MOVEL     RECBRV        WSAVB
063200161111     C                   MOVEL     brvimm        Wimm             14 0
063300161111     C                   MOVEL     d53DFV        WDFV
063400161111     C                   MOVEL     d53spg        WSPG
063500161111     C                   MOVEL     'S'           WTROV             1
063600161111     c
063700161111     c* Se uguale tengo la data ora immissione + alta
063800161111     c                   when      d53dfv=wdfv  and brvimm>wimm
063900161111     C                   MOVEL     brvimm        Wimm             14 0
064000161111     C                   MOVEL     RECBRV        WSAVB
064100161111     C                   MOVEL     d53DFV        WDFV
064200161111     C                   MOVEL     d53spg        WSPG
064300161111     C                   MOVEL     'S'           WTROV             1
064400161111     C                   eval      wimm=brvimm
064500161111     c* Se stessa data e ora immissione, tengo cat 3 o 4
064600161111     c                   when      d53dfv=wdfv and brvimm=wimm and brvnpg<>2
064700161111     C                   MOVEL     brvimm        Wimm             14 0
064800161111     C                   MOVEL     RECBRV        WSAVB
064900161111     C                   MOVEL     d53DFV        WDFV
065000161111     C                   MOVEL     d53spg        WSPG
065100161111     C                   MOVEL     'S'           WTROV             1
065200161111     C                   eval      wimm=brvimm
065300161111    3c                   endsl
065400161111    2c                   endif
065500161111     c
065600161111RM*  C     Kartd         reade     FNBRV07l                               33
065700161111    1c                   enddo
065800161111     c
065900161111     C     WTROV         IFEQ      'S'
066000161111     C                   MOVEL     WSAVB         RECBRV
066100161111     c
066200161111     c* Memorizzo nel campo ubicazione
066300161111     C* DECODIFICO LA CATEGORIA SPUNTA
066400161111    1C     BRVNPG        IFEQ      3
066500161111     C                   MOVEL     '7B'          COD
066600161111     C                   MOVEL(P)  wspg          KEY
066700161111     C     KTAB          CHAIN     TABEL                              32
066800161111     C  N32              MOVEL     TBLUNI        ds7b
066900161111     C   32              CLEAR                   ds7b
067000161111     C                   MOVEL     §7bdesb       ST2CAT            3
067100161111   X1C                   ELSE
067200161111     C                   MOVEL     '7N'          COD
067300161111     C                   MOVEL(P)  BRVNPG        KEY
067400161111     C     KTAB          CHAIN     TABEL                              32
067500161111     C  N32              MOVEL     TBLUNI        ST2CAT
067600161111     C   32              CLEAR                   ST2CAT
067700161111    1C                   ENDIF
067800161111     C* SE CAT 4 AGGANCIO FNFVV PER AVERE DECODIFICA DEL PADRONCINO
067900161111    1C                   IF        BRVNPG=4
068000161111     C                   Z-ADD     BRVNFV        WNFV5
068100161111     C     KFVV2         CHAIN(N)  FNFVV000                           32
068200161111     C  N32              eval      stadsf=fvvdsf
068300161111    1C                   ENDIF
068400161111     C* GIRO LA DATA
068500161111     C                   MOVE      WDFV          G02INV
068600161111     C                   MOVE      *ZEROS        G02DAT
068700161111     C                   MOVEL     3             G02ERR
068800161111     C                   CALL      'XSRDA8'
068900161111     C                   PARM                    WLBDAT
069000161111     C                   MOVEL     G02DAT        ST2DFV            4 0
069100161111     c* ora
069200161111     c                   clear                   st2hms            4 0
069300161111     c                   eval      st2hms=brvhms/100
069400161111     c
069500170222     c***                eval      GCSzng=%trim(gcszng)
069600170222     c***                if        %subst(gcszng:1:1)<>' '
069700170222     c***                eval      gcszng=%trim(gcszng)+'-'
069800170222     c***                endif
069900161111     c
070000161111     c                   if        brvnpg<>4
070100170222     c                   eval      GCSultsp=
070200161111     c                             st2cat+
070300161111     c                             ' ' +%editw(st2dfv:'  /  ')+
070400161111     c                             ' '+%editw(st2hms:'  :  ')
070500161111     c                   else
070600170222     c                   eval      GCSultsp=
070700161111     c                             st2cat+
070800161111     c                             ' ' +%editw(st2dfv:'  /  ')+
070900161111     c                             ' '+stadsf
071000161111     c                   endif
071100170310     c                   else
071200170310     c                   clear                   recbrv
071300170310     c                   clear                   gcsultsp
071400170310     C                   ENDIF
071500161111     c
071600161111     c                   endsr
071700050221     C*---------------------------------------------------------------*
071800050221     C     *INZSR        BEGSR
071900050221     C*---------------------------------------------------------------*
072000050221     C     *ENTRY        PLIST
072100050221     C                   PARM                    KPJBA
072200050221     C                   MOVEL     KPJBU         FNLG87
072300050221      *
072400050322     c                   move      d87comm       gescomm           1
072500050221      *
072600050221     C                   Z-ADD     1             CODUT
072700050221     C                   CALL      'X§PARUT'
072800050221     C                   PARM                    UTEDSE
072900050221     C                   MOVEL     REC80         CNCR80
073000050221     C                   Z-ADD     0             SAVLNP            3 0
073100050221      *
073200050221     C* VERIFICO SE SI E' COLLEGATA UN SECONDO LIVELLO
073300050221     C     SimTpP        COMP      '2'                                    02
073400170221     c* la filiale da elaborare non è più dal profilo utente ma passata in DS
073500170221     C*****              Z-ADD     SIMPOU        §FLE
073600170221     c                   if        d87fgs<=*zeros
073700170221     C                   Z-ADD     SIMPOU        §FLE
073800170221     c                   else
073900170221     C                   movel     d87fgs        §FLE
074000170221     c                   endif
074100050221      *
074200050221     C                   TIME                    WHHUDT           14 0
074300050221     C                   MOVEL     WHHUDT        ORA               6 0
074400050221     C                   MOVE      WHHUDT        GMAUD8            8 0
074500050221     C* GIRO UDATE
074600050221     C                   MOVE      GMAUD8        G08DAT
074700050221     C                   MOVE      *ZEROS        G08INV
074800050221     C                   MOVEL     *BLANK        G08ERR
074900050221     C                   CALL      'XSRDA8'
075000050221     C                   PARM                    WLBDA8
075100050221     C                   Z-ADD     G08INV        AMGUD8            8 0
075200050221     C*
075300050221      *Trasformo la data dal formato AA/MM/GG al formato GG/MM/AA
075400050221     C     D87RIS        IFEQ      'R'
075500050221     C                   MOVE      D87DAT        G08INV
075600050221     C                   MOVE      *ZEROS        G08DAT
075700050221     C                   MOVEL     3             G08ERR
075800050221     C                   CALL      'XSRDA8'
075900050221     C                   PARM                    WLBDA8
076000050221     C                   Z-ADD     G08DAT        GMADRI            8 0
076100050221     C                   END
076200050221     C*
076300071011      * genero file di work in qtemp
076400071011     c                   Movel(p)  cmd2(1)       comman
076500071011     c                   eval      lenght = %len(%trim(comman))
076600071011     c                   Call      'QCMDEXC'                            97
076700071011     c                   Parm                    comman
076800071011     c                   Parm                    lenght
076900170308     c                   open      wfgcs80f
077000050221     C     D87TST        IFEQ      '1'
077100071011      * merce da portare
077200050221     C                   SETON                                        01
077300071011     c                   Movel(p)  cmd6(1)       comman
077400071011     c                   eval      lenght = %len(%trim(comman))
077500071011     c                   Call      'QCMDEXC'                            97
077600071011     c                   Parm                    comman
077700071011     c                   Parm                    lenght
077800170308     c                   open      wfgcs82l
077900050221     C                   ELSE
078000071011      * merce da prelevare
078100050221     C                   SETON                                        03
078200170224     c                   if        d87ord='Z'
078300071011     c                   Movel(p)  cmd4(1)       comman
078400170222     c                   else
078500170222     c                   Movel(p)  cmd8(1)       comman
078600170222     c                   endif
078700071011     c                   eval      lenght = %len(%trim(comman))
078800071011     c                   Call      'QCMDEXC'                            97
078900071011     c                   Parm                    comman
079000071011     c                   Parm                    lenght
079100170224     c                   if        d87ord='Z'
079200170308     c                   open      wfgcs81l
079300170222     c                   else
079400170308     c                   open      wfgcs85l
079500170222     c                   endif
079600170222     c
079700050221     C                   END
079800071011     c   97              except    errore
079900170307
080000170307     C* SOLO SPED IN FERMO DEPOSITO
080100170307     c                   if        *in01 and d87fdep='S'
080200170307     c                   seton                                        06
080300170307     c                   endif
080400170331     c                   if        *in01 and d87MES ='S'
080500170331     c                   seton                                        07
080600170331     c                   endif
080700170331
080800050221      *
080900050221     C* DEFINIZIONE CHIAVI
081000050221     C     KGCp          KLIST
081100050221     C                   KFLD                    FLE
081200050221     C                   KFLD                    FAS               3 0
081300161111RM*  C     KFVV2         KLIST
081400161111RM*  C                   KFLD                    BRVNPG
081500161111RM*  C                   KFLD                    WNFV5
081600161111RM*  C                   KFLD                    BRVFGS
081700161111RM*  C     KARTD         KLIST
081800161111RM*  C                   KFLD                    ARTFLS
081900161111RM*  C                   KFLD                    ARTLNA
082000161111RM*  C                   KFLD                    ARTNRS
082100161111RM*  C                   KFLD                    ARTNSC
082200050221     C*
082300050221     C     KARB          KLIST
082400050412     C                   KFLD                    GCPAASa
082500050412     C                   KFLD                    GCPLNPa
082600050412     C                   KFLD                    GCPNRSa
082700050412     C                   KFLD                    GCPNSPa
082800141215      *
082900141215     C     Kart1         KLIST
083000141216     C                   KFLD                    ARbAAS
083100141216     C                   KFLD                    ARbLNP
083200141216     C                   KFLD                    ARbNRS
083300141216     C                   KFLD                    ARbNSP
083400141216     C     Kbrv          KLIST
083500141219     C                   KFLD                    artFLS
083600141216     C                   KFLD                    artLNA
083700141216     C                   KFLD                    artNRS
083800141216     C                   KFLD                    artNSC
083900071026     C*
084000071026     C     Karbf         KLIST
084100071026     C                   KFLD                    arbAAS
084200071026     C                   KFLD                    arbLNP
084300071026     C                   KFLD                    arbNRS
084400071026     C                   KFLD                    arbNSP
084500071026     C*
084600071026     C     Kblp          KLIST
084700071026     C                   KFLD                    LBLAAN
084800071026     C                   KFLD                    LBLLPN
084900071026     C                   KFLD                    LBLNRN
085000071026     C                   KFLD                    LBLNSN
085100071026      *
085200071015     C     Kart          KLIST
085300071015     C                   KFLD                    GCsAAS
085400071015     C                   KFLD                    GCsLNP
085500071015     C                   KFLD                    GCsNRS
085600071015     C                   KFLD                    GCsNSP
085700050221     C*
085800071026     C     Kblt          KLIST
085900071026     C                   KFLD                    gcsAASf
086000071026     C                   KFLD                    gcsLNPf
086100071026     C                   KFLD                    gcsNRSf
086200071026     C                   KFLD                    gcsNSPf
086300071026     C*
086400071026     C     KAR5          KLIST
086500071026     C                   KFLD                    GCPAASa
086600071026     C                   KFLD                    GCPLNPa
086700071026     C                   KFLD                    GCPNRSa
086800071026     C                   KFLD                    GCPNSPa
086900071026     C                   KFLD                    trec5             3
087000071026     c                   move      'GEN'         trec5
087100050221     C*
087200050221     C     KTAB          KLIST
087300050221     C                   KFLD                    CODUT
087400050221     C                   KFLD                    COD               2
087500050221     C                   KFLD                    KEY               8
087600170331     C     KTAB2         KLIST
087700170331     C                   KFLD                    CODUT
087800170331     C                   KFLD                    COD
087900050221     C*
088000071026     c                   movel     *all'_'       lin198          198
088100170331     C***
088200170331     C* CARICO EVENTUALI ALTRI FILIALI GESTITE DA QUELLA COLLEGATA
088300170331     C***
088400050221     c                   clear                   trul06ds
088500050221     c                   eval      d06cod = '£6'
088600050221     c                   movel     §fle          d06key
088700050221     c                   movel     trul06ds      kpjbu
088800050221     c                   call      'TRUL06R'
088900050221     c                   parm                    kpjba
089000050221     c                   movel     kpjbu         trul06ds
089100050221     c                   movea     lin           fil£6
089200170331     c
089300170331     C* carico ZONE MESSAGGERIE
089400170331    1c                   if        *in01 and d87MES ='S'
089500170331     c                   clear                   xx                4 0
089600170331     c                   clear                   yy                4 0
089700170331     c                   clear                   ZZ                4 0
089800170331     C                   EVAL      ZZ=1
089900170331   1aC                   DOW       FIL£6(zz)>0
090000170331     c
090100170331     c                   movel     fil£6(zz)     alffil            3
090200170331     c                   movel     fil£6(zz)     wpo
090300170331     c                   movel(P)  '03'          cod
090400170331     c                   movel(P)  fil£6(zz)     key
090500170331     c     ktab          setll     tabel
090600170331     c     ktab2         reade     tabel
090700170331    2c                   dow       not %eof(tabel00f) and  wpo=alffil
090800170331    3c                   if        tblflg=' '
090900170331     c                   movel     tblkey        wpo               3
091000170331    4c                   if        wpo=alffil
091100170331     c                   movel     tbluni        ds03
091200170331
091300170331    4c                   if        §03tpm='M'
091400170331     c                   add       1             xx
091500170331     c                   movel     tblkey        w005a             5
091600170331     c                   move      w005a         zone(xx)
091700170331    5c                   endif
091800170331    4c                   endif
091900170331    3c                   endif
092000170331ù    c     ktab2         reade     tabel
092100170331    2c                   enddo
092200170331
092300170331     c                   add       1             zz
092400170331   1ac                   enddo
092500170331    1c                   endif
092600170331    c
092700050221     C                   ENDSR
092800050221     C*****************************************************************
092900050221     Otigcp000  E            AGG0
093000050221     O                       GCPDuma
093100050221     Otigcp000  E            AGG5
093200050221     O                       GCPDLMa
093300050221     OPRTF198   E    03      TESTA          1 02
093400050221     O                       RAGUT             +  0
093500170221     O                                           25 '   '
093600050221     O                                         + 25 '* ELENCO SPEDIZIONI DA '
093700050221     O                                         +  0 'PRELEVARE DAL MAGAZZINO '
093800050221     O                                         +  0 'GIACENZE *'
093900050221     O                       §PGM               127
094000050221     O                       GMAUD8             147 '  /  /    '
094100170308     O                                          192 'PAG:'
094200170308     O                       PAGE          Z    198
094300050221     O          E    01      TESTA          1 02
094400050221     O                       RAGUT             +  0
094500170221     O                                           25 '   '
094600050221     O                                         + 25 '* ELENCO SPEDIZIONI DA '
094700050221     O                                         +  0 'PORTARE NEL MAGAZZINO '
094800050221     O                                         +  0 'GIACENZE *'
094900050221     O                       §PGM               127
095000050221     O                       GMAUD8             147 '  /  /    '
095100170308     O                                          192 'PAG:'
095200170308     O                       PAGE          Z    198
095300050221     O          E            TESTA          1
095400050221     O                       KNSIF             +  0
095500050221     O                       KNMUS             +  0
095600050221     O                       ORA                147 '  :  :  '
095700050221     O          E    03      TESTA          1
095800050221     O                                         +  0 'CON DISPOSIZIONI DI:'
095900050221     O                       DESCR             +  1
096000071026     O          E    03 05   TESTA       1
096100071026     O                                         +  0 '* Prima riga bolla O'
096200071026     O                                         +  0 'riginale '
096300071026     O                                         +  0 'Seconda riga nuova '
096400071026     O                                         +  0 'Bolla *'
096500071026     O          E    03 05   TESTA          0
096600071026     O                                         +  0 '* Prima riga bolla O'
096700071026     O                                         +  0 'riginale '
096800071026     O                                         +  0 'Seconda riga nuova '
096900071026     O                                         +  0 'Bolla *'
097000071026     O          E    03 05   TESTA          0
097100071026     O                                         +  0 '* Prima riga bolla O'
097200071026     O                                         +  0 'riginale '
097300071026     O                                         +  0 'Seconda riga nuova '
097400071026     O                                         +  0 'Bolla *'
097500050221     O          E            TESTAR      1
097600170308     O                  03                     +  0 '---------  RISTAMPA'
097700170308     O                  03                     +  1 '---------  ELENCO DEL'
097800170308     O                  03   GMADRI            +  1 '  /  /    '
097900170307     O                  06                     +  5 '- SOLO SPEDIZIONI IN'
098000170307     O                  06                     +  1 'FERMO DEPOSITO -'
098100170331     O                  07                     +  5 '- SOLO ZONE MESSAGGE'
098200170331     O                  07                     +  0 'RIA -'
098300050221     O          E            TESTAT      2
098400071026     O                                            3 'LNP'
098500161125     O                                            7 'LNA'
098600161125     O                                           13 'ANNO'
098700161125     O                                           17 'SER'
098800161125     O                                           25 'SPEDIZ.'
098900161125     O                                           35 'Mittente'
099000161125     O                                           64 'Destinatario'
099100161125     O               03                          95 'Motivo di chiusura'
099200170306     O                                          109 'FlSgn'
099300170306     O                                          125 'Segnacol.Dal-Al'
099400170306     O                                          131 'Colli'
099500170306     O                                          140 'Peso Kg'
099600170306     O                                          145 'Zona'
099700170308     O               01                         156 'F.Deposito'
099800170308     O               03                         154 'DataEsec'
099900170306     O               03                         161 'MagGIA'
100000161125     o                                          172 'Ubicazione'
100100161125     o                                          195 'Val'
100200161125     o                                          198 'Sp'
100300050221     O          E            RIGA        2
100400071026     O                       GCsLNP               3
100500161125     O                       GCsLNA               7
100600161125     O                       GCsAAS              13
100700161125     O                       GCsNRS              17
100800161125     O                       GCsNSP        Z     25
100900161125     O                       arbRSMs             50
101000170125     O                       arbRSDs             75
101100170306     O                       gcscfgd            103
101200170306     O                       gcsfls        Z    108
101300170306     O                       gcsNCD        Z    117
101400170306     O                                          118 '/'
101500170306     O                       gcsNCA        Z    125
101600170306     O                       gcsNCL        Z    131
101700170306     O                       gcspkf        2    140
101800170306     O                       gcsZNC             144
101900170308     O               01      STPfdep            153
102000170306     O               03      STPDED             154 '  /  /  '
102100170306     O               03      STPsta             159
102200170306     O                       S30_zng            192
102300170306     O                       gcsbva             194
102400070629     O          E            segn1       1
102500070629     O                       sk1(1)        z   + 30
102600070629     O                       sk1(2)        z   +  1
102700070629     O                       sk1(3)        z   +  1
102800070629     O                       sk1(4)        z   +  1
102900070629     O                       sk1(5)        z   +  1
103000070629     O                       sk1(6)        z   +  1
103100070629     O                       sk1(7)        z   +  1
103200070629     O                       sk1(8)        z   +  1
103300070629     O                       sk1(9)        z   +  1
103400070629     O                       sk1(10)       z   +  1
103500070629     O                       sk1(11)       z   +  1
103600070629     O                       sk1(12)       z   +  1
103700070629     O                       sk1(13)       z   +  1
103800070629     O                       sk1(14)       z   +  1
103900070629     O                       sk1(15)       z   +  1
104000070629     O                       sk1(16)       z   +  1
104100070629     O                       sk1(17)       z   +  1
104200070629     O                       sk1(18)       z   +  1
104300070629     O                       sk1(19)       z   +  1
104400070629     O                       sk1(20)       z   +  1
104500070629     O          E            segn2       1
104600070629     O                       sk2(1)        z   + 30
104700070629     O                       sk2(2)        z   +  1
104800070629     O                       sk2(3)        z   +  1
104900070629     O                       sk2(4)        z   +  1
105000070629     O                       sk2(5)        z   +  1
105100070629     O                       sk2(6)        z   +  1
105200070629     O                       sk2(7)        z   +  1
105300070629     O                       sk2(8)        z   +  1
105400070629     O                       sk2(9)        z   +  1
105500070629     O                       sk2(10)       z   +  1
105600070629     O                       sk2(11)       z   +  1
105700070629     O                       sk2(12)       z   +  1
105800070629     O                       sk2(13)       z   +  1
105900070629     O                       sk2(14)       z   +  1
106000070629     O                       sk2(15)       z   +  1
106100070629     O                       sk2(16)       z   +  1
106200070629     O                       sk2(17)       z   +  1
106300070629     O                       sk2(18)       z   +  1
106400070629     O                       sk2(19)       z   +  1
106500070629     O                       sk2(20)       z   +  1
106600070629     O          E            segn3       1
106700070629     O                       sk3(1)        z   + 30
106800070629     O                       sk3(2)        z   +  1
106900070629     O                       sk3(3)        z   +  1
107000070629     O                       sk3(4)        z   +  1
107100070629     O                       sk3(5)        z   +  1
107200070629     O                       sk3(6)        z   +  1
107300070629     O                       sk3(7)        z   +  1
107400070629     O                       sk3(8)        z   +  1
107500070629     O                       sk3(9)        z   +  1
107600070629     O                       sk3(10)       z   +  1
107700070629     O                       sk3(11)       z   +  1
107800070629     O                       sk3(12)       z   +  1
107900070629     O                       sk3(13)       z   +  1
108000070629     O                       sk3(14)       z   +  1
108100070629     O                       sk3(15)       z   +  1
108200070629     O                       sk3(16)       z   +  1
108300070629     O                       sk3(17)       z   +  1
108400070629     O                       sk3(18)       z   +  1
108500070629     O                       sk3(19)       z   +  1
108600070629     O                       sk3(20)       z   +  1
108700070629     O          E            segn4       1
108800070629     O                       sk4(1)        z   + 30
108900070629     O                       sk4(2)        z   +  1
109000070629     O                       sk4(3)        z   +  1
109100070629     O                       sk4(4)        z   +  1
109200070629     O                       sk4(5)        z   +  1
109300070629     O                       sk4(6)        z   +  1
109400070629     O                       sk4(7)        z   +  1
109500070629     O                       sk4(8)        z   +  1
109600070629     O                       sk4(9)        z   +  1
109700070629     O                       sk4(10)       z   +  1
109800070629     O                       sk4(11)       z   +  1
109900070629     O                       sk4(12)       z   +  1
110000070629     O                       sk4(13)       z   +  1
110100070629     O                       sk4(14)       z   +  1
110200070629     O                       sk4(15)       z   +  1
110300070629     O                       sk4(16)       z   +  1
110400070629     O                       sk4(17)       z   +  1
110500070629     O                       sk4(18)       z   +  1
110600070629     O                       sk4(19)       z   +  1
110700070629     O                       sk4(20)       z   +  1
110800070629     O          E            segn5       1
110900070629     O                       sk5(1)        z   + 30
111000070629     O                       sk5(2)        z   +  1
111100070629     O                       sk5(3)        z   +  1
111200070629     O                       sk5(4)        z   +  1
111300070629     O                       sk5(5)        z   +  1
111400070629     O                       sk5(6)        z   +  1
111500070629     O                       sk5(7)        z   +  1
111600070629     O                       sk5(8)        z   +  1
111700070629     O                       sk5(9)        z   +  1
111800070629     O                       sk5(10)       z   +  1
111900070629     O                       sk5(11)       z   +  1
112000070629     O                       sk5(12)       z   +  1
112100070629     O                       sk5(13)       z   +  1
112200070629     O                       sk5(14)       z   +  1
112300070629     O                       sk5(15)       z   +  1
112400070629     O                       sk5(16)       z   +  1
112500070629     O                       sk5(17)       z   +  1
112600070629     O                       sk5(18)       z   +  1
112700070629     O                       sk5(19)       z   +  1
112800070629     O                       sk5(20)       z   +  1
112900071026     O          E            RIGAf       1
113000071026     O                       GCsLNPf              3
113100161125     O                       GCsLNAf              7
113200161125     O                       GCsAASf             13
113300161125     O                       GCsNRSf             17
113400161125     O                       GCsNSPf       Z     25
113500161125     O                       gcsRSMfs            50
113600170125     O                       gcsRSDf             87
113700170306     O                       gcsflsf       z    108
113800170306     O                       gcsNCDf       Z    117
113900170306     O                                          118 '/'
114000170306     O                       gcsNCAf       Z    125
114100161125     O                       spuntato           198
114200071026     O          E            segn1f      1
114300071026     O                       sk1f(1)       z   + 30
114400071026     O                       sk1f(2)       z   +  1
114500071026     O                       sk1f(3)       z   +  1
114600071026     O                       sk1f(4)       z   +  1
114700071026     O                       sk1f(5)       z   +  1
114800071026     O                       sk1f(6)       z   +  1
114900071026     O                       sk1f(7)       z   +  1
115000071026     O                       sk1f(8)       z   +  1
115100071026     O                       sk1f(9)       z   +  1
115200071026     O                       sk1f(10)      z   +  1
115300071026     O                       sk1f(11)      z   +  1
115400071026     O                       sk1f(12)      z   +  1
115500071026     O                       sk1f(13)      z   +  1
115600071026     O                       sk1f(14)      z   +  1
115700071026     O                       sk1f(15)      z   +  1
115800071026     O                       sk1f(16)      z   +  1
115900071026     O                       sk1f(17)      z   +  1
116000071026     O                       sk1f(18)      z   +  1
116100071026     O                       sk1f(19)      z   +  1
116200071026     O                       sk1f(20)      z   +  1
116300071026     O          E            segn2f      1
116400071026     O                       sk2f(1)       z   + 30
116500071026     O                       sk2f(2)       z   +  1
116600071026     O                       sk2f(3)       z   +  1
116700071026     O                       sk2f(4)       z   +  1
116800071026     O                       sk2f(5)       z   +  1
116900071026     O                       sk2f(6)       z   +  1
117000071026     O                       sk2f(7)       z   +  1
117100071026     O                       sk2f(8)       z   +  1
117200071026     O                       sk2f(9)       z   +  1
117300071026     O                       sk2f(10)      z   +  1
117400071026     O                       sk2f(11)      z   +  1
117500071026     O                       sk2f(12)      z   +  1
117600071026     O                       sk2f(13)      z   +  1
117700071026     O                       sk2f(14)      z   +  1
117800071026     O                       sk2f(15)      z   +  1
117900071026     O                       sk2f(16)      z   +  1
118000071026     O                       sk2f(17)      z   +  1
118100071026     O                       sk2f(18)      z   +  1
118200071026     O                       sk2f(19)      z   +  1
118300071026     O                       sk2f(20)      z   +  1
118400071026     O          E            segn3f      1
118500071026     O                       sk3f(1)       z   + 30
118600071026     O                       sk3f(2)       z   +  1
118700071026     O                       sk3f(3)       z   +  1
118800071026     O                       sk3f(4)       z   +  1
118900071026     O                       sk3f(5)       z   +  1
119000071026     O                       sk3f(6)       z   +  1
119100071026     O                       sk3f(7)       z   +  1
119200071026     O                       sk3f(8)       z   +  1
119300071026     O                       sk3f(9)       z   +  1
119400071026     O                       sk3f(10)      z   +  1
119500071026     O                       sk3f(11)      z   +  1
119600071026     O                       sk3f(12)      z   +  1
119700071026     O                       sk3f(13)      z   +  1
119800071026     O                       sk3f(14)      z   +  1
119900071026     O                       sk3f(15)      z   +  1
120000071026     O                       sk3f(16)      z   +  1
120100071026     O                       sk3f(17)      z   +  1
120200071026     O                       sk3f(18)      z   +  1
120300071026     O                       sk3f(19)      z   +  1
120400071026     O                       sk3f(20)      z   +  1
120500071026     O          E            segn4f      1
120600071026     O                       sk4f(1)       z   + 30
120700071026     O                       sk4f(2)       z   +  1
120800071026     O                       sk4f(3)       z   +  1
120900071026     O                       sk4f(4)       z   +  1
121000071026     O                       sk4f(5)       z   +  1
121100071026     O                       sk4f(6)       z   +  1
121200071026     O                       sk4f(7)       z   +  1
121300071026     O                       sk4f(8)       z   +  1
121400071026     O                       sk4f(9)       z   +  1
121500071026     O                       sk4f(10)      z   +  1
121600071026     O                       sk4f(11)      z   +  1
121700071026     O                       sk4f(12)      z   +  1
121800071026     O                       sk4f(13)      z   +  1
121900071026     O                       sk4f(14)      z   +  1
122000071026     O                       sk4f(15)      z   +  1
122100071026     O                       sk4f(16)      z   +  1
122200071026     O                       sk4f(17)      z   +  1
122300071026     O                       sk4f(18)      z   +  1
122400071026     O                       sk4f(19)      z   +  1
122500071026     O                       sk4f(20)      z   +  1
122600071026     O          E            segn5f      1
122700071026     O                       sk5f(1)       z   + 30
122800071026     O                       sk5f(2)       z   +  1
122900071026     O                       sk5f(3)       z   +  1
123000071026     O                       sk5f(4)       z   +  1
123100071026     O                       sk5f(5)       z   +  1
123200071026     O                       sk5f(6)       z   +  1
123300071026     O                       sk5f(7)       z   +  1
123400071026     O                       sk5f(8)       z   +  1
123500071026     O                       sk5f(9)       z   +  1
123600071026     O                       sk5f(10)      z   +  1
123700071026     O                       sk5f(11)      z   +  1
123800071026     O                       sk5f(12)      z   +  1
123900071026     O                       sk5f(13)      z   +  1
124000071026     O                       sk5f(14)      z   +  1
124100071026     O                       sk5f(15)      z   +  1
124200071026     O                       sk5f(16)      z   +  1
124300071026     O                       sk5f(17)      z   +  1
124400071026     O                       sk5f(18)      z   +  1
124500071026     O                       sk5f(19)      z   +  1
124600071026     O                       sk5f(20)      z   +  1
124700071026     O          E            separa      1
124800071026     o                       lin198             198
124900071026     O          E            errRIGAf    1
125000071026     O                                         + 25 '* Attenzione non trovata '
125100071026     O                                         +  0 'spedizione figlia verifi'
125200071026     O                                         +  0 'care i dati della bolla *'
125300071011     O          E            errore         1 02
125400071011     O                       RAGUT             +  0
125500170221     O                                           25 '   '
125600071011     O                                         + 25 '* ELENCO SPEDIZIONI DA '
125700071011     O                                         +  0 'PORTARE NEL MAGAZZINO '
125800071011     O                                         +  0 'GIACENZE *'
125900071011     O                       §PGM               127
126000071011     O                       GMAUD8             147 '  /  /    '
126100071011     O                                         + 10 'PAG:'
126200071011     O                       PAGE          Z   +  1
126300071011     O          E            errore      2
126400071011     O                                         + 25 '* Attenzione errore in '
126500071011     O                                         +  0 'esecuzione programma  '
126600071011     O                                         +  0 'avvisare il CED *'
126700071011      *_________________________________________________________________________
126800071011**  CMD1
126900170308DLTF FILE(QTEMP/WFGCS80F)
127000071011**  CMD2
127100170308CRTDUPOBJ OBJ(WFGCS80F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
127200071011**  CMD3
127300170308DLTF FILE(QTEMP/WFGCS81L)
127400071011**  CMD4
127500170308CRTDUPOBJ OBJ(WFGCS81L) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
127600071011**  CMD5
127700170308DLTF FILE(QTEMP/WFGCS82L)
127800071011**  CMD6
127900170308CRTDUPOBJ OBJ(WFGCS82L) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
128000170222**  CMD7
128100170308DLTF FILE(QTEMP/WFGCS85L)
128200170222**  CMD8
128300170308CRTDUPOBJ OBJ(WFGCS85L) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)

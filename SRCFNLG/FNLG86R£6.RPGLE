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
001900050221     D §FLE            S              3  0
002000050221     D WLBDA8          DS
002100050221     D  G08DAT                 1      8  0
002200050221     D  G08INV                 9     16  0
002300050221     D  G08ERR                17     17
002400050221     D  G08TGI                18     22  0
002500050221      *
002600161111     D RECBRV        E DS                  EXTNAME(FNBRV00F)
002700161111     D WSAVB         E DS                  EXTNAME(FNBRV00F)
002800161111     d                                     prefix(S_)
002900050221     d dar5gen       e ds
003000050221     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
003100070629     d dskiere         ds
003200070629     d  sk1                    1    140  0 dim(20)
003300070629     d  sk2                  141    280  0 dim(20)
003400070629     d  sk3                  281    420  0 dim(20)
003500070629     d  sk4                  421    560  0 dim(20)
003600070629     d  sk5                  561    700  0 dim(20)
003700070629     d  skt                    1    700  0 dim(100)
003800071026     d dskieref        ds
003900071026     d  sk1f                   1    140  0 dim(20)
004000071026     d  sk2f                 141    280  0 dim(20)
004100071026     d  sk3f                 281    420  0 dim(20)
004200071026     d  sk4f                 421    560  0 dim(20)
004300071026     d  sk5f                 561    700  0 dim(20)
004400071026     d  sktf                   1    700  0 dim(100)
004500071026
004600070629
004700161007     D tigcp00ds     E DS                  extname(tigcp00f)
004800161007     D fnlgi1ds      E DS
004900050221     D CNCR80        E DS
005000050221     D KPJBA         E DS
005100161111     D DS7B          E DS
005200050221     d trul06ds      e ds
005300050221     d  lin                    1     90  0 dim(30)
005400050221     D FNLG87        E DS                  EXTNAME(FNLG87DS)
005500161111     D* REPERISCE DATI FOGLI
005600161111     D DSLV53        E DS                  EXTNAME(FNLV53DS)
005700050221      *
005800050221     D                SDS
005900050221     D  §PGM                   1     10
006000071011     D cmd1            S             66    DIM(01) CTDATA PERRCD(1)
006100071011     D cmd2            S             66    DIM(01) CTDATA PERRCD(1)
006200071011     D cmd3            S             66    DIM(01) CTDATA PERRCD(1)
006300071011     D cmd4            S             66    DIM(01) CTDATA PERRCD(1)
006400071011     D cmd5            S             66    DIM(01) CTDATA PERRCD(1)
006500071011     D cmd6            S             66    DIM(01) CTDATA PERRCD(1)
006600170222     D cmd7            S             66    DIM(01) CTDATA PERRCD(1)
006700170222     D cmd8            S             66    DIM(01) CTDATA PERRCD(1)
006800161111     D WLBDAT          DS
006900161111     D  G02DAT                 1      8  0
007000161111     D  G02INV                 9     16  0
007100161111     D  G02ERR                17     17
007200161111     D  G02TGI                18     22  0
007300071011     d comman          s             80
007400071011     d lenght          s             15  5 inz(80)
007500161111     d wdfv            S                   LIKE(d53dfv)
007600161111     d WSPG            S                   LIKE(FVVSPG)
007700161111     d Wnfv5           S                   LIKE(FVVnfv)
007800070629      *----------------------------------------------------------------
007900050221     C                   DO        30            I                 2 0
008000050221     c                   if        fil£6(i) = 0
008100050221     c                   leave
008200050221     c                   endif
008300050221     C                   Z-ADD     fil£6(i)      FLE               3 0
008400050221     C*
008500050221     c   01              z-add     10            fas
008600050221     c   03              z-add     35            fas
008700050221      * lettura giacenze
008800050221      *
008900050418     c     kgcp          setll     tigcp26l
009000000607     c                   do        *hival
009100050418     c     fle           reade     tigcp26l                             90
009200050413      *allocazione
009300050413     c                   if        *in90
009400050418     c     fle           reade(n)  tigcp26l
009500050413     c                   if        %eof
009600050413     c                   leave
009700050413     c                   end
009800050413     c                   iter
009900050413     c                   endif
010000050413      *
010100000607     c                   if        %eof
010200000607     c                   leave
010300000607     c                   end
010400050221      * se richiesto materiale da portare a magazzino la fase non può essere
010500050221      * maggiore di 26
010600050418     c                   if        gcpfas > 30 and *in01
010700050221     c                   leave
010800040507     c                   end
010900050221
011000050408      *solo progressivo 0
011100050408     c                   if        gcpfrg <> 0
011200050408     c                   iter
011300050408     c                   end
011400161007     c*
011500161007     c                   clear                   fnlgi1ds
011600161007     c                   eval      ilgi1agc=gcpagc
011700161007     c                   eval      ilgi1fgc=gcpfgc
011800161007     c                   eval      ilgi1ngc=gcpngc
011900161007     c                   movel     fnlgi1ds      kpjbu
012000161007     c                   call      'FNLGI1R'
012100161007     c                   parm                    kpjba
012200161007     c                   parm                    tigcp00ds
012300161007     c                   movel     kpjbu         fnlgi1ds
012400161007
012500050411      * in uscita dal magazzino
012600050221--2)>C     GCPFAS        IFEQ      35
012700050221     C     GCPDuma       ANDEQ     0
012800050221     C     D87RIS        ANDEQ     *BLANK
012900050221     C     *IN03         ANDEQ     '1'
013000161007     c     olgi1outmg    andne     ' '
013100050221      *
013200050221     C     GCPFAS        OREQ      35
013300050221     C     GCPDuma       ANDEQ     D87DAT
013400050221     C     D87RIS        ANDEQ     'R'
013500050221     C     *IN03         ANDEQ     '1'
013600161007     c     olgi1outmg    andne     ' '
013700050221      *
013800050411      * in uscita dal magazzino x resi
013900141110---->C*mm  GCPFAS        OReq      50
014000141107---->C     GCPFAS        ORge      50
014100050411---->C     GCPdis        andeq     'R'
014200050411     C     GCPDuma       ANDEQ     0
014300050411     C     D87RIS        ANDEQ     *BLANK
014400050411     C     *IN03         ANDEQ     '1'
014500161007     c     olgi1outmg    andne     ' '
014600050221      *
014700050411---->C     GCPdis        oreq      'R'
014800050411     C     GCPDuma       ANDEQ     D87DAT
014900050411     C     D87RIS        ANDEQ     'R'
015000050411     C     *IN03         ANDEQ     '1'
015100161007     c     olgi1outmg    andne     ' '
015200161007      *
015300050411      * in uscita dal magazzino x dirottamenti
015400141110---->C*mm  GCPFAS        OReq      40
015500141107---->C     GCPFAS        ORge      40
015600050411---->C     GCPdis        andeq     'D'
015700050411     C     GCPDuma       ANDEQ     0
015800050411     C     D87RIS        ANDEQ     *BLANK
015900050411     C     *IN03         ANDEQ     '1'
016000161007     c     olgi1outmg    andne     ' '
016100050411      *
016200141110---->C*mm  GCPFAS        OReq      50
016300141110---->C*mm  GCPdis        andeq     'D'
016400141110     C*mm  GCPDuma       ANDEQ     0
016500141110     C*mm  D87RIS        ANDEQ     *BLANK
016600141110     C*mm  *IN03         ANDEQ     '1'
016700050420      *
016800050411---->C     GCPdis        oreq      'D'
016900050411     C     GCPDuma       ANDEQ     D87DAT
017000050411     C     D87RIS        ANDEQ     'R'
017100050411     C     *IN03         ANDEQ     '1'
017200161007     c     olgi1outmg    andne     ' '
017300050411
017400050411      *merce da portare in magazzino
017500050413---->C     GCPFAS        ORle      30
017600050221     C     D87RIS        ANDEQ     *BLANK
017700050221     C     *IN01         ANDEQ     '1'
017800161007     c     olgi1amg      andeq     'S'
017900161007     C**   GCPDLMa       ANDEQ     0
018000050221      *
018100050221     C     GCPFAS        ORle      70
018200050221     C     GCPDLMa       ANDEQ     D87DAT
018300050221     C     D87RIS        ANDEQ     'R'
018400050221     C     *IN01         ANDEQ     '1'
018500050221      *
018600050221     C* PRELEVO DATI BOLLA
018700141119     C     KARB          CHAIN     FNARB01L
018800170307    1c                   if        %found(fnarb01l)
018900050221     c                   clear                   dar5gen
019000050221     C     KAR5          CHAIN     FiAR501L
019100170307    2c                   if        %found(fiar501l)
019200050221     c                   movel     ar5uni        dar5gen
019300170307    2c                   end
019400141215     c                   clear                   znc               2 0
019500141216     C     Kart1         chain     Fnart01L
019600170307    2c                   if        %found(fnart01l)
019700141216     C     Kbrv          setll     Fnbrv07L
019800170307    3c                   do        *hival
019900141216     C     Kbrv          reade     Fnbrv07L
020000170307    4c                   if        %eof(fnbrv07l)
020100141215     c                   leave
020200170307    4c                   endif
020300170307    4c                   if        brvdcs = 0 or brvnpg = 8
020400141215     c                   iter
020500170307   X4c                   else
020600141215     c                   move      brvznc        znc
020700141215     c                   leave
020800170307    4c                   endif
020900170307    3c                   enddo
021000170307    2c                   endif
021100071026      * solo per merce da prelevare dal magazzino *IN03=on
021200071026      * se dirottamento o reso reperisce riferimenti figlia da stampare
021300170307    2c                   if        *in03
021400080220     c                   seton                                        31
021500170307    3c                   if        gcpdis = 'D' or gcpdis = 'R'
021600071026     C     Karb          chain     fnlbl02l                           31
021700071026     C  n31Kblp          chain     fnblp01l                           31
021800170307    3c                   endif
021900170307    2c                   endif
022000170307     c
022100170307     c* solo per merce da portare a mag giacenze posso parzializzare
022200170307     c*  per fermo deposito
022300170307
022400071011      *scrittura wf con bolle da stampare successivamente
022500170307    2c                   if        *in03 or d87fdep=' ' or arbffd=d87fdep
022600071011     c                   exsr      scriviwf
022700050221     c
022800071026     C* AGGIORNO DATA DI STAMPA
022900170307    3C     D87RIS        IFEQ      *BLANK
023000071011     C   01              MOVE      AMGUD8        GCPDLMa
023100071011     C   01              EXCEPT    AGG5
023200071011     C   03              MOVE      AMGUD8        GCPDuma
023300071011     C   03              EXCEPT    AGG0
023400170307    4c                   if        gescomm = 'S'
023500071011     c                   commit
023600170307    4c                   endif
023700170307    3c                   endif
023800170307    2c                   endif
023900071011
024000170307    1C                   END
024100071011     C*
024200170307    0C                   END
024300071011     C*
024400071011     c                   enddo
024500071011
024600071011     c                   enddo
024700071011      * eliminazione file di work
024800170308     c                   close     wfgcs80f
024900071011      * rilegge wf e produce il tabulato
025000071011     c                   exsr      stampa
025100071011      *
025200170302     c                   if        *in03  and d87ord='Z'
025300170308     c   03              close     wfgcs81l
025400170302     c                   else
025500170308     c                   close     wfgcs85l
025600170302     c                   endif
025700170308     c   01              close     wfgcs82l
025800170224     c                   if        *in03  and d87ord='Z'
025900170222     c                   Movel(p)  cmd3(1)       comman
026000170222     c                   endif
026100170222     c                   if        *in03  and d87ord='L'
026200170222     c                   Movel(p)  cmd7(1)       comman
026300170222     c                   endif
026400071011     c   01              Movel(p)  cmd5(1)       comman
026500071011     c                   eval      lenght = %len(%trim(comman))
026600071011     c                   Call      'QCMDEXC'                            97
026700071011     c                   Parm                    comman
026800071011     c                   Parm                    lenght
026900170302     c
027000170302     c* Cancellazione file fisico da QTEMP
027100170302     c                   Movel(p)  cmd1(1)       comman
027200170302     c                   eval      lenght = %len(%trim(comman))
027300170302     c                   Call      'QCMDEXC'                            97
027400170302     c                   Parm                    comman
027500170302     c                   Parm                    lenght
027600161007
027700161007     c                   clear                   fnlgi1ds
027800161007     c                   eval      ILGI1TLA= 'C'
027900161007     c                   movel     fnlgi1ds      kpjbu
028000161007     c                   call      'FNLGI1R'
028100161007     c                   parm                    kpjba
028200161111     c
028300161111     C                   CLEAR                   DSLV53
028400161111     C                   MOVEL     'C'           D53TLA
028500161111     C                   CALL      'FNLV53R'
028600161111     C                   PARM                    DSLV53
028700161111     C*
028800071011     c                   seton                                        lr
028900071011      *_________________________________________________________
029000071011     c     stampa        begsr
029100071011      *_________________________________________________________
029200071011      * legge la v.l. opportuna x l'ordinamento a seconda che sia
029300071011      * merce da prelevare o da portare a magazzino
029400170224     c   03              if        d87ord='Z'
029500170308     c     *loval        setll     wfgcs81l
029600170222     c                   endif
029700170222     c   03              if        d87ord='L'
029800170308     c     *loval        setll     wfgcs85l
029900170222     c                   endif
030000170308     c   01*loval        setll     wfgcs82l
030100170222
030200071011     c                   do        *hival
030300170222
030400170224     c   03              if        d87ord='Z'
030500170308     c                   read      wfgcs81l                               98
030600170222     c                   endif
030700170222     c   03              if        d87ord='L'
030800170308     c                   read      wfgcs85l                               98
030900170222     c                   endif
031000170308     c   01              read      wfgcs82l                               98
031100170222
031200071011     c   98              leave
031300071011      * segnacolli non sequenziali prelevo i numeri da dettaglio colli
031400070629     c                   clear                   skt
031500071011     c                   if        gcsfns = 'S'
031600071015     C     Kart          setll     Fnart01L
031700070629     c                   z-add     0             ncl               3 0
031800071011     c                   do        *hival
031900071015     C     Kart          reade     Fnart01L
032000070629     c                   if        %eof(fnart01l) or ncl = 100
032100070629     c                   leave
032200070629     c                   endif
032300070629     c                   add       1             ncl
032400070629     c                   z-add     artnsc        skt(ncl)
032500070629     c                   enddo
032600070629     c                   endif
032700070629     c
032800071026      * bolla figlia
032900071026     c                   clear                   sktf
033000140728     c                   clear                   spuntato          8
033100140728     c                   clear                   spu               3 0
033200140728     c                   clear                   nospu             3 0
033300170306     c                   clear                   stpsta            2
033400170306
033500140718     c                   if        *in03
033600071026     C     Kblt          setll     Fnblt01L
033700071026     c                   z-add     0             ncl               3 0
033800071026     c                   do        *hival
033900071026     C     Kblt          reade     Fnblt01L
034000071026     c                   if        %eof(fnblt01l) or ncl = 100
034100140728     c                   select
034200140728     c                   when      spu >= 1 and nospu = 0
034300140728     c                   eval      spuntato = 'Totale'
034400140728     c                   when      spu >= 1 and nospu >= 1
034500140728     c                   eval      spuntato = 'Parziale'
034600140728     c                   other
034700140728     c                   clear                   spuntato
034800140728     c                   endsl
034900071026     c                   leave
035000071026     c                   endif
035100140718     c* se presente la data entrata vuol dire
035200140718     c* che è già stato etichettato il pacco con la nuova
035300140718     c* etichetta della figlia
035400140728     c                   if        bltdse <> 0
035500140728     c                   add       1             spu
035600140728     c                   else
035700140728     c                   add       1             nospu
035800140728     c                   end
035900140718     c*
036000140718     c                   if        gcsfnsf = 'S'
036100071026     c                   add       1             ncl
036200071026     c                   z-add     bltnsc        sktf(ncl)
036300071026     c                   endif
036400140718     c                   enddo
036500140718     c                   endif
036600071026     c
036700050221      *
036800071011     C   03GCsDIS        IFNE      SAVDIS
036900050221     C* PRELEVO DESCRIZIONE TIPO DISPOSIZIONE
037000050221     C                   MOVEL     '2D'          COD
037100050221     C                   MOVEL     *BLANKS       KEY
037200071011     C                   MOVEL     GCsDIS        KEY
037300050221     C     KTAB          CHAIN     TABEL                              50
037400050221     C  N50              MOVEL     TBLUNI        DESCR            20
037500050221     C   50              MOVEL     *ALL'?'       DESCR
037600071026     c                   setoff                                       05
037700071026     c                   if        gcsdis  = 'D' or gcsdis = 'R'
037800071026     c                   seton                                        05
037900071026     c                   endif
038000050221     C                   EXCEPT    TESTA
038100050221     C     D87RIS        IFEQ      'R'
038200170308     c     *in06         oreq      *on
038300050221     C                   EXCEPT    TESTAR
038400050221     C                   END
038500050221     C                   EXCEPT    TESTAT
038600050221     C                   SETON                                        99
038700071011     C                   MOVE      GCsDIS        SAVDIS            1
038800071011     C                   END
038900050221      *
039000050221     C  N99
039100050221     COR OF              DO
039200050221     C                   EXCEPT    TESTA
039300050221     C     D87RIS        IFEQ      'R'
039400170308     c     *in06         oreq      *on
039500050221     C                   EXCEPT    TESTAR
039600050221     C                   END
039700050221     C                   EXCEPT    TESTAT
039800050221     C                   END
039900050221     C  N99              SETON                                        99
040000050221      *
040100140718     c                   movel     gcsrsm        arbrsms          23
040200140718     c                   movel     gcsrsd        arbrsds          23
040300170222     c***                movel     gcszng        s30_zng          30
040400170222
040500170222     c                   clear                   s30_zng          30
040600170222     c                   if        gcszng<>*blanks
040700170222     c                   eval      s30_zng=%trim(gcszng)+'-'+gcsultsp
040800170222     c                   else
040900170222     c                   eval      s30_zng=gcsultsp
041000170222     c                   endif
041100170222     c
041200050221     C     *IN03         IFEQ      '1'
041300071011     C                   Z-ADD     GCsdeda       G08INV
041400050221     C                   MOVE      *ZEROS        G08DAT
041500050221     C                   MOVEL     '3'           G08ERR
041600050221     C                   CALL      'XSRDA8'
041700050221     C                   PARM                    WLBDA8
041800170306     C                   move      G08DAT        w0020             2 0
041900170306     C                   movel     g08dat        STPDED            6 0
042000170306     C                   move      w0020         STPDED
042100170308     c*
042200170306     c
042300070522     C                   MOVEL     '2C'          COD
042400070522     C                   MOVEL     *BLANKS       KEY
042500071011     C                   MOVEL     GCscfg        KEY
042600070522     C     KTAB          CHAIN     TABEL                              50
042700170306     C  N50              MOVEL     TBLUNI        gcscfgd          26
042800071011     C   50              MOVEL     *ALL'?'       gcscfgd
042900170306     c* Vedo se è già a mag gaicenze oppure no
043000170306     c                   if        gcsdlma=0
043100170306     c                   eval      stpsta='NO'
043200170306     c                   else
043300170306     c                   eval      stpsta='SI'
043400170306     c                   endif
043500170306     C                   END
043600170308     c
043700170308     c                   clear                   stpfdep           2
043800170308     c                   if        *in01  and gcsffd='S'
043900170308     c                   eval      stpfdep='SI'
044000170308     c                   endif
044100170306
044200050221     C                   EXCEPT    RIGA
044300070629      * stampa dettaglio dei segnacolli se non sequenziali
044400070629     c                   if        skt(1) > 0
044500070629     c                   except    segn1
044600070629     c                   endif
044700070629     c                   if        skt(21) > 0
044800070629     c                   except    segn2
044900070629     c                   endif
045000070629     c                   if        skt(41) > 0
045100070629     c                   except    segn3
045200070629     c                   endif
045300070629     c                   if        skt(61) > 0
045400070629     c                   except    segn4
045500070629     c                   endif
045600070629     c                   if        skt(81) > 0
045700070629     c                   except    segn5
045800070629     c                   endif
045900071026      * solo per merce da prelevare dal magazzino *IN03=ON
046000071026      * seconda riga dettaglio bolla figlia
046100071026      * testa se il campo spedizione è pieno per capire se è stato
046200071026      * agganciato il legame altrimenti stampa anomalia
046300071026     c   03              if        gcsnspf > 0
046400140718     c                   movel     gcsRSMf       gcsRSMfs         23
046500071026     C                   EXCEPT    RIGAf
046600071026      * stampa dettaglio dei segnacolli se non sequenziali
046700071026     c                   if        sktf(1) > 0
046800071026     c                   except    segn1f
046900071026     c                   endif
047000071026     c                   if        sktf(21) > 0
047100071026     c                   except    segn2f
047200071026     c                   endif
047300071026     c                   if        sktf(41) > 0
047400071026     c                   except    segn3f
047500071026     c                   endif
047600071026     c                   if        sktf(61) > 0
047700071026     c                   except    segn4f
047800071026     c                   endif
047900071026     c                   if        sktf(81) > 0
048000071026     c                   except    segn5f
048100071026     c                   endif
048200071026      * separazione
048300071026     C                   EXCEPT    separa
048400071026     c                   else
048500071026     c                   if        gcsdis = 'D' or gcsdis = 'R'
048600071026     C                   EXCEPT    errRIGAf
048700071026     c                   endif
048800071026     c                   endif
048900071011      *
049000071011     c                   enddo
049100071011     c                   endsr
049200071011      *_________________________________________________________
049300071011     c     scriviwf      begsr
049400071011      *_________________________________________________________
049500170308     c                   clear                   wfgcs080
049600071011     c                   z-add     arbaas        GCSaas
049700071011     c                   z-add     arblnp        GCSlnp
049800071011     c                   z-add     arbnrs        GCSnrs
049900071011     c                   z-add     arbnsp        GCSnsp
050000071011     c                   z-add     arblna        GCSlna
050100170308     c                   move      arbffd        GCSffd
050200071011     c                   move      arbrsm        GCSrsm
050300071011     c                   move      arbrsd        GCSrsd
050400141215     c                   if        arbznc <> znc and znc <> 0
050500141215     c                   z-add     znc           gcsznc
050600141215     c                   else
050700071011     c                   z-add     arbznc        GCSznc
050800141215     c                   endif
050900071011     c                   z-add     gcpfgc        GCSfgc
051000071011     c                   z-add     gcpngc        GCSngc
051100071011     c                   z-add     gcpdeda       GCSdeda
051200170306     c                   z-add     gcpdlma       GCSdlma
051300071011     c                   move      gcpdis        GCSdis
051400071011     c                   move      gcpcfg        GCScfg
051500170126     c
051600170126     c* se non è stato portato in mag giacenza, non stampo
051700170126     c*  l'ubicazine, altrimenti si
051800170307     c   03              if        gcpdlma>0
051900071011     c                   movel     §ar5zng       GCSzng
052000170126     c                   endif
052100170307     c  n03              movel     §ar5zng       GCSzng
052200161111     c* se la bolla ha la data merce a mag giacenze, significa che
052300161111     c*  probabilmente non e' stata portata in giacenza
052400161111     c*  quindi al posto della ubicazione stampo la data e ora ultima spunta
052500161111     c*
052600170126     c**                 if        gcpdlma=0
052700161111     C     Kart1         chain     Fnart01L
052800161111     c                   if        %found(fnart01l)
052900161111     c                   exsr      Ricspu
053000161111     c                   endif
053100170126     c***                endif
053200161111     c
053300071011     c                   move      §ar5bva       GCSbva
053400071026     c                   z-add     arbfls        GCSfls
053500071011     c                   z-add     arbncd        GCSncd
053600071011     c                   z-add     arbnca        GCSnca
053700071011     c                   move      arbfns        GCSfns
053800071011     c                   z-add     arbncl        GCSncl
053900071011     c                   z-add     arbpkf        GCSpkf
054000071011     c                   z-add     arbvlf        GCSvlf
054100071026      * solo per merce da prelevare da magazzino *IN03=on
054200071026      *figlia se non trova legame o blp non scrive nulla
054300071026     c   03              if        not *in31
054400071026     c                   z-add     blpaas        GCSaasf
054500071026     c                   z-add     blplnp        GCSlnpf
054600071026     c                   z-add     blpnrs        GCSnrsf
054700071026     c                   z-add     blpnsp        GCSnspf
054800071026     c                   z-add     blplna        GCSlnaf
054900071026     c                   move      blprsm        GCSrsmf
055000071026     c                   move      blprsd        GCSrsdf
055100071026     c                   z-add     blpznc        GCSzncf
055200071026     c                   z-add     blpfls        GCSflsf
055300071026     c                   z-add     blpncd        GCSncdf
055400071026     c                   z-add     blpnca        GCSncaf
055500071026     c                   move      blpfns        GCSfnsf
055600071026     c                   z-add     blpncl        GCSnclf
055700071026     c                   z-add     blppkf        GCSpkff
055800071026     c                   z-add     blpvlf        GCSvlff
055900071026     c                   endif
056000170308     c                   write     wfgcs080
056100071011     c                   endsr
056200161111     C**************************************************************************
056300161111     C* cerco spunta del collo in qualsiasi categoria
056400161111     C**************************************************************************
056500161111     C     RICSPU        BEGSR
056600161111     C                   CLEAR                   Wimm
056700161111     C                   CLEAR                   WSAVB
056800161111     C                   CLEAR                   WDFV
056900161111     C                   CLEAR                   WTROV
057000161111     C                   CLEAR                   WSPG
057100161111     C                   CLEAR                   stadsf           10
057200161111RM*  C     kbrv          CHAIN     FNBRV07l                           33
057300161111RM*  C*
057400161111RM* 1C     *IN33         DOWEQ     *OFF
057500161111     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
057600161111     c                   clear                   dslv53
057700161111     C                   z-add     brvnFV        D53NFV
057800161111     C                   MOVEL     brvNPG        D53NPG
057900161111     C                   MOVEL     brvFGS        D53FGS
058000161111     C****               MOVEL     brvfle        D53flf
058100161111     C                   CALL      'FNLV53R'
058200161111     C                   PARM                    DSLV53
058300161111     C* ERRORE
058400161111    2C     D53ERR        IFeq      ' '
058500161111     c                   movel     brvdfs        brvimm           14 0
058600161111     c                   move      brvhms        brvimm
058700161111    3c                   select
058800161111     c* Se maggiore memorizzo
058900161111     c                   when      d53dfv>wdfv
059000161111     C                   MOVEL     RECBRV        WSAVB
059100161111     C                   MOVEL     brvimm        Wimm             14 0
059200161111     C                   MOVEL     d53DFV        WDFV
059300161111     C                   MOVEL     d53spg        WSPG
059400161111     C                   MOVEL     'S'           WTROV             1
059500161111     c
059600161111     c* Se uguale tengo la data ora immissione + alta
059700161111     c                   when      d53dfv=wdfv  and brvimm>wimm
059800161111     C                   MOVEL     brvimm        Wimm             14 0
059900161111     C                   MOVEL     RECBRV        WSAVB
060000161111     C                   MOVEL     d53DFV        WDFV
060100161111     C                   MOVEL     d53spg        WSPG
060200161111     C                   MOVEL     'S'           WTROV             1
060300161111     C                   eval      wimm=brvimm
060400161111     c* Se stessa data e ora immissione, tengo cat 3 o 4
060500161111     c                   when      d53dfv=wdfv and brvimm=wimm and brvnpg<>2
060600161111     C                   MOVEL     brvimm        Wimm             14 0
060700161111     C                   MOVEL     RECBRV        WSAVB
060800161111     C                   MOVEL     d53DFV        WDFV
060900161111     C                   MOVEL     d53spg        WSPG
061000161111     C                   MOVEL     'S'           WTROV             1
061100161111     C                   eval      wimm=brvimm
061200161111    3c                   endsl
061300161111    2c                   endif
061400161111     c
061500161111RM*  C     Kartd         reade     FNBRV07l                               33
061600161111    1c                   enddo
061700161111     c
061800161111     C     WTROV         IFEQ      'S'
061900161111     C                   MOVEL     WSAVB         RECBRV
062000161111     c
062100161111     c* Memorizzo nel campo ubicazione
062200161111     C* DECODIFICO LA CATEGORIA SPUNTA
062300161111    1C     BRVNPG        IFEQ      3
062400161111     C                   MOVEL     '7B'          COD
062500161111     C                   MOVEL(P)  wspg          KEY
062600161111     C     KTAB          CHAIN     TABEL                              32
062700161111     C  N32              MOVEL     TBLUNI        ds7b
062800161111     C   32              CLEAR                   ds7b
062900161111     C                   MOVEL     §7bdesb       ST2CAT            3
063000161111   X1C                   ELSE
063100161111     C                   MOVEL     '7N'          COD
063200161111     C                   MOVEL(P)  BRVNPG        KEY
063300161111     C     KTAB          CHAIN     TABEL                              32
063400161111     C  N32              MOVEL     TBLUNI        ST2CAT
063500161111     C   32              CLEAR                   ST2CAT
063600161111    1C                   ENDIF
063700161111     C* SE CAT 4 AGGANCIO FNFVV PER AVERE DECODIFICA DEL PADRONCINO
063800161111    1C                   IF        BRVNPG=4
063900161111     C                   Z-ADD     BRVNFV        WNFV5
064000161111     C     KFVV2         CHAIN(N)  FNFVV000                           32
064100161111     C  N32              eval      stadsf=fvvdsf
064200161111    1C                   ENDIF
064300161111     C* GIRO LA DATA
064400161111     C                   MOVE      WDFV          G02INV
064500161111     C                   MOVE      *ZEROS        G02DAT
064600161111     C                   MOVEL     3             G02ERR
064700161111     C                   CALL      'XSRDA8'
064800161111     C                   PARM                    WLBDAT
064900161111     C                   MOVEL     G02DAT        ST2DFV            4 0
065000161111     c* ora
065100161111     c                   clear                   st2hms            4 0
065200161111     c                   eval      st2hms=brvhms/100
065300161111     c
065400170222     c***                eval      GCSzng=%trim(gcszng)
065500170222     c***                if        %subst(gcszng:1:1)<>' '
065600170222     c***                eval      gcszng=%trim(gcszng)+'-'
065700170222     c***                endif
065800161111     c
065900161111     c                   if        brvnpg<>4
066000170222     c                   eval      GCSultsp=
066100161111     c                             st2cat+
066200161111     c                             ' ' +%editw(st2dfv:'  /  ')+
066300161111     c                             ' '+%editw(st2hms:'  :  ')
066400161111     c                   else
066500170222     c                   eval      GCSultsp=
066600161111     c                             st2cat+
066700161111     c                             ' ' +%editw(st2dfv:'  /  ')+
066800161111     c                             ' '+stadsf
066900161111     c                   endif
067000170310     c                   else
067100170310     c                   clear                   recbrv
067200170310     c                   clear                   gcsultsp
067300170310     C                   ENDIF
067400161111     c
067500161111     c                   endsr
067600050221     C*---------------------------------------------------------------*
067700050221     C     *INZSR        BEGSR
067800050221     C*---------------------------------------------------------------*
067900050221     C     *ENTRY        PLIST
068000050221     C                   PARM                    KPJBA
068100050221     C                   MOVEL     KPJBU         FNLG87
068200050221      *
068300050322     c                   move      d87comm       gescomm           1
068400050221      *
068500050221     C                   Z-ADD     1             CODUT
068600050221     C                   CALL      'X§PARUT'
068700050221     C                   PARM                    UTEDSE
068800050221     C                   MOVEL     REC80         CNCR80
068900050221     C                   Z-ADD     0             SAVLNP            3 0
069000050221      *
069100050221     C* VERIFICO SE SI E' COLLEGATA UN SECONDO LIVELLO
069200050221     C     SimTpP        COMP      '2'                                    02
069300170221     c* la filiale da elaborare non è più dal profilo utente ma passata in DS
069400170221     C*****              Z-ADD     SIMPOU        §FLE
069500170221     c                   if        d87fgs<=*zeros
069600170221     C                   Z-ADD     SIMPOU        §FLE
069700170221     c                   else
069800170221     C                   movel     d87fgs        §FLE
069900170221     c                   endif
070000050221      *
070100050221     C                   TIME                    WHHUDT           14 0
070200050221     C                   MOVEL     WHHUDT        ORA               6 0
070300050221     C                   MOVE      WHHUDT        GMAUD8            8 0
070400050221     C* GIRO UDATE
070500050221     C                   MOVE      GMAUD8        G08DAT
070600050221     C                   MOVE      *ZEROS        G08INV
070700050221     C                   MOVEL     *BLANK        G08ERR
070800050221     C                   CALL      'XSRDA8'
070900050221     C                   PARM                    WLBDA8
071000050221     C                   Z-ADD     G08INV        AMGUD8            8 0
071100050221     C*
071200050221      *Trasformo la data dal formato AA/MM/GG al formato GG/MM/AA
071300050221     C     D87RIS        IFEQ      'R'
071400050221     C                   MOVE      D87DAT        G08INV
071500050221     C                   MOVE      *ZEROS        G08DAT
071600050221     C                   MOVEL     3             G08ERR
071700050221     C                   CALL      'XSRDA8'
071800050221     C                   PARM                    WLBDA8
071900050221     C                   Z-ADD     G08DAT        GMADRI            8 0
072000050221     C                   END
072100050221     C*
072200071011      * genero file di work in qtemp
072300071011     c                   Movel(p)  cmd2(1)       comman
072400071011     c                   eval      lenght = %len(%trim(comman))
072500071011     c                   Call      'QCMDEXC'                            97
072600071011     c                   Parm                    comman
072700071011     c                   Parm                    lenght
072800170308     c                   open      wfgcs80f
072900050221     C     D87TST        IFEQ      '1'
073000071011      * merce da portare
073100050221     C                   SETON                                        01
073200071011     c                   Movel(p)  cmd6(1)       comman
073300071011     c                   eval      lenght = %len(%trim(comman))
073400071011     c                   Call      'QCMDEXC'                            97
073500071011     c                   Parm                    comman
073600071011     c                   Parm                    lenght
073700170308     c                   open      wfgcs82l
073800050221     C                   ELSE
073900071011      * merce da prelevare
074000050221     C                   SETON                                        03
074100170224     c                   if        d87ord='Z'
074200071011     c                   Movel(p)  cmd4(1)       comman
074300170222     c                   else
074400170222     c                   Movel(p)  cmd8(1)       comman
074500170222     c                   endif
074600071011     c                   eval      lenght = %len(%trim(comman))
074700071011     c                   Call      'QCMDEXC'                            97
074800071011     c                   Parm                    comman
074900071011     c                   Parm                    lenght
075000170224     c                   if        d87ord='Z'
075100170308     c                   open      wfgcs81l
075200170222     c                   else
075300170308     c                   open      wfgcs85l
075400170222     c                   endif
075500170222     c
075600050221     C                   END
075700071011     c   97              except    errore
075800170307
075900170307     C* SOLO SPED IN FERMO DEPOSITO
076000170307     c                   if        *in01 and d87fdep='S'
076100170307     c                   seton                                        06
076200170307     c                   endif
076300050221      *
076400050221     C* DEFINIZIONE CHIAVI
076500050221     C     KGCp          KLIST
076600050221     C                   KFLD                    FLE
076700050221     C                   KFLD                    FAS               3 0
076800161111RM*  C     KFVV2         KLIST
076900161111RM*  C                   KFLD                    BRVNPG
077000161111RM*  C                   KFLD                    WNFV5
077100161111RM*  C                   KFLD                    BRVFGS
077200161111RM*  C     KARTD         KLIST
077300161111RM*  C                   KFLD                    ARTFLS
077400161111RM*  C                   KFLD                    ARTLNA
077500161111RM*  C                   KFLD                    ARTNRS
077600161111RM*  C                   KFLD                    ARTNSC
077700050221     C*
077800050221     C     KARB          KLIST
077900050412     C                   KFLD                    GCPAASa
078000050412     C                   KFLD                    GCPLNPa
078100050412     C                   KFLD                    GCPNRSa
078200050412     C                   KFLD                    GCPNSPa
078300141215      *
078400141215     C     Kart1         KLIST
078500141216     C                   KFLD                    ARbAAS
078600141216     C                   KFLD                    ARbLNP
078700141216     C                   KFLD                    ARbNRS
078800141216     C                   KFLD                    ARbNSP
078900141216     C     Kbrv          KLIST
079000141219     C                   KFLD                    artFLS
079100141216     C                   KFLD                    artLNA
079200141216     C                   KFLD                    artNRS
079300141216     C                   KFLD                    artNSC
079400071026     C*
079500071026     C     Karbf         KLIST
079600071026     C                   KFLD                    arbAAS
079700071026     C                   KFLD                    arbLNP
079800071026     C                   KFLD                    arbNRS
079900071026     C                   KFLD                    arbNSP
080000071026     C*
080100071026     C     Kblp          KLIST
080200071026     C                   KFLD                    LBLAAN
080300071026     C                   KFLD                    LBLLPN
080400071026     C                   KFLD                    LBLNRN
080500071026     C                   KFLD                    LBLNSN
080600071026      *
080700071015     C     Kart          KLIST
080800071015     C                   KFLD                    GCsAAS
080900071015     C                   KFLD                    GCsLNP
081000071015     C                   KFLD                    GCsNRS
081100071015     C                   KFLD                    GCsNSP
081200050221     C*
081300071026     C     Kblt          KLIST
081400071026     C                   KFLD                    gcsAASf
081500071026     C                   KFLD                    gcsLNPf
081600071026     C                   KFLD                    gcsNRSf
081700071026     C                   KFLD                    gcsNSPf
081800071026     C*
081900071026     C     KAR5          KLIST
082000071026     C                   KFLD                    GCPAASa
082100071026     C                   KFLD                    GCPLNPa
082200071026     C                   KFLD                    GCPNRSa
082300071026     C                   KFLD                    GCPNSPa
082400071026     C                   KFLD                    trec5             3
082500071026     c                   move      'GEN'         trec5
082600050221     C*
082700050221     C     KTAB          KLIST
082800050221     C                   KFLD                    CODUT
082900050221     C                   KFLD                    COD               2
083000050221     C                   KFLD                    KEY               8
083100050221     C*
083200071026     c                   movel     *all'_'       lin198          198
083300050221     C***--------------------------------------------------------***
083400050221     C* CARICO EVENTUALI ALTRI FILIALI GESTITE DA QUELLA COLLEGATA *
083500050221     C***--------------------------------------------------------***
083600050221     c                   clear                   trul06ds
083700050221     c                   eval      d06cod = '£6'
083800050221     c                   movel     §fle          d06key
083900050221     c                   movel     trul06ds      kpjbu
084000050221     c                   call      'TRUL06R'
084100050221     c                   parm                    kpjba
084200050221     c                   movel     kpjbu         trul06ds
084300050221     c                   movea     lin           fil£6
084400050221     C                   ENDSR
084500050221     C*****************************************************************
084600050221     Otigcp000  E            AGG0
084700050221     O                       GCPDuma
084800050221     Otigcp000  E            AGG5
084900050221     O                       GCPDLMa
085000050221     OPRTF198   E    03      TESTA          1 02
085100050221     O                       RAGUT             +  0
085200170221     O                                           25 '   '
085300050221     O                                         + 25 '* ELENCO SPEDIZIONI DA '
085400050221     O                                         +  0 'PRELEVARE DAL MAGAZZINO '
085500050221     O                                         +  0 'GIACENZE *'
085600050221     O                       §PGM               127
085700050221     O                       GMAUD8             147 '  /  /    '
085800170308     O                                          192 'PAG:'
085900170308     O                       PAGE          Z    198
086000050221     O          E    01      TESTA          1 02
086100050221     O                       RAGUT             +  0
086200170221     O                                           25 '   '
086300050221     O                                         + 25 '* ELENCO SPEDIZIONI DA '
086400050221     O                                         +  0 'PORTARE NEL MAGAZZINO '
086500050221     O                                         +  0 'GIACENZE *'
086600050221     O                       §PGM               127
086700050221     O                       GMAUD8             147 '  /  /    '
086800170308     O                                          192 'PAG:'
086900170308     O                       PAGE          Z    198
087000050221     O          E            TESTA          1
087100050221     O                       KNSIF             +  0
087200050221     O                       KNMUS             +  0
087300050221     O                       ORA                147 '  :  :  '
087400050221     O          E    03      TESTA          1
087500050221     O                                         +  0 'CON DISPOSIZIONI DI:'
087600050221     O                       DESCR             +  1
087700071026     O          E    03 05   TESTA       1
087800071026     O                                         +  0 '* Prima riga bolla O'
087900071026     O                                         +  0 'riginale '
088000071026     O                                         +  0 'Seconda riga nuova '
088100071026     O                                         +  0 'Bolla *'
088200071026     O          E    03 05   TESTA          0
088300071026     O                                         +  0 '* Prima riga bolla O'
088400071026     O                                         +  0 'riginale '
088500071026     O                                         +  0 'Seconda riga nuova '
088600071026     O                                         +  0 'Bolla *'
088700071026     O          E    03 05   TESTA          0
088800071026     O                                         +  0 '* Prima riga bolla O'
088900071026     O                                         +  0 'riginale '
089000071026     O                                         +  0 'Seconda riga nuova '
089100071026     O                                         +  0 'Bolla *'
089200050221     O          E            TESTAR      1
089300170308     O                  03                     +  0 '---------  RISTAMPA'
089400170308     O                  03                     +  1 '---------  ELENCO DEL'
089500170308     O                  03   GMADRI            +  1 '  /  /    '
089600170307     O                  06                     +  5 '- SOLO SPEDIZIONI IN'
089700170307     O                  06                     +  1 'FERMO DEPOSITO -'
089800050221     O          E            TESTAT      2
089900071026     O                                            3 'LNP'
090000161125     O                                            7 'LNA'
090100161125     O                                           13 'ANNO'
090200161125     O                                           17 'SER'
090300161125     O                                           25 'SPEDIZ.'
090400161125     O                                           35 'Mittente'
090500161125     O                                           64 'Destinatario'
090600161125     O               03                          95 'Motivo di chiusura'
090700170306     O                                          109 'FlSgn'
090800170306     O                                          125 'Segnacol.Dal-Al'
090900170306     O                                          131 'Colli'
091000170306     O                                          140 'Peso Kg'
091100170306     O                                          145 'Zona'
091200170308     O               01                         156 'F.Deposito'
091300170308     O               03                         154 'DataEsec'
091400170306     O               03                         161 'MagGIA'
091500161125     o                                          172 'Ubicazione'
091600161125     o                                          195 'Val'
091700161125     o                                          198 'Sp'
091800050221     O          E            RIGA        2
091900071026     O                       GCsLNP               3
092000161125     O                       GCsLNA               7
092100161125     O                       GCsAAS              13
092200161125     O                       GCsNRS              17
092300161125     O                       GCsNSP        Z     25
092400161125     O                       arbRSMs             50
092500170125     O                       arbRSDs             75
092600170306     O                       gcscfgd            103
092700170306     O                       gcsfls        Z    108
092800170306     O                       gcsNCD        Z    117
092900170306     O                                          118 '/'
093000170306     O                       gcsNCA        Z    125
093100170306     O                       gcsNCL        Z    131
093200170306     O                       gcspkf        2    140
093300170306     O                       gcsZNC             144
093400170308     O               01      STPfdep            153
093500170306     O               03      STPDED             154 '  /  /  '
093600170306     O               03      STPsta             159
093700170306     O                       S30_zng            192
093800170306     O                       gcsbva             194
093900070629     O          E            segn1       1
094000070629     O                       sk1(1)        z   + 30
094100070629     O                       sk1(2)        z   +  1
094200070629     O                       sk1(3)        z   +  1
094300070629     O                       sk1(4)        z   +  1
094400070629     O                       sk1(5)        z   +  1
094500070629     O                       sk1(6)        z   +  1
094600070629     O                       sk1(7)        z   +  1
094700070629     O                       sk1(8)        z   +  1
094800070629     O                       sk1(9)        z   +  1
094900070629     O                       sk1(10)       z   +  1
095000070629     O                       sk1(11)       z   +  1
095100070629     O                       sk1(12)       z   +  1
095200070629     O                       sk1(13)       z   +  1
095300070629     O                       sk1(14)       z   +  1
095400070629     O                       sk1(15)       z   +  1
095500070629     O                       sk1(16)       z   +  1
095600070629     O                       sk1(17)       z   +  1
095700070629     O                       sk1(18)       z   +  1
095800070629     O                       sk1(19)       z   +  1
095900070629     O                       sk1(20)       z   +  1
096000070629     O          E            segn2       1
096100070629     O                       sk2(1)        z   + 30
096200070629     O                       sk2(2)        z   +  1
096300070629     O                       sk2(3)        z   +  1
096400070629     O                       sk2(4)        z   +  1
096500070629     O                       sk2(5)        z   +  1
096600070629     O                       sk2(6)        z   +  1
096700070629     O                       sk2(7)        z   +  1
096800070629     O                       sk2(8)        z   +  1
096900070629     O                       sk2(9)        z   +  1
097000070629     O                       sk2(10)       z   +  1
097100070629     O                       sk2(11)       z   +  1
097200070629     O                       sk2(12)       z   +  1
097300070629     O                       sk2(13)       z   +  1
097400070629     O                       sk2(14)       z   +  1
097500070629     O                       sk2(15)       z   +  1
097600070629     O                       sk2(16)       z   +  1
097700070629     O                       sk2(17)       z   +  1
097800070629     O                       sk2(18)       z   +  1
097900070629     O                       sk2(19)       z   +  1
098000070629     O                       sk2(20)       z   +  1
098100070629     O          E            segn3       1
098200070629     O                       sk3(1)        z   + 30
098300070629     O                       sk3(2)        z   +  1
098400070629     O                       sk3(3)        z   +  1
098500070629     O                       sk3(4)        z   +  1
098600070629     O                       sk3(5)        z   +  1
098700070629     O                       sk3(6)        z   +  1
098800070629     O                       sk3(7)        z   +  1
098900070629     O                       sk3(8)        z   +  1
099000070629     O                       sk3(9)        z   +  1
099100070629     O                       sk3(10)       z   +  1
099200070629     O                       sk3(11)       z   +  1
099300070629     O                       sk3(12)       z   +  1
099400070629     O                       sk3(13)       z   +  1
099500070629     O                       sk3(14)       z   +  1
099600070629     O                       sk3(15)       z   +  1
099700070629     O                       sk3(16)       z   +  1
099800070629     O                       sk3(17)       z   +  1
099900070629     O                       sk3(18)       z   +  1
100000070629     O                       sk3(19)       z   +  1
100100070629     O                       sk3(20)       z   +  1
100200070629     O          E            segn4       1
100300070629     O                       sk4(1)        z   + 30
100400070629     O                       sk4(2)        z   +  1
100500070629     O                       sk4(3)        z   +  1
100600070629     O                       sk4(4)        z   +  1
100700070629     O                       sk4(5)        z   +  1
100800070629     O                       sk4(6)        z   +  1
100900070629     O                       sk4(7)        z   +  1
101000070629     O                       sk4(8)        z   +  1
101100070629     O                       sk4(9)        z   +  1
101200070629     O                       sk4(10)       z   +  1
101300070629     O                       sk4(11)       z   +  1
101400070629     O                       sk4(12)       z   +  1
101500070629     O                       sk4(13)       z   +  1
101600070629     O                       sk4(14)       z   +  1
101700070629     O                       sk4(15)       z   +  1
101800070629     O                       sk4(16)       z   +  1
101900070629     O                       sk4(17)       z   +  1
102000070629     O                       sk4(18)       z   +  1
102100070629     O                       sk4(19)       z   +  1
102200070629     O                       sk4(20)       z   +  1
102300070629     O          E            segn5       1
102400070629     O                       sk5(1)        z   + 30
102500070629     O                       sk5(2)        z   +  1
102600070629     O                       sk5(3)        z   +  1
102700070629     O                       sk5(4)        z   +  1
102800070629     O                       sk5(5)        z   +  1
102900070629     O                       sk5(6)        z   +  1
103000070629     O                       sk5(7)        z   +  1
103100070629     O                       sk5(8)        z   +  1
103200070629     O                       sk5(9)        z   +  1
103300070629     O                       sk5(10)       z   +  1
103400070629     O                       sk5(11)       z   +  1
103500070629     O                       sk5(12)       z   +  1
103600070629     O                       sk5(13)       z   +  1
103700070629     O                       sk5(14)       z   +  1
103800070629     O                       sk5(15)       z   +  1
103900070629     O                       sk5(16)       z   +  1
104000070629     O                       sk5(17)       z   +  1
104100070629     O                       sk5(18)       z   +  1
104200070629     O                       sk5(19)       z   +  1
104300070629     O                       sk5(20)       z   +  1
104400071026     O          E            RIGAf       1
104500071026     O                       GCsLNPf              3
104600161125     O                       GCsLNAf              7
104700161125     O                       GCsAASf             13
104800161125     O                       GCsNRSf             17
104900161125     O                       GCsNSPf       Z     25
105000161125     O                       gcsRSMfs            50
105100170125     O                       gcsRSDf             87
105200170306     O                       gcsflsf       z    108
105300170306     O                       gcsNCDf       Z    117
105400170306     O                                          118 '/'
105500170306     O                       gcsNCAf       Z    125
105600161125     O                       spuntato           198
105700071026     O          E            segn1f      1
105800071026     O                       sk1f(1)       z   + 30
105900071026     O                       sk1f(2)       z   +  1
106000071026     O                       sk1f(3)       z   +  1
106100071026     O                       sk1f(4)       z   +  1
106200071026     O                       sk1f(5)       z   +  1
106300071026     O                       sk1f(6)       z   +  1
106400071026     O                       sk1f(7)       z   +  1
106500071026     O                       sk1f(8)       z   +  1
106600071026     O                       sk1f(9)       z   +  1
106700071026     O                       sk1f(10)      z   +  1
106800071026     O                       sk1f(11)      z   +  1
106900071026     O                       sk1f(12)      z   +  1
107000071026     O                       sk1f(13)      z   +  1
107100071026     O                       sk1f(14)      z   +  1
107200071026     O                       sk1f(15)      z   +  1
107300071026     O                       sk1f(16)      z   +  1
107400071026     O                       sk1f(17)      z   +  1
107500071026     O                       sk1f(18)      z   +  1
107600071026     O                       sk1f(19)      z   +  1
107700071026     O                       sk1f(20)      z   +  1
107800071026     O          E            segn2f      1
107900071026     O                       sk2f(1)       z   + 30
108000071026     O                       sk2f(2)       z   +  1
108100071026     O                       sk2f(3)       z   +  1
108200071026     O                       sk2f(4)       z   +  1
108300071026     O                       sk2f(5)       z   +  1
108400071026     O                       sk2f(6)       z   +  1
108500071026     O                       sk2f(7)       z   +  1
108600071026     O                       sk2f(8)       z   +  1
108700071026     O                       sk2f(9)       z   +  1
108800071026     O                       sk2f(10)      z   +  1
108900071026     O                       sk2f(11)      z   +  1
109000071026     O                       sk2f(12)      z   +  1
109100071026     O                       sk2f(13)      z   +  1
109200071026     O                       sk2f(14)      z   +  1
109300071026     O                       sk2f(15)      z   +  1
109400071026     O                       sk2f(16)      z   +  1
109500071026     O                       sk2f(17)      z   +  1
109600071026     O                       sk2f(18)      z   +  1
109700071026     O                       sk2f(19)      z   +  1
109800071026     O                       sk2f(20)      z   +  1
109900071026     O          E            segn3f      1
110000071026     O                       sk3f(1)       z   + 30
110100071026     O                       sk3f(2)       z   +  1
110200071026     O                       sk3f(3)       z   +  1
110300071026     O                       sk3f(4)       z   +  1
110400071026     O                       sk3f(5)       z   +  1
110500071026     O                       sk3f(6)       z   +  1
110600071026     O                       sk3f(7)       z   +  1
110700071026     O                       sk3f(8)       z   +  1
110800071026     O                       sk3f(9)       z   +  1
110900071026     O                       sk3f(10)      z   +  1
111000071026     O                       sk3f(11)      z   +  1
111100071026     O                       sk3f(12)      z   +  1
111200071026     O                       sk3f(13)      z   +  1
111300071026     O                       sk3f(14)      z   +  1
111400071026     O                       sk3f(15)      z   +  1
111500071026     O                       sk3f(16)      z   +  1
111600071026     O                       sk3f(17)      z   +  1
111700071026     O                       sk3f(18)      z   +  1
111800071026     O                       sk3f(19)      z   +  1
111900071026     O                       sk3f(20)      z   +  1
112000071026     O          E            segn4f      1
112100071026     O                       sk4f(1)       z   + 30
112200071026     O                       sk4f(2)       z   +  1
112300071026     O                       sk4f(3)       z   +  1
112400071026     O                       sk4f(4)       z   +  1
112500071026     O                       sk4f(5)       z   +  1
112600071026     O                       sk4f(6)       z   +  1
112700071026     O                       sk4f(7)       z   +  1
112800071026     O                       sk4f(8)       z   +  1
112900071026     O                       sk4f(9)       z   +  1
113000071026     O                       sk4f(10)      z   +  1
113100071026     O                       sk4f(11)      z   +  1
113200071026     O                       sk4f(12)      z   +  1
113300071026     O                       sk4f(13)      z   +  1
113400071026     O                       sk4f(14)      z   +  1
113500071026     O                       sk4f(15)      z   +  1
113600071026     O                       sk4f(16)      z   +  1
113700071026     O                       sk4f(17)      z   +  1
113800071026     O                       sk4f(18)      z   +  1
113900071026     O                       sk4f(19)      z   +  1
114000071026     O                       sk4f(20)      z   +  1
114100071026     O          E            segn5f      1
114200071026     O                       sk5f(1)       z   + 30
114300071026     O                       sk5f(2)       z   +  1
114400071026     O                       sk5f(3)       z   +  1
114500071026     O                       sk5f(4)       z   +  1
114600071026     O                       sk5f(5)       z   +  1
114700071026     O                       sk5f(6)       z   +  1
114800071026     O                       sk5f(7)       z   +  1
114900071026     O                       sk5f(8)       z   +  1
115000071026     O                       sk5f(9)       z   +  1
115100071026     O                       sk5f(10)      z   +  1
115200071026     O                       sk5f(11)      z   +  1
115300071026     O                       sk5f(12)      z   +  1
115400071026     O                       sk5f(13)      z   +  1
115500071026     O                       sk5f(14)      z   +  1
115600071026     O                       sk5f(15)      z   +  1
115700071026     O                       sk5f(16)      z   +  1
115800071026     O                       sk5f(17)      z   +  1
115900071026     O                       sk5f(18)      z   +  1
116000071026     O                       sk5f(19)      z   +  1
116100071026     O                       sk5f(20)      z   +  1
116200071026     O          E            separa      1
116300071026     o                       lin198             198
116400071026     O          E            errRIGAf    1
116500071026     O                                         + 25 '* Attenzione non trovata '
116600071026     O                                         +  0 'spedizione figlia verifi'
116700071026     O                                         +  0 'care i dati della bolla *'
116800071011     O          E            errore         1 02
116900071011     O                       RAGUT             +  0
117000170221     O                                           25 '   '
117100071011     O                                         + 25 '* ELENCO SPEDIZIONI DA '
117200071011     O                                         +  0 'PORTARE NEL MAGAZZINO '
117300071011     O                                         +  0 'GIACENZE *'
117400071011     O                       §PGM               127
117500071011     O                       GMAUD8             147 '  /  /    '
117600071011     O                                         + 10 'PAG:'
117700071011     O                       PAGE          Z   +  1
117800071011     O          E            errore      2
117900071011     O                                         + 25 '* Attenzione errore in '
118000071011     O                                         +  0 'esecuzione programma  '
118100071011     O                                         +  0 'avvisare il CED *'
118200071011      *_________________________________________________________________________
118300071011**  CMD1
118400170308DLTF FILE(QTEMP/WFGCS80F)
118500071011**  CMD2
118600170308CRTDUPOBJ OBJ(WFGCS80F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
118700071011**  CMD3
118800170308DLTF FILE(QTEMP/WFGCS81L)
118900071011**  CMD4
119000170308CRTDUPOBJ OBJ(WFGCS81L) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
119100071011**  CMD5
119200170308DLTF FILE(QTEMP/WFGCS82L)
119300071011**  CMD6
119400170308CRTDUPOBJ OBJ(WFGCS82L) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
119500170222**  CMD7
119600170308DLTF FILE(QTEMP/WFGCS85L)
119700170222**  CMD8
119800170308CRTDUPOBJ OBJ(WFGCS85L) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)

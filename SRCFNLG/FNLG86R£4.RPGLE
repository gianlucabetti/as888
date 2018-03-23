000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300050418     ftigcp26l  uf   e           k disk    commit
000400050221     FFNARB01L  IF   E           K DISK
000500071026     FFnlbl02l  IF   E           K DISK
000600071026     FFnblp01l  IF   E           K DISK
000700050221     FFiAR501L  IF   E           K DISK
000800050221     FTABEL00F  IF   E           K DISK
000900170222     FWFGCS60F  O    E             DISK    USROPN
001000170222     FWFGCS61L  IF   E           K DISK    RENAME(WFGCS060:WFGCS1) USROPN
001100170222     FWFGCS62L  IF   E           K DISK    RENAME(WFGCS060:WFGCS2) USROPN
001200170302     FWFGCS65L  IF   E           K DISK    RENAME(WFGCS060:WFGCS5) USROPN
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
018800080115     c                   if        %found(fnarb01l)
018900050221     c                   clear                   dar5gen
019000050221     C     KAR5          CHAIN     FiAR501L
019100070629     c                   if        %found(fiar501l)
019200050221     c                   movel     ar5uni        dar5gen
019300050221     c                   end
019400141215     c                   clear                   znc               2 0
019500141216     C     Kart1         chain     Fnart01L
019600141215     c                   if        %found(fnart01l)
019700141216     C     Kbrv          setll     Fnbrv07L
019800141215     c                   do        *hival
019900141216     C     Kbrv          reade     Fnbrv07L
020000141215     c                   if        %eof(fnbrv07l)
020100141215     c                   leave
020200141215     c                   endif
020300141216     c                   if        brvdcs = 0 or brvnpg = 8
020400141215     c                   iter
020500141215     c                   else
020600141215     c                   move      brvznc        znc
020700141215     c                   leave
020800141215     c                   endif
020900141215     c                   enddo
021000141215     c                   endif
021100071026      * solo per merce da prelevare dal magazzino *IN03=on
021200071026      * se dirottamento o reso reperisce riferimenti figlia da stampare
021300071026     c                   if        *in03
021400080220     c                   seton                                        31
021500071026     c                   if        gcpdis = 'D' or gcpdis = 'R'
021600071026     C     Karb          chain     fnlbl02l                           31
021700071026     C  n31Kblp          chain     fnblp01l                           31
021800071026     c                   endif
021900071026     c                   endif
022000071011      *scrittura wf con bolle da stampare successivamente
022100071011     c                   exsr      scriviwf
022200080115     c                   endif
022300050221     c
022400071026     C* AGGIORNO DATA DI STAMPA
022500071011     C     D87RIS        IFEQ      *BLANK
022600071011     C   01              MOVE      AMGUD8        GCPDLMa
022700071011     C   01              EXCEPT    AGG5
022800071011     C   03              MOVE      AMGUD8        GCPDuma
022900071011     C   03              EXCEPT    AGG0
023000071011     c                   if        gescomm = 'S'
023100071011     c                   commit
023200071011     c                   endif
023300071011
023400071011     C                   END
023500071011     C*
023600071011     C                   END
023700071011     C*
023800071011     c                   enddo
023900071011
024000071011     c                   enddo
024100071011      * eliminazione file di work
024200170222     c                   close     wfgcs60f
024300071011      * rilegge wf e produce il tabulato
024400071011     c                   exsr      stampa
024500071011      *
024600170302     c                   if        *in03  and d87ord='Z'
024700170222     c   03              close     wfgcs61l
024800170302     c                   else
024900170302     c                   close     wfgcs65l
025000170302     c                   endif
025100170222     c   01              close     wfgcs62l
025200170224     c                   if        *in03  and d87ord='Z'
025300170222     c                   Movel(p)  cmd3(1)       comman
025400170222     c                   endif
025500170222     c                   if        *in03  and d87ord='L'
025600170222     c                   Movel(p)  cmd7(1)       comman
025700170222     c                   endif
025800071011     c   01              Movel(p)  cmd5(1)       comman
025900071011     c                   eval      lenght = %len(%trim(comman))
026000071011     c                   Call      'QCMDEXC'                            97
026100071011     c                   Parm                    comman
026200071011     c                   Parm                    lenght
026300170302     c
026400170302     c* Cancellazione file fisico da QTEMP
026500170302     c                   Movel(p)  cmd1(1)       comman
026600170302     c                   eval      lenght = %len(%trim(comman))
026700170302     c                   Call      'QCMDEXC'                            97
026800170302     c                   Parm                    comman
026900170302     c                   Parm                    lenght
027000161007
027100161007     c                   clear                   fnlgi1ds
027200161007     c                   eval      ILGI1TLA= 'C'
027300161007     c                   movel     fnlgi1ds      kpjbu
027400161007     c                   call      'FNLGI1R'
027500161007     c                   parm                    kpjba
027600161111     c
027700161111     C                   CLEAR                   DSLV53
027800161111     C                   MOVEL     'C'           D53TLA
027900161111     C                   CALL      'FNLV53R'
028000161111     C                   PARM                    DSLV53
028100161111     C*
028200071011     c                   seton                                        lr
028300071011      *_________________________________________________________
028400071011     c     stampa        begsr
028500071011      *_________________________________________________________
028600071011      * legge la v.l. opportuna x l'ordinamento a seconda che sia
028700071011      * merce da prelevare o da portare a magazzino
028800170224     c   03              if        d87ord='Z'
028900170222     c     *loval        setll     wfgcs61l
029000170222     c                   endif
029100170222     c   03              if        d87ord='L'
029200170302     c     *loval        setll     wfgcs65l
029300170222     c                   endif
029400170222     c   01*loval        setll     wfgcs62l
029500170222
029600071011     c                   do        *hival
029700170222
029800170224     c   03              if        d87ord='Z'
029900170222     c                   read      wfgcs61l                               98
030000170222     c                   endif
030100170222     c   03              if        d87ord='L'
030200170302     c                   read      wfgcs65l                               98
030300170222     c                   endif
030400170222     c   01              read      wfgcs62l                               98
030500170222
030600071011     c   98              leave
030700071011      * segnacolli non sequenziali prelevo i numeri da dettaglio colli
030800070629     c                   clear                   skt
030900071011     c                   if        gcsfns = 'S'
031000071015     C     Kart          setll     Fnart01L
031100070629     c                   z-add     0             ncl               3 0
031200071011     c                   do        *hival
031300071015     C     Kart          reade     Fnart01L
031400070629     c                   if        %eof(fnart01l) or ncl = 100
031500070629     c                   leave
031600070629     c                   endif
031700070629     c                   add       1             ncl
031800070629     c                   z-add     artnsc        skt(ncl)
031900070629     c                   enddo
032000070629     c                   endif
032100070629     c
032200071026      * bolla figlia
032300071026     c                   clear                   sktf
032400140728     c                   clear                   spuntato          8
032500140728     c                   clear                   spu               3 0
032600140728     c                   clear                   nospu             3 0
032700140718     c                   if        *in03
032800071026     C     Kblt          setll     Fnblt01L
032900071026     c                   z-add     0             ncl               3 0
033000071026     c                   do        *hival
033100071026     C     Kblt          reade     Fnblt01L
033200071026     c                   if        %eof(fnblt01l) or ncl = 100
033300140728     c                   select
033400140728     c                   when      spu >= 1 and nospu = 0
033500140728     c                   eval      spuntato = 'Totale'
033600140728     c                   when      spu >= 1 and nospu >= 1
033700140728     c                   eval      spuntato = 'Parziale'
033800140728     c                   other
033900140728     c                   clear                   spuntato
034000140728     c                   endsl
034100071026     c                   leave
034200071026     c                   endif
034300140718     c* se presente la data entrata vuol dire
034400140718     c* che è già stato etichettato il pacco con la nuova
034500140718     c* etichetta della figlia
034600140728     c                   if        bltdse <> 0
034700140728     c                   add       1             spu
034800140728     c                   else
034900140728     c                   add       1             nospu
035000140728     c                   end
035100140718     c*
035200140718     c                   if        gcsfnsf = 'S'
035300071026     c                   add       1             ncl
035400071026     c                   z-add     bltnsc        sktf(ncl)
035500071026     c                   endif
035600140718     c                   enddo
035700140718     c                   endif
035800071026     c
035900050221      *
036000071011     C   03GCsDIS        IFNE      SAVDIS
036100050221     C* PRELEVO DESCRIZIONE TIPO DISPOSIZIONE
036200050221     C                   MOVEL     '2D'          COD
036300050221     C                   MOVEL     *BLANKS       KEY
036400071011     C                   MOVEL     GCsDIS        KEY
036500050221     C     KTAB          CHAIN     TABEL                              50
036600050221     C  N50              MOVEL     TBLUNI        DESCR            20
036700050221     C   50              MOVEL     *ALL'?'       DESCR
036800071026     c                   setoff                                       05
036900071026     c                   if        gcsdis  = 'D' or gcsdis = 'R'
037000071026     c                   seton                                        05
037100071026     c                   endif
037200050221     C                   EXCEPT    TESTA
037300050221     C     D87RIS        IFEQ      'R'
037400050221     C                   EXCEPT    TESTAR
037500050221     C                   END
037600050221     C                   EXCEPT    TESTAT
037700050221     C                   SETON                                        99
037800071011     C                   MOVE      GCsDIS        SAVDIS            1
037900071011     C                   END
038000050221      *
038100050221     C  N99
038200050221     COR OF              DO
038300050221     C                   EXCEPT    TESTA
038400050221     C     D87RIS        IFEQ      'R'
038500050221     C                   EXCEPT    TESTAR
038600050221     C                   END
038700050221     C                   EXCEPT    TESTAT
038800050221     C                   END
038900050221     C  N99              SETON                                        99
039000050221      *
039100140718     c                   movel     gcsrsm        arbrsms          23
039200140718     c                   movel     gcsrsd        arbrsds          23
039300170222     c***                movel     gcszng        s30_zng          30
039400170222
039500170222     c                   clear                   s30_zng          30
039600170222     c                   if        gcszng<>*blanks
039700170222     c                   eval      s30_zng=%trim(gcszng)+'-'+gcsultsp
039800170222     c                   else
039900170222     c                   eval      s30_zng=gcsultsp
040000170222     c                   endif
040100170222     c
040200050221     C     *IN03         IFEQ      '1'
040300071011     C                   Z-ADD     GCsdeda       G08INV
040400050221     C                   MOVE      *ZEROS        G08DAT
040500050221     C                   MOVEL     '3'           G08ERR
040600050221     C                   CALL      'XSRDA8'
040700050221     C                   PARM                    WLBDA8
040800050221     C                   Z-ADD     G08DAT        STPDED            8 0
040900070522     C                   MOVEL     '2C'          COD
041000070522     C                   MOVEL     *BLANKS       KEY
041100071011     C                   MOVEL     GCscfg        KEY
041200070522     C     KTAB          CHAIN     TABEL                              50
041300071011     C  N50              MOVEL     TBLUNI        gcscfgd          29
041400071011     C   50              MOVEL     *ALL'?'       gcscfgd
041500070522     C                   END
041600050221     C                   EXCEPT    RIGA
041700070629      * stampa dettaglio dei segnacolli se non sequenziali
041800070629     c                   if        skt(1) > 0
041900070629     c                   except    segn1
042000070629     c                   endif
042100070629     c                   if        skt(21) > 0
042200070629     c                   except    segn2
042300070629     c                   endif
042400070629     c                   if        skt(41) > 0
042500070629     c                   except    segn3
042600070629     c                   endif
042700070629     c                   if        skt(61) > 0
042800070629     c                   except    segn4
042900070629     c                   endif
043000070629     c                   if        skt(81) > 0
043100070629     c                   except    segn5
043200070629     c                   endif
043300071026      * solo per merce da prelevare dal magazzino *IN03=ON
043400071026      * seconda riga dettaglio bolla figlia
043500071026      * testa se il campo spedizione è pieno per capire se è stato
043600071026      * agganciato il legame altrimenti stampa anomalia
043700071026     c   03              if        gcsnspf > 0
043800140718     c                   movel     gcsRSMf       gcsRSMfs         23
043900071026     C                   EXCEPT    RIGAf
044000071026      * stampa dettaglio dei segnacolli se non sequenziali
044100071026     c                   if        sktf(1) > 0
044200071026     c                   except    segn1f
044300071026     c                   endif
044400071026     c                   if        sktf(21) > 0
044500071026     c                   except    segn2f
044600071026     c                   endif
044700071026     c                   if        sktf(41) > 0
044800071026     c                   except    segn3f
044900071026     c                   endif
045000071026     c                   if        sktf(61) > 0
045100071026     c                   except    segn4f
045200071026     c                   endif
045300071026     c                   if        sktf(81) > 0
045400071026     c                   except    segn5f
045500071026     c                   endif
045600071026      * separazione
045700071026     C                   EXCEPT    separa
045800071026     c                   else
045900071026     c                   if        gcsdis = 'D' or gcsdis = 'R'
046000071026     C                   EXCEPT    errRIGAf
046100071026     c                   endif
046200071026     c                   endif
046300071011      *
046400071011     c                   enddo
046500071011     c                   endsr
046600071011      *_________________________________________________________
046700071011     c     scriviwf      begsr
046800071011      *_________________________________________________________
046900170222     c                   clear                   wfgcs060
047000071011     c                   z-add     arbaas        GCSaas
047100071011     c                   z-add     arblnp        GCSlnp
047200071011     c                   z-add     arbnrs        GCSnrs
047300071011     c                   z-add     arbnsp        GCSnsp
047400071011     c                   z-add     arblna        GCSlna
047500071011     c                   move      arbrsm        GCSrsm
047600071011     c                   move      arbrsd        GCSrsd
047700141215     c                   if        arbznc <> znc and znc <> 0
047800141215     c                   z-add     znc           gcsznc
047900141215     c                   else
048000071011     c                   z-add     arbznc        GCSznc
048100141215     c                   endif
048200071011     c                   z-add     gcpfgc        GCSfgc
048300071011     c                   z-add     gcpngc        GCSngc
048400071011     c                   z-add     gcpdeda       GCSdeda
048500071011     c                   move      gcpdis        GCSdis
048600071011     c                   move      gcpcfg        GCScfg
048700170126     c
048800170126     c* se non è stato portato in mag giacenza, non stampo
048900170126     c*  l'ubicazine, altrimenti si
049000170307     c   03              if        gcpdlma>0
049100071011     c                   movel     §ar5zng       GCSzng
049200170126     c                   endif
049300170307     c  n03              movel     §ar5zng       GCSzng
049400161111     c* se la bolla ha la data merce a mag giacenze, significa che
049500161111     c*  probabilmente non e' stata portata in giacenza
049600161111     c*  quindi al posto della ubicazione stampo la data e ora ultima spunta
049700161111     c*
049800170126     c**                 if        gcpdlma=0
049900161111     C     Kart1         chain     Fnart01L
050000161111     c                   if        %found(fnart01l)
050100161111     c                   exsr      Ricspu
050200161111     c                   endif
050300170126     c***                endif
050400161111     c
050500071011     c                   move      §ar5bva       GCSbva
050600071026     c                   z-add     arbfls        GCSfls
050700071011     c                   z-add     arbncd        GCSncd
050800071011     c                   z-add     arbnca        GCSnca
050900071011     c                   move      arbfns        GCSfns
051000071011     c                   z-add     arbncl        GCSncl
051100071011     c                   z-add     arbpkf        GCSpkf
051200071011     c                   z-add     arbvlf        GCSvlf
051300071026      * solo per merce da prelevare da magazzino *IN03=on
051400071026      *figlia se non trova legame o blp non scrive nulla
051500071026     c   03              if        not *in31
051600071026     c                   z-add     blpaas        GCSaasf
051700071026     c                   z-add     blplnp        GCSlnpf
051800071026     c                   z-add     blpnrs        GCSnrsf
051900071026     c                   z-add     blpnsp        GCSnspf
052000071026     c                   z-add     blplna        GCSlnaf
052100071026     c                   move      blprsm        GCSrsmf
052200071026     c                   move      blprsd        GCSrsdf
052300071026     c                   z-add     blpznc        GCSzncf
052400071026     c                   z-add     blpfls        GCSflsf
052500071026     c                   z-add     blpncd        GCSncdf
052600071026     c                   z-add     blpnca        GCSncaf
052700071026     c                   move      blpfns        GCSfnsf
052800071026     c                   z-add     blpncl        GCSnclf
052900071026     c                   z-add     blppkf        GCSpkff
053000071026     c                   z-add     blpvlf        GCSvlff
053100071026     c                   endif
053200170222     c                   write     wfgcs060
053300071011     c                   endsr
053400161111     C**************************************************************************
053500161111     C* cerco spunta del collo in qualsiasi categoria
053600161111     C**************************************************************************
053700161111     C     RICSPU        BEGSR
053800161111     C                   CLEAR                   Wimm
053900161111     C                   CLEAR                   WSAVB
054000161111     C                   CLEAR                   WDFV
054100161111     C                   CLEAR                   WTROV
054200161111     C                   CLEAR                   WSPG
054300161111     C                   CLEAR                   stadsf           10
054400161111RM*  C     kbrv          CHAIN     FNBRV07l                           33
054500161111RM*  C*
054600161111RM* 1C     *IN33         DOWEQ     *OFF
054700161111     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
054800161111     c                   clear                   dslv53
054900161111     C                   z-add     brvnFV        D53NFV
055000161111     C                   MOVEL     brvNPG        D53NPG
055100161111     C                   MOVEL     brvFGS        D53FGS
055200161111     C****               MOVEL     brvfle        D53flf
055300161111     C                   CALL      'FNLV53R'
055400161111     C                   PARM                    DSLV53
055500161111     C* ERRORE
055600161111    2C     D53ERR        IFeq      ' '
055700161111     c                   movel     brvdfs        brvimm           14 0
055800161111     c                   move      brvhms        brvimm
055900161111    3c                   select
056000161111     c* Se maggiore memorizzo
056100161111     c                   when      d53dfv>wdfv
056200161111     C                   MOVEL     RECBRV        WSAVB
056300161111     C                   MOVEL     brvimm        Wimm             14 0
056400161111     C                   MOVEL     d53DFV        WDFV
056500161111     C                   MOVEL     d53spg        WSPG
056600161111     C                   MOVEL     'S'           WTROV             1
056700161111     c
056800161111     c* Se uguale tengo la data ora immissione + alta
056900161111     c                   when      d53dfv=wdfv  and brvimm>wimm
057000161111     C                   MOVEL     brvimm        Wimm             14 0
057100161111     C                   MOVEL     RECBRV        WSAVB
057200161111     C                   MOVEL     d53DFV        WDFV
057300161111     C                   MOVEL     d53spg        WSPG
057400161111     C                   MOVEL     'S'           WTROV             1
057500161111     C                   eval      wimm=brvimm
057600161111     c* Se stessa data e ora immissione, tengo cat 3 o 4
057700161111     c                   when      d53dfv=wdfv and brvimm=wimm and brvnpg<>2
057800161111     C                   MOVEL     brvimm        Wimm             14 0
057900161111     C                   MOVEL     RECBRV        WSAVB
058000161111     C                   MOVEL     d53DFV        WDFV
058100161111     C                   MOVEL     d53spg        WSPG
058200161111     C                   MOVEL     'S'           WTROV             1
058300161111     C                   eval      wimm=brvimm
058400161111    3c                   endsl
058500161111    2c                   endif
058600161111     c
058700161111RM*  C     Kartd         reade     FNBRV07l                               33
058800161111    1c                   enddo
058900161111     c
059000161111     C     WTROV         IFEQ      'S'
059100161111     C                   MOVEL     WSAVB         RECBRV
059200161111     C                   ENDIF
059300161111     c
059400161111     c* Memorizzo nel campo ubicazione
059500161111     C* DECODIFICO LA CATEGORIA SPUNTA
059600161111    1C     BRVNPG        IFEQ      3
059700161111     C                   MOVEL     '7B'          COD
059800161111     C                   MOVEL(P)  wspg          KEY
059900161111     C     KTAB          CHAIN     TABEL                              32
060000161111     C  N32              MOVEL     TBLUNI        ds7b
060100161111     C   32              CLEAR                   ds7b
060200161111     C                   MOVEL     §7bdesb       ST2CAT            3
060300161111   X1C                   ELSE
060400161111     C                   MOVEL     '7N'          COD
060500161111     C                   MOVEL(P)  BRVNPG        KEY
060600161111     C     KTAB          CHAIN     TABEL                              32
060700161111     C  N32              MOVEL     TBLUNI        ST2CAT
060800161111     C   32              CLEAR                   ST2CAT
060900161111    1C                   ENDIF
061000161111     C* SE CAT 4 AGGANCIO FNFVV PER AVERE DECODIFICA DEL PADRONCINO
061100161111    1C                   IF        BRVNPG=4
061200161111     C                   Z-ADD     BRVNFV        WNFV5
061300161111     C     KFVV2         CHAIN(N)  FNFVV000                           32
061400161111     C  N32              eval      stadsf=fvvdsf
061500161111    1C                   ENDIF
061600161111     C* GIRO LA DATA
061700161111     C                   MOVE      WDFV          G02INV
061800161111     C                   MOVE      *ZEROS        G02DAT
061900161111     C                   MOVEL     3             G02ERR
062000161111     C                   CALL      'XSRDA8'
062100161111     C                   PARM                    WLBDAT
062200161111     C                   MOVEL     G02DAT        ST2DFV            4 0
062300161111     c* ora
062400161111     c                   clear                   st2hms            4 0
062500161111     c                   eval      st2hms=brvhms/100
062600161111     c
062700170222     c***                eval      GCSzng=%trim(gcszng)
062800170222     c***                if        %subst(gcszng:1:1)<>' '
062900170222     c***                eval      gcszng=%trim(gcszng)+'-'
063000170222     c***                endif
063100161111     c
063200161111     c                   if        brvnpg<>4
063300170222     c                   eval      GCSultsp=
063400161111     c                             st2cat+
063500161111     c                             ' ' +%editw(st2dfv:'  /  ')+
063600161111     c                             ' '+%editw(st2hms:'  :  ')
063700161111     c                   else
063800170222     c                   eval      GCSultsp=
063900161111     c                             st2cat+
064000161111     c                             ' ' +%editw(st2dfv:'  /  ')+
064100161111     c                             ' '+stadsf
064200161111     c                   endif
064300161111     c
064400161111     c
064500161111     c                   endsr
064600050221     C*---------------------------------------------------------------*
064700050221     C     *INZSR        BEGSR
064800050221     C*---------------------------------------------------------------*
064900050221     C     *ENTRY        PLIST
065000050221     C                   PARM                    KPJBA
065100050221     C                   MOVEL     KPJBU         FNLG87
065200050221      *
065300050322     c                   move      d87comm       gescomm           1
065400050221      *
065500050221     C                   Z-ADD     1             CODUT
065600050221     C                   CALL      'X§PARUT'
065700050221     C                   PARM                    UTEDSE
065800050221     C                   MOVEL     REC80         CNCR80
065900050221     C                   Z-ADD     0             SAVLNP            3 0
066000050221      *
066100050221     C* VERIFICO SE SI E' COLLEGATA UN SECONDO LIVELLO
066200050221     C     SimTpP        COMP      '2'                                    02
066300170221     c* la filiale da elaborare non è più dal profilo utente ma passata in DS
066400170221     C*****              Z-ADD     SIMPOU        §FLE
066500170221     c                   if        d87fgs<=*zeros
066600170221     C                   Z-ADD     SIMPOU        §FLE
066700170221     c                   else
066800170221     C                   movel     d87fgs        §FLE
066900170221     c                   endif
067000050221      *
067100050221     C                   TIME                    WHHUDT           14 0
067200050221     C                   MOVEL     WHHUDT        ORA               6 0
067300050221     C                   MOVE      WHHUDT        GMAUD8            8 0
067400050221     C* GIRO UDATE
067500050221     C                   MOVE      GMAUD8        G08DAT
067600050221     C                   MOVE      *ZEROS        G08INV
067700050221     C                   MOVEL     *BLANK        G08ERR
067800050221     C                   CALL      'XSRDA8'
067900050221     C                   PARM                    WLBDA8
068000050221     C                   Z-ADD     G08INV        AMGUD8            8 0
068100050221     C*
068200050221      *Trasformo la data dal formato AA/MM/GG al formato GG/MM/AA
068300050221     C     D87RIS        IFEQ      'R'
068400050221     C                   MOVE      D87DAT        G08INV
068500050221     C                   MOVE      *ZEROS        G08DAT
068600050221     C                   MOVEL     3             G08ERR
068700050221     C                   CALL      'XSRDA8'
068800050221     C                   PARM                    WLBDA8
068900050221     C                   Z-ADD     G08DAT        GMADRI            8 0
069000050221     C                   END
069100050221     C*
069200071011      * genero file di work in qtemp
069300071011     c                   Movel(p)  cmd2(1)       comman
069400071011     c                   eval      lenght = %len(%trim(comman))
069500071011     c                   Call      'QCMDEXC'                            97
069600071011     c                   Parm                    comman
069700071011     c                   Parm                    lenght
069800170222     c                   open      wfgcs60f
069900050221     C     D87TST        IFEQ      '1'
070000071011      * merce da portare
070100050221     C                   SETON                                        01
070200071011     c                   Movel(p)  cmd6(1)       comman
070300071011     c                   eval      lenght = %len(%trim(comman))
070400071011     c                   Call      'QCMDEXC'                            97
070500071011     c                   Parm                    comman
070600071011     c                   Parm                    lenght
070700170222     c                   open      wfgcs62l
070800050221     C                   ELSE
070900071011      * merce da prelevare
071000050221     C                   SETON                                        03
071100170224     c                   if        d87ord='Z'
071200071011     c                   Movel(p)  cmd4(1)       comman
071300170222     c                   else
071400170222     c                   Movel(p)  cmd8(1)       comman
071500170222     c                   endif
071600071011     c                   eval      lenght = %len(%trim(comman))
071700071011     c                   Call      'QCMDEXC'                            97
071800071011     c                   Parm                    comman
071900071011     c                   Parm                    lenght
072000170224     c                   if        d87ord='Z'
072100170222     c                   open      wfgcs61l
072200170222     c                   else
072300170302     c                   open      wfgcs65l
072400170222     c                   endif
072500170222     c
072600050221     C                   END
072700071011     c   97              except    errore
072800050221      *
072900050221     C* DEFINIZIONE CHIAVI
073000050221     C     KGCp          KLIST
073100050221     C                   KFLD                    FLE
073200050221     C                   KFLD                    FAS               3 0
073300161111RM*  C     KFVV2         KLIST
073400161111RM*  C                   KFLD                    BRVNPG
073500161111RM*  C                   KFLD                    WNFV5
073600161111RM*  C                   KFLD                    BRVFGS
073700161111RM*  C     KARTD         KLIST
073800161111RM*  C                   KFLD                    ARTFLS
073900161111RM*  C                   KFLD                    ARTLNA
074000161111RM*  C                   KFLD                    ARTNRS
074100161111RM*  C                   KFLD                    ARTNSC
074200050221     C*
074300050221     C     KARB          KLIST
074400050412     C                   KFLD                    GCPAASa
074500050412     C                   KFLD                    GCPLNPa
074600050412     C                   KFLD                    GCPNRSa
074700050412     C                   KFLD                    GCPNSPa
074800141215      *
074900141215     C     Kart1         KLIST
075000141216     C                   KFLD                    ARbAAS
075100141216     C                   KFLD                    ARbLNP
075200141216     C                   KFLD                    ARbNRS
075300141216     C                   KFLD                    ARbNSP
075400141216     C     Kbrv          KLIST
075500141219     C                   KFLD                    artFLS
075600141216     C                   KFLD                    artLNA
075700141216     C                   KFLD                    artNRS
075800141216     C                   KFLD                    artNSC
075900071026     C*
076000071026     C     Karbf         KLIST
076100071026     C                   KFLD                    arbAAS
076200071026     C                   KFLD                    arbLNP
076300071026     C                   KFLD                    arbNRS
076400071026     C                   KFLD                    arbNSP
076500071026     C*
076600071026     C     Kblp          KLIST
076700071026     C                   KFLD                    LBLAAN
076800071026     C                   KFLD                    LBLLPN
076900071026     C                   KFLD                    LBLNRN
077000071026     C                   KFLD                    LBLNSN
077100071026      *
077200071015     C     Kart          KLIST
077300071015     C                   KFLD                    GCsAAS
077400071015     C                   KFLD                    GCsLNP
077500071015     C                   KFLD                    GCsNRS
077600071015     C                   KFLD                    GCsNSP
077700050221     C*
077800071026     C     Kblt          KLIST
077900071026     C                   KFLD                    gcsAASf
078000071026     C                   KFLD                    gcsLNPf
078100071026     C                   KFLD                    gcsNRSf
078200071026     C                   KFLD                    gcsNSPf
078300071026     C*
078400071026     C     KAR5          KLIST
078500071026     C                   KFLD                    GCPAASa
078600071026     C                   KFLD                    GCPLNPa
078700071026     C                   KFLD                    GCPNRSa
078800071026     C                   KFLD                    GCPNSPa
078900071026     C                   KFLD                    trec5             3
079000071026     c                   move      'GEN'         trec5
079100050221     C*
079200050221     C     KTAB          KLIST
079300050221     C                   KFLD                    CODUT
079400050221     C                   KFLD                    COD               2
079500050221     C                   KFLD                    KEY               8
079600050221     C*
079700071026     c                   movel     *all'_'       lin198          198
079800050221     C***--------------------------------------------------------***
079900050221     C* CARICO EVENTUALI ALTRI FILIALI GESTITE DA QUELLA COLLEGATA *
080000050221     C***--------------------------------------------------------***
080100050221     c                   clear                   trul06ds
080200050221     c                   eval      d06cod = '£6'
080300050221     c                   movel     §fle          d06key
080400050221     c                   movel     trul06ds      kpjbu
080500050221     c                   call      'TRUL06R'
080600050221     c                   parm                    kpjba
080700050221     c                   movel     kpjbu         trul06ds
080800050221     c                   movea     lin           fil£6
080900050221     C                   ENDSR
081000050221     C*****************************************************************
081100050221     Otigcp000  E            AGG0
081200050221     O                       GCPDuma
081300050221     Otigcp000  E            AGG5
081400050221     O                       GCPDLMa
081500050221     OPRTF198   E    03      TESTA          1 02
081600050221     O                       RAGUT             +  0
081700170221     O                                           25 '   '
081800050221     O                                         + 25 '* ELENCO SPEDIZIONI DA '
081900050221     O                                         +  0 'PRELEVARE DAL MAGAZZINO '
082000050221     O                                         +  0 'GIACENZE *'
082100050221     O                       §PGM               127
082200050221     O                       GMAUD8             147 '  /  /    '
082300050221     O                                         + 10 'PAG:'
082400050221     O                       PAGE          Z   +  1
082500050221     O          E    01      TESTA          1 02
082600050221     O                       RAGUT             +  0
082700170221     O                                           25 '   '
082800050221     O                                         + 25 '* ELENCO SPEDIZIONI DA '
082900050221     O                                         +  0 'PORTARE NEL MAGAZZINO '
083000050221     O                                         +  0 'GIACENZE *'
083100050221     O                       §PGM               127
083200050221     O                       GMAUD8             147 '  /  /    '
083300050221     O                                         + 10 'PAG:'
083400050221     O                       PAGE          Z   +  1
083500050221     O          E            TESTA          1
083600050221     O                       KNSIF             +  0
083700050221     O                       KNMUS             +  0
083800050221     O                       ORA                147 '  :  :  '
083900050221     O          E    03      TESTA          1
084000050221     O                                         +  0 'CON DISPOSIZIONI DI:'
084100050221     O                       DESCR             +  1
084200071026     O          E    03 05   TESTA       1
084300071026     O                                         +  0 '* Prima riga bolla O'
084400071026     O                                         +  0 'riginale '
084500071026     O                                         +  0 'Seconda riga nuova '
084600071026     O                                         +  0 'Bolla *'
084700071026     O          E    03 05   TESTA          0
084800071026     O                                         +  0 '* Prima riga bolla O'
084900071026     O                                         +  0 'riginale '
085000071026     O                                         +  0 'Seconda riga nuova '
085100071026     O                                         +  0 'Bolla *'
085200071026     O          E    03 05   TESTA          0
085300071026     O                                         +  0 '* Prima riga bolla O'
085400071026     O                                         +  0 'riginale '
085500071026     O                                         +  0 'Seconda riga nuova '
085600071026     O                                         +  0 'Bolla *'
085700050221     O          E            TESTAR      1
085800050221     O                                         +  0 '---------  RISTAMPA'
085900050221     O                                         +  1 '---------  ELENCO DEL'
086000050221     O                       GMADRI            +  1 '  /  /    '
086100050221     O          E            TESTAT      2
086200071026     O                                            3 'LNP'
086300161125     O                                            7 'LNA'
086400161125     O                                           13 'ANNO'
086500161125     O                                           17 'SER'
086600161125     O                                           25 'SPEDIZ.'
086700161125     O                                           35 'Mittente'
086800161125     O                                           64 'Destinatario'
086900161125     O               03                          95 'Motivo di chiusura'
087000161125     O                                          112 'FlSgn'
087100161125     O                                          128 'Segnacol.Dal-Al'
087200161125     O                                          134 'Colli'
087300161125     O                                          144 'Peso Kg'
087400161125     O                                          149 'Zona'
087500161125     O               03                         160 'Data esec.'
087600161125     o                                          172 'Ubicazione'
087700161125     o                                          195 'Val'
087800161125     o                                          198 'Sp'
087900050221     O          E            RIGA        2
088000071026     O                       GCsLNP               3
088100161125     O                       GCsLNA               7
088200161125     O                       GCsAAS              13
088300161125     O                       GCsNRS              17
088400161125     O                       GCsNSP        Z     25
088500161125     O                       arbRSMs             50
088600170125     O                       arbRSDs             75
088700161125     O                       gcscfgd            106
088800161125     O                       gcsfls        Z    111
088900161125     O                       gcsNCD        Z    120
089000161125     O                                          121 '/'
089100161125     O                       gcsNCA        Z    128
089200161125     O                       gcsNCL        Z    134
089300161125     O                       gcspkf        2    144
089400161125     O                       gcsZNC             148
089500161125     O               03      STPDED             160 '  /  /    '
089600161125     O                       S30_zng            192
089700161125     O                       gcsbva             194
089800070629     O          E            segn1       1
089900070629     O                       sk1(1)        z   + 30
090000070629     O                       sk1(2)        z   +  1
090100070629     O                       sk1(3)        z   +  1
090200070629     O                       sk1(4)        z   +  1
090300070629     O                       sk1(5)        z   +  1
090400070629     O                       sk1(6)        z   +  1
090500070629     O                       sk1(7)        z   +  1
090600070629     O                       sk1(8)        z   +  1
090700070629     O                       sk1(9)        z   +  1
090800070629     O                       sk1(10)       z   +  1
090900070629     O                       sk1(11)       z   +  1
091000070629     O                       sk1(12)       z   +  1
091100070629     O                       sk1(13)       z   +  1
091200070629     O                       sk1(14)       z   +  1
091300070629     O                       sk1(15)       z   +  1
091400070629     O                       sk1(16)       z   +  1
091500070629     O                       sk1(17)       z   +  1
091600070629     O                       sk1(18)       z   +  1
091700070629     O                       sk1(19)       z   +  1
091800070629     O                       sk1(20)       z   +  1
091900070629     O          E            segn2       1
092000070629     O                       sk2(1)        z   + 30
092100070629     O                       sk2(2)        z   +  1
092200070629     O                       sk2(3)        z   +  1
092300070629     O                       sk2(4)        z   +  1
092400070629     O                       sk2(5)        z   +  1
092500070629     O                       sk2(6)        z   +  1
092600070629     O                       sk2(7)        z   +  1
092700070629     O                       sk2(8)        z   +  1
092800070629     O                       sk2(9)        z   +  1
092900070629     O                       sk2(10)       z   +  1
093000070629     O                       sk2(11)       z   +  1
093100070629     O                       sk2(12)       z   +  1
093200070629     O                       sk2(13)       z   +  1
093300070629     O                       sk2(14)       z   +  1
093400070629     O                       sk2(15)       z   +  1
093500070629     O                       sk2(16)       z   +  1
093600070629     O                       sk2(17)       z   +  1
093700070629     O                       sk2(18)       z   +  1
093800070629     O                       sk2(19)       z   +  1
093900070629     O                       sk2(20)       z   +  1
094000070629     O          E            segn3       1
094100070629     O                       sk3(1)        z   + 30
094200070629     O                       sk3(2)        z   +  1
094300070629     O                       sk3(3)        z   +  1
094400070629     O                       sk3(4)        z   +  1
094500070629     O                       sk3(5)        z   +  1
094600070629     O                       sk3(6)        z   +  1
094700070629     O                       sk3(7)        z   +  1
094800070629     O                       sk3(8)        z   +  1
094900070629     O                       sk3(9)        z   +  1
095000070629     O                       sk3(10)       z   +  1
095100070629     O                       sk3(11)       z   +  1
095200070629     O                       sk3(12)       z   +  1
095300070629     O                       sk3(13)       z   +  1
095400070629     O                       sk3(14)       z   +  1
095500070629     O                       sk3(15)       z   +  1
095600070629     O                       sk3(16)       z   +  1
095700070629     O                       sk3(17)       z   +  1
095800070629     O                       sk3(18)       z   +  1
095900070629     O                       sk3(19)       z   +  1
096000070629     O                       sk3(20)       z   +  1
096100070629     O          E            segn4       1
096200070629     O                       sk4(1)        z   + 30
096300070629     O                       sk4(2)        z   +  1
096400070629     O                       sk4(3)        z   +  1
096500070629     O                       sk4(4)        z   +  1
096600070629     O                       sk4(5)        z   +  1
096700070629     O                       sk4(6)        z   +  1
096800070629     O                       sk4(7)        z   +  1
096900070629     O                       sk4(8)        z   +  1
097000070629     O                       sk4(9)        z   +  1
097100070629     O                       sk4(10)       z   +  1
097200070629     O                       sk4(11)       z   +  1
097300070629     O                       sk4(12)       z   +  1
097400070629     O                       sk4(13)       z   +  1
097500070629     O                       sk4(14)       z   +  1
097600070629     O                       sk4(15)       z   +  1
097700070629     O                       sk4(16)       z   +  1
097800070629     O                       sk4(17)       z   +  1
097900070629     O                       sk4(18)       z   +  1
098000070629     O                       sk4(19)       z   +  1
098100070629     O                       sk4(20)       z   +  1
098200070629     O          E            segn5       1
098300070629     O                       sk5(1)        z   + 30
098400070629     O                       sk5(2)        z   +  1
098500070629     O                       sk5(3)        z   +  1
098600070629     O                       sk5(4)        z   +  1
098700070629     O                       sk5(5)        z   +  1
098800070629     O                       sk5(6)        z   +  1
098900070629     O                       sk5(7)        z   +  1
099000070629     O                       sk5(8)        z   +  1
099100070629     O                       sk5(9)        z   +  1
099200070629     O                       sk5(10)       z   +  1
099300070629     O                       sk5(11)       z   +  1
099400070629     O                       sk5(12)       z   +  1
099500070629     O                       sk5(13)       z   +  1
099600070629     O                       sk5(14)       z   +  1
099700070629     O                       sk5(15)       z   +  1
099800070629     O                       sk5(16)       z   +  1
099900070629     O                       sk5(17)       z   +  1
100000070629     O                       sk5(18)       z   +  1
100100070629     O                       sk5(19)       z   +  1
100200070629     O                       sk5(20)       z   +  1
100300071026     O          E            RIGAf       1
100400071026     O                       GCsLNPf              3
100500161125     O                       GCsLNAf              7
100600161125     O                       GCsAASf             13
100700161125     O                       GCsNRSf             17
100800161125     O                       GCsNSPf       Z     25
100900161125     O                       gcsRSMfs            50
101000170125     O                       gcsRSDf             87
101100161125     O                       gcsflsf       z    111
101200161125     O                       gcsNCDf       Z    120
101300161125     O                                          121 '/'
101400161125     O                       gcsNCAf       Z    128
101500161125     O                       spuntato           198
101600071026     O          E            segn1f      1
101700071026     O                       sk1f(1)       z   + 30
101800071026     O                       sk1f(2)       z   +  1
101900071026     O                       sk1f(3)       z   +  1
102000071026     O                       sk1f(4)       z   +  1
102100071026     O                       sk1f(5)       z   +  1
102200071026     O                       sk1f(6)       z   +  1
102300071026     O                       sk1f(7)       z   +  1
102400071026     O                       sk1f(8)       z   +  1
102500071026     O                       sk1f(9)       z   +  1
102600071026     O                       sk1f(10)      z   +  1
102700071026     O                       sk1f(11)      z   +  1
102800071026     O                       sk1f(12)      z   +  1
102900071026     O                       sk1f(13)      z   +  1
103000071026     O                       sk1f(14)      z   +  1
103100071026     O                       sk1f(15)      z   +  1
103200071026     O                       sk1f(16)      z   +  1
103300071026     O                       sk1f(17)      z   +  1
103400071026     O                       sk1f(18)      z   +  1
103500071026     O                       sk1f(19)      z   +  1
103600071026     O                       sk1f(20)      z   +  1
103700071026     O          E            segn2f      1
103800071026     O                       sk2f(1)       z   + 30
103900071026     O                       sk2f(2)       z   +  1
104000071026     O                       sk2f(3)       z   +  1
104100071026     O                       sk2f(4)       z   +  1
104200071026     O                       sk2f(5)       z   +  1
104300071026     O                       sk2f(6)       z   +  1
104400071026     O                       sk2f(7)       z   +  1
104500071026     O                       sk2f(8)       z   +  1
104600071026     O                       sk2f(9)       z   +  1
104700071026     O                       sk2f(10)      z   +  1
104800071026     O                       sk2f(11)      z   +  1
104900071026     O                       sk2f(12)      z   +  1
105000071026     O                       sk2f(13)      z   +  1
105100071026     O                       sk2f(14)      z   +  1
105200071026     O                       sk2f(15)      z   +  1
105300071026     O                       sk2f(16)      z   +  1
105400071026     O                       sk2f(17)      z   +  1
105500071026     O                       sk2f(18)      z   +  1
105600071026     O                       sk2f(19)      z   +  1
105700071026     O                       sk2f(20)      z   +  1
105800071026     O          E            segn3f      1
105900071026     O                       sk3f(1)       z   + 30
106000071026     O                       sk3f(2)       z   +  1
106100071026     O                       sk3f(3)       z   +  1
106200071026     O                       sk3f(4)       z   +  1
106300071026     O                       sk3f(5)       z   +  1
106400071026     O                       sk3f(6)       z   +  1
106500071026     O                       sk3f(7)       z   +  1
106600071026     O                       sk3f(8)       z   +  1
106700071026     O                       sk3f(9)       z   +  1
106800071026     O                       sk3f(10)      z   +  1
106900071026     O                       sk3f(11)      z   +  1
107000071026     O                       sk3f(12)      z   +  1
107100071026     O                       sk3f(13)      z   +  1
107200071026     O                       sk3f(14)      z   +  1
107300071026     O                       sk3f(15)      z   +  1
107400071026     O                       sk3f(16)      z   +  1
107500071026     O                       sk3f(17)      z   +  1
107600071026     O                       sk3f(18)      z   +  1
107700071026     O                       sk3f(19)      z   +  1
107800071026     O                       sk3f(20)      z   +  1
107900071026     O          E            segn4f      1
108000071026     O                       sk4f(1)       z   + 30
108100071026     O                       sk4f(2)       z   +  1
108200071026     O                       sk4f(3)       z   +  1
108300071026     O                       sk4f(4)       z   +  1
108400071026     O                       sk4f(5)       z   +  1
108500071026     O                       sk4f(6)       z   +  1
108600071026     O                       sk4f(7)       z   +  1
108700071026     O                       sk4f(8)       z   +  1
108800071026     O                       sk4f(9)       z   +  1
108900071026     O                       sk4f(10)      z   +  1
109000071026     O                       sk4f(11)      z   +  1
109100071026     O                       sk4f(12)      z   +  1
109200071026     O                       sk4f(13)      z   +  1
109300071026     O                       sk4f(14)      z   +  1
109400071026     O                       sk4f(15)      z   +  1
109500071026     O                       sk4f(16)      z   +  1
109600071026     O                       sk4f(17)      z   +  1
109700071026     O                       sk4f(18)      z   +  1
109800071026     O                       sk4f(19)      z   +  1
109900071026     O                       sk4f(20)      z   +  1
110000071026     O          E            segn5f      1
110100071026     O                       sk5f(1)       z   + 30
110200071026     O                       sk5f(2)       z   +  1
110300071026     O                       sk5f(3)       z   +  1
110400071026     O                       sk5f(4)       z   +  1
110500071026     O                       sk5f(5)       z   +  1
110600071026     O                       sk5f(6)       z   +  1
110700071026     O                       sk5f(7)       z   +  1
110800071026     O                       sk5f(8)       z   +  1
110900071026     O                       sk5f(9)       z   +  1
111000071026     O                       sk5f(10)      z   +  1
111100071026     O                       sk5f(11)      z   +  1
111200071026     O                       sk5f(12)      z   +  1
111300071026     O                       sk5f(13)      z   +  1
111400071026     O                       sk5f(14)      z   +  1
111500071026     O                       sk5f(15)      z   +  1
111600071026     O                       sk5f(16)      z   +  1
111700071026     O                       sk5f(17)      z   +  1
111800071026     O                       sk5f(18)      z   +  1
111900071026     O                       sk5f(19)      z   +  1
112000071026     O                       sk5f(20)      z   +  1
112100071026     O          E            separa      1
112200071026     o                       lin198             198
112300071026     O          E            errRIGAf    1
112400071026     O                                         + 25 '* Attenzione non trovata '
112500071026     O                                         +  0 'spedizione figlia verifi'
112600071026     O                                         +  0 'care i dati della bolla *'
112700071011     O          E            errore         1 02
112800071011     O                       RAGUT             +  0
112900170221     O                                           25 '   '
113000071011     O                                         + 25 '* ELENCO SPEDIZIONI DA '
113100071011     O                                         +  0 'PORTARE NEL MAGAZZINO '
113200071011     O                                         +  0 'GIACENZE *'
113300071011     O                       §PGM               127
113400071011     O                       GMAUD8             147 '  /  /    '
113500071011     O                                         + 10 'PAG:'
113600071011     O                       PAGE          Z   +  1
113700071011     O          E            errore      2
113800071011     O                                         + 25 '* Attenzione errore in '
113900071011     O                                         +  0 'esecuzione programma  '
114000071011     O                                         +  0 'avvisare il CED *'
114100071011      *_________________________________________________________________________
114200071011**  CMD1
114300170222DLTF FILE(QTEMP/WFGCS60F)
114400071011**  CMD2
114500170222CRTDUPOBJ OBJ(WFGCS60F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
114600071011**  CMD3
114700170222DLTF FILE(QTEMP/WFGCS61L)
114800071011**  CMD4
114900170222CRTDUPOBJ OBJ(WFGCS61L) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
115000071011**  CMD5
115100170222DLTF FILE(QTEMP/WFGCS62L)
115200071011**  CMD6
115300170222CRTDUPOBJ OBJ(WFGCS62L) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
115400170222**  CMD7
115500170302DLTF FILE(QTEMP/WFGCS65L)
115600170222**  CMD8
115700170302CRTDUPOBJ OBJ(WFGCS65L) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
